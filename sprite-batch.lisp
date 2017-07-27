(in-package sprite-batch)

;;; fixme: be consistent about calling things sprite-sheet vs tileset
;;; (or atlas).

(defclass batch-draw ()
  ((uniforms :reader uniforms :initarg :uniforms :initform nil)
   (sprite-sheet :reader sprite-sheet :initarg :sprite-sheet :initform nil)
   ;; probably always same VAO for current version, but will probably
   ;; extend to allow caching a VAO for static geometry instead of
   ;; respecifying it every frame
   (vao :reader vao :initarg :vao :initform nil)
   (start :reader start :initarg :start :initform 0)
   (element-count :reader element-count :initarg :element-count :initform 0)))

(defclass sprite-batch () ;; not intended for direct use
  ;; todo: cache last set uniforms, don't reset if not changed
  ((program :reader program :initarg :program :initform 'generic)
   (dict-name :reader dict-name :initarg :dict :initform 'shaders)
   (dict :reader dict :initform nil)
   ;; symbol -> (function-designator index), function-designator is
   ;; function to set uniform (gl:uniformi etc)
   (uniforms :reader uniforms :initform (make-hash-table))
   (draw-list :accessor draw-list :initform nil)
   (tileset :accessor tileset :initform nil)
   (view-projection :accessor view-projection
                    :initform (sb-cga:identity-matrix))))

(defgeneric finish-batch (s))


(defmethod bind ((s sprite-batch))
  (let ((old (dict s)))
    (setf (slot-value s 'dict) (recompile-shader-dictionary (dict-name s)))
    (when (and old (not (eql old (dict s))))
      (kit.gl:gl-delete-object old)
      (setf (slot-value s 'dict) (recompile-shader-dictionary (dict-name s)))
      (kit.gl.shader:use-program (dict s) (program s))
      (kit.gl.shader:uniformi (dict s) 'sprite-batch/shaders::spritesheet 0)
      (kit.gl.shader:uniformi (dict s) 'sprite-batch/shaders::tile-extents 1)
      (kit.gl.shader:uniformi (dict s) 'sprite-batch/shaders::tile-offsets 2)))
  (kit.gl.shader:use-program (dict s) (program s))
  (kit.gl.shader:uniform-matrix-1-sv (dict s) 'view-projection
                                     (view-projection s)))

(defmethod (setf tileset) :before (n (s sprite-batch))
  (finish-batch s))

(defmethod (setf view-projection) :before (n (s sprite-batch))
  (finish-batch s))

(defmethod flush ((s sprite-batch))
  (finish-batch s)
  (when (draw-list s)
    (bind s) ;; use program
    (loop for batch in (nreverse (shiftf (draw-list s) nil))
          unless (zerop (element-count batch))
            do ;; bind textures
               (bind (sprite-sheet batch))
               ;; bind vao
               (gl:bind-vertex-array (vao batch))
               ;; update uniforms
               (loop for uniform in (uniforms batch)
                     when uniform
                       do (apply (car uniform) (cdr uniform)))
               ;; draw
               (%gl:draw-arrays :points
                                (start batch)
                                (element-count batch)))))

;;; main API class, generic sprites with scale, rotation, auto-z
(defclass generic-sprite-batch (sprite-batch)
  ((transform :initform (make-array 6
                                    :element-type 'single-float
                                    ;; x,y,w from first 2 rows of 4x4 matrix
                                    :initial-contents '(1.0 0.0 0.0
                                                        0.0 1.0 0.0))
              :reader %transform)
   (base-z :initform 0.0 :reader base-z)
   (z-scale :initform 0.0 :reader z-scale)
   (vao :initform nil :reader vao)
   (vbos :initform nil :reader vbos)
   (vertex-data/i :reader vertex-data/i
                  :initform (make-array 1024 :element-type '(unsigned-byte 32)
                                             :fill-pointer 0 :adjustable t))
   (vertex-data/f :reader vertex-data/f
                  :initform (make-array 1024 :element-type 'single-float
                                             :fill-pointer 0 :adjustable t))
   (batch-start :initform 0 :accessor batch-start))
  (:default-initargs :program ':generic))

(defconstant +gsb-float-buffer-stride+ 10)
(defconstant +gsb-int-buffer-stride+ 1)

(defmethod initialize-instance :after ((s generic-sprite-batch) &key)
  (let* ((vao (gl:gen-vertex-array))
         (vbo (gl:gen-buffers 2)))
    (gl:bind-vertex-array vao)
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:enable-vertex-attrib-array 2)
    (gl:enable-vertex-attrib-array 3)
    (gl:bind-buffer :array-buffer (first vbo))
    (assert (= +gsb-float-buffer-stride+ (+ 4 3 3)))
    (let ((stride (* +gsb-float-buffer-stride+ 4)))
      (gl:vertex-attrib-pointer 0 4 :float nil stride 0)
      (gl:vertex-attrib-pointer 1 3 :float nil stride (* 4 4))
      (gl:vertex-attrib-pointer 2 3 :float nil stride (* 7 4)))
    (gl:bind-buffer :array-buffer (second vbo))
    (assert (= +gsb-int-buffer-stride+ 1))
    (let ((stride (* +gsb-int-buffer-stride+ 4)))
      (gl:vertex-attrib-ipointer 3 1 :int stride 0))
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    (setf (slot-value s 'vao) vao
          (slot-value s 'vbos) vbo)))

(defmethod flush :after ((s generic-sprite-batch))
  ;; reset cpu-side vertex buffers
  (setf (batch-start s) 0
        (fill-pointer (vertex-data/i s)) 0
        (fill-pointer (vertex-data/f s)) 0))

(defmethod bind :after ((s generic-sprite-batch))
  ;; upload vertex data
  (let ((count (/ (length (vertex-data/i s))
                  +gsb-int-buffer-stride+)))
    (assert (= count
               (/ (length (vertex-data/i s))
                  +gsb-int-buffer-stride+)
               (/ (length (vertex-data/f s))
                  +gsb-float-buffer-stride+)))
    (when (plusp count)
      ;; todo: avoid extra copies
      (static-vectors:with-static-vector (v (* +gsb-float-buffer-stride+
                                               count)
                                            :element-type 'single-float)
        (replace v (vertex-data/f s))
        (gl:bind-buffer :array-buffer (first (vbos s)))
        (%gl:buffer-data :array-buffer (* 4
                                          +gsb-float-buffer-stride+
                                          count)
                         (static-vectors:static-vector-pointer v)
                         :static-draw))
      (static-vectors:with-static-vector (v (* +gsb-int-buffer-stride+
                                               count)
                                            :element-type '(unsigned-byte 32))
        (replace v (vertex-data/i s))
        (gl:bind-buffer :array-buffer (second (vbos s)))
        (%gl:buffer-data :array-buffer (* 4
                                          +gsb-int-buffer-stride+
                                          count)
                         (static-vectors:static-vector-pointer v)
                         :static-draw)))))

(defmethod (setf base-z) (n (s generic-sprite-batch))
  (setf (slot-value s 'base-z) (float n 1.0))
  n)

(defmethod (setf z-scale) (n (s generic-sprite-batch))
  (setf (slot-value s 'z-scale) (float n 1.0))
  n)

;; todo: reader?
(defmethod (setf transform) (m (s generic-sprite-batch))
  ;; todo: support other formats?
  (assert (= (length m) 16))
  (let ((tx (%transform s)))
    (setf (aref tx 0) (aref m 0))
    (setf (aref tx 1) (aref m 4))
    (setf (aref tx 2) (aref m 12))
    (setf (aref tx 3) (aref m 1))
    (setf (aref tx 4) (aref m 5))
    (setf (aref tx 5) (aref m 13))
    m))


(defmethod vertex ((batch generic-sprite-batch) id x y)
  (vector-push-extend id (vertex-data/i batch))
  (let ((v (vertex-data/f batch)))
    (vector-push-extend x v)
    (vector-push-extend y v)
    (vector-push-extend (base-z batch) v)
    (vector-push-extend (z-scale batch) v)
    (loop for tx across (%transform batch)
          do (vector-push-extend tx v))))


(defmethod make-uniform ((s sprite-batch) id &rest arguments)
  (let ((u (gethash id (uniforms s))))
    (when (and u (second u))
      (append u arguments))))

(defmethod make-uniforms ((s sprite-batch))
  ;; assuming samplers are constant and already set
  ;; (0=2d sprite texture, 1,2=metadata buffer textures
  (list (make-uniform s 'view-projection (view-projection s))))

(defmethod finish-batch ((s generic-sprite-batch))
  (let* ((l (/ (length (vertex-data/i s))
               +gsb-int-buffer-stride+)))
    (when (and (plusp l)
               (> l (batch-start s)))
      (push  (make-instance 'batch-draw
                            :uniforms (make-uniforms s)
                            :sprite-sheet (tileset s)
                            :vao (vao s)
                            :start (batch-start s)
                            :element-count (- l (batch-start s)))
             (draw-list s))
      (setf (batch-start s) l))))
