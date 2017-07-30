(in-package sprite-batch)

(defclass test1 (window timing-helper)
  ((tileset :accessor tileset :initform nil)
   (sprite-batch :accessor sprite-batch :initform nil)
   (mvp :accessor mvp :initform (sb-cga:identity-matrix))
   #++(sprites :accessor sprites :initform (make-array 1024 :adjustable t
                                                            :fill-pointer 0))
   (sprites/x :accessor sprites/x
              :initform (make-array 1024 :adjustable t :fill-pointer 0
                                         :element-type 'single-float))
   (sprites/y :accessor sprites/y
              :initform (make-array 1024 :adjustable t :fill-pointer 0
                                         :element-type 'single-float))
   (sprites/id :accessor sprites/id
               :initform (make-array 1024 :adjustable t :fill-pointer 0
                                          :element-type '(unsigned-byte 32)))
   (sprites/s :accessor sprites/s
              :initform (make-array 1024 :adjustable t :fill-pointer 0
                                         :element-type 'single-float))))

(defparameter *fc* (list nil 0))

(defmethod init ((w test1))
  (setf (car *fc*) (now))
  (setf (sprite-batch w) (make-instance 'generic-sprite-batch))
  (setf (tileset w)
        (load-atlas
         "~/quicklisp/local-projects/3bgl-misc/data/debug-texture.png"
         64 64 :name :debug-texture
         :tile-offset #(-32 -32))))

(defmethod main-loop :around ((w test1))
  (with-texture-manager ()
    (kit.gl.shader:without-missing-uniform-errors (:print t)
      (with-program-tracking ()
        (call-next-method)))))

(defmethod resized :after ((w test1))
  (setf (mvp w)
        (print (sb-cga:matrix*
                (kit.math:ortho-matrix 0 (wx w) (wy w) 0 0 10)
                (sb-cga:scale* 1.0 1.0 1.0)
                (sb-cga:translate* 0.0 0.0 0.0)))))

(defparameter *w* nil)

(defun update (w)
  (declare (optimize speed))
  (when (and (tileset w) (sprite-batch w))
    (let ((sprites/x (sprites/x w))
          (sprites/y (sprites/y w))
          (sprites/s (sprites/s w))
          (sprites/id (sprites/id w))
          (sb (sprite-batch w))
          (m (sb-cga:identity-matrix)))
      (declare (type (array single-float (*))
                     sprites/x sprites/y sprites/s)
               (type (array (unsigned-byte 32) (*))
                     sprites/id))
      (setf (tileset sb) (tileset w))
      (setf (base-z sb) -9.0)
      (setf (z-scale sb) 1.0)
      (with-sprite-batch (sb)
        (loop with now = (float (now) 1.0)
              with wx = (float (wx w) 1.0)
              with wy = (float (wy w) 1.0)
              with cx = (/ wx 2.0)
              with cy = (/ wy 2.0)
              for x single-float across sprites/x
              for y single-float across sprites/y
              for .scale across sprites/s
              for scale = (* .scale 0.125 0.5)
              for id fixnum across sprites/id
              for n fixnum from 0
              ;;when (zerop (mod n 2))
              #+do (let* ((a (float (* (if (evenp id) -1 1)
                                       now id 0.01)
                                    1.0))
                          (c (* scale (cos a)))
                          (s (* scale (sin a))))
                     (setf (aref m 0) c
                           (aref m 1) s
                           (aref m 4) (- s)
                           (aref m 5) c)
                     (setf (transform sb) m))
              #++(setf (transform sb)
                       (sb-cga:matrix*
                        (let* ((a (float (* (if (evenp id) -1 1)
                                            now id 0.01)
                                         1.0))
                               (c (cos a))
                               (s (sin a)))
                          (sb-cga:matrix c     (- s) 0f0    0f0
                                         s     c     0f0    0f0
                                         0f0   0f0   1f0    0f0
                                         0f0   0f0   0f0    1f0))
                        #++
                        (sb-cga:rotate* 0.0 0.0 a)
                        (sb-cga:scale* scale scale 1.0)))
              do
              #++
               (setf x (+ x (- (random 1) 0.5) (* -0.1 (/ (- x cx) wx))))
              #++
               (setf y (+ y (1- (random 3)) (* -0.1 (/ (- y cy) wy))))
               (setf (aref sprites/x n) (nth-value 1 (ffloor (+ x
                                                                (/ id 512.0))
                                                             wx)))
               (setf (aref sprites/y n) (nth-value 1 (ffloor
                                                      (+ y
                                                         (/ id 1024.0))
                                                      wy)))
              #++
               (vertex sb id (float x 1.0) (float y 1.0))
              #++
               (progn ;; generic mode
                 (vx/f x)
                 (vx/f y)
                 (vx/f -9.0)
                 (vx/f 1.0)
                 (vx/f (aref m 0))
                 (vx/f (aref m 4))
                 (vx/f (aref m 12))
                 (vx/f (aref m 1))
                 (vx/f (aref m 5))
                 (vx/f (aref m 13))
                 (vx/i id))

               (progn ;; particle mode
                 (vx/f x)
                 (vx/f y)
                 (vx/f -9.0)
                 (vx/f 1.0)
                 (vx/f (* (if (evenp id) -1 1) now id 0.01)) ;; angle
                 (vx/f scale)                                ;; scale
                 (vx/f 0.0)  ;; ignored
                 (vx/f 0.0)  ;; tint u
                 (vx/f -1.0) ;; tint v (negative = no tint)
                 (vx/f 0.0)  ;; ignored
                 (vx/i (dpb 2 (byte 8 24) id))))))))

(defmethod draw ((w test1))
  (setf *w* (list w *texture-manager*))
  (incf (second *fc*))
  (gl:clear-color 0.3 0.1 (* (abs (sin (frame-time w))) 0.2) 1.0)
  (gl:clear :color-buffer :depth-buffer)
                                        ;  (gl:enable :cull-face)
  (gl:enable :depth-test)
  (gl:point-size 10)
  (gl:line-width 10)
  (let ((sb (sprite-batch w)))
    (when sb
      (mark w)
      (update w)
      (mark w :mode :cpu-delta)
      (setf (view-projection sb) (mvp w))
      (flush sb)
      (mark w)
      (mark w :mode :total)
      (mark w :mode :cpu-total)))
  #++(let ((p (gethash :main (programs w))))
       (when p
         (setf (3bgl-shaders::uniform p 'sprite-test/shaders::mvp)
               (mvp w))
         (setf (3bgl-shaders::uniform p 'sprite-test/shaders::spritesheet) 0)
         (setf (3bgl-shaders::uniform p 'sprite-test/shaders::tile-extents) 1)
         (setf (3bgl-shaders::uniform p 'sprite-test/shaders::tile-offsets) 2)
         (3bgl-shaders::with-program (p)
           (loop
             for map in (maps w)
             do (map nil 'draw (layers map))))
         (gl:bind-vertex-array 0))))

(setf 3bgl-shaders::*print-shaders* t)
(defparameter *sd* nil)
(defmethod key-event ((w test1) pressed keycode keysym string)
  (print keysym)
  (when pressed
    (case keysym
      (:escape
       (exit-main-loop w))
      (:f (let ((n (frame-time w)))
            (when (car *fc*)
              (let ((dt (- n (car *fc*)))
                    (c (cadr *fc*)))
                (format t "~s frames in ~6f sec = ~5f fps = ~5fms. c=~s~%"
                        c dt (/ c dt) (* 1000 (/ dt c))
                        (length (sprites/x w)))))
            (setf *fc* (list n 0))))
      #++(:c (setf (maps w)
                   (loop with *default-pathname-defaults* =
                         #p"d:/tmp/tilesets/colony/"
                         for m in (load-tmx "colony-db-map.tmx")
                         collect (apply #'make-map/tmx m))))
      #++(:k
          (setf (maps w)
                (loop with *default-pathname-defaults* =
                      #p"d:/tmp/tilesets/TileKit/"
                      for m in (load-tmx "TileKitDemo.tmx")
                      collect (apply #'make-map/tmx m)))
          ;; allocate a dynamic sprite layer
          (setf (sprite-layer w)
                (make-instance 'dynamic-layer
                               :tilesets (tilesets (car (layers (car (maps w)))))))
          ;; and add it to map so it will be drawn. add as 2nd layer of
          ;; first (=only) map
          (push (sprite-layer w) (cdr (layers (car (maps w))))))
      (:r (setf (fill-pointer (sprites w)) 0))
      (:p (setf *sd* (kit.gl.shader:compile-shader-dictionary
                      'sprite-batch::shaders)))
      ((:= :oem-plus)
       (loop for n below (length (sprites/x w))
             do (setf (aref (sprites/x w) n) (float (random (wx w)) 1.0)
                      (aref (sprites/y w) n) (float (random (wy w)) 1.0))))
      ((:oem3)
       (glop:swap-interval (glop-window w) 1))
      ((:- :oem-minus :substract)
       (loop for a in (list (sprites/x w)
                            (sprites/y w)
                            (sprites/s w)
                            (sprites/id w))
             do (setf (fill-pointer a) (max 0 (- (fill-pointer a) 1000))))
       (format t "~s sprites~%" (fill-pointer (sprites/x w))))
      (:space
       (loop repeat 1000
             do (vector-push-extend (float (random (wx w)) 1.0) (sprites/x w))
                (vector-push-extend (float (random (wy w)) 1.0) (sprites/y w))
                (vector-push-extend (random 2.0) (sprites/s w))
                (vector-push-extend (random 256) (sprites/id w)))
       (format t "~s sprites~%" (fill-pointer (sprites/x w)))))))

;;(run (make-instance 'test1 :width 800 :height 1000 :x 1924 :y 32))
;;(run (make-instance 'test1 :width 1920 :height 1080 :x 1920 :y 32))
;;(run (make-instance 'test1 :width 800 :height 1000 :x 4 :y 32))
