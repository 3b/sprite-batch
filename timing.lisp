(in-package sprite-batch)

(defclass timing-helper ()
  ((max-queries :reader max-queries :initform 16 :initarg :max-timer-queries)
   (query-index :accessor query-index :initform 0)
   (timestamps :accessor timestamps :initform nil)
   (timestamp-masks :accessor timestamp-masks :initform nil)
   (timestamp-mode :accessor timestamp-mode :initform nil)
   (cpu-timestamps :accessor cpu-timestamps :initform nil)
   (times :accessor times :initform nil)
   (times-x :accessor times-x :initform 0)
   (timing-vao :accessor timing-vao :initform 0)
   (timing-vbo :accessor timing-vbo :initform 0)))

(defmethod main-loop :before ((w timing-helper))
  ;; todo: adjust max-queries incrementally?
  (let ((mq (max-queries w)))
    (setf (timestamps w) (list (gl:gen-queries mq)
                               (gl:gen-queries mq)))
    (setf (timestamp-masks w) (list (make-array mq :initial-element nil)
                                    (make-array mq :initial-element nil)))
    (setf (timestamp-mode w) (list (make-array mq :initial-element nil)
                                   (make-array mq :initial-element nil)))
    (setf (cpu-timestamps w) (make-array mq :initial-element 0d0
                                            :element-type 'double-float))
    (setf (times w)
          (coerce (loop repeat mq collect (make-array 512 :initial-element 0))
                  'vector)))
  (let* ((vao (gl:gen-vertex-array))
         (vbo (gl:gen-buffer)))
    (gl:bind-vertex-array vao)
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 4)
    (gl:bind-buffer :array-buffer vbo)
    (let ((stride (* (+ 2 4) 4)))
      (gl:vertex-attrib-pointer 0 2 :float nil stride 0)        ;; pos
      (gl:vertex-attrib-pointer 4 4 :float nil stride (* 2 4))) ;; color
    (gl:bind-buffer :array-buffer 0)
    (gl:bind-vertex-array 0)
    (setf (timing-vao w) vao)
    (setf (timing-vbo w) vbo)))

(defmethod mark (w &key mode)
  (declare (ignore w mode))
  ;; ignore if we don't have the mixin so it can be easily disabled
  ;; without modifying all the code
  nil)

(defmethod mark ((w timing-helper) &key (mode :delta))
  (let ((n (query-index w)))
    (assert (< n (max-queries w)))
    (%gl:query-counter (nth n (car (timestamps w))) :timestamp)
    (setf (aref (car (timestamp-masks w)) n) t)
    (setf (aref (car (timestamp-mode w)) n)
          mode)
    (setf (aref (cpu-timestamps w) n) (float (gl:get* :timestamp) 1d0))
    (incf (query-index w))))

(defmethod draw :around ((w timing-helper))
  (setf (query-index w) 0)
  (mark w :mode :start)
  (loop for a in (timestamp-mode w) do (fill a :unused))

  (call-next-method)
  (gl:disable :depth-test)
  (rotatef (first (timestamps w)) (second (timestamps w)))
  (rotatef (first (timestamp-masks w)) (second (timestamp-masks w)))
  ;; update TIMES
  (cffi:with-foreign-objects ((done '%gl:int)
                              (time '%gl:uint64))
    (let ((queries (make-array (max-queries w) :initial-element 0)))
                                        ;(declare (dynamic-extent queries))
      (loop for i below (max-queries w)
            for id in (car (timestamps w))
            when (aref (car (timestamp-masks w)) i)
              do (setf (aref (car (timestamp-masks w)) i) nil)
                 (%gl:get-query-object-iv id :query-result-available done)
                 (when (plusp (cffi:mem-ref done '%gl:int))
                   (%gl:get-query-object-ui64v id :query-result time)
                   (setf (aref queries i) (cffi:mem-ref time '%gl:uint64))))
      (loop with x = (setf (times-x w)
                           (mod (1+ (times-x w))
                                (length (aref (times w) 0))))
            with timestamps = (cpu-timestamps w)
            for i from 0
            for mode across (car (timestamp-mode w))
            do (case mode
                 (:delta
                  (assert (plusp i))
                  (setf (aref (aref (times w) i) x)
                        (- (aref queries i)
                           (aref queries (1- i)))))
                 (:cpu-delta
                  (assert (plusp i))
                  (setf (aref (aref (times w) i) x)
                        (- (aref timestamps i)
                           (aref timestamps (1- i)))))
                 (:total
                  (setf (aref (aref (times w) i) x)
                        (- (aref queries i)
                           (aref queries 0))))
                 (:cpu-total
                  (setf (aref (aref (times w) i) x)
                        (- (aref timestamps i)
                           (aref timestamps 0))))
                 (t
                  (setf (aref (aref (times w) i) x) 0))))))
  ;; draw graphs
  ;; fixme: convert this stuff to VBOs or something instead of immediate mode
  (let ((d (dict (sprite-batch w))))
    (when d
      (kit.gl.shader:use-program d :timing)
      (kit.gl.shader:uniform-matrix-1-sv d 'view-projection
                                         (kit.math:ortho-matrix 0 (wx w)
                                                                0 (wy w)
                                                                -1 1))
      #++(format t "~s, ~s, ~s~%" (length (times w))
                 (map 'vector 'length (times w))
                 (reduce '+ (map 'vector 'length (times w))))
      (static-vectors:with-static-vector (v (* 6
                                               (+ (* 2 20)
                                                  (* 2 (reduce '+ (map 'vector 'length (times w)))))
                                               32)
                                            :element-type 'single-float)
        (gl:line-width 1.0)
        (let ((index 0)
              (c (list 1 1 1 1)))
          (flet ((c (&rest cc)
                   (setf c cc))
                 (v (x y)
                   (setf (aref v (+ (* index 6) 0)) (float x 1.0))
                   (setf (aref v (+ (* index 6) 1)) (float (+ y 0.5) 1.0))
                   (destructuring-bind (r g b a) c
                     (setf (aref v (+ (* index 6) 2)) (float r 1.0))
                     (setf (aref v (+ (* index 6) 3)) (float g 1.0))
                     (setf (aref v (+ (* index 6) 4)) (float b 1.0))
                     (setf (aref v (+ (* index 6) 5)) (float a 1.0)))
                   (incf index)
                   (assert (< (* 6 index) (length v)))))
            (loop
              with d = 101
              with s = 10
              with i = 0
              for x1 = (times-x w)
              for mode across (car (timestamp-mode w))
              for times across (times w)
              for color = (1+ i)
              for r = (ldb (byte 1 0) color)
              for g = (ldb (byte 1 1) color)
              for b = (ldb (byte 1 2) color)
              for a = (ldb (byte 1 3) color)
              when (member mode '(:delta :total :cpu-delta :cpu-total))
                do (when (= color 4)
                     ;; pure blue is hard to see, lighten it a bit
                     (setf g 0.4 r 0.4))
                   (if (plusp a)
                       (c (* r 0.5) (* g 0.5) (* b 0.5) 1)
                       (c r g b 1))
                   (v 0 (* i d))
                   (v 512 (* i d))
                   (loop for j below 20 by 1
                         do (v 0 (+ (* s j) (* i d)))
                            (v 10 (+ (* s j) (* i d))))

                   (progn              ;gl:with-primitives :line-strip
                     (loop for j from 2 below 512
                           for y = (aref times (1- j))
                           for y2 = (aref times j)
                           ;; 10/1000000.0 =  10px/ms
                           do (v (1- j) (+ (* i d) (* s (/ y 1000000.0))))
                              (v j (+ (* i d) (* s (/ y2 1000000.0))))))
                   (incf i)
              finally (progn
                        (c 1 1 1 1)
                        (v x1 0)
                        (v x1 (* 13 d)))))
          (gl:bind-buffer :array-buffer (timing-vbo w))
          (%gl:buffer-data :array-buffer (* 4 6 index)
                           (static-vectors:static-vector-pointer v)
                           :static-draw)
          (gl:bind-vertex-array (timing-vao w))
          (%gl:draw-arrays :lines 0 index)))))
                                        ;(gl:disable :texture-2d :texture-3d)
  #++(gl:with-pushed-matrix* (:modelview)
       (gl:load-identity)))
