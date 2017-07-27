(in-package sprite-batch)

;; users should define a subclass and pass an instance to RUN
(defclass window ()
  ((glop-window :accessor glop-window)
   (frame-time :accessor frame-time)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)
   (wx :accessor wx :initarg :width :initarg :wx)
   (wy :accessor wy :initarg :height :initarg :wy)
   (title :accessor title :initarg :title)
   (gc-epoch :accessor gc-epoch :initform (list nil 0)))
  (:default-initargs :wx 640 :wy 480 :x 0 :y 0 :title "sprite-test"))

;;; users should define methods
(defgeneric resized (w))
(defgeneric init (w))
(defgeneric draw (w))
(defgeneric cleanup (w))
(defmethod key-event (w pressed keycode keysym string))

;;; users can define :before/:after/:around methods
(defgeneric run (w)) ;; creates window, calls other functions with GL context
(defgeneric main-loop (w)) ;; runs main loop, called with valid GL context

;;; call to exit
(defgeneric exit-main-loop (w))


(defclass %window (glop:window)
  ((exiting :initform nil :accessor exiting)
   (window :initform nil :accessor window)))

(defmethod glop:on-button ((w %window) pressed button))
(defmethod glop:on-mouse-motion ((w %window) x y dx dy))
(defmethod glop:on-resize ((w %window) width height)
  (setf (wx (window w)) width
        (wy (window w)) height)
  (resized (window w)))

(defmethod glop:on-draw ((w %window)))
(defmethod glop:on-close ((w %window)))
(defmethod glop:on-key ((w %window) pressed keycode keysym string)
  (key-event (window w) pressed keycode keysym string))

(defun now ()
  "return current GL timestamp in seconds as double float (returns 0d0
if no valid GL 3.2+ context is available)"
  ;; single float runs out of precision, so use doubles here and cast to single
  ;; if needed after calculating a delta or whatever
  (/ (or (ignore-errors (gl:get* :timestamp)) 0d0)
     1000000000d0))

(defmethod exit-main-loop ((w window))
  (setf (exiting (glop-window w)) t))

(defmethod key-event ((w window) pressed keycode keysym string)
  (when (eql keysym :escape)
    (exit-main-loop w)))

(defmethod resized ((w window))
  (gl:viewport 0 0 (wx w) (wy w)))


(defmethod init ((w window)))
(defmethod draw ((w window)))
(defmethod cleanup ((w window)))

(defparameter *modified-shaders* (list nil))

(defmethod draw :before ((w window))
  #+sbcl
  (let ((g (list sb-kernel::*gc-epoch* sb-ext:*gc-run-time*)))
    (unless (equalp g (gc-epoch w))
      (format t "~&~,5f:GC last frame, ~sms~%"
              (now) (- (second g) (second (gc-epoch w))))
      (setf (gc-epoch w) g)))
  (when *modified-shaders*
    (format t "updating modified shaders before draw: ~s~%"
            *modified-shaders*))
  (update-modified-shaders (shiftf *modified-shaders* nil)))

(defun modified-shader-hook (modified)
  (format t "saw modified shaders: ~s~%" modified)
  (setf *modified-shaders* (append modified *modified-shaders*)))

(pushnew 'modified-shader-hook 3bgl-shaders::*modified-function-hook*)


(defmethod main-loop ((w window))
  (with-simple-restart (continue "continue")
    (resized w))
  (with-simple-restart (continue "continue")
    (init w))
  (loop
    with gw = (glop-window w)
    ;; see if we are done
    until (exiting gw)
    while (with-simple-restart (continue "continue")
            (glop:dispatch-events gw))
    do (with-simple-restart (continue "continue")
         (draw w)
         (glop:swap-buffers gw)
         (setf (frame-time w) (now)))))


(defmethod run ((w window))
  (glop:with-window (gw (title w) (wx w) (wy w)
                        ;; we ask for gl 3.3+ core profile
                        :major 3 :minor 3 :profile :core
                        ;; :WIN-CLASS class tells it to use the
                        ;; specified class for the window object
                        :win-class '%window
                        :x (x w) :y (y w))

    ;; cl-opengl handles loading the function pointers, so no GLEW

    ;; initialize window data, update size/position
    (setf (glop-window w) gw
          (window gw) w
          (wx w) (glop:window-width gw)
          (wy w) (glop:window-height gw)
          (x w) (glop:window-x gw)
          (y w) (glop:window-y gw)
          (frame-time w) (now))
    (with-simple-restart (clean-exit "exit main loop")
      (main-loop w))
    (cleanup w)))
