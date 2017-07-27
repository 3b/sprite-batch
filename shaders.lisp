(cl:in-package sprite-batch/shaders)

;; position vec4 = x,y,z,z-scale, location 0
(input transform1 :vec3 :location 1)
(input transform2 :vec3 :location 2)
(input tile :int :location 3) ;; layer or index into tileset metadata

(interface varyings (:out (:vertex outs)
                     :in (:geometry (gins "ins" :*)))
  (position :vec4)
  (tile :int)
  (transform1 :vec3)
  (transform2 :vec3))

(interface varyings (:out (:geometry gouts)
                     :in (:fragment ins))
  (uv :vec3)
  (tint :vec4))

(defun vertex ()
  (setf gl-position position)
  (setf (@ outs transform1) transform1
        (@ outs transform2) transform2
        (@ outs tile) tile))


(uniform view-projection :mat4)

(uniform spritesheet :sampler-2d)

(uniform tile-extents :sampler-buffer)
(uniform tile-offsets :sampler-buffer)

(uniform tint-texture :sampler-2d)

;; fragment outputs
(output color :vec4 :stage :fragment)

(defun mvp-generic (pos)
  (let* ((t1 (@ (aref gins 0) transform1))
         (t2 (@ (aref gins 0) transform2))
         (model (mat4 (.x t1) (.x t2) 0 0
                      (.y t1) (.y t2) 0 0
                      0 0 1 0
                      (.z t1) (.z t2) 0 1)))
    (return (* view-projection
               (mat4 1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     pos 0 1)
               model))))

(defun mvp-map (pos)
  (return (* view-projection
             (mat4 1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   pos 0 1))))

(defun mvp-particle (pos)
  (let* ((tx (@ (aref gins 0) transform1))
         (a (.x tx))
         (scale (.y tx))
         (s (* scale (sin a)))
         (c (* scale (cos a)))
         (model (mat4 c s 0 0
                      (- s) c 0 0
                      0 0 1 0
                      0 0 0 1)))
    (return (* view-projection
               (mat4 1 0 0 0
                     0 1 0 0
                     0 0 1 0
                     pos 0 1)
               model))))

(defun particle-tint ()
  (let ((uv (.xy (@ (aref gins 0) transform2))))
    (if (< (.y uv) 0) ;; row < 0 is treated as white
        (return (vec4 1 1 1 1))
        (return (texture tint-texture uv)))))

(defun emit-quad (mvp extents offsets z tint)
  (setf (@ gouts tint) tint)
  (setf (@ gouts uv) (vec3 (.xy extents) 0))
  (setf gl-position (* mvp (vec4 (.xy offsets) z 1)))
  (emit-vertex)
  (setf (@ gouts tint) tint)
  (setf (@ gouts uv) (vec3 (.xw extents) 0))
  (setf gl-position (* mvp (vec4 (.xw offsets) z 1)))
  (emit-vertex)
  (setf (@ gouts tint) tint)
  (setf (@ gouts uv) (vec3 (.zy extents) 0))
  (setf gl-position (* mvp (vec4 (.zy offsets) z 1)))
  (emit-vertex)
  (setf (@ gouts tint) tint)
  (setf (@ gouts uv) (vec3 (.zw extents) 0))
  (setf gl-position (* mvp (vec4 (.zw offsets) z 1)))
  (emit-vertex)
  (end-primitive))


(defun geometry ()
  (declare (layout (:in :points) (:out :triangle-strip
                                  :max-vertices 4)))
  (symbol-macrolet ((in (aref gins 0)))
    (let* ((c (@ (aref gl-in 0) gl-position))
           (mode (>> (logand (@ in tile) #xff000000) 24))
           (tile (logand (@ in tile) #x00ffffff))
           (extents (texel-fetch tile-extents tile))
           (offsets (texel-fetch tile-offsets tile))
           (mvp)
           (z 0.0)
           (tint (if (= mode 2) (particle-tint) (vec4 1 1 1 1))))
      (if (= mode 0)
          (setf mvp (mvp-generic (.xy c)))
          (if (= mode 1)
              (setf mvp (mvp-map (.xy c)))
              (setf mvp (mvp-particle (.xy c)))))
      (setf z (+ (.z c)
                 ;; w-component is scale for auto-z, which uses Y
                 ;; value to calculate an offset for Z value, so
                 ;; for example things lower on screen obscure
                 ;; things higher (= behind)
                 (* (/ (.y c) 1000) (.w c))))
      (emit-quad mvp extents offsets z tint))))


(defun fragment ()
  (let ((c (texture spritesheet (.xy (@ ins uv)))))
    (if (zerop (.w c))
        (discard)
        (setf color (* (@ ins tint) c)))))


(defun fragment/sdf ()
  (let ((c (texture spritesheet (.xy (@ ins uv)))))
    (if (zerop (.w c))
        (discard)
        (setf color c))))



(input color :vec4 :location 4)
(interface tv (:out (:vertex tv-outs)
               :in (:fragment tv-ins))
  (color :vec4))

(defun timing/v ()
  (setf gl-position (* view-projection (vec4 (.xy position) 0 1)))
  (setf (@ tv-outs color) color))

(defun timing/f ()
  (setf color (@ tv-ins color)))

(sprite-batch::defdict/3bgl sprite-batch::shaders ()
  (:generic :vertex vertex :fragment fragment :geometry geometry)
  (:generic/sdf :vertex vertex :fragment fragment/sdf :geometry geometry)
  (:timing :vertex timing/v :fragment timing/f))
