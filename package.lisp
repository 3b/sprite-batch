(defpackage sprite-batch
  (:use :cl)
  (:intern #:spritesheet #:tile-extents #:tile-offsets)
  (:export #:view-projection))

(defpackage sprite-batch/shaders
  (:use :3bgl-glsl/cl)
  (:import-from #:sprite-batch
                #:view-projection
                #:tile-extents
                #:tile-extents))

