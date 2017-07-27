(defsystem :sprite-batch
  :description "2d sprite lib"
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :depends-on (alexandria cl-opengl 3bgl-shader static-vectors sb-cga opticl
                          mathkit glkit glop)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "shader-wrapper")
               (:file "shaders")
               (:file "textures")
               (:file "sprite-sheet")
               (:file "sprite-batch")
               ;; todo:separate these out...
               (:file "backend")
               (:file "timing")
               (:file "test")))

