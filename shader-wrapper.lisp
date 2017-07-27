(in-package sprite-batch)


;; hash of uncompiled -> compiled dictionaries
(defparameter *active-dictionaries* nil)
;; hash of function name -> list of dictionary objects
(defparameter *active-dictionaries-index* nil)
;; names of modified dictionaries
(defparameter *modified-dictionaries* nil)

(defmacro with-program-tracking (() &body body)
  `(let ((*active-dictionaries* (make-hash-table))
         (*active-dictionaries-index* (make-hash-table))
         (*modified-dictionaries* (make-hash-table)))
     ,@body))

(defun update-dict (dict)
  (format t "update ~s~%" dict)
  (when (symbolp dict)
    (setf dict (kit.gl.shader:find-dictionary dict)))
  (let ((3bgl-shaders:*default-version* 330))
    #++(loop
         for dn being the hash-keys of *active-dictionaries*
         for d = (kit.gl.shader:find-dictionary dn)
         do (loop
              for p in (slot-value d 'kit.gl.shader::programs)
              do (loop
                   for (stage f) in (slot-value p 'kit.gl.shader::shaders)
                   when (typep f '(cons (eql :3bgl-shader)))
                     do (let ((fn (second f)))
                          (pushnew dn
                                   (gethash fn *active-dictionaries-index*))))))
    (loop
      for p in (slot-value dict 'kit.gl.shader::programs)
      do (setf (slot-value p 'kit.gl.shader::uniforms) nil)
         (loop
           for (stage f) in (slot-value p 'kit.gl.shader::shaders)
           when (typep f '(cons (eql :3bgl-shader)))
             do (let ((fn (second f)))
                  (format t "~&  generate stage ~s ~s~%" stage fn)
                  (multiple-value-bind (s u)
                      (3bgl-shaders:generate-stage stage fn)
                    (format t "~s~%~%uniforms= ~s~%~%" s u)
                    (setf (third f) s)
                    (setf (slot-value p 'kit.gl.shader::uniforms)
                          (union u
                                 (slot-value p 'kit.gl.shader::uniforms))))))
         (format t "~&~%program ~s: unioforms ~s~%"
                 (slot-value p 'kit.gl.shader::name)
                 (slot-value p 'kit.gl.shader::uniforms)))))

(defun recompile-shader-dictionary (dict-name)
  #++(break "rsd" dict-name *modified-dictionaries* *active-dictionaries-index*
            *active-dictionaries*
            (kit.gl.shader:find-dictionary dict-name))
  (when (or (gethash dict-name *modified-dictionaries*)
            (not (gethash dict-name *active-dictionaries*)))
    (clrhash *active-dictionaries-index*)
    (setf (gethash dict-name *modified-dictionaries*) nil)
    (update-dict dict-name)
    (setf (gethash dict-name *active-dictionaries*)
          (kit.gl.shader:compile-shader-dictionary dict-name))
    (loop
      for dn being the hash-keys of *active-dictionaries*
      for d = (kit.gl.shader:find-dictionary dn)
      do (loop
           for p in (slot-value d 'kit.gl.shader::programs)
           do (loop
                for (nil f) in (slot-value p 'kit.gl.shader::shaders)
                when (typep f '(cons (eql :3bgl-shader)))
                  do (let ((fn (second f)))
                       (pushnew dn (gethash fn *active-dictionaries-index*)))))))
  (gethash dict-name *active-dictionaries*))

(defun update-modified-shaders (functions)
  (when (and functions *active-dictionaries*)
    (format t "~&checking programs: ~s~% in ~s~% (~s)~%~%"
            functions
            (alexandria:hash-table-alist *active-dictionaries-index*)
            (alexandria:hash-table-alist *active-dictionaries*))
    #++(break "ums" *active-dictionaries*
              *active-dictionaries-index*
              (kit.gl.shader:find-dictionary 'SPRITE-BATCH::SHADERS))
    (loop for function in functions
          do (loop for d in (gethash function *active-dictionaries-index*)
                   do (setf (gethash d *modified-dictionaries*) t)))))

(defmethod kit.gl.shader:parse-shader-source-complex ((key (eql :3bgl-shader))
                                                      params
                                                      shader-type
                                                      shader-list)
  (second params))


(defparameter *stage-name-map*
  (alexandria:plist-hash-table
   '(:vertex :vertex-shader
     :fragment :fragment-shader
     :geometry :geometry-shader
     :tess-control :tess-control-shader
     :tess-evaluation :tess-evaluation-shader
     :tess-eval :tess-evaluation-shader
     :compute :compute-shader)))

(defmacro defdict/3bgl (name () &body programs)
  (let ((dict-programs))
    (loop for (program . shaders) in programs
          for program-shaders
            = (loop for (.stage name) on shaders by #'cddr
                    for stage = (gethash .stage *stage-name-map* .stage)
                    collect (list stage (list :3bgl-shader name nil)))
          do (push `(make-instance 'kit.gl.shader::program-source
                                   :name ',program
                                   :uniform-style :camel-case
                                   :uniforms nil
                                   :shaders ',program-shaders)
                   dict-programs))
    `(let ((d (kit.gl.shader:define-dictionary ',name (list ,@dict-programs)
                :path *default-pathname-defaults*
                ;; todo: figure out if there is any point in keeping a
                ;; separate list of shaders used
                :shaders nil)))
       (update-dict ',name)
       d)))
