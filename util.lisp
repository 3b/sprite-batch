(in-package sprite-batch)

(defun make-keyword (name)
  (when (and name (not (keywordp name)))
    (alexandria:make-keyword (string-upcase name))))
