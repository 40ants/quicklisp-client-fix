#+quicklisp
(in-package #:quicklisp)

#+quicklisp
(defun compute-load-strategy (name)
  (setf name (string-downcase name))
  (let ((asdf-systems nil)
        (quicklisp-systems nil)
        (already-processed (make-hash-table :test 'equal)))
    (labels ((recurse (name)
               (setf (gethash name already-processed)
                     t)
               (let ((asdf-system (asdf:find-system name nil))
                     (quicklisp-system (find-system name)))
                 (cond
                   (asdf-system
                    (push asdf-system asdf-systems))
                   
                   (quicklisp-system
                    (push quicklisp-system quicklisp-systems)
                    (dolist (subname (required-systems quicklisp-system))
                      (unless (gethash subname already-processed)
                        (recurse subname))))
                   
                   (t
                    (cond
                      ((string-equal
                        (asdf:primary-system-name name)
                        name)
                       (cerror "Try again"
                               'system-not-found
                               :name name)
                       (recurse name))
                      (t
                       (recurse (asdf:primary-system-name name)))))))))
      (with-consistent-dists
        (recurse name)))
    (make-instance 'load-strategy
                   :name name
                   :asdf-systems (remove-duplicates asdf-systems)
                   :quicklisp-systems (remove-duplicates quicklisp-systems))))
