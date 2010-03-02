(define defun (macro (name binds body) `(define ,name (lambda ,binds ,body))))
(define defmacro (macro (name binds body) `(define ,name (macro ,binds ,body))))

(defmacro 1+  (x) `(+ 1 ,x))
(defmacro incf (x) `(setq ,x (1+ ,x)))

(defun counter-class (x) (lambda () (incf x)))
(define counter (counter-class 0))

