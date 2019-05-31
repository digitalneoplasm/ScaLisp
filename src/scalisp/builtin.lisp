(defun map (f lst)
  (if (atom lst)
    lst
    (cons (f (car lst)) (map f (cdr lst)))))

(defun ff (lst)
  (if (atom lst)
    lst
    (ff (car lst))))

;; car/cdr compound operations
(defun caar (x)
  (car (car x)))

(defun cadr (x)
  (car (cdr x)))

(defun cdar (x)
  (cdr (car x)))

(defun cddr (x)
  (cdr (cdr x)))