(defun map (f lst) (if (atom lst) lst (cons (f (car lst)) (map f (cdr lst)))))

(defun ff (lst) (if (atom lst) lst (ff (car lst))))