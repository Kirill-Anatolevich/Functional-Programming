(defun extra-fun (res l)
  (if (rest l)
      (extra-fun (+ res (first l)) (rest l))
    (+ res (first l))))
      
(defun sum-of-list (l)
  (extra-fun 0 l))

(defun sum-product2 (l) (sum-of-list (mapcar (lambda (x1 x2) (* x1 x2)) l (reverse l))))
