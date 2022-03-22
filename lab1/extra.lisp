(defun knight-moves (k l m n) 
  (extra-fun k l m n 0))
;d - depth
(defun extra-fun (k l m n d)
  (cond ((OR (= d 3) (> k 8) (> l 8) (< k 1) (< l 1)) NIL)
        ((AND (= k m) (= l n) (/= d 0)) T)
        ((cond ((extra-fun (+ 2 k) (+ 1 l) m n (+ 1 d)) T))
		.....
