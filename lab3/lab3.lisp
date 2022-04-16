(defun print-matrix (matrix &optional (chars 3) stream)
  ;; Предполагаем, что требуется
  ;;  3 знака по умолчанию на каждый элемент,
  ;;  6 знаков на #2A и скобки.
  (let ((*print-right-margin* (+ 6 (* (1+ chars) ; плюс пробел
                                      (array-dimension matrix 1)))))
    (pprint matrix stream)
    (values)))

(defun matrix-tr-br (n)
  (let ((arr (make-array (list n n))))
    (loop for i from 0 to n do 
        (when (= i n) (return arr))
        (if (oddp i)
            (loop for j from 1 to n do
                  (setf (aref arr i (- j 1)) (+ (* n i) j)))
          (loop for j downfrom n to 1 do
                (setf (aref arr i (- n j)) (+ (* n i) j)))))))
