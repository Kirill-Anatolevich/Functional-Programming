; точка в полярных координатах
(defclass polar ()
 ((radius :initarg :radius :accessor radius) 	; длина >=0
  (angle  :initarg :angle  :accessor angle)))

(defmethod print-object ((p polar) stream)
  (format stream "[POLAR radius ~d angle ~d]"
          (radius p) (angle p)))


; точка в декартовых координатах
(defclass cart ()                ; имя класса и надклассы
 ((x :initarg :x :reader cart-x)   ; дескриптор слота x
  (y :initarg :y :reader cart-y))) ; дескриптор слота y

(defmethod print-object ((c cart) stream)
  (format stream "[CART x ~d y ~d]"
          (cart-x c) (cart-y c)))

(defmethod cart-x ((p polar))
  (* (radius p) (cos (angle p))))

(defmethod cart-y ((p polar))
  (* (radius p) (sin (angle p))))

(defgeneric to-cart (arg)
 (:documentation "Преобразование аргумента в декартову систему.")
 (:method ((c cart))
  c)
 (:method ((p polar))
  (make-instance 'cart
                 :x (cart-x p)
                 :y (cart-y p))) )

(defclass line ()
 ((start :initarg :start :accessor line-start)
  (end   :initarg :end   :accessor line-end)))

(defmethod print-object ((lin line) stream)
  (format stream "[ОТРЕЗОК ~s ~s]"
          (line-start lin) (line-end lin)))

(defgeneric vector-product (vec1 vec2)
  (:documentation "Векторное произведение двух векторов vec1 и vec2 на плоскости. Возращает координату z полученного вектора."))

(defgeneric find-projection-x (pt1 pt2)
			(:documentation "Находит проекцию на ось x вектора, имеющего начало точку pt1 и конец pt2."))

(defmethod find-projection-x ((pt1 cart) (pt2 cart))
  (- (cart-x pt2) (cart-x pt1)))

(defmethod find-projection-x ((pt1 polar) (pt2 cart))
  (- (cart-x (to-cart pt2)) (cart-x pt1)))

(defmethod find-projection-x ((pt1 polar) (pt2 polar))
  (- (cart-x (to-cart pt2)) (cart-x (to-cart pt1))))

(defmethod find-projection-x ((pt1 cart) (pt2 polar))
  (- (cart-x pt2) (cart-x (to-cart pt1))))

(defgeneric find-projection-y (pt1 pt2)
			(:documentation "Находит проекцию на ось y вектора, имеющего начало точку pt1 и конец pt2."))

(defmethod find-projection-y ((pt1 cart) (pt2 cart))
  (- (cart-y pt2) (cart-y pt1)))

(defmethod find-projection-y ((pt1 polar) (pt2 cart))
  (- (cart-y (to-cart pt2)) (cart-y pt1)))

(defmethod find-projection-y ((pt1 polar) (pt2 polar))
  (- (cart-y (to-cart pt2)) (cart-y (to-cart pt1))))

(defmethod find-projection-y ((pt1 cart) (pt2 polar))
  (- (cart-y pt2) (cart-y (to-cart pt1))))

(defmethod vector-product ((vec1 line) (vec2 line))
  (let ((vec1-proj-x (find-projection-x (line-start vec1) (line-end vec1)))
		(vec1-proj-y (find-projection-y (line-start vec1) (line-end vec1)))
		(vec2-proj-x (find-projection-x (line-start vec2) (line-end vec2)))
		(vec2-proj-y (find-projection-y (line-start vec2) (line-end vec2))))
		(- (* vec1-proj-x vec2-proj-y) (* vec1-proj-y vec2-proj-x))))

(defun check-intersection (l1 l2)
  (let ((z1 (vector-product l1 (make-instance 'line
                   :start (line-start l1)
                   :end (line-start l2))))
		(z2 (vector-product l1 (make-instance 'line
                   :start (line-start l1)
                   :end (line-end l2))))
		(z3 (vector-product l2 (make-instance 'line
                   :start (line-start l2)
                   :end (line-start l1))))
		(z4 (vector-product l2 (make-instance 'line
                   :start (line-start l2)
                   :end (line-end l1)))))
		(if (and (< (* z1 z2) 0) (< (* z3 z4) 0))
		  (let ((cx (cart-x (line-start l2)))
				(dx (cart-x (line-end l2)))
				(cy (cart-y (line-start l2)))
				(dy (cart-y (line-end l2))))
			(make-instance 'cart :x (+ cx (/ (* (- dx cx) (abs z1)) (abs (- z2 z1)))) :y (+ cy (/ (* (- dy cy) (abs z1)) (abs (- z2 z1))))))
		  NIL)))

(defun line-intersections (lines)
  (let ((res (list)))
	  (loop for i from 0 below (length lines) do
		(when (= i (- (length lines) 1)) (return res))
		(loop for j from (+ i 1) below (length lines) do 
			  (let ((ans (check-intersection (nth i lines) (nth j lines))))
			(if ans 
			(setq res (append res (list i j ans)))
			  ))))
	  ))

(setq lines (list (make-instance 'line
                   :start (make-instance 'cart :x 1 :y 4)
                   :end (make-instance 'cart :x 4 :y 1))
					(make-instance 'line
                   :start (make-instance 'cart :x 1 :y 1)
                   :end (make-instance 'cart :x 4 :y 3))
					(make-instance 'line
                   :start (make-instance 'cart :x 1 :y 2)
                   :end (make-instance 'cart :x 3 :y 4))
					))

(line-intersections lines)


(setq lines (list (make-instance 'line
                   :start (make-instance 'polar :radius 10 :angle 2.094)
                   :end (make-instance 'polar :radius 12 :angle 2.792))
					(make-instance 'line
                   :start (make-instance 'cart :x -14 :y 9)
                   :end (make-instance 'polar :radius 6 :angle 1))
					(make-instance 'line
                   :start (make-instance 'cart :x 0 :y 4)
                   :end (make-instance 'cart :x 4 :y 10))
					))

(line-intersections lines)
