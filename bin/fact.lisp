(defun fact(n) 

	(do ((i 1 (+ i 1)) (j 1 (* j i)))
         	((> i n) j)))
         