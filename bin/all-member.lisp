(defun all-member (x lst)
	(setq temp lst)
	(setq flag nil)
	(loop
		(if(listp (car temp))
			(setq flag (all-member x (car temp)))
			(if(eql x (car temp))
				(progn (setq flag t) (return nil))
				(setq temp (cdr temp))))
		(cond ((endp temp)(return nil))))
	flag)
