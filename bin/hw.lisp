(defun mem-str (x lst)
	(setq temp lst)
	(setq flag nil)
	(loop
		(if(string= x (car temp))
			(progn (setq flag T) (return nil))
			(setq temp (cdr temp)))
		(cond ((null temp) (return nil))))
	(if(null flag)
		nil (car temp)))




(defun not-alpha-space (x) 
	(not(or(alpha-char-p x) (char= x #\space))))





(defun strtok (str delimiters) 
  (let ((len (length str))) 
    (defun find-not-in-delimiters (i) 
      (if (>= i len) 
          len 
          (if (position (char str i) delimiters) 
              (find-not-in-delimiters (1+ i)) 
              i))) 
    (defun find-in-delimiters (i) 
      (if (>= i len) 
          len 
          (if (position (char str i) delimiters) 
              i 
              (find-in-delimiters (1+ i))))) 
    (defun find-pair (i) 
      (if (< i len) 
          (let* ((s (find-not-in-delimiters i)) 
                 (e (find-in-delimiters (1+ s)))) 
            (if (< s len) 
                (append (list (subseq str s e)) 
                        (find-pair (1+ e))))))) 
    (find-pair 0)))




	
	
	



(setq in(open "story.txt" :direction :input))
	
(setq word-list nil)
(setq mem-flag nil)

(loop
	(setq line (read-line in nil 'EOF))
	(if (eql line 'EOF)
		(return nil)
		(progn 
			(setq line (string-downcase (remove-if #'not-alpha-space line)))
			(setq buf (strtok line " "))
			(loop
				(if (endp buf)
					(return nil)
					(progn
						(setq tok (car buf))
						(setq temp-tok (mem-str tok word-list))
						(if (mem-str tok word-list)
							(setq mem-flag T)
							(progn
								(setq mem-flag nil)
								(setq word-list (cons tok word-list))))

						(if (null mem-flag)
							(setf (get 'sym (car word-list)) 1)
							(setf (get 'sym temp-tok) (+ (get 'sym temp-tok) 1)))
						(setf buf (cdr buf))))))))








(setq word-list (stable-sort word-list 'string<))
(dolist (x word-list)
	(format t "~A : ~D~%" x (get 'sym x)))
















