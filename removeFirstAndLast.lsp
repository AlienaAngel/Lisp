(defun f1 (m)
   (cond
      ( (null (cddr m) ) nil)
      (t
          (cons (cadr m) (f1(cdr m)))
      ) 
   )
)

;f1 removes 1st and last elements