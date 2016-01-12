(defun d(list)
(cond
    (
     (null list) nil
    )
   (
    (numberp (car list) ) (cons( car list ) ( d (cdr list) ) )
   )
   (
     (d (cdr list) )
   )
)
)