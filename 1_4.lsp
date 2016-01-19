(defun d(mylist)
(cond
   (
     (null mylist) nil
   )
   (
    (listp (car mylist))
     (cons (d(car mylist))(d(cdr mylist)))
   )
  (
    (symbolp (car mylist) ) (cons (car mylist) (d (cdr mylist) ) )
  )
  (
    (zerop(car mylist) )
       (cons "nol" ( d (cdr mylist) ) )
  )
  (
    (minusp (car mylist) ) 
         (cons "otricatelnie" ( d(cdr mylist) ) )
  )
  (
    (plusp (car mylist) ) 
        (cons "poloshitelnie" ( d(cdr mylist) ) )
  )
   (t
     (cons (d(car mylist)) (d (cdr mylist) )  )
   )
 )
)