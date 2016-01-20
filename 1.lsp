(defun d(mylist)
(cond
   (
     (null mylist)  "All number in list"
   )
  (
   (numberp (car mylist) ) (d (cdr mylist) )  )
   )
   (
     "Not all number in list"
   )
 )
)