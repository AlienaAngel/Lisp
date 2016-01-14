(defun sn (lst)
    (cond
       (
          (null lst) (cons 0 0)
       )
       (
          (listp (car lst))

           (cons (+ (car (sn(cdr lst))) (car (sn(car lst))) ) (+ (cdr(sn(car lst))) (cdr (sn(cdr lst))) ) )
       )
       (
          (< (car lst) 0)
          (cons  (+ (car lst) (car(sn(cdr lst))) ) (+ 1 (cdr (sn(cdr lst))) ) )
       )
       (t
          (sn (cdr lst))
       )
    )
) 