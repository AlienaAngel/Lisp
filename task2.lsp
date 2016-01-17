(defun f2 (mylst)
    (cond
        (
            (null mylst)
                nil
        )
        (
            (minusp (car mylst) )
            (cons 0 (f2 (cdr mylst) ) )
        )
        (
            (cons (car mylst) (f2 (cdr mylst) ) )
        )
    )
)


(defun t1 (m)
    (cond
        (
            (null m) nil
        )
        (
            (zerop (car m)) (t1 (cdr m))

        )
        (t
            (cons (car m) (t1 (cdr m)))
 
        )
    )
)


(defun t2 (x m)
    (cond
        (
            (null m) nil
        )
        (
            (eq (car m) x) (cons x (cons x (t2  x (cdr m)))  )

        )
        (t
            (cons (car m) (t2 x (cdr m)))
 
        )
    )
)


(defun t3 (x m)
    (cond
        (
            (null m) 0
        )
        (
            (eq (car m) x) (+ 1 (t3 x (cdr m)))

        )
        (t
            (t3 x (cdr m))
 
        )
    )
)


(defun md (inN) 



(defun max_divisor (N i)
(cond
(
(zerop (rem N i) )
i
)
(
(max_divisor N (- i 1) )
)
)
)

    (max_divisor inN (- inN 1) )
)


(defun t4 (N)
    (cond
       ( (< N 10) N)
       (t
            (t4 (/ ( - N (rem N 10)) 10) )
       ) 
    )
)


(defun t5 (N)
    (__t5 N (div N 2))    
)

(defun __t5 (N i)
    (cond
        ((eq i 0) (list N))
        ((zerop (rem N i)) (cons i (__t5 N (- i 1))) )
        (t
            (__t5 N (- i 1))
        )
    )
)


(defun div (N a) 
    (/ ( - N (rem N a)) a)
)


(defun t6 (N)
    (cond 
        (  (zerop ( div N 10))   1  )
        (t
            (+ 1 (t6 (div N 10)))
        )
    )
)


(defun t7 (N)
    (cond 
        (  (zerop ( div N 10))   N  )
        (t
            (+ (rem N 10) (t7 (div N 10)))
        )
    )
)


(defun remlist (N d)
    (cond
        ((null N) 0)
        (
             (zerop (rem(car N) d)) (remlist (cdr N) d)

        )
        (t
            1
        )
    )
)

(defun t88 (N)
    (t8 N (car N))    
)

(defun t8 (N i)

(cond
        
      ((eq i 0)  Nil )
        
      ( (zerop (remlist N i) )  (cons i (t8 N (- i 1))) )
        
      (t
            (t8 N (- i 1))
        )

    )

)




(defun tt (N)
    (cond 
        (  (zerop ( div N 10))   (cons 1 N) )
        (t
          (cons
            (+ 1 (car (tt (div N 10))))
           (+ (rem N 10) (cdr (tt (div N 10))))
          )
        )
    )
)