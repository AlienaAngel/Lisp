;411
(defun ng(lst)
    ;(sn lst)
    (ml (sn lst))
)
(defun sn(lst)
    (cond
        ((Null lst)
            (cons 0 0)
        )
        ((listp (car lst))
            (sac (sn (car lst)) (sn (cdr lst)))
        )
        ((< (car lst) 0)
            (sac (cons (car lst) 1) (sn (cdr lst)))
        )
        (
            (sac (cons 0 0) (sn (cdr lst)))
        )
    )
)
(defun sac (lst1 lst2)
    (cons (+ (car lst1) (car lst2)) (+ (cdr lst1) (cdr lst2)))
)
(defun ml (lst)
    (cond 
        ((> (cdr lst) 0)
            (/ (car lst) (cdr lst))
        )
        (
            0
        )
    )
)

;412
(defun n2 (x)
    (iai x 1)
)
(defun iai (x i)
    (cond
        ((<= (* i i) (/ x 2))
            (hq x i)
        )
        (
            -1
        )
    )
)
(defun hq (x i)
    (cond
        ((eq (root (- x (* i i)) 1) -1)
            (iai x (+ i 1))
        )
        (
            (list i (root (- x (* i i)) 1))
        )
    )
)
(defun root (x i)
    (cond
        ((<(* i i) x)
            (root x (+ i 1))
        )
        ((= (* i i) x)
            i
        )
        (
            -1
        )
    )
)

;413
(defun n3 (x)
    (n3f x 1)
)
(defun n3f (x i)
    (cond
        ((<= (* i i) (/ x 3))
            (root3 x (iai (- x (* i i)) i) i)
        )
        (
            -1
        )
    )
)
(defun root3 (x res i)
    (cond
        ((eq res -1)
            (n3f x (+ i 1))
        )
        (
            (cons i res)
        )
    )
)

;414
(defun Nn (x n)
    (nnf x n 1)
)
(defun nnf (x n i)
    (cond
        ((eq n 1)
            (list (root x 1))
        )
        ((<= (* i i) (/ x n))
            (rootn x (nnf (- x (* i i)) (- n 1) i) i n)
        )
        (
           (list -1)
        )
    )
)
(defun rootn (x res i n)
    (cond
        ((eq (car res) -1)
            (nnf x n (+ i 1))
        )
        (
            (cons i res)
        )
    )
)

;415
(defun mono (lst)
   (ch (rn lst) )
)
(defun ch (lst)
      (or (apply #' >= lst) (apply #' <= lst) )
)
(defun rn (lst)
 (cond
     (
       (null lst) nil
     )
     (
      (listp (car lst) )
            (append (rn (car lst)) (rn (cdr lst)) )
     )
     (
       (cons (car lst) (rn (cdr lst) ) )
     )
 )
)



















