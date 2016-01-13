;311
(defun checkNumericList (lst)
    (cond
        ((Null lst)
            T
        )
        ((numberp (car lst))
            (checkNumericList (cdr lst))
        )
        (
            Nil
        )
    )
)

;312
(defun findMinPositive (lst)
    (findMinFunc (cdr lst) (car lst))
)
(defun findMinFunc (lst min)
    (cond
        ((Null lst)
            min
        )
        ((and (>(car lst) 0) (< (car lst) min))
            (findMinFunc (cdr lst) (car lst))
        )
        (
            (findMinFunc (cdr lst) min)            
        )
    )
)

;313
(defun checkMonotonous (lst)
    (checkMonoFunc (cdr (cdr lst)) (car (cdr lst)) (- (car (cdr lst)) (car lst)))
)
(defun checkMonoFunc (lst prev diff)
    (cond
        ((Null lst)
            T
        )
        ((>= (* (- (car lst) prev) diff) 0)
            (checkMonoFunc (cdr lst) (car lst) diff)
        )
        (
            Nil
        )
    )
)

;314
(defun mergeLists (lst1 lst2)
    (connectLists (concat lst1 Nil) (concat lst2 Nil) Nil)
)
(defun connectLists (lst1 lst2 lst3)
    (cond
        ((and (Null lst1) (Null lst2))
            lst3
        )
        ((Null lst1)
            (concat lst2 lst3)
        )
        ((Null lst2)
            (concat lst1 lst3)
        )
        ((> (car lst1) (car lst2))
            (connectLists (cdr lst1) lst2 (cons (car lst1) lst3))
        )
        (
            (connectLists lst1 (cdr lst2) (cons (car lst2) lst3))
        )
    )
)
(defun concat (lst lst2)
    (cond 
        ((Null lst)
            lst2
        )
        (
            (concat (cdr lst) (cons (car lst) lst2))
        )
    )
)

;315
(defun calculate (lst)
    (+ (findMax (cdr lst) (car (cdr lst))) (findMin lst (car lst)))
)
(defun findMax (lst max)
    (cond
        ((Null lst)
            max
        )
        ((> (car lst) max)
            (findMax (cdr (cdr lst)) (car lst))
        )
        (
            (findMax (cdr (cdr lst)) max)
        )
    )
)
(defun findMin (lst min)
    (cond
        ((Null lst)
            min
        )
        ((< (car lst) min)
            (findMin (cdr (cdr lst)) (car lst))
        )
        (
            (findMin (cdr (cdr lst)) min)
        )
    )
)

;316
(defun calculate2 (A N)
    (calcFunc A (list 0 N (* 2 N) (* N N)))
)
(defun calcFunc (A lst)
    (cond
        ((Null lst)
            1
        )
        (
            (* (- A (car lst)) (calcFunc A (cdr lst)))
        )
    )
)

;317
(defun TaylorSin (x N)
    (resolveTaylor (concat (TaylorSinFunc x N) Nil) 1)
)
(defun TaylorSinFunc (x N)
    (cond
        ((eq n 1)
            (cons x Nil)
        )
        (
            (cons (- 0 (/ (* x x) (* (- (* 2 N) 1) (- (* 2 N) 2)))) (TaylorSinFunc x (- N 1)))
        )
    )
)
(defun resolveTaylor (lst prev)
    (cond
        ((Null lst)
            0
        )
        (
            (+ (* (car lst) prev) (resolveTaylor (cdr lst) (* (car lst) prev)))
        )
    )
)

;318
(defun calculate3 (eps)
    (calc3Func eps 1 1)
)
(defun calc3Func (eps c sign)
    (cond
        ((< eps (/ 1 c))
            (+ (* sign (/ 1 c)) (calc3Func eps (+ c 1) (* sign -1)))
        )
        (
            0
        )
    )
)

;319
(defun findRoot (eps)
    (findRootFunc eps 0 (- 2 eps) (xFunc eps 0) (xFunc eps (- 2 eps)))
)
(defun findRootFunc (eps a b ax bx)
    (cond
        ((< (- b a) eps)
            (/ (+ b a) 2)
        )
        (
            (findRootFunc2 eps a b (xFunc eps (/ (+ b a) 2)) ax bx)
        )
    )
)
(defun findRootFunc2 (eps a b x ax bx)
    (cond
        ((< (* ax x) 0)
            (findRootFunc eps a (/ (+ b a) 2) ax x)
        )
        (
            (findRootFunc eps (/ (+ b a) 2) b x bx)
        )
    )
)
(defun calc4 (x eps)
    (TaylorLog x 1 x eps)
)
(defun TaylorLog (x c k eps)
    (cond
        ((< (abs (/ k c)) eps)
            0
        )
        (
            (+ (/ k c) (TaylorLog x (+ c 1) (* (* k x) -1) eps))
        )
    )
)
(defun xFunc (eps x)
    (+  x (-  (log (+ x 0.5)) 0.5))
)

;31A
(defun multiply (list)
    (cond
        ((Null list)
            1
        )
        ((listp (car list))
            (* (multiply (car list)) (multiply (cdr list)))
        )
        ((zerop (car list))
            (multiply (cdr list))
        )
        (
            (* (car list) (multiply (cdr list)))
        )
    )
)










