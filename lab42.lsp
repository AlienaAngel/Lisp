;421
(defun co (lst)
    (cof lst 1)
)
(defun cof (lst d)
    (cond
        ((Null lst)
            d
        )
        ((listp (car lst))
            (dc (cof (car lst) (+ d 1)) (cof (cdr lst) d))
        )
        (
            (cof (cdr lst) d)
        )
    )
)
(defun dc (d1 d2)
    (cond
        ((< d1 d2)
            d2
        )
        (
            d1
        )
    )
)

;422
(defun arg (max)
    (do ((i 1 (+ i 1))        
        (x 0.1 (+ x (cond ((zerop (mod i 2)) 0.1) (0.15)))))        
        ((> x max) (/ x i))
        Nil
    )
)

;423
(defun dl (lst)
    (let ((res 0) (mn 0) (i 0))
        (dolist (x lst mn)
            (setq i (+ i 1))
            (setq mn (+ mn x))
        )
        (setq mn (/ mn i))    
        (dolist (x lst res)
            (setq res (+ res (kv (- x mn))))
        )
        (/ res i)
    )
)
(defun kv (x)
    (* x x)
)

;424
(defun ap (lst1 lst2)
    (mapcon  #'apf lst1 lst2)
)
(defun apf (a b)
    (cond
        ((Null (cdr a))
            (cons (car a) b)
        )
        ((Null (cdr b))
            (cons (car a) (cons (car b) (cdr a)))
        )
        (
            (list (car a) (car b))
        )
    )
)

;425
(defun ch (lst)
    (pr (tf lst Nil Nil))
)
(defun pr (lst)
    (cons lst (eval lst))
)
(defun co (x)
    (cond
        ((or (eq x '/) (eq x '*))
            2
        )
        ((or (eq x '+) (eq x '-))
            1
        )
        (
            Nil
        )        
    )
)
(defun tf (lst vs os)
    (cond
        ((Null lst)
            (te vs os)
        )
        ((co (car lst))
            (ho (car lst) lst vs os)
        )
        ((listp (car lst))
            (tf (cdr lst) (cons (tf (car lst) Nil Nil) vs) os)    
        )
        (
            (tf (cdr lst) (cons (car lst) vs) os)            
        )
    )
)
(defun tr (vs os)
    (list (car os) (car (cdr vs)) (car vs))
)
(defun ho (op lst vs os)
    (cond
        ((Null os)
            (tf (cdr lst) vs (list op))            
        )
        ((<= (co op) (co (car os)))
            (tf (cdr lst) (cons (tr vs os) (cdr (cdr vs))) (cons op (cdr os)))
        )
        (
            (tf (cdr lst) vs (cons op os))                  
        )
    )
)
(defun te (vs os)
    (cond
        ((Null (cdr os))
            (tr vs os)
        )
        (
            (te (cons (tr vs os) (cdr (cdr vs))) (cdr os))
        )
    )
)