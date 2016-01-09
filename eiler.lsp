(defun eiler (n)
     (eil (fact1 n) 1)
)

(defun eil(n i)
    (cond
        (
             ( > i (- n 1)) 0
        )
        (
             ( = 1 (gcd n i) ) (+ 1 (eil n (+ 1 i) ) )
        )
        (t
             (eil n (+ 1 i))
        )
    )
)


(defun fact1 (n)
     (cond
         (
             (zerop n) 1
         )
        (t
             (* n (fact1 (- n 1)))
        )
     )
)

