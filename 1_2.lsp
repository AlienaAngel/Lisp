(defun mn(list m n)
  (m_n list m n 1)
)






(defun m_n(list m n)
   (cond
     (
        (null list) nil
     )
     (
        (> m 0) (m_n (cdr list) (- m 1) n ) )
     )
     (
        (and (< m 0) (> m n)) (cons (car list) (m_n (cdr list) (- m 1) n ) )
     )
     (
        (< m n)list
     )
     (
        (m_n (cdr list) (- m 1) n ) )
     )
   )
)

(defun q( m)
(cond
    (
       (and (< m 0) (> m -88)) m
    )
)