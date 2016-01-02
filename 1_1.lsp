(defun m(a b)
(cond
    (
       (null a) nil
   )
   (
     (null b) nil
   )
   (t
       (append (list (car a) (car b))  (m (cdr a) (cdr b) ) )
   )
)
)

;function m makes from two lists 1 in format (a1 b1 a2 b2 a3 b3). The tail of
;the longest list will be cut.