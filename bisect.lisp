(defun bisect (list elt &key (low 0) (high nil))
  "Find position in ordered LIST where ELT would be inserted such that
the result remains ordered. The keyword arguments :LOW and :HIGH are
used to specify the region of LIST in which to search for the position.

Note that the position will be a number X such that all positions in
LIST before X have (<= (NTH X LIST) ELT). This results in the position
being _after_ any elements that match ELT. For example
    (BISECT '(1 2 2 3) 2) => 3."
  (let* ((high (or high (length list)))
         (mid (floor (/ (+ low high) 2))))
    (if (>= low high)
        low
        (if (< elt (nth mid list))
            (bisect list elt :low low :high mid)
            (bisect list elt :low (1+ mid) :high high)))))
