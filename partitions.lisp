;; p(1) => ((1)) ;; 1 
;; p(2) => ((2), (1 1)) ;; 2
;; p(3) => ((3), (2 1), (1 1 1)) ;; 3
;; p(4) => ((4), (3 1), (2 2), (2 1 1), (1 1 1 1)) ;; 5
;; p(5) => ((5), (4 1), (3 2), (3 1 1), (2 2 1), (2 1 1 1), (1 1 1 1 1)) ;; 7
;; p(6) => ((6), (5 1), (4 2), (4 1 1), (3 3), (3 2 1), (3 1 1 1), (2 2 2), (2 2 1 1) (2 1 1 1 1), (1 1 1 1 1 1))

;; partitions(n) = [1 appended to partitions(n-1), 2 appended to partitions(n-2), ...,
;;                  n-i appended to partitions(n-i), ..., n appended to partitions(0)]
;; 
;; where "k appended to partitions(m)" means append k to all of the
;; ways of partitioning m, with the condition that k is no larger than
;; the element in the tail position of any partition. That's an ugly
;; mouthful.

(defun partitions (n)
  (if (< n 1)
      '()
      (labels ((rec (i)
                 (when (< i n)
                   (append (mapcar (lambda (p) (append p (list i)))
                                   (remove-if (lambda (p) (> i (first (last p)))) (partitions (- n i))))
                           (rec (1+ i))))))
        (append (list (list n))
                (rec 1)))))

(length (partitions 5))
