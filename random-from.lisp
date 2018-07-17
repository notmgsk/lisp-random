(defun random-from (choices &key weights (n 1))
  "Take N from CHOICES sampled according to :WEIGHTS if provided,
otherwise each of CHOICES is equally likely."
  (let* ((len (length choices))
         (weights (or weights (make-list len :initial-element (float (/ len)))))
         (cumm (loop for i in weights for c = i then (+ c i) collect c))
         (total (first (last cumm))))
    (loop repeat n
          collect (nth (bisect cumm (random total) :low 0 :high (1- len))
                       choices))))

;; (count 5 (choose-from '(1 2 3 4 5 6 7 8 9 0)
;;                       :weights '(0 0 0 0 0.5 0.1 0.2 0.1 0.1)
;;                       :n 1000)) ;; => ~500
