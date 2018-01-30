;; See http://www.watrophy.com/posts/24-Run-Length-Encoding.html

(defun runs (list)
  ;; This is what the construction of runs "looks like" using REDUCE.
  ;; 
  ;; (1 1 2 2 3 1 2) -> (1 1 2 2 3 1 (2)) -> (1 1 2 2 3 (1) (2))
  ;; -> (1 1 2 2 (3) (1) (2)) -> (1 1 2 (2) (3) (1) (2))
  ;; -> (1 1 (2 2) (3) (1) (2))
  ;; -> (1 (1) (2 2) (3) (1) (2))
  ;; -> ((1 1) (2 2) (3) (1) (2))
  ;; (runs '(1 1 2 2 3 1 2) => ((1 1) (2 2) (3) (1) (2))
  (reduce (lambda (left right)
	    (cond
	      ;; This is what should happen first. REDUCE starting
	      ;; from the right-most element. RIGHT is NIL, LEFT is
	      ;; not. Obviously we have no other runs to worry about.
	      ((null right)
	       (list (list left)))
	      ;; Since we are going from the end of the input list to
	      ;; the beginning, any run that may be on-going will be
	      ;; the first element of RIGHT. If the first element of
	      ;; the first run is equal to LEFT, add it to that run.
	      ((= (car (car right)) left)
	       (append (list (append (list left) (car right)))
		       (rest right)))
	      ;; Otherwise we are starting a new run.
	      (t
	       (append (list (list left))
		       right))))
	  list :from-end t :initial-value '()))

(defun run-length-encoding (list)
  (mapcar (lambda (run) (list (car run) (length run)))
	  (runs list)))

;; (run-length-encoding '(1 2 2 3 3 3 4 4 4 4 5))
;;    -> ((1 1) (2 2) (3 3) (4 4) (5 1))
