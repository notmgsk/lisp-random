(ql:quickload :cl-quil)
(in-package :cl-quil)

(defun cnot (control target)
  (format nil "CNOT ~A ~A~%" control target))

(defun h (target)
  (format nil "H ~A~%" target))

(defparameter parsed-program
  (parse-quil
   (concatenate 'string
                (H 3)
                (CNOT 1 2)
                (CNOT 2 3)
                (H 3)
                (CNOT 0 3)
                (H 3)
                (CNOT 2 3)
                (H 3)
                (H 2)
                (CNOT 0 3)
                (H 3)
                (CNOT 0 2))))

(defun cnot-p (instr)
  (and (typep instr 'gate-application)
       (equalp "CNOT"
               (operator-description-root-name (application-operator instr)))))

(defparameter cs-paper
  (build-chip-from-dag '((0 1 2)
                         (1 2)
                         (3 2 4)
                         (4 2))
                       :architecture ':cnot))


(defparameter cnot-02
  (parse-quil
   (concatenate 'string
                (cnot 0 2)
                (cnot 0 1))))
 
(defparameter cs-3-lin
  (build-chip-from-dag '((0 1) (1 2))))

(defun qubit-qubit-dependencies (parsed-program)
  (map 'list
       (lambda (instr)
         (mapcar #'qubit-index (application-arguments instr)))
       (remove-if-not #'cnot-p
                      (parsed-program-executable-code parsed-program))))

(defparameter +transforms+
  '((:cnot     . 0)
    (:reversal . 4)
    (:swap     . 7)
    (:bridge   . 10)))

(defun transform-cost (transform)
  (cdr (assoc transform +transforms+)))

;; In the paper this is ζ.
(defun transform-cost-for-mapping (transform mapping i)
  ;; TODO
  )


;; In the paper this is ϕ.
(defun minimum-cost-to-satisfy-dependency (mapping i)
  ;; TODO
  )

;; 
(defun)

;; In the paper this is L.
(defun minimim-cost-to-satisfy-all-dependencies (mapping i)
  ;; TODO
  )

(defun all-logical-to-physical-pairs (program chip)
  "A list of all pairs (LOGICAL PHYSICAL) mapping logical qubits used in PROGRAM to physical qubits available on CHIP."
  (a:map-product #'list (qubits-used program) (chip-spec-live-qubits chip)))


