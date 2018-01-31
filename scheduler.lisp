(defstruct (pkg (:constructor %make-pkg))
  desc
  time
  subs)

(defun make-pkg (&key desc (time 0) (subs nil))
  (%make-pkg :desc desc
	     :time (if (null subs) time (reduce #'+ (mapcar #'pkg-time subs)))
	     :subs subs))

(defun export-pkgs (pkgs &key (start 0))
  (loop with start = start
	for pkg in pkgs
	do
	   (format t "~s, ~D, ~D ~%" (pkg-desc pkg) start (pkg-time pkg))
	   (export-pkgs (pkg-subs pkg) :start start)
	   (incf start (pkg-time pkg))))

(setq
 *test*
 (list
  (make-pkg :desc "Collect data" :time 7)
  (make-pkg :desc "Cloud free image"
	    :subs
	    (list
	     (make-pkg :desc "Investigate algorithms"
		       :subs
		       (list
			(make-pkg :desc "Thresholding" :time 2)
			(make-pkg :desc "Otsu" :time 4)
			(make-pkg :desc "Multiband" :time 4)))
	     (make-pkg :desc "False colour CFI" :time 12)
	     (make-pkg :desc "Seasonal CFI" :time 12)
	     (make-pkg :desc "Threshold satellite data with CFI" :time 10)
	     (make-pkg :desc "Time-series of cloud coverage" :time 7)))
  (make-pkg :desc "NDVI"
	    :subs
	    (list
	     (make-pkg :desc "Obtain cloud and land masks" :time 3)
	     (make-pkg :desc "Calculate NDVI" :time 4)
	     (make-pkg :desc "Analyse NDVI" :time 5)
	     (make-pkg :desc "get a life" :time 6)))
  (make-pkg :desc "SST"
	    :subs
	    (list
	     (make-pkg :desc "Refine cloud coverage algorithm" :time 7)
	     (make-pkg :desc "Test cloud coverage algorithm" :time 4)
	     (make-pkg :desc "Calculate short term means" :time 4)
	     (make-pkg :desc "Look for patterns" :time 4)))
  (make-pkg :desc "Ice caps" :time 13)
  (make-pkg :desc "Report writing" :time 21)))

(export-pkgs *test*)
