(defpackage :asterblaster 
  (:use :cl :clws
        :cl-json
        :hu.dwim.defclass-star
        :hu.dwim.def
        :alexandria
        :bordeaux-threads
        :anaphora)
  (:import-from :chanl unbounded-channel send recv))
