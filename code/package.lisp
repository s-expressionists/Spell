(cl:defpackage #:spell
  (:use
   #:cl)

  (:shadow
   #:case
   #:number)

  (:export
   #:english-lookup
   #:english-check-paragraph))
