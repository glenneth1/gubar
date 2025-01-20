(define-module (gubar blocks date-time)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 textual-ports)
  #:export (date-time))

(define* (date-time #:key (format "%c") (interval 1))
  "Creates a new date-time gublock."
  (let ((initial-block-scm
         `(("full_text" . ,(strftime format (localtime (current-time))))
           ("color" . "#B0E0E6"))))  ; Light blue in hex
    (gublock
     #:interval interval
     #:block initial-block-scm
     #:procedure
     (lambda (block)
       (set-block-full-text!
        block
        (strftime format (localtime (current-time))))
       block))))
