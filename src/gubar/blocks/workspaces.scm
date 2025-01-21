(define-module (gubar blocks workspaces)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (json)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (workspaces))

;; Main workspace block constructor
(define* (workspaces #:key 
                     (names '())
                     (interval 1))
  (format (current-error-port) "Creating workspace block with names: ~a~%" names)
  (gublock
   #:block '(("full_text" . "Test Workspaces")
            ("name" . "workspaces")
            ("color" . "#FFFFFF")
            ("align" . "left"))
   #:interval interval
   #:procedure
   (lambda (block)
     (catch #t
       (lambda ()
         (format (current-error-port) "Running workspace procedure~%")
         block)
       (lambda (key . args)
         (format (current-error-port) "Error in workspace block: ~a ~a~%" key args)
         block))))


;; Helper function to run swaymsg commands and get JSON output
(define (run-swaymsg-command cmd)
  (let ((port #f))
    (catch #t
      (lambda ()
        (set! port (open-input-pipe (string-append "swaymsg -rt " cmd)))
        (let ((output (get-string-all port)))
          (format (current-error-port) "swaymsg output: ~a~%" output)
          (close-pipe port)
          (if (string-null? output)
              '()
              (json-string->scm output))))
      (lambda (key . args)
        (when port (close-pipe port))
        (format (current-error-port) "Error in workspace block: ~a ~a~%" key args)
        '()))))

;; Format a single workspace for display
(define (format-workspace ws names)
  (let* ((num (assoc-ref ws "num"))
         (focused (assoc-ref ws "focused"))
         (visible (assoc-ref ws "visible"))
         (num-str (number->string num))
         (custom-name (assoc-ref names num-str)))
    (string-append 
     (if focused "*" (if visible "+" " "))
     "[" (or custom-name num-str) "]")))
