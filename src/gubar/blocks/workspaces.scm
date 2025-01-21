(define-module (gubar blocks workspaces)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (json)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)  ; Add rdelim for get-string-all
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (workspaces))

;; Helper function to run swaymsg commands and get JSON output
(define (run-swaymsg-command cmd)
  (let ((port #f))
    (catch #t
      (lambda ()
        (format (current-error-port) "Running swaymsg command: ~a~%" cmd)
        (set! port (open-input-pipe (string-append "swaymsg -rt " cmd)))
        (let* ((output (get-string-all port))
               (result (if (string-null? output)
                         '()
                         (begin
                           (format (current-error-port) "Raw output: ~a~%" output)
                           (let ((json (json-string->scm output)))
                             (format (current-error-port) "Parsed JSON: ~a~%" json)
                             (if (vector? json)
                                 (vector->list json)
                                 (list json)))))))
          (close-pipe port)
          result))
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

;; Main workspace block constructor
(define* (workspaces #:key 
                     (names '())
                     (interval 1))
  (format (current-error-port) "Creating workspace block with names: ~a~%" names)
  (gublock
   #:block '(("full_text" . "Loading...")
            ("name" . "workspaces")
            ("color" . "#FFFFFF")  ; Default color for workspaces
            ("align" . "left"))
   #:interval interval
   #:procedure
   (lambda (block)
     (catch #t
       (lambda ()
         (format (current-error-port) "Running workspace procedure~%")
         (let* ((ws-info (run-swaymsg-command "get_workspaces")))
           (format (current-error-port) "Workspace count: ~a~%" (length ws-info))
           (if (null? ws-info)
               (begin
                 (set-block-full-text! block "No Workspaces")
                 (set-block-color! block "#FF0000")  ; Red for error
                 block)
               (let ((text (string-join 
                          (map (lambda (ws)
                                (format-workspace ws names))
                               (sort ws-info 
                                     (lambda (a b)
                                       (< (assoc-ref a "num")
                                          (assoc-ref b "num")))))
                          " ")))
                 (format (current-error-port) "Final text: ~a~%" text)
                 (set-block-full-text! block text)
                 (set-block-color! block "#FFFFFF")  ; Reset color to default
                 block))))
       (lambda (key . args)
         (format (current-error-port) "Error in workspace block: ~a ~a~%" key args)
         (set-block-full-text! block "WS Error")
         (set-block-color! block "#FF0000")  ; Red for error
         block)))))
