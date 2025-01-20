(define-module (gubar blocks battery)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module ((ice-9 format) #:prefix fmt:)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (battery))

(define (read-proc-file path)
  "Safely read a single line from a file"
  (catch #t
    (lambda ()
      (let* ((port (open-input-file path))
             (content (read-line port)))
        (close-port port)
        (if (eof-object? content)
            #f
            (string-trim-both content))))
    (lambda (key . args)
      #f)))

(define (detect-power-supplies)
  "Detects available power supplies and returns a pair of (battery-path . ac-path)"
  (let* ((base-path "/sys/class/power_supply")
         (cmd (string-append "ls " base-path))
         (port (open-input-pipe cmd))
         (supplies (string-split (read-delimited "" port) #\newline))
         (_ (close-pipe port))
         (batteries '())
         (ac-adapters '()))
    
    ;; For each supply, check its type
    (for-each
     (lambda (supply)
       (let ((type-file (string-append base-path "/" supply "/type")))
         (when (file-exists? type-file)
           (let ((type (read-proc-file type-file)))
             (cond
              ((string= type "Battery")
               (set! batteries (cons supply batteries)))
              ((or (string= type "Mains")
                   (string= supply "AC")
                   (string= supply "ADP1"))
               (set! ac-adapters (cons supply ac-adapters))))))))
     supplies)
    
    ;; Return detected paths
    (cons 
     (if (null? batteries)
         #f  ; No battery found
         (string-append base-path "/" (car batteries) "/capacity"))
     (if (null? ac-adapters)
         #f  ; No AC adapter found
         (string-append base-path "/" (car ac-adapters) "/online")))))

(define* (battery #:key 
                 (battery-path #f)
                 (ac-path #f)
                 (format "~a ~a%") 
                 (nerd-icons #f))
  "Creates a battery monitor block.
   Arguments:
   battery-path: Optional path to battery capacity file
   ac-path: Optional path to AC adapter online status file
   format: Format string for display (default: \"~a ~a%\")
   nerd-icons: Whether to use nerd font icons (default: #f)"
  (let* ((detected-paths (detect-power-supplies))
         (bat (or battery-path 
                 (car detected-paths)
                 "/sys/class/power_supply/BAT0/capacity"))
         (ac (or ac-path 
                (cdr detected-paths)
                "/sys/class/power_supply/AC/online"))
         (icons '(󰁺 󰁻 󰁼 󰁽 󰁾 󰁿 󰂀 󰂁 󰂂 󰁹)))
    
    (gublock
     #:block '(("name" . "battery") ("full_text" . "N/A"))
     #:interval 3
     #:procedure
     (lambda (block)
       (let* ((level-str (read-proc-file bat))
              (ac-str (read-proc-file ac)))
         (if (and level-str ac-str)
             (let* ((level (string->number level-str))
                    (ac-status (string->number ac-str))
                    (label
                     (cond
                      ((= ac-status 1) (if nerd-icons "󰂄" "CHRG:"))
                      (nerd-icons (list-ref icons (truncate-quotient level 10)))
                      (else "BAT:"))))
               (set-block-full-text! block (fmt:format #f format label level))
               (set-block-urgent! block (<= level 10))
               block)
             (begin
               (set-block-full-text! block "BAT: ERR")
               block)))))))
