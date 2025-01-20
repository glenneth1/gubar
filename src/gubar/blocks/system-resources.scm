(define-module (gubar blocks system-resources)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:export (cpu-monitor memory-monitor disk-monitor gpu-monitor))

;; Helper functions
(define (safe-read-line port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        #f
        (string-trim-both line))))

(define (safe-string->number str)
  (and str
       (string->number str)))

(define (read-proc-file path)
  (catch #t
    (lambda ()
      (let* ((port (open-input-file path))
             (content (safe-read-line port)))
        (close-port port)
        content))
    (lambda (key . args)
      (format #t "Error reading ~a: ~a ~a\n" path key args)
      #f)))

(define (run-command cmd)
  (catch #t
    (lambda ()
      (let* ((port (open-input-pipe cmd))
             (output (safe-read-line port)))
        (close-pipe port)
        output))
    (lambda (key . args)
      (format #t "Error running ~a: ~a ~a\n" cmd key args)
      #f)))

;; Helper functions to get colors
(define (get-color-for-percentage percent)
  (cond
    ((>= percent 90) "#FF0000")  ; Red for critical (>=90%)
    ((>= percent 75) "#FFA500")  ; Orange for warning (>=75%)
    ((>= percent 50) "#FFFF00")  ; Yellow for moderate (>=50%)
    (else "#00FF00")))           ; Green for normal (<50%)

;; Helper function to get color for temperature
(define (get-color-for-temp temp)
  (cond
    ((>= temp 80) "#FF0000")     ; Red for hot (>=80°C)
    ((>= temp 70) "#FFA500")     ; Orange for warm (>=70°C)
    ((>= temp 60) "#FFFF00")     ; Yellow for moderate (>=60°C)
    (else "#00FF00")))           ; Green for cool (<60°C)

;; CPU Monitor
(define* (cpu-monitor #:key (format-str "CPU: ~a% ~a°C") (nerd-icons #f))
  (let ((prev-idle 0)
        (prev-total 0))
    (gublock
     #:block '(("name" . "cpu") ("full_text" . "CPU: N/A"))
     #:interval 2
     #:procedure
     (lambda (block)
       (catch #t
         (lambda ()
           (let* ((stat-port (open-input-file "/proc/stat"))
                  (stat-line (safe-read-line stat-port)))
             (close-port stat-port)
             (if (not stat-line)
                 (begin
                   (set-block-full-text! block "CPU: ERR")
                   (set-block-color! block "#FF0000")  ; Red for error
                   block)
                 (let* ((parts (string-split stat-line #\space))
                        (cpu-nums (filter-map safe-string->number (cdr parts)))
                        (idle (list-ref cpu-nums 3))
                        (total (apply + cpu-nums))
                        (diff-idle (- idle prev-idle))
                        (diff-total (- total prev-total)))
                   (set! prev-idle idle)
                   (set! prev-total total)
                   (let* ((cpu-usage (if (> diff-total 0)
                                       (truncate (- 100.0 (* 100.0 (/ diff-idle diff-total))))
                                       0))
                          (temp-port (open-input-file "/sys/class/thermal/thermal_zone2/temp"))
                          (temp-str (safe-read-line temp-port)))
                     (close-port temp-port)
                     (if (not temp-str)
                         (begin
                           (set-block-full-text! block (format #f "CPU: ~a%" cpu-usage))
                           (set-block-color! block (get-color-for-percentage cpu-usage))
                           block)
                         (let ((temp (truncate (/ (string->number temp-str) 1000.0))))
                           (set-block-full-text! block (format #f format-str cpu-usage temp))
                           (set-block-color! block (get-color-for-percentage cpu-usage))
                           (set-block-urgent! block (> temp 80))
                           block)))))))
         (lambda (key . args)
           (format #t "CPU Error: ~a ~a\n" key args)
           (set-block-full-text! block "CPU: ERR")
           (set-block-color! block "#FF0000")  ; Red for error
           block))))))

;; Memory Monitor
(define* (memory-monitor #:key (format-str "MEM: ~a%") (nerd-icons #f))
  (gublock
   #:block '(("name" . "memory") ("full_text" . "MEM: N/A"))
   #:interval 2
   #:procedure
   (lambda (block)
     (catch #t
       (lambda ()
         (let* ((cmd "free | grep Mem: | awk '{printf(\"%d\", ($2-$7)/$2 * 100)}'")
                (port (open-input-pipe cmd))
                (percent-str (read-line port))
                (_ (close-pipe port)))
           (if (or (eof-object? percent-str) (not percent-str))
               (begin
                 (set-block-full-text! block "MEM: ERR")
                 (set-block-color! block "#FF0000")  ; Red for error
                 block)
               (let ((percent (string->number percent-str)))
                 (if (not percent)
                     (begin
                       (set-block-full-text! block "MEM: ERR")
                       (set-block-color! block "#FF0000")  ; Red for error
                       block)
                     (begin
                       (set-block-full-text! block (format #f format-str percent))
                       (set-block-color! block (get-color-for-percentage percent))
                       (set-block-urgent! block (> percent 90))
                       block))))))
       (lambda (key . args)
         (format #t "Memory Error: ~a ~a\n" key args)
         (set-block-full-text! block "MEM: ERR")
         (set-block-color! block "#FF0000")  ; Red for error
         block)))))

;; Disk Monitor
(define* (disk-monitor #:key (format-str "DISK: ~a%") (path "/") (nerd-icons #f))
  (gublock
   #:block '(("name" . "disk") ("full_text" . "DISK: N/A"))
   #:interval 30
   #:procedure
   (lambda (block)
     (catch #t
       (lambda ()
         (let* ((cmd (string-append "df -P " path " | tail -n1 | awk '{printf(\"%d\", $5)}'")))
           (let* ((port (open-input-pipe cmd))
                  (percent-str (read-line port))
                  (_ (close-pipe port)))
             (if (or (eof-object? percent-str) (not percent-str))
                 (begin
                   (set-block-full-text! block "DISK: ERR")
                   (set-block-color! block "#FF0000")  ; Red for error
                   block)
                 (let ((percent (string->number percent-str)))
                   (if (not percent)
                       (begin
                         (set-block-full-text! block "DISK: ERR")
                         (set-block-color! block "#FF0000")  ; Red for error
                         block)
                       (begin
                         (set-block-full-text! block (format #f format-str percent))
                         (set-block-color! block (get-color-for-percentage percent))
                         (set-block-urgent! block (> percent 90))
                         block)))))))
       (lambda (key . args)
         (format #t "Disk Error: ~a ~a\n" key args)
         (set-block-full-text! block "DISK: ERR")
         (set-block-color! block "#FF0000")  ; Red for error
         block)))))

;; GPU Monitor
(define* (gpu-monitor #:key (format-str "GPU: ~a°C") (nerd-icons #f))
  (gublock
   #:block '(("name" . "gpu") ("full_text" . "GPU: N/A"))
   #:interval 2
   #:procedure
   (lambda (block)
     (catch #t
       (lambda ()
         (let ((nvidia-present? (file-exists? "/usr/bin/nvidia-smi")))
           (if nvidia-present?
               (let ((output (run-command "nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader")))
                 (if (not output)
                     (begin
                       (set-block-full-text! block "GPU: ERR")
                       block)
                     (let ((temp (safe-string->number (string-trim-both output))))
                       (if (not temp)
                           (begin
                             (set-block-full-text! block "GPU: ERR")
                             block)
                           (begin
                             (set-block-full-text! 
                              block 
                              (format #f format-str temp))
                             (set-block-color! block (get-color-for-temp temp))
                             (set-block-urgent! block (> temp 80))
                             block)))))
               ;; Try reading from AMD/Intel GPU if available
               (let ((gpu-temp-path "/sys/class/drm/card0/device/hwmon/hwmon*/temp1_input"))
                 (if (file-exists? gpu-temp-path)
                     (let ((temp-content (read-proc-file gpu-temp-path)))
                       (if (not temp-content)
                           (begin
                             (set-block-full-text! block "GPU: ERR")
                             (set-block-color! block "#FF0000")  ; Red for error
                             block)
                           (let ((temp (truncate (/ (string->number temp-content) 1000.0))))
                             (set-block-full-text! 
                              block 
                              (format #f format-str temp))
                             (set-block-color! block (get-color-for-temp temp))
                             (set-block-urgent! block (> temp 80))
                             block)))
                     (begin
                       (set-block-full-text! block "GPU: N/A")
                       (set-block-color! block "#808080")  ; Gray for N/A
                       block)))))
         block)
       (lambda (key . args)
         (format #t "GPU Error: ~a ~a\n" key args)
         (set-block-full-text! block "GPU: ERR")
         block)))))
