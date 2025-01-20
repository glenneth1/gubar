# Error Handling in Gubar Blocks

This document details the error handling mechanisms implemented across various blocks in Gubar.

## Common Error Handling Patterns

### Visual Feedback
All blocks now use consistent visual indicators for different states:
- Error state: "ERR" in red (#FF0000)
- Not Available: "N/A" in gray (#808080)
- Normal state: Green (#00FF00)
- Warning state: Yellow (#FFFF00)
- Critical state: Red (#FF0000)

### Resource Management
All blocks implement proper resource cleanup:
- File handles are always closed, even in error cases
- Pipes are properly closed after use
- Commands are checked for successful execution
- Memory is properly managed

## Block-Specific Error Handling

### Battery Block
```scheme
Error handling for:
- Battery path detection
- AC adapter path detection
- File reading operations
- Capacity parsing
- Status validation

Key improvements:
- Auto-detection of power supplies
- Fallback paths for different systems
- Validation of battery capacity (0-100%)
- Safe file operations with proper cleanup
```

### Network Manager WiFi Block
```scheme
Error handling for:
- nmcli command execution
- Network status parsing
- Signal strength validation
- SSID validation

Key improvements:
- Command execution safety checks
- Signal strength bounds checking (0-100%)
- Proper pipe cleanup
- Safe string parsing
```

### Volume PipeWire Block
```scheme
Error handling for:
- pactl command execution
- Volume level parsing
- Sink detection
- JSON parsing

Key improvements:
- Safe command execution
- Volume level validation (0-100%)
- Proper pipe cleanup
- Safe JSON parsing with validation
- Mute state handling
```

### XKB Layout Block
```scheme
Error handling for:
- swaymsg command execution
- Layout parsing
- JSON parsing
- Alias validation

Key improvements:
- Command execution safety
- Layout name validation
- Safe JSON parsing
- Proper pipe cleanup
- Alias pair validation
```

### System Resources Block

#### CPU Monitor
```scheme
Error handling for:
- /proc/stat reading
- CPU usage calculation
- Temperature reading
- Numeric parsing

Key improvements:
- Safe file reading
- Bounds checking for CPU usage (0-100%)
- Temperature validation
- Proper cleanup of file handles
```

#### Memory Monitor
```scheme
Error handling for:
- free command execution
- Memory usage calculation
- Percentage parsing
- Command output validation

Key improvements:
- Safe command execution
- Memory usage bounds checking (0-100%)
- Safe numeric parsing
- Proper pipe cleanup
```

#### Disk Monitor
```scheme
Error handling for:
- df command execution
- Path validation
- Usage calculation
- Percentage parsing

Key improvements:
- Safe command execution
- Path existence checking
- Usage bounds checking (0-100%)
- Safe numeric parsing
```

#### GPU Monitor
```scheme
Error handling for:
- nvidia-smi command execution
- AMD/Intel GPU file reading
- Temperature parsing
- Device detection

Key improvements:
- Multiple GPU type support
- Temperature validation
- Safe command execution
- Proper file handle cleanup
```

## Common Helper Functions

### Safe File Reading
```scheme
(define (read-proc-file path)
  "Read content from a proc file safely.
   Returns #f if file cannot be read."
  (catch #t
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (let ((content (safe-read-line port)))
            (if content
                content
                (begin
                  (format #t "Empty or invalid content in ~a\n" path)
                  #f))))))
    (lambda (key . args)
      (format #t "Error reading ~a: ~a ~a\n" path key args)
      #f)))
```

### Safe Command Execution
```scheme
(define (run-command cmd)
  "Run a command and return its output safely.
   Returns #f if command fails."
  (catch #t
    (lambda ()
      (let* ((port (open-input-pipe cmd))
             (output (safe-read-line port))
             (status (close-pipe port)))
        (and (zero? status) output)))
    (lambda (key . args)
      (format #t "Error running ~a: ~a ~a\n" cmd key args)
      #f)))
```

### Safe Number Parsing
```scheme
(define (safe-string->number str)
  "Convert string to number safely.
   Returns #f if conversion fails."
  (catch #t
    (lambda ()
      (and str
           (let ((n (string->number str)))
             (and n (>= n 0) n))))
    (lambda (key . args)
      #f)))
```

## Best Practices

1. **Always Use Error Catching**
   - Wrap all I/O operations in catch blocks
   - Provide meaningful error messages
   - Clean up resources in error handlers

2. **Validate Input**
   - Check numeric bounds
   - Validate string content
   - Verify file/command existence

3. **Provide User Feedback**
   - Use consistent error messages
   - Use color coding for status
   - Show meaningful status text

4. **Resource Management**
   - Close files after use
   - Clean up pipes
   - Handle command failures

5. **Graceful Degradation**
   - Provide fallback values
   - Handle missing features
   - Support multiple systems
