# Gubar Configuration Improvements

This document outlines proposed improvements to make gubar's configuration system more user-friendly and ergonomic.

## Current State

Currently, users configure blocks using association lists and the `gublock` procedure. For example:

```scheme
(gublock 
    #:block '(("full_text" . "foo")
             ("name" . "example"))
    #:interval 60
    #:procedure (lambda (block) block))
```

While this is flexible, it can be verbose and requires users to understand Scheme's association lists.

## Proposed Improvements

### 1. Domain-Specific Language (DSL) for Block Definition

Create a more intuitive syntax for defining blocks using macros. Example:

```scheme
(define-block my-clock
  (display "%H:%M:%S")    ; replaces full_text in assoc list
  (update-interval 60)    ; more intuitive than #:interval
  (on-click (lambda (block) 
             (do-something)))
  (on-update (lambda (block)
               (update-time block))))
```

### 2. Common Block Templates

Provide pre-defined templates for common block types:

```scheme
(define-simple-block label "Hello")  ; For static text
(define-time-block clock "%H:%M")    ; For time displays
(define-command-block cpu "top -bn1") ; For command output
```

### 3. Block Properties Syntax

Introduce a more readable way to set block properties:

```scheme
(define-block volume
  (properties
    (name "volume")
    (icon "🔊")
    (color "#ffffff")
    (urgent? #f))
  (on-update update-volume))
```

### 4. Configuration Sections

Allow logical grouping of blocks and their properties:

```scheme
(define-config
  (defaults
    (font "Monospace")
    (colors
      (normal "#ffffff")
      (urgent "#ff0000")))
  
  (blocks
    (left
      (clock)
      (cpu)
      (memory))
    (center
      (workspace))
    (right
      (volume)
      (battery))))
```

## Benefits

1. **Readability**: Configuration becomes more self-documenting and easier to understand
2. **Type Safety**: Can add compile-time checks for required properties
3. **Maintainability**: Easier to modify and update configurations
4. **Learning Curve**: Reduces the need to understand Scheme-specific data structures

## Implementation Considerations

1. **Backward Compatibility**: Maintain support for current association list format
2. **Performance**: Ensure macros expand to efficient code
3. **Error Handling**: Provide clear error messages for configuration mistakes
4. **Documentation**: Include examples for all new syntax features

## Next Steps

1. Create prototype implementations of the DSL macros
2. Test with various block configurations
3. Gather user feedback on the proposed syntax
4. Document migration path from old to new syntax

## Example Full Configuration

Here's how a complete configuration might look with the proposed improvements:

```scheme
(use-modules (gubar blocks))

(define-config
  ;; Global settings
  (settings
    (separator "|")
    (separator-fg "#666666"))

  ;; Block definitions
  (blocks
    (date-time
      (format "%a %b %d %H:%M")
      (interval 60))
    
    (network
      (interface "wlan0")
      (icon "󰤨")
      (on-click toggle-wifi))
    
    (volume
      (mixer "pipewire")
      (on-scroll adjust-volume)
      (on-click toggle-mute))
    
    (battery
      (format "BAT: ~a%")
      (urgent-when (< level 15))
      (interval 120))))
```

This proposed syntax maintains the power and flexibility of Scheme while providing a more intuitive and user-friendly configuration experience.
