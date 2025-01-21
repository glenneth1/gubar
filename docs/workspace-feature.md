# Gubar Workspace Integration

This document outlines the design and implementation of workspace management in gubar.

## Overview

The workspace block provides a customizable way to display and interact with Sway workspaces in gubar. It replaces the default Sway workspace buttons with a more flexible, scheme-based implementation.

## Features

### Core Features
- Custom workspace naming
- Real-time workspace status updates
- Click-to-switch workspace functionality
- Visual indication of active workspace
- Support for workspace events (focused, urgent, etc.)

### Planned Extensions
- Workspace icons based on primary application
- Color coding for different workspace states
- Multi-monitor workspace management
- Dynamic workspace creation/removal
- Custom click actions (move windows, etc.)
- Workspace layouts preview

## Implementation Details

### 1. Core Components

#### Workspace Block Module
```scheme
(define-module (gubar blocks workspaces)
  #:use-module (gubar gublock)
  #:use-module (gubar swaybar-protocol)
  #:use-module (json)
  #:export (workspaces))
```

#### Data Structures
```scheme
;; Workspace configuration
(define-record-type <workspace-config>
  (make-workspace-config names icons colors)
  workspace-config?
  (names ws-names)    ; alist mapping numbers to names
  (icons ws-icons)    ; alist mapping types to icons
  (colors ws-colors)) ; alist mapping states to colors
```

#### Core Functions
1. `get-workspace-info`: Retrieves current workspace state
2. `format-workspace`: Formats a single workspace for display
3. `handle-workspace-click`: Processes click events
4. `subscribe-to-events`: Sets up IPC event subscription

### 2. Sway IPC Integration

The block will use Sway's IPC interface for:
- Getting workspace information
- Receiving workspace events
- Sending workspace commands

#### IPC Message Types
```scheme
;; Command message
(define-record-type <sway-command>
  (make-sway-command type payload)
  sway-command?
  (type cmd-type)
  (payload cmd-payload))

;; Event subscription
(define-record-type <sway-subscription>
  (make-sway-subscription events)
  sway-subscription?
  (events subscription-events))
```

### 3. Configuration Interface

Users can configure the workspace block through a simple interface:

```scheme
(workspaces 
 #:names '(("1" . "dev")
           ("2" . "web")
           ("3" . "chat"))
 #:icons '(("terminal" . "")
          ("browser" . "")
          ("chat" . ""))
 #:colors '(("focused" . "#ffffff")
           ("urgent" . "#ff0000")
           ("unfocused" . "#888888"))
 #:interval 1)
```

## Workspace Feature Implementation Status

### Current Status (2025-01-21)

### What Works
- Basic module structure is in place
- Successfully disabled default Sway workspace buttons
- Can communicate with Sway IPC (confirmed via `swaymsg -rt get_workspaces`)
- Module loads and appears in the bar (showing "WS Error")
- Fixed initial module loading issues with GUILE_LOAD_PATH

### Current Issues
1. Workspace display showing "WS Error" - error handling is catching some error but we need to identify what's failing
2. Need to verify if the error is in:
   - JSON parsing
   - Workspace data processing
   - Text formatting
   - Block updating

### Debug Progress
1. Confirmed Sway IPC works:
   - `swaymsg -rt get_workspaces` returns valid JSON
   - Shows correct workspace info including focused and visible states
   - Current workspace structure includes: num, name, focused, visible states

2. Fixed Initial Issues:
   - Fixed unbound variable error with `get-string-all`
   - Updated GUILE_LOAD_PATH to include all necessary directories
   - Simplified workspace block to basic functionality
   - Added extensive debug logging

3. Current Implementation:
   - Basic workspace display without Pango markup
   - Simple formatting with asterisk for focused workspaces
   - Error handling with debug output
   - Logging to `/tmp/gubar.log`

### Next Debug Steps
1. Check `/tmp/gubar.log` for specific error messages
2. Test JSON parsing separately from workspace formatting
3. Add more granular error handling to identify failing component
4. Test with hardcoded workspace data to isolate IPC issues

### Configuration
Current workspace block configuration:
```scheme
(workspaces #:names '(("1" . "dev")
                     ("2" . "web")
                     ("3" . "chat")
                     ("4" . "media")
                     ("5" . "misc")
                     ("6" . "sys")
                     ("7" . "doc")
                     ("8" . "vm")
                     ("9" . "gfx")
                     ("0" . "term"))
            #:interval 1)
```

### Files Modified
1. `/home/glenn/Projects/Code/gubar/src/gubar/blocks/workspaces.scm`
   - Created basic workspace block
   - Added error handling and logging
   - Simplified implementation for debugging

2. `/home/glenn/.config/sway/gubar.sh`
   - Updated GUILE_LOAD_PATH to include all necessary directories
   - Added logging to `/tmp/gubar.log`

3. `/home/glenn/.dotfiles/.config/gubar/config.scm`
   - Added workspace block configuration
   - Configured custom workspace names

### Next Steps
1. Check gubar logs for specific error messages
2. Test JSON parsing in isolation
3. Add more granular error handling
4. Consider testing with hardcoded workspace data
5. Once basic functionality works:
   - Re-add Pango markup for colors
   - Implement proper workspace click handling
   - Add workspace icons support
   - Add theme customization options

### Dependencies
- Requires Sway with IPC support
- Uses Guile's JSON module
- Uses ice-9 modules for process and string handling

## Implementation Plan

### Phase 1: Basic Functionality
1. Create workspace block module
2. Implement workspace info retrieval
3. Basic workspace display
4. Click-to-switch functionality

### Phase 2: Enhanced Features
1. Custom naming system
2. Icon support
3. Color theming
4. Event handling

### Phase 3: Advanced Features
1. Multi-monitor support
2. Dynamic workspace management
3. Application-based icons
4. Layout previews

## Testing

### Unit Tests
- Workspace info parsing
- Name/icon mapping
- Event handling
- Configuration validation

### Integration Tests
- Workspace switching
- Event subscription
- Multi-monitor scenarios
- Configuration changes

## Usage Examples

### Basic Configuration
```scheme
(use-modules (gubar blocks workspaces))

(list
 (workspaces #:names '(("1" . "dev")
                      ("2" . "web")
                      ("3" . "chat"))
             #:interval 1)
 ;; other blocks...
 )
```

### Advanced Configuration
```scheme
(workspaces 
 #:names '(("1" . "dev")
           ("2" . "web")
           ("3" . "chat"))
 #:icons '(("terminal" . "")
          ("browser" . "")
          ("chat" . ""))
 #:colors '(("focused" . "#ffffff")
           ("urgent" . "#ff0000")
           ("unfocused" . "#888888"))
 #:click-actions '(("left" . workspace-switch)
                  ("right" . workspace-menu)
                  ("middle" . workspace-move))
 #:interval 1)
```

## Contributing

Areas where contributions would be particularly valuable:
1. Icon mapping for different applications
2. Color theme presets
3. Additional click actions
4. Multi-monitor configurations
5. Performance optimizations
