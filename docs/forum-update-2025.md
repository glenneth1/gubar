# Gubar Update 2025: A Call for SystemCrafters Collaboration

Hi SystemCrafters community! 

Following up on @trev's original gubar project, I wanted to share the progress we've made and invite the community to join in developing this excellent Guile-based status bar generator.

## What is Gubar?

For those new to the project, gubar is a status bar generator for Sway written in Guile Scheme. It provides a hackable, scheme-based alternative to tools like i3blocks or waybar. What makes it special is its use of Guile's powerful features:

- Fiber-based concurrency for efficient block updates
- Full Scheme configuration for ultimate customization
- Native integration with system services
- Real-time interactive blocks

## Recent Improvements

Since the original release, we've added several enhancements:

1. **Robust Error Handling**
   - Graceful recovery from block failures
   - Detailed error reporting for debugging
   - Better user feedback for configuration issues

2. **System Resource Monitoring**
   - CPU usage and temperature
   - Memory utilization
   - Disk space monitoring
   - GPU temperature tracking

3. **Basic Audio Display**
   - PipeWire volume display
   - Basic volume status monitoring

4. **Network Status**
   - Basic network status display
   - WiFi connection indicator

## Why Join the Project?

Gubar has been an incredible learning platform for:

1. **Guile Scheme Programming**
   - Practical experience with Scheme in a real-world application
   - Understanding functional programming patterns
   - Working with Guile's concurrency model (fibers)

2. **Systems Integration**
   - Interfacing with system services
   - Real-time monitoring and reporting
   - Event handling and user interaction

3. **Linux Desktop Environment**
   - Understanding Wayland/Sway architecture
   - Status bar protocol implementation
   - System resource management

## Future Directions

We have several exciting areas for improvement:

1. **Configuration Ergonomics**
   - Developing a more intuitive configuration DSL
   - Creating block templates for common use cases
   - Improving error feedback

2. **Feature Expansion**
   - DBus integration for better service communication
   - Additional block types for various system metrics
   - Custom theming support
   - Interactive volume control with mute toggle
   - Advanced network management integration
   - Connection quality monitoring
   - WiFi network selection

3. **Documentation and Examples**
   - Comprehensive user guide
   - Block creation tutorials
   - Configuration examples

## Getting Started

The project is forked at: [github.com/glenneth1/gubar](https://github.com/glenneth1/gubar)

Requirements:
- Guile 3.0+
- guile-fibers
- guile-json
- Sway/i3bar compatible environment

## Call to Action

If you're interested in:
- Learning Guile Scheme in a practical context
- Contributing to a real-world system tool
- Improving Linux desktop integration
- Working with system monitoring and reporting

We'd love to have you join the project! Whether you're new to Scheme or an experienced Guiler, there are plenty of ways to contribute:

- Code contributions
- Documentation improvements
- Testing and bug reports
- Feature suggestions
- Configuration examples

## Next Steps

1. Check out the repository
2. Join the discussion here
3. Try gubar in your Sway setup
4. Share your experience and ideas

Let's make gubar a flagship SystemCrafters project and show what's possible with Guile Scheme in system tooling!

Happy Hacking! 

---
Note: Special thanks to @trev for creating the original gubar project and setting such solid foundations for this work.
