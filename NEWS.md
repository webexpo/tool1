# Tool 1 version `4.1.0` (In development)

## User Visible Changes

* Tool 1 is now mobile-friendly and should work with a wide variety of screens,
  including smaller ones. A minimum width of `400` pixels is strongly
  recommended.

* The title bar is now slightly smaller, and has more intuitive buttons with
  text labels.

## Server Changes

* The implementation of `ui_title()` was revamped and optimized. The objective
  was to create a responsive Title Bar module. It now leverages
  [Bootrap 5 Navbar classes](https://getbootstrap.com/docs/5.3/components/navbar/).

* The implementation of `server_title()` is now a little bit faster. There are
  less calls to `bslib::update_tooltip()` and all static outputs are now cached.

## Fixes

None.
