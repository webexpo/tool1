# Tool 1 version `5.0.0` (In development)

This major version introduces Tool 1 Simplified directly into Tool 1. The former
was rewritten during the process. All existing functionalities were preserved.

## User Visible Changes

* There is now additional information on the treatment of non-detects in the
  Frequently Asked Questions.

* The vocabulary used inside Estimates cards was shortened.

* Risk Band plots use the latest AIHA color scheme.

* Inference panels were renamed to Statistical Inference.

## Server Changes

None.

## Fixes

None.

# Tool 1 version `4.1.0`

## User Visible Changes

* Tool 1 is now mobile-friendly and should work with a wide variety of screens,
  including smaller ones. A minimum width of `400` pixels is strongly
  recommended.

* The title bar is now slightly smaller, and has more intuitive buttons with
  text labels.

* This changelog is now accessible directly from a link included in the footer
  of the Calculation Parameters sidebar and Frequently Asked Questions modal.

## Server Changes

* The implementation of `ui_title()` was revamped and optimized. The objective
  was to create a responsive Title Bar module leveraging
  [Bootrap 5 Navbar classes](https://getbootstrap.com/docs/5.3/components/navbar/).

* The implementation of `server_title()` is now a little bit faster. The number
  of times `bslib::update_tooltip()` is called has been reduced. All outputs
  are cached.

* Development script `R/publish.R` was refactored into a `publish()` function.
  It can either publish a development version or an official version of Tool 1.
  See `README.md` for details.

* Some application-level observers (defined in `app.R`) are now executed once
  and discarded afterwards.

* Attribute `lang` of the root `<html>` element and the window's title (the
  browser's tab name) are now only updated if `lang` is not equal to the
  default language. Doing so yields faster loading times.

## Fixes

* Estimates boxes now properly overflow vertically as a whole (with only one
  scrollbar) and not as two separate elements. The latter was confusing on
  smaller screens.

* Message boxes included in the Calculation Parameters sidebar now properly
  grow and shrink on all screens. They no longer collapse to a single line
  with a width equal to `2px` (top and bottom borders).
