---
title: Expostats - Tool 1 Changelog
output:
    html_document:
        toc: TRUE
        toc_float: TRUE
        mathjax: null
        css: www/static/_static.css
---

# Tool 1 version `5.1.0`

This version is a massive milestone for Expostats as it becomes the official
version of Tool 1 and Tool 1 Express (it is now *out of beta*).

## User Visible Changes

* Tool 1 is now available in French.

* The source text was reviewed and rewritten for consistency and simplicity.
  Notably, the abbrevation OEL is now always used for brevity and clarity.

* Fixed incorrect descriptions of the Exceedance Plot's variants.

* Mode names were revamped: `default` is now `extended` and `simplified` is now
  `express`. Labels were renamed accordingly.

* The default mode is now `express` (Tool 1 Express).

* There is a new optional input in extended mode: Occupational Exposure Limit
  Multiplier. Use this value to modify the OEL with an arbitrary multiplier.
  The resulting product of these values will be used as the effective OEL in
  all subsequent calculations and analyses.

* Panels now have a title bar showing the current mode and the panel's
  underlying name.

* The footer of the sidebar and the FAQ panel was rearranged and updated.

* The contents of the FAQ panel got a lot of small updates.

* Latest changes (this document) are now natively accessible from Tool 1.
  GitHub is no longer needed.

## Server Changes

* Many small tweaks and optimizations were introduced to ease the translation
  process.

* The Simplified Mode Inference Panel Module was renamed to Express Mode
  Inference Panel Module. Functions, scripts, and identifiers were changed
  accordingly.

* The `README` now contains clear instructions to follow when adding a new
  supported language.

* `ui_element()` is removed.

* `ui_link()` and `ui_link_mailto()` were rewritten (fixing a lof of bugs and
  problems that would arise when they were integrated into the source text with
  `html()`).

* All `server_panel_*()` functions now returns a `shiny::reactive()` object
  that can be called to get the panel's underlying title.

## Fixes

* Fixed a logical error in development script `find-text.R` that would discard
  existing translations stored in `intl/` without reusing them.

* Fixed some pieces of text that were not wrapped in `translate()` calls.

* Fixed rare annoying edge cases by forcing `html()` to explicitly convert
  elements passed to `...` to character strings instead of deferring this
  operation to `sprintf()`.

* Fixed a bug in the Exceedance Plot Sidebar Module where input
  `col_bg_threshold` was hidden for `plot3` (which was wrong).

* Fixed incorrect descriptions and labels in the Exceedance Plot Module.

# Tool 1 version `5.0.0`

This major version introduces Tool 1 Express (Simplified) directly into Tool 1.
The former was rewritten during the process. All existing functionalities were
preserved.

## User Visible Changes

* The user interface can now be customized via query parameters (included in
  the URL).

* The Title Bar was revamped bringing new modes: default or simplified.

* There is a new Statistical Panel for the new simplified mode.

* There is now additional information on the treatment of non-detects in the
  Frequently Asked Questions.

* There is now additional information on how Tool 1 can be used and how it
  works (behind the scenes) in the Frequently Asked Questions.

* The vocabulary used inside Estimates cards was shortened.

* Risk Band plots use the latest AIHA color scheme.

* Inference panels were renamed to Statistical Inference panels.

* French is temporarily removed from the list of officially supported languages.
  It will be reintroduced in the next version (`5.1.0`).

* Many superfluous external links were removed. We now try to only add links in
  the Frequently Asked Questions (unless it really matters elsewhere).

* Some icons used for decoration purposes only were removed. Others were
  standardized (their usage).

* Many smaller cosmetic changes were introduced (mostly for consistence and
  ease of use).

## Server Changes

* The logic of `server()` was revamped prior to the introduction of application
  modes, UI parameters, and the Simplified Mode Inference Panel Module.

* The Title Bar module was revamped following the introduction of *modes*.

* The Sidebar module was updated following the introduction of the Simplified
  Mode Inference Panel Module. Spaces between elements was standardized and
  buttons were reimplemented.

* Generic `<span>` and `<div>` containers used only to make the text **bold**
  or **italic** were replaced by tags such as `<em>` and `<strong>`. This
  simplifies the code.

* Various global constants were updated. Some were removed.

* Constants `default_simplified_inputs` and `aiha_risk_levels` were added. The
  latter introduces the new AIHA naming convention for discrete risk levels.
  All panels were updated accordingly. Notably, all `risk_assessment()` reactive
  values were rewritten. Observers and outputs that depend on them were updated.

* Various new helper functions were added following changes to the Title Bar
  and Sidebar modules. Some of them ease the management of the UI state.

* All calls to `translate()` were removed from observers and refactored into
  reactive values that can be cached with `shiny::bindCache()`. These values
  are passed to `update_*()` functions called inside `shiny::observe()`. This
  yields a significant performance boost. This was worth it even if working
  with a large number of `shiny::reactive()` calls can sometimes be cumbersome.

* More outputs are now cached with `shiny::bindCache()` to increase performance.

* Some inputs of the Sidebar module were renamed.

* The code was partially restyled. Notably, the usage of space was reduced
  to improve readability.

* Many new code comments and documentation block were added.

* Further smaller changes.

## Fixes

* Many grammar errors and typos were fixed.

* Percentages were incorrectly used as units in the Percentiles and Arithmetic
  Mean panels.

* The parent container of the Sidebar's warning card is now hidden as a whole
  when the Submit button is clicked (instead of just the card itself).

* Shiny's auto-reload feature was removed. Its broken state was breaking
  functionalities of Tool 1 in development environments.

* Various smaller bugs were fixed.

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

# Tool 1 version `4.0.0`

This major version is a complete redesign of Tool 1. It was entirely rewritten
except for core scientific back-end components. They were left nearly as is in
the source code.
