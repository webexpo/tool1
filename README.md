<div align="center">

<!-- Expostats' logo -->
<img src="www/android-chrome-192x192.png" alt="Expostats's logo" height="192" width="192" />

# Tool 1: Data Interpretation for One Similar Exposure Group (SEG)

<!-- badges: start -->
[![Version](https://img.shields.io/badge/version-5.0.0-blue)](https://github.com/webexpo/app-tool1/releases/tag/v5.0.0)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Location](https://img.shields.io/badge/live-shinyapps.io-5b90bf)](https://lavoue.shinyapps.io/tool1/)
[![License](https://img.shields.io/badge/license-MIT-orange.svg)](https://github.com/webexpo/tool1/blob/main/LICENSE.md)
<!-- badges: end -->

</div>

A Shiny application developed by the Industrial Hygiene team of the Department
of Environmental and Occupational Health at the
[École de Santé Publique](https://espum.umontreal.ca/english/home/) of the
[Université de Montréal](https://www.umontreal.ca/en/) (EPSUM) in collaboration
with [Ununoctium](https://ununoctium.dev) (Jean-Mathieu Potvin).
[Ununoctium](https://ununoctium.dev) designed the user interface (for versions
greater than or equal to `4.0.0`) and currently maintains the web application
(but not scripts stored in `scripts/`).

## Introduction

This tool interprets a dataset of exposure measurements (including non detects)
with regards to an OEL (Occupational Exposure Limit). In addition to multiple
illustrative graphs, it exposes five components.

1. Goodness of fit with respect to the lognormal model (graphical evaluation).
2. Descriptive statistics.
3. Risk assessment based on an exceedance fraction.
4. Risk assessment based on percentiles.
5. Risk assessment based on the arithmetic mean.

Calculations are performed using a Bayesian model fit using a Monte Carlo
Markov Chain (MCMC) engine. It assumes that the underlying exposure distribution
is lognormal.

The underlying Bayesian models and data interpretation procedures are derived
from best practices in industrial hygiene data interpretation techniques. They
are thoroughly described in
*[Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements](https://doi.org/10.1093/annweh/wxy100)*
(Annals of Work Exposures and Health, Volume 63, Issue 3, April 2019, Pages
267–279).

## Requirements

R version `4.4.0` is required to work on and serve Tool 1 locally. To be
completed.

## Usage

To serve Tool 1 locally, call

```r
.run()
```

This sources `.scripts/run.R`. See this script for details.

## Deploy

Tool 1 is deployed to [shinyapps.io](https://lavoue.shinyapps.io/tool1).
To deploy a new version, call `.pub()`.

```r
# Deploy a beta version (useful for testing purposes).
# It is publicly accessible at https://lavoue.shinyapps.io/tool1-beta/.
.pub()

# Deploy an official version.
# It is publicly accessible at https://lavoue.shinyapps.io/tool1/.
.pub("prod")
```

This sources `.scripts/publish.R`. See this script for details.

Some environment variables are required to publish new releases with `.pub()`.
They must be stored in a top-level `.Renviron` file as shown below. This file is
ignored by Git and `rsconnect`.

```
# .Renviron
RSCONNECT_ACCOUNT_NAME=<account>
RSCONNECT_ACCOUNT_TOKEN=<token>
RSCONNECT_ACCOUNT_SECRET=<secret>
```

## Internationalization

> We are actively looking for external collaborators who could help us with
> supporting more languages. If you are interested, please send an e-mail
> to <jerome.lavoue@umontreal.ca>.

Tool 1 relies on package [transltr](https://cran.r-project.org/package=transltr)
to support multiple languages. Translations (and related metadata) are stored
in `intl/`. To update its content, call

```r
.find()
```

This sources `.scripts/find-text.R` and updates all underlying
translations files. See that script for details.

Tool 1 further supports translation of ordinal numbers. Each supported
language requires an `ordinal_rules_<lang>()` function. For example, the
`ordinal_rules_english()` function implements grammar rules for English
ordinal numbers. See `R/helpers-translate.R` for more information.

### Working with tranlations files stored in `intl/`

To be completed.

### Warning

Package [transltr](https://cran.r-project.org/package=transltr) currently lacks
a proper way to (intelligently and incrementally) update existing translations
files. This feature will (likely) be released soon. Until further notice,
updating `intl/` should only be done by
[Jean-Mathieu Potvin](https://github.com/jeanmathieupotvin).

### Placeholders

Tool 1 uses `sprintf()-`placeholders (conversion specifications beginning by
`%`) to dynamically insert HTML content into template strings. Tokens such as
`%s`, `%i`, and `%%` in the source text and translations **must be left as is**.
See `R/helpers-html.R` for more information.

## Styling

Tool 1 uses as few custom CSS (Cascading Style Sheets) rules as possible. It
does so by maximizing usage of `bslib` features and predefined
[Bootstrap 5](https://getbootstrap.com/docs/5.3/getting-started/introduction/)
CSS classes. Any CSS rule not declared in file `www/main.css` is highly likely
to stem from [Bootstrap 5](https://getbootstrap.com/docs/5.3/getting-started/introduction/).
This is a well-known and well-maintained framework that can be trivially
understood and leveraged.

It is worthwhile to note that it is **not** necessary to include
[Bootstrap's assets](https://getbootstrap.com/docs/5.3/getting-started/download/#cdn-via-jsdelivr)
(in the `<head>` of Tool 1) because `bslib` already does that automatically.

## Known Issues

We may work on these issues in a near future.

- French translations are officially supported but missing [COMING SOON].

- Inputs lack robust validation mechanisms and may lead to undefined behavior
  in some cases.

- Accessibility mechanisms
  ([ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)) are
  not implemented.

- Margins, aspect ratios, font sizes, font families, dimensions, and colors of
  all plots are inconsistent and must be standardized.
  - This could be achieved with `shiny::getCurrentOutputInfo()` and
    `bslib::bs_current_theme()`.

- In the source text, many inputs have slightly different names. For example,
  OEL (Occupational Exposure Limit) is sometimes named EL (Exposure Limit).

- There are currently no explicit Terms of Service and Privacy Policy.

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).

If you do not have a GitHub account, send an email to <jerome.lavoue@umontreal.ca>
**and** <jeanmathieupotvin@ununoctium.dev>. Please use the following standard
subject line:

```
Expostats [Tool 1] [Bug|Feedback]: <short description>
```
