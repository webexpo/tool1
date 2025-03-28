<!-- Logo: start -->
<p align="center"><img src="www/android-chrome-192x192.png" alt="Expostats's logo" height="192" width="192" /></p>
<!-- Logo: end -->

# Tool 1: Data Interpretation for One Similarly Exposed Group

<!-- badges: start -->
[![Version](https://img.shields.io/badge/version-4.0.0-blue)](https://github.com/webexpo/app-tool1/releases/tag/v4.0.0)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Location](https://img.shields.io/badge/live-shinyapps.io-5b90bf)](https://lavoue.shinyapps.io/tool1/)
<!-- badges: end -->

A Shiny application developed by the Industrial Hygiene team of the Department
of Environmental and Occupational Health at the
[École de Santé Publique](https://espum.umontreal.ca/english/home/) of the
[Université de Montréal](https://www.umontreal.ca/en/) (EPSUM) in collaboration
with [Ununoctium](https://ununoctium.dev) (Jean-Mathieu Potvin).
[Ununoctium](https://ununoctium.dev) designed the user interface and currently
maintains the web application (but not scripts stored in `scripts/`).

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

## Usage

To launch and serve Tool 1 locally, call

```r
.run()
```

This sources `.scripts/run.R`. See this script for details.

## Deploy

Tool 1 is deployed to and runs on [shinyapps.io](https://lavoue.shinyapps.io/tool1).
To deploy a new version, call

```r
.pub()
```

This sources `.scripts/publish.R`. See this script for details.

Some environments variables are required to publish new releases with `.pub()`.
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
in `intl/`. To updates its content, call

```r
.intl()
```

This sources `.scripts/find-text.R` and updates all underlying
translations files. See this script for details.

Tool 1 further supports translation of ordinal numbers. Each supported
language requires an `ordinal_rules_*()` function. For example, English
has its own  `ordinal_rules_english()` function implementing grammar
rules for English ordinal numbers. See `R/helpers-translate.R` for more
information.

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

## Known Issues

We may work on these issues in a near future.

- French translations are officially supported but missing.

- Inputs lack robust validation mechanisms and may lead to undefined behavior.

- Application is not optimized for mobile phones and small screens (<500px).

- Accessibility mechanisms
  ([ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)) are
  not implemented.

- Margins, aspect ratios, font sizes, font families, dimensions, and colors of
  all plots are inconsistent and must be standardized.
  - This could be achieved with `shiny::getCurrentOutputInfo()`.

- In the source text, many inputs have multiple slightly different names. For
  example, OEL (Occupational Exposure Limit) is sometimes named EL (Exposure
  Limit).

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).

If you do not have an account, send an email to <jerome.lavoue@umontreal.ca>
**and** <jeanmathieupotvin@ununoctium.dev>. Please use the following standard
subject line:

```
Expostats [Tool 1] [Bug|Feedback]: <short description>
```
