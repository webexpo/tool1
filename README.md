# Tool1: Data Interpretation for One Similarly Exposed Group

<!-- badges: start -->
[![Version](https://img.shields.io/badge/version-1.0.0-blue)](https://github.com/webexpo/app-tool1/releases/tag/v1.0.0)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Location](https://img.shields.io/badge/live-shinyapps.io-5b90bf)](https://lavoue.shinyapps.io/Tool1v3En/)
<!-- badges: end -->

A Shiny application developed by the Industrial Hygiene team from the
Department of Environmental and Occupational Health at the
[School of Public Health](https://espum.umontreal.ca/english/home/) of the
[Université de Montréal](https://www.umontreal.ca/en/).

## Disclaimer

[Jean-Mathieu Potvin](https://github.com/jeanmathieupotvin)
([Ununoctium, coding simplified](https://ununoctium.dev/en)) is currently
working on (partially) refactoring this project. The objective is threefold:

1. Integrate package [transltr](https://cran.r-project.org/package=transltr)
   and use it as its internationalization engine.
2. Restructure code and make it easier to maintain (including translations).
3. Fix bugs, inconsistencies and enforce R best practices, including
   [Google's R Style Guide](https://google.github.io/styleguide/Rguide.html)
   (whenever possible).

The scripts stored in `scripts/` are **out of scope** of this refactoring
effort.

## Introduction

This tool interprets a dataset of exposure measurements (including non detects)
with regards to an OEL (Occupational Exposure Limit). In addition to multiple
illustrative graphs, it exposes five components.

1. Goodness of fit with respect to the lognormal model (graphical evaluation).
2. Descriptive statistics.
3. Risk assessment based on exceedance of the OEL.
4. Risk assessment based on the 95<sup>th</sup> percentile.
5. Risk assessment based on the arithmetic mean.

Calculations are performed using a Bayesian model fit using a Monte Carlo
Markov Chain (MCMC) engine. It assumes that the underlying exposure distribution
is lognormal.

## Scientific Basis

The underlying Bayesian models and data interpretation procedures are derived
from best practices in industrial hygiene data interpretation techniques. They
are thoroughly described in
[Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements](https://doi.org/10.1093/annweh/wxy100)
(Annals of Work Exposures and Health, Volume 63, Issue 3, April 2019, Pages
267–279).

Further references are available on
[expostats.ca](https://www.expostats.ca/site/en/info.html).

## Usage

To serve this application locally, run the following lines of code in a fresh
R session.

```r
# Further arguments may be passed to runApp().
shiny::runApp(port = 3090L, launch.browser = FALSE)
```

You may use any other `port` value (aside from standard unsafe ports `3659`,
`4045`, `5060`, `5061`, `6000`, `6566`, and `6665:6669`).

### Deploying to shinyapps.io

This application is deployed to and runs on
[shinyapps.io](https://lavoue.shinyapps.io/Tool1v3En/). Use this code snippet
to automatically upload a new version.

```r
# To be determined.
```

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).
