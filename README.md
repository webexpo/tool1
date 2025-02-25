# Tool 1: Data Interpretation for One Similarly Exposed Group

<!-- badges: start -->
[![Version](https://img.shields.io/badge/version-4.0.0--rc1-blue)](https://github.com/webexpo/app-tool1/releases/tag/v4.0.0-rc1)
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Location](https://img.shields.io/badge/live-shinyapps.io-5b90bf)](https://lavoue.shinyapps.io/tool1/)
<!-- badges: end -->

A Shiny application developed by [Jérôme Lavoué](https://orcid.org/0000-0003-4950-5475)
on behalf of the Industrial Hygiene team of the Department of Environmental and
Occupational Health at the
[School of Public Health](https://espum.umontreal.ca/english/home/) of the
[Université de Montréal](https://www.umontreal.ca/en/).

[Ununoctium](https://ununoctium.dev) (Jean-Mathieu Potvin) collaborated on the
development efforts (partial refactoring of the source code) that led to version
4.0.0.

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

## Methodological Background

The underlying Bayesian models and data interpretation procedures are derived
from best practices in industrial hygiene data interpretation techniques. They
are thoroughly described in
*[Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements](https://doi.org/10.1093/annweh/wxy100)*
(Annals of Work Exposures and Health, Volume 63, Issue 3, April 2019, Pages
267–279).

## Usage

To serve this application locally, call

```r
.run()
```

This is a shortcut to source file `.scripts/entrypoint.R`.

## Deploying to shinyapps.io

This application is deployed to and runs on
[shinyapps.io](https://lavoue.shinyapps.io/tool1). See file
`.scripts/publish-to-shinyapps.R` for more information.

4 environments variables are required to push new releases. They must be stored
in a top-level `.Renviron` file as shown below. This file is ignored by Git
and `rsconnect`.

```
# .Renviron
RSCONNECT_ACCOUNT_NAME=lavoue
RSCONNECT_ACCOUNT_TOKEN=<token>
RSCONNECT_ACCOUNT_SECRET=<secret>
APP_VISIBILITY_ON_SHINYAPPS=public
```

## Internationalization (`intl`)

> We are actively looking for external collaborators who could help us with
> supporting more languages. If you are interested, please send an e-mail
> to <jerome.lavoue@umontreal.ca>.

Tool 1 relies on package [transltr](https://cran.r-project.org/package=transltr)
to support multiple languages. Mechanisms are split among two scripts.

  * Actions that must be performed before runtime are in
    `.scripts/find-source-text.R`. Notably, this script is in charge of
    generating the contents of subdirectory `intl/`.
  * Objects required at runtime are defined in `R/intl.R`.

Further mechanisms not related to `transltr` (such as support for ordinal
numbers) are defined in `R/helpers.R`.

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).
