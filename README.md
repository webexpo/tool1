# Tool1: Data Interpretation for One Similarly Exposed Group

<!-- badges: start -->
[![Lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

A Shiny application developed by the Industrial Hygiene team from the
Department of Environmental and Occupational Health at the
[School of Public Health](https://espum.umontreal.ca/english/home/) of the
[Université de Montréal](https://www.umontreal.ca/en/).

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

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).
