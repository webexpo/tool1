<div align="center">

<!-- Expostats' logo -->
<img src="www/android-chrome-192x192.png" alt="Expostats's logo" height="192" width="192" />

# Tool 1: Data Interpretation for One Similar Exposure Group (SEG)

<!-- badges: start -->
[![Version](https://img.shields.io/badge/version-5.2.0--beta-blue)](https://github.com/webexpo/app-tool1/releases/tag/v5.2.0)
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

`.run()` is defined in `.scripts/run.R`.

## Deploy

Tool 1 is deployed to [shinyapps.io](https://lavoue.shinyapps.io/tool1).
To deploy a new version, call `.pub()`.

```r
# Deploy a beta version (useful for testing purposes).
# It is publicly accessible at https://lavoue.shinyapps.io/tool1-beta/.
.pub()

# Deploy an official (production) version.
# It is publicly accessible at https://lavoue.shinyapps.io/tool1/.
.pub("prod")
```

`.pub()` is defined in `.scripts/publish.R`.

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

`.find()` is defined in `.scripts/find-text.R`.

Tool 1 further supports translation of ordinal numbers. Each supported
language requires an `ordinal_rules_<lang>()` function. For example, the
`ordinal_rules_english()` function implements grammar rules for English
ordinal numbers. See `R/helpers-translate.R` for more information.

### Working with translation files stored in `intl/`

All files are **required at runtime** and read by `transltr::translator_read()`
to create global constant `tr`.

#### Translator file

The `_translator.yml` file must **never be modified manually**. It should not
be shared with collaborators working on translations. Notably, developers may
consult it to locate translations in the source code.

#### Translation files

Further `<lang>.txt` files contain actual translations. These files are shared
with collaborators working on translations and must be edited manually (using
any text editor). They always include basic instructions to follow at all times.

`TRANSLATIONS.md` contains further instructions for working with translation
files.

### Placeholders

Tool 1 uses `sprintf()-`placeholders (conversion specifications beginning by
`%`) to dynamically insert HTML content into template strings. Tokens such as
`%s`, `%i`, and `%%` in the source text and translations **must be left as is**.
See `R/helpers-html.R` for more information.

### Adding new languages

Follow these steps to support a new language. Use the existing code as a
template to follow.

1. Implement a dedicated `ordinal_rules_<lang>()` function in
   `R/helpers-translate.R`.

2. Incorporate the function created at step (1) into `ordinal_rules()` in
   `R/helpers-translate.R`.

3. Add a new entry to formal argument `other_lang_names` of `.find()` in
   `.scripts/find-text.R`. Follow instructions contained in the script to
   do so properly.

4. Implement the required button and observer in `ui_title()` and
   `server_title()` respectively. Follow instructions contained in
   `R/ui-title.R` to do so (see subsection Languages).

   * You may add class `disabled` to the `shiny::actionButton()` created at
     step (4) to deactivate the language in the user interface until further
     notice. This is entirely optional.

5. Call `.find()`.

6. Share the `intl/<lang>.txt` file created at step (5) with collaborator(s)
   in charge of translating Tool 1.

   * Never share the `_translator.yml` file. The latter is only useful to
     developers.

7. Import the `intl/<lang>.txt` file updated at step (6) back into the source
   code. **Commit it.**

You may repeat steps (5) to (7) whenever required. Notably, they **must** be
repeated whenever the source text is changed (whenever a call to `translate()`
is either added or updated).

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

- Inputs lack robust validation mechanisms and may lead to undefined behavior
  in some cases.

- Accessibility mechanisms
  ([ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)) are
  not implemented.

- Margins, aspect ratios, font sizes, font families, dimensions, and colors of
  all plots are inconsistent and must be standardized.
  - This could be achieved with `shiny::getCurrentOutputInfo()` and
    `bslib::bs_current_theme()`.

- There are currently no explicit Terms of Service and Privacy Policy.

- Some usual `<meta>` tags are currently missing from the `<head>` of Tool 1.

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).

If you do not have a GitHub account, send an email to <jerome.lavoue@umontreal.ca>
**and** <jeanmathieupotvin@ununoctium.dev>. Please use the following standard
subject line:

```
Expostats [Tool 1] [Bug|Feedback]: <short description>
```
