# Shiny Inputs and Outputs

Tables below list all `shiny` identifiers (`inputId` and `outputId`) defined in
the application. Identifiers were revamped in version `4` for consistency and
easier maintenance. A mapping between old and new names is also provided.

Further components such as HTML tags and `bslib` components may require an `id`.
These are not `inputId` or `outputId` *per se*, but the same naving conventions
are used for them too. They are listed below.

## Naming Conventions

There are 6 rules. They exist for consistency and readability.

1. All identifier must be written in `snake_case` (lower case letters with
   spaces replaced by an underscore).

2. Identifiers must begin by a standardized prefix stating where they are
   located in the application:

    * `sb_`: Sidebar,
    * `st_`: panel Statistics,
    * `ef_`: panel Exceedance Fraction,
    * `pe_`: panel Percentiles,
    * `am_`: panel Arithmetic Mean, and
    * `ab_`: panel About.

   Some inputs and outputs may not be located in these *areas*. In that case,
   the prefix can be omitted. Good examples of this are `title` and `top_title`,
   the window's title and the application's main title, respectively.

3. Identifiers should be intuitive and not use (too) many abbreviations.

4. Similar identifiers should adopt the same pattern. For example,
   `ef_seq_plot`, `pe_seq_plot`, and `am_seq_plot` refer to similar
   objects in different panels.

5. Identifiers must be in English.

6. Do not use words like `in`, `out`, `input`, `output`, etc. The underlying
   context always makes it very clear what it is: `shiny::plotOutput("my_plot")`,
   `output$my_plot`, `input$my_number`, etc.

## Inputs

| Location            | Generator                     | `inputId` (v4)                    | `inputId` (v3)                    |
| ------------------- | ----------------------------- | --------------------------------- | --------------------------------- |
| *Body*              | `shiny::tabsetPanel()`        | `active_panel`                    | None                              |
| Sidebar             | `shiny::selectInput()`        | `lang`                            | None                              |
| Sidebar             | `shiny::numericInput()`       | `oel`                             | `oel`                             |
| Sidebar             | `shiny::numericInput()`       | `al`                              | `al`                              |
| Sidebar             | `shiny::numericInput()`       | `conf`                            | `conf`                            |
| Sidebar             | `shiny::numericInput()`       | `psi`                             | `psi`                             |
| Sidebar             | `shiny::textAreaInput()`      | `data`                            | `data`                            |
| Sidebar             | `shiny::numericInput()`       | `frac_threshold`                  | `frac_threshold`                  |
| Sidebar             | `shiny::numericInput()`       | `target_perc`                     | `target_perc`                     |
| Exceedance Fraction | `shiny::radioButtons()`       | `ef_exceed_plot_btn_choose`       | `varianteFracDep`                 |
| Exceedance Fraction | `shiny::actionButton()`       | `ef_exceed_plot_btn_customize`    | None                              |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_risk`         | `couleurRisque`                   |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_no_risk`      | `couleurAucunRisque`              |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_bg`           | `couleurFond`                     |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_bg_threshold` | `couleurSeuil`                    |

### Sidebar's inputs

Since these inputs are used everywhere in the application, the usual `sb_`
prefix is omitted. Other specific inputs always have a prefix.

## Outputs

| Location            | Generator                     | `outputId` (v4)                   | `outputId` (v3)                   |
| ------------------- | ----------------------------- | --------------------------------- | --------------------------------- |
| *None*              | `shiny::fluidPage()`          | `lang`                            | None                              |
| *None*              | `shiny::fluidPage()`          | `title`                           | None                              |
| *Body*              | `shiny::uiOutput()`           | `top_title`                       | None                              |
| *Body*              | `shiny::uiOutput()`           | `top_banner`                      | None                              |
| Sidebar             | `bslib::tooltip()`            | `lang_tooltip`                    | None                              |
| Sidebar             | `bslib::tooltip()`            | `oel_tooltip`                     | None                              |
| Sidebar             | `bslib::tooltip()`            | `al_tooltip`                      | None                              |
| Sidebar             | `bslib::tooltip()`            | `conf_tooltip`                    | None                              |
| Sidebar             | `bslib::tooltip()`            | `psi_tooltip`                     | None                              |
| Sidebar             | `bslib::tooltip()`            | `data_tooltip`                    | None                              |
| Sidebar             | `bslib::tooltip()`            | `frac_threshold_tooltip`          | None                              |
| Sidebar             | `bslib::tooltip()`            | `target_perc_tooltip`             | None                              |
| Sidebar             | `shiny::uiOutput()`           | `sb_footer_app_version`           | None                              |
| Sidebar             | `shiny::uiOutput()`           | `sb_footer_copyright`             | None                              |
| Statistics          | `shiny::uiOutput()`           | `st_tab_name`                     | None                              |
| Statistics          | `shiny::uiOutput()`           | `st_desc_stats_title`             | None                              |
| Statistics          | `shiny::uiOutput()`           | `st_desc_stats_subtitle`          | None                              |
| Statistics          | `shiny::tableOutput()`        | `st_desc_stats_tbl`               | `res.desc`                        |
| Statistics          | `shiny::uiOutput()`           | `st_desc_stats_alert_info`        | None                              |
| Statistics          | `shiny::uiOutput()`           | `st_qq_title`                     | None                              |
| Statistics          | `shiny::plotOutput()`         | `st_qq_plot`                      | `qqplot`                          |
| Statistics          | `shiny::uiOutput()`           | `st_qq_desc`                      | None                              |
| Statistics          | `shiny::uiOutput()`           | `st_box_title`                    | None                              |
| Statistics          | `shiny::plotOutput()`         | `st_box_plot`                     | `boxplot`                         |
| Statistics          | `shiny::uiOutput()`           | `st_box_desc`                     | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_tab_name`                     | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_risk_decision_title`          | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_risk_decision_subtitle`       | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_risk_decision`                | None                              |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_decision_frac`           | `acceptableExpo1`                 |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_decision_criterion`      | `probrisk`                        |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_decision_limit`          | `frac.probSituUnacceptable1`      |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_decision_conclusion`     | `finalrisk`                       |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_risk_meter_desc`              | None                              |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_risk_meter_plot`              | `risquemetre`                     |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_estim_title`                  | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_estim`                        | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_estim_dist_title`             | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_estim_dist`                   | None                              |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_estim_dist_geo_mean`          | `gm1`                             |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_estim_dist_geo_sd`            | `gsd1`                            |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_estim_ef_title`               | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_estim_ef`                     | None                              |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_estim_ef_frac`                | `Frac`                            |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_exceed_title`                 | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_exceed`                       | None                              |
| Exceedance Fraction | `htmltools::fieldset()`       | `ef_exceed_cols`                  | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_exceed_cols_label`            | None                              |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_exceed_plot`                  | `fracDepVariantes`                |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_exceed_desc_sub_plot`         | `fracDepVarianteDesc`             |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_seq_title`                    | None                              |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_seq_plot`                     | `seqplot.frac`                    |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_seq_desc`                     | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_dist_title`                   | None                              |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_dist_plot`                    | `distplot.frac`                   |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_dist_desc`                    | None                              |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_risk_band_title`              | None                              |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_risk_band_plot`               | `riskband.frac`                   |
| Exceedance Fraction | `shiny::uiOutput()`           | `ef_risk_band_desc`               | None                              |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_band_desc_low_val_1`     | `frac.acceptableExpoDiv1`         |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_band_desc_low_val_2`     | `frac.acceptableExpoDiv2`         |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_band_desc_high_val_1`    | `acceptableExpo2`                 |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_band_desc_high_val_2`    | `acceptableExpo3`                 |
| Percentiles         | `shiny::uiOutput()`           | `pe_tab_name`                     | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_risk_decision_title`          | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_risk_decision_subtitle`       | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_risk_decision`                | None                              |
| Percentiles         | `shiny::textOutput()`         | `pe_risk_decision_perc`           | `percentile5`                     |
| Percentiles         | `shiny::textOutput()`         | `pe_risk_decision_criterion`      | `probrisk.perc`                   |
| Percentiles         | `shiny::textOutput()`         | `pe_risk_decision_limit`          | `probSituUnacceptable1`           |
| Percentiles         | `shiny::textOutput()`         | `pe_risk_decision_conclusion`     | `finalrisk.perc`                  |
| Percentiles         | `shiny::uiOutput()`           | `pe_risk_meter_desc`              | None                              |
| Percentiles         | `shiny::plotOutput()`         | `pe_risk_meter_plot`              | `risquemetre2`                    |
| Percentiles         | `shiny::uiOutput()`           | `pe_estim_title`                  | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_estim`                        | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_estim_dist_title`             | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_estim_dist`                   | None                              |
| Percentiles         | `shiny::textOutput()`         | `pe_estim_dist_geo_mean`          | `gm2`                             |
| Percentiles         | `shiny::textOutput()`         | `pe_estim_dist_geo_sd`            | `gsd2`                            |
| Percentiles         | `shiny::uiOutput()`           | `pe_estim_pe_title`               | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_estim_pe`                     | None                              |
| Percentiles         | `shiny::textOutput()`         | `pe_estim_pe_perc`                | `Perc`                            |
| Percentiles         | `shiny::uiOutput()`           | `pe_seq_title`                    | None                              |
| Percentiles         | `shiny::plotOutput()`         | `pe_seq_plot`                     | `seqplot.perc`                    |
| Percentiles         | `shiny::uiOutput()`           | `pe_seq_desc`                     | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_dist_title`                   | None                              |
| Percentiles         | `shiny::plotOutput()`         | `pe_dist_plot`                    | `distplot.perc`                   |
| Percentiles         | `shiny::uiOutput()`           | `pe_dist_desc`                    | None                              |
| Percentiles         | `shiny::uiOutput()`           | `pe_risk_band_title`              | None                              |
| Percentiles         | `shiny::plotOutput()`         | `pe_risk_band_plot`               | `riskband.perc`                   |
| Percentiles         | `shiny::uiOutput()`           | `pe_risk_band_desc`               | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_tab_name`                     | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_decision_title`          | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_decision_subtitle`       | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_decision`                | None                              |
| Arithmetic Mean     | `shiny::textOutput()`         | `am_risk_decision_criterion`      | `probrisk.AM`                     |
| Arithmetic Mean     | `shiny::textOutput()`         | `am_risk_decision_limit`          | `probSituUnacceptable2`           |
| Arithmetic Mean     | `shiny::textOutput()`         | `am_risk_decision_conclusion`     | `finalrisk.AM`                    |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_decision_alert_warn`     | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_meter_desc`              | None                              |
| Arithmetic Mean     | `shiny::plotOutput()`         | `am_risk_meter_plot`              | `risquemetre.am`                  |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_estim_title`                  | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_estim`                        | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_estim_dist_title`             | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_estim_dist`                   | None                              |
| Arithmetic Mean     | `shiny::textOutput()`         | `am_estim_dist_geo_mean`          | `gm3`                             |
| Arithmetic Mean     | `shiny::textOutput()`         | `am_estim_dist_geo_sd`            | `gsd3`                            |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_estim_am_title`               | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_estim_am`                     | None                              |
| Arithmetic Mean     | `shiny::textOutput()`         | `am_estim_am_mean`                | `AM`                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_seq_title`                    | None                              |
| Arithmetic Mean     | `shiny::plotOutput()`         | `am_seq_plot`                     | `seqplot.AM`                      |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_seq_desc`                     | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_dist_title`                   | None                              |
| Arithmetic Mean     | `shiny::plotOutput()`         | `am_dist_plot`                    | `distplot.AM`                     |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_dist_desc`                    | None                              |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_band_title`              | None                              |
| Arithmetic Mean     | `shiny::plotOutput()`         | `am_risk_band_plot`               | `riskband.am`                     |
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_band_desc`               | None                              |
| About               | `shiny::uiOutput()`           | `ab_tab_name`                     | None                              |
| About               | `shiny::uiOutput()`           | `ab_about_title`                  | None                              |
| About               | `shiny::uiOutput()`           | `ab_about`                        | None                              |
| About               | `shiny::uiOutput()`           | `ab_how_to_use_title`             | None                              |
| About               | `shiny::uiOutput()`           | `ab_how_to_use`                   | None                              |
| About               | `shiny::uiOutput()`           | `ab_metho_bg_title`               | None                              |
| About               | `shiny::uiOutput()`           | `ab_metho_bg`                     | None                              |

### `title` and `lang`

These two outputs are special because they are attributes of the web page
created by `shiny::fluidPage()`. They are initialized as `NULL` values in
the R object `ui` and later updated via Shiny custom messages whenever
**input** `lang` changes.

```r
# This code block is extracted from server() defined in app.R.
shiny::observeEvent(input$lang, {
    lang  <- input$lang
    title <- sprintf("Expostats: %s", translate("Tool 1"))

    # ...

    # See www/main.js for more information.
    session$sendCustomMessage("update_page_lang", lang)
    session$sendCustomMessage("update_window_title", title)

    # ...
})
```

Related JS custom handler functions for these two messages are defined in
script `www/main.js`. Sadly, JS must be used to update these values from the
server's side.

### Identifiers Following the `*_estim_*_*` Naming Pattern

The names of outputs like `am_estim_am`, `am_estim_am_mean`, `pe_estim_pe`,
`pe_estim_pe_perc` and many others is odd at first glance.

Interpret them backwards. For example, `am_estim_am_mean` is the
point estimate of the arithmetic mean (`mean`) located in the subsubsection
*Arithmetic Mean* (`am`) of the *Parameter Estimates* subsection (`estim`)
of the *Arithmetic Mean* (`am`) panel.
