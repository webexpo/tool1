# Shiny Inputs and Outputs

Tables below list all `shiny` identifiers (`inputId` and `outputId`) defined in
the application. Identifiers were revamped in version `4` for consistency and
easier maintenance. A mapping between old and new names is also provided.

Descriptions are included only when relevant.

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
   the prefix can be omitted. A good example is `title`, the maint title of
   the application.

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
| None                | `shiny::fluidPage()`          | `lang`                            | None                              |
| None                | `shiny::tabsetPanel()`        | `active_panel`                    | None                              |
| Sidebar             | `shiny::numericInput()`       | `sb_oel`                          | `oel`                             |
| Sidebar             | `shiny::numericInput()`       | `sb_al`                           | `al`                              |
| Sidebar             | `shiny::numericInput()`       | `sb_conf`                         | `conf`                            |
| Sidebar             | `shiny::numericInput()`       | `sb_psi`                          | `psi`                             |
| Sidebar             | `add_input_text_area()`       | `sb_data`                         | `data`                            |
| Sidebar             | `shiny::numericInput()`       | `sb_frac_threshold`               | `frac_threshold`                  |
| Sidebar             | `shiny::numericInput()`       | `sb_target_perc`                  | `target_perc`                     |
| Exceedance Fraction | `shiny::radioButtons()`       | `ef_exceed_plot_btn_variant`      | `varianteFracDep`                 |
| Exceedance Fraction | `shiny::actionButton()`       | `ef_exceed_plot_btn_custom`       | None                              |
| Exceedance Fraction | `add_input_field_set()`       | `ef_exceed_plot_cols`             | None                              |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_risk`         | `couleurRisque`                   |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_no_risk`      | `couleurAucunRisque`              |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_bg`           | `couleurFond`                     |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_bg_threshold` | `couleurSeuil`                    |

## Outputs

| Location            | Generator                     | `outputId` (v4)                   | `outputId` (v3)                   |
| ------------------- | ----------------------------- | --------------------------------- | --------------------------------- |
| Statistics          | `shiny::tableOutput()`        | `st_stats_tbl`                    | `res.desc`                        |
| Statistics          | `shiny::plotOutput()`         | `st_qq_plot`                      | `qqplot`                          |
| Statistics          | `shiny::plotOutput()`         | `st_box_plot`                     | `boxplot`                         |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_sb_frac_threshold_percent_1`  | `acceptableExpo1`                 |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_sb_frac_threshold_percent_2`  | `acceptableExpo2`                 |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_sb_frac_threshold_percent_3`  | `acceptableExpo3`                 |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_prob_criterion`          | `probrisk`                        |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_prob_limit_1`            | `frac.probSituUnacceptable1`      |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_prob_limit_2`            | `frac.probSituUnacceptable2`      |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_risk_decision`                | `finalrisk`                       |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_risk_meter_plot`              | `risquemetre`                     |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_estimate_geo_mean`            | `gm1`                             |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_estimate_geo_sd`              | `gsd1`                            |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_estimate`                     | `Frac`                            |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_exceed_plot`                  | `fracDepVariantes`                |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_exceed_plot_description`      | `fracDepVarianteDesc`             |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_seq_plot`                     | `seqplot.frac`                    |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_dist_plot`                    | `distplot.frac`                   |
| Exceedance Fraction | `shiny::plotOutput()`         | `ef_risk_band_plot`               | `riskband.frac`                   |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_good_exposure_percent_1`      | `frac.acceptableExpoDiv1`         |
| Exceedance Fraction | `shiny::textOutput()`         | `ef_good_exposure_percent_2`      | `frac.acceptableExpoDiv2`         |
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
| Arithmetic Mean     | `shiny::uiOutput()`           | `am_risk_decision_alert`          | None                              |
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

### Identifiers Following the `*_estim_*_*` Pattern

The names of outputs like `am_estim_am`, `am_estim_am_mean`, `pe_estim_pe`,
`pe_estim_pe_perc` and many others is odd at first glance.

Interpret them backwards. For example, `am_estim_am_mean` is the
point estimate of the arithmetic mean (`mean`) located in the subsubsection
*Arithmetic Mean* (`am`) of the *Parameter Estimates* subsection (`estim`)
of the *Arithmetic Mean* (`am`) panel.
