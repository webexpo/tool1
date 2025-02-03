# Shiny Inputs and Outputs

Tables below list all `shiny` identifiers (`inputId` and `outputId`) defined in
the application. Identifiers were revamped in version `4` for consistency and
easier maintenance. A mapping between old and new names is also provided.

Descriptions are included only when relevant.

## Naming conventions for I/O

There are 7 rules. They exist for consistency and readability.

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

   Some inputs may not have a location. In that case, the prefix can be
   omitted.

3. Identifiers should be intuitive and not use (too) many abbreviations.

4. Similar identifiers should adopt the same pattern. For example,
   `ef_seq_plot`, `pe_seq_plot`, and `am_seq_plot` refer to similar
   objects in different panels.

5. Identifiers must be in English.

6. Do not use words like `in`, `out`, `input`, `output`, etc. The underlying
   context always makes it very clear what it is: `shiny::plotOutput("my_plot")`,
   `output$my_plot`, `input$my_number`, etc.

7. Translations use a second prefix: `intl_` standing for internationalization.

## Inputs

| Location            | Generator                     | `inputId` (v4)                    | `inputId` (v3)       | Description |
| ------------------- | ----------------------------- | --------------------------------- | -------------------- | ----------- |
| None                | `shiny::fluidPage()`          | `lang`                            | None                 | None        |
| None                | `shiny::tabsetPanel()`        | `active_panel`                    | None                 | None        |
| Sidebar             | `shiny::numericInput()`       | `sb_oel`                          | `oel`                | None        |
| Sidebar             | `shiny::numericInput()`       | `sb_al`                           | `al`                 | None        |
| Sidebar             | `shiny::numericInput()`       | `sb_conf`                         | `conf`               | None        |
| Sidebar             | `shiny::numericInput()`       | `sb_psi`                          | `psi`                | None        |
| Sidebar             | `add_input_text_area()`       | `sb_data`                         | `data`               | None        |
| Sidebar             | `shiny::numericInput()`       | `sb_frac_threshold`               | `frac_threshold`     | None        |
| Sidebar             | `shiny::numericInput()`       | `sb_target_perc`                  | `target_perc`        | None        |
| Exceedance Fraction | `shiny::radioButtons()`       | `ef_exceed_plot_btn_variant`      | `varianteFracDep`    | None        |
| Exceedance Fraction | `shiny::actionButton()`       | `ef_exceed_plot_btn_custom`       | None                 | None        |
| Exceedance Fraction | `add_input_field_set()`       | `ef_exceed_plot_cols`             | None                 | None        |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_risk`         | `couleurRisque`      | None        |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_no_risk`      | `couleurAucunRisque` | None        |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_bg`           | `couleurFond`        | None        |
| Exceedance Fraction | `colourpicker::colourInput()` | `ef_exceed_plot_col_bg_threshold` | `couleurSeuil`       | None        |

## Outputs

| Location            | Generator              | `outputId` (v4)               | `outputId` (v3)                   | Description |
| ------------------- | ---------------------- | ----------------------------- | --------------------------------- | ----------- |
| Statistics          | `shiny::tableOutput()` | `st_stats_tbl`                | `res.desc`                        | None        |
| Statistics          | `shiny::plotOutput()`  | `st_qq_plot`                  | `qqplot`                          | None        |
| Statistics          | `shiny::plotOutput()`  | `st_box_plot`                 | `boxplot`                         | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_frac_threshold_percent_1` | `acceptableExpo1`                 | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_frac_threshold_percent_2` | `acceptableExpo2`                 | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_frac_threshold_percent_3` | `acceptableExpo3`                 | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_risk_prob_criterion`      | `probrisk`                        | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_risk_prob_limit_1`        | `frac.probSituUnacceptable1`      | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_risk_prob_limit_2`        | `frac.probSituUnacceptable2`      | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_risk_decision`            | `finalrisk`                       | None        |
| Exceedance Fraction | `shiny::plotOutput()`  | `ef_risk_meter_plot`          | `risquemetre`                     | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_estimate_geo_mean`        | `gm1`                             | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_estimate_geo_sd`          | `gsd1`                            | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_estimate`                 | `Frac`                            | None        |
| Exceedance Fraction | `shiny::plotOutput()`  | `ef_exceed_plot`              | `fracDepVariantes`                | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_exceed_plot_description`  | `fracDepVarianteDesc`             | None        |
| Exceedance Fraction | `shiny::plotOutput()`  | `ef_seq_plot`                 | `seqplot.frac`                    | None        |
| Exceedance Fraction | `shiny::plotOutput()`  | `ef_dist_plot`                | `distplot.frac`                   | None        |
| Exceedance Fraction | `shiny::plotOutput()`  | `ef_risk_band_plot`           | `riskband.frac`                   | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_good_exposure_percent_1`  | `frac.acceptableExpoDiv1`         | None        |
| Exceedance Fraction | `shiny::textOutput()`  | `ef_good_exposure_percent_2`  | `frac.acceptableExpoDiv2`         | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_sb_target_perc_ordinal_1` | `perc.percentile.risk.decision`   | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_sb_target_perc_ordinal_2` | `perc.percentile.param.estimates` | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_sb_target_perc_ordinal_3` | `perc.percentile.risk.band`       | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_risk_prob_criterion`      | `probrisk.perc`                   | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_risk_prob_limit_1`        | `perc.probSituUnacceptable1`      | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_risk_prob_limit_2`        | `perc.probSituUnacceptable2`      | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_risk_decision`            | `finalrisk.perc`                  | None        |
| Percentiles         | `shiny::plotOutput()`  | `pe_risk_meter_plot`          | `risquemetre2`                    | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_estimate_geo_mean`        | `gm2`                             | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_estimate_geo_sd`          | `gsd2`                            | None        |
| Percentiles         | `shiny::textOutput()`  | `pe_estimate`                 | `Perc`                            | None        |
| Percentiles         | `shiny::plotOutput()`  | `pe_seq_plot`                 | `seqplot.perc`                    | None        |
| Percentiles         | `shiny::plotOutput()`  | `pe_dist_plot`                | `distplot.perc`                   | None        |
| Percentiles         | `shiny::plotOutput()`  | `pe_risk_band_plot`           | `riskband.perc`                   | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_risk_prob_criterion`      | `probrisk.AM`                     | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_risk_prob_limit_1`        | `am.probSituUnacceptable1`        | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_risk_prob_limit_2`        | `am.probSituUnacceptable2`        | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_risk_decision`            | `finalrisk.AM`                    | None        |
| Arithmetic Mean     | `shiny::plotOutput()`  | `am_risk_meter_plot`          | `risquemetre.am`                  | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_estimate_geo_mean`        | `gm3`                             | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_estimate_geo_sd`          | `gsd3`                            | None        |
| Arithmetic Mean     | `shiny::textOutput()`  | `am_estimate`                 | `AM`                              | None        |
| Arithmetic Mean     | `shiny::plotOutput()`  | `am_seq_plot`                 | `seqplot.AM`                      | None        |
| Arithmetic Mean     | `shiny::plotOutput()`  | `am_dist_plot`                | `distplot.AM`                     | None        |
| Arithmetic Mean     | `shiny::plotOutput()`  | `am_risk_band_plot`           | `riskband.am`                     | None        |
