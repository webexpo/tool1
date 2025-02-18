# Shiny Inputs and Outputs

Tables below list all `shiny` identifiers (`inputId` and `outputId`) defined in
the application. Identifiers were revamped in version `4` for consistency and
easier maintenance. A mapping between old and new names is also provided.

Descriptions are included only when relevant.

## Naming conventions for I/O

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

| About               | `shiny::uiOutput()`           | `ab_tab_name`                     | None                              |
| About               | `shiny::uiOutput()`           | `ab_about_title`                  | None                              |
| About               | `shiny::uiOutput()`           | `ab_about`                        | None                              |
| About               | `shiny::uiOutput()`           | `ab_how_to_use_title`             | None                              |
| About               | `shiny::uiOutput()`           | `ab_how_to_use`                   | None                              |
| About               | `shiny::uiOutput()`           | `ab_metho_bg_title`               | None                              |
| About               | `shiny::uiOutput()`           | `ab_metho_bg`                     | None                              |

### `am_estim_am_mean`

The naming of this output is a bit odd. Interpret it backwards. This is the
point estimate of the arithmetic mean (`mean`) located in the subsubsection
*Arithmetic Mean* (`am`) of the *Parameter Estimates* subsection (`estim`)
of the *Arithmetic Mean* (`am`) panel.

This also holds for output `am_estim_am`.
