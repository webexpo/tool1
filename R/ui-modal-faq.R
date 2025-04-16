#' Frequently Asked Questions Modal Module
#'
#' @description
#' This module controls the Frequently Asked Questions modal nested into the
#' Title module.
#'
#' ```
#' -----------------------------------------------------------
#' | Title                   --------  -----------------     |
#' |                         |      |  | Button (?)    |     |
#' |                         |      |  | (this module) | ... |
#' |                         --------  -----------------     |
#' -----------------------------------------------------------
#' | Sidebar | Main                                          |
#' |         |  -------------------------------------------- |
#' |         |  | Panels Navigation                        | |
#' |         |  -------------------------------------------- |
#' |         |  | Active Panel                             | |
#' |         |  |                                          | |
#' |         |  |     ---------------------------------    | |
#' |         |  |     | Modal (this module)           |    | |
#' |         |  |     | Shown when button is clicked  |    | |
#' |         |  |     | (on top of other elements)    |    | |
#' |         |  |     ---------------------------------    | |
#' |         |  |                                          | |
#' |         |  -------------------------------------------- |
#' -----------------------------------------------------------
#' ```
#'
#' The button displays and hides an hidden container. When shown, it is
#' shown at the center of the page on top of other elements.
#'
#' @details
#' This module implicitly relies on values defined in `R/global.R` and
#' `R/helpers*.R` scripts. They are sourced by [shiny::runApp()].
#'
#' ## Modals
#'
#' A modal is a child window layered on top of the main user interface. While
#' the latter is still visible, users must interact with the former to return
#' to the parent window.
#'
#' The modal's body is a [bslib::navset_bar()]. Doing so requires fixing some
#' CSS rules defined by bslib (for a smooth integration). This is implemented
#' in CSS class `app-navset-bar-fix` defined in www/main.css.
#'
#' ## Accordions
#'
#' Questions are presented to users as [bslib::accordion()] objects. What these
#' accordions contain is static text that only depends on `lang`. To preserve
#' as much space as possible in the source code and separate this content from
#' the logic of [server_modal_faq()], each [bslib::accordion()] is isolated in
#' its own helper function.
#'
#' @template param-id
#'
#' @param lang Usage depends on the underlying function.
#'
#'   * [ui_modal_faq()] and [server_modal_faq()] expect a [shiny::reactive()]
#'     object returning the current language.
#'
#'   * [ui_panel_intro_accordion()],
#'     [ui_panel_parameters_accordion()],
#'     [ui_panel_usage_accordion()], and
#'     [ui_panel_metho_accordion()] expect a character string. The underlying
#'     language code.
#'
#' @returns
#' [ui_modal_faq()] returns a `shiny.tag` object.
#'
#' [server_modal_faq()] returns `NULL`, invisibly.
#'
#' [ui_panel_intro_accordion()],
#' [ui_panel_parameters_accordion()],
#' [ui_panel_usage_accordion()], and
#' [ui_panel_metho_accordion()] return a `bslib_fragment` object stemming from
#' [bslib::accordion()].
#'
#' @seealso
#' [Bootstrap Modals](https://getbootstrap.com/docs/5.3/components/modal/),
#' [Bootstrap Accordions](https://getbootstrap.com/docs/5.3/components/accordion/),
#' [Bootstrap List Groups](https://getbootstrap.com/docs/5.3/components/list-group/)
#' [bslib::accordion()]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-modal-faq
#'
#' @export
ui_modal_faq <- function(id) {
    ns <- shiny::NS(id)
    modal_id <- ns("modal")

    # Button that shows the modal.
    # bslib::tooltip() sets attribute "data-bs-toggle"
    # on the target element. If this is called on the
    # button directly, it overwrites "data-bs-toggle"
    # of the modal (there can only be one toggle event
    # per element). The solution is to separate these
    # two events by setting the modal toggle target in
    # an outer container.
    btn_open <- tags$div(
        "data-bs-toggle" = "modal",
        "data-bs-target" = paste0("#", modal_id),

        tags$button(
            class = "nav-link",
            type  = "button",

            tags$span(
                class = "pe-1",
                bsicons::bs_icon("info-circle-fill", a11y = "sem")
            ),

            shiny::textOutput(ns("btn_open_text"), tags$span)
        )
    )

    # data-bs-dismiss must not be equal to
    # modal_id according to Bootstrap. This
    # is a little weird, but this value is
    # used to id the currently opened modal.
    # Only shown on larger screens (>= 992px).
    btn_close <- tags$button(
        class             = "btn btn-outline-secondary app-btn",
        type              = "button",
        "data-bs-dismiss" = "modal",
        bsicons::bs_icon("x-lg", a11y = "sem")
    )

    panel_intro <- bslib::nav_panel(
        value = ns("panel_intro"),
        title = shiny::textOutput(ns("panel_intro_title"), tags$span),
        shiny::uiOutput(ns("panel_intro_accordion"))
    )

    panel_parameters <- bslib::nav_panel(
        value = ns("panel_parameters"),
        title = shiny::textOutput(ns("panel_parameters_title"), tags$span),

        bslib::card(
            id    = ns("panel_parameters_warning"),
            class = "border-warning bg-warning-subtle text-center mx-5",
            fill  = FALSE,

            bslib::card_body(
                shiny::textOutput(ns("panel_parameters_warning_text"), tags$span)
            )
        ),

        shiny::uiOutput(ns("panel_parameters_accordion"))
    )

    panel_usage <- bslib::nav_panel(
        value = ns("panel_usage"),
        title = shiny::textOutput(ns("panel_usage_title"), tags$span),
        shiny::uiOutput(ns("panel_usage_accordion"))
    )

    panel_metho <- bslib::nav_panel(
        value = ns("panel_metho"),
        title = shiny::textOutput(ns("panel_metho_title"), tags$span),
        shiny::uiOutput(ns("panel_metho_accordion"))
    )

    # This implements a Bootstrap modal into a single
    # <div> that combines both the button and the modal.
    # (https://getbootstrap.com/docs/5.3/components/modal).
    return(
        tags$div(
            btn_open,

            # Modal.
            tags$div(
                id       = modal_id,
                class    = "modal",
                # Expand buttons of bslib::card() have a predefined z-index
                # set equal to 1070. This is greater than Bootstrap default
                # z-index for modal (1055). To prevent Expand buttons from
                # automatically appearing over the modal when users hover
                # over their position, the modal gets a really high z-index
                # to ensure it remains stacked on everything else.
                style    = "z-index: 9999",
                tabindex = "-1",

                # Outer container of modal.
                # It is postioned at the bottom of the screen unless
                # classes modal-dialog-centered and mt-5 are set.
                tags$div(
                    class = "modal-dialog modal-dialog-scrollable modal-xl",

                    tags$div(
                        # Class app-navset-bar-fix is a special
                        # fix that forces the navigation bar of
                        # a bslib::navset_bar() to look exactly
                        # like what bslib::navset_card_underline()
                        # probuces. See details in www/main.css.
                        class = "modal-content app-navset-bar-fix",

                        # Modal's body.
                        tags$div(
                            class = "modal-body p-0",

                            bslib::navset_bar(
                                id       = ns("panel_active"),
                                selected = ns("panel_intro"),
                                title    = shiny::textOutput(ns("title"), tags$span),
                                footer   = tags$div(
                                    class = "border-top py-3",
                                    ui_footer(ns("footer"))
                                ),

                                bslib::nav_spacer(),

                                panel_intro,
                                panel_parameters,
                                panel_usage,
                                panel_metho,

                                # Close button.
                                bslib::nav_item(
                                    # Extra padding to separate
                                    # button from other nav items.

                                    # Only shown on larger screens (>= 992px).
                                    tags$div(class = "d-none d-lg-block"),

                                    # Only shown on smaller screens (<= 992px).
                                    tags$div(class = "d-lg-none mt-2"),

                                    btn_close
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}

#' @rdname ui-modal-faq
#' @export
server_modal_faq <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- \(input, output, session) {
        server_footer("footer", lang)

        output$btn_open_text <- shiny::renderText({
            translate(lang = lang(), "FAQ")
        }) |>
        shiny::bindCache(lang())

        output$title <- shiny::renderText({
            translate(lang = lang(), "Frequently Asked Questions")
        }) |>
        shiny::bindCache(lang())

        output$panel_intro_title <- shiny::renderText({
            translate(lang = lang(), "General")
        }) |>
        shiny::bindCache(lang())

        output$panel_parameters_title <- shiny::renderText({
            translate(lang = lang(), "Calculation Parameters")
        }) |>
        shiny::bindCache(lang())

        output$panel_usage_title <- shiny::renderText({
            translate(lang = lang(), "Usage")
        }) |>
        shiny::bindCache(lang())

        output$panel_metho_title <- shiny::renderText({
            translate(lang = lang(), "Methodological Background")
        }) |>
        shiny::bindCache(lang())

        output$panel_parameters_warning_text <- shiny::renderText({
            translate(lang = lang(), "
                Tool 1 assumes that the measurements represent a random
                sample drawn from the underlying distribution of
                exposures within the sampled context. In other words,
                the data is considered representative of the specific
                exposure regimen being assessed.
            ")
        }) |>
        shiny::bindCache(lang())

        output$panel_intro_accordion <- shiny::renderUI({
            ui_panel_intro_accordion(lang())
        }) |>
        shiny::bindCache(lang())

        output$panel_parameters_accordion <- shiny::renderUI({
            ui_panel_parameters_accordion(lang())
        }) |>
        shiny::bindCache(lang())

        output$panel_usage_accordion <- shiny::renderUI({
            ui_panel_usage_accordion(lang())
        }) |>
        shiny::bindCache(lang())

        output$panel_metho_accordion <- shiny::renderUI({
            ui_panel_metho_accordion(lang())
        }) |>
        shiny::bindCache(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}

# Static Panels ----------------------------------------------------------------

#' @rdname ui-modal-faq
#' @export
ui_panel_intro_accordion <- function(lang = "") {
    # To preserve space, the return()
    # statement is avoided exceptionally.
    bslib::accordion(
        open     = FALSE,
        multiple = FALSE,
        class    = "accordion-flush app-accordion-active-bold",
        style    = "overflow: auto;",

        ## Panel: What is Tool 1? ----------------------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "What is Tool 1?"),

            tags$p(translate(lang = lang, "
                Tool 1 is an open-source and free-to-use web application
                that facilitates the  interpretation of industrial hygiene
                measurements, particularly in assessing compliance with
                occupational exposure limits (OELs).
            ")),

            tags$p(translate(lang = lang, "
                It builds on a recognized risk assessment framework
                endorsed by leading institutions, including
                the American Industrial Hygiene Association (AIHA),
                the British Occupational Hygiene Society (BOHS),
                the Dutch Society for Occupational Hygiene (NVVA),
                the French Institut national de recherche et de sécurité (INRS),
                the National Institute for Occupational Safety and Health (NIOSH),
                and the European Standards Organization.
            "))
        ),

        ## Panel: Who created Tool 1? ------------------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Who created Tool 1?"),

            tags$p(
                html(
                    translate(lang = lang, "
                        Tool 1 was developped by the Industrial Hygiene team of
                        the Department of Environmental and Occupational Health
                        at the %s of the %s.
                    "),
                    ui_link(default_urls$epsum[[lang]], "École de Santé Publique"),
                    ui_link(default_urls$udm[[lang]], "Université de Montréal")
                )
            )
        ),

        ## Panel: Who currently maintains Tool 1? ------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Who currently maintains Tool 1?"),

            tags$p(
                html(
                    translate(lang = lang, "
                        %s and %s currently maintains Tool 1. Ununoctium is an
                        external collaborator working on the design and
                        implementation of newer versions greater than or equal
                        to 4.0.0. It oversees non-scientific components of the
                        web application on behalf of Jérôme Lavoué.
                    "),
                    ui_link(default_urls$jerome_lavoue, "Jérôme Lavoué"),
                    ui_link(default_urls$ununoctium, "Ununoctium")
                )
            )
        ),

        ## Panel: How can I submit bugs or provide feedback? -------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "
                How can I submit bugs or provide feedback?
            "),

            tags$p(
                html(
                    translate(lang = lang, "
                        If you have a GitHub account, you may submit bugs,
                        request features, and provide feedback by creating
                        an issue on %s.
                    "),
                    ui_link(default_urls$code, "GitHub")
                )
            ),

            tags$p(
                html(
                    translate(lang = lang, "
                        Otherwise, you may send an email to the current
                        maintainers of Tool 1: %s and %s. The content of your
                        email will be used to create an issue on GitHub on your
                        behalf. The objective is twofold: be transparent with
                        the problems of Tool 1 and have a robust way to track
                        and discuss these issues publicly.
                    "),
                    ui_link_mailto(
                        default_maintainers_emails[["jerome_lavoue"]],
                        "Jérôme Lavoué"
                    ),
                    ui_link_mailto(
                        default_maintainers_emails[["ununoctium"]],
                        "Ununoctium"
                    )
                )
            )
        )
    )
}

#' @rdname ui-modal-faq
#' @export
ui_panel_parameters_accordion <- function(lang = "") {
    # To preserve space, the return()
    # statement is avoided exceptionally.
    bslib::accordion(
        open     = FALSE,
        multiple = FALSE,
        class    = "accordion-flush app-accordion-active-bold",
        style    = "overflow: auto;",

        ## Panel: How should measurements be formatted? ------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "How should measurements be formatted?"),

            tags$p(
                html(
                    translate(lang = lang, "
                        Measurements are entered in the Measurements text area
                        of the %s sidebar. Failing to follow the following rules
                        below will inevitably lead to undefined behavior.
                    "),
                    ui_panel_title_display(
                        title     = translate(lang = lang, "Calculation Parameters"),
                        icon_name = "calculator-fill"
                    )
                )
            ),

            tags$ol(
                class = "list-group list-group-flush px-2",
                style = "text-align: justify;",

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "There must be one value per line.")
                ),

                tags$li(
                    class = "list-group-item",

                    html(
                        translate(lang = lang, "
                            In accordance with the International System of Units
                            (SI) and what the National Institute of Standards and
                            Technology (NIST) of the United States recommends
                            (see %s for more information), always put a leading
                            zero before decimals for numbers strictly smaller
                            than one.
                        "),
                        ui_link(default_urls$nist_j032, "J-032")
                    )
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "Always use a dot for decimals.")
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "Do not use a separator for thousands.")
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Do not put a blank character (tab, space, etc.) before
                        and after special characters used to indicate how a
                        measurement is censored. See below for more information.
                    ")
                )
            )
        ),

        ## Panel: How can I verify that my measurements were ... ---------------

        bslib::accordion_panel(
            title = translate(lang = lang, "
                How can I verify that my measurements were successfully imported?
            "),

            tags$p(
                html(
                    translate(lang = lang, "
                        Use the %s panel. It provides descriptive statistics, a
                        quantile-quantile plot, and a box and whiskers plot that
                        can be used to ensure that measurements were parsed as
                        expected and successfully imported.
                    "),
                    ui_panel_title_display(
                        title     = translate(lang = lang, "About My Measurements"),
                        icon_name = "123"
                    )
                )
            ),

            tags$p(
                html(
                    translate(lang = lang, "
                        Descriptive statistics should not be viewed as useful
                        estimates of the underlying exposure distribution. Use
                        the %s panels for that purpose. These provide
                        information inferred from Bayesian models.
                    "),
                    ui_panel_title_display(
                        title      = translate(lang = lang, "Inference"),
                        icon_name  = "body-text",
                        icon_class = "app-rotated-minus-90"
                    )
                )
            )
        ),

        ## Panel: Can measurements be censored? --------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Can measurements be censored?"),

            tags$p(translate(lang = lang, "
                Measurements can be censored as long as they use the same units
                as other non-censored measurements.
            ")),

            tags$ol(
                class = "list-group list-group-flush px-2",
                style = "text-align: justify;",

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Add a lower than or equal sign before each
                        measurement censored to the left (e.g. <30.0).
                    "),
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Add a greater than or equal sign before each
                        measurement censored to the right (e.g. >30.0).
                    ")
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Use square brackets to denote interval censored values
                        (e.g. [20-30]).
                    ")
                )
            )
        ),

        ## Panel: How are censored measurements imputed? -----------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "How are censored measurements imputed?"),

            tags$p(translate(lang = lang, "
                Censored measurements are subject to one of the following
                procedures.
            ")),

            tags$ul(
                class = "list-group list-group-flush px-2",

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Interval censored measurements are imputed as the
                        mid-range.
                    ")
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Measurements censored to the right are imputed as 9/4
                        of the censoring point.
                    ")
                ),

                tags$li(
                    class = "list-group-item",

                    html(
                        translate(lang = lang, "
                            Measurements censored to the left are treated using
                            robust Log-probit regression on order statistics.
                            The algorithm used is derived from %s (itself
                            derived from the work of %s).
                        "),
                        ui_link(default_urls$ndexpo, "NDExpo"),
                        ui_link(default_urls$dennis_helsel, "Dennis Helsel")
                    )
                )
            )
        )
    )
}

#' @rdname ui-modal-faq
#' @export
ui_panel_usage_accordion <- function(lang = "") {
    # To preserve space, the return()
    # statement is avoided exceptionally.
    bslib::accordion(
        open     = FALSE,
        multiple = FALSE,
        class    = "accordion-flush app-accordion-active-bold",
        style    = "overflow: auto;",

        ## Panel: Why is there no shown output initially? ----------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Why is there no shown output initially?"),

            tags$p(translate(lang = lang, "
                No outputs are shown until inputs are submitted. Some static
                elements, such as text and icons, are pre-rendered for
                optimization purposes.
            "))
        ),

        ## Panel: How can I generate results? ----------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "How can I generate results?"),

            tags$p(translate(lang = lang, "
                Locate the primary sidebar on the left and follow these steps.
            ")),

            tags$ol(
                class = "list-group list-group-flush px-2",

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Enter your measurements. Measurements can be pasted
                        from a copied spreadsheet column.
                    ")
                ),

                tags$li(
                    class = "list-group-item",

                    html(
                        translate(lang = lang, "
                            Enter other parameters. Some inputs are specific to
                            certain panels and are initially hidden. They are
                            shown when an %s panel is chosen.
                        "),
                        ui_panel_title_display(
                            title      = translate(lang = lang, "Inference"),
                            icon_name  = "body-text",
                            icon_class = "app-rotated-minus-90"
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",

                    html(
                        translate(lang = lang, "
                            Submit inputs by clicking on the %s button.
                        "),
                        ui_panel_title_display(
                            title     = translate(lang = lang, "Submit"),
                            icon_name = "check-circle-fill"
                        )
                    )
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Wait for the server to perform the calculations. All
                        results will be shown once they are ready.
                    ")
                )
            )
        ),

        ## Panel: How long should I wait for the results? ----------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "How long should I wait for the results?"),

            tags$p(
                class = "pt-3",

                html(
                    translate(lang = lang, "
                        Depending on the server's current load and the sample's
                        size, you may have to wait a little while before obtaining
                        results. Waiting times are usually lower than 30 seconds.
                        Some %s panels require more computing time and resources.
                    "),
                    ui_panel_title_display(
                        title      = translate(lang = lang, "Inference"),
                        icon_name  = "body-text",
                        icon_class = "app-rotated-minus-90"
                    )
                )
            )
        ),

        ## Panel: Can plots be expanded? ---------------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Can plots be expanded?"),

            tags$p(
                html(
                    translate(lang = lang, "
                        Yes. Each plot may be expanded to a full-screen size.
                        Hover over it and click on the %s button on the bottom
                        right.
                    "),
                    ui_panel_title_display(
                        title     = translate(lang = lang, "Expand"),
                        icon_name = "arrows-angle-expand"
                    )
                )
            )
        )
    )
}

#' @rdname ui-modal-faq
#' @export
ui_panel_metho_accordion <- function(lang = "") {
    # To preserve space, the return()
    # statement is avoided exceptionally.
    bslib::accordion(
        open     = FALSE,
        multiple = FALSE,
        class    = "accordion-flush app-accordion-active-bold",
        style    = "overflow: auto;",

        ## Panel: What is the statistical approach used by Tool 1? -------------

        bslib::accordion_panel(
            title = translate(lang = lang, "
                What is the statistical approach used by Tool 1?
            "),

            tags$p(translate(lang = lang, "
                Tool 1 uses a Bayesian approach to estimate the parameters of
                the lognormal distribution. Doing so offers at least three
                advantages compared to traditional (frequentist) methods.
            ")),

            tags$ul(
                class = "list-group list-group-flush px-2",

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        Its resulting probabilistic statements are easier to
                        convey to stakeholders.
                    ")
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        It naturally integrates the treatment of non-detects.
                    ")
                ),

                tags$li(
                    class = "list-group-item",

                    translate(lang = lang, "
                        It allows the inclusion of external information in the
                        measurements (not yet leveraged).
                    ")
                )
            )
        ),

        ## Panel: Is the approach of Tool 1 recognized and peer-reviewed? ------

        bslib::accordion_panel(
            title = translate(lang = lang, "
                Is the approach of Tool 1 recognized and peer-reviewed?
            "),

            tags$p(
                html(
                    translate(lang = lang, "
                        The Bayesian models and data interpretation procedures
                        used by this application are derived from current best
                        practices in industrial hygiene, as reviewed in the
                        following scientific paper. Further details and
                        references are also available on %s.
                    "),
                    ui_link(default_urls$expostats[[lang]], "expostats.ca")
                )
            ),

            bslib::card(
                class = "border-primary bg-primary-subtle mx-5",

                bslib::card_body(
                    tags$p(
                        html(
                            translate(lang = lang, "
                                Jérôme Lavoué, Lawrence Joseph, Peter Knott,
                                Hugh Davies, France Labrèche, Frédéric Clerc,
                                Gautier Mater, Tracy Kirkham, %s, Annals of
                                Work Exposures and Health, Volume 63, Issue
                                3, April 2019, Pages 267-279.
                            "),
                            ui_link(
                                default_urls$expostats_paper,
                                tags$span(
                                    class = "fst-italic",
                                    "Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements"
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}
