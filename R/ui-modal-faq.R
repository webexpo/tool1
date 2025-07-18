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
            class = "btn btn-outline-secondary app-btn",
            type  = "button",
            bsicons::bs_icon("info-circle-fill", a11y = "sem")
        ) |>
        bslib::tooltip(
            id        = ns("btn_open_tooltip"),
            placement = "bottom",
            ""
        )
    )

    # data-bs-dismiss must not be equal to
    # modal_id according to Bootstrap. This
    # is a little weird, but this value is
    # used to id the currently opened modal.
    # Only shown on larger screens (>= 992px).
    btn_close <- tags$button(
        class             = "btn btn-outline-secondary app-btn ms-1",
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
                            class = "modal-body px-0 pb-0 pt-2",

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

        btn_open_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Get additional information on Tool 1.
            ")
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
            translate(lang = lang(), "Methodology")
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

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            bslib::update_tooltip("btn_open_tooltip", btn_open_tooltip_text())
        })

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}

# Static Panels ----------------------------------------------------------------

#' @rdname ui-modal-faq
#' @export
ui_panel_intro_accordion <- function(lang = "en") {
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
                occupational exposure limits (OEL).
            ")),

            tags$p(translate(lang = lang, "
                It builds on a recognized risk assessment framework
                endorsed by leading institutions, including
                the American Industrial Hygiene Association (AIHA),
                the British Occupational Hygiene Society (BOHS),
                the Dutch Society for Occupational Hygiene (NVVA),
                the French Institut national de recherche et de sécurité (INRS),
                the National Institute for Occupational Safety and Health (NIOSH),
                and the European Committee for Standardization (CEN).
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

                    ui_link(
                        "École de Santé Publique",
                        href = i18n_url(
                            "https://espum.umontreal.ca/english/home",
                            fr = "https://espum.umontreal.ca/accueil"
                        )[[lang]]
                    ),

                    ui_link(
                        "Université de Montréal",
                        href = i18n_url(
                            "https://www.umontreal.ca/en",
                            fr = "https://www.umontreal.ca"
                        )[[lang]]
                    )
                )
            )
        ),

        ## Panel: Who translated Tool 1? ---------------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Who translated Tool 1?"),

            tags$p(translate(lang = lang, "
                Tool 1 is available in multiple languages thanks to the
                following collaborators, who kindly contributed their time,
                skills, and expertise to provide accurate translations. Many
                thanks to them!
            ")),

            tags$p(
                html(
                    translate(lang = lang, "
                        If you notice an error or would like to submit an update,
                        please send an e-mail to %s. He will gladly submit your
                        case to the appropriate maintainers listed below.
                    "),
                    ui_link_mailto("jerome.lavoue@umontreal.ca", "Jérôme Lavoué")
                )
            ),

            tags$div(
                class = "px-5",

                as_html_table(
                    read_collaborators(),
                    colnames = c(
                        translate(lang = lang, "Language"),
                        translate(lang = lang, "Role"),
                        translate(lang = lang, "Profile"),
                        translate(lang = lang, "Email Address")
                    )
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
                    ui_link(urls$jerome_lavoue, "Jérôme Lavoué"),
                    ui_link(urls$ununoctium, "Ununoctium")
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
                        You may submit bugs, request features, and provide
                        feedback on %s if you have an account. You may also
                        send an email to %s, the maintainer of Tool 1.
                    "),
                    ui_link(urls$code, "GitHub"),
                    ui_link_mailto("jerome.lavoue@umontreal.ca", "Jérôme Lavoué")
                )
            )
        )
    )
}

#' @rdname ui-modal-faq
#' @export
ui_panel_parameters_accordion <- function(lang = "en") {
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

            tags$p(translate(lang = lang, "
                Measurements are entered in the Measurements text area of the
                Calculation Parameters sidebar. Failing to follow the following
                rules below will inevitably lead to undefined behavior.
            ")),

            tags$ol(
                class = "list-group list-group-flush px-2",

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
                        ui_link(
                            i18n_url(
                                "https://www.nist.gov/system/files/documents/2023/09/26/J-032%20Writing%20with%20the%20SI.pdf"
                            )[[lang]],
                            "J-032"
                        )
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

            tags$p(translate(lang = lang, "
                Use the About My Measurements panel. It provides descriptive
                statistics, a quantile-quantile plot, and a box and whiskers
                plot that can be used to ensure that measurements were parsed
                as expected.
            ")),

            bslib::card(
                class = "border-danger bg-danger-subtle mx-5",
                fill  = FALSE,

                bslib::card_body(
                    tags$p(
                        class = "text-danger text-center",
                        translate(lang = lang, "
                            Descriptive statistics should not be viewed as
                            useful estimates of the underlying exposure
                            distribution. Use the Statistical Inference panels
                            for that purpose. These provide information inferred
                            from Bayesian models.
                        ")
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
                This depends on the chosen panel.
            ")),

            tags$p(
                html(
                    translate(lang = lang, "
                        In all Statistical Inference panels, non-detects as
                        interpreted as such by the Bayesian model. The latter
                        natively extracts and uses the corresponding information.
                        There is no imputation and no creation of arbitrary
                        values. See %s for an example.
                    "),
                    ui_link(
                        i18n_url(
                            "https://academic.oup.com/annweh/article-abstract/60/1/56/2196069"
                        )[[lang]],
                        "Huynh et al. (2015)"
                    )
                )
            ),

            tags$p(translate(lang = lang, "
                In the About My Measurements panel, censored measurements are
                subject to one of the following procedures.
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
                        ui_link(urls$ndexpo, "NDExpo"),
                        ui_link(
                            i18n_url(
                                "https://www.practicalstats.com/info2use/books.html"
                            )[[lang]],
                            "Dennis Helsel"
                        )
                    )
                )
            )
        )
    )
}

#' @rdname ui-modal-faq
#' @export
ui_panel_usage_accordion <- function(lang = "en") {
    # To preserve space, the return()
    # statement is avoided exceptionally.
    bslib::accordion(
        open     = FALSE,
        multiple = FALSE,
        class    = "accordion-flush app-accordion-active-bold",
        style    = "overflow: auto;",

        ## Panel: What are modes? ----------------------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "What are modes?"),

            tags$p(translate(lang = lang, "
                Tool 1 offers two modes controlling how much information is
                displayed to the user.
            ")),

            tags$ul(
                class = "list-group list-group-flush px-2 mb-3",

                tags$li(
                    class = "list-group-item",
                    translate(lang = lang, "
                        Express mode shows a curated subset of results in a
                        single Statistical Inference panel. It is the default
                        mode.
                    ")
                ),

                tags$li(
                    class = "list-group-item",
                    translate(lang = lang, "
                        Extended mode has more inputs and shows additional
                        results split into multiple Statistical Inference
                        panels.
                    ")
                )
            ),

            tags$p(translate(lang = lang, "
                Choosing either one is a matter of personal preference.
            ")),

            tags$p(translate(lang = lang, "
                Express mode used to be a distinct web application called
                Tool 1 Express (Simplified) in earlier versions of Tool 1
                and Expostats. The latter was integrated into Tool 1 in
                version 5.0.0.
            "))
        ),

        ## Panel: How can I customize the user interface? ----------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "How can I customize the user interface?"),

            tags$p(translate(lang = lang, "
                You may customize the user interface by using the menus and
                buttons located in the title bar of Tool 1 (in the top right
                corner). All parameters may be changed anytime on-the-fly
                throughout the session.
            ")),

            tags$p(translate(lang = lang, "
                You may bookmark your preferred settings by passing them as
                query parameters (see the current URL displayed by your web
                browser for the current web page). Values must be valid.
                Otherwise, they are automatically replaced by default values.
            "))
        ),

        ## Panel: Why is there no shown output initially? ----------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Why is there no shown output initially?"),

            tags$p(translate(lang = lang, "
                No outputs are shown until calculation parameters are submitted.
                Some static elements, such as text and icons, are pre-rendered
                for optimization purposes.
            "))
        ),

        ## Panel: How can I generate results? ----------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "How can I generate results?"),

            tags$p(translate(lang = lang, "
                Locate the Calculation Parameters sidebar on the left and
                follow these steps.
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
                    translate(lang = lang, "
                        Enter other parameters. Some are specific to certain
                        panels and are hidden. They are only shown when their
                        corresponding Statistical Inference panel is chosen.
                    "),
                ),

                tags$li(
                    class = "list-group-item",
                    translate(lang = lang, "
                        Submit inputs by clicking on the green button
                        located below calculation parameters.
                    ")
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

            tags$p(translate(lang = lang, "
                Depending on the server's current load and the sample's
                size, you may have to wait a little while before obtaining
                results. Waiting times are usually lower than 30 seconds.
                Some panels require more computing time and resources.
            "))
        ),

        ## Panel: Can plots be expanded? ---------------------------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "Can plots be expanded?"),

            tags$p(translate(lang = lang, "
                Yes. Each plot may be expanded to a full-screen size. Hover
                over it and click on the Expand button on the bottom right.
            "))
        )
    )
}

#' @rdname ui-modal-faq
#' @export
ui_panel_metho_accordion <- function(lang = "en") {
    expostats_paper_card <- bslib::card(
        class = "border-primary bg-primary-subtle mx-5",

        bslib::card_body(
            tags$p(
                class = "text-center",
                html(
                    translate(lang = lang, "
                        Jérôme Lavoué, Lawrence Joseph, Peter Knott,
                        Hugh Davies, France Labrèche, Frédéric Clerc,
                        Gautier Mater, Tracy Kirkham, %s, Annals of
                        Work Exposures and Health, Volume 63, Issue
                        3, April 2019, Pages 267-279.
                    "),
                    ui_link(
                        i18n_url(
                            "https://doi.org/10.1093/annweh/wxy100"
                        )[[lang]],
                        tags$em(
                            "Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements"
                        )
                    )
                )
            )
        )
    )

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

        ## Panel: What is the rationale for the proposed ... -------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "
                What is the rationale for the proposed risk assessment framework?
            "),

            tags$p(translate(lang = lang, "
                The rationale proposed by Expostats for decision-making when
                interpreting industrial hygiene exposure data is described in
                the following accompanying paper.
            ")),

            expostats_paper_card,

            tags$p(translate(lang = lang, "It is as follows.")),

            tags$ol(
                class = "list-group list-group-flush list-group-numbered px-2 mb-3",

                tags$li(
                    class = "list-group-item",
                    html(
                        translate(lang = lang, "
                            Select a criterion for what is considered
                            overexposure (for example, the %s percentile
                            being greater than or equal to the OEL).
                        "),
                        ordinal(95.0, lang)
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    translate(lang = lang, "
                        Select a threshold for the probability that this
                        criterion is met, above which remedial action (or
                        additional sampling) should be required. Equivalently,
                        one can reason in terms of the degree of confidence
                        that the exposure criterion is not met: an overexposure
                        risk threshold of at most 30% is equivalent to reaching
                        at least 70% confidence that there is no overexposure.
                    ")
                )
            ),

            tags$p(translate(lang = lang, "
                The traditional threshold for the probability of overexposure is
                5% (i.e. reaching 95% confidence that there is no overexposure).
                More recently, following foundational work in France around 2010,
                several institutions have adopted an overexposure risk threshold
                of 30% (70% level of confidence). Discussion about the rationale
                for the use of 70% confidence (or a 30% tolerable overexposure
                risk) can be found in the following scientific paper.
            ")),

            bslib::card(
                class = "border-primary bg-primary-subtle mx-5",

                bslib::card_body(
                    tags$p(
                        class = "text-center",
                        # Only the main title is used for the
                        # link. Otherwise, it is too long and
                        # breaks the layout.
                        html(
                            translate(lang = lang, "
                                Ogden, Trevor, and Jérôme Lavoué. %s, %s,
                                Journal of Occupational and Environmental
                                Hygiene, Volume 9, Issue 4, 2012, Pages
                                D63-D70.
                            "),
                            ui_link(
                                i18n_url(
                                    "https://doi.org/10.1080/15459624.2012.663702"
                                )[[lang]],
                                tags$em("2011 William P. Yant Award Lecture")
                            ),
                            tags$em("Testing Compliance with Occupational Exposure Limits: Development of the British-Dutch Guidance")
                        )
                    )
                )
            ),

            tags$p(
                html(
                    translate(lang = lang, "
                        The decision scheme adopted for the Express mode of
                        Tool 1 was elaborated during the creation of the AIHA
                        video series %s (English only). In that scheme, reaching
                        95%% is ideal and yields an acceptable situation. Not
                        reaching 95%% confidence but reaching 70%% confidence
                        makes the situation tolerable. Not reaching 70%%
                        confidence that there is no overexposure makes the
                        situation problematic.
                    "),
                    ui_link(
                        urls$aiha_videos,
                        "Making Accurate Exposure Risk Decisions"
                    )
                )
            ),

            tags$p(
                html(
                    translate(lang = lang, "
                        The %s, %s, and %s about measurement data interpretation
                        are very similar but aggregate the %s and %s categories
                        as %4$s.
                    "),
                    ui_link(
                        i18n_url(
                            "https://www.inrs.fr/dms/inrs/PDF/metropol-strategie-principe/metropol-strategie-principe.pdf"
                        )[[lang]],
                        translate(lang = lang, "French guidelines")
                    ),
                    ui_link(
                        i18n_url(
                            "https://knowledge.bsigroup.com/products/workplace-exposure-measurement-of-exposure-by-inhalation-to-chemical-agents-strategy-for-testing-compliance-with-occupational-exposure-limit-values"
                        )[[lang]],
                        translate(lang = lang, "European guidelines EN689")
                    ),
                    ui_link(
                        i18n_url(
                            "https://www.bohs.org/app/uploads/2022/11/Testing-Compliance-with-OELs-for-Airborne-Substances-2022.pdf"
                        )[[lang]],
                        translate(lang = lang, "Dutch-British guidelines")
                    ),
                    tags$em(translate(lang = lang, "acceptable")),
                    tags$em(translate(lang = lang, "tolerable"))
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
                        used by Tool 1 are derived from current best practices
                        in industrial hygiene, as reviewed in the the following
                        scientifice paper. Further details and references are
                        also available on %s.
                    "),
                    ui_link(urls$expostats[[lang]], "expostats.ca")
                )
            ),

            expostats_paper_card
        ),

        ## Panel: Are there other educational resources ... --------------------

        bslib::accordion_panel(
            title = translate(lang = lang, "
                Are there other educational resources about industrial hygiene
                statistics?
            "),

            tags$p(translate(lang = lang, "
                Yes. Below are four videos made by the authors of Expostats.
                The last two stem from a 6-hour-long professional development
                course (PDC) made in 2024 for the Indonesian Industrial Hygiene
                Association (IIHA).
            ")),

            tags$ul(
                class = "list-group list-group-flush px-2 mb-3",

                tags$li(
                    class = "list-group-item",
                    ui_link(
                        i18n_url(
                            "https://umontreal.ca.panopto.com/Panopto/Pages/Viewer.aspx?pid=0a7847f9-66be-4efb-9752-aed201487e1b"
                        )[[lang]],
                        "Bayesian Statistics for Non-Statisticians Part 1"
                    ),
                    tags$span(
                        class = "ps-1",
                        translate(lang = lang, "(English only)")
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    ui_link(
                        i18n_url(
                            "https://umontreal.ca.panopto.com/Panopto/Pages/Viewer.aspx?pid=0a7847f9-66be-4efb-9752-aed201487e1b&id=68aeaea6-3186-44fc-80e0-aeb300b2d326&advance=true"
                        )[[lang]],
                        "Bayesian Statistics for Non-Statisticians Part 2"
                    ),
                    tags$span(
                        class = "ps-1",
                        translate(lang = lang, "(English only)")
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    ui_link(
                        i18n_url(
                            "https://umontreal.ca.panopto.com/Panopto/Pages/Viewer.aspx?id=39e5beaa-8033-4430-8991-b2de0147b366"
                        )[[lang]],
                        "Statistical Background"
                    ),
                    tags$span(
                        class = "ps-1",
                        translate(lang = lang, "(English only)")
                    )
                ),

                tags$li(
                    class = "list-group-item",
                    ui_link(
                        i18n_url(
                            "https://umontreal.ca.panopto.com/Panopto/Pages/Viewer.aspx?id=21e20e15-24aa-4946-8361-b2de0147bae3"
                        )[[lang]],
                        "Bayesian Statistics & Overview of Current Recommendations"
                    ),
                    tags$span(
                        class = "ps-1",
                        translate(lang = lang, "(English only)")
                    )
                )
            ),

            tags$p(translate(lang = lang, "
                You may also consult the scientific report of the Webexpo
                project. It includes a large introductory section focusing
                on industrial hygiene statistics.
            ")),

            bslib::card(
                class = "border-primary bg-primary-subtle mx-5",

                bslib::card_body(
                    tags$p(
                        class = "text-center",
                        html(
                            translate(lang = lang, "
                                Jérôme Lavoué, Lawrence Joseph, Tracy L.
                                Kirkham, France Labrèche, Gautier Mater, and
                                Frédéric Clerc (2020). %s (Report R-1065). IRSST.
                            "),
                            ui_link(
                                i18n_url(
                                    "https://pharesst.irsst.qc.ca/cgi/viewcontent.cgi?article=1087&context=rapports-scientifique"
                                )[[lang]],
                                tags$em("WebExpo: Towards a Better Interpretation of Measurements of Occupational Exposure to Chemicals In the Workplace")
                            )
                        )
                    )
                )
            )
        )
    )
}
