#' About Modal Module
#'
#' @description
#' This module controls the About modal nested into the Title module.
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
#' @template param-id
#'
#' @template param-lang
#'
#' @returns
#' [ui_modal_about()] returns a `shiny.tag` object.
#'
#' [server_modal_about()] returns `NULL`, invisibly.
#'
#' @seealso [Bootstrap Modals](https://getbootstrap.com/docs/5.3/components/modal/)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-modal-about
#'
#' @export
ui_modal_about <- function(id) {
    ns <- shiny::NS(id)
    modal_id <- ns("modal")

    # Button that shows and hides the modal.
    # bslib::tooltip() sets attribute "data-bs-toggle"
    # on the target element. If this is called on the
    # button directly, it overwrites "data-bs-toggle"
    # of the modal (there can only be one toggle event
    # per element). The solution is to separate these
    # two events by setting the modal toggle target in
    # an outer container.
    btn <- tags$div(
        "data-bs-toggle" = "modal",
        "data-bs-target" = paste0("#", modal_id),

        tags$button(
            class = "btn btn-outline-secondary app-btn",
            type  = "button",
            bsicons::bs_icon("info-circle-fill", a11y = "sem")
        ) |> bslib::tooltip(
            id        = ns("btn_about_tooltip"),
            placement = "bottom",
            ""
        )
    )

    # Title of the modal.
    title <- tags$div(
        class = "modal-header border-bottom",

        tags$h2(
            class = "modal-title fw-bold",
            shiny::textOutput(ns("title"), tags$span)
        ),

        # data-bs-dismiss must not be equal to
        # modal_id according to Bootstrap. This
        # is a little weird, but this value is
        # used to id the currently opened modal.
        tags$button(
            class             = "btn btn-outline-secondary app-btn",
            type              = "button",
            "data-bs-dismiss" = "modal",
            bsicons::bs_icon("x-square-fill", a11y = "sem")
        )
    )

    # Content of the modal.
    accordion <- tags$div(
        class = "modal-body",

        bslib::accordion(
            id       = ns("accordion"),
            open     = FALSE,
            multiple = FALSE,
            class    = "accordion-flush",

            # How To Use.
            bslib::accordion_panel(
                value = ns("how_to_panel"),
                title = "",
                icon  = bsicons::bs_icon(
                    "layout-text-window-reverse",
                    a11y = "deco"),

                shiny::uiOutput(ns("objectives")),

                # Subsections.
                htmltools::tagAppendAttributes(
                    shiny::textOutput(ns("usage_title"), tags$h3),
                    class = "fs-6 fw-bold"
                ),

                shiny::uiOutput(ns("usage")),

                # Embedded card for warning.
                bslib::card(
                    fill  = FALSE,
                    class = "w-75 mx-auto bg-warning-subtle border-warning",

                    bslib::card_body(
                        htmltools::tagAppendAttributes(
                            class = "text-center",
                            shiny::textOutput(ns("warning"), tags$p)
                        )
                    )
                ),

                htmltools::tagAppendAttributes(
                    shiny::textOutput(ns("censoring_title"), tags$h3),
                    class = "fs-6 fw-bold"
                ),

                shiny::textOutput(ns("censoring"), tags$p)
            ),

            # Methodological Background.
            bslib::accordion_panel(
                value = ns("metho_bg_panel"),
                title = "",
                icon  = bsicons::bs_icon("stars", a11y = "deco"),

                shiny::uiOutput(ns("approach")),

                # Subsection.
                htmltools::tagAppendAttributes(
                    shiny::textOutput(ns("expostats_title"), tags$h3),
                    class = "fs-5"
                ),

                shiny::uiOutput(ns("expostats"))
            ),

            # Authors.
            bslib::accordion_panel(
                value = ns("authors_panel"),
                title = "",
                icon  = bsicons::bs_icon(
                    "people-fill",
                    a11y = "deco"),

                shiny::uiOutput(ns("authors"))
            )
        )
    )

    # This implements a Bootstrap modal into a single
    # <div> that combines both the button and the modal.
    # (https://getbootstrap.com/docs/5.3/components/modal).
    return(
        tags$div(
            # Button.
            btn,

            # Modal.
            tags$div(
                id       = modal_id,
                class    = "modal",
                tabindex = "-1",

                # Outer box of modal.
                # It is postioned at the bottom of the screen unless
                # classes modal-dialog-centered and mt-5 are set.
                tags$div(
                    class = "modal-dialog modal-dialog-centered modal-dialog-scrollable modal-xl mt-5",

                    # Content of modal.
                    tags$div(
                        class = "modal-content",

                        title,
                        accordion,

                        # Footer.
                        tags$div(
                            class = "modal-footer border-top",
                            ui_footer(ns("footer"))
                        )
                    )
                )
            )
        )
    )
}

#' @rdname ui-modal-about
#' @export
server_modal_about <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- \(input, output, session) {
        server_footer("footer", lang)

        output$title <- shiny::renderText({
            translate(lang = lang(), "About")
        })

        output$objectives <- shiny::renderUI({
            lang <- lang()
            list(
                tags$p(translate(lang = lang, "
                    This application facilitates the interpretation of
                    industrial hygiene measurements, particularly in assessing
                    compliance with occupational exposure limits (OELs).
                ")),

                tags$p(translate(lang = lang, "
                    It builds on a recognized risk assessment framework endorsed
                    by leading institutions, including the American Industrial
                    Hygiene Association (AIHA), the British Occupational Hygiene
                    Society (BOHS), the Dutch Society for Occupational Hygiene
                    (NVVA), the French Institut national de recherche et de
                    sécurité (INRS), the National Institute for Occupational
                    Safety and Health (NIOSH), and the European Standards
                    Organization.
                ")),

                tags$p(translate(lang = lang, "
                    The application assumes that the input measurements
                    represent a random sample drawn from the underlying
                    distribution of exposures within the sampled context.
                    In other words, the data is considered representative
                    of the specific exposure regimen being assessed.
                "))
            )
        })

        output$usage_title <- shiny::renderText({
            translate(lang = lang(), "How To Use")
        })

        output$usage <- shiny::renderUI({
            lang <- lang()
            list(
                tags$p(translate(lang = lang, "
                    Locate the sidebar on the left and follow these steps.
                ")),

                tags$ol(
                    tags$li(translate(lang = lang, "
                        Enter your measurements. There must be one value per
                        line. You may paste values copied from a spreadsheet
                        column.
                    ")),

                    tags$li(translate(lang = lang, "
                        Enter other parameters.
                    ")),

                    tags$li(translate(lang = lang, "
                        Submit inputs by clicking on the Submit button
                        and wait for the calculations to be performed.
                    "))
                ),

                tags$p(translate(lang = lang, "
                    Note that no outputs are shown until inputs are submitted.
                    Boxes will remain empty until they are processed. Some
                    elements may be pre-rendered to speed up computations.

                    Each plot may be expanded to a full-screen size. Hover over
                    it and click on the Expand button on the bottom right.
                "))
            )
        })

        output$warning <- shiny::renderText({
            translate(lang = lang(), "
                Depending on your system settings and locale, Tool 1 may
                interpret the decimal separator as either a dot or a comma.
                Use the Statistics panel to ensure all data has been read
                correctly.
            ")
        })

        output$censoring_title <- shiny::renderText({
            translate(lang = lang(), "Censored Measurements")
        })

        output$censoring <- shiny::renderText({
            translate(lang = lang(), "
                Measurements can be censored as long as they use the same units
                as other non-censored measurements. Add a lower than or equal
                (<) sign before each measurement censored to the left (e.g.
                <30.0) and a greater than or equal (>) sign before each
                measurement censored to the right (e.g. >30.0). Use square
                brackets to denote interval censored values (e.g. [20-30]).
            ")
        })

        output$approach <- shiny::renderUI({
            lang <- lang()
            list(
                tags$p(translate(lang = lang, "
                    This application uses a Bayesian approach to estimate
                    the parameters of the lognormal distribution.
                ")),

                tags$ul(
                    tags$li(translate(lang = lang, "
                        Compared to traditional (frequentist) methods, it allows
                        making probabilistic statements about exposures that are
                        easier to convey to stakeholders.
                    ")),

                    tags$li(translate(lang = lang, "
                        It also naturally integrates the treatment of
                        non-detects.
                    ")),

                    tags$li(translate(lang = lang, "
                        It also allows the inclusion of external information in
                        the measurements (not yet leveraged).
                    "))
                )
            )
        })

        output$expostats_title <- shiny::renderText({
            translate(lang = lang(), "Expostats")
        })

        output$expostats <- shiny::renderUI({
            lang <- lang()
            list(
                tags$p(
                    html(
                        translate(lang = lang, "
                            The Bayesian models and data interpretation procedures
                            used by this application are derived from current best
                            practices in industrial hygiene, as reviewed in the
                            following scientific paper. Further details and
                            references are also available on %s.
                        "),

                        tags$a(
                            href   = default_urls$expostats[[lang]],
                            target = "_blank",
                            "expostats.ca"
                        )
                    )
                ),

                bslib::card(
                    class = "w-75 mx-auto",

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

                                tags$a(
                                    .noWS  = "after",
                                    href   = default_urls$expostats_paper,
                                    target = "_blank",
                                    style  = "font-style: italic",
                                    "Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements"
                                )
                            )
                        )
                    )
                )
            )
        })

        output$authors <- shiny::renderUI({
            lang <- lang()
            html(
                translate(lang = lang(), "
                    This application (and related tools) is developped by the
                    Industrial Hygiene team of the Department of Environmental
                    and Occupational Health at the %s of the %s.
                "),

                tags$a(
                    href   = default_urls$epsum[[lang]],
                    target = "_blank",
                    "École de Santé Publique"
                ),

                tags$a(
                    href   = default_urls$udm[[lang]],
                    target = "_blank",
                    "Université de Montréal"
                )
            )
        })

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            lang <- lang()

            bslib::update_tooltip("btn_about_tooltip", translate(lang = lang, "
                Get more information on this application and learn how to
                use it properly.
            "))

            bslib::accordion_panel_update(
                id     = "accordion",
                target = session$ns("how_to_panel"),
                title  = translate(lang = lang, "How To Use The Application")
            )

            bslib::accordion_panel_update(
                id     = "accordion",
                target = session$ns("metho_bg_panel"),
                title  = translate(lang = lang, "Methodological Background")
            )

            bslib::accordion_panel_update(
                id     = "accordion",
                target = session$ns("authors_panel"),
                title  = translate(lang = lang, "Authors")
            )
        }) |>
        shiny::bindEvent(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
