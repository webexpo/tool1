#' Title Module
#'
#' @description
#' This module cotrols the Title component conceptually illustrated below.
#'
#' ```
#' ---------------------------------------
#' | Title (this module)                 |
#' ---------------------------------------
#' | Sidebar | Main                      |
#' |         |  -----------------------  |
#' |         |  | Panels Navigation   |  |
#' |         |  -----------------------  |
#' |         |  | Active Panel        |  |
#' |         |  |                     |  |
#' |         |  |                     |  |
#' |         |  |                     |  |
#' |         |  -----------------------  |
#' ---------------------------------------
#' ```
#'
#' @details
#' This module implicitly relies on values defined in `R/global.R` and
#' `R/helpers*.R` scripts. They are sourced by [shiny::runApp()].
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @returns
#' [ui_title()] returns a `shiny.tag` object.
#'
#' [server_title()] returns `NULL`, invisibly.
#'
#' @note
#' This module implements two Bootstrap dropdown menus with single buttons
#' (https://getbootstrap.com/docs/5.3/components/dropdowns).
#'
#' Both buttons have a [bslib::tooltip()]. This function automatically sets
#' attribute `data-bs-toggle` on its input element. Since there can only be
#' one Bootstrap toggle event per element, calling this function on buttons
#' overwrites any previous `data-bs-toggle` attribute, like the one required
#' for the dropdown menu.
#'
#' The solution is to separate these two events (dropdown and tooltip trigger
#' metadata) by attaching the tooltip toggle to an outer container containing
#' each button.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-title
#'
#' @export
ui_title <- function(id) {
    ns <- shiny::NS(id)

    submit_bug_mailto <- paste0(
        "mailto:",
        paste0(default_maintainers_emails, collapse = ",")
    )

    # Bootstrap recommends to mark elements of dropdowns
    # within <li> elements. Each <li> encapsulate a link
    # (an <a> tag) that sets the URL's lang parameter.
    # Tags are constructed from supported languages.
    btn_langs_choices <- mapply(
        code     = names(tr$native_languages),
        lang     = tr$native_languages,
        SIMPLIFY = FALSE,
        \(code, lang) {
            return(
                tags$li(
                    tags$a(
                        class    = "dropdown-item",
                        href     = sprintf("?lang=%s", code),
                        hreflang = code,
                        rel      = "alternate",
                        target   = "_self",
                        lang
                    )
                )
            )
        }
    )

    ui <- shiny::div(
        class = "w-100 d-flex flex-wrap align-items-center justify-content-between",
        style = "gap: 15px;",

        # Left: Logo and Title -------------------------------------------------

        tags$div(
            class = "d-flex align-items-center",

            # Logo is a link that can be
            # clicked to reset everything.
            tags$a(
                href     = "",
                hreflang = default_lang,
                target   = "_self",

                # Height of logo is manually set
                # equal to the height of buttons.
                tags$img(
                    id     = ns("logo"),
                    src    = "images/logo-400x400.png",
                    alt    = "Logo",
                    width  = "400px",
                    height = "400px",
                    class  = "pe-3",
                    style  = "height: 46px; width: auto;"
                )
            ),

            # Bottom margin is removed to align logo
            # and name/title on the horizontal axis.
            tags$h1(
                class = "d-flex mb-0",

                htmltools::tagAppendAttributes(
                    class = "fw-bold",
                    shiny::textOutput(ns("name"), tags$span)
                ),

                # Colons are used to separate the app's name
                # from its title. The latter is only shown on
                # screens of >=1500 pixels.
                tags$span(
                    class = "app-large-screen-only text-muted",

                    ":",

                    shiny::textOutput(ns("title"), tags$span),

                    # This can be combined with the textOutput()
                    # above if it ever requires translation.
                    tags$span(class = "ms-1", "(SEG)")
                ),


            )
        ),

        # Right: Buttons -------------------------------------------------------

        tags$div(
            class = "d-flex",
            style = "gap: 15px;",

            ## Languages -------------------------------------------------------

            # See @note above on dropdowns and tooltips.
            tags$div(
                id = ns("btn_langs"),
                # Display inline-block is required to align
                # the dropdown button with other buttons on
                # the vertical axis.
                class = "d-inline-block dropdown",

                tags$button(
                    class            = "btn btn-outline-secondary app-btn dropdown-toggle",
                    type             = "button",
                    "data-bs-toggle" = "dropdown",
                    bsicons::bs_icon("translate", a11y = "none")
                ),

                htmltools::tagSetChildren(
                    tags$ul(class = "dropdown-menu"),
                    list = btn_langs_choices
                )
            ) |> bslib::tooltip(
                id        = ns("btn_langs_tooltip"),
                placement = "left",
                ""
            ),

            ## Links -----------------------------------------------------------

            # See @note above on dropdowns and tooltips.
            tags$div(
                id = ns("btn_links"),
                # Display inline-block is required to align
                # the dropdown button with other buttons on
                # the vertical axis.
                class = "d-inline-block dropdown",

                tags$button(
                    class            = "btn btn-outline-secondary app-btn dropdown-toggle",
                    type             = "button",
                    "data-bs-toggle" = "dropdown",
                    bsicons::bs_icon("link", a11y = "none")
                ),

                shiny::uiOutput(
                    ns("btn_links_choices"),
                    container = tags$ul,
                    class     = "dropdown-menu"
                )
            ) |> bslib::tooltip(
                id        = ns("btn_links_tooltip"),
                placement = "left",
                ""
            ),

            ## Help/About ------------------------------------------------------

            ui_modal_about(ns("about")),

            ## Dark Mode -------------------------------------------------------

            # UI starts in light mode, so the icon to show
            # is the one indicating dark mode is available.
            shiny::actionButton(
                inputId = ns("btn_color_mode"),
                label   = bsicons::bs_icon("moon-fill", a11y = "none"),
                class   = "btn btn-outline-secondary app-btn"
            ) |>
            bslib::tooltip(
                id        = ns("btn_color_mode_tooltip"),
                placement = "bottom",
                ""
            ),

            # Submit Bug -------------------------------------------------------

            tags$a(
                href   = submit_bug_mailto,
                target = "_blank",
                class  = "btn btn-outline-secondary app-btn",
                bsicons::bs_icon("bug-fill", a11y = "none")
            ) |>
            bslib::tooltip(
                id        = ns("btn_bug_tooltip"),
                placement = "bottom",
                ""
            ),

            # Source Code ------------------------------------------------------

            tags$a(
                href   = default_urls$code,
                target = "_blank",
                class  = "btn btn-outline-secondary app-btn",
                bsicons::bs_icon("github", a11y = "none")
            ) |>
            bslib::tooltip(
                id        = ns("btn_code_tooltip"),
                placement = "bottom",
                ""
            )
        )
    )

    return(ui)
}

#' @rdname ui-title
#' @export
server_title <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- \(input, output, session) {
        server_modal_about("about", lang)

        output$name <- shiny::renderText({
            translate(lang = lang(), "Tool 1")
        }) |>
        shiny::bindCache(lang())

        output$title <- shiny::renderText({
            translate(lang = lang(), "
                Data Interpretation for One Similar Exposure Group
            ")
        }) |>
        shiny::bindCache(lang())

        output$btn_links_choices <- shiny::renderUI({
            lang <- lang()
            links <- list(
                list(
                    default_urls$tool1_simplified[[lang]],
                    translate(lang = lang, "Tool 1 (Simplified)")
                ),
                list(
                    default_urls$tool2[[lang]],
                    translate(lang = lang, "Tool 2")
                ),
                list(
                    default_urls$tool3[[lang]],
                    translate(lang = lang, "Tool 3")
                ),
                list(
                    default_urls$expostats[[lang]],
                    "Expostats"
                ),
                list(
                    default_urls$ndexpo,
                    "NDExpo"
                )
            )

            lapply(links, \(link) {
                return(
                    tags$li(
                        tags$a(
                            class    = "dropdown-item",
                            href     = link[[1L]],
                            hreflang = lang,
                            rel      = "external",
                            target   = "_blank",
                            link[[2L]]
                        )
                    )
                )
            })
        })

        # Toggle color mode (light/dark).
        shiny::observe({
            # Icon shown in light mode is the one for dark mode,
            # and vice-versa. Since light is the initial mode,
            # even values (including 0) of input$btn_color_mode
            # show icon for dark mode, and odd numbers show the
            # icon of light mode (each click toggles dark mode).
            icon <- if (input$btn_color_mode %% 2L == 0L) {
                bsicons::bs_icon("moon-fill", a11y = "none")
            } else {
                bsicons::bs_icon("sun-fill", a11y = "none")
            }

            bslib::toggle_dark_mode()
            shiny::updateActionButton(session, "btn_color_mode", label = icon)
        }) |>
        shiny::bindEvent(input$btn_color_mode)

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            lang <- lang()

            bslib::update_tooltip("btn_langs_tooltip", translate(lang = lang, "
                Choose your preferred language.
            "))

            bslib::update_tooltip("btn_links_tooltip", translate(lang = lang, "
                Explore Expostats and other tools.
            "))

            bslib::update_tooltip("btn_color_mode_tooltip", translate(lang = lang, "
                Choose your preferred color scheme. This is an experimental
                feature. Contents may not be displayed appropriately.
            "))

            bslib::update_tooltip("btn_bug_tooltip", translate(lang = lang, "
                Submit a bug or provide feedback to the maintainers.
            "))

            bslib::update_tooltip("btn_code_tooltip", translate(lang = lang, "
                See the source code on GitHub.
            "))
        }) |>
        shiny::bindEvent(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
