#' Title Bar Module
#'
#' @usage
#' ui <- bslib::page_sidebar(
#'   title = ui_title("title"),
#'   ...
#' )
#'
#' server <- function(input, output, session) {
#'   server_title("title")
#'   ...
#' }
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
#' Call [ui_title()] in the application's user interface and [server_title()]
#' in its [server()] function.
#'
#' @details
#' This module takes in parameters ony when necessary and relies on global
#' constants defined in `R/global.R` otherwise.
#'
#' @param id The module's unique identifier. It is passed to [shiny::NS()]
#'   to scope names of inputs and outputs. Shiny handles namespaces inside
#'   server functions. However, users must explicitly declare the current
#'   namespace when using [update_attribute()].
#'
#' @param lang A [shiny::reactive()] object returning the code of the current
#'   language.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @export
ui_title <- function(id = "") {
    ns <- shiny::NS(id)

    # Height of logo is derived from h1's font size
    # (which is in rem units) for consistency. Width
    # is automatically set to preserve aspect ratio.
    logo_size <- sprintf(
        "height: calc(%s * 1.0);",
        bslib::bs_get_variables(ui_bs_theme, "h1-font-size")
    )

    # Bootstrap recommends to mark elements of dropdowns
    # within <li> elements. Each <li> encapsulate a link
    # (an <a> tag) that sets the URL's lang parameter.
    # Tags are constructed from supported languages.
    btn_lang_choices <- mapply(
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

    ui <- shiny::tagList(
        # Left: Logo and Title -------------------------------------------------

        tags$h1(
            class = "d-flex flex-row align-items-center fw-bolder",

            # Width and height of the logo are resized
            # based on the theme's H1 font-size property.
            tags$img(
                id     = ns("logo"),
                src    = "logo-400x400.png",
                alt    = "Logo",
                width  = "400px",
                height = "400px",
                class  = "pe-3 w-auto",
                style  = logo_size
            ),

            shiny::textOutput(ns("title"), inline = TRUE)
        ),

        # Right: Buttons -------------------------------------------------------

        # Buttons are shown on larger screens only.
        # Otherwise, they are shown in the sidebar.
        tags$div(
            class = "app-large-screen-only",

            # This implements a Bootstrap dropdown via a single button
            # (https://getbootstrap.com/docs/5.3/components/dropdowns).
            # Since clicking on the button is very self-explanatory,
            # there is no tooltip.
            tags$div(
                id    = ns("btn_lang"),
                class = "dropdown d-inline-block",

                tags$button(
                    class            = "btn btn-outline-secondary dropdown-toggle mx-2 app-btn",
                    type             = "button",
                    "data-bs-toggle" = "dropdown",
                    "aria-expanded"  = "false",
                    ui_icons$intl
                ),

                tags$ul(class = "dropdown-menu") |>
                htmltools::tagSetChildren(list = btn_lang_choices)
            ),

            # UI starts in light mode, so the icon to show
            # is the one indicating dark mode is available.
            shiny::actionButton(
                inputId = ns("btn_color_mode"),
                label   = ui_icons$dark_mode,
                class   = "btn btn-outline-secondary mx-2 app-btn"
            ) |>
            bslib::tooltip(
                id        = ns("btn_color_mode_tooltip"),
                placement = "bottom",
                ""
            ),

            tags$a(
                href   = urls$code,
                target = "_blank",
                class  = "btn btn-outline-secondary mx-2 app-btn",
                ui_icons$github
            ) |>
            bslib::tooltip(
                id        = ns("btn_code_tooltip"),
                placement = "bottom",
                ""
            ),

            tags$a(
                id     = ns("btn_expostats"),
                href   = urls$expostats[[default_lang]],
                target = "_blank",
                class  = "btn btn-outline-secondary mx-2 app-btn",
                ui_icons$globe
            ) |>
            bslib::tooltip(
                id        = ns("btn_expostats_tooltip"),
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

    # Only required when using custom messages.
    # Shiny handles the rest automatically.
    ns <- shiny::NS(id)

    server <- \(input, output, session) {
        output$title <- shiny::renderText(intl(lang = lang(), "
            Tool 1: Data Interpretation for One Similarly Exposed Group
        "))

        # Observer for color mode (light/dark).
        shiny::observe({
            # Icon shown in light mode is the one for dark mode,
            # and vice-versa. Since light is the initial mode,
            # even values (including 0) of input$btn_color_mode
            # show icon for dark mode, and odd numbers show the
            # icon of light mode (each click toggles dark mode).
            icon <- if (input$btn_color_mode %% 2L == 0L) {
                ui_icons$dark_mode
            } else {
                ui_icons$light_mode
            }

            bslib::toggle_dark_mode()
            shiny::updateActionButton(session, "btn_color_mode", label = icon)
        }) |>
        shiny::bindEvent(input$btn_color_mode)

        # Observer for internationalization.
        shiny::observe({
            lang <- lang()

            update_attribute(ns("btn_expostats"), "href", urls$expostats[[lang]])

            bslib::update_tooltip("btn_color_mode_tooltip", intl(lang = lang, "
                Choose your preferred color scheme.
            "))

            bslib::update_tooltip("btn_code_tooltip", intl(lang = lang, "
                Click here to see the source code hosted on GitHub.
            "))

            bslib::update_tooltip("btn_expostats_tooltip", intl(lang = lang, "
                Click here for more information on Expostats and related
                tools.
            "))
        }) |>
        shiny::bindEvent(lang())
    }

    return(shiny::moduleServer(id, server))
}
