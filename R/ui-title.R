#' Title Module
#'
#' @description
#' This module controls the Title component conceptually illustrated below.
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
#' ## Bootstrap Navbar
#'
#' [bslib::page_sidebar()] implicitly wraps the content of what is passed to
#' argument `title` with a div.navbar.navbar-static-top > div.container-fluid
#' in accordance with what Bootstrap prescribes. Consequently, [ui_title()]
#' returns a list of HTML elements styled with Bootstrap `navbar-*` classes
#' directly (and not within a div.navbar). This is currently undocumented by
#' bslib.
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @returns
#' [ui_title()] returns a list of `shiny.tag` objects.
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
#' @seealso
#' [Bootrap 5 Navbars](https://getbootstrap.com/docs/5.3/components/navbar/),
#' [Bootstrap 5 Dropdowns](https://getbootstrap.com/docs/5.3/components/dropdowns),
#' [Bootstrap 5 Breakpoints](https://getbootstrap.com/docs/5.3/layout/breakpoints/)
#'
#' @rdname ui-title
#'
#' @export
ui_title <- function(id) {
    ns <- shiny::NS(id)
    nav_id <- ns("navbar_nav")

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

    # Temporarily disable French language (explicitly).
    # TODO: Reactivate menu item once translations become available.
    btn_langs_choices$fr <- htmltools::tagAppendAttributes(
        btn_langs_choices$fr,
        class           = "disabled",
        "aria-disabled" = "true",
        .cssSelector    = "a"
    )

    return(
        list(
            # Branding (Logo and title) ----------------------------------------

            tags$div(
                class = "navbar-brand py-0",

                tags$a(
                    style    = "text-decoration: none;",
                    href     = "",
                    hreflang = default_lang,
                    target   = "_self",

                    tags$img(
                        id     = ns("logo"),
                        src    = "images/logo-400x400.png",
                        alt    = "Logo",
                        width  = "400px",
                        height = "400px",
                        style  = "height: 40px;",
                        class  = "w-auto pe-1"
                    )
                ),

                htmltools::tagAppendAttributes(
                    class = "fw-bolder",
                    shiny::textOutput(ns("name"), tags$span)
                ),

                # The full title is only shown on extra extra large
                # screens (>=1400px). See Bootstrap breakpoints for
                # more information.
                tags$span(
                    class = "d-none d-xxl-inline",

                    ":",
                    shiny::textOutput(ns("title"), tags$span),
                    "(SEG)"
                )
            ),

            # Menu Button ------------------------------------------------------

            # Hamburger button to toggle menu.
            # Only shown on smaller screens (<= 992px).
            tags$button(
                class            = "navbar-toggler",
                type             = "button",
                "data-bs-toggle" = "collapse",
                "data-bs-target" = paste0("#", nav_id),

                # Default Boostrap hamburger icon.
                tags$span(class = "navbar-toggler-icon")
            ),

            # Navigation Bar ---------------------------------------------------

            tags$div(
                id    = nav_id,
                class = "collapse navbar-collapse justify-content-end",

                tags$ul(
                    class = "navbar-nav",

                    # Extra padding to separate branding from nav items.
                    # Only shown on smaller screens (<= 992px).
                    tags$div(class = "d-lg-none mt-3"),

                    ## Frequently Asked Questions ------------------------------

                    tags$li(
                        class = "nav-item",
                        ui_modal_faq(ns("faq"))
                    ),

                    ## Languages -----------------------------------------------

                    tags$li(
                        class = "nav-item dropdown",

                        tags$button(
                            class            = "nav-link dropdown-toggle",
                            type             = "button",
                            "data-bs-toggle" = "dropdown",

                            tags$span(
                                class = "pe-1",
                                bsicons::bs_icon("translate", a11y = "none")
                            ),

                            shiny::textOutput(ns("nav_item_language"), tags$span)
                        ),

                        htmltools::tagSetChildren(
                            tags$ul(class = "dropdown-menu"),
                            list = btn_langs_choices
                        )
                    ),

                    ## Expostats Links -----------------------------------------

                    tags$li(
                        class = "nav-item dropdown",

                        tags$button(
                            class            = "nav-link dropdown-toggle",
                            type             = "button",
                            "data-bs-toggle" = "dropdown",

                            tags$span(
                                class = "pe-1",
                                bsicons::bs_icon("link", a11y = "none")
                            ),

                            "Expostats"
                        ),

                        shiny::uiOutput(
                            ns("nav_item_expostats_links"),
                            container = tags$ul,
                            class     = "dropdown-menu"
                        )
                    ),

                    ## Spacers -------------------------------------------------

                    # Vertical bar for larger screens to
                    # separate buttons from other nav items.
                    # Only shown on larger screens (>= 992px).
                    tags$div(class = "d-none d-lg-block vr ms-2 me-3"),

                    # Extra padding to separate buttons from other nav items.
                    # Only shown on smaller screens (<= 992px).
                    tags$div(class = "d-lg-none mt-3"),

                    ## Buttons -------------------------------------------------

                    # They are grouped together as a single nav item.
                    tags$li(
                        class = "nav-item",

                        ### GitHub ---------------------------------------------

                        tags$a(
                            class  = "btn btn-outline-secondary app-btn me-2",
                            href   = default_urls$code,
                            target = "_blank",
                            bsicons::bs_icon("github", a11y = "none")
                        ) |>
                        bslib::tooltip(
                            id        = ns("btn_code_tooltip"),
                            placement = "bottom",
                            ""
                        ),

                        ### Submit Bugs ----------------------------------------

                        tags$a(
                            class  = "btn btn-outline-secondary app-btn me-2",
                            href   = paste0(
                                "mailto:",
                                paste(default_maintainers_emails, collapse = ",")
                            ),
                            target = "_blank",
                            bsicons::bs_icon("bug-fill", a11y = "none")
                        ) |>
                        bslib::tooltip(
                            id        = ns("btn_bug_tooltip"),
                            placement = "bottom",
                            ""
                        ),

                        ### Source Code ----------------------------------------

                        shiny::actionButton(
                            class   = "btn btn-outline-secondary app-btn",
                            inputId = ns("btn_color"),
                            label   = bsicons::bs_icon("moon-fill", a11y = "none")
                        ) |>
                        bslib::tooltip(
                            id        = ns("btn_color_tooltip"),
                            placement = "bottom",
                            ""
                        )
                    )
                )
            )
        )
    )
}

#' @rdname ui-title
#' @export
server_title <- function(id, lang) {
    stopifnot(shiny::is.reactive(lang))

    server <- \(input, output, session) {
        server_modal_faq("faq", lang)

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

        output$nav_item_language <- shiny::renderText({
            translate(lang = lang(), "Language")
        }) |>
        shiny::bindCache(lang())

        output$nav_item_expostats_links <- shiny::renderUI({
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
        }) |>
        shiny::bindCache(lang())

        # Toggle color mode (light/dark).
        shiny::observe({
            # Icon shown in light mode is the one for dark mode,
            # and vice-versa. Since light is the initial mode,
            # even values (including 0) of input$btn_color show
            # icon for dark mode, and odd numbers show the icon
            # of light mode (each click toggles dark mode).
            icon <- if (input$btn_color %% 2L == 0L) {
                bsicons::bs_icon("moon-fill", a11y = "none")
            } else {
                bsicons::bs_icon("sun-fill", a11y = "none")
            }

            bslib::toggle_dark_mode()
            shiny::updateActionButton(session, "btn_color", label = icon)
        }) |>
        shiny::bindEvent(input$btn_color)

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            lang <- lang()

            bslib::update_tooltip("btn_code_tooltip", translate(lang = lang, "
                See the source code of Tool 1 on GitHub.
            "))

            bslib::update_tooltip("btn_bug_tooltip", translate(lang = lang, "
                Submit a bug or provide feedback to the maintainers by email.
            "))

            bslib::update_tooltip("btn_color_tooltip", translate(lang = lang, "
                Toogle the current color scheme (light or dark).
            "))
        }) |>
        shiny::bindEvent(lang())

        return(invisible())
    }

    return(shiny::moduleServer(id, server))
}
