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
#' ## User Interface State
#'
#' This module implements buttons that can be used to customize the user
#' interface and oversees their underlying internal states using functions
#' defined in `R/helpers-ui-title.R`.
#'
#' ## Languages
#'
#' This module automatically creates required buttons and observers for all
#' supported languages. In other words, it does not need to be updated when
#' a new language is added.
#'
#' @template param-id
#'
#' @param langs A named character vector. Elements are native language names
#'   (used as untranslated button labels). Names are the underlying language
#'   codes. It is equal to global constant `tr$native_languages` by default.
#'
#' @returns
#' [ui_title()] returns a list of `shiny.tag` objects.
#'
#' [server_title()] returns a named list of length 3 containing these elements:
#' `lang`, `mode`, and `color`. These are [shiny::reactive()] objects returning
#' the current value of these parameters.
#'
#' @note
#' This module implements two Bootstrap dropdown menus with single buttons
#' (https://getbootstrap.com/docs/5.3/components/dropdowns).
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [Bootstrap 5 Navbars](https://getbootstrap.com/docs/5.3/components/navbar/),
#' [Bootstrap 5 Dropdowns](https://getbootstrap.com/docs/5.3/components/dropdowns),
#' [Bootstrap 5 Breakpoints](https://getbootstrap.com/docs/5.3/layout/breakpoints/),
#' [Bootstrap 5 Color Modes](https://getbootstrap.com/docs/5.3/customize/color-modes/)
#'
#' @rdname ui-title
#' @export
ui_title <- function(id, langs = tr$native_languages) {
    ns <- shiny::NS(id)
    nav_id <- ns("navbar_nav")

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

                # .navbar-nav is a flex container by design.
                tags$ul(
                    class = "navbar-nav",
                    style = "gap: 0.5rem;",

                    # Extra padding to separate branding from nav items.
                    # Only shown on smaller screens (<= 992px).
                    tags$div(class = "d-lg-none mt-3"),

                    ## Modes ---------------------------------------------------

                    tags$li(
                        class = "nav-item dropdown",

                        tags$button(
                            class            = "nav-link dropdown-toggle",
                            type             = "button",
                            "data-bs-toggle" = "dropdown",

                            tags$span(
                                class = "pe-1",
                                bsicons::bs_icon(
                                    name = "layout-text-window-reverse",
                                    a11y = "deco"
                                )
                            ),

                            shiny::textOutput(ns("btn_modes_label"), tags$span)
                        ),

                        # Bootstrap use a smaller font for buttons.
                        # Class fs-6 ensures the same font is used
                        # for both links and buttons in dropdowns.
                        tags$ul(
                            class = "dropdown-menu dropdown-menu-end",

                            tags$li(
                                shiny::actionButton(
                                    inputId = ns("btn_mode_default"),
                                    class   = "dropdown-item fs-6",
                                    label   = shiny::textOutput(
                                        outputId  = ns("btn_mode_default_label"),
                                        container = tags$span
                                    )
                                )
                            ),

                            tags$li(
                                shiny::actionButton(
                                    inputId = ns("btn_mode_simplified"),
                                    class   = "dropdown-item fs-6",
                                    label   = shiny::textOutput(
                                        outputId  = ns("btn_mode_simplified_label"),
                                        container = tags$span
                                    )
                                )
                            )
                        )
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
                                bsicons::bs_icon("translate", a11y = "deco")
                            ),

                            shiny::textOutput(ns("btn_langs_label"), tags$span)
                        ),

                        # Generate a button for each supported language.
                        # Labels are created from native language names
                        # that must not be translated.
                        do.call(tags$ul, c(
                            class = "dropdown-menu dropdown-menu-end",
                            mapply(
                                lang      = names(langs),
                                name      = langs,
                                SIMPLIFY  = FALSE,
                                USE.NAMES = FALSE,
                                \(lang, name) {
                                    tags$li(
                                        shiny::actionButton(
                                            inputId = ns(sprintf("btn_lang_%s", lang)),
                                            class   = "dropdown-item fs-6",
                                            label   = name
                                        )
                                    )
                                }
                            )
                        ))
                    ),

                    ## Links ---------------------------------------------------

                    tags$li(
                        class = "nav-item dropdown",

                        tags$button(
                            class            = "nav-link dropdown-toggle",
                            type             = "button",
                            "data-bs-toggle" = "dropdown",

                            tags$span(
                                class = "pe-1",
                                bsicons::bs_icon("link", a11y = "deco")
                            ),

                            "Expostats"
                        ),

                        shiny::uiOutput(
                            ns("nav_item_expostats_links"),
                            container = tags$ul,
                            class     = "dropdown-menu dropdown-menu-end"
                        )
                    ),

                    ## Spacer --------------------------------------------------

                    # Vertical padding to separate
                    # buttons from other nav items.
                    # Only shown on smaller screens (<= 992px).
                    tags$div(class = "d-lg-none mt-1"),

                    ## Buttons -------------------------------------------------

                    # They are grouped together as a single nav item.
                    tags$li(
                        class = "nav-item d-flex",
                        style = "gap: 1rem;",

                        ### UI Color Mode --------------------------------------

                        shiny::actionButton(
                            class   = "btn btn-outline-secondary app-btn",
                            inputId = ns("btn_color"),
                            label   = bsicons::bs_icon("moon-fill", a11y = "sem")
                        ) |>
                        bslib::tooltip(
                            id        = ns("btn_color_tooltip"),
                            placement = "bottom",
                            ""
                        ),

                        ### Frequently Asked Questions -------------------------

                        ui_modal_faq(ns("faq")),

                        ### GitHub ---------------------------------------------

                        tags$a(
                            class  = "btn btn-outline-secondary app-btn",
                            href   = default_urls$code,
                            target = "_blank",
                            bsicons::bs_icon("github", a11y = "sem")
                        ) |>
                        bslib::tooltip(
                            id        = ns("btn_code_tooltip"),
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
server_title <- function(id, langs = tr$native_languages) {
    server <- \(input, output, session) {
        # UI Parameters --------------------------------------------------------

        # Each language is tied to a button which can
        # be captured by input$btn_lang_<lang>. These
        # inputs represent click events to be attached
        # to the reactive value returning the current
        # language and to observers listening to these
        # clicks (see below).
        lang_btn_ids <- sprintf("btn_lang_%s", names(langs))

        # Update lang whenever one of the related buttons is clicked.
        lang <- shiny::reactive({
            get_lang()
        }) |>
        # Create a list where the first element is the reactive above,
        # and further elements are unevaluated input[[btn_lang_<lang>]]
        # calls created using partial substitution with bquote().
        c(
            x = _,
            lapply(lang_btn_ids, \(btn_id) {
                bquote(input[[.(btn_id)]])
            })
        ) |>
        # Pass this list as arguments to shiny::bindEvent() with do.call().
        do.call(shiny::bindEvent, args = _)

        # Update mode whenever one of the related buttons is clicked.
        mode <- shiny::reactive({
            get_mode()
        }) |>
        shiny::bindEvent(input$btn_mode_default, input$btn_mode_simplified)

        # Update color whenever the related button is clicked.
        color <- shiny::reactive({
            get_color("current")
        }) |>
        shiny::bindEvent(input$btn_color)

        # Observers that set/control UI parameters (below) have a higher
        # priority to ensure they are always executed first (before all
        # other observers and reactive expressions). This is because
        # values must always be updated with set_*() functions first.

        # Apply UI parameters passed as query parameters.
        shiny::observe(priority = 10L, {
            # Extract parameters from the URL.
            query_params <- shiny::getQueryString()

            # Validate and set extracted parameters.
            set_lang(parse_lang(query_params$lang))
            set_mode(parse_mode(query_params$mode))
            color <- set_color(parse_color(query_params$color))

            # Update the URL with valid values. Some
            # values could had been invalid initially.
            update_query_string()

            # Update the color mode and the label
            # of the button controlling it.
            bslib::toggle_dark_mode(color[["current"]])
            shiny::updateActionButton(
                inputId = "btn_color",
                label   = color[["label"]]
            )
        }) |>
        # After execution, the user may update current
        # parameters by using the buttons of the module.
        shiny::bindEvent(session$clientData$url_search, once = TRUE)

        # Update the current language.
        # Each language has a dedicated button requiring its own observer.
        mapply(
            lang   = names(langs),
            btn_id = lang_btn_ids,
            \(lang, btn_id) {
                shiny::observe(priority = 10L, {
                    update_query_string(lang = set_lang(lang))
                }) |>
                shiny::bindEvent(input[[btn_id]], ignoreInit = TRUE)
            }
        )

        # Update the current mode.
        # Each mode has a dedicated button.
        shiny::observe(priority = 10L, {
            update_query_string(mode = set_mode("default"))
        }) |>
        shiny::bindEvent(input$btn_mode_default, ignoreInit = TRUE)

        shiny::observe(priority = 10L, {
            update_query_string(mode = set_mode("simplified"))
        }) |>
        shiny::bindEvent(input$btn_mode_simplified, ignoreInit = TRUE)

        # Update the current color mode.
        shiny::observe(priority = 10L, {
            # Passing a NULL toggles the state.
            color <- set_color(NULL)

            update_query_string(color = color[["current"]])

            bslib::toggle_dark_mode(color[["current"]])
            shiny::updateActionButton(
                inputId = "btn_color",
                label   = color[["label"]]
            )
        }) |>
        shiny::bindEvent(input$btn_color, ignoreInit = TRUE)

        # Modules --------------------------------------------------------------

        # Module is loaded after defining
        # lang() because it is required.
        server_modal_faq("faq", lang)

        # Outputs and Other Observers ------------------------------------------

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

        output$btn_modes_label <- shiny::renderText({
            translate(lang = lang(), "Mode")
        }) |>
        shiny::bindCache(lang())

        output$btn_mode_default_label <- shiny::renderText({
            translate(lang = lang(), "Default")
        }) |>
        shiny::bindCache(lang())

        output$btn_mode_simplified_label <- shiny::renderText({
            translate(lang = lang(), "Simplified")
        }) |>
        shiny::bindCache(lang())

        output$btn_langs_label <- shiny::renderText({
            translate(lang = lang(), "Language")
        }) |>
        shiny::bindCache(lang())

        output$nav_item_expostats_links <- shiny::renderUI({
            lang <- lang()
            links <- list(
                list(
                    href  = default_urls$tool2[[lang]],
                    label = translate(lang = lang, "Tool 2")
                ),
                list(
                    href  = default_urls$tool3[[lang]],
                    label = translate(lang = lang, "Tool 3")
                ),
                list(
                    href  = default_urls$expostats[[lang]],
                    label = "Expostats"
                ),
                list(
                    href  = default_urls$ndexpo,
                    label = "NDExpo"
                )
            )

            lapply(links, \(link) {
                return(
                    tags$li(
                        tags$a(
                            class    = "dropdown-item",
                            href     = link$href,
                            hreflang = lang,
                            rel      = "external",
                            target   = "_blank",
                            link$label
                        )
                    )
                )
            })
        }) |>
        shiny::bindCache(lang())

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            lang <- lang()

            bslib::update_tooltip("btn_color_tooltip", translate(lang = lang, "
                Toggle the current color scheme (light or dark).
            "))

            bslib::update_tooltip("btn_code_tooltip", translate(lang = lang, "
                See the source code of Tool 1 on GitHub.
            "))
        }) |>
        shiny::bindEvent(lang())

        return(
            list(
                lang  = lang,
                mode  = mode,
                color = color
            )
        )
    }

    return(shiny::moduleServer(id, server))
}
