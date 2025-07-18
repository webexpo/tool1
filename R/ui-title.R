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
#' This module relies on a set of internal functions to parse, set, and get
#' internal values controlling the user interface's state. They are defined
#' in `R/ui-title-internals.R` and prefixed by a dot. To fetch UI parameters
#' outside of [server_title()], use the [shiny::reactive()] objects it returns.
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
#' ## Languages
#'
#' Three actions are required when adding a new language.
#'
#'   1. Add a new dedicated [shiny::actionButton()] in [ui_title()] under
#'      section Languages. Its `id` must be `btn_lang_<lang>`. It must be
#'      wrapped in an `<li>` tag.
#'
#'   2. Bind the button created at step 1 to `lang()` by passing
#'      `input$btn_lang_<lang>` to the [shiny::bindEvent()] call.
#'
#'   3. Create an observer for the button created at step 1 and bind it
#'      to `input$btn_lang_<lang>` with a call to [shiny::bindEvent()].
#'      The observer must (1) highlight the chosen `lang` in the menu,
#'      (2) call [.set_lang()], and (3) call [update_query_string()].
#'
#' You may copy/paste the existing logic for the default language and
#' adapt it to the new `lang` value.
#'
#' Earlier versions of Tool 1 automated these tasks. While this was much safer
#' (it guaranteed that language codes and names matched expectations), it
#' required a deeper knowledge of R language objects and how shiny manipulates
#' them. It was removed in favour of simplicity.
#'
#' @template param-id
#'
#' @template param-lang-names
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
ui_title <- function(id, lang_names = tr$native_languages) {
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
                    hreflang = "en",
                    target   = "_self",

                    tags$img(
                        id     = ns("logo"),
                        src    = "assets/images/logo-400x400.png",
                        alt    = "Logo",
                        width  = "400px",
                        height = "400px",
                        style  = "height: 40px;",
                        class  = "w-auto pe-1"
                    )
                ),

                # Tool 1 is treated as a proper noun
                # that must not be translated.
                tags$span(
                    class = "fw-bolder",
                    "Tool 1"
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
                                    inputId = ns("btn_mode_express"),
                                    class   = "dropdown-item fs-6",
                                    label   = shiny::textOutput(
                                        outputId  = ns("btn_mode_express_label"),
                                        container = tags$span
                                    )
                                )
                            ),

                            tags$li(
                                shiny::actionButton(
                                    inputId = ns("btn_mode_extended"),
                                    class   = "dropdown-item fs-6",
                                    label   = shiny::textOutput(
                                        outputId  = ns("btn_mode_extended_label"),
                                        container = tags$span
                                    )
                                )
                            ),

                            tags$li(
                                tags$hr(class = "dropdown-divider")
                            ),

                            tags$p(
                                class = "px-3 mb-0 text-start",
                                shiny::textOutput(ns("btn_modes_footer"), tags$small)
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

                        # Labels must not be translated.
                        tags$ul(
                            class = "dropdown-menu dropdown-menu-end",

                            tags$li(
                                shiny::actionButton(
                                    inputId = ns("btn_lang_en"),
                                    class   = "dropdown-item fs-6",
                                    label   = lang_names[["en"]]
                                )
                            ),

                            tags$li(
                                shiny::actionButton(
                                    inputId = ns("btn_lang_fr"),
                                    class   = "dropdown-item fs-6",
                                    label   = lang_names[["fr"]]
                                )
                            ),

                            tags$li(
                                shiny::actionButton(
                                    inputId = ns("btn_lang_es"),
                                    class   = "dropdown-item fs-6",
                                    label   = lang_names[["es"]]
                                )
                            ),

                            tags$li(
                                tags$hr(class = "dropdown-divider")
                            ),

                            tags$p(
                                class = "px-3 mb-0 text-start",
                                shiny::textOutput(ns("btn_langs_footer"), tags$small)
                            )
                        )
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

                        # Labels must not be translated.
                        tags$ul(
                            class = "dropdown-menu dropdown-menu-end",

                            shiny::uiOutput(
                                outputId  = ns("a_link_tool2"),
                                container = tags$li
                            ),

                            shiny::uiOutput(
                                outputId  = ns("a_link_tool3"),
                                container = tags$li
                            ),

                            tags$li(
                                tags$hr(class = "dropdown-divider")
                            ),

                            shiny::uiOutput(
                                outputId  = ns("a_link_expostats"),
                                container = tags$li
                            ),

                            tags$li(
                                tags$a(
                                    class    = "dropdown-item",
                                    href     = urls$ndexpo,
                                    hreflang = "en",
                                    rel      = "external",
                                    target   = "_blank",
                                    "NDExpo"
                                )
                            )

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
                            href   = urls$code,
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
server_title <- function(id) {
    server <- \(input, output, session) {
        # UI Parameters --------------------------------------------------------

        # Update lang whenever one of the related buttons is clicked.
        lang <- shiny::reactive({
            .get_lang()
        }) |>
        shiny::bindEvent(
            session$clientData$url_search,
            input$btn_lang_en,
            input$btn_lang_fr,
            input$btn_lang_es
        )

        # Update mode whenever one of the related buttons is clicked.
        mode <- shiny::reactive({
            .get_mode()
        }) |>
        shiny::bindEvent(
            input$btn_mode_extended,
            input$btn_mode_express
        )

        # Update color whenever the related button is clicked.
        color <- shiny::reactive({
            .get_color()
        }) |>
        shiny::bindEvent(input$btn_color)

        # Observers that set/control UI parameters (below) have a higher
        # priority to ensure they are always executed first (before all
        # other observers and reactive expressions). This is because
        # values must always be updated with .set_*() functions first.

        # Apply UI parameters passed as query parameters.
        shiny::observe(priority = 10L, {
            # Extract parameters from the URL.
            query_params <- shiny::getQueryString()

            # Validate and set extracted parameters.
            lang  <- .set_lang(.parse_lang(query_params$lang))
            mode  <- .set_mode(.parse_mode(query_params$mode))
            color <- .set_color(.parse_color(query_params$color))

            # Update the URL with valid values. Some
            # values could had been invalid initially.
            update_query_string()

            # Highlight lang and mode parameters
            # in their respective dropdown menus.
            shinyjs::addClass(sprintf("btn_lang_%s", lang), "active")
            shinyjs::addClass(sprintf("btn_mode_%s", mode), "active")

            # Update the color mode and the label
            # of the button controlling it.
            bslib::toggle_dark_mode(color)
            shiny::updateActionButton(
                inputId = "btn_color",
                label   = attr(color, "icon", TRUE)
            )
        }) |>
        # After execution, the user may update current
        # parameters by using the buttons of the module.
        shiny::bindEvent(session$clientData$url_search, once = TRUE)

        # Update the current language.
        # Each language has a dedicated button.
        shiny::observe(priority = 10L, {
            old_lang <- .get_lang()

            shinyjs::removeClass(sprintf("btn_lang_%s", old_lang), "active")
            shinyjs::addClass("btn_lang_en", "active")
            update_query_string(lang = .set_lang("en"))
        }) |>
        shiny::bindEvent(input$btn_lang_en, ignoreInit = TRUE)

        shiny::observe(priority = 10L, {
            old_lang <- .get_lang()

            shinyjs::removeClass(sprintf("btn_lang_%s", old_lang), "active")
            shinyjs::addClass("btn_lang_fr", "active")
            update_query_string(lang = .set_lang("fr"))
        }) |>
        shiny::bindEvent(input$btn_lang_fr, ignoreInit = TRUE)

        shiny::observe(priority = 10L, {
            old_lang <- .get_lang()

            shinyjs::removeClass(sprintf("btn_lang_%s", old_lang), "active")
            shinyjs::addClass("btn_lang_es", "active")
            update_query_string(lang = .set_lang("es"))
        }) |>
        shiny::bindEvent(input$btn_lang_es, ignoreInit = TRUE)

        # Update the current mode.
        # Each mode has a dedicated button.
        shiny::observe(priority = 10L, {
            shinyjs::addClass("btn_mode_extended", "active")
            shinyjs::removeClass("btn_mode_express", "active")
            update_query_string(mode = .set_mode("extended"))
        }) |>
        shiny::bindEvent(input$btn_mode_extended, ignoreInit = TRUE)

        shiny::observe(priority = 10L, {
            shinyjs::addClass("btn_mode_express", "active")
            shinyjs::removeClass("btn_mode_extended", "active")
            update_query_string(mode = .set_mode("express"))
        }) |>
        shiny::bindEvent(input$btn_mode_express, ignoreInit = TRUE)

        # Update the current color mode.
        shiny::observe(priority = 10L, {
            # Passing a NULL toggles the state.
            color <- .set_color(NULL)

            update_query_string(color = color)
            bslib::toggle_dark_mode(color)
            shiny::updateActionButton(
                inputId = "btn_color",
                label   = attr(color, "icon", TRUE)
            )
        }) |>
        shiny::bindEvent(input$btn_color, ignoreInit = TRUE)

        # Modules --------------------------------------------------------------

        server_modal_faq("faq", lang)

        # Outputs and Other Observers ------------------------------------------

        btn_color_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Toggle the current theme of the interface (light or dark).
            ")
        }) |>
        shiny::bindCache(lang())

        btn_code_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                See the source code of Tool 1 on GitHub (English only).
            ")
        }) |>
        shiny::bindCache(lang())

        output$title <- shiny::renderText({
            translate(lang = lang(), "
                Data Interpretation for One Similar Exposure Group
            ")
        }) |>
        shiny::bindCache(lang())

        output$btn_langs_label <- shiny::renderText({
            translate(lang = lang(), "Language")
        }) |>
        shiny::bindCache(lang())

        output$btn_langs_footer <- shiny::renderText({
            translate(lang = lang(), "More languages are coming soon.")
        }) |>
        shiny::bindCache(lang())

        output$btn_modes_label <- shiny::renderText({
            translate(lang = lang(), "Mode")
        }) |>
        shiny::bindCache(lang())

        output$btn_modes_footer <- shiny::renderText({
            translate(lang = lang(), "The default mode is Tool 1 Express.")
        }) |>
        shiny::bindCache(lang())

        output$btn_mode_extended_label <- shiny::renderText({
            translate(lang = lang(), "Tool 1 Extended")
        }) |>
        shiny::bindCache(lang())

        output$btn_mode_express_label <- shiny::renderText({
            translate(lang = lang(), "Tool 1 Express")
        }) |>
        shiny::bindCache(lang())

        output$a_link_tool2 <- shiny::renderUI({
            lang <- lang()
            tags$a(
                class = "dropdown-item",
                href  = i18n_url(
                    "https://lavoue.shinyapps.io/Tool2v3En/",
                    fr = "https://lavoue.shinyapps.io/Tool2v3Fr/"
                )[[lang]],
                hreflang = lang,
                rel      = "external",
                target   = "_blank",
                "Tool 2"
            )
        }) |>
        shiny::bindCache(lang())

        output$a_link_tool3 <- shiny::renderUI({
            lang <- lang()
            tags$a(
                class = "dropdown-item",
                href  = i18n_url(
                    "https://lavoue.shinyapps.io/Tool3v3En/",
                    fr = "https://lavoue.shinyapps.io/Tool3v3Fr/"
                )[[lang]],
                hreflang = lang,
                rel      = "external",
                target   = "_blank",
                "Tool 3"
            )
        }) |>
        shiny::bindCache(lang())

        output$a_link_expostats <- shiny::renderUI({
            lang <- lang()
            tags$a(
                class    = "dropdown-item",
                href     = urls$expostats[[lang]],
                hreflang = lang,
                rel      = "external",
                target   = "_blank",
                "Expostats"
            )
        }) |>
        shiny::bindCache(lang())

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            bslib::update_tooltip("btn_color_tooltip", btn_color_tooltip_text())
            bslib::update_tooltip("btn_code_tooltip", btn_code_tooltip_text())
        })

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
