#' Tool1: Data Interpretation for One Similar Exposure Group (SEG)
#'
#' @description
#' User interface and server-side logic.
#'
#' @usage
#' ## In interactive sessions
#' .run()
#'
#' @details
#' Shiny modules, global constants, functions, and other objects are stored
#' in R/. They are loaded automatically by [shiny::runApp()] and `.run()`.
#' They are also sourced into the global environment for development purposes
#' in [interactive()] R sessions. See .Rprofile for more information.
#'
#' Tool 1 depends on a large set of functions stemming from previous projects.
#' For historical reasons, these are stored in scripts/ and are not structured
#' in a standard way. They are explicitly sourced at runtime (see R/global.R).
#'
#' ## Development Scripts
#'
#' Development scripts are stored in .scripts/ (not scripts/ !) and automate
#' various actions. They are not required at runtime.
#'
#' .Rprofile defines small helper functions to seamlessly run them.
#'
#' ## Static Assets
#'
#' Static assets are stored in www/ and served under the root URL at runtime.
#'
#' ## Naming Conventions
#'
#' `snake_case_with_lower_cases` is used at all times.
#'
#' CSS classes use `dash-case-with-lower-cases` for consistency with usual
#' best practices in web development. Each CSS class name must begin with
#' the `app-` prefix (when possible).
#'
#' For historical reasons, R objects defined in scripts/ use a different
#' naming convention. Avoid it.
#'
#' ## Namespaces
#'
#' For historical reasons, scripts stored in scripts/ do not reference the
#' namespace (the package) of the functions they call. Consequently, some
#' packages must be attached to the search path. This is a bad practice from
#' which Tool 1 is moving away.
#'
#' ```
#' # Good
#' transltr::language_source_get()
#'
#' # Bad
#' language_source_get()
#' ```
#'
#' @format
#' `ui` is a `bslib_page` object (an output of [bslib::page_sidebar()]).
#' This is a list of `shiny.tag` objects.
#.
#' @returns
#' [server()] returns `NULL`, invisibly.
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname app
#'
#' @export
ui <- bslib::page_sidebar(
    # lang and window_title are both updated
    # by server() based on the chosen lang.
    lang         = default_lang,
    window_title = "Expostats - Tool 1",
    theme        = bslib::bs_theme(5L, "shiny"),
    title        = ui_title("layout_title"),
    sidebar      = ui_sidebar("layout_sidebar"),

    # <head> -------------------------------------------------------------------

    tags$head(
        tags$link(
            rel  = "manifest",
            href = "site.webmanifest"
        ),

        tags$link(
            rel   = "apple-touch-icon",
            sizes = "180x180",
            href  = "apple-touch-icon.png"
        ),

        tags$link(
            rel   = "icon",
            type  = "image/png",
            sizes = "32x32",
            href  = "favicon-32x32.png"
        ),

        tags$link(
            rel   = "icon",
            type  = "image/png",
            sizes = "16x16",
            href  = "favicon-16x16.png"
        ),

        tags$link(
            rel   = "stylesheet",
            media = "all",
            href  = "main.css"
        ),

        tags$script(src = "main.js"),

        shinyjs::useShinyjs()
    ),

    # <main> -------------------------------------------------------------------

    # Banner shown whenever the Shiny engine is blocked.
    ui_banner("busy_banner"),

    bslib::navset_card_underline(
        id       = "panel_active",
        selected = "panel_stats",
        header   = bslib::card_header(
            class = "d-flex align-items-center",
            style = "gap: 8px;",

            # Current mode.
            tags$div(
                class = "d-flex alert alert-primary p-2 m-0",

                tags$span(
                    class = "pe-1",
                    bsicons::bs_icon(
                        name = "layout-text-window-reverse",
                        a11y = "deco"
                    )
                ),
                shiny::textOutput("panel_title_mode", tags$span)
            ),

            # Current panel.
            bslib::card_title(
                container = tags$h2,
                class     = "m-0 fs-5 opacity-75",
                shiny::textOutput("panel_title", tags$span)
            )
        ),

        ui_panel_descriptive_statistics("panel_stats"),

        # This panel is hidden by default.
        # It is shown if mode() returns "simplified".
        ui_panel_simplified("panel_simplified"),

        # Group other default inference panels.
        # It is shown by default and hidden if mode() returns "simplified".
        bslib::nav_menu(
            value = "panels_menu",
            title = shiny::textOutput("panels_menu_title", tags$span),
            icon  = tags$span(
                class = "pe-1",
                bsicons::bs_icon(name = "list", a11y = "deco")
            ),

            ui_panel_exceedance_fraction("panel_fraction"),
            ui_panel_percentiles("panel_percentiles"),
            ui_panel_arithmetic_mean("panel_mean")
        )
    )
)

#' @rdname app
#' @export
server <- function(input, output, session) {
    data_sample <- shiny::reactive({
        # calc_parameters() is a shiny::reactive object
        # returned by the Sidebar module below. It holds
        # all user input values.
        calc_parameters <- calc_parameters()

        # oel.mult is set equal to 1 until Webexpo scripts
        # are rewritten. In what follows, c.oel is ignored
        # and input$oel is used instead.
        data.formatting.SEG(
            data.in  = calc_parameters$data,
            oel      = calc_parameters$oel,
            oel.mult = 1L
        )
    })

    bayesian_analysis <- shiny::reactive({
        data_sample <- data_sample()

        fun.bayes.jags(
            observations  = data_sample$data,
            notcensored   = data_sample$notcensored,
            leftcensored  = data_sample$leftcensored,
            rightcensored = data_sample$rightcensored,
            intcensored   = data_sample$intcensored,
            seed          = data_sample$seed,
            c.oel         = calc_parameters()$oel,
            n.iter        = default_n_bayes_iter
        )
    })

    num_results <- shiny::reactive({
        calc_parameters <- calc_parameters()
        bayesian_analysis <- bayesian_analysis()

        all.numeric(
            mu.chain       = bayesian_analysis$mu.chain,
            sigma.chain    = bayesian_analysis$sigma.chain,
            c.oel          = calc_parameters$oel,
            conf           = calc_parameters$conf,
            frac_threshold = calc_parameters$frac_threshold,
            target_perc    = calc_parameters$target_perc
        )
    })

    estimates_params <- shiny::reactive({
        num_results <- num_results()

        gm  <- lapply(num_results$gm,  signif, digits = default_n_digits)
        gsd <- lapply(num_results$gsd, signif, digits = default_n_digits)

        list(
            gm  = sprintf("%s [%s - %s]", gm$est,  gm$lcl,  gm$ucl),
            gsd = sprintf("%s [%s - %s]", gsd$est, gsd$lcl, gsd$ucl)
        )
    })

    # Modules ------------------------------------------------------------------

    # This returns a list of shiny::reactive()
    # objects that can be used to get the UI's
    # current language, mode, and color.
    ui_parameters <- server_title("layout_title")

    lang <- ui_parameters$lang
    mode <- ui_parameters$mode

    # This returns a shiny::reactive() object
    # returning a named list containing all
    # user inputs in a list.
    calc_parameters <- server_sidebar(
        id   = "layout_sidebar",
        lang = lang,
        mode = mode,
        # This input can only be accessed
        # from within a reactive context.
        panel_active = shiny::reactive({ input$panel_active })
    )

    server_banner(
        id   = "busy_banner",
        lang = lang
    )

    # Each server_panel_*() function below returns a
    # shiny::reactive() object that can be called to
    # get the underlying panel's title.
    panel_stats_title <- server_panel_descriptive_statistics(
        id          = "panel_stats",
        lang        = lang,
        parameters  = calc_parameters,
        data_sample = data_sample
    )

    panel_simplified_title <- server_panel_simplified(
        id                = "panel_simplified",
        lang              = lang,
        parameters        = calc_parameters,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    panel_fraction_title <- server_panel_exceedance_fraction(
        id                = "panel_fraction",
        lang              = lang,
        parameters        = calc_parameters,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    panel_percentiles_title <- server_panel_percentiles(
        id                = "panel_percentiles",
        lang              = lang,
        parameters        = calc_parameters,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    panel_mean_title <- server_panel_arithmetic_mean(
        id                = "panel_mean",
        lang              = lang,
        parameters        = calc_parameters,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    # Outputs ------------------------------------------------------------------

    output$panels_menu_title <- shiny::renderText({
        translate(lang = lang(), "Statistical Inference")
    }) |>
    shiny::bindCache(lang())

    output$panel_title <- shiny::renderText({
        switch(input$panel_active,
            panel_simplified  = panel_simplified_title(),
            panel_stats       = panel_stats_title(),
            panel_fraction    = panel_fraction_title(),
            panel_percentiles = panel_percentiles_title(),
            panel_mean        = panel_mean_title(),
        )
    }) |>
    shiny::bindCache(input$panel_active, lang())

    output$panel_title_mode <- shiny::renderText({
        switch(mode(),
            default    = translate(lang = lang(), "Default"),
            simplified = translate(lang = lang(), "Simplified")
        )
    }) |>
    shiny::bindCache(mode(), lang())

    # Observers ----------------------------------------------------------------

    # Set the lang attribute of the root <html>
    # element and translate the title of the
    # browser's tab. See www/main.js for more
    # information.
    shiny::observe(priority = 1L, {
        lang <- lang()

        # Update the window's title (what the browser's tab displays).
        session$sendCustomMessage("update_page_lang", lang)

        # Update the lang attribute of the root <html> tag.
        session$sendCustomMessage(
            "update_window_title",
            sprintf("Expostats - %s", translate(lang = lang, "Tool 1")))
    }) |>
    shiny::bindEvent(lang())

    # Toggle panel(s) based on the current mode.
    shiny::observe({
        state <- switch(mode(),
            default = c(
                show = "panels_menu",
                hide = "panel_simplified"
            ),
            simplified = c(
                show = "panel_simplified",
                hide = "panels_menu"
            )
        )

        bslib::nav_show("panel_active", state[["show"]])
        bslib::nav_hide("panel_active", state[["hide"]])
        bslib::nav_select("panel_active", "panel_stats")
    }) |>
    shiny::bindEvent(mode())

    return(invisible())
}

#' @rdname app
#' @export
app <- shiny::shinyApp(ui, server)
