#' Tool 1: Data Interpretation for One Similar Exposure Group (SEG)
#'
#' @description
#' User interface and server-side logic.
#'
#' @usage
#' ## In interactive sessions
#' .run()
#'
#' @format
#' `ui` is a `bslib_page` object (an output of [bslib::page_sidebar()]). This
#' is a list of `shiny.tag` objects.
#.
#' @returns [server()] returns `NULL`, invisibly.
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname app
#' @export
ui <- bslib::page_sidebar(
    lang         = "en",
    window_title = "Expostats - Tool 1",
    theme        = bslib::bs_theme(5L, "shiny"),
    title        = ui_title("layout_title"),
    sidebar      = ui_sidebar("layout_sidebar"),

    # <head> -------------------------------------------------------------------

    ui_head("head"),

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
        # It is shown if mode() returns "express".
        ui_panel_express("panel_express"),

        # Group other default inference panels.
        # It is shown by default and hidden if mode() returns "express".
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
    # Step 1: Collect calculation parameters from
    # user and format the data sample it provided.
    data_sample <- shiny::reactive({
        inputs_calc <- inputs_calc()

        data.formatting.SEG(
            data.in  = inputs_calc$data,
            oel      = inputs_calc$oel,
            oel.mult = inputs_calc$oel_multiplier
        )
    })

    # Step 2: Generate Bayesian simulations of the
    # mean and standard deviation on the log scale.
    simulations <- shiny::reactive({
        data_sample <- data_sample()

        fun.bayes.jags(
            observations  = data_sample$data,
            notcensored   = data_sample$notcensored,
            leftcensored  = data_sample$leftcensored,
            rightcensored = data_sample$rightcensored,
            intcensored   = data_sample$intcensored,
            seed          = data_sample$seed,
            c.oel         = data_sample$c.oel,
            n.iter        = getOption("app_number_bayes_iter")
        )
    })

    # Step 3: Compute outputs from calculation
    # parameters and simulated values. Format
    # them for displaying purposes.
    results <- shiny::reactive({
        # data_sample() computes c.oel
        # form oel and oel_multiplier.
        data_sample <- data_sample()
        inputs_calc <- inputs_calc()
        simulations <- simulations()

        psi <- inputs_calc$psi

        results <- all.numeric(
            conf           = inputs_calc$conf,
            frac_threshold = inputs_calc$frac_threshold,
            target_perc    = inputs_calc$target_perc,
            c.oel          = data_sample$c.oel,
            mu.chain       = simulations$mu.chain,
            sigma.chain    = simulations$sigma.chain
        )

        gm   <- results$gm
        gsd  <- results$gsd
        frac <- lapply(results$frac, as_percentage)
        perc <- results$perc
        am   <- results$am

        # Formatted outputs are prefixed by a dot to
        # distinguish them from results stemming from
        # all.numeric().
        formatted <- list(
            .rounded = rapply(results, signif,
                how    = "replace",
                digits = getOption("app_n_signif_digits")
            ),
            .intervals = list(
                gm   = as_interval(gm$est, gm$lcl, gm$ucl),
                gsd  = as_interval(gsd$est, gsd$lcl, gsd$ucl),
                frac = as_interval(frac$est, frac$lcl, frac$ucl),
                perc = as_interval(perc$est, perc$lcl, perc$ucl),
                am   = as_interval(am$est, am$lcl, am$ucl)
            ),
            .risk_levels = list(
                frac    = if (results$frac.risk >= psi) 3L else 1L,
                perc    = if (results$perc.risk >= psi) 3L else 1L,
                am      = if (results$am.risk   >= psi) 3L else 1L,
                express = findInterval(results$perc.risk, get_risk_level_thresholds())
            )
        )

        c(results, formatted)
    })

    # Modules ------------------------------------------------------------------

    # This returns a list of shiny::reactive()
    # objects that can be used to get the UI's
    # current language, mode, and color.
    inputs_ui <- server_title("layout_title")

    lang <- inputs_ui$lang
    mode <- inputs_ui$mode

    # This returns a shiny::reactive() object
    # returning a named list containing all
    # user inputs in a list.
    inputs_calc <- server_sidebar(
        id           = "layout_sidebar",
        lang         = lang,
        mode         = mode,
        panel_active = shiny::reactive({ input$panel_active })
    )

    server_banner(id = "busy_banner", lang = lang)

    # Each server_panel_*() function below returns a
    # shiny::reactive() object that can be called to
    # get the underlying panel's title.
    panel_stats_title <- server_panel_descriptive_statistics(
        id          = "panel_stats",
        lang        = lang,
        inputs_calc = inputs_calc,
        data_sample = data_sample
    )

    panel_express_title <- server_panel_express(
        id          = "panel_express",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_fraction_title <- server_panel_exceedance_fraction(
        id          = "panel_fraction",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_percentiles_title <- server_panel_percentiles(
        id          = "panel_percentiles",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_mean_title <- server_panel_arithmetic_mean(
        id          = "panel_mean",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    # Outputs ------------------------------------------------------------------

    output$panels_menu_title <- shiny::renderText({
        translate(lang = lang(), "Statistical Inference")
    }) |>
    shiny::bindCache(lang())

    output$panel_title <- shiny::renderText({
        switch(input$panel_active,
            panel_express     = panel_express_title(),
            panel_stats       = panel_stats_title(),
            panel_fraction    = panel_fraction_title(),
            panel_percentiles = panel_percentiles_title(),
            panel_mean        = panel_mean_title(),
        )
    }) |>
    shiny::bindCache(input$panel_active, lang())

    output$panel_title_mode <- shiny::renderText({
        lang <- lang()
        switch(mode(),
            extended = translate(lang = lang, "Tool 1 Extended"),
            express  = translate(lang = lang, "Tool 1 Express")
        )
    }) |>
    shiny::bindCache(mode(), lang())

    # Observers ----------------------------------------------------------------

    # Update the lang attribute of the root <html> tag.
    # See www/main.js for more information.
    shiny::observe(priority = 1L, {
        session$sendCustomMessage("update_page_lang", lang())
    }) |>
    shiny::bindEvent(lang())

    # Toggle panel(s) based on the current mode.
    shiny::observe({
        state <- switch(mode(),
            extended = c(
                show = "panels_menu",
                hide = "panel_express"
            ),
            express = c(
                show = "panel_express",
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
