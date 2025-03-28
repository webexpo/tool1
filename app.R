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
#' Development scripts are stored in .scripts/. These are not required at
#' runtime. Helper functions that can be called to source them are attached
#' to the search path of [interactive()] R sessions. See .Rprofile for more
#' information.
#'
#' ## R Objects
#'
#' Shiny modules, global constants, helper functions, and other objects
#' are stored in R/. They are loaded automatically by [shiny::runApp()]
#' and `.run()`.
#'
#' Tool 1 depends on a large set of functions stemming from previous projects.
#' For historical reasons, these are stored in scripts/ and are not structured
#' in a standard way. They are explicitly loaded at runtime by R/global.R.
#'
#' ## Static Assets
#'
#' Static assets are stored in www/ and served under the root URL at runtime.
#'
#' ## Naming Conventions
#'
#' The `snake_case_with_lower_cases` naming pattern is used at all times. CSS
#' classes uses `dash-case-with-lower-cases` for consistency with usual best
#' practices in web development. Each class name must begin with `app-`.
#'
#' For historical reasons, R objects defined in scripts/ use a different
#' naming convention. Avoid it.
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

ui <- bslib::page_sidebar(
    # lang and window_title are updated
    # with session$sendCustomMessage().
    lang         = NULL,
    window_title = NULL,
    theme        = bslib::bs_theme(version = 5L, preset = "shiny"),
    title        = ui_title("layout_title"),
    sidebar      = ui_sidebar("layout_sidebar"),

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

    # It is shown whenever the Shiny engine is blocked.
    ui_banner("busy_banner"),

    bslib::navset_card_underline(
        id       = "panel_active",
        selected = "panel_stats",

        ui_panel_statistics("panel_stats"),
        ui_panel_exceedance_fraction("panel_fraction"),
        ui_panel_percentiles("panel_percentiles"),
        ui_panel_arithmetic_mean("panel_mean")
    )
)

server <- function(input, output, session) {
    # Values -------------------------------------------------------------------

    lang <- shiny::reactive({
        # lang is extracted from URL's search paramter ?lang.
        lang <- shiny::parseQueryString(session$clientData$url_search)$lang

        # If lang is not supplied or does not match
        # any supported language, the default_lang
        # is used and ?lang is updated accordingly.
        if (is.null(lang) || !match(lang, names(tr$native_languages), 0L)) {
            shiny::updateQueryString(sprintf("?lang=%s", default_lang))
            return(default_lang)
        }

        return(lang)
    }) |>
    shiny::bindEvent(session$clientData$url_search)

    data_sample <- shiny::reactive({
        parameters <- parameters()

        # oel.mult is set equal to 1 until Webexpo scripts
        # are rewritten. In what follows, c.oel is ignored
        # and input$oel is used instead.
        data.formatting.SEG(
            data.in  = parameters$data,
            oel      = parameters$oel,
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
            c.oel         = parameters()$oel,
            n.iter        = default_n_bayes_iter
        )
    })

    num_results <- shiny::reactive({
        parameters <- parameters()
        bayesian_analysis <- bayesian_analysis()

        all.numeric(
            mu.chain       = bayesian_analysis$mu.chain,
            sigma.chain    = bayesian_analysis$sigma.chain,
            c.oel          = parameters$oel,
            conf           = parameters$conf,
            frac_threshold = parameters$frac_threshold,
            target_perc    = parameters$target_perc
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

    server_title("layout_title", lang)

    server_banner("busy_banner", lang)

    # This returns a shiny::reactive()
    # that itself returns all user inputs.
    parameters <- server_sidebar(
        id           = "layout_sidebar",
        lang         = lang,
        panel_active = shiny::reactive({ input$panel_active })
    )

    server_panel_statistics(
        id          = "panel_stats",
        lang        = lang,
        parameters  = parameters,
        data_sample = data_sample
    )

    server_panel_exceedance_fraction(
        id                = "panel_fraction",
        lang              = lang,
        parameters        = parameters,
        data_sample       = data_sample,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    server_panel_percentiles(
        id                = "panel_percentiles",
        lang              = lang,
        parameters        = parameters,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    server_panel_arithmetic_mean(
        id                = "panel_mean",
        lang              = lang,
        parameters        = parameters,
        bayesian_analysis = bayesian_analysis,
        num_results       = num_results,
        estimates_params  = estimates_params
    )

    # Observers ----------------------------------------------------------------

    # See www/main.js for more information.
    shiny::observe({
        # Update the window's title (what the browser's tab displays).
        session$sendCustomMessage("update_page_lang", lang())

        # Update the lang attribute of the root <html> tag.
        session$sendCustomMessage(
            "update_window_title",
            sprintf("Expostats - %s", translate(lang = lang(), "Tool 1")))
    }) |>
    shiny::bindEvent(lang())
}

# Create a shiny.appobj object that
# can be passed to shiny::runApp().
app <- shiny::shinyApp(ui, server)
