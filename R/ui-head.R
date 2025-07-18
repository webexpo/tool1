#' Head Module
#'
#' This module controls the Head component which is a collection of tags to
#' include in the `<head>` HTML tag of Tool 1.
#'
#' @details
#' Shiny automatically adds many other `<meta>` tags already. These are not
#' included in [ui_head()].
#'
#' Since these tags target developers, search engines, and automated systems,
#' they are not translated.
#'
#' The Head Module is static. It has no corresponding `server_head()` function
#' like other modules. Consequently, `id` is required (and checked) for
#' consistency, but ignored otherwise.
#'
#' @template param-id
#'
#' @template param-lang-names
#'
#' @returns
#' [ui_head()] returns a `shiny.tag` object.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-head
#' @export
ui_head <- function(id, lang_names = tr$native_languages) {
    ns <- shiny::NS(id)
    langs <- names(lang_names)
    head <- htmltools::singleton(
        tags$head(
            # General Metadata -------------------------------------------------

            tags$meta(
                name    = "author",
                content = "Jérôme Lavoué, Jean-Mathieu Potvin"
            ),

            tags$meta(
                name    = "description",
                content = "Tool 1 is an open-source and free-to-use web application that facilitates the interpretation of industrial hygiene measurements, particularly in assessing compliance with occupational exposure limits (OEL)."
            ),

            tags$meta(
                name    = "keywords",
                content = "exceedance fraction, percentile, lognormal distribution, occupational exposure compliance, risk assessment, sampling strategies"
            ),

            tags$meta(
                name    = "version",
                content = getOption("app_version")
            ),

            tags$meta(
                name    = "release-date",
                content = getOption("app_release_date")
            ),

            tags$meta(
                name    = "theme-color",
                content = "#f8f8f8"
            ),

            # Google Tags ------------------------------------------------------

            tags$meta(
                itemprop = "name",
                content  = "Tool 1: Data Interpretation for One Similar Exposure Group (SEG)"
            ),

            tags$meta(
                itemprop = "description",
                content  = "Tool 1 is an open-source and free-to-use web application that facilitates the interpretation of industrial hygiene measurements, particularly in assessing compliance with occupational exposure limits (OEL)."
            ),

            tags$meta(
                itemprop = "image",
                content  = "/assets/images/logo-400x400.png"
            ),

            # Open Graph Protocol ----------------------------------------------

            tags$meta(
                property = "og:title",
                content  = "Tool 1: Data Interpretation for One Similar Exposure Group (SEG)"
            ),

            tags$meta(
                property = "og:type",
                content  = "website"
            ),

            tags$meta(
                property = "og:url",
                content  = "https://lavoue.shinyapps.io/tool1/"
            ),

            tags$meta(
                property = "og:image",
                content  = "assets/images/logo-400x400.png"
            ),

            tags$meta(
                property = "og:image:type",
                content  = "image/png"
            ),

            tags$meta(
                property = "og:image:alt",
                content  = "Expostats logo"
            ),

            tags$meta(
                property = "og:image:width",
                content  = "400"
            ),

            tags$meta(
                property = "og:image:height",
                content  = "400"
            ),

            tags$meta(
                property = "og:description",
                content  = "Tool 1 is an open-source and free-to-use web application that facilitates the interpretation of industrial hygiene measurements, particularly in assessing compliance with occupational exposure limits (OEL)."
            ),

            tags$meta(
                property = "og:locale",
                content  = "en"
            ),

            lapply(langs[langs != "en"], \(lang) {
                return(
                    tags$meta(
                        property = "og:locale:alternate",
                        content  = lang
                    )
                )
            }),

            tags$meta(
                property = "og:site_name",
                content  = "Expostats - Tool 1"
            ),

            # Links ------------------------------------------------------------

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

            # Scripts ----------------------------------------------------------

            tags$script(src = "main.js")
        )
    )

    return(htmltools::tagList(head, shinyjs::useShinyjs()))
}
