#' Bootstrap Theme
#'
#' [ui_bs_theme()] is the [`bs_theme`][bslib::bs_theme()] object used to
#' globally customize the user interface.
#'
#' [ui_icons] is a named list of predefined
#' [Boostrap Icons](https://icons.getbootstrap.com/).
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @export
# TODO: A _brand.yml file could be beneficial for future purposes.
ui_bs_theme <- bslib::bs_theme(version = 5L, preset = "shiny")

#' @rdname ui-bs-theme
#' @export
# TODO: Aria labels should be provided and translated. Since this
# would require significant changes, they are left as is for now.
ui_icons <- list(
    dark_mode  = bsicons::bs_icon("moon-fill", a11y = "sem"),
    light_mode = bsicons::bs_icon("sun-fill",  a11y = "sem"),
    globe      = bsicons::bs_icon("globe",     a11y = "sem"),
    github     = bsicons::bs_icon("github",    a11y = "sem"),
    intl       = bsicons::bs_icon("translate", a11y = "sem")
)
