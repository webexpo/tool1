#' @param results A [shiny::reactive()] object returning a named list of length
#'   3 containing the following elements.
#'
#'   \describe{
#'     \item{`asis`}{A named list. The output of [all.numeric()], a standard
#'       Expostats function. It contains outputs computed from simulations and
#'       calculation parameters.
#'     }
#'     \item{`rounded`}{A named list identical to `asis` but each element is
#'       rounded to a number of significant digits.
#'     }
#'     \item{`intervals`}{A named list of character strings. These are estimates
#'       and their underlying confidnce intervals formatted specifically for
#'       displaying purposes with [as_interval()].
#'     }
#'   }
