#' Validate Values
#'
#' Check whether values meets certain criteria.
#'
#' @param x Any \R object.
#'
#' @param allow_empty A logical value. Are empty character strings valid values?
#'
#' @returns A logical value.
#'
#' @note
#' All these functions stem from package transltr (they are unexported).
#'
#' @examples
#' is_lgl1(TRUE)
#' is_lgl1(FALSE)
#' is_lgl1(1L)
#'
#' is_int1(1L)   # TRUE
#' is_int1(1.0)  # FALSE
#'
#' is_num1(1L)   # TRUE
#' is_num1(1.0)  # TRUE
#' is_num1(c(1.3, 2.4, 3.5))
#' is_num1("Hello")
#'
#' is_chr1("")
#' is_chr1("Hello")
#' is_chr1(c("Hello", "world"))
#'
#' is_chr(character())
#' is_chr(c("a", "b", "c"))
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname is
#' @export
is_lgl1 <- function (x) {
    return(is.logical(x) && length(x) == 1L && !is.na(x))
}

#' @rdname is
#' @export
is_int1 <- function(x) {
    return(is.integer(x) && length(x) == 1L && !is.na(x))
}

#' @rdname is
#' @export
is_num1 <- function(x) {
    return(is.numeric(x) && length(x) == 1L && !is.na(x))
}

#' @rdname is
#' @export
is_chr1 <- function(x, allow_empty = FALSE) {
    return(
        is.character(x) &&
        length(x) == 1L &&
        (nzchar(x) || allow_empty) &&
        !is.na(x)
    )
}

#' @rdname is
#' @export
is_chr <- function (x, allow_empty = FALSE) {
    return(is.character(x) && (length(x) || allow_empty) && !anyNA(x))
}
