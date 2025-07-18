#' Match an Argument Against Candidate Values
#'
#' Validate a function's argument by attempting to match it against a vector
#' of candidates inferred from the parent function (or the execution context).
#'
#' @param x A character string or `NULL`. The latter signals to use the default
#'   value.
#'
#' @param default Any \R object. The default value to return if `x` is `NULL`
#'   or invalid. If `default` is `NULL`, it is inferred from the context and
#'   set equal to the first value of the formal argument being matched.
#'
#' @returns
#' The value passed to `x` or `default` if `NULL`. If `default` is `NULL`, it
#' is inferred from the context and set equal to the first value of the formal
#' argument being matched (just like [match.arg()] does).
#'
#' @note
#' This is a refactoring of [match.arg()]. Many thanks to the original authors.
#'
#' @examples
#' foo <- function(my_arg = c("a", "b", "c")) {
#'   my_arg <- match_arg(my_arg)
#'   cat("The value of my argument is: ", my_arg, ".\n", sep = "")
#'   return(invisible())
#' }
#'
#' foo()     ## Outputs The value of my argument is: a.
#' foo("b")  ## Outputs The value of my argument is: b.
#' foo("z")  ## Outputs The value of my argument is: a.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname misc-match-arg
#' @export
match_arg <- function(x, default = NULL) {
    i_stack   <- sys.parent()
    x_formals <- formals(sys.function(i_stack))
    x_choices <- eval(x_formals[[deparse1(substitute(x))]], sys.frame(i_stack))

    if (length(x) == 1L && match(x, x_choices, 0L)) {
        return(x)
    }

    return(default %||% x_choices[[1L]])
}
