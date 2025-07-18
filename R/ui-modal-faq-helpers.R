#' Read the Collaborators File
#'
#' Read the i18n/_collaborators.yml file listing all contributors who worked
#' on the translations of Tool 1 and format it into a `data.frame`.
#'
#' @param path A character string. The path to the YAML file to read.
#'
#' @template param-lang-names
#'
#' @returns A `data.frame` object.
#'
#' @seealso
#' [yaml::read_yaml()]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-modal-faq-helpers
#' @export
read_collaborators <- function(
    path       = file.path("i18n", "_collaborators.yml"),
    lang_names = tr$native_languages)
{
    stopifnot(exprs = {
        is_chr1(path)
        is_chr(lang_names)
    })

    persons <- yaml::read_yaml(path)

    # This returns a named vector of integers.
    # Names are language codes and the lengths
    # are the number of collaborators for each
    # language.
    n_persons <- lengths(persons)

    persons <- persons |>
        unlist(FALSE, FALSE) |>
        do.call(rbind, args = _) |>
        as.data.frame(optional = TRUE)

    return(
        data.frame(
            # Replicate names(n_persons)[[1]] n_persons[[1]] times,
            # names(n_persons)[[2]], n_persons[[2]] times, ..., and
            # map all corresponding language codes to their names.
            language = lang_names[rep.int(names(n_persons), n_persons)],
            role     = as.character(persons$Role),
            profile  = mapply(
                USE.NAMES = FALSE,
                SIMPLIFY  = TRUE,
                href      = persons$Profile,
                name      = persons$Name,
                \(href, name) {
                    if (is.null(href)) {
                        return(name)
                    }

                    return(as.character(ui_link(href, name)))
                }
            ),
            email = vapply(
                FUN.VALUE = NA_character_,
                USE.NAMES = FALSE,
                persons$Email,
                \(email) {
                    if (is.null(email)) {
                        return("—")
                    }

                    return(as.character(ui_link_mailto(email, email)))
                }
            )
        )
    )
}
