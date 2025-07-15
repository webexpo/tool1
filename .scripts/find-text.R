#' Find Text That Requires Translations
#'
#' Scan the source code, create a new [transltr::Translator] object and create
#' required translations files.
#'
#' @details
#' This creates or updates the contents of directory i18n/.
#'
#' Support further languages by adding a new entry to the default value of
#' formal argument `other_lang_names` of [.find()] and calling it.
#'
#' @param id A character string. It is passed to argument `id` of
#'   [transltr::translator()].
#'
#' @param source_lang_code A character string. The code of the source language.
#'
#' @param source_lang_name A character string. The native name of the source
#'   language.
#'
#' @param other_lang_names A named list of character strings. Further languages
#'   supported by Tool 1.
#'
#'   * Elements are the full native names of the languages.
#'   * Names are the corresponding language codes. Always use BCP-47 codes.
#'
#' @returns A [transltr::Translator] object, invisibly. It is also bound to
#'   variable `tr` in the global environment automatically. This keeps the
#'   main [transltr::Translator] object of the project always up-to-date.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @seealso
#' [The transltr package](https://cran.r-project.org/web/packages/transltr/index.html),
#' [BCP-47/IETF language tags](https://en.wikipedia.org/wiki/IETF_language_tag#List_of_common_primary_language_subtags)
#'
#' @examples
#' .find()
.find <- function(
    id               = "expostats:tool1",
    source_lang_code = "en",
    source_lang_name = "English",
    # Add entries to this argument to support more languages.
    other_lang_names = list(
        fr = "Français",
        es = "Español"
    ))
{
    path <- getOption("transltr.path")

    # Get the directory holding translations.
    dir <- dirname(path)

    # Create a new (empty) Translator object.
    tr <- transltr::translator(id = id)

    # Register native names of languages that must be supported.
    do.call(
        what = tr$set_native_languages,
        args = structure(
            c(as.list(source_lang_name), other_lang_names),
            names = c(source_lang_code, names(other_lang_names))
        )
    )

    # Detect existing translation files in i18n/.
    # The source language (English) never has one.
    files <- file.path(dir, sprintf("%s.txt", names(other_lang_names)))
    files <- files[utils::file_test("-f", files)]

    # Extract source text to translate from source scripts
    # and update the Translator object (by reference).
    transltr::find_source(tr = tr, interface = quote(translate))

    # Read existing translations and import them
    # back into the Translator object created above.
    # The latter is updated by reference.
    lapply(files, transltr::translations_read, tr = tr)

    # Export source text and translations.
    # This updates the contents of i18n/.
    transltr::translator_write(tr, overwrite = TRUE)

    # Update any previous Translator object
    # defined in the global environment.
    assign("tr", tr, globalenv())

    return(invisible(tr))
}
