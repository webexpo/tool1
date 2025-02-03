ordinal_number <- function(value = numeric()) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value)) {
        stop("'value' must a non-NA numeric value of length 1.")
    }

    suffix <- if (value > 10.0 & value < 14.0) {
        translate("th")
    } else {
        map <- c(
            translate("th"),               # Reminder = 0.
            translate("st"),               # Reminder = 1.
            translate("nd"),               # Reminder = 2.
            translate("rd"),               # Reminder = 3.
            rep.int(translate("th"), 6L))  # Reminder = 4, 5, ..., 9.)

        map[(value %% 10L) + 1L]
    }

    return(paste0(value, suffix))
}
