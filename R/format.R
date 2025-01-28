percText <- function(perc) {
    rem <- as.integer(perc %% 10L)

    if (rem == 1L) {
        suff <- gett("st")
    } else if ( rem == 2L) {
        suff <- gett("nd")
    } else if ( rem == 3L) {
        suff <- gett("rd")
    } else {
        suff <- gett("th")
    }

    return(paste0(perc, suff, " ", gett("percentile")))
}
