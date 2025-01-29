percText <- function(perc) {
    rem <- as.integer(perc %% 10L)

    if (rem == 1L) {
        suff <- translate("st")
    } else if (rem == 2L) {
        suff <- translate("nd")
    } else if (rem == 3L) {
        suff <- translate("rd")
    } else {
        suff <- translate("th")
    }

    return(paste0(perc, suff, " ", translate("percentile")))
}

    } else {
        suff <- gett("th")
    }

    return(paste0(perc, suff, " ", gett("percentile")))
}
