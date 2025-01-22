transDomain <- "default"
transDir <- "./Rtrans"

# NOTE: (JMP) Sys.setlocale() throws an error on Ubuntu 24.04.
# Since NLS is deactivated below, I also deactivated these calls until
# further notice.
#loc <- "en_CA"
#Sys.setlocale("LC_MESSAGES", loc)


# NOTE: (JMP) gett() and gettt() will soon be deprecated and are unstable
# functions that yield errors on Ubuntu 24.04. I replaced them by identity
# functions that simply returns the source text's IDs until further notice.
gett  <- \(msgid, ...) return(msgid)
gettt <- gett

# gett <- function(msgid)
# {
#   bindtextdomain(transDomain, transDir)
#   return(gettext(msgid, domain=transDomain))
#   #return(loc)
# }

# gettt <- function(msgid, ...)
# {
#   return(sprintf(gett(msgid), ...))
# }
