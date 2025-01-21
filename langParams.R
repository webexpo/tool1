transDomain <- "default"
transDir <- "./Rtrans"
loc <- "en_CA"
Sys.setlocale("LC_MESSAGES", loc)

gett <- function(msgid)
{
  bindtextdomain(transDomain, transDir)
  return(gettext(msgid, domain=transDomain))
  #return(loc)
}

gettt <- function(msgid, ...)
{
  return(sprintf(gett(msgid), ...))
}
