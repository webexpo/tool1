#
#
###################################### raw data processing
#
#
#
#           SEG specific analysis
#
# V1.01    22 mars 2018 minor correction for the seed
# V1.00   20 march 2018
#
#


## libraries

library(bitops)
library(openssl)

##



data.formatting.SEG <-function(data.in,oel,oel.mult) {

  ##data.in is in the format of what is entered in expostats too1

  ## OEL is the OEL in expostats

  ## OEL is the OEL multiplication factor


  ######  Part A :  initial formating

          ##finding the first digit


          Min <-regexpr('[0123456789]',data.in)

          if (substring(data.in,Min-1,Min-1)=='<') Min <-Min-1
          if (substring(data.in,Min-1,Min-1)=='>') Min <-Min-1
          if (substring(data.in,Min-1,Min-1)=='[') Min <-Min-1


          ##finding the last

          reva <-paste(rev(strsplit(data.in, split = "")[[1]]), collapse = "")

          Max <-nchar(data.in)-regexpr('[0123456789]',reva)+1

          if (substring(data.in,Max+1,Max+1)==']') Max <-Max+1

          #cutting...

          data.in <-substring(data.in,Min,Max)

          dataforseed <-paste("A",data.in) #to make sure dataforseed is a character string

          #splitting into observations


          data.in <- strsplit(data.in,"\n")
          data.in <- unlist(data.in)
          data.in <-data.in[!is.na(data.in)]
          data.in <-data.in[!data.in==""]


######  Part B :  Information about censorship for further analysis

      #preparing the result object

        result <-list()

        result$data <-data.in


        ## corrected OEL
        result$c.oel <-oel*oel.mult

        result$leftcensored <-grepl('<' , data.in, fixed = TRUE)
        result$rightcensored <-grepl('>' , data.in , fixed = TRUE)
        result$intcensored <-grepl('[' , data.in , fixed = TRUE)
        result$notcensored <-!(result$leftcensored | result$rightcensored | result$intcensored)

##### pat C : unique seed to obtain invariant bayesian results

        result$seed <-genererHash(dataforseed)

return(result)


}


###################   SUPPORT FUNCTIONS


genererHash <- function(measures) {
  md5 <- md5(charToRaw(measures))
  hash <- 0
  mask <- 0x08
  for ( i in md5 ) {
    highNibble <- bitShiftR(bitAnd(i, 0xF0), 4)
    lowNibble <- bitAnd(i, 0x0F);
    hash <- bitShiftL(hash, 2)
    hash <- bitOr(hash, ifelse(highNibble >= mask, 2, 0))
    hash <- bitOr(hash, ifelse(lowNibble >= mask, 1, 0))
  }
  return(hash)
}




