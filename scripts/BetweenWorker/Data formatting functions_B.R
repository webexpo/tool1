#
#
###################################### raw data processing
#
#
###########          Between worker analysis tool
#
#
#
# V1.00   20 march 2018
#
#
#

## libraries

library(bitops)
library(openssl)

##




data.formatting.B <-function(data.in,oel,oel.mult) {


######  Part A :  initial formating

          ##finding the first digit


          Min <-regexpr('[0123456789]',data.in)

          if (substring(data.in,Min-1,Min-1)=='<') Min <-Min-1
          if (substring(data.in,Min-1,Min-1)=='>') Min <-Min-1
          if (substring(data.in,Min-1,Min-1)=='[') Min <-Min-1


          ##finding the last

          reva <-paste(rev(strsplit(data.in, split = "")[[1]]), collapse = "")

          # Le dernier caractère qui est une lettre ou un chiffre indique la terminaison de la chaîne
          Max <-nchar(data.in)-regexpr('[0-9a-zA-Z]',reva)+1

          if (substring(data.in,Max+1,Max+1)==']') Max <-Max+1

          #cutting...

          data.in <-substring(data.in,Min,Max)

          dataforseed <-data.in

          #splitting into observations


          data.in <- strsplit(data.in,"\n")
          data.in <- unlist(data.in)
          data.in <-data.in[!is.na(data.in)]
          data.in <-data.in[!data.in==""]

          data.in <- strsplit(data.in,"\t")
          data.in <- unlist(data.in)

          n <-length(data.in)/2

          data.in<-data.frame(x=data.in[seq(from=1,to=(2*n-1),by=2)],
                              worker=data.in[seq(from=2,to=(2*n),by=2)],
                              stringsAsFactors=F)


######  Part B :  Information about censorship for further analysis

      #preparing the result object

        result <-list()

        result$data <-data.in


        ## corrected OEL
        result$c.oel <-oel*oel.mult

        result$leftcensored <-grepl('<' , data.in$x, fixed = TRUE)
        result$rightcensored <-grepl('>' , data.in$x , fixed = TRUE)
        result$intcensored <-grepl('[' , data.in$x , fixed = TRUE)
        result$notcensored <-!(result$leftcensored | result$rightcensored | result$intcensored)
        result$seed <-genererHash(dataforseed)

##### pat C : unique seed to obtain invariant bayesian results


return(result)


}


###################   SUPPORT FUNCTION



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

##### modifying a formatted list for selection of a group

select.worker.formatted <-function(data.formatted , worker.id) {


  result <- data.formatted

  restrict <- result$data$worker==worker.id

  result$data <- result$data[ restrict ,]

  result$notcensored <- result$notcensored[restrict]

  result$leftcensored <- result$leftcensored[restrict]

  result$rightcensored <- result$rightcensored[restrict]

  result$intcensored <- result$intcensored[restrict]

  return(result)


}


