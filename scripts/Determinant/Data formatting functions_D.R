#
#
###################################### raw data processing
#
#
#                  Determinant analysis tools
#
#
# V1.00   20 march 2018
# V1.01   8 mai 2018 correcting data.formatting.D
#
#

## libraries

library(bitops)
library(openssl)

data.formatting.D <-function(data.in , oel , oel.mult, VarOfInterest ) {


        #data.in <-data.sample

        #oel <-0.6

        #oel.mult <-1

       # VarOfInterest <-"job"


######  Part B :  Information about censorship for further analysis

      #preparing the result object

        result <-list()

        result$data <-unlist(data.in[,1])

        dataforseed <-paste(data.in,collapse = "")

        ## corrected OEL
        result$c.oel <-oel*oel.mult

        result$leftcensored <-grepl('<' , result$data, fixed = TRUE)
        result$rightcensored <-grepl('>' , result$data , fixed = TRUE)
        result$intcensored <-grepl('[' , result$data , fixed = TRUE)
        result$notcensored <-!(result$leftcensored | result$rightcensored | result$intcensored)
        result$seed <-genererHash(dataforseed)
        result$var <-unlist(data.in[,VarOfInterest])

##### pat C : unique seed to obtain invariant bayesian results


return(result)


}


###################   FONCTIONS DE SOUTIEN


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

select.cat.formatted <-function(data.formatted , cat) {

  result <- data.formatted

  restrict <- data.formatted$var==cat

  result$data <- result$data[ restrict ]

  result$notcensored <- result$notcensored[restrict]

  result$leftcensored <- result$leftcensored[restrict]

  result$rightcensored <- result$rightcensored[restrict]

  result$intcensored <- result$intcensored[restrict]

  result$var <- result$var[restrict]

  return(result)


}
