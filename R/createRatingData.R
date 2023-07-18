#' Create the rating definitions and rating maps used to identify the OC_SRRI proxy
#'
#' Create the rating definitions for rating agencies Moody's, Fitch an Standard and
#' Poors. The function returns the map from the pair <rating, maturity> to the iid_proxy
#' as well as the maturity classes used, i.e. [0,1), [1,3), ... .
#'
#' @param asOfDate a valid date used to extract the rating map that was in use at asOfDate.
#'  
#' @return a named list with the following variables:
#'
#' - rating.l a named list with the rating definitions and rating classes for the 3 rating agencies.
#' - matClass.v a numeric named vector used to map the maturity of the fixed income instrument in the corresponding
#' maturity class
#' - allowedInvType.v a character vector of allowed investment types. Only the investment types in the vector are
#' allowed to be mapped by the 'by_rat_mat' proxy method (see the classifier definition in allocare AMS). Currently
#' the following inv. types are allowed: "Floater", "Perpetual Bonds", "Fix-To-Float", "Mid & Long Term Notes", "Ordinary Bonds", "High Yield Bonds", "Zero Bonds", "Other Bonds".
#' - mapByCcy.l a named list with the maps <rating, maturity> --> iid_proxy for the 3 currencies EUR, USD and CHF.
#' 
#' @examples
#' ratingData.l <- createRatingData(as.Date("2021-08-30"))
#'  
#' @export
createRatingData <- function(asOfDate) {
  
  ## create the list of rating definitions and closely linked rating classes
  rating.l <- list(
      sp = list(
          "2010-01-01" = list(
              rating.v = c("AAA",	"AAAr",	"AA+",	"AA+r",	"AA",	"AAr",	"AA-",	"AA-r",	"A+",	"A+r",	"A",	"Ar",	"A-",	"A-r",				
                  "BBB+",	"BBB+r",	"BBB",	"BBBr",	"BBB-",	"BBB-r",	"BB+",	"BB+r",	"BB",	"BBr",	"BB-",	"BB-r",	"B+",	"B+r",	"B",	"Br",	"B-",	"B-r",
                  "CCC+",	"CCC+r",	"CCC",	"CCCr",	"CCC-",	"CCC-r",	"CC",	"CCr",	"C",	"Cr",	"D",	"Dr",	"N/A",	"N/R",	"NR"),
              ratClass.l = list(
                  '1. Investment grade' = c("AAA",	"AAAr",	"AA+",	"AA+r",	"AA",	"AAr",	"AA-",	"AA-r",	"A+",	"A+r",	"A",	"Ar",	"A-",	"A-r",				
                      "BBB+",	"BBB+r",	"BBB",	"BBBr",	"BBB-",	"BBB-r"),
                  '2. Non-investment grade' = c("BB+",	"BB+r",	"BB",	"BBr",	"BB-",	"BB-r",	"B+",	"B+r",	"B",	"Br",	"B-",	"B-r",
                      "CCC+",	"CCC+r",	"CCC",	"CCCr",	"CCC-",	"CCC-r",	"CC",	"CCr",	"C",	"Cr",	"D",	"Dr",	"N/A",	"N/R",	"NR")
              )
          )
      ),
      moody = list(
          "2010-01-01" = list(
              rating.v = c("Aaa",	"Aa1",	"Aa2",	"Aa3",	"A1",	"A2",	"A3",		
                  "Baa1",	"Baa2",	"Baa3",	"Ba1",	"Ba2",	"Ba3",	"B1",	"B2",	"B3",
                  "Caa1",	"Caa2",	"Caa3",	"Ca",	"C",	"N/A",	"NR",	"WR"),
              ratClass.l = list(
                  '1. Investment grade' = c("Aaa",	"Aa1",	"Aa2",	"Aa3",	"A1",	"A2",	"A3",		
                      "Baa1",	"Baa2",	"Baa3"),
                  '2. Non-investment grade' = c("Ba1",	"Ba2",	"Ba3",	"B1",	"B2",	"B3",
                      "Caa1",	"Caa2",	"Caa3",	"Ca",	"C",	"N/A",	"NR",	"WR")
              )
          
          )
      ),
      fitch = list(
          "2010-01-01" = list(
              rating.v = c("AAA",	"AA+",	"AA",	"AA-",	"A+",	"A",	"A-",		
                  "BBB+",	"BBB",	"BBB-",	"BB+",	"BB",	"BB-",	"B+",	"B",	"B-",
                  "CCC",	"DDD",	"DD",	"D",	"N/A",	"N/R",	"NR"),
              ratClass.l = list(
                  '1. Investment grade' = c("AAA",	"AA+",	"AA",	"AA-",	"A+",	"A",	"A-",		
                      "BBB+",	"BBB",	"BBB-"),
                  '2. Non-investment grade' = c("BB+",	"BB",	"BB-",	"B+",	"B",	"B-",
                      "CCC",	"DDD",	"DD",	"D",	"N/A",	"N/R",	"NR")
              )
          )
      )
  )
  
  ## create the list of maturity classes
  matClass.l <- list(
      "2010-01-01" = c('[0,1)' = 1, '[1,3)' = 3, '[3,5)' = 5, '[5,Inf)' = Inf)
  )
  
  ## create the list containing the named matrix used to map the pair <rating class, maturity class>
  ## to the iid_proxy to be used for the SRRI computation
  mapByCcy.l <- list(
      "2010-01-01" = list(
          EUR = matrix(
              c('57056', '57041', '57043', '57044',
                '57045', '57045', '57046', '57047'), 
              ncol = length(matClass.l[["2010-01-01"]]), 
              byrow = TRUE, dimnames = list(names(rating.l[["sp"]][["2010-01-01"]][["ratClass.l"]]), 
                  names(matClass.l[["2010-01-01"]]))),
          CHF = matrix(
              c('57056', '57041', '57043', '57044',
                  '57045', '57045', '57046', '57047'),
              ncol = length(matClass.l[["2010-01-01"]]), 
              byrow = TRUE, dimnames = list(names(rating.l[["sp"]][["2010-01-01"]][["ratClass.l"]]), 
                  names(matClass.l[["2010-01-01"]]))),
          USD = matrix(
              c('57048', '57048', '57049', '57051',
                  '57055', '57052', '57053', '57054'),
              ncol = length(matClass.l[["2010-01-01"]]), 
              byrow = TRUE, dimnames = list(names(rating.l[["sp"]][["2010-01-01"]][["ratClass.l"]]), 
                  names(matClass.l[["2010-01-01"]])))
      )
  )
  
  ## define the vector of allowed investment type for the "by_rat_mat" SRRI method
  allowedInvType.v <- c("Mid & Long Term Notes", "Ordinary Bonds", "High Yield Bonds", "Zero Bonds", 
    "Other Bonds", "Fix-To-Float", "Perpetual Bonds","Floater")
 
  #rating.l = lapply(rating.l, getLastDateAvailable, asOfDate)
  #matClass.v = getLastDateAvailable(matClass.l, asOfDate)
  #mapByCcy.l = getLastDateAvailable(mapByCcy.l, asOfDate)
  
  ratingData.l <- list(
      allowedInvType.v = allowedInvType.v,
      rating.l = lapply(rating.l, getLastDateAvailable, asOfDate),
      matClass.v = getLastDateAvailable(matClass.l, asOfDate),
      mapByCcy.l = getLastDateAvailable(mapByCcy.l, asOfDate)
  )
  
  
  return(ratingData.l)
  
}
