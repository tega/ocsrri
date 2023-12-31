% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createLipperInvestmentTypes.R
\name{createLipperInvestmentTypes}
\alias{createLipperInvestmentTypes}
\title{Create a data.frame with the information on which investment types are in Lipper}
\usage{
createLipperInvestmentTypes(asOfDate)
}
\arguments{
\item{asOfDate}{a valid date used to extract the situation as of asOfDate.}
}
\value{
a data frame with the columns "Value", "Description", "DownloadFromLipper"  
and "Investment Category". The column "Description" contains the name of the investment type
while "DownwloadFromLipper" contains 0 or 1 depending on the availability of the give 
investment type in the Lipper database.
}
\description{
Create a data.frame with the information on which investment types are in Lipper. This
function is mainly used to identify the investment objects whose SRRI is available in the
Lipper database.
}
\examples{
data.df <- createLipperInvestmentTypes(as.Date("2021-08-30"))

}
