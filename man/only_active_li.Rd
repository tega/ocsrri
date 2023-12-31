% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/only_active_li.R
\name{only_active_li}
\alias{only_active_li}
\title{Title Remove inactive investments from a li(x) list of investments (indices)}
\usage{
only_active_li(li)
}
\arguments{
\item{li}{the list of Pxx files. P01 (or P04) is mandatory in the list.}
}
\value{
a named list similar to li with the inactive investments
removed from every Pxx tibble of the list.
}
\description{
Given a list of investments (indices) where each component of the list is
a Pxx tibble (for a R01 request a P01 and additional
Pxx files as output or for a R04 request a P04 and additinal Pxx files) 
the function returns for each Pxx tibble the
subset of attributes of the active investments (indices) only.
}
\examples{
\dontrun{
lia <- only_active_li(li)
}
}
\author{
Claudio Ortelli
}
