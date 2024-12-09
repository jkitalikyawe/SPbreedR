\name{genfam}
\alias{genfam}
\title{Generate seed families}
\description{Generate seed families for a number of germinated seed crosses}
\usage{genfam(country,year = format(Sys.Date(),"\%Y"),from = 1, to = 1, inst = c("N","P"))}
\arguments{
 \item{country}{A character string of the country name or two letter ISO-Code in which seeds are germinated}
 \item{year}{The year in which seedlings have been germinated}
 \item{from}{An integer from which to start generating seed families}
 \item{to}{An integer on which to stop generating seed families}
 \item{inst}{One of \emph{N} or \emph{P} for National Institute or International research center respectively representing the institution responsible for germinating the seed families}
}
\examples{
genfam(country = "UG",from = 2,to = 14,inst = "P")
genfam(country = "Mozambique",from = 10,to = 50,inst = "N")
genfam(country = "American Samoa",from = 10,to = 50,inst = "P")
genfam(country = "RW",from = 1,to = 4,inst = "N")
genfam(country = "UGA",from = 4,to = 30,inst = "P")
genfam(country = "RWA",from = 1,to = 4,inst = "N")
genfam(country = "AND",from = 4,to = 30,inst = "P")
}
