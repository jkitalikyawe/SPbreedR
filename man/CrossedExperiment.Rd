\name{CrossedExperiment}
\alias{genCrssExp}
\alias{genCrssID}
\title{
Generate standard crossing experiment name
}
\description{
The function genCrssExp generates a standardized sweet potato crossing experiment name whereas genCrssID generates a predefined set of cross unique IDs for the crosses in the given experiment.
}
\usage{
genCrssExp(country, year = format(Sys.Date(), "\%Y"), location = character(),
type = c("OP", "PC", "BC"), inst = c("N", "P"), expNumber = 1, parNumber = integer())
genCrssID(country,year = format(Sys.Date(),"\%Y"),location = character(),
type = c("OP","PC","BC"),inst = c("N","P"), expNumber = 1, from = 1, to = 10)
}
\arguments{
  \item{country}{A character string of the country name or two letter ISO-Code in which the experiment is conducted}
  \item{year}{The year in which the experiment is conducted}
  \item{location}{Research station or place in which experiment is conducted}
  \item{type}{Type of crossing experiment conducted}
  \item{inst}{One of \emph{N} or \emph{P} for National Institute or International research center respectively representing the institution responsible for crossing experiment being conducted}
  \item{expNumber}{Experiment number of similar type in the same year at a particular location}
  \item{parNumber}{Number of parents involved in the crossing experiment}
  \item{from}{An integer from which to start generating cross unique identifiers}
 \item{to}{An integer on which to stop generating cross unique identifiers}
}
\details{
\emph{type} can be one of \emph{OP} for Open pollinated crosses from non-isolated field trials or multiplication blocks, \emph{PC} for Polycross (open pollinated seed from parents in an isolated crossing block), \emph{BC} for Biparental controlled crosses.
}
\value{
For genCrssExp, a character vector of length one of the name of the crossing experiment as stipulated in the reference. The function genCrssID generates a character vector of length \emph{from:to} giving crossing uniques identifiers to be used in the stated crossing experiment.
}
\references{
Wanjohi, L., Eyzaguirre, R., & Boeck, B. D. (2020). Standard operating procedures for sweetpotato breeding data management. COP Breeding Data Management SweetGAINS.
}

\examples{
genCrssExp(country = "Uganda",location = "Namulonge",
type = "PC",inst = "P",expNumber = 2,parNumber = 40)
genCrssExp(country = "Mozambique",location = "Maputo",
type = "BC",inst = "P",expNumber = 3,parNumber = 40)
genCrssExp(country = "Uganda",location = "Namulonge",
type = "OP",inst = "P",expNumber = 2,parNumber = 20)
genCrssID(country = "Uganda",location = "Namulonge",
type = "PC",inst = "P",expNumber = 2,from = 3,to = 15)
genCrssID(country = "Mozambique",location = "Maputo",
type = "BC",inst = "P",expNumber = 3)
genCrssID(country = "Uganda",location = "Namulonge",
type = "OP",inst = "P",expNumber = 2)
}
