\name{genTName}
\alias{genTName}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a standardized trial name
}
\description{
Generate a standardized trial name for sweet potato breeding experiment.
}
\usage{
genTName(country, year = format(Sys.Date(), "\%Y"),location = character(),
type = c("SN", "OT", "PT", "AT", "ST", "FT", "CT"), inst = c("N", "P"),
trNumber = integer())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{country}{A character string of the country name or two letter ISO-Code in which the trial is conducted}
  \item{year}{The year in which the trial is conducted}
  \item{location}{Research station or place in which trial is conducted}
  \item{type}{Type of trial to be conducted}
  \item{inst}{One of \emph{N} or \emph{P} for National Institute or International research center respectively representing the institution responsible for trial being conducted}
  \item{trNumber}{Trial number or sequence of similar type in the same year at a particular location}
}
\details{Trial type can be one of \emph{SN} for seed nursery, \emph{OT} for observational yield trial, \emph{PT} for preliminary yield trial, \emph{AT} for advanced yield trial, \emph{FT} for on-farm trial, \emph{CT} for crossing trial, \emph{ST} for special trial (e.g. Storage, genetic gain, drought, bio assay)}
\value{A character vector of length one representing the trial name as stipulated in SOPs for sweet potato breeding data management.}
\references{
Wanjohi, L., Eyzaguirre, R., & Boeck, B. D. (2020). Standard operating procedures for sweetpotato breeding data management. COP Breeding Data Management SweetGAINS.
}

\seealso{
\code{\link{genfam}}
}
\examples{
genTName(country = "Uganda",location = "Namulonge",type = "PT",inst = "P",trNumber = 2)
genTName(country = "Kenya",location = "Serere",type = "OT",inst = "P",trNumber = 1,year = 2023)
genTName(country = "Uganda",location = "Kapiti",type = "PT",inst = "P",trNumber = 2)
genTName(country = "Uganda",location = "Bulindi",type = "AT",inst = "P",trNumber = 3)
}
