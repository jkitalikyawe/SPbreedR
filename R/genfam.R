genfam <- function(country,year = format(Sys.Date(),"%Y"),from = 1, to = 1, inst = c("N","P")){
  genFam <- vector("character",length(from:to))
  countCode <- if(is.element(country,ISO_CountCodes$countName)){
    "countName"
  } else if(is.element(country,ISO_CountCodes$iso2Code)){
    "iso2code"
  } else if(is.element(country,ISO_CountCodes$iso3Code)){
    "iso3code"
  } else {"error"}
  switch(
    countCode,
    countName = paste0(
      ISO_CountCodes$iso2Code[ISO_CountCodes$countName == country],match.arg(inst), year,
      stringr::str_pad(from:to,width = 5,side = "left",pad = 0)),
    iso2code = paste0(
      country,match.arg(inst), year,
      stringr::str_pad(from:to,width = 5,side = "left",pad = 0)),
    iso3code = paste0(
      ISO_CountCodes$iso2Code[ISO_CountCodes$iso3Code == country],match.arg(inst), year,
      stringr::str_pad(from:to,width = 5,side = "left",pad = 0)),
    error = stop(paste0(country," is not a recognized country"))
  )
}

