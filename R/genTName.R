genTName <- function(country,year = format(Sys.Date(),"%Y"),location = character(),
                     type = c("SN", "OT","PT","AT","ST","FT","CT"), inst = c("N","P"),
                     trNumber = integer()){
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
      paste0(toupper(substr(location,start = 1,stop = 3)),"-"),match.arg(type),sprintf("%02.0f",trNumber)),
    iso2code = paste0(
      country,match.arg(inst), year,
      paste0(toupper(substr(location,start = 1,stop = 3)),"-"),match.arg(type),sprintf("%02.0f",trNumber)),
    iso3code = paste0(
      ISO_CountCodes$iso2Code[ISO_CountCodes$iso3Code == country],match.arg(inst), year,
      paste0(toupper(substr(location,start = 1,stop = 3)),"-"),match.arg(type),sprintf("%02.0f",trNumber)),
    error = stop(paste0(country," is not a recognized country"))
  )
}

