#----------------------------
# Lat/Lon Functions
#----------------------------

fetch_latlon <- function(location, verbose = 0)
{
  if(verbose) cat(" * location:", location, "\n")
  
  df <- data.frame(lon = NA, lat = NA)
  if(is.na(location)) {
    return(df)
  }
  
  ret <- try({geocode(location)})
  if(class(ret) == "data.frame") {
    stopifnot(nrow(df) == 1)
    df <- ret
  }
  
  return(df)
}

#----------------------------
# Lat/Lon Functions
#----------------------------

fetch_geoinfo <- function(lon, lat, verbose = 0)
{
  if(verbose) cat(" * lon/lat:", lon, "/", lat, "\n")
  
  out <- list(lon = lon, lat = lat)
  if(any(is.na(c(lon, lat)))) {
    return(out)
  }
  
  ret <- try({revgeocode(as.numeric(c(lon, lat)), output = "more")})
  if(class(ret) == "data.frame") {
    stopifnot(nrow(ret) == 1)
    out <- as.list(ret)
  }
  
  return(out)
}
