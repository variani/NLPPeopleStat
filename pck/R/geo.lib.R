#----------------------------
# Geo functions
#----------------------------
update_columns_geo <- function(df, verbose = 1)
{
  geo.colnames <- c("lon", "lat")
  
  geo <- data.frame(N = df$N)
  if(all(geo.colnames %in% colnames(df))) {
    geo <- cbind(geo, subset(df, select = geo.colnames))
  }
  else {
    geo <- add_col_to_df(geo, c(geo.colnames))
  }
  
  rf <- ldply(1:nrow(geo), function(i, verbose = verbose) {
    # case 1/2: data exists  
    gfi <- geo[i, c("lon", "lat"), drop = FALSE]
    if(all(!is.na(as.numeric(gfi)))) {
      return(gfi)
    }
    
    # case 2/2: data to be updated
    cat(" * update", i, "/", nrow(geo), "\n")
    loc <- df$Location[i]
    country <- df$Country[i]
  
    q <- loc
    if(!is.na(q) & !is.na(country)) {
      q <- paste(q, country, sep = ",") # add `country` if both are not NA
    }  
    fetch_latlon(q, verbose = verbose)
  }, verbose = verbose)
  
  df$lon <- rf$lon
  df$lat <- rf$lat 
  
  return(df)
}


update_columns_geoinfo <- function(df, verbose = 1)
{

  geo.colnames <- c("lon", "lat")
  stopifnot(all(geo.colnames %in% colnames(df)))
  
  geo <- subset(df, select = c("N", geo.colnames))
  
  geoinfo <- llply(1:nrow(geo), function(i, verbose = verbose) {
    if(verbose) { cat(" * fetch geoinfo", i, "/", nrow(geo), "\n") }
    
    out <- fetch_geoinfo(geo[i, "lon"], geo[i, "lat"], verbose = verbose)

    c(out, list(N = geo$N[i]))
  }, verbose = verbose)
  names(geoinfo) <- as.character(geo$N)

  # filrer from `contry`/`Country`
  gf <- ldply(1:nrow(geo), function(i) {
    x <- geoinfo[[i]]
    
    data.frame(N = geo[i, "N"], 
      lon = geo[i, "lon"], lat = geo[i, "lat"],
      country = ifelse(is.null(x$country), NA, x$country))
  })
  
  gf <- join(subset(df, select = c("N", "Location", "Country")), gf, by = "N")
  gf <- mutate(gf,
    geoinfo.ok = (Country == country))
  
  ind <- which(gf$geoinfo.ok == FALSE)
  cat(" * No of postings with NA geoinfo:", length(ind), "\n")

  for(i in ind) {
    gf[i, geo.colnames] <- NA
  }
  
  # return
  df <- join(df, subset(gf, select = c("N", "geoinfo.ok")), by = "N")
  
  return(df)
}


#----------
# Add geo functions
#-----
fix_Country <- function(df)
{
  ind <- df$Country == "Netherlands"
  if(length(ind)) { df$Country[ind] <- "The Netherlands" }

  ind <- df$Country == "Korea"
  if(length(ind)) { df$Country[ind] <- "South Korea" }
  
  return(df) 
}

#----------
# Add geo functions
#-----
add_col_Region2 <- function(df)
{
  stopifnot(all(c("lon", "lat") %in% colnames(df)))
  
  ### `Region2`
  ind <- with(df,
    lon > -15 & lon < 45 & lat > 34 & lat < 70)
  
  ind <- which(ind)  
  Country.europe <- unique(df[ind, "Country"])

  df$Region2 <- "Rest"

  ind <- which(df$Country %in% Country.europe)
  df$Region2[ind] <- "Europe"

  ind <- which(df$Country == "United States")
  df$Region2[ind] <- "US"
  
  return(df)
}








