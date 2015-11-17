#----------------------------
# Summary Functions
#----------------------------
df_to_sf_month <- function(df, variable, lev)
{
  stopifnot(variable %in% names(df))
  names(df)[which(names(df) == variable)] <- "variable"
  
  # month.year
  ind <- which(is.na(df$month.year))
  if(length(ind)) {
    df <- df[-ind, ]
  }
  
  # lev
  if(!missing(lev)) {
    df <- mutate(df, variable = as.factor(variable))
    df <- subset(df, variable %in% lev)  
    df <- mutate(df, variable = factor(as.character(variable), levels = lev))
  }
  else {
    lev <- levels(df$variable)
  }
  
  sf <- ddply(df, "month.year", function(x) table(x$variable, useNA = "ifany"))
  ind <- which(is.na(names(sf)))
  if(length(ind)) {
    names(sf)[ind] <- "NA"
  }
  
  mf <- melt(sf, id.vars = names(sf)[1])
  if(length(ind)) {
    mf <- mutate(mf, variable = as.character(variable))
    mf <- within(mf, {
      variable[variable == "NA"] <- NA
    })
    mf <- mutate(mf, variable = factor(variable, levels = lev))
  }
  
  mf
}

add_col_to_df <- function(df, colnames)
{
  for(colname in colnames) {
    if(!(colname %in% colnames(df))) {
      df <- cbind(df, NA)
      print(names(df))
      names(df)[ncol(df)] <- colname
    }
  }
  df
}

