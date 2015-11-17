#----------------------------
# Plot Functions
#----------------------------
plotOverview <- function(df)
{
  pf <- ddply(df, "month.year", summarize,
    num.postings = length(N))

  p <- ggplot(pf, aes(as.numeric(month.year), num.postings)) +
    geom_line() + geom_point() 
    #geom_area(position = "stack") + 
    #common_x + theme_nlp

  # x axis
  p <- p + scale_x_continuous(breaks = 1:nlevels(df$month.year), labels = levels(df$month.year))

  p <- p + labs(x = "Month", y = "Number of postings per month")

  # theme
  p <- p + theme_bw()
  
  return(p)
}

plotCategory <- function(df, type = "line", bpal = "Set1")
{
  lev <- levels(df$Category)
  lev <- lev[!is.na(lev)] # remove NA
  
  sf <- df_to_sf_month(df, "Category", lev)
  
  p <- switch(type,
    "line" = {
      sf_month_to_p(sf, "line", brewer_palette = bpal) +
        labs(x = "Month", y = "Number of job postings", color = "Posting Category")
    },
    "area" = { 
      sf_month_to_p(sf, "area", brewer_palette = bpal) + 
        guides(color = FALSE) +
        labs(x = "Month", y = "Number of job postings", fill = "Posting Category")
    },
    stop("Error in `plotCategory`: switch."))
  
  # theme + legend.position
  p <- p + theme_bw() + theme(legend.position = "top")
  
  return(p)
}

plotSubcategory <- function(df, type = "line", bpal = "Paired")
{
  lev <- levels(df$Sub.category)
  lev <- lev[!is.na(lev)] # remove NA
  
  sf <- df_to_sf_month(df, "Sub.category", lev)
  
  p <- switch(type,
    "line" = {
      sf_month_to_p(sf, "line", brewer_palette = bpal) +
        guides(col = guide_legend(nrow = 2)) +
        labs(x = "Month", y = "Number of job postings", color = "Posting Category")
    },
    "area" = { 
      sf_month_to_p(sf, "area", brewer_palette = bpal) + 
        guides(color = FALSE) + guides(fill = guide_legend(nrow = 2)) +
        labs(x = "Month", y = "Number of job postings", fill = "Posting Category")
    },
    stop("Error in `plotCategory`: switch."))
  
  # theme + legend.position
  p <- p + theme_bw() + theme(legend.position = "top")
  
  return(p)
}

#-----------------------
# Support functions
#-----------------------
sf_month_to_p <- function(sf, type = "line", brewer_palette)
{
  stopifnot(all(c("variable", "value") %in% names(sf)))
  
  p <- switch(type,
    "line" = {
      ggplot(sf, aes(as.numeric(month.year), value, color = variable, group = variable)) +
        geom_line() + geom_point()
     },
    "area" = {
      ggplot(sf, aes(as.numeric(month.year), value)) +
        geom_area(aes(color = variable, fill = variable), position = "stack")
      },
    stop("Error in switch."))
  
  # x axis
  p <- p + scale_x_continuous(breaks = 1:nlevels(sf$month.year), labels = levels(sf$month.year))

  
  if(!missing(brewer_palette)) {
    col <- RColorBrewer::brewer.pal(nlevels(sf$variable), brewer_palette)
    p <- p + scale_color_manual(values = col, na.value = "grey50") + 
      scale_fill_manual(values = col, na.value = "grey50")
  }
  
  return(p)
}
