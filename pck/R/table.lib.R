#----------------------------
# Fix Functions
#----------------------------

fix_names <- function(df) 
{
  rename(df, c('Level.of.experience..years.' = 'Level.of.experience.years'))
}

fix_columns <- function(df)
{
  # Post.date
  df <- mutate(df,
    Post.date = as.Date(Post.date)
  )

  # extract info from `Post.date`
  df <- mutate(df,
    year = as.numeric(format(df$Post.date, "%Y")),
    month = factor(month.abb[as.numeric(format(df$Post.date, "%m"))]),
    month.year = ifelse(is.na(year) | is.na(month), NA, paste(month, substr(year, 3, 4), sep = "-")))
  
  ind <- which(!duplicated(df$month.year))
  lev <- df[ind, "month.year"]
  df <- mutate(df,
    month.year = factor(month.year, levels = lev))
  
  # column `N`
  df <- mutate(df,
    N = as.numeric(gsub(",", "", N))) # get rid of patterns like "1,002"
  
  # empty entries -> NA
  df <- empty_to_na(df, c('Position', 'Company', 'Location', 'Country', 'Type.of.contract', 
    'Qualifications', 'Language.requirements', 'Specific.requirements', 'Educational.level', 
    'Level.of.experience.years', 'Description', 'URL', 'How.to.apply', 'Valid.until', 
    'Category', 'Sub.category', 'Salary', 'Source'))

  # `Company.type`
  df <- mutate(df,
    Company.type = ifelse(is.na(Company), NA,
      ifelse(Company %in% grep("University|College|Institute", Company, value = TRUE), "Academia", "Industry")))

  # 'Type.of.contract'
  Type.of.contract.levels <- c('FULLTIME', 'CONTRACT', 'INTERN', 'PARTTIME', 
    'TEMP', 'OTHER')
  
  Type.of.contract.unique <- unique(df$Type.of.contract)
  Type.of.contract.unique <- Type.of.contract.unique[!is.na(Type.of.contract.unique)] # remove NA
  stopifnot(all(Type.of.contract.unique %in% Type.of.contract.levels))
  
  df <- mutate(df, 
    Type.of.contract = factor(Type.of.contract, levels = Type.of.contract.levels))
    
  # 'Educational.level'
  Educational.level.levels <- c("Bachelor's Degree", 'Master Degree', 'Ph. D.', 
    'Diploma', 'BA, MS or PhD', 'High School')
  
  Educational.level.unique <- unique(df$Educational.level)
  Educational.level.unique <- Educational.level.unique[!is.na(Educational.level.unique)] # remove NA
  stopifnot(all(Educational.level.unique %in% Educational.level.levels))
  
  df <- mutate(df, 
    Educational.level = factor(Educational.level, levels = Educational.level.levels))

  # `Country`
  ind <- df$Country == "Netherlands"
  if(length(ind)) { 
    df$Country[ind] <- "The Netherlands"
  }

  ind <- df$Country == "Korea"
  if(length(ind)) { 
    df$Country[ind] <- "South Korea" 
  }
      
  return(df)
}

fix_Category <- function(df)
{
  # remove `http*` entry
  ind <- grep("http", df$Category)
  if(length(ind) == 1) {
    stopifnot(df$N[ind] == 1348)
    df$Category[ind] <- NA
  }

  # remove blank spaces
  df <- mutate(df,
    Category = gsub(" ", "", df$Category))

  # "PM" -> "Localization"
  df <- within(df, {
    Category[Category == "PM"] <- "Localization"
  })
  
  Category.levels <- c("NLP", "DM", "Localization", "ML")
  
  Category.unique <- unique(df$Category)
  Category.unique <- Category.unique[!is.na(Category.unique)] # get rid off NA
  stopifnot(all(Category.unique %in% Category.levels))
  
  # convert to factor
  df <- mutate(df, 
    Category = factor(Category, levels = Category.levels))
  
  return(df)
}

fix_Sub.category <- function(df)
{
  # fix entries `Internship/`Internships`
  df <- mutate(df,
    Sub.category = ifelse(Sub.category %in% c("Internship", "Internships"), "Internships", Sub.category))

  # get rid of entries with >2 sub-categories like `Consultants/experts, Software Engineers/Developers`
  out <- strsplit(df$Sub.category, ',')
  num.subcat.cut <- sum(laply(out, function(x) length(x) > 1))
  cat(" * No of Sub.categories with >2 entries:", num.subcat.cut, "\n")

  df <- mutate(df, 
    Sub.category = laply(out, function(x) x[[1]]))

  # convert to factor 
  Sub.category.levels <- c('Researchers/Scientists', 'Software Engineers/Developers', 
    'Consultants/Experts', 'Managers/Executives', 
    'Others', 'Computational Linguists', 
    'Internships', 'Software Testers', 
    'Product Managers', 'Consultants/experts')
  
  Sub.category.unique <- unique(df$Sub.category)
  Sub.category.unique <- Sub.category.unique[!is.na(Sub.category.unique)] # remove NA
  stopifnot(all(Sub.category.unique %in% Sub.category.levels))
  
  # convert to factor
  df <- mutate(df, 
    Sub.category = factor(Sub.category, levels = Sub.category.levels))
  
  return(df)
}

#----------
# Add geo functions
#-----

# - questions: 'Research Associate'
add_col_Position.Type <- function(df)
{
  df$Position.type <- NA

  ind <- grep("cientist|esearcher|Research Group Leader|Full Time research|RESEARCH FELLOW|\
Research Position|Research fellow|Research Associate", df$Position)
  df$Position.type[ind] <- "Scientist"

  ind <- grep("rofessor", df$Position)
  df$Position.type[ind] <- "Professor"

  # 'PhD' goes after 'Scientist`
  ind <- grep("PhD|Ph.D", df$Position)
  df$Position.type[ind] <- "PhD"

  # 'PostDoc' goes after 'Scientist`
  ind <- grep("Post-doc|POSTDOCTORAL|Post Doc|Postdoc", df$Position)
  df$Position.type[ind] <- "PostDoc"

  # 'Staff`
  ind <- is.na(df$Position.type)
  df$Position.type[ind] <- "Staff"

  return(df)
}

#----------------------------
# Support functions
#----------------------------

empty_to_na <- function(df, colnames) 
{
  ind <- which(names(df) %in% colnames)
  stopifnot(length(ind) == length(colnames))
  
  for(i in ind) {
    x <- df[, i]
    if(class(x) != "character") {
      stop("Error in empty_to_na: column ", names(df)[i], ", class", class(x), "\n")
    }
    df[, i] <- ifelse(x == "", NA, x)
  } 
   
  df
}

