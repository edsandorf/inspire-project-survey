#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------
pkgs <- c("stringr", "shiny", "shinyjs", "shinyWidgets", "DT",
          "RMariaDB", "config", "dplyr", "sortable", "jsonlite", "pool")
invisible(lapply(pkgs, require, character.only = TRUE))

#-------------------------------------------------------------------------------
# Load the configuration options
#-------------------------------------------------------------------------------
db_config <- config::get("dataconnection")

#-------------------------------------------------------------------------------
# Read in the survey questions, choice tasks and sample weights
#-------------------------------------------------------------------------------
outline <- readr::read_csv(file.path("csv", "outline.csv"))
design <- readr::read_csv(file.path("csv", "design.csv"))
weights <- readr::read_csv(file.path("csv", "weights.csv"))
weights <- tibble::deframe(weights)

#-------------------------------------------------------------------------------
# Define global variables
#-------------------------------------------------------------------------------
pages <- nrow(outline)
tasks <- 10
nattr <- ncol(design)
time_inc <- 250

#-------------------------------------------------------------------------------
# If TRUE removes printing the answers and enables next page checks
#-------------------------------------------------------------------------------
production <- TRUE

#-------------------------------------------------------------------------------
# Define a function to save results to the database
#-------------------------------------------------------------------------------
save_db <- function (db_pool, x, db_name, db_config, replace_val) {
  # Construct the DB query to be sent to the database
  if (!replace_val) {
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      db_name,
      paste(names(x), collapse = ", "),
      paste(x, collapse = "', '")
    )
  } else {
    query <- sprintf(
      "UPDATE %s SET %s WHERE %s;",
      db_name,
      paste(paste0(names(x)[-1], " = \'", x[-1], "\'"), collapse = ", "),
      paste0(names(x)[1], " = \'", x[1], "\'")
    )
  }
  
  # Submit the insert query to the database via the opened connection
  RMariaDB::dbExecute(db_pool, query)
}

#-------------------------------------------------------------------------------
# Define a function for dropping columns that only contain NA
#-------------------------------------------------------------------------------
not_all_na <- function (x) {!all(is.na(x))}

#-------------------------------------------------------------------------------
# Define a function for labeling the mandatory fields in the consent form
#-------------------------------------------------------------------------------
label_mandatory <- function(label){
  tagList(
    label,
    span("*", class = "mandatory-star")
  )
}
