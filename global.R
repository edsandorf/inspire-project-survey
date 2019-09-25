#-------------------------------------------------------------------------------
# Load packages
#-------------------------------------------------------------------------------
pkgs <- c("stringr", "shiny", "shinyjs", "shinyWidgets", "DT",
          "RMariaDB", "config", "dplyr")
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
# Define a function to save results to the database
#-------------------------------------------------------------------------------
save_db <- function (x, db_name, db_config) {
  # Connect to the database
  db <- RMariaDB::dbConnect(RMariaDB::MariaDB(),
                            dbname = db_config$dbname,
                            host = db_config$host,
                            username = db_config$username,
                            password = db_config$password,
                            ssl.key = db_config$ssl.key,
                            ssl.cert = db_config$ssl.cert,
                            ssl.ca = db_config$ssl.ca)
  
  # Construct the DB query to be sent to the database
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    db_name,
    paste(names(x), collapse = ", "),
    paste(x, collapse = "', '")
  )
  
  # Submit the insert query to the database via the opened connection
  RMariaDB::dbExecute(db, query)
  
  # Close the database connection
  on.exit(RMariaDB::dbDisconnect(db))
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
