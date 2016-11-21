require(magrittr)

streetsData <- function(){
  streets <- read.csv(
    file = "C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\street.csv",
    header = TRUE,
    sep = ",",
    stringsAsFactors = FALSE
  )
  streets
}

# Define the fields we want to save from the form
fields <- c("name", "r_num_years")
fieldsRoute <- c("location", "destination")
fieldsFactors <-
  c(
    "route_id",
    "time",
    "day",
    "cars",
    "lanes",
    "damage",
    "zones",
    "events",
    "weather",
    "distance"
  )
workspace <- 'C:\\Users\\Adam\\Documents\\CIT-U\\Masters of Computer Science\\Capstone - Thesis\\Navify - Traffixer\\Navify\\'
setwd(workspace)

options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "root",
  "password" = ""
))

# If you modify global options() or graphics par(), save the old values and reset when you’re done
old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)