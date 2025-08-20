library(surveydown)
library(dplyr)

db <- sd_db_connect()

data <- sd_get_data(db = db)

glimpse(data)
