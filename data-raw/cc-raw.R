
library(dplyr)

cc_data <- read.csv("data-raw/cc.csv", stringsAsFactors = FALSE, na.strings = NULL)
cc_lookup <- structure(toupper(cc_data$code),
                       names = toupper(cc_data$country))

usethis::use_data(cc_lookup, internal = TRUE, overwrite = TRUE)
