
library(dplyr)

cc_data <- read.csv("data-raw/cc.csv", stringsAsFactors = FALSE)
cc_lookup <- structure(toupper(cc_data$code),
                       names = toupper(cc_data$country))

devtools::use_data(cc_lookup, internal = TRUE, overwrite = TRUE)
