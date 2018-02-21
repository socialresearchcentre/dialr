
library(dialr)
library(dplyr)
library(tidyr)
library(magrittr)

x <- tibble(id = 1:4,
            phone1 = c(0, 0123, "0404753828", "61410938817"),
            phone2 = c("0393881234", 1234, "+12015550123", 0),
            country = c("AU", "AU", "AU", "AU"))

x %>%
  gather(col, val, phone1, phone2) %>%
  mutate(val = phone(val, country)) %>%
  pull(val) %>%
  format(home = "US")

x %>%
  gather(col, val, phone1, phone2) %>%
  mutate(val = phone(val, country)) %>%
  pull(val) %>%
  get_region

x %>% mutate_at(vars(matches("^phone")), funs(phone(., country))) %>% gather(col, val, phone1, phone2)

x %<>% mutate_at(vars(matches("^phone")), funs(phone(., country)))

save(x, file = "tests/pointer_test.RData")
print(x$phone1)

rstudioapi::restartSession()
load("tests/pointer_test.RData")
print(x$phone1)

y <- phone(c(0, 0123, "0404753828", "61410938817"), "AU")

system.time(y <- phone(rep("0404753828", times = 10000), "AU"))
system.time(is_valid(y))
system.time(ph_valid(rep("0404753828", times = 10000), "AU"))
system.time(get_region(y))
system.time(get_type(y))
system.time(format(y))
