
library(dialr)
library(dplyr)
library(tidyr)
library(magrittr)

x <- tibble(id = 1:4,
            phone1 = c(0, 0123, "0404753123", "61410123817"),
            phone2 = c("0393881234", 1234, "+12015550123", 0),
            country = c("AU", "AU", "AU", "AU"))

z <- tibble(id = 1:1000,
            phone1 = rep(c("0404753123", 0), times = 500),
            country = rep("AU", times = 1000),
            log = rep(c(TRUE, FALSE), times = 500))

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

x %>% mutate_at(vars(matches("^phone")), ~phone(., country)) %>% gather(col, val, phone1, phone2)

x %<>% mutate_at(vars(matches("^phone")), ~phone(., country))
z %<>% mutate_at(vars(matches("^phone")), ~phone(., country))

save(x, file = "scratch/pointer_test.RData")
print(x$phone1)

rstudioapi::restartSession()
load("scratch/pointer_test.RData")
print(x$phone1)

y <- phone(c(0, 0123, "0404 753 123", "61410123817"), "AU")

system.time(y <- phone(rep("0412345678", times = 10000), "AU"))
system.time(is_valid(y))
system.time(ph_valid(rep("0412345678", times = 10000), "AU"))
system.time(get_region(y))
system.time(get_type(y))
system.time(format(y))

#-------------------------------------------------------------------------------

library(stringr)

phone_util <- dialr:::.get_phoneNumberUtil()

phone_util_methods <-
  .jmethods(phone_util) %>%
  tibble(raw = .) %>%
  mutate(tmp = raw,
         type = str_trim(str_extract(tmp, "^((public|final|native|static|synchronized) )+")),
         tmp = str_remove(tmp, "^((public|final|native|static|synchronized) )+"),
         return = str_trim(str_extract(tmp, "^[^ ]* ")),
         tmp = str_remove(tmp, "^[^ ]* "),
         exception = str_trim(str_extract(tmp, " throws [^ ]*$")),
         tmp = str_remove(tmp, " throws [^ ]*$"),
         method = str_extract(tmp, "[^.]*\\(.*$"),
         name = str_remove(method, "\\(.*$"),
         args = str_remove(method, "[^(]*"),
         method = tmp) %>%
  select(-tmp)

#-------------------------------------------------------------------------------

# Generate example numbers for a all valid phone types by region

region_type <-
  full_join(tibble(country = dialr:::.getSupportedRegions(), f = 1),
            tibble(type = dialr:::.get_phoneNumberType(), f = 1)) %>%
  select(-f)

region_type %<>%
  rowwise() %>%
  mutate(valid = type %in% dialr:::.getSupportedTypesForRegion(country)) %>%
  ungroup()

region_type %<>% bind_rows(., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .)
  
region_type %<>%
  filter(valid) %>%
  mutate(phone = get_example(country, type))

region_type %<>% mutate(phone = phone(as.character(phone), country))
region_type %<>% mutate(validnum = is_valid(phone), regionnum = get_region(phone))
region_type %<>% mutate(final = format(phone, clean = FALSE))
