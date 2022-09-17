# Excerpt of all_drugs for 10 example participants
# Gabriel Odom
# 2022-09-16

library(ctn0094data)
library(ctn0094DataExtra)
library(tidyverse)

interestingPeople_int <- c(
  163L, 210L, 242L, 4L, 17L, 13L, 1103L, 33L, 233L, 2089L
)

egOpioidsCTN0094 <- 
  all_drugs %>% 
  filter(who %in% interestingPeople_int) %>% 
  filter(source %in% c("UDS", "UDSAB")) %>% 
  filter(what == "Opioid") %>% 
  select(-source) %>% 
  distinct() %>% 
  arrange(who, when)

usethis::use_data(egOpioidsCTN0094)
