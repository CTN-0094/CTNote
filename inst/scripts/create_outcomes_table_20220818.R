# Create All Outcomes Table
# Gabriel Odom
# 2022-08-18

# Following the 3 abstinence, relapse, and reduction "library" vignettes, we 
#   need to join these files and save them as a data set for this package.

library(tidyverse)
outcomesAbs_df <- read_csv("inst/extdata/outcomes_abstinence_20220818.csv")
outcomesRed_df <- read_csv("inst/extdata/outcomes_reduction_20220818.csv")
outcomesRel_df <- read_csv("inst/extdata/outcomes_relapse_20220818.csv")

outcomesCTN0094 <- 
  outcomesAbs_df %>% 
  left_join(
    outcomesRed_df %>% 
      select(-usePatternUDS),
    by = "who"
  ) %>% 
  left_join(
    outcomesRel_df %>% 
      select(-usePatternUDS),
    by = "who"
  )

# If (who am I kidding? *When*) we make newer versions of this data, make sure
#   to move the old version to inst/extdata/ and add the date.
usethis::use_data(outcomesCTN0094)
