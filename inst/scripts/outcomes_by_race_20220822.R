# Outcomes by Race/Ethnicity
# Gabriel Odom
# 2022-03-10
# UPDATED: 2022-08-22

library(ctn0094data)
library(ctn0094DataExtra)
library(CTNote)
library(tidyverse)

# We use ctn0094data v. 0.0.0.9023 and ctn0094DataExtra v. 0.0.0.9009



######  Outcomes (Treatment) Data  ############################################


###  Load and Scale the Data  ###
data("outcomesCTN0094")

is_count <- function(x, ignore_na = TRUE) {
  if (!is.numeric(x)) {
    return(FALSE)
  } else {
    max(x, na.rm = ignore_na) > 1 
  }
}

outcomesScaled_df <- 
  outcomesCTN0094 %>% 
  mutate(who = as.character(who)) %>% 
  # Scale count features
  mutate(
    across(.cols = where(is_count), ~ { .x / max(.x) } )
  ) %>% 
  # Sort outcomes from same authors together
  select(sort(colnames(.))) %>% 
  select(who, usePatternUDS, everything())


###  Splitting the Data by Outcome  ###
uniqueNames_char <- 
  outcomesScaled_df %>% 
  select(-who, -usePatternUDS) %>% 
  colnames() %>% 
  str_remove(pattern = "_time") %>% 
  str_remove(pattern = "_event") %>% 
  unique()

outcomesScaledDF_ls <- map(
  .x = uniqueNames_char,
  .f = ~{
    outcomesScaled_df %>% 
      select(who, starts_with(.x))
  }
)
names(outcomesScaledDF_ls) <- uniqueNames_char

rm(outcomesCTN0094, is_count, outcomesScaled_df, uniqueNames_char)



######  Wrangle Response and Covariates  ######################################
# We said that we would include age, sex, study, treatment, and pertinent
#   medical and psychiatric history (including dx of other substance misuse) as
#   covariates. For treatment, we can simply have bup, methadone, and naltrexone


###  Demographics  ###
# ctn0094DataExtra::derived_raceEthnicity
# table(derived_raceEthnicity[, c("race", "is_hispanic")])
# #                is_hispanic
# # race              No  Yes
# # Black            347   17
# # Other            186  320
# # Refused/missing    2   23
# # White           2484  147
# table(derived_raceEthnicity$race_ethnicity)
# # Hisp   NHB   NHW  Other 
# #  507   347  2484    222 

V1_df <- 
  ctn0094data::demographics %>%
  select(who, age, is_male) %>%
  # # Site
  # left_join(ctn0094data::site_masked, by = "who") %>% 
  # Project
  left_join(ctn0094data::everybody, by = "who") %>% 
  left_join(
    ctn0094DataExtra::derived_raceEthnicity %>% 
      select(who, race_ethnicity),
    by = "who"
  )


###  Medical/Psychiatric  ###
V2_df <- 
  ctn0094data::psychiatric %>% 
  select(who, starts_with("has")) %>% 
  select(-has_opiates_dx, -has_epilepsy, -has_brain_damage) %>% 
  mutate(across(where(is.factor), ~{ .x == "Yes" })) %>% 
  mutate(
    has_other_subs_dx = rowSums( across(has_alcol_dx:has_sedatives_dx) ) > 0
  ) %>% 
  select(-(has_alcol_dx:has_sedatives_dx)) %>% 
  mutate(
    has_any_illness = rowSums( across(-c(who, has_other_subs_dx)) ) > 0
  ) %>% 
  select(who, has_any_illness, has_other_subs_dx)

# table(V2_df$has_other_subs_dx, useNA = "ifany")
# # FALSE  TRUE  <NA> 
# # 	852  1304  1404 
# table(V2_df$has_any_illness, useNA = "ifany")
# # FALSE  TRUE  <NA> 
# #  1575  1511   474 


###  Treatment  ###
# table(ctn0094data::randomization$treatment)
V3_df <- 
  ctn0094data::randomization %>% 
  mutate(
    treatClass = case_when(
      str_detect(treatment, "BUP") ~ "buprenorphine",
      treatment == "Inpatient NR-NTX" ~ "naltrexone",
      treatment == "Methadone" ~ "methadone",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(who, treatClass) %>% 
  distinct() %>% 
  mutate(treatClass = as.factor(treatClass))

# table(V3_df$treatClass)
# # buprenorphine     methadone    naltrexone 
# #          1680           529           283 


###  Make the Base Design Matrix  ###
design_df <-
  V3_df %>%
  left_join(
    V2_df %>% 
      select(-has_other_subs_dx),
    by = "who"
  ) %>%
  left_join(V1_df, by = "who") %>% 
  mutate(
    race_ethnicity = relevel(race_ethnicity, ref = "Non-Hispanic White")
  ) %>% 
  mutate(who = as.character(who)) %>% 
  select(who, race_ethnicity, everything())


###  Check NAs  ###
# In the previous version of this script, we had included data from ASI, but we
#   discovered that this was missing for almost all of CTN-0027.
map_dbl(
  design_df,
  .f = ~{ mean(is.na(.x)) }
)

rm(V1_df, V2_df, V3_df)



######  Models  ###############################################################
# See: https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
library(nnet)


###  Baseline  ###
baseline_mod <- multinom(race_ethnicity ~ ., data = design_df[, -1])

# baselineSummary_ls <- summary(baseline_mod)
# z_mat <- baselineSummary_ls$coefficients / baselineSummary_ls$standard.errors
# (1 - pnorm(abs(z_mat), 0, 1) ) * 2
# deviance(baseline_mod)
# AIC(baseline_mod)


###  GLMM Model Function  ###
FitGLMM <- function(model_df, v_df,
                    resp = "race_ethnicity",
                    by_char = "who",
                    ...) {
  
  x_df <- left_join(model_df, v_df, by = by_char)
  x_df[[by_char]] <- NULL
  
  forml <- as.formula(paste0(resp, " ~ ."))
  multinom(formula = forml, data = x_df, ...)
  
}

# Test
new_mod <- FitGLMM(
  model_df = design_df,
  v_df = outcomesScaledDF_ls[[1]]
)

lmtest::lrtest(new_mod, baseline_mod)
lmtest::lrtest(new_mod, baseline_mod)[["Pr(>Chisq)"]][2]
# 0.0372488 with v 9023 of the data with site included, 0.0001213802 with site
#   excluded (this is site masked, not real site)
# summary(new_mod)$coefficients[, ncol(summary(new_mod)$coefficients)]


###  All Outcomes  ###
mods_ls <- map(
  .x = outcomesScaledDF_ls,
  .f = ~{
    FitGLMM(model_df = design_df, v_df = .x)	
  }
)



######  Rank Models by Racial/Ethnic Information in Outcome Metrics  ##########

###  LR Test Function  ###
safeLRtest <- function(full_mod, red_mod) {
  # browser()
  
  ###  Model Summaries  ###
  q <- red_mod$n[1]
  p <- full_mod$n[1]
  newFeats_int <- (q + 1):p
  modSumm_ls <- summary(full_mod)
  
  out_df <- 
    modSumm_ls[["coefficients"]] %>% 
    as.data.frame() %>% 
    select(all_of(newFeats_int)) %>% 
    rownames_to_column(var = "race_ethnicity") %>% 
    pivot_longer(-race_ethnicity, values_to = "beta")
  
  out_df <- 
    modSumm_ls[["standard.errors"]] %>% 
    as.data.frame() %>% 
    select(all_of(newFeats_int)) %>% 
    rownames_to_column(var = "race_ethnicity") %>% 
    pivot_longer(-race_ethnicity, values_to = "stdE") %>% 
    left_join(out_df, ., by = c("race_ethnicity", "name")) %>% 
    mutate(
      zVal = beta / stdE,
      pVal = (1 - pnorm(abs(zVal), mean = 0, sd = 1) ) * 2
    )
  
  
  ###  LR Test  ###
  lrTest_ls <- safely(lmtest::lrtest)(full_mod, red_mod)
  if (!is.null(lrTest_ls$error)) {
    out_df$lrTest_p <- NA_real_
  } else {
    out_df$lrTest_p <- lrTest_ls$result[["Pr(>Chisq)"]][2]
  }
  
  out_df
  
}

# Test
safeLRtest(full_mod = mods_ls[[1]], red_mod = baseline_mod)
safeLRtest(full_mod = mods_ls[[2]], red_mod = baseline_mod)


###  Compile Results  ###
res_df <- map_dfr(
  .x = mods_ls,
  .f = ~{
    safeLRtest(full_mod = .x, red_mod = baseline_mod)
  },
  .id = "Outcome"
)


###  Adjust p-Values  ###
res_df <- 
  res_df %>% 
  select(Outcome, lrTest_p) %>% 
  distinct() %>% 
  mutate(FDR = p.adjust(lrTest_p, method = "fdr")) %>% 
  select(-lrTest_p) %>% 
  left_join(res_df, ., by = "Outcome")

res_df <- 
  res_df %>% 
  select(Outcome, name, race_ethnicity, everything())


###  Inspect Results  ###
hist(res_df$lrTest_p)

res_df %>%  
  select(Outcome, FDR) %>% 
  filter(FDR < 0.05) %>%
  distinct() %>% 
  arrange(FDR, Outcome) %>% 
  View()

saveRDS(res_df, "vignettes/outcome_metric_biases.rda")


###  Explore Results  ###
View(res_df)
# That's A LOT of negative betas. Remember that all these outcomes are "larger
#   is better", and we have the baseline as NHW. So, negative betas are saying:
#   "if you succeeded in this treatment, you are less likely to be a minority".
# Ouch.
res_df %>% 
  filter(FDR < 0.01) %>% 
  mutate(worseBeta = sign(beta) == -1) %>% 
  group_by(race_ethnicity) %>% 
  summarise(propWorse = mean(worseBeta))
# For the significantly different endpoints, it's almost always worse to not be
#   white.

