---
title: CTNote
---

<!-- badges: start -->
[![R-CMD-check](https://github.com/CTN-0094/CTNote/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CTN-0094/CTNote/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Introduction to CTNote: The CTN Outcomes, Treatments, and Endpoints Library

The package `CTNote` exists as a comprehensive implementation of *outcomes*, *treatments*, and *endpoints* used in clinical trials to evaluate the efficacy of medication-assisted treatment of substance use disorders (SUDs). The functions in this package are programmatic building blocks with which to construct algorithms which calculate single-value summaries of clinical trial participants' substance use patterns. We assume that these substance use patterns are symbolized as a "word" (see below). The vignettes for this package include a standard and code-based library of algorithms for treatment outcome definitions useful to evaluate medication-based treatments for SUDs.

## Installation

You can install the released version of CTNote from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("CTNote")
```

-------------------------------------------------------------------------------

</br>


## Use Pattern "Words"

If you wish to apply the functions in this package to create algorithms to calculate endpoints for your data, the participants' substance use patterns must be stored in the "substance use pattern word" format shown below. 

We define the following five-value legend:

- **+**: positive for the substance(s) in a specified window of time (a day, week, month, etc.) by urine screen (or participant self report, if such data are of interest)
- **–**: negative for the substance(s)
- **o**: subject failed to provide a urine sample
- <b>*</b>: inconclusive results or mixed results (e.g. subject provided more than one urine sample in the time interval and they did not agree)
- <b>_</b>: no specimens required (weekends, holidays, pre-randomization period, alternating visit days/weeks)

For example, a participant who attended the clinic for three consecutive weeks on Mondays, Wednesdays, and Fridays, and was abstinent from the substance of interest on all days but the first two, would have their substance use pattern recorded as (assuming the week starts on Sunday):
```
"_+_+_-__-_-_-__-_-_-_"
```

As another example, consider a clinical trial with urine samples submitted weekly over a 12 week protocol. If a trial participant submitted positive urine samples for the first 3 weeks, but then did not return to the clinic for the remainder of the trial (they were lost to follow-up), then their substance use pattern would be recorded in a "word" as:
```
"+++ooooooooo"
```

-------------------------------------------------------------------------------
</br>


## Example

This is a basic example which shows how to measure a participant's duration in the study:

``` r
library(CTNote)

usePattern_char <- "__++++*o-------+--+-o-o-o+o+oooooo"

measure_retention(usePattern_char)
```

This function will count the number of visits (weeks in this case) until the last non-missing symbol is observed---in this case, 28 weeks, including the first two weeks before the randomization period (shown as `__`).

