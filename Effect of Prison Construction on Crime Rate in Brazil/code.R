# necessary libraries
library(tidyverse)
library(haven)
library(Synth)
library(devtools)
if(!require(SCtools)) devtools::install_github("bcastanho/SCtools")
library(SCtools)

# Texas dataset
read_data <- function(df)
{
  full_path <- paste("https://github.com/scunning1975/mixtape/tree/master/Texas/Data", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

texas <- read_data("texas.dta") %>%
  as.data.frame(.)
  
###################
### REPLICATION ###
###################

## Texas and its Synthetic Control

dataprep_out <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1993,
  special.predictors = list(
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1993, # pretreatment for dependent
  time.plot = 1985:2000
)

synth_out <- synth(data.prep.obj = dataprep_out)

# synth table
synth.tables <- synth.tab(
  dataprep.res = dataprep_out,
  synth.res = synth_out)
print(synth.tables)

# path plot
path.plot(synth_out, dataprep_out,
          Ylab         = c("bmprison"),
          Xlab         = c("year"),
          Legend       = c("Texas","Synthetic Texas"),
          Legend.position = c("topright"),
          Main = "Trends in African-American Male Incarceration:\nTexas versus Synthetic Texas Replication")

abline(v   = 1993,
       lty = 2)

arrows(1991, 65000, 1993, 65000,
       #col    = "black",
       length = .1)

text(1989, 65000,
     "Increased Prison Capacity",
     cex = 1)
     
# gaps plot
gaps.plot(synth.res    = synth_out,
          dataprep.res = dataprep_out,
          Ylab         = c("Gap in black male prisoner prediction error"),
          Xlab         = c("year"),
          Main         = "African-American Male Incarceration Gap\nbetween Texas and Synthetic Texas\nReplication",
          Ylim         = c(-2000, 27000)
)

abline(v   = 1993,
       lty = 2)

arrows(1991, 15000, 1993, 15000,
       col    = "black",
       length = .1)

text(1989, 15000,
     "Increased Prison Capacity",
     cex = 1)
     
     
###################
#### EXTENSION ####
###################

# ROBUSTNESS TEST -> taking out highest weighted state (Louisiana 22) from synthetic control donor pool
dataprep_out_robust <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  time.predictors.prior = 1985:1993,
  special.predictors = list(
    list("bmprison", c(1988, 1990:1992), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  # taking out Louisiana 22
  controls.identifier = c(1,2,4:6,8:13,15:21,23:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1993, # pretreatment for dependent
  time.plot = 1985:2000
)

synth_out_robust <- synth(data.prep.obj = dataprep_out_robust)

synth.tables <- synth.tab(
  dataprep.res = dataprep_out_robust,
  synth.res = synth_out_robust)
print(synth.tables)

# path plot
path.plot(synth_out_robust, dataprep_out_robust,
          Ylab         = c("bmprison"),
          Xlab         = c("year"),
          Legend       = c("Texas","Synthetic Texas"),
          Legend.position = c("topright"),
          Main = "Trends in African-American Male Incarceration:\nTexas versus Synthetic Texas Robustness Check")

abline(v   = 1993,
       lty = 2)

arrows(1991, 65000, 1993, 65000,
       #col    = "black",
       length = .1)

text(1989, 65000,
     "Increased Prison Capacity",
     cex = 1)

# gaps plot
gaps.plot(synth.res    = synth_out_robust,
          dataprep.res = dataprep_out_robust,
          Ylab         = c("Gap in black male prisoner prediction error"),
          Xlab         = c("year"),
          Main         = "African-American Male Incarceration Gap\nbetween Texas and Synthetic Texas\nRobustness Check",
          Ylim         = c(-3000, 27000)
)

abline(v   = 1993,
       lty = 2)

arrows(1991, 15000, 1993, 15000,
       col    = "black",
       length = .1)

text(1989, 15000,
     "Increased Prison Capacity",
     cex = 1)

# IN-TIME PLACEBO TEST -> moving treatment to an earlier date when treatment has not yet occurred

dataprep_out_rep <- dataprep(
  foo = texas,
  predictors = c("poverty", "income"),
  predictors.op = "mean",
  # changing treatment year to 1990 for in-time placebo test
  time.predictors.prior = 1985:1990,
  # adjusting the dates to pre-1990 for special predictors
  special.predictors = list(
    list("bmprison", c(1988, 1990), "mean"),
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990, "mean"),
    list("black", 1990, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",
  unit.variable = "statefip",
  unit.names.variable = "state",
  time.variable = "year",
  treatment.identifier = 48,
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56),
  time.optimize.ssr = 1985:1990,
  # plotting time for up to 1993 treatment year
  time.plot = 1985:1993
)

synth_out_rep <- synth(data.prep.obj = dataprep_out_rep)

synth.tables <- synth.tab(
  dataprep.res = dataprep_out_rep,
  synth.res = synth_out_rep)
print(synth.tables)

# path plot
path.plot(synth_out_rep, dataprep_out_rep,
          Ylab         = c("bmprison"),
          Xlab         = c("year"),
          Legend       = c("Texas","Synthetic Texas"),
          Legend.position = c("topright"),
          Main = "Placebo Increased Prison Capacity 1990:\nTrends in African-American Male Incarceration\nTexas versus Synthetic Texas",
          Ylim         = c(10000, 80000))

abline(v   = 1990,
       lty = 2)

arrows(1989, 65000, 1990, 65000,
       #col    = "black",
       length = .1)

text(1988, 65000,
     "Placebo Increased\nPrison Capacity",
     cex = 1)

# gaps plot
gaps.plot(synth.res    = synth_out_rep,
          dataprep.res = dataprep_out_rep,
          Ylab         = c("Gap in black male prisoner prediction error"),
          Xlab         = c("year"),
          Main         = "Placebo Increased Prison Capacity 1990:\nAfrican-American Male Incarceration Gap\nbetween Texas and Synthetic Texas",
          Ylim         = c(-2000, 27000)
)

abline(v   = 1990,
       lty = 2)

arrows(1989, 15000, 1990, 15000,
       col    = "black",
       length = .1)

text(1988, 15000,
     "Placebo Increased\nPrison Capacity",
     cex = 1)
