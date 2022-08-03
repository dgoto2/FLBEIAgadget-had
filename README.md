# FLBEIA-Gadget
A Management Strategy Evaluation (MSE) framework using FLBEIA and Gadget

## Description
FLBEIA-Gadget is a Management Strategy Evaluation (MSE) framework using a customized version of FLBEIA (Bio-Economic Impact Assessment of Management strategies; https://github.com/REDUS-IMR/FLBEIA) with an R package of customized Gadget (Globally applicable Area Disaggregated General Ecosystem Toolbox, https://github.com/Hafro/gadget2), GadgetR (https://github.com/REDUS-IMR/gadget and also see a quick user guide, https://redus-imr.github.io/gadget/index.html), as operating model (OM). This framework is designed to run single and multispecies MSEs with one or more fleets and one or more areas on quarterly and yearly time steps. This framework has been tested for a single-species OM (a simple haddock Gadget model, https://github.com/gadget-framework/gadget2) only, however. 

## Prerequisites
Install the following packages:
```r
# GadgetR
  devtools::install_github("REDUS-IMR/gadget", ref="gadgetr")

# Rgadget
  devtools::install_github("hafro/rgadget")

# FLR (Fisheries Library in R) packages
  install.packages(c("FLFleet", "FLa4a", "FLash", "FLAssess", "ggplotFL", "FLSAM", "FLCore"), repos="http://flr-project.org/R")
  
  install.packages(c("dplyr", "copula", "triangle", "coda"))  
  
# FLBEIAgadget  
  devtools::install_github("REDUS-IMR/FLBEIA", ref="FLBEIAgadget")


```

#### OM conditioning (see the scripts (`flbeia_gadget_mse_age_4s_had_XX.R` with different assessment models) above for examples using the haddock model)
To condition a gadget-based OM, the gadget.fit() function from Rgadget can be used to extract stock information (numbers and weight by age, etc.) as
```r
paramsfile <- 'refinputfile' # Gadget model input file name
fit <- gadget.fit(wgts = NULL, params.file = paramsfile, steps = "all")

## stock number by age 
data_n <- fit$stock.std %>% 
  filter(year < proj.yr) %>% 
  select(year, step, age, area, number) %>% 
  rename(season = step, data = number) ## from gadget model output

## stock weight by age
data_wt <- fit$stock.std %>% 
  filter(year < proj.yr) %>% 
  select(year, step, age, area, mean_weight) %>% 
  rename(season = step, data = mean_weight)

## recruit number
data_rec <- fit$stock.recruitment %>% 
  filter(year < proj.yr) 

```
And the following stock-specific parameters need to be specified based on the gadget model input files (stock and fleet names must be the same as in the gadget files) and the script, `gadget-fls.R`, contains a collection of helper functions for MSE simulations;

```r
## Gadget parameters placeholders
oneGDGT <- list()
oneGDGT$gadget.inputDir <- paste0(getwd(), "/models/had")
oneGDGT$gadget.mainFile <- "main"
oneGDGT$gadget.paramFile <- "refinputfile"
oneGDGT$runNow <- FALSE
oneGDGT$fleetMode <- "exclusive" # "exclusive" = gadget fleets only model, "mixed" = FLBEIA & gadget fleets mode

## setting up gadget an OM
## If Gadget model names and FLBEIA names are not the same
convertStockName <- list(stk1 = "had")
convertFleetName <- list(fl1 = "future")
stockList <- c("had")

## specify stocks and fleets for gadget input and simualtions 
had.fleets <- c("comm", "survey", "future")
had.stocks <- c("had")
had.stocks.mature <- c("had")
had.surveys <- c("survey")
had.forecasts <- c("future")
had.forecasts.tac.proportion <- c(0.232, 0.351, 0.298, 0.119) ## this needs to be the same as FLBEIA ################
#had.forecasts.tac.proportion <- c(1, 1, 1, 1)

## specify fleets and metiers
fleetList <- c("fl1")

## Below is using the same information as the FLBEIA conditioning
#fl1.mets       <- c('met1')
#fl1.met1.stks  <- c('stk1')

## parameterize mortality
## m2=NULL means we calculate m2 from gadget result, m2=0 means we use only residual mortality (m1). 
## NOTE: m1 can be a vector or scalar.
## stockStep is the step number where the stock is going to be calculated
stockstep = 4
stk1_ind1_range.startf <- 0.12 # the initial time step 
stk1_ind1_range.endf <- 1 - 0.12 # the last time step
had.params <- list(stockStep = stockstep, 
                   minage = stk1.age.min, 
                   maxage = stk1.age.max, 
                   minfbar = stk1_range.minfbar, 
                   maxfbar = stk1_range.maxfbar, 
                   startf = stk1_ind1_range.startf, 
                   endf = stk1_ind1_range.endf, 
                   m1 = c(0.5, 0.35, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.4, 0.7),
                   m2 = NULL)

## gadget simulation parameters (should be the same as FLBEIA)
firstYear <- first.yr
projYear <- proj.yr
finalYear <- last.yr

```


## References
GadgetR: Interactive and Exploratory Gadget Model Simulation in R (https://redus-imr.github.io/gadget/index.html).

Olsen, E., S. Aanes, M. Aldrin, O.N. Breivik, E. Fuglebakk, D. Goto, N. O. Handegard, C. Hansen, A.J. Holmin, D. Howell, E. Johnsen, N. Jourdain, K. Korsbrekke, O. Kotaro, H. Otterå, H.A. Perryman, S. Subbey, G. Søvik, I. Umar, S. Vatnehol, J.H. Vølstad. 2021. [Final Report for the REDUS (Reduced Uncertainty in Stock Assessment) Project](https://www.hi.no/hi/nettrapporter/rapport-fra-havforskningen-en-2021-16). Rapport fra havforskningen 2021-16 ISSN: 1893-4536. https://github.com/REDUS-IMR.
