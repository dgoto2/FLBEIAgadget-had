## testing the FLBEIA-Gadget MSE framework with the Icelandic cod Gadget example model as operating model and SAM as assessment model
## started: mar 11, 2019; updated may 3, 2019

## install customized packages for gadget
# devtools::install_github("REDUS-IMR/FLBEIA", ref="FLBEIAgadget") # FLBEIA modified to work with a gadget model as the OM
# devtools::install_github("REDUS-IMR/gadget", ref="gadgetr") # for linking with a gadget model
# devtools::install_github("hafro/rgadget")

## load requied packages
library(dplyr)
library(FLCore) 
library(FLAssess)
library(FLash)
library(FLFleet)
library(FLa4a)
library(FLBEIA) # requires FLEBIAgadget version
library(ggplot2)
library(FLSAM)  # requires FLBIEAgadget version
library(gtools)
library(Rgadget)
library(gadgetr)
library(patchwork)  ## optional package 

# a simple example cod model with single stock, single fleet and one iteration (from https://github.com/gadget-framework/rgadget). 
#system.file('extdata', 'cod_model.tgz', package = 'Rgadget') %>% untar(exdir = path.expand('./gadget_example/'))

## The Operating Model (OM) is a gadget (single or multispecies) model linked by gadgetr  
## Only one fleet which activity is performed in an unique metier and the time step is annual. 
## The historic data for the example cod stock: from 1961 to 2013, projection period: from 2014 onward.

## Operating model:
## Population dynamics: Age-length structured population growth using a gadget model
## SR model: Ricker autoregressive regression or other alternative models, if more suitable

## Management Procedure:
## age-specific indices
## catch-at-age model (SAM)
## ICES HCR

## Conditioning
## biols: 
first.yr          <- 1962
proj.yr           <- 2009
last.yr           <- 2013
yrs <- c(first.yr=first.yr, proj.yr=proj.yr, last.yr=last.yr)
fls  <- c('fl1')
stks <- c('stk1')
fl1.mets       <- c('met1')
fl1.met1.stks  <- c('stk1')
ni           <- 1 
it           <- 1:ni
ns           <- 1
stk1.age.min    <- 1
stk1.age.max    <- 12
stk1.unit       <- 1         

## Data: stk1_n.flq, m, spwn, fec, wt - use FLQuant
## stock stk1
## get the output from gadget fit
## change the working directory to the location of the gadget model
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path_model <- paste0("models/cod")
setwd(list.dirs(path = path_model, recursive = T)[1])
mypath <- getwd()

## To estimate the model parameters the suggested procedure is to use the iterative reweighting approach with is implemented in the gadget.iterative function.
paramsfile <- 'params.in'
#gadget.iterative(main='main',
#                  grouping=list(sind1=c('si.gp1','si.gp1a'),
#                               sind2=c('si.gp2','si.gp2a'),
#                               sind3=c('si.gp3','si.gp3a')),
#                  params.file = 'paramsfile', 
#                  wgts='WGTS')

## If you ran the gadget.iterative
fit <- gadget.fit()
## otherwise
#fit <- gadget.fit(wgts = NULL, params.file = paramsfile)
fit$res.by.year
fit$fleet.info
fit$stock.std

## Check the stock input data for conditioning the OM
theme_set(theme_light()) ## set the plot theme (optional)
scale_fill_crayola <- function(n = 100, ...) {
  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
           "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
           "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
  pal <- rep(pal, n)
  ggplot2::scale_fill_manual(values = pal, ...)
}
theme_set(theme_light())
plot(fit, data='res.by.year', type='total')
plot(fit, data='res.by.year', type='F')
plot(fit, data='res.by.year', type='rec')
plot(fit, data='res.by.year', type='catch')
plot(fit, data='stock.std')

## reset the directory
setwd("../..")
dir <- getwd()

## get the stock input data (derived from the Gadget model fitting output or assessments) for conditioning the OM
data_n = fit$stock.std %>% # stock size only for 'hindcasts'
  filter(year < proj.yr) %>% 
  select(year, age, area, number) %>% 
  rename(data = number)
data_wt = fit$stock.std %>%
  filter(year < proj.yr ) %>% 
  select(year, age, area, mean_weight) %>% 
  rename(data = mean_weight)
stk1_n.flq = iter(as.FLQuant(as.data.frame(data_n)), it) 
#stk1_n.flq[1, ] = fit$res.by.year$recruitment[1:(proj.yr-first.yr)]  
stk1_wt.flq = iter(as.FLQuant(as.data.frame(data_wt)), it) 
stk1_m.flq = FLQuant(c(0.5, 0.35, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.4, 0.7), 
                     dim=c(stk1.age.max, proj.yr-first.yr, 1), 
                     quant='age', 
                     dimnames=list(age = stk1.age.min:stk1.age.max, 
                                   year = first.yr:(proj.yr-1))) 
stk1_spwn.flq = FLQuant(1, 
                        dim=c(stk1.age.max, proj.yr-first.yr, 1), 
                        quant='age', 
                        dimnames=list(age = stk1.age.min:stk1.age.max, 
                                     year = first.yr:(proj.yr-1))) 
stk1_fec.flq = FLQuant(c(0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
                       dim=c(stk1.age.max, proj.yr-first.yr,1), 
                       quant='age', 
                       dimnames=list(age = stk1.age.min:stk1.age.max, 
                                     year = first.yr:(proj.yr-1))) 
#stk1_mat.flq   <- stk1_fec.flq 
mature <- fit$stock.std %>%
  filter(stock =='codmat' & year < proj.yr) %>%
  select(year, area, age, number) %>%
  rename(number.mat = number) 
immature <- fit$stock.std %>%
  filter(stock =='codimm' & year < proj.yr) %>%
  select(year, area, age, number) %>%
  rename(number.imm = number) %>%
  as.data.frame()
mature_prop <- immature %>%
  full_join(mature) %>%
  mutate(number.imm =replace(number.imm, which(is.na(number.imm)), 0)) %>%
  mutate(number.mat =replace(number.mat, which(is.na(number.mat)), 0)) %>%
  group_by(year, area, age) %>%
  summarise(number.imm = number.imm,
            number.mat = number.mat,
            number.total = number.imm + number.mat, 
    data = number.mat/(number.imm+number.mat))
mature_prop = mature_prop %>%
  mutate(data=replace(data, which(is.na(data)), 0)) %>%
  select(-c(number.imm, number.mat, number.total)) %>%
  as.data.frame()
stk1_mat.flq <- iter(as.FLQuant(as.data.frame(mature_prop)), it) 
stk1_range.min       <- 1
stk1_range.max       <- 12
stk1_range.plusgroup <- 12
stk1_range.minyear   <- 1962
stk1_range.minfbar   <- 3 
stk1_range.maxfbar   <- 10 
stk1_biol.proj.avg.yrs  <- c((proj.yr-3):(proj.yr-1)) 
stks.data <- list(stk1=ls(pattern="^stk1")) 
biols    <- create.biols.data(yrs, ns, ni, stks.data)

## SRs: 
data_rec = fit$res.by.year %>% 
  filter(stock == 'codimm') %>%
  filter(year < proj.yr) %>% 
  select(year, area, recruitment) %>% 
  rename(data = recruitment)
data_rec$data = data_rec$data/10000
data_rec$age <- 2
stk1_rec.flq = iter(as.FLQuant(as.data.frame(data_rec)), it) 
stk1_ssb.flq = ssb(biols[[1]][, 1:(proj.yr-first.yr)]) 

## fit the stock-recruit model 
stk1_sr.model        <- 'rickerAR1'
stk1_params.n        <- 3
stk1_params.name     <- c('a','b','c')
sr.modelfit <- fmle(FLSR(model = stk1_sr.model, #"rickerAR1", 
                         ssb = stk1_ssb.flq[, 1:(proj.yr-first.yr-1)],
                         rec = stk1_rec.flq[, 2:(proj.yr-first.yr)]))
#plot(sr.modelfit)
sr_params = as.data.frame(FLQuant(sr.modelfit@params@.Data[, 1], 
                                  dim = c(stk1_params.n, last.yr-first.yr+1, 1, ns, 1, ni), 
                                  quant = 'param', 
                                  dimnames = list(param = c("a", "b", "c"), 
                                                  year = first.yr:last.yr, 
                                                  #area = na,
                                                  season = ns,
                                                  iter = it)))
stk1_params.array    <- xtabs2(data ~ param + year + season + iter, 
                               data = sr_params,#as.data.frame(DATAparams[, -1]),
                               exclude = NULL, 
                               na.action = na.pass)[, , , it, drop = F]   
stk1_uncertainty.flq = FLQuant(1,#c(1, 1.1, 1.2, 1.3, 1.4), 
                               dim = c(1.1, last.yr-first.yr+1, 1, ns, 1, ni), 
                               dimnames = list(year = first.yr:last.yr, season=1, iter=it))
stk1_proportion.flq = FLQuant(1, 
                              dim = c(1, last.yr-first.yr+1, 1, ns, 1, ni), 
                              dimnames = list(year = first.yr:last.yr, season=1, iter=it))
stk1_prop.avg.yrs    <- c((proj.yr-3):(proj.yr-1)) 
stk1_timelag.matrix  <- matrix(c(1, 1), 
                               nrow=2, ncol=1, 
                               dimnames=list(c('year', 'season'), 'all'))

## FLBEIA input object: SRs
stks.data <- list(stk1 = ls(pattern="^stk1")) 
SRs     <- create.SRs.data(yrs, ns, ni, stks.data)

## Data per fleet
##    effort, crewshare, fcost, capacity
## Data per fleet and metier
##    effshare, vcost
## Data per fleet, metier and stock
data_catch = fit$stock.prey %>% 
  filter(year < proj.yr & year >= first.yr) %>% 
  select(year, stock, age, area, biomass_consumed) %>% 
  group_by(year, area, age) %>%
  summarise(data = sum(biomass_consumed))

## landings.n, discards.n,landings.wt, discards.wt, landings, discards, landings.sel, discards.sel, price
fl1.met1.stk1_landings.n.flq = iter(as.FLQuant(as.data.frame(data_catch)), it) 
fl1.met1.stk1_discards.n.flq = fl1.met1.stk1_landings.n.flq 
fl1.met1.stk1_discards.n.flq[] = 0 

## economic parameter values are arbitrarily set for this example
fl1_effort.flq = FLQuant(1, 
                         dim = c(1, proj.yr-first.yr, 1, ns), 
                         dimnames = list(year = first.yr:(proj.yr-1)))
fl1_capacity.flq = FLQuant(1, 
                           dim = c(1, proj.yr-first.yr, 1, ns),
                           dimnames = list(year = first.yr:(proj.yr-1)))
fl1_fcost.flq = FLQuant(1, 
                        dim = c(1, proj.yr-first.yr, 1, ns), 
                        dimnames = list(year = first.yr:(proj.yr-1)))
fl1_crewshare.flq = FLQuant(1, 
                            dim = c(1, proj.yr-first.yr, 1, ns), 
                            dimnames = list(year = first.yr:(proj.yr-1)))
fl1.met1_effshare.flq = FLQuant(1, 
                                dim = c(1, proj.yr-first.yr, 1, ns),
                                dimnames = list(year = first.yr:(proj.yr-1)))

## Projection
## fleets: fl1
fl1_proj.avg.yrs            <- c((proj.yr-3):(proj.yr-1))  
fl1.met1_proj.avg.yrs       <- c((proj.yr-3):(proj.yr-1))  
fl1.met1.stk1_proj.avg.yrs  <- c((proj.yr-3):(proj.yr-1))  

## create fleets object
fls.data <- list(fl1=ls(pattern="^fl1")) 
fleets   <- create.fleets.data(yrs, ns, ni, fls.data, stks.data)

## advice: 
data_tac = fit$res.by.year %>% 
  select(year, area, catch) %>% 
  rename(data = catch)
data_tac$data[(proj.yr-first.yr+1):(last.yr-first.yr+1)] = NA
stk1_advice.TAC.flq         <- iter(as.FLQuant(as.data.frame(data_tac)), it)
stk1_advice.TAC.flq    <- window(stk1_advice.TAC.flq, first.yr, last.yr)
stk1_advice.quota.share.flq <- FLQuant(1, 
                                       dim = c(1, last.yr-first.yr+1, 1), 
                                       dimnames = list(year = first.yr:last.yr))
stk1_advice.avg.yrs         <- c((proj.yr-3):(proj.yr-1)) 

## create advice object
stks.data <- list(stk1=ls(pattern="^stk1")) 
advice   <- create.advice.data(yrs, ns, ni, stks.data, fleets)

## indices:
indices <- NULL

## generate indices
flq   <- biols[["stk1"]]@n 
unc   <- id <- q <- flq 
unc[] <- rlnorm(prod(dim(flq)), 0, 0.3) 
q[]   <- rep(runif(dim(flq)[1], 1e-05/2, 1e-05*5), dim(flq)[2]) 
id    <- biols[["stk1"]]@n*unc*q 
stk1_indices = c('ind1') 
stk1_ind1_index.flq  <- id 
stk1_ind1_index.q.flq <- q 
stk1_ind1_index.var.flq <- unc 
stk1_ind1_range.startf <- 0.12 
stk1_ind1_range.endf <- 1 - 0.12 
stk1_ind1_range.min <- stk1.age.min+1 
stk1_ind1_range.max <- stk1.age.max 
##  YFT_cpue_range.plusgroup <- 0
stk1_ind1_range.minyear <- first.yr
stk1_ind1_range.maxyear <- proj.yr-1
stk1_ind1_type <- "FLIndex"
stks.data <- list(stk1=ls(pattern="^stk1")) 
oneIndAge <- create.indices.data(yrs, ns, ni, stks.data)
#summary(oneIndAge)

## main.ctrl
main.ctrl           <- list()
main.ctrl$sim.years <- c(initial = proj.yr, final = last.yr)

## Gadget parameters placeholders
oneGDGT <- list()
oneGDGT$gadget.inputDir <- paste0(getwd(), "/cod")
oneGDGT$gadget.mainFile <- "main"
oneGDGT$gadget.paramFile <- paramsfile
oneGDGT$runNow <- FALSE

## biols.ctrl: 
growth.model     <- c('gadgetGrowth')
biols.ctrl       <- create.biols.ctrl(stksnames = stks, growth.model = growth.model)

## fleets.ctrl: 
n.fls.stks      <- 1
fls.stksnames   <- 'stk1'
effort.models    <- 'fixedEffort'
effort.restr.fl1 <- 'stk1'
restriction.fl1  <- 'catch'
catch.models     <- 'gadgetCatch'
capital.models   <- 'fixedCapital'       
flq.stk1<- FLQuant(dimnames = list(age = 'all', 
                                   year = first.yr:last.yr, 
                                   unit = stk1.unit, 
                                   season = 1:ns, 
                                   iter = 1:ni)) 
fleets.ctrl      <- create.fleets.ctrl(fls = fls, 
                                       n.fls.stks = n.fls.stks, 
                                       fls.stksnames = fls.stksnames,
                                       effort.models = effort.models, 
                                       catch.models = catch.models,
                                       capital.models = capital.models, 
                                       flq = flq.stk1,
                                       effort.restr.fl1 = effort.restr.fl1, 
                                       restriction.fl1 = restriction.fl1)
fleets.ctrl$fl1$stk1$discard.TAC.OS  <- FALSE
fleets.ctrl$fl1$restriction <- "landings"

## advice.ctrl: 
# hypothetical HCR and reference points for this example
HCR.models       <- c('IcesHCR') 
blim = mean(stk1_ssb.flq)*0.15
btrigger = mean(stk1_ssb.flq)*0.20
print("Blim"); blim
print("Btrigger"); btrigger
fmsy = 0.10
ref.pts.stk1      <- matrix(rep(c(blim, btrigger, fmsy), 3), 3, ni, 
                            dimnames=list(c('Blim', 'Btrigger','Fmsy'), 1:ni))
advice.ctrl      <- create.advice.ctrl(stksnames = stks, 
                                       HCR.models = HCR.models, 
                                       ref.pts.stk1 = ref.pts.stk1, 
                                       first.yr = first.yr, 
                                       last.yr = last.yr)
advice.ctrl[['stk1']][['sr']]            <- list()
advice.ctrl[['stk1']][['sr']][['model']] <- 'geomean'
#advice.ctrl[['stk1']][['sr']][['params']] <- c(sr_params[, (last.yr-proj.yr+1)],  ni) # optional parameters for SR
#advice.ctrl[['stk1']][['sr']][['years']] <- c(y.rm = 3, num.years = 10) # optional parameters for SR
advice.ctrl$stk1$AdvCatch <- rep(TRUE, length(first.yr:last.yr))   #TRUE advice in catches, FALSE advice in landings
names(advice.ctrl$stk1$AdvCatch) <- as.character((first.yr:last.yr))

## assess.ctrl: 
assess.ctrl <- create.assess.ctrl(stksnames = stks, assess.models = assess.models)
assess.ctrl[['stk1']]$work_w_Iter   <- TRUE
## statistical catch-at-age assessment - SAM
assess.ctrl.sam <- assess.ctrl
assess.ctrl.sam[["stk1"]]$assess.model <- "sam2flbeia"
assess.ctrl.sam[["stk1"]]$harvest.units <- "f"
assess.ctrl.sam[["stk1"]]$control$indices.type <- "number"

### obs.ctrl: 
## age-structured observation model (age2ageDat) option
stkObs.models    <- 'age2ageDat'
flq.stk1 <- FLQuant(dimnames = list(age = 'all', 
                                    year = first.yr:last.yr, 
                                    unit = stk1.unit, 
                                    season = ns, 
                                    iter = 1:ni)) 
obs.ctrl.age <- create.obs.ctrl(stksnames = stks, 
                                stkObs.models = stkObs.models, 
                                flq.stk1 = flq.stk1)
obs.ctrl.age[['stk1']][['indObs']] <- vector('list', 1)
names(obs.ctrl.age[['stk1']][['indObs']]) <- c("ind1")
obs.ctrl.age[['stk1']][['indObs']][['ind1']] <- list()
obs.ctrl.age[['stk1']][['indObs']][['ind1']][['indObs.model']]  <- 'ageInd'

## Create the FLIndices object
flq   <- biols[["stk1"]]@n
na <- stk1.age.max
ny <- length(first.yr:last.yr)
ages.error <- array(0, dim = c(na, na, ny, ni))
## generate errors using the Dirichlet distribution (n, alpha)
for(a in 1:na){
  for(i in 1:ni){
    for(y in 1:ny){
      if(a == 1) ages.error[1,,y,i] <- rdirichlet(1, c(0.85, 0.1, 0.05, rep(0, 9))) 
      if(a == 2) ages.error[2,,y,i] <- rdirichlet(1, c(0.1, 0.75, 0.1, 0.05, rep(0, 8)))
      if(a == 3) ages.error[3,,y,i] <- rdirichlet(1, c(0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 7)))
      if(a == 4) ages.error[4,,y,i] <- rdirichlet(1, c(rep(0, 1), 0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 6)))
      if(a == 5) ages.error[5,,y,i] <- rdirichlet(1, c(rep(0, 2), 0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 5)))
      if(a == 6) ages.error[6,,y,i] <- rdirichlet(1, c(rep(0, 3), 0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 4)))
      if(a == 7) ages.error[7,,y,i] <- rdirichlet(1, c(rep(0, 4), 0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 3)))
      if(a == 8) ages.error[8,,y,i] <- rdirichlet(1, c(rep(0, 5), 0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 2)))
      if(a == 9) ages.error[9,,y,i] <- rdirichlet(1, c(rep(0, 6), 0.05, 0.1, 0.7, 0.1, 0.05, rep(0, 1)))
      if(a == 10) ages.error[10,,y,i] <- rdirichlet(1, c(rep(0, 7), 0.05, 0.1, 0.7, 0.1, 0.05))
      if(a == 11) ages.error[11,,y,i] <- rdirichlet(1, c(rep(0, 8), 0.05, 0.1, 0.7, 0.1))
      if(a == 12) ages.error[12,,y,i] <- rdirichlet(1, c(rep(0, 9), 0.05, 0.1, 0.85))
      
    }
  }
} 

## set dataframes for uncertainty parameters
nmort.error       <- fec.error         <-  land.wgt.error    <- stk.nage.error <- stk.wgt.error <-
  disc.wgt.error    <- land.nage.error   <- disc.nage.error   <-    flq
TAC.ovrsht        <- flq[1, ]
dimnames(TAC.ovrsht)[[1]] <- 'all' 
obs.ctrl.age$stk1$stkObs$TAC.ovrsht <- TAC.ovrsht
obs.ctrl.age$stk1$stkObs$TAC.ovrsht[] <- 10
obs.ctrl.age$stk1$stkObs$land.bio.error <- TAC.ovrsht
obs.ctrl.age$stk1$stkObs$land.bio.error[] <- 50
bs.ctrl.age$stk1$stkObs$disc.bio.error <- TAC.ovrsht
obs.ctrl.age$stk1$stkObs$disc.bio.error[] <- 10
obs.ctrl.age[['stk1']][['stkObs']][['ages.error']] <- ages.error
slts <- c('nmort.error', 'fec.error', 'land.wgt.error', 
          'stk.nage.error', 'stk.wgt.error', 'disc.wgt.error', 
          'land.nage.error', 'disc.nage.error')
for(sl in slts){
  obs.ctrl.age[['stk1']][['stkObs']][[sl]]   <- get(sl)
  obs.ctrl.age[['stk1']][['stkObs']][[sl]][] <- rnorm(prod(dim(flq)), 1, .1)
  }

## Check the observation controls related to the assessment and the observation of the index
obs.ctrl.age$stk1$stkObs$stkObs.model
obs.ctrl.age$stk1$indObs
summary(obs.ctrl.age)

## BDs/covars/covars.ctrl/ NULL objects
BDs         <- NULL
covars      <- NULL
covars.ctrl <- NULL

## Save input objects
save(biols, SRs, BDs, fleets, covars, indices, advice, main.ctrl, biols.ctrl, fleets.ctrl,
     covars.ctrl, obs.ctrl.age, assess.ctrl.a4a, advice.ctrl, file="input_flbeia-gadget_age_cod.RData")

## check for errors in the input objects:
checkFLBEIAData( biols = biols,
                 SRs = SRs, 
                 BDs = BDs, 
                 fleets = fleets, 
                 covars = covars,
                 indices = indices, 
                 advice = advice, 
                 main.ctrl = main.ctrl, 
                 biols.ctrl = biols.ctrl, 
                 fleets.ctrl = fleets.ctrl,
                 covars.ctrl = covars.ctrl, 
                 obs.ctrl = obs.ctrl.age, 
                 assess.ctrl = assess.ctrl.a4a, 
                 advice.ctrl = advice.ctrl)

## parameterize the gadget model
## If Gadget model names and FLBEIA names are not the same
convertStockName <- list(stk1=c("cod"))
convertFleetName <- list(fl1="future")
stockList <- c("cod")

## specify stocks and fleets for gadget input and simulations 
stk1.fleets <- c("comm", "future")
stk1.stocks <- c("codimm", "codmat")
stk1.stocks.mature <- c("codmat")
stk1.surveys <- c("igfs", "aut", "future.igfs")
stk1.forecasts <- c("future")
stk1.forecasts.tac.proportion <- c(0.232, 0.351, 0.298, 0.119)
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
stockstep = 2 
cod.params <- list(stockStep = stockstep, 
                   minage = stk1.age.min, 
                   maxage = stk1.age.max, 
                   minfbar = stk1_range.minfbar, 
                   maxfbar = stk1_range.maxfbar, 
                   startf = stk1_ind1_range.startf, 
                   endf = stk1_ind1_range.endf, 
                   m1 = c(0.5, 0.35, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.4, 0.7),
                   m2 = NULL)

## gadget simulation parameters (should be the same as FLBEIA)
firstYear <- first.yr
projYear <- proj.yr
finalYear <- last.yr

## reset the directory
dir <- getwd()

## Load helper functions
source(paste0(dir, "/gadget-fls.R"), local = T)

#----------------
#updateFLFleet("fl1", 0 , 0 , 0)
#stop()

## Run FLBEIA
s0 <- FLBEIA(biols = biols, 
             SRs = SRs, 
             BDs = BDs,
             GDGT = oneGDGT,      # GADGET as OM
             fleets = fleets, 
             covars = covars, 
             indices = oneIndAge, 
             advice = advice, 
             main.ctrl = main.ctrl, 
             biols.ctrl = biols.ctrl, 
             fleets.ctrl = fleets.ctrl, 
             covars.ctrl = covars.ctrl, 
             obs.ctrl = obs.ctrl.age, 
             assess.ctrl = assess.ctrl.sam,  
             advice.ctrl = advice.ctrl) 

## reset the directory
setwd(dir)

## Results
plot(s0$biols[[1]]) 
plot(s0$stocks[[1]]) 

stk1.mp1 <- s0$stocks[['stk1']]
stk1.om1 <- FLBEIA:::perfectObs(s0$biols[['stk1']], s0$fleets, year = dim(s0$biols[['stk1']]@n)[2])
adf <- as.data.frame
s0_pop <- rbind( data.frame(population='obs', indicator='SSB', as.data.frame(ssb(stk1.mp1))), 
                 data.frame(population='obs', indicator='Harvest', as.data.frame(fbar(stk1.mp1))), 
                 data.frame(population='obs', indicator='Catch', as.data.frame(catch(stk1.mp1))),
                 data.frame(population='obs', indicator='Recruitment', as.data.frame(rec(stk1.mp1))),
                 data.frame(population='real', indicator='SSB', as.data.frame(ssb(stk1.om1))), 
                 data.frame(population='real', indicator='Harvest', as.data.frame(fbar(stk1.om1))), 
                 data.frame(population='real', indicator='Catch', as.data.frame(catch(stk1.om1))),
                 data.frame(population='real', indicator='Recruitment', as.data.frame(rec(stk1.om1))))
plot1 <- ggplot(data=s0_pop, aes(x=year, y=data, color=population)) + 
  geom_line() +
  facet_grid(indicator ~ ., scales="free") + 
  geom_vline(xintercept = main.ctrl$sim.years[['initial']]-1, linetype = "longdash")+
  theme_bw()+
  theme(text=element_text(size=15),
        title=element_text(size=15,face="bold"),
        strip.text=element_text(size=15), 
        legend.position="top")+
  ylab("")
print(plot1)
