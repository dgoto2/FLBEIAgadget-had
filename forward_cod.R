## updating Gadget input files using Rgadget uisng the example cod model
## updated may 7, 2019

#devtools::install_github("hafro/rgadget")
library(Rgadget)
library(mfdb)

## input parameters
path <- 'models/cod'

mainfile <- 'main'
num.years <- 20
num.iter <- 10
params.file <- 'params.in'

pre <- 'ALT'
imm.stock <- 'codimm'
mat.stock <- 'codmat'
options(stringsAsFactors = FALSE)

block.size <- 5
rec.step <- 1
ref.fleet <- c('aut', 'igfs')
harvest.rate <- seq(0, 1, by=0.01)
asserr.cv <- 0.26
asserr.rho <- 0.8
ref.cm <- 75
bloss <- 9929629
bpa <- bloss
blim <- bpa/exp(1.645*0.2) ## always set as B_pa/1.4

## read function
readoutput <- function(x){
  tmp <- readLines(x)
  file.remove(x)
  preamble <- tmp[grepl(';', tmp)]
  body <- tmp[!grepl(';', tmp)]
  header <- preamble[grepl('year-step-area', preamble)] %>% 
    gsub('; (*)','\\1',.) %>% 
    str_split('-') %>% 
    unlist() %>% 
    gsub(' ','_',.)
  body %>% 
    paste(collapse='\n') %>% 
    read.table(text=.,
               col.names = header, fill=TRUE) %>% 
    dplyr::mutate(trial = cut(1:length(year),
                              c(0,which(diff(year)<0),1e9),
                              labels = FALSE)-1) %>% 
    tibble::as_tibble() 
  }
pre.fleets <- data_frame(fleet_name = c('comm','aut','igfs'))
ref.points <- 
  data_frame(switch = c('codmat.btrigger',
                        'codmat.blim',
                        'cod.hr.low',
                        'cod.hr.high',
                        'fleet.prop.predict.comm',
                        'fleet.prop.predict.aut',
                        'fleet.prop.predict.igfs'),
             value = c(0, bloss, 0, 0.2,
                       0.66, 0.27, .07),
             lower = 0.5*value,
             upper = pmax(1.5*value,1),
             optimise = 0)


## Prediction fleets are specified as a data.frame
## these borrow selection functions from the model fleets
## if base_fleet is not defined it is assumed that the fleet_name column
## is the base_fleet. Also if not specified the fleet will be a totalfleet
pre.fleets$base_fleet <- pre.fleets$base_fleet%||%pre.fleets$fleet_name
pre.fleets$fleet_type <- pre.fleets$fleet_type%||%'totalfleet'
params <- read.gadget.parameters(file = sprintf('%s/%s', path, params.file))

## Read in the model:
run.dir <- gadget.variant.dir(path = path,variant_dir = pre)

## stock files
main <- read.gadget.file(path, mainfile, file_type = 'main')
stocks <- 
  main$stock$stockfiles %>% 
  purrr::map(~gadgetstock(.,path)) %>% 
  set_names(.,map(.,~.[[1]]$stockname) %>% unlist())

## collect the model fleets
fleets <- 
  main$fleet$fleetfiles %>% 
  purrr::map(~gadgetfleet(., path))

## check if the projection fleets are defined 
tmp <- 
  fleets %>% 
  purrr::flatten() %>% 
  purrr::map(~.[[1]]) %>% 
  unlist() %>% 
  setdiff(pre.fleets$base_fleet,.) %>% 
  map(~stop(sprintf('Projection fleet %s not found',.)))

## set the projection period
class(main[[1]]$timefile) <- c('gadgettime',class(main[[1]]$timefile))
attr(main[[1]]$timefile,'file_config')$file_type <- 'time'
attr(main[[1]]$timefile,'file_config')$mainfile_section <- 'timefile'

main[[1]]$timefile %>%  
  gadget_update(lastyear = .[[1]]$lastyear + num.years) %>% 
  write.gadget.file(run.dir)

## create a timing schedule for the fleet operations
schedule <- 
  expand.grid(year = seq(main[[1]]$timefile[[1]]$lastyear,
                         by=1,
                         length.out = num.years+1),
              step = 1:main[[1]]$timefile[[1]]$notimesteps[1],
              area = main[[1]]$areafile[[1]]$areas) %>% 
  dplyr::filter(!(year==main[[1]]$timefile[[1]]$lastyear & 
                    step <= main[[1]]$timefile[[1]]$laststep),
                !(year==max(year) & step > main[[1]]$timefile[[1]]$laststep)) %>% 
  dplyr::arrange(year,step,area)

## update the area file to include temperature data in the projection
attr(main[[1]]$areafile,'file_config')$file_type <- 'area'
attr(main[[1]]$areafile,'file_config')$mainfile_section <- 'areafile'

watertemp <- 5
main[[1]]$areafile[[1]] <- 
  main[[1]]$areafile[[1]] %>%
  #bind_rows(list(newareafile))
  bind_rows(list(schedule %>%
              mutate(mean=watertemp)))   
main[[1]]$areafile %>% 
  write.gadget.file(run.dir) 

## setup hockey stick spawning function
attr(stocks[[mat.stock]],'file_config')$mainfile_overwrite = TRUE
stocks[[mat.stock]] %>% 
  gadget_update('doesspawn',
                spawnfile = gadgetfile(
                  'hockeyspawnfile', component=list(list(
                    spawnsteps = stocks[[imm.stock]]$doesrenew$normalparamfile[[1]]$step %>% unique(),
                    spawnareas = .[[1]]$livesonareas,
                     firstspawnyear = min(schedule$year)-stocks[imm.stock][[1]][[1]]$minage,
                     lastspawnyear = max(schedule$year),
                     spawnstocksandratios = paste('hockeystock',1),
                     proportionfunction = 'constant 1',
                     mortalityfunction = 'constant 0',
                     weightlossfunction = 'constant 0',
                     recruitment = 'hockeystick',
                     gadgetfile(
                       'hockeyrec', file_type = 'timevariable',
                       components=list(list(
                         'hockeyrec', data = tibble::data_frame(    
                           year = main[[1]]$timefile[[1]]$firstyear,        
                           step = main[[1]]$timefile[[1]]$firststep, 
                           value = "0") %>%
                           dplyr::bind_rows(expand.grid(year = unique(schedule$year), 
                                         step = stocks[[imm.stock]]$doesrenew$normalparamfile[[1]]$step %>% 
                                        unique()) %>%      
                               dplyr::mutate(value = paste0('#', imm.stock,'.rec.', year), 
                                             year = year - stocks[[imm.stock]][[1]]$minage)) %>% 
                                            dplyr::select(year,step,value) %>%
                                            as.data.frame()))),
                     sprintf('#%s.blim',mat.stock),
                     stockparameters =  stocks[[imm.stock]]$doesrenew$normalparamfile[[1]] %>% 
                     ## what about other types of recruitment files?
                       tail(1) %>% 
                       select(mean:beta)%>% 
                       unlist() %>% 
                       map(to.gadget.formulae) %>% 
                       unlist()
                     )))) %>% 
  write.gadget.file(run.dir)

## pick a scenario first
stocks[[imm.stock]] %>% write.gadget.file(run.dir)

## setup hockeystick recruitment
gadgetstock('hockeystock', run.dir, missingOkay = TRUE) %>% 
  gadget_update('refweight', data = stocks[[imm.stock]][[1]]$refweightfile[[1]] ) %>%
  gadget_update('stock',
                minage = 0,
                maxage = stocks[imm.stock][[1]][[1]]$minage,
                minlength = stocks[imm.stock][[1]][[1]]$minlength,
                maxlength = stocks[imm.stock][[1]][[1]]$maxlength,
                dl = stocks[imm.stock][[1]][[1]]$dl,
                livesonareas = stocks[imm.stock][[1]][[1]]$livesonareas) %>%
  gadget_update('naturalmortality', rep(0,.[[1]]$maxage+1)) %>% 
  gadget_update('doesgrow', 0) %>% 
  gadget_update('doesmove',
                transitionstocksandratios = sprintf('%s 1', imm.stock),
                transitionstep = stocks[[imm.stock]]$doesrenew$normalparamfile[[1]]$step %>% unique() %>% head(1)) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = .[[1]]$livesonareas,
                                         age.factor = 0,   
                                         area.factor = 0,
                                         mean = .[[1]]$minlength,
                                         stddev = 1,
                                         alpha = 0.00001,
                                         beta = 3)) %>% 
  write.gadget.file(run.dir)

suits <-                                                      
  setNames(pre.fleets$base_fleet%||%pre.fleets$fleet_name, 
           pre.fleets$fleet_name) %>% 
  map(function(x){
    fleets %>% 
      purrr::map(~purrr::keep(.,~.[[1]]==x)) %>% 
      purrr::flatten() %>% 
      purrr::map(c('suitability')) %>%
      purrr::flatten() 
  }) %>% 
  purrr::map(~bind_rows(.)) %>% 
  dplyr::bind_rows(.id='fleet') %>% 
  tidyr::gather(stock,val,-c(1)) %>% 
  dplyr::group_by(fleet,stock) %>% 
  dplyr::mutate(n=paste0('n', 1:n())) %>% 
  tidyr::spread(n,val,fill= '') %>% 
  dplyr::ungroup()

## define fleet amounts that are parametrised by year, step, area 
fleet.amount <- 
  pre.fleets$fleet_name %>% 
  setNames(.,.) %>% 
  purrr::map(function(x) 
    schedule %>% 
      dplyr::mutate(number= 1) %>% #paste('#fleet',x,year,step,area,sep='.')) %>% 
      structure(area_group = lift(mfdb_group)(main[[1]]$areafile[[1]]$areas %>% 
                                                setNames(.,.))))

## define the projection fleets
fleet.tmp <- 
  gadgetfleet('fleet.predict',path,missingOkay = TRUE) 
for(i in seq_along(pre.fleets$fleet_name)){
  fleet.tmp <- 
    fleet.tmp %>% 
    gadget_update(pre.fleets$fleet_type[i],
                  name = paste0('predict.',pre.fleets$fleet_name[i]),
                  suitability = 
                    suits %>% 
                    dplyr::filter(fleet == pre.fleets$fleet_name[i]) %>% 
                    tidyr::unite(col,-matches('fleet'),sep='\t') %>% 
                    dplyr::select(-fleet) %>% 
                    unlist() %>% 
                    paste(collapse="\n") %>% 
                    sprintf('\n%s',.),
                  data = fleet.amount[[pre.fleets$fleet_name[i]]])
  }
fleet.tmp %>% write.gadget.file(run.dir)

## Define the prognosis likelihood
pre.fleet.names <- fleet.tmp %>% map(~.[[1]]) %>% unlist() %>% as.character()

## assumes expL50 as that is the only thing allowed
biocoeffs <- 
  suits %>% 
  dplyr::filter(fleet == ref.fleet,tolower(n2) == 'exponentiall50') %>%
  dplyr::select(-c(1:4)) %>% 
  tidyr::unite(col,everything(),sep='\t') %>% 
  dplyr::mutate(col=paste(col,1,sep='\t')) %>% 
  unlist() %>% 
  paste(collapse="\n")

progn <- 
  progn <- 
  gadgetfile('fleet.likelihood',
             file_type = 'likelihood',
             components = list(list('[component]',name = 'prognosis',
                                    weight = 1,
                                    type = "proglikelihood",
                                    fleetnames = pre.fleet.names,
                                    stocknames = c(imm.stock,mat.stock),
                                    biocoeffs = c(-100,ref.cm,1,-100,ref.cm,1),
                                    triggercoeffs=c(0,0,0,0,0,1), 
                                    triggervalues=c(0, sprintf('#%s.btrigger',mat.stock)),
                                    harvestrates = sprintf(c('#%s.hr.low','#%s.hr.high'),ref.fleet),
                                    quotasteps = c(3, 4, 5, 6),
                                    quotaproportions = c(0.25, 0.25, 0.25, 0.25),
                                    fleetproportions= sprintf('#fleet.prop.%s',pre.fleet.names),
                                    weightoflastyearstac=0,
                                    maxchange=4,
                                    functionnumber=1,
                                    firsttacyear=min(schedule$year),
                                    assessmentstep=2,
                                    asserr=gadgetfile(
                                      'asserr', file_type = 'timevariable', 
                                      components=list(
                                        list('asserr', data = data_frame(
                                          year = main[[1]]$timefile[[1]]$firstyear,     
                                          step = main[[1]]$timefile[[1]]$firststep,   
                                          value = "0") %>% 
                                            bind_rows(expand.grid(year = unique(schedule$year),  
                                                                  step = 2) %>% 
                                                        dplyr::mutate(value = paste0('#',mat.stock,'.asserr.',year))) %>%       
                                            as.data.frame())))))) 
attr(progn,'file_config')$mainfile_overwrite = TRUE
progn %>%  write.gadget.file(run.dir)

## projections (stochastic)
rep.params <- function(params,year_range = 2016){ 
  expand.grid(iter=1:num.iter,harvest.rate=harvest.rate) %>% 
    dplyr::mutate(run.id=1:n()) %>% 
    split(.$run.id) %>% 
    purrr::map(function(x){
        block.rec <-
            imm.stock %>% 
            purrr::set_names(.,.) %>% 
            purrr::map(~stocks[[.]]$doesrenew$normalparamfile[[1]] %>%
                                   dplyr::filter(year < year_range) %>% 
                                   dplyr::mutate(number = number %>% 
                                                     unlist() %>% 
                                                     map(~eval(.,setNames( nm=params$switch,
                                                                           params$value) %>% 
                                                                 as.list())) %>% 
                                                     unlist())) %>%
            dplyr::bind_rows(.id='stock') %>%
            split(.$stock) %>% 
            purrr::map(function(x) {
                schedule %>%
                    filter(step==1) %>% 
                    mutate(number=(tseries::tsbootstrap(x$number,type='block',
                                                        nb=2*num.years/30,b=block.size) %>% 
                                     as.numeric())[1:n()])
            }) %>%
            bind_rows(.id='stock') %>% 
            dplyr::transmute(switch=sprintf('%s.rec.%s',stock,year),
                             value = number,
                             upper=1.5*number,
                             lower=0.5*number,
                             optimise = 0)

      asserr <- 
        mat.stock %>% 
        purrr::set_names(.,.) %>%
        purrr::map(~schedule %>%  
                     dplyr::select(year) %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(number = arima.sim(list(ar = asserr.rho), n = n(),
                                                      rand.gen = function(x) rnorm(x,sd=asserr.cv*sqrt(1-asserr.rho^2))) %>% 
                                     exp())) %>% 
        dplyr::bind_rows(.id='stock') %>% 
        dplyr::ungroup() %>% 
        dplyr::transmute(switch=sprintf('%s.asserr.%s',stock,year),
                         value = number,
                         upper=1.5*number,
                         lower=0.5*number,
                         optimise = 0)
      params %>% 
        bind_rows(asserr,block.rec,ref.points) %>% 
        dplyr::mutate(value = ifelse(grepl('hr.high',switch),x$harvest.rate,value))
    })
}

#params.pre <- rep.params(params)
params.pre <-
  bootfit$params %>% 
  split(.$model) %>% 
  map(~select(.,-model)) %>% 
  parallel::mclapply(rep.params, 
                     mc.cores = parallel::detectCores(logical = TRUE),
                     year_range = 2005) %>% 
  bind_rows(.id='repl') %>% 
  tidyr::gather(model,col,-repl) %>% 
  tidyr::unnest(col)

## define the output, needs to be very compact
print <- gadgetfile(
  'pre.print',  components = list(
    list('[component]',      
         type = 'stockprinter',
         stocknames = mat.stock,
         areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg',mat.stock),
                         data = data.frame(
                           name = sprintf('area%s', stocks[[mat.stock]][[1]]$livesonareas),
                                           value = stocks[[mat.stock]][[1]]$livesonareas)),
         ageaggfile = gadgetdata(sprintf('Aggfiles/%s.ssb.age.agg', mat.stock),
                        data = data.frame(name = 'allages',
                                          value = paste(stocks[[mat.stock]][[1]]$minage:stocks[[mat.stock]][[1]]$maxage,
                                                        collapse = ' '))),
         lenaggfile =   gadgetdata(paste0('Aggfiles/',stocks[[mat.stock]][[1]]$stockname, '.stock.len.agg'),
                          data = data.frame(name = 'alllen',
                                            value = paste(stocks[[mat.stock]][[1]]$minlength,stocks[[mat.stock]][[1]]$maxlength,
                                                          sep = ' '))),
         printfile = gadgetfile(sprintf('out/%s.ssb',mat.stock)),
         printatstart = 1,
         yearsandsteps = 'all 1'),
    list('[component]',
         type = 'stockprinter',
         stocknames = imm.stock,
         areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg',imm.stock),
                         data = data.frame(name = sprintf('area%s',stocks[[imm.stock]][[1]]$livesonareas),
                                           value = stocks[[imm.stock]][[1]]$livesonareas)),
         ageaggfile = gadgetdata(sprintf('Aggfiles/%s.age.agg',imm.stock),
                        data = data.frame(name = stocks[[imm.stock]][[1]]$minage,
                                          value = stocks[[imm.stock]][[1]]$minage)),
         lenaggfile =   gadgetdata(paste0('Aggfiles/', stocks[[imm.stock]][[1]]$stockname, '.stock.len.agg'),
                          data = data.frame(name = 'alllen',
                                            value = paste(stocks[[imm.stock]][[1]]$minlength,stocks[[imm.stock]][[1]]$maxlength,
                                                          sep = ' '))),
         printfile = gadgetfile(sprintf('out/%s.rec',imm.stock)),
         printatstart = 1,
         yearsandsteps = 'all 2'),
    list('[component]',
         type = 'predatorpreyprinter',
         predatornames = pre.fleet.names,
         preynames = c(imm.stock, 
                       mat.stock),
         areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg', mat.stock),
                         data = data.frame(name = sprintf('area%s', stocks[[mat.stock]][[1]]$livesonareas),
                                           value = stocks[[mat.stock]][[1]]$livesonareas)),
         ageaggfile = gadgetdata(sprintf('Aggfiles/%s.stock.allage.agg', mat.stock),
                        data = tibble::data_frame(
                          value = paste(stocks[[imm.stock]][[1]]$minage:stocks[[mat.stock]][[1]]$maxage, 
                                        collapse = ' '), name = 'allages') %>% 
                          dplyr::select(name,value) %>% 
                          as.data.frame()),
         lenaggfile = gadgetdata(paste0('Aggfiles/', stocks[[mat.stock]][[1]]$stockname, '.stock.len.agg'),
                        data = data.frame(name = 'alllen',
                                          value = paste(stocks[[imm.stock]][[1]]$minlength,stocks[[mat.stock]][[1]]$maxlength,
                                                        sep = ' '))),
         printfile = gadgetfile('out/catch.lw'),
         yearsandsteps = 'all all'),
    list('[component]',
         type = 'predatorpreyprinter',
         predatornames = pre.fleet.names,
         preynames = c(imm.stock,mat.stock),
         areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg', mat.stock),   
                                  data = data.frame(name = sprintf('area%s', stocks[[mat.stock]][[1]]$livesonareas),
                                                    value = stocks[[mat.stock]][[1]]$livesonareas)),
         ageaggfile = gadgetdata(sprintf('Aggfiles/%s.stock.Fage.agg', mat.stock),                        
                                 data = tibble::data_frame(value = stocks[[mat.stock]][[1]]$maxage, 
                                                           name = 'allages') %>%
                                   dplyr::select(name,value) %>% 
                                   as.data.frame()), 
         lenaggfile = gadgetdata(paste0('Aggfiles/', stocks[[mat.stock]][[1]]$stockname, '.stock.len.agg'), 
                                 data = data.frame(name = 'alllen',           
                                                   value = paste(stocks[[imm.stock]][[1]]$minlength,stocks[[mat.stock]][[1]]$maxlength,
                                                                 sep = ' '))),
         printfile = gadgetfile('out/catch.F'),
         yearsandsteps = 'all all')))
attr(print,'file_config')$mainfile_section <- 'printfiles'
#write.gadget.file(print,run.dir)

Sys.setenv(GADGET_WORKING_DIR = normalizePath(path))
run.func <- function(x, asserr = TRUE, btrigger = FALSE){
  curr.dir <- sprintf('%s/run%s', tempdir(), attr(x, 'run_id'))
  dir.create(curr.dir)
  dir.create(paste(curr.dir,'out',sep='/'))
  vd <- gadget.variant.dir(path,variant_dir = curr.dir,mainfile = 'main')
  vd[[1]] <- ''
  read.gadget.file(run.dir, 'main', recursive = FALSE) %>% 
    write.gadget.file(vd, recursive = FALSE)  
  print %>% write.gadget.file(vd)
  if(!asserr){
    x<-
      x %>%
      mutate_at(vars(matches("asserr")),function(x) 1)
    }
  if(btrigger){
    x<-
      x %>%
      mutate_at(vars(matches("btrigger")),function(x) bpa)
    }
  write.gadget.parameters(x, file = paste(curr.dir, 'params.pre', sep='/'), columns = FALSE)
  callGadget(s=1,
             main = attr(vd, 'mainfile'),
             i = paste(curr.dir, 'params.pre', sep='/'),
             ignore.stderr = FALSE,
             gadget.exe = '~/bin/gadget')
  list.files(paste(curr.dir,'out', sep='/'), 
             full.names = TRUE) %>%
    purrr::set_names(.,.) %>% 
    purrr::map(readoutput) 
  }

tmp <- 
  params.pre %>% 
  mutate(repl = ceiling(1:n()/686)) %>% 
  split(. $repl) %>%
  parallel::mclapply(function(x) {
    dplyr::select(x,switch,value) %>% 
      tidyr::spread(switch,value)
    }, mc.cores = parallel::detectCores(logical = TRUE)) %>% 
  dplyr::bind_rows() %>% 
  split(. $lln.hr.high) 

tmp2 <- 
  harvest.rate %>% 
  purrr::set_names(as.character(.),.) %>% 
  purrr::map(function(x){ attr(tmp[[x]], 'run_id') <- x;tmp[[x]]})

resnotrigger <- mclapply(tmp2, run.func, mc.cores = detectCores(logical = TRUE))
res0 <- mclapply(tmp2, run.func, mc.cores = detectCores(logical = TRUE),asserr=FALSE)
resbtrigger <- mclapply(tmp2, run.func, mc.cores = detectCores(logical = TRUE),btrigger=TRUE)

res_adv <- 
  names(res) %>% 
  set_names(.,.) %>% 
  discard(~class(res[[.]])!='list')%>% 
  mclapply(.,function(x){
    mF <- 
      res[[x]][[1]] %>% 
      dplyr::filter(year>2060) %>% 
      dplyr::group_by(trial) %>% 
      dplyr::summarise(mF=mean(mortality))
    mLND <- 
      res[[x]][[2]] %>% 
      dplyr::filter(year > 2060) %>% 
      group_by(year, trial) %>% 
      dplyr::summarise(lnd = sum(biomass_consumed)/1e6) %>% 
      group_by(trial) %>% 
      dplyr::summarise(ML=mean(lnd),
                       lndq95 = quantile(lnd, 0.95),
                       lndq05 = quantile(lnd, 0.05))
    rec <- 
      res[[x]][[3]] %>% 
      dplyr::filter(year > 2060) %>% 
      group_by(trial) %>% 
      dplyr::summarise(rec=mean(number),
                       recq95 = quantile(number, 0.95),
                       recq05 = quantile(number, 0.05))
    mSSB <-     
      res[[x]][[4]] %>% 
      dplyr::filter(year > 2060) %>% 
      group_by(year,trial) %>% 
      dplyr::summarise(SSB = sum(number*mean_weight)/1e6) %>% 
      group_by(trial) %>% 
      dplyr::summarise(below_Bpa = max(SSB<(bpa/1e6)),
                       below_Blim = max(SSB<(blim/1e6)),
                       pBpa = mean(SSB<(bpa/1e6)),
                       pBpaplus = mean(SSB<(1.4*bpa/1e6)),
                       pBlim = mean(SSB<(blim/1e6)),
                       SSBq95 = quantile(SSB, 0.95),
                       SSBq05 = quantile(SSB, 0.05),
                       SSB=mean(SSB))
    
    mF %>% 
      left_join(mLND,by = 'trial') %>% 
      left_join(rec,by = 'trial') %>% 
      left_join(mSSB,by = 'trial')
    }, 
    mc.cores = detectCores(logical = TRUE))

 
res2 <- 
  bind_rows(res_adv, .id='rate') %>% 
  group_by(rate) %>% 
  dplyr::summarise(mssb = median(SSB),
                   ussb = quantile(SSB, 0.95),
                   lssb = quantile(SSB, 0.05),
                   mssbq5 = median(SSBq05),
                   mssbq95 = median(SSBq95),
                   mrec = median(rec),
                   urec = quantile(rec, 0.95),
                   lrec = quantile(rec, 0.05),
                   mlnd = median(ML),
                   ulnd = quantile(ML, 0.95),
                   llnd = quantile(ML, 0.05),
                   mlndq5 = median(lndq05),
                   mlndq95 = median(lndq95),
                   uF = quantile(mF, 0.95),
                   lF = quantile(mF, 0.05),
                   mF = median(mF),
                   p_belowBpa = mean(below_Bpa),
                   p_belowBlim = mean(below_Blim),
                   pBpa = mean(pBpa),
                   pBlim = mean(pBlim),
                   pBpaplus = mean(pBpaplus))
save(res0, file = 'res0_cod.Rdata')
save(res,file = 'res_cod.Rdata')

res_by_year <-
    names(res) %>% 
    set_names(.,.) %>% 
    discard(~class(res[[.]])!='list')%>% 
    mclapply(.,function(x){
        mF <- 
            res[[x]][[1]] %>% 
            dplyr::group_by(year) %>% 
            dplyr::summarise(mF = mean(mortality),
                             uF = quantile(mortality, 0.95),
                             uuF = quantile(mortality, 0.75),
                             llF = quantile(mortality, 0.25),
                             lF = quantile(mortality, 0.05),
                             F6 = max(mortality[trial==6]),
                             F542 = max(mortality[trial==542]),
                             F931 = max(mortality[trial==931]))
        mLND <- 
            res[[x]][[2]] %>% 
            dplyr::group_by(year) %>% 
            dplyr::summarise(mlnd = mean(biomass_consumed),
                             ulnd = quantile(biomass_consumed, 0.95),
                             uulnd = quantile(biomass_consumed, 0.75),
                             lllnd = quantile(biomass_consumed, 0.25),
                             llnd = quantile(biomass_consumed, 0.05),
                             lnd6 = sum(biomass_consumed[trial==6]),
                             lnd542 = sum(biomass_consumed[trial==542]),
                             lnd931 = sum(biomass_consumed[trial==931]))
        rec <- 
            res[[x]][[3]] %>% 
            dplyr::group_by(year) %>% 
            dplyr::summarise(mrec = mean(number),
                             urec = quantile(number, 0.95),
                             uurec = quantile(number, 0.75),
                             llrec = quantile(number, 0.25),
                             lrec = quantile(number, 0.05),
                             rec6 = sum(number[trial==6]),
                             rec542=sum(number[trial==542]),
                             rec931=sum(number[trial==931]))
        mSSB <-     
            res[[x]][[4]] %>%
            dplyr::group_by(year) %>%
            dplyr::summarise(mssb = mean(number*mean_weight),
                             ussb = quantile(number*mean_weight, 0.95),
                             uussb = quantile(number*mean_weight, 0.75),
                             llssb = quantile(number*mean_weight, 0.25),
                             lssb = quantile(number*mean_weight, 0.05),
                             ssb6 = sum(number[trial==6]*mean_weight[trial==6]),
                             ssb542 = sum(number[trial==542]*mean_weight[trial==542]),
                             ssb931 = sum(number[trial==931]*mean_weight[trial==931]))
        mF %>% 
            left_join(mLND) %>% 
            left_join(rec) %>% 
            left_join(mSSB)
        },mc.cores = detectCores(logical = TRUE)) %>%
    bind_rows(.id='rate')
save(res20,res_by_year0,res_adv0,file='cod_adv_res0.Rdata')
