# FLBEIA-Gadget
A Management Strategy Evaluation (MSE) framework using FLBEIA and Gadget

## Description
FLBEIA-Gadget is a Management Strategy Evaluation (MSE) framework using a customized version of FLBEIA (Bio-Economic Impact Assessment of Management strategies; https://github.com/REDUS-IMR/FLBEIA) with an R package of customized Gadget (Globally applicable Area Disaggregated General Ecosystem Toolbox, https://github.com/Hafro/gadget2), GadgetR (https://github.com/REDUS-IMR/gadget), as operating model (OM). This framework is designed to run single and multispecies MSEs with one or more fleets and one or more areas on quarterly and yearly time steps. This framework has been tested for a single-species OM only, however. 

## Prerequisites
Install the following packages:
```
  devtools::install_github("REDUS-IMR/gadget", ref="gadgetr")
  devtools::install_github("hafro/rgadget")
  devtools::install_github("REDUS-IMR/FLBEIA", ref="FLBEIAgadget")

  install.packages(c("FLFleet", "FLa4a", "FLash", "FLAssess", "ggplotFL", "FLSAM", "FLCore"), repos="http://flr-project.org/R")
  
  install.packages(c("dplyr", "copula", "triangle", "coda"))  
```
