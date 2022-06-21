# FLBEIA-Gadget
A Management Strategy Evaluation (MSE) framework using FLBEIA and Gadget

## Description
FLBEIA-Gadget is a Management Strategy Evaluation (MSE) framework using a customized version of FLBEIA (Bio-Economic Impact Assessment of Management strategies; https://github.com/REDUS-IMR/FLBEIA) with an R package of customized Gadget (Globally applicable Area Disaggregated General Ecosystem Toolbox, https://github.com/Hafro/gadget2), GadgetR (https://github.com/REDUS-IMR/gadget and also see a quick user guide, https://redus-imr.github.io/gadget/index.html), as operating model (OM). This framework is designed to run single and multispecies MSEs with one or more fleets and one or more areas on quarterly and yearly time steps. This framework has been tested for a single-species OM (a simple haddock Gadget model, https://github.com/gadget-framework/gadget2) only, however. 

## Prerequisites
Install the following packages:
```
# GadgetR
  devtools::install_github("REDUS-IMR/gadget", ref="gadgetr")

# Rgadget
  devtools::install_github("hafro/rgadget")

# FLBEIAgadget  
  devtools::install_github("REDUS-IMR/FLBEIA", ref="FLBEIAgadget")

# FLR (Fisheries Library in R) packages
  install.packages(c("FLFleet", "FLa4a", "FLash", "FLAssess", "ggplotFL", "FLSAM", "FLCore"), repos="http://flr-project.org/R")
  
  install.packages(c("dplyr", "copula", "triangle", "coda"))  
```

## References
GadgetR: Interactive and Exploratory Gadget Model Simulation in R (https://redus-imr.github.io/gadget/index.html).

Olsen, E., S. Aanes, M. Aldrin, O.N. Breivik, E. Fuglebakk, D. Goto, N. O. Handegard, C. Hansen, A.J. Holmin, D. Howell, E. Johnsen, N. Jourdain, K. Korsbrekke, O. Kotaro, H. Otterå, H.A. Perryman, S. Subbey, G. Søvik, I. Umar, S. Vatnehol, J.H. Vølstad. 2021. [Final Report for the REDUS (Reduced Uncertainty in Stock Assessment) Project](https://www.hi.no/hi/nettrapporter/rapport-fra-havforskningen-en-2021-16). Rapport fra havforskningen 2021-16 ISSN: 1893-4536. https://github.com/REDUS-IMR.
