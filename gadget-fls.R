# FLFleet might not be needed?
updateFLFleet <- function(fleetTitle, out, gadgetYear, fl_fleet) {

	getFleetMetiers <- function(fleetName, stocksInvolved, out) {
		xtemp <- NULL

		catches <- out[["fleets"]][[fleetName]][["catch"]]

		for(stockName in names(catches)){
			if(stockName %in% stocksInvolved){
				if(ncol(catches[[stockName]]) > 0)					
					xtemp <- rbind(xtemp, catches[[stockName]])
			}
		}
		return(xtemp)
	}

	print(paste("Fleet name:", fleetTitle))

	realFleetName <- convertFleetName[fleetTitle]

	mets <- eval(parse(text=paste0(fleetTitle, ".mets")))

	print(paste("Metiers:", mets))

	# For each metier in the fleet
	for(metName in mets){
		metStocks <- eval(parse(text=paste0(fleetTitle, ".", metName, ".stks")))

		# Translate stock names
		realStkNames <- unlist(sapply(metStocks, function(x) convertStockName[[x]]))
		print(realStkNames)
		print(paste(metName, "catch", realStkNames, "from", realFleetName))

		# Get metier catches
		metCatches <- getFleetMetiers(realFleetName, realStkNames, out)
	}
}

# Helper function for FLStock
updateFLStock <- function(stockTitle, out, gadgetYear, fl_stock, fl_index, stockStep = 1) {

	getStocks <- function(stockTitle, out, suffix=".stocks") {
		xtemp <- list()
		xtemp$stk <- NULL
		xtemp$ssb <- NULL
		xtemp$rec <- NULL

		for(stockName in names(out[["stocks"]])){
			if(stockName %in% eval(parse(text=paste0(stockTitle, suffix)))){
				if(ncol(out[["stocks"]][[stockName]][["stk"]]) > 0)
					xtemp$stk <- rbind(xtemp$stk, out[["stocks"]][[stockName]][["stk"]])
				if(ncol(out[["stocks"]][[stockName]][["ssb"]]) > 0)				
					xtemp$ssb <- rbind(xtemp$ssb, out[["stocks"]][[stockName]][["ssb"]])
				if(ncol(out[["stocks"]][[stockName]][["rec"]][["spawn"]]) > 0)		
					xtemp$rec <- rbind(xtemp$rec, out[["stocks"]][[stockName]][["rec"]][["spawn"]])			
			}
		}
		return(xtemp)
	}

	getCatches <- function(stockTitle, out, type) {
		xtemp <- NULL

		for(fleetName in names(out[["fleets"]])) {
			catches <- out[["fleets"]][[fleetName]][["catch"]]
			#print(paste(fleetName, "->"))
			#print(fleetName %in% eval(parse(text=paste0(stockTitle, ".", type))))

			for(stockName in names(catches)){
				#print(stockName)
				#print(stockName %in% eval(parse(text=paste0(stockTitle, ".stocks"))))
				if(fleetName %in% eval(parse(text=paste0(stockTitle, ".", type))) && stockName %in% eval(parse(text=paste0(stockTitle, ".stocks")))){
					#print(paste(paste0(stockTitle, ".", type), "-", paste0(stockTitle, ".stocks")))
					if(ncol(catches[[stockName]]) > 0)					
						xtemp <- rbind(xtemp, catches[[stockName]])
				}
			}
		}
		return(xtemp)
	}

	getEaten <- function(stockTitle, out) {
		xtemp <- NULL

		for(stockNm in names(out[["stocks"]])) {
			catches <- out[["stocks"]][[stockNm]][["eat"]]
			for(stockName in names(catches)){
				if(stockName %in% eval(parse(text=paste0(stockTitle, ".stocks")))){
					if(ncol(catches[[stockName]]) > 0)
						xtemp <- rbind(xtemp, catches[[stockName]])
				}
			}
		}
		return(xtemp)
	}

	print(gadgetYear)

	## Get params
	stockParams <- eval(parse(text=paste0(stockTitle, ".params")))

	# Get iteration (or ID)
	#iter <-	match(stockTitle, stockList)
	iter <- 1	
	print(paste("Iter:", iter))
	
	# Getting survey and catch
	survey.data <- getCatches(stockTitle, out, "surveys")
	catch.data <- getCatches(stockTitle, out, "fleets")
	eaten.data <- getEaten(stockTitle, out)

	# Delete Age zero
	survey.data <- survey.data[!survey.data[,"age"]==0,]
	catch.data <- catch.data[!catch.data[,"age"]==0,]
	eaten.data <- eaten.data[!eaten.data[,"age"]==0,]

	# Process catch
	catch <- aggregate(biomassConsumed ~ year + area, data=catch.data, FUN=sum)
	catch.n <- aggregate(numberConsumed ~ year + area + age, data=catch.data, FUN=sum)
	catch.biomass <- aggregate(biomassConsumed ~ year + area + age, data=catch.data, FUN=sum) 
	catch.wt <- catch.biomass
	catch.wt[,"biomassConsumed"] <- catch.wt[,"biomassConsumed"]/catch.n[,"numberConsumed"]
	#catch.wt[is.na(catch.wt[,"biomassConsumed"]),"biomassConsumed"]  <- max(catch.wt[,"biomassConsumed"], na.rm = T)

	# Process survey
	index <- aggregate(biomassConsumed ~ year + area, data=survey.data, FUN=sum)
	index.n <- aggregate(numberConsumed ~ year + area + age, data=survey.data, FUN=sum)
	index.biomass <- aggregate(biomassConsumed ~ year + area + age, data=survey.data, FUN=sum)
	index.wt <- index.biomass
	index.wt[,"biomassConsumed"] <- index.wt[,"biomassConsumed"]/index.n[,"numberConsumed"]
	#index.wt[is.na(index.wt[,"biomassConsumed"]),"biomassConsumed"] <- max(index.wt[,"biomassConsumed"], na.rm = T)

	# Process Eaten
	isEaten <- FALSE
	#print(eaten.data)
	if(!is.null(eaten.data) && nrow(eaten.data)>0) {
		isEaten <- TRUE
		eaten <- aggregate(biomassConsumed ~ year + area, data=eaten.data, FUN=sum)
		eaten.n <- aggregate(numberConsumed ~ year + area + age, data=eaten.data, FUN=sum)
		eaten.biomass <- aggregate(biomassConsumed ~ year + area + age, data=eaten.data, FUN=sum)
		eaten.wt <- eaten.biomass
		eaten.wt[,"biomassConsumed"] <- eaten.wt[,"biomassConsumed"]/eaten.n[,"numberConsumed"]
	}

	# Getting stock information
	stock.data <- getStocks(stockTitle, out)

	# Delete Age zero
	stock.data$stk <- stock.data$stk[!stock.data$stk[,"age"]==0,]

	# Only take the first timestep. This is dynamic (as recruitment can happen in step 1 in the current year or step 4 in the previous year)
	#stock.data$stk <- stock.data$stk[stock.data$stk[,"step"]==stockStep,]

	# Process stock information
	stock <- aggregate(number * meanWeights ~ year + area, data=stock.data$stk, FUN=sum)
	stock.n <- aggregate(number ~ year + area + age, data=stock.data$stk, FUN=sum)
	stock.biomass <- aggregate(number * meanWeights ~ year + area + age, data=stock.data$stk, FUN=sum)
	stock.wt <- stock.biomass
	stock.wt[, ncol(stock.wt)] <- stock.wt[, ncol(stock.wt)]/stock.n[, "number"]
	#stock.wt[is.na(stock.wt[, ncol(stock.wt)]), ncol(stock.wt)]  <- max(stock.wt[, ncol(stock.wt)], na.rm = T)

	# Process SSB information
	if(!is.null(stock.data$ssb))
		ssb <- aggregate(SSB ~ year + area, data=stock.data$ssb, FUN=sum)

	# Process Recruitment information
	if(!is.null(stock.data$rec))
		rec <- aggregate(Rec ~ year + area, data=stock.data$rec, FUN=sum)

	# Process maturity matrix
	## Getting mature stock information
	stock.mature.data <- getStocks(stockTitle, out, suffix = ".stocks.mature")
	## Delete Age zero
	stock.mature.data$stk <- stock.mature.data$stk[!stock.mature.data$stk[,"age"]==0,]
	
	## Only take from the first timestep
	## If we are not in step 1, copy from step 1
	if(stockStep == 1) {
		## Calculate mature stocks number
		stock.mature.n <- aggregate(number ~ year + area + age, data=stock.mature.data$stk, FUN=sum)
		mature <- stock.n
		mature[,ncol(mature)] <- stock.mature.n[,ncol(stock.mature.n)] / stock.n[,ncol(stock.n)]

		# Ensure maturity is valid number (usually happen when we have only one stock)
		mature[is.infinite(mature$number), "number"] <- 1
		mature[is.nan(mature$number), "number"] <- 1
		mature[mature$number>1, "number"] <- 1
	} else {
		mature <- stock.n
		mature[,ncol(mature)] <- as.numeric(mat(fl_stock)[,gadgetYear,,1,,iter])
	}

	# Process the fishing mortality matrix (F)
	mortF <- stock.n
	mortF[,ncol(mortF)] <- -log((stock.n[,ncol(stock.n)] - catch.n[,ncol(catch.n)])/stock.n[,ncol(stock.n)])

	# Process the predation mortality matrix (M2)
	mortPred <- stock.n
	if(isEaten)
		mortPred[,ncol(mortPred)] <- -log((stock.n[,ncol(stock.n)] - eaten.n[,ncol(eaten.n)])/stock.n[,ncol(stock.n)])
	else
		mortPred[,ncol(mortPred)] <- 0

	print("Putting into FLStock")

	# Put everything into FLStock	
	# Catch

	#print(stock.n)

	print("Catches")

	fl_stock@catch[,gadgetYear,,stockStep,,iter] <- catch[,ncol(catch)]
	fl_stock@catch.n[catch.n[,"age"],gadgetYear,,stockStep,,iter] <- catch.n[,ncol(catch.n)]
	fl_stock@catch.wt[catch.wt[,"age"],gadgetYear,,stockStep,,iter] <- catch.wt[,ncol(catch.wt)]

	print("Landings")

	# Landings
	fl_stock@landings[,gadgetYear,,stockStep,,iter] <- catch[,ncol(catch)]
	fl_stock@landings.n[catch.n[,"age"],gadgetYear,,stockStep,,iter] <- catch.n[,ncol(catch.n)]
	fl_stock@landings.wt[catch.wt[,"age"],gadgetYear,,stockStep,,iter] <- catch.wt[,ncol(catch.wt)]

	print("Discards")

	# Discards (use catch weight as discard weights)
	fl_stock@discards[,gadgetYear,,stockStep,,iter] <- 0
	fl_stock@discards.n[,gadgetYear,,stockStep,,iter] <- 0
	fl_stock@discards.wt[,gadgetYear,,stockStep,,iter] <- catch.wt[,ncol(catch.wt)]

	print("Stocks")

	# Stocks
	fl_stock@stock[,gadgetYear,,stockStep,,iter] <- stock[,ncol(stock)]
	fl_stock@stock.n[stock.n[,"age"], gadgetYear,,stockStep,,iter] <- stock.n[,ncol(stock.n)]
	fl_stock@stock.wt[stock.n[,"age"], gadgetYear,,stockStep,,iter] <- stock.wt[,ncol(stock.wt)]
	
	# SSB
	#fl_stock@stock <- FLQuant(stock[,ncol(stock)], dimnames=list(age="all", year=gadgetYear))

	# Recruitment
	#fl_stock@stock <- FLQuant(stock[,ncol(stock)], dimnames=list(age="all", year=gadgetYear))

	# Maturity
	fl_stock@mat[mature[,"age"], gadgetYear,,stockStep,,iter] <- mature[,ncol(mature)]

	# Mortality (in harvest with unit f)
	mortF[is.nan(mortF[,ncol(mortF)]), ncol(mortF)] <- 0
	fl_stock@harvest[mortF[,"age"], gadgetYear,,stockStep,,iter] <- mortF[,ncol(mortF)]

	#print(harvest(fl_stock))

	# Natural mortality (m)
        mortPred[is.nan(mortPred[,ncol(mortPred)]), ncol(mortPred)] <- 0
	if(is.null(stockParams[["m2"]]))
		fl_stock@m[stock.n[,"age"], gadgetYear,,stockStep,,iter] <- stockParams[["m1"]] + mortPred[,ncol(mortPred)]
	else
		fl_stock@m[stock.n[,"age"], gadgetYear,,stockStep,,iter] <- stockParams[["m1"]] + stockParams[["m2"]]

	#print(m(fl_stock))

	# Set spwns as 0
	fl_stock@m.spwn[stock.n[,"age"], gadgetYear,,stockStep,,iter] <- rep(0, length(stock.n[,"age"]))
	fl_stock@harvest.spwn[stock.n[,"age"], gadgetYear,,stockStep,,iter] <- rep(0, length(stock.n[,"age"]))

	print("Surveys")

	# Survey
	if(!is.null(fl_index)) {
		# Ensure we have free slot
		fl_index <- window(fl_index, end = as.numeric(gadgetYear))
		fl_index@catch.n[index.n[,"age"],gadgetYear,,stockStep,,iter] <- index.n[,ncol(index.n)]
		fl_index@catch.wt[index.wt[,"age"],gadgetYear,,stockStep,,iter] <- index.wt[,ncol(index.wt)]
		fl_index@index[index.n[,"age"],gadgetYear,,stockStep,,iter] <- index.n[,ncol(index.n)]
		fl_index@effort[,gadgetYear,,stockStep,,iter] <- 1
	}

	# Cleaning ups
	## Make sure we don't have zeros
	catch.n(fl_stock)[catch.n(fl_stock)==0] <- 1
	stock.n(fl_stock)[stock.n(fl_stock)==0] <- 1

	catch.n(fl_index)[catch.n(fl_index)==0] <- 1
	index(fl_index)[index(fl_index)==0] <- 1

	## Filling mean weights using Daisuke's method
	### Catch
#	catch.wt(fl_stock) <- fillWeights(catch.wt(fl_stock))
#	discards.wt(fl_stock) <- fillWeights(discards.wt(fl_stock))
#	landings.wt(fl_stock) <- fillWeights(landings.wt(fl_stock))
#	stock.wt(fl_stock) <- fillWeights(stock.wt(fl_stock))
#	mat(fl_stock) <- fillWeights(mat(fl_stock))
#	harvest(fl_stock) <- fillWeights(harvest(fl_stock))	
	### Survey
#	catch.wt(fl_index) <- fillWeights(catch.wt(fl_index))

	# Setting units and ranges (TODO: only at the first run)
	#if(units(harvest(fl_stock)) == "NA") {
		## Setting units
		units(catch(fl_stock)) <- "kg"
		units(catch.n(fl_stock)) <- "1"
		units(catch.wt(fl_stock)) <- "kg"

		units(discards(fl_stock)) <- "kg"
		units(discards.n(fl_stock)) <- "1"
		units(discards.wt(fl_stock)) <- "kg"

		units(landings(fl_stock)) <- "kg"
		units(landings.n(fl_stock)) <- "1"
		units(landings.wt(fl_stock)) <- "kg"

		units(stock(fl_stock)) <- "kg"
		units(stock.n(fl_stock)) <- "1"
		units(stock.wt(fl_stock)) <- "kg"

		units(m(fl_stock)) <- "m"
		units(harvest(fl_stock)) <- "f"

		## Setting fbar range stk
		range(fl_stock)["minfbar"] <- stockParams[["minfbar"]]
		range(fl_stock)["maxfbar"] <- stockParams[["maxfbar"]]

		## Setting f range for index
		range(fl_index)["startf"] <- stockParams[["startf"]]
		range(fl_index)["endf"] <- stockParams[["endf"]]
	#}

	return(list(stk=fl_stock, idx=fl_index))
}




# Instruct gadget to run until a specific year
runUntil <- function(until) {

	combinedOut <- list()

	for (sname in stockList) {
		# Get params (for getting the ages)
		stockParams <- eval(parse(text=paste0(sname, ".params")))

		#print(paste(stockParams[["minage"]], stockParams[["maxage"]]))

		# Prepare FLstocks
		fl_stock <- FLStock(FLQuant(NA, dimnames=list(age=stockParams[["minage"]]:stockParams[["maxage"]], year=firstYear:(projYear-1), season=1:ns)))
		fl_index <- FLIndex(FLQuant(NA, dimnames=list(age=stockParams[["minage"]]:stockParams[["maxage"]], year=firstYear:(projYear-1), season=1:ns)))

		fl_stock@name <- sname
		fl_index@name <- sname

		combinedOut[[sname]] <- list(stk = fl_stock, idx = fl_index)
	}

	while (TRUE)
	{
		# Get params
		stockParams <- eval(parse(text=paste0(sname, ".params")))

		#stockStep <- stockParams[["stockStep"]]

		stats <- getEcosystemInfo()

		# Run and collect the stats for a step (use stock status after step)
		out <- runStep(stockAfterStep = TRUE)
		
		for (sname in stockList) {
			#print(sname)
			
			# Generate FLs
			updated <- updateFLStock(sname, out, as.character(stats$time[["currentYear"]]), combinedOut[[sname]]$stk, combinedOut[[sname]]$idx, as.character(stats$time[["currentStep"]]))

			combinedOut[[sname]]$stk = updated$stk
			combinedOut[[sname]]$idx = updated$idx
		}

		if( getEcosystemInfo()$time[["currentYear"]] == until)
			break
	}
	return(combinedOut)
}

# Try to fill empty weight using the available mean weights
#fillWeights <- function(wt) {
#	wt <- as.data.frame(wt)	
#	wt2 <- wt %>%
#		na.omit() %>%
#		dplyr::group_by(season, age) %>%
#		dplyr::summarise(data = mean(data, na.rm=T))
#	wt$data[which(is.na(wt$data))] <- wt2$data
#	return(wt$data)
#}

