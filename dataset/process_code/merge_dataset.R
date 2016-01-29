
########################################################################
# INPUT FILES (see also README in the Documentation)
########################################################################

# Get the usual file where all data is saved at each step:
# global_output_file: see create_dataset.R
# by now this one has the SDC with the added COMPUSTAT data
load(global_output_file) # always get the data from the previous step, add new data, and save back to it again

# Market and FF Data
# This file has all the FF and market data, already done by get_ff_and_market_data.R
load("dataset/indices_and_factors/market_ff_data.Rdata")

# CRSP data
# These 3 .csv files are assumed to be there. 
# These 2 are generated manually from CRSP as described in Steps 3 in README
file_in_CRSP_daily   = paste("dataset/",datasetname,"/returns/crsp_returns.csv",sep="")
file_in_CRSP_monthly = paste("dataset/",datasetname,"/returns/crsp_returns_monthly.csv",sep="")

# IBES data
# This file is generated manually from IBES as described in Step 5 in README
file_in_IBES <- paste("dataset/",datasetname,"/ibes/ibes_all.csv",sep="")

########################################################################
# OUTPUT FILES 
########################################################################

# again to the usual file:
# global_output_file: see create_dataset.R

#################################################### 
# CLEAN UP IN THIS FILE:

# we remove these events here:
# sum(dropi | duplicatesi | notfoundi)
# these are events for which we could not find the CRSP data (notfoundi) or 
# did not have enough pre-event CRSP returns (min 20 non-zero daily 
# prices to get the penny-stock filter related price) (dropi)
# or we had multiple  CRSP data (duplicatesi)


######################################################################
## Step 1: load "returns" matrix and "return_links" mapping from the CRSP related files
######################################################################

CRSPdata = read.csv(file_in_CRSP_daily,header = TRUE, stringsAsFactors= FALSE,check.names=FALSE, dec=".")
CRSPdata$RET <- suppressWarnings(scrub(as.double(gsub(" ", "", CRSPdata$RET)))) # make sure we don't miss any spaces...
CRSPdata$date <- as.Date(as.character(CRSPdata$date),format="%Y%m%d",origin="19700101")

CRSPdata_monthly = read.csv(file_in_CRSP_monthly,header = TRUE, stringsAsFactors= FALSE,check.names=FALSE, dec=".")
CRSPdata_monthly$RET <- suppressWarnings(scrub(as.double(gsub(" ", "", CRSPdata_monthly$RET)))) # make sure we don't miss any spaces...
CRSPdata_monthly$date <- as.Date(as.character(CRSPdata_monthly$date),format="%Y%m%d",origin="19700101")

# Get the CRSP returns first
#####

CRSP_returns= dcast(CRSPdata, date ~ PERMNO, value.var="RET")  
CRSP_returns_monthly= dcast(CRSPdata_monthly, date ~ PERMNO, value.var="RET")  
rownames(CRSP_returns) <- CRSP_returns$date  
# ALL monthly returns have rownames "Year-month-01"  (TO CONFIRM!)
rownames(CRSP_returns_monthly) <-  as.Date(paste(gsub("/","",as.character(format(CRSP_returns_monthly$date,"%Y/%m"))), "01",sep=""),format="%Y%m%d")  
CRSP_returns$date <- NULL
CRSP_returns_monthly$date <- NULL
CRSP_returns <- as.matrix(CRSP_returns)   
CRSP_returns_monthly <- as.matrix(CRSP_returns_monthly) 

# Get the CRSP closing prices 
# http://www.crsp.com/products/documentation/crsp-calculations
CRSP_prc = dcast(CRSPdata, date ~ PERMNO, value.var="PRC")  
rownames(CRSP_prc) <- CRSP_prc$date
CRSP_prc$date <- NULL
CRSP_prc <- as.matrix(abs(CRSP_prc)) # use absolute value since prices in CRSP are negative when bid-ask is not completed (see CRSP specs)
CRSP_prc[abs(CRSP_prc) == 99] <- 0 #missing prices in CRSP (see CRSP specs)

CRSP_prc_monthly = dcast(CRSPdata_monthly, date ~ PERMNO, value.var="PRC")  
rownames(CRSP_prc_monthly) <- CRSP_prc_monthly$date
CRSP_prc_monthly$date <- NULL
CRSP_prc_monthly <- as.matrix(abs(CRSP_prc_monthly)) # use absolute value since prices in CRSP are negative when bid-ask is not completed (see CRSP specs)
CRSP_prc_monthly[abs(CRSP_prc_monthly) == 99] <- 0 #missing prices in CRSP (see CRSP specs)

# Now get the number of shares, monthly only
CRSP_shrout_monthly = dcast(CRSPdata_monthly, date ~ PERMNO, value.var="SHROUT")  
rownames(CRSP_shrout_monthly) <- CRSP_shrout_monthly$date
CRSP_shrout_monthly$date <- NULL
CRSP_shrout_monthly <- as.matrix(abs(CRSP_shrout_monthly)) # use absolute value since prices in CRSP are negative when bid-ask is not completed (see CRSP specs)

# clean up a bit
CRSP_returns[is.na(CRSP_returns)] <- 0
CRSP_returns_monthly[is.na(CRSP_returns_monthly)] <- 0
CRSP_prc[is.na(CRSP_prc)] <- 0
CRSP_prc_monthly[is.na(CRSP_prc_monthly)] <- 0
CRSP_shrout_monthly[is.na(CRSP_shrout_monthly)] <- 0

# NOW MAKE SURE WE GET THE INTERSECTION - see what is droping
useonly = intersect(colnames(CRSP_returns_monthly), colnames(CRSP_returns))
drop_daily_monthly = length(setdiff(union(colnames(CRSP_returns_monthly), colnames(CRSP_returns)), useonly))
if (drop_daily_monthly !=0)
  stop("\nCRSP daily and monthly data have different columns")
#just use unique cases - we shoud not have duplicate permnos!
#if (sum(duplicated(useonly)) !=0)
#  cat("removing ", sum(duplicated[useonly]), " duplicated CRSP permnos - they should have been unique!")
#useonly = useonly[!duplicated(useonly)]
CRSP_returns <- CRSP_returns[,useonly]
CRSP_returns_monthly <- CRSP_returns_monthly[,useonly]
CRSP_prc <- CRSP_prc[,useonly]
CRSP_prc_monthly <- CRSP_prc_monthly[,useonly]
CRSP_shrout_monthly <- CRSP_shrout_monthly[,useonly]

# Finally check the alignment of the monthly returns columns with those of the daily ones
if (sum(colnames(CRSP_returns_monthly)!=colnames(CRSP_returns)))
  stop("Monthly and Daily returns are not aligned.")

rm("CRSPdata","CRSPdata_monthly")

######################################################################
## Step 2: now couple CRSP returns and the SDC data
## the SDC$permno are already in the SDC data (from get_compustat_data.R)
######################################################################

# now do the merging of the CRSP with th SDC+Compustat data
returns_by_event = matrix(0,nrow(CRSP_returns),length(SDC$CUSIP)) 
rownames(returns_by_event) <- rownames(CRSP_returns)
returns_by_event_monthly = matrix(0,nrow(CRSP_returns_monthly),length(SDC$CUSIP)) 
rownames(returns_by_event_monthly) <- rownames(CRSP_returns_monthly)

# we need only the months for the Market Cap
crsp_shrout_yearmonths = as.numeric(format(as.Date(rownames(CRSP_shrout_monthly)),"%Y%m"))
SDC_years         = as.numeric(format(SDC$Event.Date,"%Y"))
SDC_months         = as.numeric(format(SDC$Event.Date,"%m"))

CRSP_dates = as.Date(rownames(CRSP_prc))

dropi = rep(F, length(SDC$CUSIP))
duplicatesi = rep(F, length(SDC$CUSIP))
notfoundi = rep(F, length(SDC$CUSIP))

SDC_previous_yearmonths = as.numeric(format(AddMonths(SDC$Event.Date,-1),"%Y%m"))
year_we_care = SDC_years - 1*(SDC_months > 6) - 2*(SDC_months <= 6) 
year_we_care = as.numeric(paste(as.character(year_we_care), "12", sep=""))

for(i in 1:length(SDC$CUSIP)) { 
  CRSP_idx = which(colnames(CRSP_returns) == SDC$permno[i])
  if(length(CRSP_idx) == 0)   notfoundi[i] = T #crsp didn't have this permno
  if(length(CRSP_idx) > 1)  duplicatesi[i] = T #crsp had this twice  
  if(length(CRSP_idx) == 1) { 
    #update (adjusted) closing price based on the average of the past 20 non-zero days
    previous <- CRSP_prc[CRSP_dates < SDC$Event.Date[i],CRSP_idx]    
    if(sum(previous!=0) >= 20) {      
      SDC$Closing.Price[i] = mean(tail(previous[previous!=0],20))      
      returns_by_event_monthly[,i] <- scrub(CRSP_returns_monthly[,CRSP_idx])
      returns_by_event[,i] <- scrub(CRSP_returns[,CRSP_idx])
      
      # Get the ME, and the ME for the BE/ME calculation - - see README and get_compustat_data.R
      # Get first the Market.Cap we need for the ME Break points. it should be from last month
      last_month =  (crsp_shrout_yearmonths == SDC_previous_yearmonths[i])
      if (sum(last_month) != 0)
        SDC$Market.Cap[i] = CRSP_shrout_monthly[last_month,CRSP_idx]*CRSP_prc_monthly[last_month,CRSP_idx]/1000 # it is in MILLIONS
      
      # Now get the ME_usedfor_BEME 
      December_year_we_care = (crsp_shrout_yearmonths == year_we_care[i])
      if (sum(December_year_we_care) != 0)
        SDC$ME_usedfor_BEME[i] = CRSP_shrout_monthly[December_year_we_care,CRSP_idx]*CRSP_prc_monthly[December_year_we_care,CRSP_idx]/1000  # it is in MILLIONS      
    } else {
      dropi[i] = T
    }
  }
}

cat(paste("\n\n**Coupling CRSP with SDC shows that",sum(dropi),"records that we looked up did not have all necessary results in CRSP,\n", 
          sum(duplicatesi),"were multiple times and",sum(notfoundi),"were not found."))
#backup important variables that are affected by subsetting
cleanup$merge$CRSP_NOMATCH   <- sum(dropi) + sum(notfoundi)
cleanup$merge$CRSP_DUPLICATE <- sum(duplicatesi)  
cleanup$merge$CRSP_remove   <- sum(dropi | notfoundi | duplicatesi)

for(field in ls(compustat_data))  compustat_data[[field]] <- compustat_data[[field]][!dropi & !duplicatesi & !notfoundi]
SDC = SDC[!dropi & !duplicatesi & !notfoundi,]
returns_by_event = returns_by_event[,!dropi & !duplicatesi & !notfoundi]  
returns_by_event_monthly = returns_by_event_monthly[,!dropi & !duplicatesi & !notfoundi]
colnames(returns_by_event) <- SDC$permno
colnames(returns_by_event_monthly) <- SDC$permno

cat(paste("\nAfter dropping those, we're left with",dim(SDC)[1],"records.\n\n"))

##################################################################
# Now combine the the CRSP with the Compustat data to get all BE/ME data, 
# the ME data, and the FF five-quantiles  
##################################################################

#### FIRST WE FIX THE NEGATIVE BE CASES, FOLLOWING THE STEPS IN IVO WELCH'S WEBSITE!!

# first set to NA any negative debt cases - they are supposed to be positive
NEGATIVE_BE_FIXED = sum(scrub(SDC$BE_used)< 0)
# then deal with negative BEs in this order (see README)
# First correction
NETATIVE_BE_CASE1 = sum(scrub(SDC$BE_used) < 0 & (scrub(SDC$dlc + SDC$dltt) < 0))
if (sum(scrub(SDC$BE_used) < 0) != 0)
  SDC$BE_used[scrub(SDC$BE_used) < 0 & (scrub(SDC$dlc + SDC$dltt) < 0)] <- NA
# Remaining cases
NEGATIVE_BE_CASE2 = sum(scrub(SDC$BE_used) < 0)
if (sum(scrub(SDC$BE_used) < 0) != 0)
  SDC$BE_used[scrub(SDC$BE_used) < 0] <- pmax(0,
                                              (1/100)*scrub(SDC$dlc + SDC$dltt)[scrub(SDC$BE_used) < 0],
                                              (0.1/100)*scrub(SDC$at[scrub(SDC$BE_used) < 0]))

SDC$BE.ME = SDC$BE_used/SDC$ME_usedfor_BEME
cleanup$merge$NEGATIVE_BE_FIXED <- NEGATIVE_BE_FIXED
cleanup$merge$NETATIVE_BE_CASE1 <- NETATIVE_BE_CASE1
cleanup$merge$NEGATIVE_BE_CASE2 <- NEGATIVE_BE_CASE2

# Do some cleanup now
NO_BE_ME = is.na(SDC$BE.ME)
NO_BE_ME_Breakpoint = sapply(1:length(SDC$BE.ME.Breakpoints_used), function(i) is.null(SDC$BE.ME.Breakpoints_used[i][[1]]))
NO_ME_Breakpoint = sapply(1:length(SDC$ME.Breakpoints_used), function(i) is.null(SDC$ME.Breakpoints_used[i][[1]]))
NO_MARKET_CAP = is.na(SDC$Market.Cap)
cleanup$merge$NO_BE_ME <- sum(NO_BE_ME)
cleanup$merge$NO_BE_ME_Breakpoint <- sum(NO_BE_ME_Breakpoint)
cleanup$merge$NO_ME_Breakpoint <- sum(NO_ME_Breakpoint)
cleanup$merge$NO_MARKET_CAP <- sum(NO_MARKET_CAP)
to_remove = NO_BE_ME | NO_BE_ME_Breakpoint | NO_ME_Breakpoint | NO_MARKET_CAP
cleanup$merge$compustat_remove <- sum(to_remove)

for(field in ls(compustat_data))  compustat_data[[field]] <- compustat_data[[field]][!to_remove]
SDC = SDC[!to_remove,]
returns_by_event = returns_by_event[,!to_remove]  
returns_by_event_monthly = returns_by_event_monthly[,!to_remove]

# Now get the final data we need (basically we only use later the five-quantiles for BE/ME and ME...)  
for (i in 1:nrow(SDC)){
    BEMEthres = BE.MEbreakpoints[SDC$BE.ME.Breakpoints_used[i][[1]],]
    SDC$BEME_quantile[i] = ifelse(SDC$BE.ME[i] < min(BEMEthres), 1, tail(which(BEMEthres <= SDC$BE.ME[i]),1)) 
    MEthres = MEbreakpoints[SDC$ME.Breakpoints_used[i][[1]],]
    SDC$ME_quantile[i] = ifelse(SDC$Market.Cap[i] < min(MEthres), 1, tail(which(MEthres <= SDC$Market.Cap[i]),1))
}
cat("\n\n\n**We finally have ", sum(!is.na(SDC$BEME_quantile) & !is.na(SDC$ME_quantile)), " events with both the BE/ME and ME five-quantiles - out of the total of ", nrow(SDC), "events\n\n")

######################################################################
## Step 3: finally load IBES data and attach to the records
######################################################################

# a) IBES EPS
ibes        <- read.csv(file_in_IBES)
ibes$CUSIP  <- sapply(ibes$CUSIP,function(x) substr(x,1,8))
ibes$STATPERS<- as.Date(as.character(ibes$STATPERS),format="%Y%m%d")
ibes$FPEDATS <- as.Date(as.character(ibes$FPEDATS),format="%Y%m%d")
ibes        <- ibes[with(ibes,order(STATPERS)),]    #order by date of creation of the 
backfilled  <- which(ibes$STATPERS - ibes$FPEDATS > 0)   #remove any back-filled entries
if(length(backfilled)>0)
  ibes        <- ibes[-backfilled,]
ibes_eps    <- ibes[ibes$MEASURE == "EPS",]
ibes_sls    <- ibes[ibes$MEASURE == "SAL",]
SDC$EPS.Forecast<- 0;SDC$EPS.Forecast.Date<-0;SDC$EPS.Value <- 0;SDC$EPS.Value.Date <- 0; SDC$EPS.Numest <- 0;
SDC$SLS.Forecast<- 0;SDC$SLS.Forecast.Date<-0;SDC$SLS.Value <- 0;SDC$SLS.Value.Date <- 0; SDC$SLS.Numest <- 0;

ibes_data = list()
ibes_data[["eps_forecast"]] = list(); ibes_data[["eps_value"]] = list(); ibes_data[["eps_numest"]] = list()
ibes_data[["sls_forecast"]] = list(); ibes_data[["sls_value"]] = list(); ibes_data[["sls_numest"]] = list()

#match cusips first and then find closest previous quarter in dates and make sure it's within a year
for(i in 1:length(SDC$CUSIP)) {
  #### EPS
  ibes <- ibes_eps
  #Fpedats: Forecast Period End Date: the ending month and year of the fiscal period to which the estimate applies (the end of the fiscal year or quarter).
  #Statpers: I/B/E/S Statistical Period: the date when the set of summary statistics was calculated; the date the 'snapshots' were taken.
  IbesI         = ibes[which(ibes$CUSIP == SDC$CUSIP[i]),]  #this company's IBES data
  DaysFromEvent = SDC$Event.Date[i] - IbesI$FPEDATS         #all positive DaysFromEvents are EPS Announcements (and forecasts) done BEFORE the event
  if(sum(DaysFromEvent > 0) > 1) {
    ThisQuarter = which(DaysFromEvent == min(DaysFromEvent[DaysFromEvent >= IBES_PRE_ANOUNCE_DATES]))    #closest quarter forecast
    if (length(ThisQuarter) >= 1) { 
      LastForecast = ThisQuarter[which.max(IbesI$STATPERS[ThisQuarter])] # now take the last estimate we had for that quarter
      if(DaysFromEvent[LastForecast] < 365) { #within year
        SDC$EPS.Forecast[i]      <- scrub(IbesI$MEANEST[LastForecast])
        SDC$EPS.Value[i]         <- scrub(IbesI$ACTUAL[LastForecast])
        SDC$EPS.Numest[i]        <- scrub(IbesI$NUMEST[LastForecast])
        SDC$EPS.Forecast.Date[i] <- IbesI$FPEDATS[LastForecast]
        SDC$EPS.Value.Date[i]    <- IbesI$STATPERS[LastForecast]
        
        #also save +-4 year window in the special lists
        #IbesW = IbesI[DaysFromEvent >= -4*365 & DaysFromEvent <= 4*365,]
        # WHY ONLY +-4, LET'S KEEP THEM ALL, and we sort it out later on
        IbesW = IbesI
        #group by prediction data and take last estimate each time
        uniq_f_date <- unique(IbesW$FPEDATS)
        window_idx  <- sapply(1:length(uniq_f_date),function(j) {
          Matches   <- which(IbesW$FPEDATS == uniq_f_date[j])
          LastMatch <- Matches[which.max( IbesW$STATPERS[Matches])]
          return(LastMatch)
        })
        ibes_data$eps_forecast[[i]]       <- scrub(IbesW$MEANEST[window_idx])
        ibes_data$eps_value[[i]]          <- scrub(IbesW$ACTUAL[window_idx])
        ibes_data$eps_numest[[i]]         <- scrub(IbesW$NUMEST[window_idx])
        names(ibes_data$eps_forecast[[i]])<- IbesW$STATPERS[window_idx]
        names(ibes_data$eps_value[[i]])   <- IbesW$FPEDATS[window_idx]
        names(ibes_data$eps_numest[[i]])  <- IbesW$FPEDATS[window_idx]
      }
    }
  }
  #### SLS
  ibes <- ibes_sls
  #Fpedats: Forecast Period End Date: the ending month and year of the fiscal period to which the estimate applies (the end of the fiscal year or quarter).
  #Statpers: I/B/E/S Statistical Period: the date when the set of summary statistics was calculated; the date the 'snapshots' were taken.
  IbesI         = ibes[which(ibes$CUSIP == SDC$CUSIP[i]),]  #this company's IBES data
  DaysFromEvent = SDC$Event.Date[i] - IbesI$FPEDATS         #all positive DaysFromEvents are SLS Announcements (and forecasts) done BEFORE the event
  if(sum(DaysFromEvent > 0) > 1) {
    ThisQuarter = which(DaysFromEvent == min(DaysFromEvent[DaysFromEvent >= IBES_PRE_ANOUNCE_DATES]))    #closest quarter forecast
    if (length(ThisQuarter) >= 1) { 
      LastForecast = ThisQuarter[which.max(IbesI$STATPERS[ThisQuarter])] # now take the last estimate we had for that quarter
      if(DaysFromEvent[LastForecast] < 365) { #within year
        SDC$SLS.Forecast[i]      <- scrub(IbesI$MEANEST[LastForecast])
        SDC$SLS.Value[i]         <- scrub(IbesI$ACTUAL[LastForecast])
        SDC$SLS.Numest[i]        <- scrub(IbesI$NUMEST[LastForecast])
        SDC$SLS.Forecast.Date[i] <- IbesI$FPEDATS[LastForecast]
        SDC$SLS.Value.Date[i]    <- IbesI$STATPERS[LastForecast]
        
        #also save +-4 year window in the special lists
        #IbesW = IbesI[DaysFromEvent >= -4*365 & DaysFromEvent <= 4*365,]
        # WHY ONLY +-4, LET'S KEEP THEM ALL, and we sort it out later on
        IbesW = IbesI
        #group by prediction data and take last estimate each time
        uniq_f_date <- unique(IbesW$FPEDATS)
        window_idx  <- sapply(1:length(uniq_f_date),function(j) {
          Matches   <- which(IbesW$FPEDATS == uniq_f_date[j])
          LastMatch <- Matches[which.max( IbesW$STATPERS[Matches])]
          return(LastMatch)
        })
        ibes_data$sls_forecast[[i]]       <- scrub(IbesW$MEANEST[window_idx])
        ibes_data$sls_value[[i]]          <- scrub(IbesW$ACTUAL[window_idx])
        ibes_data$sls_numest[[i]]         <- scrub(IbesW$NUMEST[window_idx])
        names(ibes_data$sls_forecast[[i]])<- IbesW$STATPERS[window_idx]
        names(ibes_data$sls_value[[i]])   <- IbesW$FPEDATS[window_idx]
        names(ibes_data$sls_numest[[i]])  <- IbesW$FPEDATS[window_idx]
      }
    }
  }
  
}
SDC$EPS.Forecast.Date <- as.Date(SDC$EPS.Forecast.Date)
SDC$EPS.Value.Date    <- as.Date(SDC$EPS.Value.Date)
SDC$SLS.Forecast.Date <- as.Date(SDC$SLS.Forecast.Date)
SDC$SLS.Value.Date    <- as.Date(SDC$SLS.Value.Date)

#cat(paste("\nCoupling IBES EPS with SDC shows that",sum( SDC$EPS.Forecast == 0 | SDC$EPS.Value == 0),"/",dim(SDC)[1],"records that we looked up were not in the results from IBES."))
#cat(paste("\nCoupling IBES SLS with SDC shows that",sum( SDC$SLS.Forecast == 0 | SDC$SLS.Value == 0),"/",dim(SDC)[1],"records that we looked up were not in the results from IBES."))
cat(paste("\n\n***Coupling IBES with SDC shows that overall",sum( SDC$EPS.Forecast == 0 | SDC$EPS.Value == 0 | SDC$SLS.Forecast == 0 | SDC$SLS.Value == 0),"/",dim(SDC)[1],"records that we looked up were not in the results from IBES."))

cleanup$merge$NO_IBES <- sum( SDC$SLS.Forecast == 0 | SDC$EPS.Forecast == 0 | SDC$SLS.Value == 0 | SDC$EPS.Value == 0)

#####################################################################
# Add misses & beats of EPS
#########
SDC$EPSmissesbeats = NULL
for (i in 1:length(SDC$permno)){
  eps_forecast = ibes_data$eps_forecast[[i]]
  eps_actual = ibes_data$eps_value[[i]]
  if (length(eps_forecast) == length(eps_actual) & length(eps_actual) > 0 ){ 
    # Just make sure we point to the actual company announcement (since we only keep
    # the last forecast per quarter and we removed any back-filling data e.g. forecasts made after the company announcement)
    names(eps_forecast) <- pmax(names(eps_forecast), names(eps_actual))
    names(eps_actual) <- pmax(names(eps_forecast), names(eps_actual))
    eps_forecast = eps_forecast[names(eps_forecast) > SDC$Event.Date[i]]
    eps_actual = eps_actual[names(eps_actual) > SDC$Event.Date[i]]
    actual_minus_forecast = eps_actual - eps_forecast
    date_diff = as.Date(names(actual_minus_forecast)) - SDC$Event.Date[i]
    actual_minus_forecast = actual_minus_forecast[date_diff < 4*365 & date_diff > 0]
    # they should be already ordered in merge_dataset.R (to confirm)... but also do temporarily here, just in case
    time_order = sort(names(actual_minus_forecast), index.return = TRUE)$ix
    actual_minus_forecast = actual_minus_forecast[time_order]
    SDC$EPSmissesbeats[i] <- list(actual_minus_forecast) # These have missing EPS data! we remove them in this analysis
  } else {
    SDC$EPSmissesbeats[i] = list(NULL) # These have missing EPS data! we remove them in this analysis
  }
}


######################################################################
## now save it all
######################################################################

dataset = list()
dataset$SDC <- SDC
dataset$returns_by_event <- returns_by_event
dataset$returns_by_event_monthly <- returns_by_event_monthly
dataset$compustat_data      <- compustat_data
dataset$ibes             <- ibes_data

save(dataset,cleanup,file=global_output_file)

