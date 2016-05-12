#  Copyright 2015, INSEAD
#  by T. Evgeniou, Enric Junque de Fortuny, Nick Nassuphis, Theo Vermaelen 
#  Dual licensed under the MIT or GPL Version 2 licenses.

if (memory.limit()<40*1024) memory.limit(40*1024) # allow at least 40GB of memory to be allocated (Windows)
##########################################################################################
# Creates the data for the Buybacks-Issuers paper
##########################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

use_major_market_score = 0 # We use all CRSP universe for the scores 

source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceLibraries/latex_code.R")
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")
# Used to get WRDS data through the API
source("../FinanceLibraries/wrds_helpers.R", chdir=TRUE)
source("~/Documents/WRDS_Drivers/startWRDSconnection.R") # This file has the username pasword for WRDS. These lines below. See wrds_config.R in FinanceLibraries
# wrds_user <- "my_username"
# wrds_pass <- "{SAS002}DBCC5712369DE1C65B19864C1564FB850F398DCF"
# wrds_path <- "C:\\Users\\my_user\\Documents\\WRDS_Drivers\\"
wrds_handle <- wrdsConnect()

load("../FinanceData/created_monthly_data/GLOBAL_MONTHLY_DATABASE.Rdata")
load("../FinanceData/created_yearly_data/GLOBAL_YEARLY_DATABASE.Rdata")
load("../FinanceData/created_ibes_data/GLOBAL_IBES_DATABASE.Rdata")
load("../FinanceData/created_buyback_data/GLOBAL_BUYBACK.Rdata")
load("../FinanceData/created_issuers_data/GLOBAL_ISSUERS.Rdata")

if (1){
  load("../FinanceData/created_daily_data/GLOBAL_DAILY_DATABASE.Rdata")
  # We don't need these, so for memory purpose we remove 
  GLOBAL_DAILY_DATABASE$volume_daily <- NULL
  GLOBAL_DAILY_DATABASE$recent_volatility_daily <- NULL
  GLOBAL_DAILY_DATABASE$FamaFrench_five_factors <- NULL
} else{ 
  GLOBAL_DAILY_DATABASE = list()
  GLOBAL_DAILY_DATABASE$returns_daily <- wrdsQueryStockFieldMatrix(wrds_handle, colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly), "RET",start=as.Date("1980-01-01"))
}

###################################################################################################
# Some extra universe data variables we need for this project (based on the raw data)
month_dates = str_sub(rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly), start = 1, end = 7) 
GLOBAL_MONTHLY_DATABASE$market_cap = GLOBAL_MONTHLY_DATABASE$prices_monthly*GLOBAL_MONTHLY_DATABASE$sharesout_monthly/1000 # In millions
# BE/ME using just this month data:
GLOBAL_MONTHLY_DATABASE$BE.ME = ifelse(GLOBAL_MONTHLY_DATABASE$market_cap, GLOBAL_YEARLY_DATABASE$book_value/GLOBAL_MONTHLY_DATABASE$market_cap, NA)
rownames(GLOBAL_MONTHLY_DATABASE$BE.ME) <- rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
colnames(GLOBAL_MONTHLY_DATABASE$BE.ME) <- colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
#  BE/ME the way FF do it 
GLOBAL_MONTHLY_DATABASE$market_cap_last_december = apply(GLOBAL_MONTHLY_DATABASE$market_cap,2,function(r){
  names(r) <- month_dates
  use_dates = paste(as.numeric(str_sub(month_dates, start = 1, end = 4))-1, "12", sep="-")
  ifelse(use_dates %in% names(r), r[use_dates], NA)
})
rownames(GLOBAL_MONTHLY_DATABASE$market_cap_last_december) <- rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
colnames(GLOBAL_MONTHLY_DATABASE$market_cap_last_december) <- colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
GLOBAL_MONTHLY_DATABASE$Book_Value_last_june = apply(GLOBAL_YEARLY_DATABASE$book_value,2,function(r){
  names(r) <- month_dates
  use_dates = ifelse(as.numeric(str_sub(names(r), start = 6, end = 7)) > 6, paste(str_sub(names(r), start = 1, end = 4), "06", sep="-"), paste(as.numeric(str_sub(names(r), start = 1, end = 4))-1, "06", sep="-"))
  ifelse(use_dates %in% names(r), r[use_dates], NA)
})
rownames(GLOBAL_MONTHLY_DATABASE$Book_Value_last_june) <- rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
colnames(GLOBAL_MONTHLY_DATABASE$Book_Value_last_june) <- colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
GLOBAL_MONTHLY_DATABASE$BE.ME_ff = ifelse(GLOBAL_MONTHLY_DATABASE$market_cap_last_december, GLOBAL_MONTHLY_DATABASE$Book_Value_last_june/GLOBAL_MONTHLY_DATABASE$market_cap_last_december, NA)
rownames(GLOBAL_MONTHLY_DATABASE$BE.ME_ff) <- rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
colnames(GLOBAL_MONTHLY_DATABASE$BE.ME_ff) <- colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)

## Ibes data - note that these are not available the whole period, so we align with the mothly data, too, first
tmp = GLOBAL_MONTHLY_DATABASE$returns_monthly*NA
match_rows = match(str_sub(rownames(GLOBAL_IBES_DATABASE$MEANREC), start=1,end=7), str_sub(rownames(tmp), start=1,end=7))
match_columns = match(colnames(GLOBAL_IBES_DATABASE$MEANREC), colnames(tmp))
tmp[match_rows,match_columns] <- GLOBAL_IBES_DATABASE$MEANREC 
GLOBAL_IBES_DATABASE$MEANREC<- tmp

tmp = GLOBAL_MONTHLY_DATABASE$returns_monthly*NA
match_rows = match(str_sub(rownames(GLOBAL_IBES_DATABASE$NUMREC), start=1,end=7), str_sub(rownames(tmp), start=1,end=7))
match_columns = match(colnames(GLOBAL_IBES_DATABASE$NUMREC), colnames(tmp))
tmp[match_rows,match_columns] <- GLOBAL_IBES_DATABASE$NUMREC 
GLOBAL_IBES_DATABASE$NUMREC<- tmp

tmp = GLOBAL_MONTHLY_DATABASE$returns_monthly*NA
match_rows = match(str_sub(rownames(GLOBAL_IBES_DATABASE$STDEV), start=1,end=7), str_sub(rownames(tmp), start=1,end=7))
match_columns = match(colnames(GLOBAL_IBES_DATABASE$STDEV), colnames(tmp))
tmp[match_rows,match_columns] <- GLOBAL_IBES_DATABASE$STDEV 
GLOBAL_IBES_DATABASE$STDEV<- tmp

tmp = GLOBAL_MONTHLY_DATABASE$returns_monthly*NA
match_rows = match(str_sub(rownames(GLOBAL_IBES_DATABASE$NUMUP), start=1,end=7), str_sub(rownames(tmp), start=1,end=7))
match_columns = match(colnames(GLOBAL_IBES_DATABASE$NUMUP), colnames(tmp))
tmp[match_rows,match_columns] <- GLOBAL_IBES_DATABASE$NUMUP 
GLOBAL_IBES_DATABASE$NUMUP<- tmp

tmp = GLOBAL_MONTHLY_DATABASE$returns_monthly*NA
match_rows = match(str_sub(rownames(GLOBAL_IBES_DATABASE$NUMDOWN), start=1,end=7), str_sub(rownames(tmp), start=1,end=7))
match_columns = match(colnames(GLOBAL_IBES_DATABASE$NUMDOWN), colnames(tmp))
tmp[match_rows,match_columns] <- GLOBAL_IBES_DATABASE$NUMDOWN 
GLOBAL_IBES_DATABASE$NUMDOWN<- tmp

## This is slow but we need it for the pre-event performance. 
GLOBAL_DAILY_DATABASE$returns_daily <- scrub(GLOBAL_DAILY_DATABASE$returns_daily)
tmp1 <- apply(GLOBAL_DAILY_DATABASE$returns_daily,2,function(r) ms(r,120)) # we only have trading days
tmp2 <- apply(GLOBAL_DAILY_DATABASE$returns_daily!=0,2,function(r) ms(r,120)) # we only have trading days
GLOBAL_DAILY_DATABASE$recent_returns_daily = ifelse(tmp2 > 60, tmp1, NA) # need at least 60 trading (non-zero returns!) days recently
rownames(GLOBAL_DAILY_DATABASE$recent_returns_daily) <- rownames(GLOBAL_DAILY_DATABASE$returns_daily)
colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily) <- colnames(GLOBAL_DAILY_DATABASE$returns_daily)
rm("tmp1", "tmp2")

###################################################################################################
# We only use permnos that traded at least once in only these exchanges: NYSE (11), American Stock Exchange (12), Nasdaq (14)
# So we update all data accordingly: For now we do so only for **scoring** (percentile) purposes, as done by FF for the breakpoints
###################################################################################################

good_exchange_traded = apply(GLOBAL_YEARLY_DATABASE$exchange,2, function(r) sum(scrub(r) %in% c(11,12,14)))
good_exchange_traded = which(good_exchange_traded !=0)

for(field in setdiff(ls(GLOBAL_DAILY_DATABASE), "FamaFrench_five_factors")) 
  if (!is.null(dim(GLOBAL_DAILY_DATABASE[[field]])))
    GLOBAL_DAILY_DATABASE[[paste(field,"majormarket",sep="_")]]<- GLOBAL_DAILY_DATABASE[[field]][,good_exchange_traded]
for(field in setdiff(ls(GLOBAL_MONTHLY_DATABASE), "FamaFrench_five_factors")) 
  if (!is.null(dim(GLOBAL_MONTHLY_DATABASE[[field]])))
    GLOBAL_MONTHLY_DATABASE[[paste(field,"majormarket",sep="_")]]<- GLOBAL_MONTHLY_DATABASE[[field]][,good_exchange_traded]
for(field in ls(GLOBAL_YEARLY_DATABASE)) 
  if (!is.null(dim(GLOBAL_YEARLY_DATABASE[[field]])))
    GLOBAL_YEARLY_DATABASE[[paste(field,"majormarket",sep="_")]]<- GLOBAL_YEARLY_DATABASE[[field]][,good_exchange_traded]
# These are to treated differently
for(field in ls(GLOBAL_IBES_DATABASE)) 
  if (!is.null(dim(GLOBAL_IBES_DATABASE[[field]])))
    GLOBAL_IBES_DATABASE[[paste(field,"majormarket",sep="_")]]<- GLOBAL_IBES_DATABASE[[field]][,colnames(GLOBAL_IBES_DATABASE[[field]]) %in% colnames(GLOBAL_YEARLY_DATABASE$exchange_majormarket)]

# Fill with latest available data - as we will also check later the exchange based on the CRSP/Compustat data 
GLOBAL_YEARLY_DATABASE$exchange <- apply(GLOBAL_YEARLY_DATABASE$exchange,2,function(r) fill_NA_previous(r))

#### ALL SCORES NOW
# Note: WE GET ALL SCORES FOR EACH PROJECT FOR TWO REASONS: a) TO AVOID SAVING ALL THIS, b) AS WE MAY WANT TO GET SCORE USING ONLY A SUBSET OF THE COMPANIES
# In this case we removed all stocks not trading in major markets already, so we use all permnos for scoring. Indeed, check this now:
if (use_major_market_score){
  GLOBAL_MONTHLY_DATABASE$volatility_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$volatility,GLOBAL_MONTHLY_DATABASE$volatility_majormarket)
  GLOBAL_MONTHLY_DATABASE$alphas_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$alphas,GLOBAL_MONTHLY_DATABASE$alphas_majormarket)
  GLOBAL_MONTHLY_DATABASE$market_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_beta,GLOBAL_MONTHLY_DATABASE$market_beta_majormarket)
  GLOBAL_MONTHLY_DATABASE$SMB_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$SMB_beta,GLOBAL_MONTHLY_DATABASE$SMB_beta_majormarket)
  GLOBAL_MONTHLY_DATABASE$HML_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$HML_beta,GLOBAL_MONTHLY_DATABASE$HML_beta_majormarket)
  GLOBAL_MONTHLY_DATABASE$RMW_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$RMW_beta,GLOBAL_MONTHLY_DATABASE$RMW_beta_majormarket)
  GLOBAL_MONTHLY_DATABASE$CMA_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$CMA_beta,GLOBAL_MONTHLY_DATABASE$CMA_beta_majormarket)
  GLOBAL_MONTHLY_DATABASE$IVOL_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$IVOL,GLOBAL_MONTHLY_DATABASE$IVOL_majormarket)
  GLOBAL_MONTHLY_DATABASE$Rsq_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$Rsq,GLOBAL_MONTHLY_DATABASE$Rsq_majormarket)
  GLOBAL_MONTHLY_DATABASE$market_cap_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_cap,GLOBAL_MONTHLY_DATABASE$market_cap_majormarket)
  GLOBAL_MONTHLY_DATABASE$BE.ME_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$BE.ME,GLOBAL_MONTHLY_DATABASE$BE.ME_majormarket)
  GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$BE.ME_ff,GLOBAL_MONTHLY_DATABASE$BE.ME_ff_majormarket)
  GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score <- get_cross_section_score(GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple,GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_majormarket)
  
  GLOBAL_MONTHLY_DATABASE$volatility_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$volatility_month,GLOBAL_MONTHLY_DATABASE$volatility_month_majormarket)
  GLOBAL_MONTHLY_DATABASE$alphas__month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$alphas_month,GLOBAL_MONTHLY_DATABASE$alphas_month_majormarket)
  GLOBAL_MONTHLY_DATABASE$market_beta_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_beta_month,GLOBAL_MONTHLY_DATABASE$market_beta_month_majormarket)
  GLOBAL_MONTHLY_DATABASE$IVOL_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$IVOL_month,GLOBAL_MONTHLY_DATABASE$IVOL_month_majormarket)
  GLOBAL_MONTHLY_DATABASE$Rsq_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$Rsq_month,GLOBAL_MONTHLY_DATABASE$Rsq_month_majormarket)
  
  GLOBAL_IBES_DATABASE$MEANREC_score = get_cross_section_score(GLOBAL_IBES_DATABASE$MEANREC,GLOBAL_IBES_DATABASE$MEANREC_majormarket)
  GLOBAL_IBES_DATABASE$NUMREC_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMREC,GLOBAL_IBES_DATABASE$NUMREC_majormarket)
  GLOBAL_IBES_DATABASE$STDEV_score = get_cross_section_score(GLOBAL_IBES_DATABASE$STDEV,GLOBAL_IBES_DATABASE$STDEV_majormarket)
  GLOBAL_IBES_DATABASE$NUMUP_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMUP,GLOBAL_IBES_DATABASE$NUMUP_majormarket)
  GLOBAL_IBES_DATABASE$NUMDOWN_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMDOWN,GLOBAL_IBES_DATABASE$NUMDOWN_majormarket)
  
  GLOBAL_DAILY_DATABASE$recent_returns_daily_score = get_cross_section_score(GLOBAL_DAILY_DATABASE$recent_returns_daily,GLOBAL_DAILY_DATABASE$recent_returns_daily_majormarket)
} else {
  GLOBAL_MONTHLY_DATABASE$volatility_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$volatility)
  GLOBAL_MONTHLY_DATABASE$alphas_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$alphas)
  GLOBAL_MONTHLY_DATABASE$market_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_beta)
  GLOBAL_MONTHLY_DATABASE$SMB_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$SMB_beta)
  GLOBAL_MONTHLY_DATABASE$HML_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$HML_beta)
  GLOBAL_MONTHLY_DATABASE$RMW_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$RMW_beta)
  GLOBAL_MONTHLY_DATABASE$CMA_beta_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$CMA_beta)
  GLOBAL_MONTHLY_DATABASE$IVOL_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$IVOL)
  GLOBAL_MONTHLY_DATABASE$Rsq_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$Rsq)
  GLOBAL_MONTHLY_DATABASE$market_cap_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_cap)
  GLOBAL_MONTHLY_DATABASE$BE.ME_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$BE.ME)
  GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$BE.ME_ff)
  GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score <- get_cross_section_score(GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple)
  
  GLOBAL_MONTHLY_DATABASE$volatility_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$volatility_month)
  GLOBAL_MONTHLY_DATABASE$alphas__month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$alphas_month)
  GLOBAL_MONTHLY_DATABASE$market_beta_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_beta_month)
  GLOBAL_MONTHLY_DATABASE$IVOL_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$IVOL_month)
  GLOBAL_MONTHLY_DATABASE$Rsq_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$Rsq_month)
  
  GLOBAL_IBES_DATABASE$MEANREC_score = get_cross_section_score(GLOBAL_IBES_DATABASE$MEANREC)
  GLOBAL_IBES_DATABASE$NUMREC_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMREC)
  GLOBAL_IBES_DATABASE$STDEV_score = get_cross_section_score(GLOBAL_IBES_DATABASE$STDEV)
  GLOBAL_IBES_DATABASE$NUMUP_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMUP)
  GLOBAL_IBES_DATABASE$NUMDOWN_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMDOWN)
  
  GLOBAL_DAILY_DATABASE$recent_returns_daily_score = get_cross_section_score(GLOBAL_DAILY_DATABASE$recent_returns_daily)
  
}

############################################################################################
############################################################################################
# Gather all data we need, aligned correctly pre-event
############################################################################################
############################################################################################

# First way to get the permnos
market_trading_days = as.Date(names(GLOBAL_DAILY_DATABASE$Market_Daily))

# Get closest trading day before the event
trading_day_event = names(GLOBAL_DAILY_DATABASE$Market_Daily)[match(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date), names(GLOBAL_DAILY_DATABASE$Market_Daily))]
if (sum(is.na(trading_day_event)))
  trading_day_event[which(is.na(trading_day_event))] <- names(GLOBAL_DAILY_DATABASE$Market_Daily)[sapply(BUYBACK_DATA$DATASET$SDC$Event.Date[which(is.na(trading_day_event))], function(i){
    x = as.numeric(i - market_trading_days)
    useonly = which(x >=0)
    useonly[which(abs(x[useonly]) == min(abs(x[useonly])))]
  })]
message(paste("retrieving PERMNOs for SDC deals (est.", round(length(BUYBACK_DATA$DATASET$SDC$CUSIP)*0.0004), "minutes)"))
permno1_bb <- wrdsGetPERMNOForCUSIP(wrds_handle, substr(BUYBACK_DATA$DATASET$SDC$CUSIP, 1, 6), as.Date(trading_day_event))
###
# Get closest trading day before the event
trading_day_event = names(GLOBAL_DAILY_DATABASE$Market_Daily)[match(as.character(ISSUERS_DATA$DATASET$SDC$Event.Date), names(GLOBAL_DAILY_DATABASE$Market_Daily))]
if (sum(is.na(trading_day_event)))
  trading_day_event[which(is.na(trading_day_event))] <- names(GLOBAL_DAILY_DATABASE$Market_Daily)[sapply(ISSUERS_DATA$DATASET$SDC$Event.Date[which(is.na(trading_day_event))], function(i){
    x = as.numeric(i - market_trading_days)
    useonly = which(x >=0)
    useonly[which(abs(x[useonly]) == min(abs(x[useonly])))]
  })]
message(paste("retrieving PERMNOs for SDC deals (est.", round(length(ISSUERS_DATA$DATASET$SDC$CUSIP)*0.0004), "minutes)"))
permno1_iss <- wrdsGetPERMNOForCUSIP(wrds_handle, substr(ISSUERS_DATA$DATASET$SDC$CUSIP, 1, 6), as.Date(trading_day_event))

# Second way to get the permnos, using the cusip-date. 
load("../FinanceData/rawdata_universe_of_companies/crsp_permno_cusip_pairs.Rdata")
cusip_date_universe = paste(universe_companies$NCUSIP, universe_companies$date, sep="-")
cusip_date = paste(BUYBACK_DATA$DATASET$SDC$CUSIP, format(BUYBACK_DATA$DATASET$SDC$Event.Date, "%Y%m%d"), sep="-")
the_matches = match(cusip_date,cusip_date_universe)
permno2_bb <- ifelse(is.na(the_matches), NA, universe_companies$PERMNO[the_matches])
###
cusip_date = paste(ISSUERS_DATA$DATASET$SDC$CUSIP, format(ISSUERS_DATA$DATASET$SDC$Event.Date, "%Y%m%d"), sep="-")
the_matches = match(cusip_date,cusip_date_universe)
permno2_iss <- ifelse(is.na(the_matches), NA, universe_companies$PERMNO[the_matches])

### Check them
cat("permno agreements BB:", sum(permno2_bb[!is.na(permno2_bb) & !is.na(permno1_bb)] != permno1_bb[!is.na(permno2_bb) & !is.na(permno1_bb)]),
    sum(is.na(permno1_bb)), sum(is.na(permno2_bb)))
cat("... permno agreements ISS:", sum(permno2_iss[!is.na(permno2_iss) & !is.na(permno1_iss)] != permno1_iss[!is.na(permno2_iss) & !is.na(permno1_iss)]),
    sum(is.na(permno1_iss)), sum(is.na(permno2_iss)))

BUYBACK_DATA$DATASET$SDC$permno <- permno1_bb
ISSUERS_DATA$DATASET$SDC$permno <- permno1_iss

BUYBACK_DATA$DATASET$SDC$permnoV2 <- permno2_bb
ISSUERS_DATA$DATASET$SDC$permnoV2 <- permno2_iss

###############################################################
### keep only permnos available in the global daily/monthly/yearly data
BUYBACK_DATA$cleanupNoPermno <- sum(is.na(BUYBACK_DATA$DATASET$SDC$permno))
useonly = which(!is.na(BUYBACK_DATA$DATASET$SDC$permno))
BUYBACK_DATA$DATASET$SDC = BUYBACK_DATA$DATASET$SDC[useonly,]

ISSUERS_DATA$cleanupNoPermno <- sum(is.na(ISSUERS_DATA$DATASET$SDC$permno))
useonly = which(!is.na(ISSUERS_DATA$DATASET$SDC$permno))
ISSUERS_DATA$DATASET$SDC = ISSUERS_DATA$DATASET$SDC[useonly,]

rm("cusip_date_universe","universe_companies")

# Drop all events for which we have missing compustat/CRSP data
useonly = which(as.character(BUYBACK_DATA$DATASET$SDC$permno) %in% colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly))
BUYBACK_DATA$cleanupNoCRSPdata <- length(BUYBACK_DATA$DATASET$SDC$permno) - length(useonly)
BUYBACK_DATA$DATASET$SDC = BUYBACK_DATA$DATASET$SDC[useonly,]
useonly = which(as.character(ISSUERS_DATA$DATASET$SDC$permno) %in% colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly))
ISSUERS_DATA$cleanupNoCRSPdata <- length(ISSUERS_DATA$DATASET$SDC$permno) - length(useonly)
ISSUERS_DATA$DATASET$SDC = ISSUERS_DATA$DATASET$SDC[useonly,]
rm("useonly")

###############################################################
### Monthly returns for the permnos of the events
BUYBACK_DATA$DATASET$returns_by_event_monthly <- GLOBAL_MONTHLY_DATABASE$returns_monthly[,as.character(BUYBACK_DATA$DATASET$SDC$permno)]
ISSUERS_DATA$DATASET$returns_by_event_monthly <- GLOBAL_MONTHLY_DATABASE$returns_monthly[,as.character(ISSUERS_DATA$DATASET$SDC$permno)]

###############################################################
### Industries
BUYBACK_DATA$DATASET$SDC$Industry <- suppressWarnings(scrub(as.numeric(BUYBACK_DATA$DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
BUYBACK_DATA$DATASET$SDC$Industry_name<- sapply(BUYBACK_DATA$DATASET$SDC$Industry, function(i){
  x=as.numeric(i)
  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
})

ISSUERS_DATA$DATASET$SDC$Industry <- suppressWarnings(scrub(as.numeric(ISSUERS_DATA$DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
ISSUERS_DATA$DATASET$SDC$Industry_name<- sapply(ISSUERS_DATA$DATASET$SDC$Industry, function(i){
  x=as.numeric(i)
  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
})

###############################################################
### SIC/SICH
year_dates_yearly = str_sub(GLOBAL_YEARLY_DATABASE$date, start = 1, end = 4) 

event.date_all = str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start=1,end=4)
event.permno_all = event.permno = BUYBACK_DATA$DATASET$SDC$permno
the_matches = lapply(1:length(event.date_all), function(i) which(GLOBAL_YEARLY_DATABASE$permnos == event.permno_all[i] & year_dates_yearly == event.date_all[i]))

BUYBACK_DATA$DATASET$CRSP$SIC <- sapply(1:length(event.date_all), function(i){
  tmp = unique(GLOBAL_YEARLY_DATABASE$sic[the_matches[[i]]])
  ifelse(length(tmp) == 1,tmp ,NA)
})
BUYBACK_DATA$DATASET$CRSP$SICH <- sapply(1:length(BUYBACK_DATA$DATASET$SDC$CUSIP), function(i){
  tmp = unique(GLOBAL_YEARLY_DATABASE$sich[the_matches[[i]]])
  ifelse(length(tmp) == 1,tmp ,NA)
})

event.date_all = str_sub(ISSUERS_DATA$DATASET$SDC$Event.Date, start=1,end=4)
event.permno_all = event.permno = ISSUERS_DATA$DATASET$SDC$permno
the_matches = lapply(1:length(event.date_all), function(i) which(GLOBAL_YEARLY_DATABASE$permnos == event.permno_all[i] & year_dates_yearly == event.date_all[i]))

ISSUERS_DATA$DATASET$CRSP$SIC <- sapply(1:length(ISSUERS_DATA$DATASET$SDC$CUSIP), function(i){
  tmp = unique(GLOBAL_YEARLY_DATABASE$sic[the_matches[[i]]])
  ifelse(length(tmp) == 1,tmp ,NA)
})
ISSUERS_DATA$DATASET$CRSP$SICH <- sapply(1:length(ISSUERS_DATA$DATASET$SDC$CUSIP), function(i){
  tmp = unique(GLOBAL_YEARLY_DATABASE$sich[the_matches[[i]]])
  ifelse(length(tmp) == 1,tmp ,NA)
})

rm("year_dates_yearly","event.date_all","event.permno_all","the_matches")

###############################################################
### GVKEYS

trading_day_event = names(GLOBAL_DAILY_DATABASE$Market_Daily)[match(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date), names(GLOBAL_DAILY_DATABASE$Market_Daily))]
if (sum(is.na(trading_day_event)))
  trading_day_event[which(is.na(trading_day_event))] <- names(GLOBAL_DAILY_DATABASE$Market_Daily)[sapply(BUYBACK_DATA$DATASET$SDC$Event.Date[which(is.na(trading_day_event))], function(i){
    x = as.numeric(i - market_trading_days)
    useonly = which(x >=0)
    useonly[which(abs(x[useonly]) == min(abs(x[useonly])))]
  })]
message(paste("retrieving GVKEYs for SDC deals (est.", round(length(BUYBACK_DATA$DATASET$SDC$permno)*0.0002), "minutes)"))
gvkey1_bb <- wrdsGetGVKEYForPERMNO(wrds_handle, BUYBACK_DATA$DATASET$SDC$permno, as.Date(trading_day_event))
gvkey2_bb <- sapply(1:length(BUYBACK_DATA$DATASET$SDC$CUSIP), function(i){
  event.date = str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date[i], start=1,end=4)
  event.permno = BUYBACK_DATA$DATASET$SDC$permno[i]
  tmp = unique(GLOBAL_YEARLY_DATABASE$gvkeys[which(GLOBAL_YEARLY_DATABASE$permnos == event.permno)])
  ifelse(length(tmp) == 1,tmp ,NA)
})

trading_day_event = names(GLOBAL_DAILY_DATABASE$Market_Daily)[match(as.character(ISSUERS_DATA$DATASET$SDC$Event.Date), names(GLOBAL_DAILY_DATABASE$Market_Daily))]
if (sum(is.na(trading_day_event)))
  trading_day_event[which(is.na(trading_day_event))] <- names(GLOBAL_DAILY_DATABASE$Market_Daily)[sapply(ISSUERS_DATA$DATASET$SDC$Event.Date[which(is.na(trading_day_event))], function(i){
    x = as.numeric(i - market_trading_days)
    useonly = which(x >=0)
    useonly[which(abs(x[useonly]) == min(abs(x[useonly])))]
  })]
message(paste("retrieving GVKEYs for SDC deals (est.", round(length(ISSUERS_DATA$DATASET$SDC$permno)*0.0002), "minutes)"))
gvkey1_iss <- wrdsGetGVKEYForPERMNO(wrds_handle, ISSUERS_DATA$DATASET$SDC$permno, as.Date(trading_day_event))
gvkey2_iss <- sapply(1:length(ISSUERS_DATA$DATASET$SDC$CUSIP), function(i){
  event.date = str_sub(ISSUERS_DATA$DATASET$SDC$Event.Date[i], start=1,end=4)
  event.permno = ISSUERS_DATA$DATASET$SDC$permno[i]
  tmp = unique(GLOBAL_YEARLY_DATABASE$gvkeys[which(GLOBAL_YEARLY_DATABASE$permnos == event.permno)])
  ifelse(length(tmp) == 1,tmp ,NA)
})

cat("GVKEY agreements BB: ", sum(as.numeric(gvkey1_bb[!is.na(gvkey1_bb) & !is.na(gvkey2_bb)]) != gvkey2_bb[!is.na(gvkey1_bb) & !is.na(gvkey2_bb)]), sum(is.na(gvkey1_bb)), sum(is.na(gvkey2_bb))) 
cat("... GVKEY agreements ISS: ", sum(as.numeric(gvkey1_iss[!is.na(gvkey1_iss) & !is.na(gvkey2_iss)]) != gvkey2_iss[!is.na(gvkey1_iss) & !is.na(gvkey2_iss)]), sum(is.na(gvkey1_iss)), sum(is.na(gvkey2_iss))) 

BUYBACK_DATA$DATASET$SDC$GVKEY <- gvkey1_bb
ISSUERS_DATA$DATASET$SDC$GVKEY <- gvkey1_iss

BUYBACK_DATA$DATASET$SDC$GVKEY_V2 <- gvkey2_bb
ISSUERS_DATA$DATASET$SDC$GVKEY_V2 <- gvkey2_iss

###############################################################
### Add market cap, closing price, B/M, volatility, etc + scores

# A helper function that given the event dates it generates the new features. date_lag_function defines the month relative to the event to get the data from
tmp_names = sapply(1:ncol(GLOBAL_MONTHLY_DATABASE$returns_monthly), function(i) paste(str_sub(rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly), start = 1, end = 7), colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)[i]))
create_event_feature <- function(event.permnos, monthly.feature.matrix, event_date){
  monthly.feature.matrix = as.vector(monthly.feature.matrix)
  names(monthly.feature.matrix) = tmp_names
  used = paste(event_date,as.character(event.permnos))
  monthly.feature.matrix[used]
}

last_month_event_date = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-1)), start = 1, end = 7))
last_month_event_date_iss = sapply(ISSUERS_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-1)), start = 1, end = 7))

BUYBACK_DATA$DATASET$CRSP$exchange = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_YEARLY_DATABASE$exchange,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$closing.price = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$prices_monthly,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Market.Cap = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_cap,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Market.Cap_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_cap_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$BE.ME = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$BE.ME,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$BE.ME_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$BE.ME_score,last_month_event_date)
# now the BE/ME the way FF do it 
BUYBACK_DATA$DATASET$CRSP$BE.ME_ff = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$BE.ME_ff,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$BE.ME_ff_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$pre_vol = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$volatility,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$IVOL = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$IVOL,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Rsq = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$Rsq,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$alpha = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$alphas,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$market_beta = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_beta,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$pre_vol_Score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$volatility_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$IVOL_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$IVOL_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Rsq_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$Rsq_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$alpha_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$alphas_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$market_beta_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_beta_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$leverage_d_over_d_plus_e = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_YEARLY_DATABASE$leverage_d_over_d_plus_e,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_YEARLY_DATABASE$leverage_lt_over_lt_plus_e,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$EV_EBITDA_multiple = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$EV_EBITDA_multiple_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score,last_month_event_date)

BUYBACK_DATA$DATASET$CRSP$pre_vol_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$volatility_month,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$IVOL_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$IVOL_month,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Rsq_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$Rsq_month,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$market_beta_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_beta_month,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$pre_vol_month_Score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$volatility_month_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$IVOL_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$IVOL_month_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Rsq_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$Rsq_month_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$market_beta_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_beta_month_score,last_month_event_date)

###
ISSUERS_DATA$DATASET$CRSP$exchange = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_YEARLY_DATABASE$exchange,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$closing.price = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$prices_monthly,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Market.Cap = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$market_cap,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Market.Cap_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$market_cap_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$BE.ME = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$BE.ME,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$BE.ME_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$BE.ME_score,last_month_event_date_iss)
# now the BE/ME the way FF do it 
ISSUERS_DATA$DATASET$CRSP$BE.ME_ff = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$BE.ME_ff,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$BE.ME_ff_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$pre_vol = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$volatility,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$IVOL = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$IVOL,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Rsq = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$Rsq,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$alpha = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$alphas,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$market_beta = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$market_beta,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$pre_vol_Score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$volatility_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$IVOL_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$IVOL_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Rsq_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$Rsq_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$alpha_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$alphas_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$market_beta_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_MONTHLY_DATABASE$market_beta_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$leverage_d_over_d_plus_e = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_YEARLY_DATABASE$leverage_d_over_d_plus_e,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_YEARLY_DATABASE$leverage_lt_over_lt_plus_e,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$EV_EBITDA_multiple = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$EV_EBITDA_multiple_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score,last_month_event_date_iss)

ISSUERS_DATA$DATASET$CRSP$pre_vol_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$volatility_month,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$IVOL_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$IVOL_month,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Rsq_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$Rsq_month,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$market_beta_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_beta_month,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$pre_vol_month_Score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$volatility_month_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$IVOL_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$IVOL_month_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Rsq_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$Rsq_month_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$market_beta_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_MONTHLY_DATABASE$market_beta_month_score,last_month_event_date_iss)

##########################################################################################
# Now add the IBES data. Note there are not available the whole period

ibeslist = list()
for (lag_month in 1:7){
  cat(lag_month,", ")
  tmp_month_event_date = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-lag_month)), start = 1, end = 7))
  tmp = list(
    mean_rec = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$MEANREC,tmp_month_event_date),
    mean_rec_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$MEANREC_score,tmp_month_event_date),
    analyst_coverage = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMREC,tmp_month_event_date),
    analyst_coverage_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMREC_score,tmp_month_event_date),
    analyst_disagreement = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$STDEV,tmp_month_event_date),
    analyst_disagreement_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$STDEV_score,tmp_month_event_date),
    analyst_numup = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMUP,tmp_month_event_date),
    analyst_numup_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMUP_score,tmp_month_event_date),
    analyst_numdown = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMDOWN,tmp_month_event_date),
    analyst_numdown_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMDOWN_score,tmp_month_event_date)
  )
  ibeslist = c(ibeslist,list(tmp))
}
names(ibeslist) <- paste("month_minus",1:7,sep="")
BUYBACK_DATA$DATASET$ibes <- ibeslist

ibeslist = list()
for (lag_month in 1:7){
  cat(lag_month,", ")
  tmp_month_event_date = sapply(ISSUERS_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-lag_month)), start = 1, end = 7))
  tmp = list(
    mean_rec = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$MEANREC,tmp_month_event_date),
    mean_rec_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$MEANREC_score,tmp_month_event_date),
    analyst_coverage = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMREC,tmp_month_event_date),
    analyst_coverage_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMREC_score,tmp_month_event_date),
    analyst_disagreement = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$STDEV,tmp_month_event_date),
    analyst_disagreement_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$STDEV_score,tmp_month_event_date),
    analyst_numup = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMUP,tmp_month_event_date),
    analyst_numup_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMUP_score,tmp_month_event_date),
    analyst_numdown = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMDOWN,tmp_month_event_date),
    analyst_numdown_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno, GLOBAL_IBES_DATABASE$NUMDOWN_score,tmp_month_event_date)
  )
  ibeslist = c(ibeslist,list(tmp))
}
names(ibeslist) <- paste("month_minus",1:7,sep="")
ISSUERS_DATA$DATASET$ibes <- ibeslist

##########################################################################################
#### Other events: we do this here before cleaning data just in case we have any changes of company sizes etc that lead to removal of the company

BUYBACK_DATA$DATASET$SDC$Other_Buyback_later = sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  event.permno = BUYBACK_DATA$DATASET$SDC$permno[i]
  event.date = BUYBACK_DATA$DATASET$SDC$Event.Date[i]
  all_events = which(BUYBACK_DATA$DATASET$SDC$permno == event.permno)
  all_dates = BUYBACK_DATA$DATASET$SDC$Event.Date[all_events]
  sum(all_dates - event.date > 0 & all_dates - event.date < 4*365) 
}) 
BUYBACK_DATA$DATASET$SDC$Other_Buyback_recently = sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  event.permno = BUYBACK_DATA$DATASET$SDC$permno[i]
  event.date = BUYBACK_DATA$DATASET$SDC$Event.Date[i]
  all_events = which(BUYBACK_DATA$DATASET$SDC$permno == event.permno)
  all_dates = BUYBACK_DATA$DATASET$SDC$Event.Date[all_events]
  sum(event.date - all_dates > 0 & event.date - all_dates  < 4*365) 
}) 

ISSUERS_DATA$DATASET$SDC$Other_Issue_later = sapply(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), function(i){
  event.permno = ISSUERS_DATA$DATASET$SDC$permno[i]
  event.date = ISSUERS_DATA$DATASET$SDC$Event.Date[i]
  all_events = which(ISSUERS_DATA$DATASET$SDC$permno == event.permno)
  all_dates = ISSUERS_DATA$DATASET$SDC$Event.Date[all_events]
  sum(all_dates - event.date > 0 & all_dates - event.date < 4*365) 
}) 
ISSUERS_DATA$DATASET$SDC$Other_Issue_recently = sapply(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), function(i){
  event.permno = ISSUERS_DATA$DATASET$SDC$permno[i]
  event.date = ISSUERS_DATA$DATASET$SDC$Event.Date[i]
  all_events = which(ISSUERS_DATA$DATASET$SDC$permno == event.permno)
  all_dates = ISSUERS_DATA$DATASET$SDC$Event.Date[all_events]
  sum(event.date - all_dates > 0 & event.date - all_dates  < 4*365) 
}) 

# Add here cross-buyback-issuers data

BUYBACK_DATA$DATASET$SDC$Otherlater = "2100-01-01"
BUYBACK_DATA$DATASET$SDC$OtherlaterEvent = 0
for (i in 1:length(BUYBACK_DATA$DATASET$SDC$permno)){
  find_other = which(ISSUERS_DATA$DATASET$SDC$permno == BUYBACK_DATA$DATASET$SDC$permno[i])
  if (length(find_other) > 0){
    otherdate = sort(ISSUERS_DATA$DATASET$SDC$Event.Date[find_other])
    date_diff = otherdate - BUYBACK_DATA$DATASET$SDC$Event.Date[i]
    if (sum(date_diff < years_across_cross_events*365 & date_diff > minCROSS_EVENTS_SINCE_LAST_EVENT)){
      BUYBACK_DATA$DATASET$SDC$Otherlater[i]  = as.character(otherdate[date_diff < years_across_cross_events*365 & date_diff > 0][1])
      BUYBACK_DATA$DATASET$SDC$OtherlaterEvent[i] = find_other[which(ISSUERS_DATA$DATASET$SDC$Event.Date[find_other] == BUYBACK_DATA$DATASET$SDC$Otherlater[i])]
    }
  }
}

ISSUERS_DATA$DATASET$SDC$Otherlater = "2100-01-01"
ISSUERS_DATA$DATASET$SDC$OtherlaterEvent = 0
for (i in 1:length(ISSUERS_DATA$DATASET$SDC$permno)){
  find_other = which(BUYBACK_DATA$DATASET$SDC$permno == ISSUERS_DATA$DATASET$SDC$permno[i])
  if (length(find_other) > 0){
    otherdate = sort(BUYBACK_DATA$DATASET$SDC$Event.Date[find_other])
    date_diff = otherdate - ISSUERS_DATA$DATASET$SDC$Event.Date[i]
    if (sum(date_diff < 4*365 & date_diff > minCROSS_EVENTS_SINCE_LAST_EVENT)){
      ISSUERS_DATA$DATASET$SDC$Otherlater[i]  = as.character(otherdate[date_diff < 4*365 & date_diff > 0][1])
      ISSUERS_DATA$DATASET$SDC$OtherlaterEvent[i] = find_other[which(BUYBACK_DATA$DATASET$SDC$Event.Date[find_other] == ISSUERS_DATA$DATASET$SDC$Otherlater[i])]
      
    }
  }
}

###############################################################
## Add the daily data based performance - and remove that data as they are large. THis is slow, so good to do after we remove all events we don't need anyway

daily_dates = as.Date(rownames(GLOBAL_DAILY_DATABASE$recent_returns_daily))
# in case the event was on a non-trading day, find the first trading day before the event up to 10 days before, else insert an NA
event_trade_days = match(BUYBACK_DATA$DATASET$SDC$Event.Date-2, daily_dates)
i=3
while (sum(is.na(event_trade_days)) & i < 10){
  event_trade_days = ifelse(!is.na(event_trade_days), event_trade_days, match(BUYBACK_DATA$DATASET$SDC$Event.Date-i, daily_dates))
  i=i+1
}
sum(is.na(event_trade_days))
tmp = as.vector(GLOBAL_DAILY_DATABASE$recent_returns_daily)
BUYBACK_DATA$DATASET$CRSP$recent_performance = tmp[(match(BUYBACK_DATA$DATASET$SDC$permno, colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily))-1)*nrow(GLOBAL_DAILY_DATABASE$recent_returns_daily) + event_trade_days]
sum(is.na(BUYBACK_DATA$DATASET$CRSP$recent_performance))
tmp = as.vector(GLOBAL_DAILY_DATABASE$recent_returns_daily_score)
BUYBACK_DATA$DATASET$CRSP$recent_performance_score = tmp[(match(BUYBACK_DATA$DATASET$SDC$permno, colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily))-1)*nrow(GLOBAL_DAILY_DATABASE$recent_returns_daily) + event_trade_days]
sum(is.na(BUYBACK_DATA$DATASET$CRSP$recent_performance_score))

event_trade_days = match(ISSUERS_DATA$DATASET$SDC$Event.Date-2, daily_dates)
i=3
while (sum(is.na(event_trade_days)) & i < 10){
  event_trade_days = ifelse(!is.na(event_trade_days), event_trade_days, match(ISSUERS_DATA$DATASET$SDC$Event.Date-i, daily_dates))
  i=i+1
}
sum(is.na(event_trade_days))
tmp = as.vector(GLOBAL_DAILY_DATABASE$recent_returns_daily)
ISSUERS_DATA$DATASET$CRSP$recent_performance = tmp[(match(ISSUERS_DATA$DATASET$SDC$permno, colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily))-1)*nrow(GLOBAL_DAILY_DATABASE$recent_returns_daily) + event_trade_days]
sum(is.na(ISSUERS_DATA$DATASET$CRSP$recent_performance))
tmp = as.vector(GLOBAL_DAILY_DATABASE$recent_returns_daily_score)
ISSUERS_DATA$DATASET$CRSP$recent_performance_score = tmp[(match(ISSUERS_DATA$DATASET$SDC$permno, colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily))-1)*nrow(GLOBAL_DAILY_DATABASE$recent_returns_daily) + event_trade_days]
sum(is.na(ISSUERS_DATA$DATASET$CRSP$recent_performance_score))


##########################################################################################
# Finally add the risk factors and the market 

BUYBACK_DATA$Risk_Factors_Monthly = GLOBAL_MONTHLY_DATABASE$FamaFrench_five_factors
ISSUERS_DATA$Risk_Factors_Monthly = GLOBAL_MONTHLY_DATABASE$FamaFrench_five_factors
BUYBACK_DATA$Market_Monthly = GLOBAL_MONTHLY_DATABASE$Market_Monthly
ISSUERS_DATA$Market_Monthly = GLOBAL_MONTHLY_DATABASE$Market_Monthly
BUYBACK_DATA$Market_daily = GLOBAL_DAILY_DATABASE$Market_Daily
ISSUERS_DATA$Market_daily = GLOBAL_DAILY_DATABASE$Market_Daily

##########################################################################################
##########################################################################################

save(BUYBACK_DATA, ISSUERS_DATA, file = "../FinanceData/created_projects_datasets/BUYBACKSnew.Rdata")

