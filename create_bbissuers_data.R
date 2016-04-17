
##########################################################################################
# Creates the data for the Buybacks-Issuers paper
##########################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

source("../FinanceLibraries/lib_helpers.R")
source("../FinanceLibraries/latex_code.R")
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")

load("../FinanceData/created_monthly_data/GLOBAL_MONTHLY_DATABASE.Rdata")
load("../FinanceData/created_yearly_data/GLOBAL_YEARLY_DATABASE.Rdata")
load("../FinanceData/created_ibes_data/GLOBAL_IBES_DATABASE.Rdata")
load("../FinanceData/created_buyback_data/GLOBAL_BUYBACK.Rdata")
load("../FinanceData/created_issuers_data/GLOBAL_ISSUERS.Rdata")

# Some extra universe data variables we need for this project (based on the raw data)

month_dates = str_sub(rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly), start = 1, end = 7) 
GLOBAL_MONTHLY_DATABASE$market_cap = GLOBAL_MONTHLY_DATABASE$prices_monthly*GLOBAL_MONTHLY_DATABASE$sharesout_monthly
GLOBAL_MONTHLY_DATABASE$market_cap_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$market_cap)
# BE/ME using just this month data:
GLOBAL_MONTHLY_DATABASE$BE.ME = ifelse(GLOBAL_MONTHLY_DATABASE$market_cap, GLOBAL_YEARLY_DATABASE$book_value/GLOBAL_MONTHLY_DATABASE$market_cap, NA)
GLOBAL_MONTHLY_DATABASE$BE.ME_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$BE.ME)
rownames(GLOBAL_MONTHLY_DATABASE$BE.ME) <- rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
colnames(GLOBAL_MONTHLY_DATABASE$BE.ME_score) <- colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
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
GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score = get_cross_section_score(GLOBAL_MONTHLY_DATABASE$BE.ME_ff)
rownames(GLOBAL_MONTHLY_DATABASE$BE.ME_ff) <- rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
colnames(GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score) <- colnames(GLOBAL_MONTHLY_DATABASE$returns_monthly)
GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score <- get_cross_section_score(GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple)

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

GLOBAL_IBES_DATABASE$MEANREC_score = get_cross_section_score(GLOBAL_IBES_DATABASE$MEANREC)
GLOBAL_IBES_DATABASE$NUMREC_score = get_cross_section_score(GLOBAL_IBES_DATABASE$NUMREC)
GLOBAL_IBES_DATABASE$STDEV_score = get_cross_section_score(GLOBAL_IBES_DATABASE$STDEV)

## This is slow but we need it for the pre-event performance. 
load("../FinanceData/created_daily_data/GLOBAL_DAILY_DATABASE.Rdata")
GLOBAL_DAILY_DATABASE$returns_daily <- scrub(GLOBAL_DAILY_DATABASE$returns_daily)
tmp1 <- apply(GLOBAL_DAILY_DATABASE$returns_daily,2,function(r) ms(r,120)) # we only have trading days
tmp2 <- apply(GLOBAL_DAILY_DATABASE$returns_daily!=0,2,function(r) ms(r,120)) # we only have trading days
GLOBAL_DAILY_DATABASE$recent_returns_daily = ifelse(tmp2 > 60, tmp1, NA) # need at least 60 trading (non-zero returns!) days recently
GLOBAL_DAILY_DATABASE$recent_returns_daily_score = get_cross_section_score(GLOBAL_DAILY_DATABASE$recent_returns_daily)
rownames(GLOBAL_DAILY_DATABASE$recent_returns_daily) <- rownames(GLOBAL_DAILY_DATABASE$returns_daily)
colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily) <- colnames(GLOBAL_DAILY_DATABASE$returns_daily)
rownames(GLOBAL_DAILY_DATABASE$recent_returns_daily_score) <- rownames(GLOBAL_DAILY_DATABASE$returns_daily)
colnames(GLOBAL_DAILY_DATABASE$recent_returns_daily_score) <- colnames(GLOBAL_DAILY_DATABASE$returns_daily)
rm("tmp1", "tmp2")

# Will use to get the permnos or other company identifiers using the cusip-date. Maybe there is a better way to do this
load("../FinanceData/rawdata_universe_of_companies/crsp_permno_cusip_pairs.Rdata")
cusip_date_universe = paste(universe_companies$NCUSIP, universe_companies$date, sep="-")

############################################################################################
# Gather all data we need, aligned correctly pre-event
############################################################################################

###############################################################
### keep only permnos available in the global daily/monthly/yearly data

cusip_date = paste(BUYBACK_DATA$DATASET$SDC$CUSIP, format(BUYBACK_DATA$DATASET$SDC$Event.Date, "%Y%m%d"), sep="-")
the_matches = match(cusip_date,cusip_date_universe)
BUYBACK_DATA$DATASET$SDC$permno <- ifelse(is.na(the_matches), NA, universe_companies$PERMNO[the_matches])
BUYBACK_DATA$cleanupNoPermno <- sum(is.na(BUYBACK_DATA$DATASET$SDC$permno))
useonly = which(!is.na(BUYBACK_DATA$DATASET$SDC$permno))
BUYBACK_DATA$DATASET$SDC = BUYBACK_DATA$DATASET$SDC[useonly,]

cusip_date = paste(ISSUERS_DATA$DATASET$SDC$CUSIP, format(ISSUERS_DATA$DATASET$SDC$Event.Date, "%Y%m%d"), sep="-")
the_matches = match(cusip_date,cusip_date_universe)
ISSUERS_DATA$DATASET$SDC$permno <- ifelse(is.na(the_matches), NA, universe_companies$PERMNO[the_matches])
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
### Add market cap, closing price, B/M, volatility, etc + scores

template_month_dates = str_sub(rownames(GLOBAL_MONTHLY_DATABASE$returns_monthly), start = 1, end = 7) # remove the day in case it is there

# A helper function that given the event dates it generates the new features. date_lag_function defines the month relative to the event to get the data from
create_event_feature <- function(event.permnos, event.dates, monthly.feature.matrix, event_date){
  tmp = as.vector(monthly.feature.matrix)
  indices = (match(as.character(event.permnos), colnames(monthly.feature.matrix))-1)*nrow(monthly.feature.matrix) + match(event_date, template_month_dates)
  tmp[indices]
  #sapply(1:length(event.permnos), function(i) monthly.feature.matrix[event_date[i],as.character(event.permnos)[i]]) # this was the slow way
}

last_month_event_date = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-1)), start = 1, end = 7))
last_month_event_date_iss = sapply(ISSUERS_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-1)), start = 1, end = 7))

BUYBACK_DATA$DATASET$CRSP$closing.price = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$prices_monthly,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Market.Cap = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$market_cap,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Market.Cap_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$market_cap_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$BE.ME = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$BE.ME_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME_score,last_month_event_date)
# now the BE/ME the way FF do it 
BUYBACK_DATA$DATASET$CRSP$BE.ME_ff = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME_ff,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$BE.ME_ff_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$pre_vol_Score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$volatility_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$IVOL_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$IVOL_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$Rsq_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$Rsq_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$market_beta_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$market_beta_score,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$leverage_d_over_d_plus_e = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$leverage_d_over_d_plus_e,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$leverage_lt_over_lt_plus_e,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$EV_EBITDA_multiple = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple,last_month_event_date)
BUYBACK_DATA$DATASET$CRSP$EV_EBITDA_multiple_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score,last_month_event_date)

ISSUERS_DATA$DATASET$CRSP$closing.price = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$prices_monthly,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Market.Cap = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$market_cap,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Market.Cap_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$market_cap_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$BE.ME = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$BE.ME_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME_score,last_month_event_date_iss)
# now the BE/ME the way FF do it 
ISSUERS_DATA$DATASET$CRSP$BE.ME_ff = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME_ff,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$BE.ME_ff_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$BE.ME_ff_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$pre_vol_Score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$volatility_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$IVOL_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$IVOL_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$Rsq_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$Rsq_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$market_beta_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_MONTHLY_DATABASE$market_beta_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$leverage_d_over_d_plus_e = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$leverage_d_over_d_plus_e,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$leverage_lt_over_lt_plus_e,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$EV_EBITDA_multiple = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple,last_month_event_date_iss)
ISSUERS_DATA$DATASET$CRSP$EV_EBITDA_multiple_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_YEARLY_DATABASE$EV_EBITDA_multiple_score,last_month_event_date_iss)

##########################################################################################
# Now add the IBES data. Note there are not available the whole period

last_last_month_event_date = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-2)), start = 1, end = 7))
last_last_month_event_date_iss = sapply(ISSUERS_DATA$DATASET$SDC$Event.Date, function(x) str_sub(as.character(AddMonths(as.Date(x),-2)), start = 1, end = 7))

BUYBACK_DATA$DATASET$ibes$mean_rec_last_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC,last_month_event_date)
BUYBACK_DATA$DATASET$ibes$mean_rec_last_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC_score,last_month_event_date)
BUYBACK_DATA$DATASET$ibes$mean_rec_last_last_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC,last_last_month_event_date)
BUYBACK_DATA$DATASET$ibes$mean_rec_last_last_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC_score,last_last_month_event_date)
BUYBACK_DATA$DATASET$ibes$analyst_coverage_last_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$NUMREC,last_month_event_date)
BUYBACK_DATA$DATASET$ibes$analyst_coverage_last_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$NUMREC_score,last_month_event_date)
BUYBACK_DATA$DATASET$ibes$analyst_disagreement_last_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV,last_month_event_date)
BUYBACK_DATA$DATASET$ibes$analyst_disagreement_last_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV_score,last_month_event_date)
BUYBACK_DATA$DATASET$ibes$analyst_disagreement_last_last_month = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV,last_last_month_event_date)
BUYBACK_DATA$DATASET$ibes$analyst_disagreement_last_last_month_score = create_event_feature(BUYBACK_DATA$DATASET$SDC$permno,BUYBACK_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV_score,last_last_month_event_date)

ISSUERS_DATA$DATASET$ibes$mean_rec_last_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC,last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$mean_rec_last_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$mean_rec_last_last_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC,last_last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$mean_rec_last_last_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$MEANREC_score,last_last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$analyst_coverage_last_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$NUMREC,last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$analyst_coverage_last_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$NUMREC_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$analyst_disagreement_last_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV,last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$analyst_disagreement_last_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV_score,last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$analyst_disagreement_last_last_month = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV,last_last_month_event_date_iss)
ISSUERS_DATA$DATASET$ibes$analyst_disagreement_last_last_month_score = create_event_feature(ISSUERS_DATA$DATASET$SDC$permno,ISSUERS_DATA$DATASET$SDC$Event.Date, GLOBAL_IBES_DATABASE$STDEV_score,last_last_month_event_date_iss)

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

rm("GLOBAL_DAILY_DATABASE")

###############################################################
# Make the U-Index scores
BUYBACK_DATA$Performance_used <- ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$recent_performance_score), NA, ceiling(5*(1-BUYBACK_DATA$DATASET$CRSP$recent_performance_score)))
BUYBACK_DATA$Performance_used[BUYBACK_DATA$Performance_used==0] <- 1
BUYBACK_DATA$Size_used <- ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap_score), NA, ceiling(5*(1-BUYBACK_DATA$DATASET$CRSP$Market.Cap_score)))
BUYBACK_DATA$Size_used[BUYBACK_DATA$Size_used==0] <- 1
# USE OUR BE.ME, NOT THE FF
BUYBACK_DATA$BEME_used <- ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$BE.ME_score), NA, ceiling(5*BUYBACK_DATA$DATASET$CRSP$BE.ME_score))
BUYBACK_DATA$BEME_used[BUYBACK_DATA$BEME_used==0] <- 1
BUYBACK_DATA$Valuation_Index = BUYBACK_DATA$Performance_used + BUYBACK_DATA$Size_used + BUYBACK_DATA$BEME_used 

# Note differences from buybacks for performance and BE/ME 
ISSUERS_DATA$Performance_used <- ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$recent_performance_score), NA, ceiling(5*(ISSUERS_DATA$DATASET$CRSP$recent_performance_score)))
ISSUERS_DATA$Performance_used[ISSUERS_DATA$Performance_used==0] <- 1
ISSUERS_DATA$Size_used <- ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap_score), NA, ceiling(5*(1-ISSUERS_DATA$DATASET$CRSP$Market.Cap_score)))
ISSUERS_DATA$Size_used[ISSUERS_DATA$Size_used==0] <- 1
# USE OUR BE.ME, NOT THE FF
ISSUERS_DATA$BEME_used <- ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$BE.ME_score), NA, ceiling(5*(1-ISSUERS_DATA$DATASET$CRSP$BE.ME_score)))
ISSUERS_DATA$BEME_used[ISSUERS_DATA$BEME_used==0] <- 1
ISSUERS_DATA$Valuation_Index = ISSUERS_DATA$Performance_used + ISSUERS_DATA$Size_used + ISSUERS_DATA$BEME_used 

##########################################################################################
##########################################################################################
# Now some clean up: remove events for which we have missing values (for the main missing values)
##########################################################################################
##########################################################################################
to_remove = which(
  # just in alphabetic order not to forget any    
  is.na(BUYBACK_DATA$BEME_used) | 
    is.na(BUYBACK_DATA$Performance_used) | 
    is.na(BUYBACK_DATA$Size_used) | 
    is.na(BUYBACK_DATA$Valuation_Index) | 
    is.na(BUYBACK_DATA$DATASET$CRSP$pre_vol_Score) |
    is.na(BUYBACK_DATA$DATASET$CRSP$IVOL_score) |
    is.na(BUYBACK_DATA$DATASET$CRSP$Rsq_score) |
    is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap) 
)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  BUYBACK_DATA$BEME_used <- BUYBACK_DATA$BEME_used[-to_remove]
  BUYBACK_DATA$Performance_used <- BUYBACK_DATA$Performance_used[-to_remove]
  BUYBACK_DATA$Size_used <- BUYBACK_DATA$Size_used[-to_remove]
  BUYBACK_DATA$Valuation_Index <- BUYBACK_DATA$Valuation_Index[-to_remove]
  
  BUYBACK_DATA$DATASET$returns_by_event_monthly <- BUYBACK_DATA$DATASET$returns_by_event_monthly[,-to_remove]
  BUYBACK_DATA$DATASET$SDC <- BUYBACK_DATA$DATASET$SDC[-to_remove,]
  for(field in ls(BUYBACK_DATA$DATASET$CRSP))  BUYBACK_DATA$DATASET$CRSP[[field]] <- BUYBACK_DATA$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA$DATASET$ibes))  BUYBACK_DATA$DATASET$ibes[[field]] <- BUYBACK_DATA$DATASET$ibes[[field]][-to_remove]
}
BUYBACK_DATA$cleanupMissingSomeValues = length(to_remove)

to_remove = which(
  # just in alphabetic order not to forget any    
  is.na(ISSUERS_DATA$BEME_used) | 
    is.na(ISSUERS_DATA$Performance_used) | 
    is.na(ISSUERS_DATA$Size_used) | 
    is.na(ISSUERS_DATA$Valuation_Index) | 
    is.na(ISSUERS_DATA$DATASET$CRSP$pre_vol_Score) |
    is.na(ISSUERS_DATA$DATASET$CRSP$IVOL_score) |
    is.na(ISSUERS_DATA$DATASET$CRSP$Rsq_score) |
    is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap) 
)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  ISSUERS_DATA$BEME_used <- ISSUERS_DATA$BEME_used[-to_remove]
  ISSUERS_DATA$Performance_used <- ISSUERS_DATA$Performance_used[-to_remove]
  ISSUERS_DATA$Size_used <- ISSUERS_DATA$Size_used[-to_remove]
  ISSUERS_DATA$Valuation_Index <- ISSUERS_DATA$Valuation_Index[-to_remove]
  
  ISSUERS_DATA$DATASET$returns_by_event_monthly <- ISSUERS_DATA$DATASET$returns_by_event_monthly[,-to_remove]
  ISSUERS_DATA$DATASET$SDC <- ISSUERS_DATA$DATASET$SDC[-to_remove,]
  for(field in ls(ISSUERS_DATA$DATASET$CRSP))  ISSUERS_DATA$DATASET$CRSP[[field]] <- ISSUERS_DATA$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(ISSUERS_DATA$DATASET$ibes))  ISSUERS_DATA$DATASET$ibes[[field]] <- ISSUERS_DATA$DATASET$ibes[[field]][-to_remove]
}
ISSUERS_DATA$cleanupMissingSomeValues = length(to_remove)

##########################################################################################
# Project specific filters now 

# Note the use of the CRSP list instead of the SDC list for data like closing prices, market cap, etc (unlike earlier version)
# Buybacks first
events = BUYBACK_DATA$DATASET
No_filter = rep(T,length(events$SDC$Event.Date))
Basic_filter =  No_filter # !is.na(events$SDC$ME_quantile) & !is.na(events$SDC$BEME_quantile)  
Period_filter <- (as.Date(events$SDC$Event.Date) >= First) & (as.Date(events$SDC$Event.Date) <= Last)
#Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$SDC$Closing.Price) >= penny_stock_price_old, scrub(events$SDC$Closing.Price) >= penny_stock_price_recent) 
Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$CRSP$closing.price) >= penny_stock_price_old, scrub(events$CRSP$closing.price) >= penny_stock_price_recent) 
# REMOVE FINANCIALS AND UTILITIES: We leave them for now so we can use them as needed. This decision is made in bb_issuers_new.R
#Industry_filter = events$SDC$Industry %in% INDUSTRY_USED 
Industry_filter = 1
US_only = (events$SDC$Currency %in% good_currencies)
major_markets_only = sapply(events$SDC$Stock.Exchange, function(i) sum(str_split(i, "\\+")[[1]] %in% major_markets)>0)
#BUYBACK SPECIFIC NOW:
Technique_filter =  events$SDC$Tech..nique.Code %in% BB_allowed_techniques # OP, OPNG, and ""    
TOTAL_FILTER_basic = No_filter & Basic_filter & Period_filter & Penny_stock_filter  & Industry_filter & US_only & major_markets_only & Technique_filter
#Market_cap_filter =  (scrub(events$SDC$Market.Cap) >= MIN_SIZE) & (scrub(events$SDC$Market.Cap) <= MAX_SIZE)
Market_cap_filter =  (scrub(events$CRSP$Market.Cap) >= MIN_SIZE) & (scrub(events$CRSP$Market.Cap) <= MAX_SIZE)
#Leverage_filter = (scrub(events$SDC$lt/events$SDC$at) > 0.5)*(!is.na(events$SDC$lt/events$SDC$at))
EventSize_filter = (events$SDC$Event.Size >= MIN_EVENT_SIZE) & (events$SDC$Event.Size <= MAX_EVENT_SIZE)
TOTAL_FILTER_complex = Market_cap_filter  & EventSize_filter 
### remove
TOTAL_FILTER = TOTAL_FILTER_basic & TOTAL_FILTER_complex
BIZ_initial_data = length(TOTAL_FILTER)
to_remove = which(!TOTAL_FILTER)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  BUYBACK_DATA$BEME_used <- BUYBACK_DATA$BEME_used[-to_remove]
  BUYBACK_DATA$Performance_used <- BUYBACK_DATA$Performance_used[-to_remove]
  BUYBACK_DATA$Size_used <- BUYBACK_DATA$Size_used[-to_remove]
  BUYBACK_DATA$Valuation_Index <- BUYBACK_DATA$Valuation_Index[-to_remove]
  
  BUYBACK_DATA$DATASET$returns_by_event_monthly <- BUYBACK_DATA$DATASET$returns_by_event_monthly[,-to_remove]
  BUYBACK_DATA$DATASET$SDC <- BUYBACK_DATA$DATASET$SDC[-to_remove,]
  for(field in ls(BUYBACK_DATA$DATASET$CRSP))  BUYBACK_DATA$DATASET$CRSP[[field]] <- BUYBACK_DATA$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(BUYBACK_DATA$DATASET$ibes))  BUYBACK_DATA$DATASET$ibes[[field]] <- BUYBACK_DATA$DATASET$ibes[[field]][-to_remove]
}
### Keep track
cleanup = list()
cleanup$initial_data <- BIZ_initial_data
cleanup$total_removed = length(to_remove)
cleanup$Basic_filter <- sum(!Basic_filter)
cleanup$Period_filter <- sum(!Period_filter)
cleanup$Penny_stock_filter <- sum(!Penny_stock_filter)
cleanup$Industry_filter <- sum(!Industry_filter)
cleanup$US_only <- sum(!US_only)
cleanup$BIZ_allowed_techniques <- sum(!Technique_filter)
cleanup$major_markets_only <- sum(!major_markets_only)
cleanup$Market_cap_filter <- sum(!Market_cap_filter)
cleanup$EventSize_filter <- sum(!EventSize_filter)
BUYBACK_DATA$cleanupBIZ = cleanup

# Issuers now
events = ISSUERS_DATA$DATASET
No_filter = rep(T,length(events$SDC$Event.Date))
Basic_filter =  No_filter # !is.na(events$SDC$ME_quantile) & !is.na(events$SDC$BEME_quantile)  
Period_filter <- (as.Date(events$SDC$Event.Date) >= First) & (as.Date(events$SDC$Event.Date) <= Last)
#Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$SDC$Closing.Price) >= penny_stock_price_old, scrub(events$SDC$Closing.Price) >= penny_stock_price_recent) 
Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$CRSP$closing.price) >= penny_stock_price_old, scrub(events$CRSP$closing.price) >= penny_stock_price_recent) 
# REMOVE FINANCIALS AND UTILITIES
# REMOVE FINANCIALS AND UTILITIES: We leave them for now so we can use them as needed. This decision is made in bb_issuers_new.R
#Industry_filter = events$SDC$Industry %in% INDUSTRY_USED 
Industry_filter = 1
US_only = (events$SDC$Currency %in% good_currencies)
major_markets_only = sapply(events$SDC$Stock.Exchange, function(i) sum(str_split(i, "\\+")[[1]] %in% major_markets)>0)
#ISSUERS SPECIFIC NOW:
Technique_filter =  events$SDC$Offering.Technique %in% ISS_allowed_techniques        
TOTAL_FILTER_basic = No_filter & Basic_filter & Period_filter & Penny_stock_filter  & Industry_filter & US_only & major_markets_only & Technique_filter
#Market_cap_filter =  (scrub(events$SDC$Market.Cap) >= MIN_SIZE) & (scrub(events$SDC$Market.Cap) <= MAX_SIZE)
Market_cap_filter =  (scrub(events$CRSP$Market.Cap) >= MIN_SIZE) & (scrub(events$CRSP$Market.Cap) <= MAX_SIZE)
#Leverage_filter = (scrub(events$SDC$lt/events$SDC$at) > 0.5)*(!is.na(events$SDC$lt/events$SDC$at))
EventSize_filter = (events$SDC$Event.Size >= MIN_EVENT_SIZE) & (events$SDC$Event.Size <= MAX_EVENT_SIZE)
TOTAL_FILTER_complex = Market_cap_filter  & EventSize_filter 
### remove
TOTAL_FILTER = TOTAL_FILTER_basic & TOTAL_FILTER_complex
BIZ_initial_data = length(TOTAL_FILTER)
to_remove = which(!TOTAL_FILTER)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  ISSUERS_DATA$BEME_used <- ISSUERS_DATA$BEME_used[-to_remove]
  ISSUERS_DATA$Performance_used <- ISSUERS_DATA$Performance_used[-to_remove]
  ISSUERS_DATA$Size_used <- ISSUERS_DATA$Size_used[-to_remove]
  ISSUERS_DATA$Valuation_Index <- ISSUERS_DATA$Valuation_Index[-to_remove]
  
  ISSUERS_DATA$DATASET$returns_by_event_monthly <- ISSUERS_DATA$DATASET$returns_by_event_monthly[,-to_remove]
  ISSUERS_DATA$DATASET$SDC <- ISSUERS_DATA$DATASET$SDC[-to_remove,]
  for(field in ls(ISSUERS_DATA$DATASET$CRSP))  ISSUERS_DATA$DATASET$CRSP[[field]] <- ISSUERS_DATA$DATASET$CRSP[[field]][-to_remove]
  for(field in ls(ISSUERS_DATA$DATASET$ibes))  ISSUERS_DATA$DATASET$ibes[[field]] <- ISSUERS_DATA$DATASET$ibes[[field]][-to_remove]
}

### Keep track
cleanup = list()
cleanup$initial_data <- BIZ_initial_data
cleanup$total_removed = length(to_remove)
cleanup$Basic_filter <- sum(!Basic_filter)
cleanup$Period_filter <- sum(!Period_filter)
cleanup$Penny_stock_filter <- sum(!Penny_stock_filter)
cleanup$Industry_filter <- sum(!Industry_filter)
cleanup$US_only <- sum(!US_only)
cleanup$BIZ_allowed_techniques <- sum(!Technique_filter)
cleanup$major_markets_only <- sum(!major_markets_only)
cleanup$Market_cap_filter <- sum(!Market_cap_filter)
cleanup$EventSize_filter <- sum(!EventSize_filter)
ISSUERS_DATA$cleanupBIZ = cleanup

###### Check time order of events
ordered_events = sort(as.numeric(BUYBACK_DATA$DATASET$SDC$Event.Date),index.return = T)$ix
if (length(unique(diff(ordered_events)))!=1)
  stop("The time order was messed up somewhere for buybacks")
rm("ordered_events")
ordered_events = sort(as.numeric(ISSUERS_DATA$DATASET$SDC$Event.Date),index.return = T)$ix
if (length(unique(diff(ordered_events)))!=1)
  stop("The time order was messed up somewhere for issuers")
rm("ordered_events")

# Finally add the risk factors and the market
BUYBACK_DATA$Risk_Factors_Monthly = GLOBAL_MONTHLY_DATABASE$FamaFrench_five_factors
ISSUERS_DATA$Risk_Factors_Monthly = GLOBAL_MONTHLY_DATABASE$FamaFrench_five_factors
BUYBACK_DATA$Market_Monthly = GLOBAL_MONTHLY_DATABASE$Market_Monthly
ISSUERS_DATA$Market_Monthly = GLOBAL_MONTHLY_DATABASE$Market_Monthly

##########################################################################################
##########################################################################################

save(BUYBACK_DATA, ISSUERS_DATA, file = "../FinanceData/created_projects_datasets/BUYBACKSnew.Rdata")

