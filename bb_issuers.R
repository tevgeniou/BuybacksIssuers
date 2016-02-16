
initial_vars = ls(all = TRUE) # takes time to save and load!!! so we save only what is needed at the end. 
# load("tmpfiles/vol_analysis.Rdata")
# load("tmpfiles/bb_issuersALL.Rdata") # If we want to add/change things without rerunning all...

##################################################################
# Load the data: EVENTS ARE ORDERED IN TIME AND TIME ALIGNED
##################################################################
if (time_align_all_data){
  source("dataset/process_code/get_ff_and_market_data.R")
  if (!file.exists("dataset/buybacks.Rdata") | generate_all_data){
    datasetname = "buybacks" 
    source("dataset/process_code/create_dataset.R")
    buybacks = events; rm("events") 
  } else {
    load("dataset/buybacks.Rdata")    
    buybacks = events; rm("events")
  }
  
  if (!file.exists("dataset/issuers.Rdata") | generate_all_data){
    datasetname = "issuers" 
    source("dataset/process_code/create_dataset.R")
    issuers = events; rm("events") 
  } else {
    load("dataset/issuers.Rdata") 
    issuers = events; rm("events") 
  }
  
  ####################################################################################
  # Get the FF and market data and ALWAYS time align all data!
  load("dataset/indices_and_factors/market_ff_data.Rdata")
  # MAKE SURE ALL DATASETS USE THE SAME TRADING DAYS. USE ONLY THE COMMON DAILY RETURNS DAYS WITH THE MARKET
  used_days = intersect(rownames(Risk_Factors),intersect(intersect(names(Market_Daily), rownames(issuers$returns_by_event)), rownames(buybacks$returns_by_event)))
  Market_Daily = Market_Daily[used_days]
  Risk_Factors = Risk_Factors[used_days,]  
  issuers$returns_by_event = issuers$returns_by_event[used_days,]
  buybacks$returns_by_event = buybacks$returns_by_event[used_days,]
  # ALL monthly returns have rownames "Year-month-01"  (TO CONFIRM!)
  used_months = intersect(rownames(Risk_Factors_Monthly),intersect(rownames(issuers$returns_by_event_monthly), rownames(buybacks$returns_by_event_monthly)))
  Risk_Factors_Monthly = Risk_Factors_Monthly[used_months,]
  issuers$returns_by_event_monthly = issuers$returns_by_event_monthly[used_months,]
  buybacks$returns_by_event_monthly = buybacks$returns_by_event_monthly[used_months,]
  ####################################################################################
  
  # Now save the final data 
  DATASET = buybacks
  DATASET$name = "buybacks"
  save(DATASET, file="dataset/buybacks_aligned.Rdata") 
  DATASET = issuers
  DATASET$name = "issuers"
  save(DATASET, file="dataset/issuers_aligned.Rdata") 
  rm("buybacks", "issuers")
  save(Market_Daily,Market_Monthly,Risk_Factors,Risk_Factors_Monthly, returnsbreakpoints,MEbreakpoints,BE.MEbreakpoints, file="dataset/indices_and_factors/market_ff_data.Rdata")
  
}

#################################################################
load("dataset/indices_and_factors/market_ff_data.Rdata")
#################################################################

#################################################################
## Get the abnormal returns and any other cross-events data we may need first

if (rerun_buyback_data_code) {
  cat("\nGenerating report_dataBuybacks.Rdata")
  filename_to_save <- "report_dataBuybacks.Rdata"
  load("dataset/buybacks_aligned.Rdata") 
  source("report_data.R"); rm("report_data","filename_to_save")
} 

if (rerun_issuers_data_code) {
  cat("\nGenerating report_dataIssuers.Rdata")
  filename_to_save <- "report_dataIssuers.Rdata"
  load("dataset/issuers_aligned.Rdata") 
  source("report_data.R"); rm("report_data","filename_to_save")
} 


##################################################################
# Now get the data we need for the report
##################################################################
if (rerun_buyback_report_code){
  cat("\nGenerating reportcode_dataBuybacks.Rdata")
  load("tmpfiles/report_dataBuybacks.Rdata")
  filename_to_save <- "reportcode_dataBuybacks.Rdata"
  source("report_code.R")  
  BUYBACK_DATA = reportcode_data; rm("reportcode_data","report_data","filename_to_save") 
} else {
  load("tmpfiles/reportcode_dataBuybacks.Rdata")
  BUYBACK_DATA = reportcode_data; rm("reportcode_data") 
}

if (rerun_issuers_report_code){
  cat("\nGenerating reportcode_dataIssuers.Rdata")
  load("tmpfiles/report_dataIssuers.Rdata")
  filename_to_save <- "reportcode_dataIssuers.Rdata"
  source("report_code.R")  
  ISSUERS_DATA = reportcode_data; rm("reportcode_data","report_data","filename_to_save")
} else {
  load("tmpfiles/reportcode_dataIssuers.Rdata")
  ISSUERS_DATA = reportcode_data; rm("reportcode_data") 
}


###############################################################################################
# Just get some variables we will be using in the report. 
# NEED TO USE EITHER the ISSUERS_DATA structure or the BUYBACK_DATA one depending on what we report. THESE TWO HAVE EVERYTHING WE NEED (except some data creation variables we need to add.... see what is missing in the report!)
###############################################################################################

valuation_index_bb = BUYBACK_DATA$Valuation_Index
valuation_index_iss = ISSUERS_DATA$Valuation_Index

IRATS_table_all_bb_betas3f = BUYBACK_DATA$IRATS_table_all_3f$betas
IRATS_table_all_bb_betas5f = BUYBACK_DATA$IRATS_table_all_5f$betas
IRATS_table_all_bb_betaststat3f = ifelse(BUYBACK_DATA$IRATS_table_all_3f$betasstderr,BUYBACK_DATA$IRATS_table_all_3f$betas/BUYBACK_DATA$IRATS_table_all_3f$betasstderr,0)
IRATS_table_all_bb_betaststat5f = ifelse(BUYBACK_DATA$IRATS_table_all_5f$betasstderr,BUYBACK_DATA$IRATS_table_all_5f$betas/BUYBACK_DATA$IRATS_table_all_5f$betasstderr,0)
IRATS_table_all_bb = BUYBACK_DATA$IRATS_table_all
BBtable_time = BUYBACK_DATA$IRATS_table_time
IRATS_table_all_bb_betas3fU = BUYBACK_DATA$IRATS_table_undervaluation_undervalued3f$betas
IRATS_table_all_bb_betas5fU = BUYBACK_DATA$IRATS_table_undervaluation_undervalued5f$betas
IRATS_table_all_bb_betaststat3fU = ifelse(BUYBACK_DATA$IRATS_table_undervaluation_undervalued3f$betasstderr,BUYBACK_DATA$IRATS_table_undervaluation_undervalued3f$betas/BUYBACK_DATA$IRATS_table_undervaluation_undervalued3f$betasstderr,0)
IRATS_table_all_bb_betaststat5fU = ifelse(BUYBACK_DATA$IRATS_table_undervaluation_undervalued5f$betasstderr,BUYBACK_DATA$IRATS_table_undervaluation_undervalued5f$betas/BUYBACK_DATA$IRATS_table_undervaluation_undervalued5f$betasstderr,0)
IRATS_table_all_bb_betas3fO = BUYBACK_DATA$IRATS_table_undervaluation_overvalued3f$betas
IRATS_table_all_bb_betas5fO = BUYBACK_DATA$IRATS_table_undervaluation_overvalued5f$betas
IRATS_table_all_bb_betaststat3fO = ifelse(BUYBACK_DATA$IRATS_table_undervaluation_overvalued3f$betasstderr,BUYBACK_DATA$IRATS_table_undervaluation_overvalued3f$betas/BUYBACK_DATA$IRATS_table_undervaluation_overvalued3f$betasstderr,0)
IRATS_table_all_bb_betaststat5fO = ifelse(BUYBACK_DATA$IRATS_table_undervaluation_overvalued5f$betasstderr,BUYBACK_DATA$IRATS_table_undervaluation_overvalued5f$betas/BUYBACK_DATA$IRATS_table_undervaluation_overvalued5f$betasstderr,0)
IRATS_table_all_bb_undervaluation = BUYBACK_DATA$IRATS_table_undervaluation
BBtable_undertime = BUYBACK_DATA$IRATS_table_undervaluation_time
## Issuers
IRATS_table_all_iss_betas3f = ISSUERS_DATA$IRATS_table_all_3f$betas
IRATS_table_all_iss_betas5f = ISSUERS_DATA$IRATS_table_all_5f$betas
IRATS_table_all_iss_betaststat3f = ifelse(ISSUERS_DATA$IRATS_table_all_3f$betasstderr,ISSUERS_DATA$IRATS_table_all_3f$betas/ISSUERS_DATA$IRATS_table_all_3f$betasstderr,0)
IRATS_table_all_iss_betaststat5f = ifelse(ISSUERS_DATA$IRATS_table_all_5f$betasstderr,ISSUERS_DATA$IRATS_table_all_5f$betas/ISSUERS_DATA$IRATS_table_all_5f$betasstderr,0)
IRATS_table_all_iss = ISSUERS_DATA$IRATS_table_all
ISStable_time = ISSUERS_DATA$IRATS_table_time  
IRATS_table_all_iss_betas3fU = ISSUERS_DATA$IRATS_table_undervaluation_undervalued3f$betas
IRATS_table_all_iss_betas5fU = ISSUERS_DATA$IRATS_table_undervaluation_undervalued5f$betas
IRATS_table_all_iss_betaststat3fU = ifelse(ISSUERS_DATA$IRATS_table_undervaluation_undervalued3f$betasstderr,ISSUERS_DATA$IRATS_table_undervaluation_undervalued3f$betas/ISSUERS_DATA$IRATS_table_undervaluation_undervalued3f$betasstderr,0)
IRATS_table_all_iss_betaststat5fU = ifelse(ISSUERS_DATA$IRATS_table_undervaluation_undervalued5f$betasstderr,ISSUERS_DATA$IRATS_table_undervaluation_undervalued5f$betas/ISSUERS_DATA$IRATS_table_undervaluation_undervalued5f$betasstderr,0)
IRATS_table_all_iss_betas3fO = ISSUERS_DATA$IRATS_table_undervaluation_overvalued3f$betas
IRATS_table_all_iss_betas5fO = ISSUERS_DATA$IRATS_table_undervaluation_overvalued5f$betas
IRATS_table_all_iss_betaststat3fO = ifelse(ISSUERS_DATA$IRATS_table_undervaluation_overvalued3f$betasstderr,ISSUERS_DATA$IRATS_table_undervaluation_overvalued3f$betas/ISSUERS_DATA$IRATS_table_undervaluation_overvalued3f$betasstderr,0)
IRATS_table_all_iss_betaststat5fO = ifelse(ISSUERS_DATA$IRATS_table_undervaluation_overvalued5f$betasstderr,ISSUERS_DATA$IRATS_table_undervaluation_overvalued5f$betas/ISSUERS_DATA$IRATS_table_undervaluation_overvalued5f$betasstderr,0)
IRATS_table_all_iss_undervaluation = ISSUERS_DATA$IRATS_table_undervaluation
ISStable_undertime = ISSUERS_DATA$IRATS_table_undervaluation_time
### Calendar
CAL_table_all_bb_betas3f = BUYBACK_DATA$CAL_table_all_3f$betas
CAL_table_all_bb_betas5f = BUYBACK_DATA$CAL_table_all_5f$betas
CAL_table_all_bb_betaststat3f = ifelse(BUYBACK_DATA$CAL_table_all_3f$betasstderr,BUYBACK_DATA$CAL_table_all_3f$betas/BUYBACK_DATA$CAL_table_all_3f$betasstderr,0)
CAL_table_all_bb_betaststat5f = ifelse(BUYBACK_DATA$CAL_table_all_5f$betasstderr,BUYBACK_DATA$CAL_table_all_5f$betas/BUYBACK_DATA$CAL_table_all_5f$betasstderr,0)
CAL_table_all_bb = BUYBACK_DATA$CAL_table_all
BBtable_time_cal = BUYBACK_DATA$CAL_table_time
CAL_table_all_bb_betas3fU = BUYBACK_DATA$CAL_table_undervaluation_undervalued3f$betas
CAL_table_all_bb_betas5fU = BUYBACK_DATA$CAL_table_undervaluation_undervalued5f$betas
CAL_table_all_bb_betaststat3fU = ifelse(BUYBACK_DATA$CAL_table_undervaluation_undervalued3f$betasstderr,BUYBACK_DATA$CAL_table_undervaluation_undervalued3f$betas/BUYBACK_DATA$CAL_table_undervaluation_undervalued3f$betasstderr,0)
CAL_table_all_bb_betaststat5fU = ifelse(BUYBACK_DATA$CAL_table_undervaluation_undervalued5f$betasstderr,BUYBACK_DATA$CAL_table_undervaluation_undervalued5f$betas/BUYBACK_DATA$CAL_table_undervaluation_undervalued5f$betasstderr,0)
CAL_table_all_bb_betas3fO = BUYBACK_DATA$CAL_table_undervaluation_overvalued3f$betas
CAL_table_all_bb_betas5fO = BUYBACK_DATA$CAL_table_undervaluation_overvalued5f$betas
CAL_table_all_bb_betaststat3fO = ifelse(BUYBACK_DATA$CAL_table_undervaluation_overvalued3f$betasstderr,BUYBACK_DATA$CAL_table_undervaluation_overvalued3f$betas/BUYBACK_DATA$CAL_table_undervaluation_overvalued3f$betasstderr,0)
CAL_table_all_bb_betaststat5fO = ifelse(BUYBACK_DATA$CAL_table_undervaluation_overvalued5f$betasstderr,BUYBACK_DATA$CAL_table_undervaluation_overvalued5f$betas/BUYBACK_DATA$CAL_table_undervaluation_overvalued5f$betasstderr,0)
CAL_table_all_bb_undervaluation = BUYBACK_DATA$CAL_table_undervaluation
BB_cal_table_undertime = BUYBACK_DATA$CAL_table_undervaluation_time
### Issuers
CAL_table_all_iss_betas3f = ISSUERS_DATA$CAL_table_all_3f$betas
CAL_table_all_iss_betas5f = ISSUERS_DATA$CAL_table_all_5f$betas
CAL_table_all_iss_betaststat3f = ifelse(ISSUERS_DATA$CAL_table_all_3f$betasstderr,ISSUERS_DATA$CAL_table_all_3f$betas/ISSUERS_DATA$CAL_table_all_3f$betasstderr,0)
CAL_table_all_iss_betaststat5f = ifelse(ISSUERS_DATA$CAL_table_all_5f$betasstderr,ISSUERS_DATA$CAL_table_all_5f$betas/ISSUERS_DATA$CAL_table_all_5f$betasstderr,0)
CAL_table_all_iss = ISSUERS_DATA$CAL_table_all
ISStable_time_cal = ISSUERS_DATA$CAL_table_time
CAL_table_all_iss_betas3fU = ISSUERS_DATA$CAL_table_undervaluation_undervalued3f$betas
CAL_table_all_iss_betas5fU = ISSUERS_DATA$CAL_table_undervaluation_undervalued5f$betas
CAL_table_all_iss_betaststat3fU = ifelse(ISSUERS_DATA$CAL_table_undervaluation_undervalued3f$betasstderr,ISSUERS_DATA$CAL_table_undervaluation_undervalued3f$betas/ISSUERS_DATA$CAL_table_undervaluation_undervalued3f$betasstderr,0)
CAL_table_all_iss_betaststat5fU = ifelse(ISSUERS_DATA$CAL_table_undervaluation_undervalued5f$betasstderr,ISSUERS_DATA$CAL_table_undervaluation_undervalued5f$betas/ISSUERS_DATA$CAL_table_undervaluation_undervalued5f$betasstderr,0)
CAL_table_all_iss_betas3fO = ISSUERS_DATA$CAL_table_undervaluation_overvalued3f$betas
CAL_table_all_iss_betas5fO = ISSUERS_DATA$CAL_table_undervaluation_overvalued5f$betas
CAL_table_all_iss_betaststat3fO = ifelse(ISSUERS_DATA$CAL_table_undervaluation_overvalued3f$betasstderr,ISSUERS_DATA$CAL_table_undervaluation_overvalued3f$betas/ISSUERS_DATA$CAL_table_undervaluation_overvalued3f$betasstderr,0)
CAL_table_all_iss_betaststat5fO = ifelse(ISSUERS_DATA$CAL_table_undervaluation_overvalued5f$betasstderr,ISSUERS_DATA$CAL_table_undervaluation_overvalued5f$betas/ISSUERS_DATA$CAL_table_undervaluation_overvalued5f$betasstderr,0)
CAL_table_all_iss_undervaluation = ISSUERS_DATA$CAL_table_undervaluation
ISS_cal_table_undertime = ISSUERS_DATA$CAL_table_undervaluation_time

# Sector stuff now
finance_deals_dates_bb = BUYBACK_DATA$finance_deals_dates
finance_IRATS_table_all_bb = BUYBACK_DATA$finance_IRATS_table_all
finance_BBtable_time = BUYBACK_DATA$finance_IRATS_table_time
finance_IRATS_table_all_bb_undervaluation = BUYBACK_DATA$finance_IRATS_table_undervaluation
finance_BB_table_undertime = BUYBACK_DATA$finance_IRATS_table_undervaluation_time
finance_CAL_table_all_bb = BUYBACK_DATA$finance_CAL_table_all
finance_BBtable_time_cal = BUYBACK_DATA$finance_CAL_table_time
finance_CAL_table_all_bb_undervaluation = BUYBACK_DATA$finance_CAL_table_undervaluation
finance_BB_cal_table_undertime = BUYBACK_DATA$finance_CAL_table_undervaluation_time

finance_deals_dates_iss = ISSUERS_DATA$finance_deals_dates
finance_IRATS_table_all_iss = ISSUERS_DATA$finance_IRATS_table_all
finance_ISStable_time = ISSUERS_DATA$finance_IRATS_table_time
finance_IRATS_table_all_iss_undervaluation = ISSUERS_DATA$finance_IRATS_table_undervaluation
finance_ISS_table_undertime = ISSUERS_DATA$finance_IRATS_table_undervaluation_time
finance_CAL_table_all_iss = ISSUERS_DATA$finance_CAL_table_all
finance_ISStable_time_cal = ISSUERS_DATA$finance_CAL_table_time
finance_CAL_table_all_iss_undervaluation = ISSUERS_DATA$finance_CAL_table_undervaluation
finance_ISS_cal_table_undertime = ISSUERS_DATA$finance_CAL_table_undervaluation_time

###

long_all6m_bb = BUYBACK_DATA$pnl_returns_events_all_6M
long_all6m_iss = ISSUERS_DATA$pnl_returns_events_all_6M
long_all6mshort_risk_factors_bb = BUYBACK_DATA$long_all6mshort_risk_factors
long_undervaluedshort_risk_factors_bb = BUYBACK_DATA$long_undervaluedshort_risk_factors
long_overvaluedshort_risk_factors_bb = BUYBACK_DATA$long_overvaluedshort_risk_factors
long_all6mshort_risk_factors_iss = ISSUERS_DATA$long_all6mshort_risk_factors
long_undervaluedshort_risk_factors_iss = ISSUERS_DATA$long_undervaluedshort_risk_factors
long_overvaluedshort_risk_factors_iss = ISSUERS_DATA$long_overvaluedshort_risk_factors

company_subset_undervalued_bb = BUYBACK_DATA$company_subset_undervalued
company_subset_overvalued_bb = BUYBACK_DATA$company_subset_overvalued
company_subset_undervalued_iss = ISSUERS_DATA$company_subset_undervalued
company_subset_overvalued_iss = ISSUERS_DATA$company_subset_overvalued

# Only finance now
finance_long_all6m_bb = BUYBACK_DATA$finance_pnl_returns_events_all_6M
finance_long_all6m_iss = ISSUERS_DATA$finance_pnl_returns_events_all_6M
finance_long_all6mshort_risk_factors_bb = BUYBACK_DATA$finance_long_all6mshort_risk_factors
finance_long_undervaluedshort_risk_factors_bb = BUYBACK_DATA$finance_long_undervaluedshort_risk_factors
finance_long_overvaluedshort_risk_factors_bb = BUYBACK_DATA$finance_long_overvaluedshort_risk_factors
finance_long_all6mshort_risk_factors_iss = ISSUERS_DATA$finance_long_all6mshort_risk_factors
finance_long_undervaluedshort_risk_factors_iss = ISSUERS_DATA$finance_long_undervaluedshort_risk_factors
finance_long_overvaluedshort_risk_factors_iss = ISSUERS_DATA$finance_long_overvaluedshort_risk_factors

###############################################################################################
# HERE WE START THE MAIN REPORT DATA GENERATION CODE
###############################################################################################

#WE NEED TO JUST REMOVE THE FINANCIALS AND UTILITIES FOR THE MAIN PAPER

DATASET = BUYBACK_DATA$DATASET
cleanup_bb = BUYBACK_DATA$DATASET$cleanup
cleanup_bb$finance = DATASET$SDC$Event.Date[which(DATASET$SDC$Event_Industry %in% INDUSTRY_FINANCIALS)]
cleanup_bb$non_finance = DATASET$SDC$Event.Date[which(!(DATASET$SDC$Event_Industry %in% INDUSTRY_FINANCIALS))]
to_remove = which(!(DATASET$SDC$Event_Industry %in% INDUSTRY_USED))
cleanup_bb$industry_removed = length(to_remove)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  DATASET$Abn_returns <- DATASET$Abn_returns[,-to_remove]
  DATASET$Betas_PB6M <- DATASET$Betas_PB6M[,-to_remove]
  DATASET$Dates <- DATASET$Dates[,-to_remove]
  DATASET$DatesMonth <- DATASET$DatesMonth[,-to_remove]
  DATASET$SDC <- DATASET$SDC[-to_remove,]
  for(field in ls(DATASET$compustat_data))  DATASET$compustat_data[[field]] <- DATASET$compustat_data[[field]][-to_remove]
  for(field in ls(DATASET$ibes))  DATASET$ibes[[field]] <- DATASET$ibes[[field]][-to_remove]
  DATASET$returns_by_event <- DATASET$returns_by_event[,-to_remove]
  DATASET$returns_by_event_monthly <- DATASET$returns_by_event_monthly[,-to_remove]  
}
BUYBACK_DATA$DATASET = DATASET

##
DATASET = ISSUERS_DATA$DATASET
cleanup_iss = ISSUERS_DATA$DATASET$cleanup
cleanup_iss$finance = DATASET$SDC$Event.Date[which(DATASET$SDC$Event_Industry %in% INDUSTRY_FINANCIALS)]
cleanup_iss$non_finance = DATASET$SDC$Event.Date[which(!(DATASET$SDC$Event_Industry %in% INDUSTRY_FINANCIALS))]
to_remove = which(!(DATASET$SDC$Event_Industry %in% INDUSTRY_USED))
cleanup_iss$industry_removed = length(to_remove)
if (length(to_remove) > 0){
  # just in alphabetic order not to forget any    
  DATASET$Abn_returns <- DATASET$Abn_returns[,-to_remove]
  DATASET$Betas_PB6M <- DATASET$Betas_PB6M[,-to_remove]
  DATASET$Dates <- DATASET$Dates[,-to_remove]
  DATASET$DatesMonth <- DATASET$DatesMonth[,-to_remove]
  DATASET$SDC <- DATASET$SDC[-to_remove,]
  for(field in ls(DATASET$compustat_data))  DATASET$compustat_data[[field]] <- DATASET$compustat_data[[field]][-to_remove]
  for(field in ls(DATASET$ibes))  DATASET$ibes[[field]] <- DATASET$ibes[[field]][-to_remove]
  DATASET$returns_by_event <- DATASET$returns_by_event[,-to_remove]
  DATASET$returns_by_event_monthly <- DATASET$returns_by_event_monthly[,-to_remove]  
}
ISSUERS_DATA$DATASET = DATASET

# Now get these variables (not before!)
cleanup_bb$ME_is_na = sum(is.na(BUYBACK_DATA$DATASET$SDC$ME_quantile))
cleanup_bb$BEME_is_na = sum(is.na(BUYBACK_DATA$DATASET$SDC$BEME_quantile))
cusip_bb = BUYBACK_DATA$DATASET$SDC$CUSIP
event.date_bb = BUYBACK_DATA$DATASET$SDC$Event.Date
permno_bb = BUYBACK_DATA$DATASET$SDC$permno

cleanup_iss$ME_is_na = sum(is.na(ISSUERS_DATA$DATASET$SDC$ME_quantile))
cleanup_iss$BEME_is_na = sum(is.na(ISSUERS_DATA$DATASET$SDC$BEME_quantile))
cusip_iss = ISSUERS_DATA$DATASET$SDC$CUSIP
event.date_iss = ISSUERS_DATA$DATASET$SDC$Event.Date
permno_iss = ISSUERS_DATA$DATASET$SDC$permno


###############################################################################################
## datasummaryBB datasummaryISS
###############################################################################################

events = BUYBACK_DATA
datasummaryBB = rbind(
  round(c(summary(events$DATASET$SDC$Event.Size[!is.na(events$DATASET$SDC$Event.Size) & events$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(events$DATASET$SDC$Event.Size[!is.na(events$DATASET$SDC$Event.Size) & events$DATASET$SDC$Event.Size!=0]),sum(is.na(events$DATASET$SDC$Event.Size) | events$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(events$DATASET$SDC$Market.Cap[!is.na(events$DATASET$SDC$Market.Cap) & events$DATASET$SDC$Market.Cap!=0])[c(1,3,4,6)], sd(events$DATASET$SDC$Market.Cap[!is.na(events$DATASET$SDC$Market.Cap) & events$DATASET$SDC$Market.Cap!=0]),sum(is.na(events$DATASET$SDC$Market.Cap) | events$DATASET$SDC$Market.Cap==0)),1),
  round(c(summary(events$DATASET$SDC$BE.ME[events$DATASET$SDC$BE.ME < 1e20 & !is.na(events$DATASET$SDC$BE.ME) & events$DATASET$SDC$BE.ME!=0])[c(1,3,4,6)], sd(events$DATASET$SDC$BE.ME[events$DATASET$SDC$BE.ME < 1e20 & !is.na(events$DATASET$SDC$BE.ME) & events$DATASET$SDC$BE.ME!=0]),sum(is.na(events$DATASET$SDC$BE.ME >= 1e20) | events$DATASET$SDC$BE.ME ==0 | events$DATASET$SDC$BE.ME==0)),1))
rownames(datasummaryBB) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryBB)[ncol(datasummaryBB)-1] <- "std"
colnames(datasummaryBB)[ncol(datasummaryBB)] <- "Missing"

events = ISSUERS_DATA
datasummaryISS = rbind(
  round(c(summary(events$DATASET$SDC$Event.Size[!is.na(events$DATASET$SDC$Event.Size) & events$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(events$DATASET$SDC$Event.Size[!is.na(events$DATASET$SDC$Event.Size) & events$DATASET$SDC$Event.Size!=0]),sum(is.na(events$DATASET$SDC$Event.Size) | events$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(events$DATASET$SDC$Market.Cap[!is.na(events$DATASET$SDC$Market.Cap) & events$DATASET$SDC$Market.Cap!=0])[c(1,3,4,6)], sd(events$DATASET$SDC$Market.Cap[!is.na(events$DATASET$SDC$Market.Cap) & events$DATASET$SDC$Market.Cap!=0]),sum(is.na(events$DATASET$SDC$Market.Cap) | events$DATASET$SDC$Market.Cap==0)),1),
  round(c(summary(events$DATASET$SDC$BE.ME[events$DATASET$SDC$BE.ME < 1e20 & !is.na(events$DATASET$SDC$BE.ME) & events$DATASET$SDC$BE.ME!=0])[c(1,3,4,6)], sd(events$DATASET$SDC$BE.ME[events$DATASET$SDC$BE.ME < 1e20 & !is.na(events$DATASET$SDC$BE.ME) & events$DATASET$SDC$BE.ME!=0]),sum(is.na(events$DATASET$SDC$BE.ME >= 1e20) | events$DATASET$SDC$BE.ME ==0 | events$DATASET$SDC$BE.ME==0)),1))
rownames(datasummaryISS) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryISS)[ncol(datasummaryISS)-1] <- "std"
colnames(datasummaryISS)[ncol(datasummaryISS)] <- "Missing"

rm("events")

###############################################################################################
# Events with a different event in the recent past
###############################################################################################
# Add some cross-events features: This code can only be here as it uses info across events

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
BB_beforeISS = which(BUYBACK_DATA$DATASET$SDC$Otherlater != "2100-01-01" & BUYBACK_DATA$DATASET$SDC$Otherlater != "1900-01-01")
BB_not_beforeISS = which(BUYBACK_DATA$DATASET$SDC$Otherlater == "2100-01-01" & BUYBACK_DATA$DATASET$SDC$Otherlater != "1900-01-01")
IRATS_BB_beforeISS = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_beforeISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_beforeISS], Risk_Factors_Monthly)$results
IRATS_BB_not_beforeISS = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_not_beforeISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_not_beforeISS], Risk_Factors_Monthly)$results
CAL_BB_beforeISS = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_beforeISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_beforeISS], Risk_Factors_Monthly)$results
CAL_BB_not_beforeISS = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_not_beforeISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_not_beforeISS], Risk_Factors_Monthly)$results

BUYBACK_DATA$DATASET$SDC$Otherbefore = "2100-01-01"
BUYBACK_DATA$DATASET$SDC$OtherbeforeEvent = 0
for (i in 1:length(BUYBACK_DATA$DATASET$SDC$permno)){
  find_other = which(ISSUERS_DATA$DATASET$SDC$permno == BUYBACK_DATA$DATASET$SDC$permno[i])
  if (length(find_other) > 0){
    otherdate = sort(ISSUERS_DATA$DATASET$SDC$Event.Date[find_other])
    #date_diff = otherdate - BUYBACK_DATA$DATASET$SDC$Event.Date[i]
    date_diff =  BUYBACK_DATA$DATASET$SDC$Event.Date[i] - otherdate
    if (sum(date_diff < years_across_cross_events*365 & date_diff > minCROSS_EVENTS_SINCE_LAST_EVENT)){
      BUYBACK_DATA$DATASET$SDC$Otherbefore[i]  = as.character(otherdate[date_diff < years_across_cross_events*365 & date_diff > 0][1])
      BUYBACK_DATA$DATASET$SDC$OtherbeforeEvent[i] = find_other[which(ISSUERS_DATA$DATASET$SDC$Event.Date[find_other] == BUYBACK_DATA$DATASET$SDC$Otherbefore[i])]
    }
  }
}
BB_afterISS = which(BUYBACK_DATA$DATASET$SDC$Otherbefore != "2100-01-01" & BUYBACK_DATA$DATASET$SDC$Otherbefore != "1900-01-01")
BB_not_afterISS = which(BUYBACK_DATA$DATASET$SDC$Otherbefore == "2100-01-01" & BUYBACK_DATA$DATASET$SDC$Otherbefore != "1900-01-01")
IRATS_BB_afterISS = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_afterISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_afterISS], Risk_Factors_Monthly)$results
IRATS_BB_not_afterISS = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_not_afterISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_not_afterISS], Risk_Factors_Monthly)$results
CAL_BB_afterISS = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_afterISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_afterISS], Risk_Factors_Monthly)$results
CAL_BB_not_afterISS = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_not_afterISS], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_not_afterISS], Risk_Factors_Monthly)$results

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

# First the buybacks with issuance afterwards
# find the related events. 
# AND EXIT AT THE BEGINING OF THE NEXT MONTH (FOR FF TO WORK....)

tmp = exit_helper_rnw(BUYBACK_DATA,"Otherlater",holding_period_pnl = "Four.Years.After")
BB_with_ISS_later = tmp$exit_events
BB_with_noISS_later = tmp$noexit_events
BB_ISS_Exit_Hedged = tmp$pnl_Exit_Hedged
BB_ISS_NoExit_Hedged = tmp$pnl_NoExit_Hedged
BB_with_ISS_later_total = tmp$number_events
Exit_on_SEO_Abn_table = round(cbind(
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_noISS_later], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_ISS_later], Risk_Factors_Monthly)$results
),2)
colnames(Exit_on_SEO_Abn_table) <- c("No SEO: CAR", "t-stat", "p-value","SEO: CAR", "t-stat", "p-value")
rownames(Exit_on_SEO_Abn_table)[nrow(Exit_on_SEO_Abn_table)] <- "Observations"

Exit_on_SEO_Abn_table_cal = round(cbind(
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_noISS_later], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_ISS_later], Risk_Factors_Monthly)$results
),2)
colnames(Exit_on_SEO_Abn_table_cal) <- c("No SEO: CAL", "t-stat", "p-value","SEO: CAL", "t-stat", "p-value")
rownames(Exit_on_SEO_Abn_table_cal)[nrow(Exit_on_SEO_Abn_table_cal)] <- "Observations"
rm("tmp")
# Issuers
tmp = exit_helper_rnw(ISSUERS_DATA,"Otherlater",holding_period_pnl = "Four.Years.After")
ISS_with_BB_later = tmp$exit_events
ISS_with_noBB_later = tmp$noexit_events
ISS_BB_Exit_Hedged = tmp$pnl_Exit_Hedged
ISS_BB_NoExit_Hedged = tmp$pnl_NoExit_Hedged
ISS_with_BB_later_total = tmp$number_events
Exit_on_BB_Abn_table = round(cbind(
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_with_noBB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_with_noBB_later], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_with_BB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_with_BB_later], Risk_Factors_Monthly)$results
),2)
colnames(Exit_on_BB_Abn_table) <- c("No Buyback: CAR", "t-stat", "p-value","Buyback: CAR", "t-stat", "p-value")
rownames(Exit_on_BB_Abn_table)[nrow(Exit_on_BB_Abn_table)] <- "Observations"

Exit_on_BB_Abn_table_cal = round(cbind(
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_with_noBB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_with_noBB_later], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_with_BB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_with_BB_later], Risk_Factors_Monthly)$results
),2)
colnames(Exit_on_BB_Abn_table_cal) <- c("No Buyback: CAL", "t-stat", "p-value","Buyback: CAL", "t-stat", "p-value")
rownames(Exit_on_BB_Abn_table_cal)[nrow(Exit_on_BB_Abn_table_cal)] <- "Observations"
rm("tmp")

# Let's see now the "cross-events" that make the primary event exit:

BB_for_ISS_later = unique(ISSUERS_DATA$DATASET$SDC$OtherlaterEvent[which(ISSUERS_DATA$DATASET$SDC$OtherlaterEvent!=0)])
BB_for_noISS_later = setdiff(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), BB_for_ISS_later)
BB_for_ISS_Abn_table = round(cbind(
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_for_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_for_ISS_later], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_for_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_for_noISS_later], Risk_Factors_Monthly)$results
),2)
colnames(BB_for_ISS_Abn_table) <- c("SEO Buyback: CAR", "t-stat", "p-value","Other Buyback: CAR", "t-stat", "p-value")
rownames(BB_for_ISS_Abn_table)[nrow(BB_for_ISS_Abn_table)] <- "Observations"

ISS_for_BB_later = unique(BUYBACK_DATA$DATASET$SDC$OtherlaterEvent[which(BUYBACK_DATA$DATASET$SDC$OtherlaterEvent!=0)])
ISS_for_noBB_later = setdiff(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), ISS_for_BB_later)
ISS_for_BB_Abn_table = round(cbind(
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_for_BB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_for_BB_later], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_for_noBB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_for_noBB_later], Risk_Factors_Monthly)$results
),2)
colnames(ISS_for_BB_Abn_table) <- c("Buyback SEO: CAR", "t-stat", "p-value","Other SEO: CAR", "t-stat", "p-value")
rownames(ISS_for_BB_Abn_table)[nrow(ISS_for_BB_Abn_table)] <- "Observations"

BB_for_ISS_Abn_table_cal = round(cbind(
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_for_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_for_ISS_later], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_for_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_for_noISS_later], Risk_Factors_Monthly)$results
),2)
colnames(BB_for_ISS_Abn_table_cal) <- c("SEO Buyback: CAR", "t-stat", "p-value","Other Buyback: CAR", "t-stat", "p-value")
rownames(BB_for_ISS_Abn_table_cal)[nrow(BB_for_ISS_Abn_table_cal)] <- "Observations"

ISS_for_BB_Abn_table_cal = round(cbind(
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_for_BB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_for_BB_later], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,ISS_for_noBB_later], ISSUERS_DATA$DATASET$SDC$Event.Date[ISS_for_noBB_later], Risk_Factors_Monthly)$results
),2)
colnames(ISS_for_BB_Abn_table_cal) <- c("Buyback SEO: CAR", "t-stat", "p-value","Other SEO: CAR", "t-stat", "p-value")
rownames(ISS_for_BB_Abn_table_cal)[nrow(ISS_for_BB_Abn_table_cal)] <- "Observations"

###############################################################################################
# Now need to get the CRSP universe data to get the vol and R2 percentile score
###############################################################################################

non_na_mean <- function(x) { mean(x[!is.na(x) & x != 0]) }
non_na_sd <- function(x) { sd(x[!is.na(x) & x != 0]) }
vol_days= floor(5*VOLwindow/7) # NOTE THAT WE USE TRADING DAYS ONLY HERE WHILE VOLwindow IS CALENDAR DAYS

# Get the data

system.time(returns_data <- fread("dataset/universe/crsp_returns.csv", colClasses = c("integer","integer","integer","character","character")))
returns_data$RET <- suppressWarnings(scrub(as.double(gsub(" ", "", returns_data$RET)))) # make sure we don't miss any spaces...
returns_data$date <- as.Date(as.character(returns_data$date),format="%Y%m%d",origin="19700101")
returns_daily= dcast(returns_data, date ~ PERMNO, value.var="RET")  
tmp <-  returns_daily$date
returns_daily$date <- as.numeric(returns_daily$date) # to avoid changing everything into characters
returns_daily = as.matrix(returns_daily)
returns_daily = returns_daily[,-c(which(colnames(returns_daily)=="date"))]
rownames(returns_daily) <-  as.character(tmp)
rm("returns_data")

recent_vol_daily = rolling_variance_nonzero(scrub(returns_daily),vol_days)
# Remove "new" companies 
recent_vol_daily_daysused = apply(scrub(returns_daily)!=0,2, function(r) ms(r,vol_days))
recent_vol_daily = recent_vol_daily*(recent_vol_daily_daysused > 0.7*vol_days) # remove cases with less than X% trading days
rownames(recent_vol_daily) <- rownames(returns_daily)
universe_vol_q1 = recent_vol_daily%-%function(r){
  if (sum(!is.na(r) & r!=0)){
    res = ifelse(!is.na(r) & r!=0, ecdf(r[!is.na(r) & r!=0])(r),NA)
  } else {
    res = r*0
  }
  res
}
rownames(universe_vol_q1) <- rownames(returns_daily)
rm("recent_vol_daily_daysused", "recent_vol_daily")
save(universe_vol_q1, file = "tmpfiles/vol_analysis.Rdata")

###############################################################################################
#<<  Idiosyncratic R2, eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
###############################################################################################

if (0){
  # This is how the R2 is calculated:
  Betas_PB6M <- Betas_lm("Six.Month.Before","One.Day.Before",BUYBACK_DATA$DATASET$Dates, BUYBACK_DATA$DATASET$returns_by_event, Risk_Factors)
  BUYBACK_DATA$DATASET$SDC$ivol <- Betas_PB6M["IVOL",]
  BUYBACK_DATA$DATASET$SDC$marketbeta <- Betas_PB6M["Delta",]
  BUYBACK_DATA$DATASET$SDC$SMBbeta <- Betas_PB6M["SMB",]
  BUYBACK_DATA$DATASET$SDC$HMLbeta <- Betas_PB6M["HML",]
  BUYBACK_DATA$DATASET$SDC$RMWbeta <- Betas_PB6M["RMW",]
  BUYBACK_DATA$DATASET$SDC$CMAbeta <- Betas_PB6M["CMA",]
  BUYBACK_DATA$DATASET$SDC$alpha <- Betas_PB6M["alpha",]
  BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns <- -Betas_PB6M["Rsq",]
}
# Buybacks
# OLD WAY
#BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns = -BUYBACK_DATA$DATASET$SDC$Rsq_returns # Since higher is "low"
#tmp = get_feature_results(BUYBACK_DATA$DATASET,"minus_Rsq_returns", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_R2,R2window)
# NEW WAY:
# universe_vol_q1 is from "tmpfiles/vol_analysis.Rdata" created above
# Get the vol score the one-before-last day of the previous month (note that these scores already use the rolling vol window so we don't need to average them e.g. over all the previous month)
#universe_R2_month <- read.csv("dataset/tmp_files/matrix_R2.csv", header=TRUE, sep=",", dec=".")
if (0){
  universe_R2_month_score = universe_R2_month%-%function(r){
    if (sum(!is.na(r) & r!=0)){
      res = ifelse(!is.na(r) & r!=0, ecdf(r[!is.na(r) & r!=0])(r),NA)
    } else {
      res = r*0
    }
    res
  }
  rownames(universe_R2_month_score) <- rownames(universe_R2_month)
  save(universe_R2_month, universe_R2_month_score, file = "dataset/tmp_files/matrix_R2.Rdata")
}
load("dataset/tmp_files/matrix_R2.Rdata")
universe_R2_month = universe_R2_month_score
#BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns_raw = BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns
BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns <- sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  #universe_R2_month[paste(str_sub(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date[i]), start=1, end=7), "01",sep="-"), as.character(BUYBACK_DATA$DATASET$SDC$permno[i])]
  themonth = paste(str_sub(as.character(AddMonths(BUYBACK_DATA$DATASET$SDC$Event.Date[i],-1)), start=1, end=7), "01",sep="-")
  ifelse(themonth %in% rownames(universe_R2_month), universe_R2_month[themonth, as.character(BUYBACK_DATA$DATASET$SDC$permno[i])], NA)
})
BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns = 1- BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns
tmp = get_feature_results(BUYBACK_DATA$DATASET,"minus_Rsq_returns", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_R2,R2window, method="Simple")

High_Idiosyncr_eventsBB = tmp$High_feature_events; Low_Idiosyncr_eventsBB = tmp$Low_feature_events
High_Idiosyncr_BB  = tmp$High_feature; Low_Idiosyncr_BB = tmp$Low_feature
High_Idiosyncr_BB_Hedged  = tmp$High_feature_Hedged; Low_Idiosyncr_BB_Hedged = tmp$Low_feature_Hedged
High_Idiosyncr_BB48m = tmp$High_feature48m; Low_Idiosyncr_BB48m  = tmp$Low_feature48m
High_Idiosyncr_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_Idiosyncr_BB_Hedged48m = tmp$Low_feature_Hedged48m
R2_IRATStableBB = tmp$feature_IRATStable; R2_IRATStableBB_cal = tmp$feature_IRATStable_cal
R2_IRATStable_underBB = tmp$feature_IRATStable_under; R2_IRATStable_underBB_cal = tmp$feature_IRATStable_under_cal
colnames(R2_IRATStableBB) <- c("Low Idiosync.: CAR", "t-stat","p-value","High Idiosync.: CAR", "t-stat","p-value")
colnames(R2_IRATStable_underBB) <- c("Low Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(R2_IRATStableBB_cal) <- c("Low Idiosync.: CAL", "t-stat","p-value","High Idiosync.: CAL", "t-stat","p-value")
colnames(R2_IRATStable_underBB_cal) <- c("Low Idiosync.: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")
# Issuers
# OLD WAY
#ISSUERS_DATA$DATASET$SDC$minus_Rsq_returns = -ISSUERS_DATA$DATASET$SDC$Rsq_returns # Since higher is "low"
#tmp = get_feature_results(ISSUERS_DATA$DATASET,"minus_Rsq_returns", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_R2,R2window)

# NEW WAY:
# Get the vol score the one-before-last day of the previous month (note that these scores already use the rolling vol window so we don't need to average them e.g. over all the previous month)
ISSUERS_DATA$DATASET$SDC$minus_Rsq_returns <- sapply(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), function(i){
  #universe_R2_month[paste(str_sub(as.character(ISSUERS_DATA$DATASET$SDC$Event.Date[i]), start=1, end=7), "01",sep="-"), as.character(ISSUERS_DATA$DATASET$SDC$permno[i])]
  themonth = paste(str_sub(as.character(AddMonths(ISSUERS_DATA$DATASET$SDC$Event.Date[i],-1)), start=1, end=7), "01",sep="-")
  ifelse(themonth %in% rownames(universe_R2_month), universe_R2_month[themonth, as.character(ISSUERS_DATA$DATASET$SDC$permno[i])], NA)
})
ISSUERS_DATA$DATASET$SDC$minus_Rsq_returns = 1- ISSUERS_DATA$DATASET$SDC$minus_Rsq_returns
tmp = get_feature_results(ISSUERS_DATA$DATASET,"minus_Rsq_returns", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_R2,R2window, method="Simple")

High_Idiosyncr_eventsISS = tmp$High_feature_events; Low_Idiosyncr_eventsISS = tmp$Low_feature_events
High_Idiosyncr_ISS  = tmp$High_feature; Low_Idiosyncr_ISS = tmp$Low_feature
High_Idiosyncr_ISS_Hedged  = tmp$High_feature_Hedged; Low_Idiosyncr_ISS_Hedged = tmp$Low_feature_Hedged
High_Idiosyncr_ISS48m = tmp$High_feature48m; Low_Idiosyncr_ISS48m  = tmp$Low_feature48m
High_Idiosyncr_ISS_Hedged48m  = tmp$High_feature_Hedged48m; Low_Idiosyncr_ISS_Hedged48m = tmp$Low_feature_Hedged48m
R2_IRATStableISS = tmp$feature_IRATStable; R2_IRATStableISS_cal = tmp$feature_IRATStable_cal
R2_IRATStable_underISS = tmp$feature_IRATStable_under; R2_IRATStable_underISS_cal = tmp$feature_IRATStable_under_cal
colnames(R2_IRATStableISS) <- c("Low Idiosync.: CAR", "t-stat","p-value","High Idiosync.: CAR", "t-stat","p-value")
colnames(R2_IRATStable_underISS) <- c("Low Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(R2_IRATStableISS_cal) <- c("Low Idiosync.: CAL", "t-stat","p-value","High Idiosync.: CAL", "t-stat","p-value")
colnames(R2_IRATStable_underISS_cal) <- c("Low Idiosync.: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")
rm("universe_R2_month","universe_R2_month_score")

###############################################################################################
#<<  VOL analysis, eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
# Buybacks

# OLD WAY
#tmp = get_feature_results(BUYBACK_DATA$DATASET,"pre_vol", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_VOL,VOLwindow)
# NEW WAY:
# universe_vol_q1 is from "tmpfiles/vol_analysis.Rdata" created above
# Get the vol score the one-before-last day of the previous month (note that these scores already use the rolling vol window so we don't need to average them e.g. over all the previous month)

universe_vol_q1_bb = universe_vol_q1[,as.character(unique(BUYBACK_DATA$DATASET$SDC$permno))]
month_date = paste(str_sub(rownames(universe_vol_q1_bb),start=1,end=7), "01", sep="-")
universe_vol_q1_bb = scrub(universe_vol_q1_bb)
universe_vol_q1_month = Reduce(rbind,lapply(sort(unique(month_date)), function(monthi){
  apply(universe_vol_q1_bb[month_date == monthi,], 2, function(r){
    ifelse(sum(r!=0) >=2, head(tail(r[r!=0],2),1), 0)
  })
}))
rownames(universe_vol_q1_month) <- sort(unique(month_date))
BUYBACK_DATA$DATASET$SDC$pre_vol_Score <- sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  #universe_vol_q1_month[paste(str_sub(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date[i]), start=1, end=7), "01",sep="-"), as.character(BUYBACK_DATA$DATASET$SDC$permno[i])]
  themonth = paste(str_sub(as.character(AddMonths(BUYBACK_DATA$DATASET$SDC$Event.Date[i],-1)), start=1, end=7), "01",sep="-")
  ifelse(themonth %in% rownames(universe_vol_q1_month), universe_vol_q1_month[themonth, as.character(BUYBACK_DATA$DATASET$SDC$permno[i])], NA)
})
tmp = get_feature_results(BUYBACK_DATA$DATASET,"pre_vol_Score", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_VOL,VOLwindow, method="Simple")
rm("universe_vol_q1_bb","universe_vol_q1_month")

High_VOL_eventsBB = tmp$High_feature_events; Low_VOL_eventsBB = tmp$Low_feature_events
High_VOL_eventsBB = tmp$High_feature_events; Low_VOL_eventsBB = tmp$Low_feature_events
High_VOL_BB  = tmp$High_feature; Low_VOL_BB = tmp$Low_feature
High_VOL_BB_Hedged  = tmp$High_feature_Hedged; Low_VOL_BB_Hedged = tmp$Low_feature_Hedged
High_VOL_BB48m = tmp$High_feature48m; Low_VOL_BB48m  = tmp$Low_feature48m
High_VOL_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_VOL_BB_Hedged48m = tmp$Low_feature_Hedged48m
VOL_IRATStableBB = tmp$feature_IRATStable; VOL_IRATStableBB_cal = tmp$feature_IRATStable_cal
VOL_IRATStable_underBB = tmp$feature_IRATStable_under; VOL_IRATStable_underBB_cal = tmp$feature_IRATStable_under_cal
colnames(VOL_IRATStableBB) <- c("Low Vol: CAR", "t-stat","p-value","High Vol: CAR", "t-stat","p-value")
colnames(VOL_IRATStable_underBB) <- c("Low Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(VOL_IRATStableBB_cal) <- c("Low Vol: CAL", "t-stat","p-value","High Vol: CAL", "t-stat","p-value")
colnames(VOL_IRATStable_underBB_cal) <- c("Low Vol: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High Vol: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")
# Issuers
# OLD WAY:
#tmp = get_feature_results(ISSUERS_DATA$DATASET,"pre_vol", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_VOL,VOLwindow)
# NEW WAY:
universe_vol_q1_iss= universe_vol_q1[,as.character(unique(ISSUERS_DATA$DATASET$SDC$permno))]
month_date = paste(str_sub(rownames(universe_vol_q1_iss),start=1,end=7), "01", sep="-")
universe_vol_q1_iss = scrub(universe_vol_q1_iss)
universe_vol_q1_month = Reduce(rbind,lapply(sort(unique(month_date)), function(monthi){
  apply(universe_vol_q1_iss[month_date == monthi,], 2, function(r){
    ifelse(sum(r!=0) >=2, head(tail(r[r!=0],2),1), 0)
  })
}))
rownames(universe_vol_q1_month) <- sort(unique(month_date))
ISSUERS_DATA$DATASET$SDC$pre_vol_Score <- sapply(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), function(i){
  #universe_vol_q1_month[paste(str_sub(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date[i]), start=1, end=7), "01",sep="-"), as.character(BUYBACK_DATA$DATASET$SDC$permno[i])]
  themonth = paste(str_sub(as.character(AddMonths(ISSUERS_DATA$DATASET$SDC$Event.Date[i],-1)), start=1, end=7), "01",sep="-")
  ifelse(themonth %in% rownames(universe_vol_q1_month), universe_vol_q1_month[themonth, as.character(ISSUERS_DATA$DATASET$SDC$permno[i])], NA)
})
tmp = get_feature_results(ISSUERS_DATA$DATASET,"pre_vol_Score", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_VOL,VOLwindow, method="Simple")
rm("universe_vol_q1_iss","universe_vol_q1_month")

High_VOL_eventsISS = tmp$High_feature_events; Low_VOL_eventsISS = tmp$Low_feature_events
High_VOL_ISS  = tmp$High_feature; Low_VOL_ISS = tmp$Low_feature
High_VOL_ISS_Hedged  = tmp$High_feature_Hedged; Low_VOL_ISS_Hedged = tmp$Low_feature_Hedged
High_VOL_ISS48m = tmp$High_feature48m; Low_VOL_ISS48m  = tmp$Low_feature48m
High_VOL_ISS_Hedged48m  = tmp$High_feature_Hedged48m; Low_VOL_ISS_Hedged48m = tmp$Low_feature_Hedged48m
VOL_IRATStableISS = tmp$feature_IRATStable; VOL_IRATStableISS_cal = tmp$feature_IRATStable_cal
VOL_IRATStable_underISS = tmp$feature_IRATStable_under; VOL_IRATStable_underISS_cal = tmp$feature_IRATStable_under_cal
colnames(VOL_IRATStableISS) <- c("Low Vol: CAR", "t-stat","p-value","High Vol: CAR", "t-stat","p-value")
colnames(VOL_IRATStable_underISS) <- c("Low Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(VOL_IRATStableISS_cal) <- c("Low Vol: CAL", "t-stat","p-value","High Vol: CAL", "t-stat","p-value")
colnames(VOL_IRATStable_underISS_cal) <- c("Low Vol: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High Vol: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")

rm("universe_vol_q1")

# JUST TO REPLACE THE OLD pre_vol AND HIGH/LOW VOL:
BUYBACK_DATA$DATASET$SDC$pre_vol_old <- BUYBACK_DATA$DATASET$SDC$pre_vol
ISSUERS_DATA$DATASET$SDC$pre_vol_old <- ISSUERS_DATA$DATASET$SDC$pre_vol
BUYBACK_DATA$DATASET$SDC$pre_vol <- BUYBACK_DATA$DATASET$SDC$pre_vol_Score
ISSUERS_DATA$DATASET$SDC$pre_vol <- ISSUERS_DATA$DATASET$SDC$pre_vol_Score
VOL_downBB_old = VOL_downBB  
VOL_upBB_old = VOL_upBB 
VOL_downISS_old = VOL_downISS 
VOL_upISS_old = VOL_upISS
thefeature = BUYBACK_DATA$DATASET$SDC$pre_vol; quantile_feature = quantile_VOL
VOL_upBB = quantile(thefeature[!is.na(thefeature)],1-quantile_feature)
VOL_downBB = quantile(thefeature[!is.na(thefeature)],quantile_feature)
thefeature = ISSUERS_DATA$DATASET$SDC$pre_vol; quantile_feature = quantile_VOL
VOL_upISS = quantile(thefeature[!is.na(thefeature)],1-quantile_feature)
VOL_downISS = quantile(thefeature[!is.na(thefeature)],quantile_feature)
####

if (0){  # just some more tests
  permnos_bb= as.character(BUYBACK_DATA$DATASET$SDC$permno)
  relative_quantiles_ma  = apply(scrub(universe_vol_q1_month[,permnos_bb]),2,function(x) ma(x,2*12))
  relative_quantiles  = scrub(universe_vol_q1_month[,permnos_bb])-relative_quantiles_ma
  rownames(relative_quantiles_ma) <- rownames(universe_vol_q1_month)
  rownames(relative_quantiles) <- rownames(universe_vol_q1_month)
  
  BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend <- sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
    #universe_vol_q1_month[paste(str_sub(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date[i]), start=1, end=7), "01",sep="-"), as.character(BUYBACK_DATA$DATASET$SDC$permno[i])]
    themonthnow = paste(str_sub(as.character(AddMonths(BUYBACK_DATA$DATASET$SDC$Event.Date[i],-1)), start=1, end=7), "01",sep="-")
    ifelse(themonthnow %in% rownames(universe_vol_q1_month), 
           universe_vol_q1_month[themonthnow, as.character(BUYBACK_DATA$DATASET$SDC$permno[i])] - relative_quantiles_ma[themonthnow, as.character(BUYBACK_DATA$DATASET$SDC$permno[i])], 
           NA)
  })
  
  useonly = scrub(BUYBACK_DATA$DATASET$SDC$pre_vol_Score) < 3 & !(is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score))
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results[reported_times,]
  useonly = scrub(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend) > quantile(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend[!is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend)],0.8) & !(is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend))
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results[reported_times,]
  useonly = scrub(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend) < quantile(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend[!is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend)],0.2) & !(is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score_trend))
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results[reported_times,]
  
  useonly = scrub(BUYBACK_DATA$DATASET$SDC$pre_vol_Score) > 9 & !(is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score))
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results[reported_times,]
  useonly = scrub(BUYBACK_DATA$DATASET$SDC$pre_vol_Score) > 9 & !(is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score)) & BUYBACK_DATA$DATASET$SDC$Performance_used == 5
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results[reported_times,]
  
  useonly = scrub(BUYBACK_DATA$DATASET$SDC$pre_vol_Score) > 9 & !(is.na(BUYBACK_DATA$DATASET$SDC$pre_vol_Score)) & BUYBACK_DATA$DATASET$SDC$Performance_used == 1
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results[reported_times,]
  
}

###############################################################################################
#<<  Leverage analysis, eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
feature_function_LEV <- function(DATASET){
  res = sapply(1:length(DATASET$SDC$Event.Date), function(i){
    dlc = DATASET$compustat_data$dlc[[i]]
    dltt = DATASET$compustat_data$dltt[[i]]
    debt = dlc + dltt
    debt = ifelse(scrub(debt) >= 0,debt, NA) 
    
    at = DATASET$compustat_data$at[[i]]
    lt = DATASET$compustat_data$lt[[i]]
    
    seq = DATASET$compustat_data$seq[[i]]
    seq = ifelse(scrub(seq) <= 0 & scrub(debt) <= 0, NA, seq) ## a firm with negative debt and negative equity --- give up
    seq= pmax(0, 0.01*scrub(debt), 0.001*scrub(at), scrub(seq) );  ## a firm with negative seq is really a zero debt firm
    
    bd2c= ifelse(scrub(debt + seq) > 0, pmin(debt/(debt + seq), 0.999), NA)
    bl2a= ifelse(scrub(lt + seq) > 0, pmin(lt/(lt+seq), 0.999), NA)
    
    uselev = bl2a
    
    thedates = DATASET$compustat_data$dates[[i]]
    useonly = !is.na(uselev) &  (thedates < DATASET$SDC$Event.Date[i])
    uselev = uselev[useonly]
    thedates = thedates[useonly]
    ifelse(length(uselev) > 0, uselev[tail(sort(as.numeric(thedates), index.return=TRUE)$ix,1)], NA)  
  })
  res[is.na(res)] <- 0.5 # only very few, so ok for now - maybe keep as NA and fix all users of this to handle NA? 
  res
}
BUYBACK_DATA$DATASET$SDC$pre_lev <- feature_function_LEV(BUYBACK_DATA$DATASET)
ISSUERS_DATA$DATASET$SDC$pre_lev <- feature_function_LEV(ISSUERS_DATA$DATASET)
# Buybacks
tmp = get_feature_results(BUYBACK_DATA$DATASET,"pre_lev", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_LEV,LEVwindow)
High_LEV_eventsBB = tmp$High_feature_events; Low_LEV_eventsBB = tmp$Low_feature_events
High_LEV_BB  = tmp$High_feature; Low_LEV_BB = tmp$Low_feature
High_LEV_BB_Hedged  = tmp$High_feature_Hedged; Low_LEV_BB_Hedged = tmp$Low_feature_Hedged
High_LEV_BB48m = tmp$High_feature48m; Low_LEV_BB48m  = tmp$Low_feature48m
High_LEV_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_LEV_BB_Hedged48m = tmp$Low_feature_Hedged48m
LEV_IRATStableBB = tmp$feature_IRATStable; LEV_IRATStableBB_cal = tmp$feature_IRATStable_cal
LEV_IRATStable_underBB = tmp$feature_IRATStable_under; LEV_IRATStable_underBB_cal = tmp$feature_IRATStable_under_cal
colnames(LEV_IRATStableBB) <- c("Low Lev.: CAR", "t-stat","p-value","High Lev.: CAR", "t-stat","p-value")
colnames(LEV_IRATStable_underBB) <- c("Low Lev.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Lev.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(LEV_IRATStableBB_cal) <- c("Low Lev.: CAL", "t-stat","p-value","High Lev.: CAL", "t-stat","p-value")
colnames(LEV_IRATStable_underBB_cal) <- c("Low Lev.: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High Lev.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")
# Issuers
tmp = get_feature_results(ISSUERS_DATA$DATASET,"pre_lev", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_LEV,LEVwindow)
High_LEV_eventsISS = tmp$High_feature_events; Low_LEV_eventsISS = tmp$Low_feature_events
High_LEV_ISS  = tmp$High_feature; Low_LEV_ISS = tmp$Low_feature
High_LEV_ISS_Hedged  = tmp$High_feature_Hedged; Low_LEV_ISS_Hedged = tmp$Low_feature_Hedged
High_LEV_ISS48m = tmp$High_feature48m; Low_LEV_ISS48m  = tmp$Low_feature48m
High_LEV_ISS_Hedged48m  = tmp$High_feature_Hedged48m; Low_LEV_ISS_Hedged48m = tmp$Low_feature_Hedged48m
LEV_IRATStableISS = tmp$feature_IRATStable; LEV_IRATStableISS_cal = tmp$feature_IRATStable_cal
LEV_IRATStable_underISS = tmp$feature_IRATStable_under; LEV_IRATStable_underISS_cal = tmp$feature_IRATStable_under_cal
colnames(LEV_IRATStableISS) <- c("Low Lev.: CAR", "t-stat","p-value","High Lev.: CAR", "t-stat","p-value")
colnames(LEV_IRATStable_underISS) <- c("Low Lev.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Lev.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(LEV_IRATStableISS_cal) <- c("Low Lev.: CAL", "t-stat","p-value","High Lev.: CAL", "t-stat","p-value")
colnames(LEV_IRATStable_underISS_cal) <- c("Low Lev.: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High Lev.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")
###############################################################################################
#<<  EPS uncertainty analysis, eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
feature_function_EPSunc <- function(DATASET,years_used = 4){
  pre_beats = sapply(1:length(DATASET$SDC$Event.Date), function(i){
    alleps_values = DATASET$ibes$eps_value[[i]]
    alleps_forecasts = DATASET$ibes$eps_forecast[[i]]
    res = NA
    if (length(alleps_values) > 0){
      useonly = which(names(alleps_values) < DATASET$SDC$Event.Date[i] 
                      & names(alleps_values) >= DATASET$SDC$Event.Date[i] - years_used*365) 
      res = ifelse(length(useonly)>0, sum(alleps_values[useonly] >= alleps_forecasts[useonly])/length(useonly),NA)
    }
    res
  })
  pre_eps_data= sapply(1:length(DATASET$SDC$Event.Date), function(i){
    alleps_values = DATASET$ibes$eps_value[[i]]
    alleps_forecasts = DATASET$ibes$eps_forecast[[i]]
    res = -1
    if (length(alleps_values) > 0){
      useonly = which(names(alleps_values) < DATASET$SDC$Event.Date[i] 
                      & names(alleps_values) >= DATASET$SDC$Event.Date[i] - years_used*365) 
      res = length(useonly)
    }
    res
  })
  entropy_func <- function(r) ifelse(scrub(r) > 0 & scrub(r) < 1, -r*log(r) - (1-r)*log(1-r), ifelse(!is.na(r),0,NA))
  ifelse(pre_eps_data > 5, entropy_func(pre_beats), NA)
}

# Buybacks
BUYBACK_DATA$DATASET$SDC$EPS_unc <- feature_function_EPSunc(BUYBACK_DATA$DATASET)
tmp = get_feature_results(BUYBACK_DATA$DATASET,"EPS_unc", company_subset_undervalued_bb, company_subset_overvalued_bb, quantile_EPS,EPSwindow)
High_EPS_eventsBB = tmp$High_feature_events; Low_EPS_eventsBB = tmp$Low_feature_events
High_EPS_BB  = tmp$High_feature; Low_EPS_BB = tmp$Low_feature
High_EPS_BB_Hedged  = tmp$High_feature_Hedged; Low_EPS_BB_Hedged = tmp$Low_feature_Hedged
High_EPS_BB48m = tmp$High_feature48m; Low_EPS_BB48m  = tmp$Low_feature48m
High_EPS_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_EPS_BB_Hedged48m = tmp$Low_feature_Hedged48m
EPS_IRATStableBB = tmp$feature_IRATStable; EPS_IRATStableBB_cal = tmp$feature_IRATStable_cal
EPS_IRATStable_underBB = tmp$feature_IRATStable_under; EPS_IRATStable_underBB_cal = tmp$feature_IRATStable_under_cal
colnames(EPS_IRATStableBB) <- c("Low EPS unc.: CAR", "t-stat","p-value","High EPS unc.: CAR", "t-stat","p-value")
colnames(EPS_IRATStable_underBB) <- c("Low EPS unc.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High EPS unc.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(EPS_IRATStableBB_cal) <- c("Low EPS: CAL", "t-stat","p-value","High EPS: CAL", "t-stat","p-value")
colnames(EPS_IRATStable_underBB_cal) <- c("Low EPS unc.: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High EPS unc.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")
# Issuers
ISSUERS_DATA$DATASET$SDC$EPS_unc <- feature_function_EPSunc(ISSUERS_DATA$DATASET)
tmp = get_feature_results(ISSUERS_DATA$DATASET,"EPS_unc", company_subset_undervalued_iss, company_subset_overvalued_iss, quantile_EPS,EPSwindow)
High_EPS_eventsISS = tmp$High_feature_events; Low_EPS_eventsISS = tmp$Low_feature_events
High_EPS_ISS  = tmp$High_feature; Low_EPS_ISS = tmp$Low_feature
High_EPS_ISS_Hedged  = tmp$High_feature_Hedged; Low_EPS_ISS_Hedged = tmp$Low_feature_Hedged
High_EPS_ISS48m = tmp$High_feature48m; Low_EPS_ISS48m  = tmp$Low_feature48m
High_EPS_ISS_Hedged48m  = tmp$High_feature_Hedged48m; Low_EPS_ISS_Hedged48m = tmp$Low_feature_Hedged48m
EPS_IRATStableISS = tmp$feature_IRATStable; EPS_IRATStableISS_cal = tmp$feature_IRATStable_cal
EPS_IRATStable_underISS = tmp$feature_IRATStable_under; EPS_IRATStable_underISS_cal = tmp$feature_IRATStable_under_cal
colnames(EPS_IRATStableISS) <- c("Low EPS unc.: CAR", "t-stat","p-value","High EPS: CAR", "t-stat","p-value")
colnames(EPS_IRATStable_underISS) <- c("Low EPS unc.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High EPS unc.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(EPS_IRATStableISS_cal) <- c("Low EPS unc.: CAL", "t-stat","p-value","High EPS unc.: CAL", "t-stat","p-value")
colnames(EPS_IRATStable_underISS_cal) <- c("Low EPS unc.: U CAL", "t-stat","p-value","O CAR", "t-stat","p-value", "High EPS: U unc. CAL", "t-stat","p-value","O CAL", "t-stat","p-value")
rm("tmp")

pre_eps_uncertainty = Reduce(rbind,lapply(0:9, function(i){
  pre_beats = BUYBACK_DATA$DATASET$SDC$EPS_unc
  useonly = !is.na(pre_beats) & scrub(pre_beats) >= quantile(pre_beats[!is.na(pre_beats)],0.1*i) & pre_beats <= quantile(pre_beats[!is.na(pre_beats)],0.1*(i+1))
  x = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  x["+48",]  
}))

###############################################################################################
###############################################################################################
#<<  Randomization analysis, eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
###############################################################################################
###############################################################################################
# Define first a helper function used for the randomation method (HENCE WE CAN CHANGE THIS AS NEEDED)

get_feature_results_randomize <- function(DATASET,feature_function,quantile_feature,featurewindow,randomization_iterations){
  DATASET$SDC$Event.Date.Orig <- DATASET$SDC$Event.Date
  
  lapply(1:randomization_iterations, function(randomiter){     
    cat(randomiter,",")
    all_dates = rownames(DATASET$returns_by_event)
    tmp = DATASET$SDC$Event.Date.Orig 
    for (i in 1:length(DATASET$SDC$Event.Date)){
      useonly = all_dates[DATASET$returns_by_event[,i]!=0]
      if (length(useonly) > 3000) { # just to require we have enough data for this company 
        useonly = useonly[1000:(length(useonly)-1000)] # just to avoid delists etc
        res = useonly[sample(1:length(useonly),1)]
      } else {
        res = useonly[sample(10:length(useonly),1)]
      }
      tmp[i]= as.Date(res,"%Y-%m-%d")
    }
    DATASET$SDC$Event.Date <- tmp
    
    # Now get the new feature for these random dates:
    DATASET$Dates <- create_dates(as.Date(DATASET$SDC$Event.Date)) 
    colnames(DATASET$Dates) <- DATASET$SDC$permno
    ordered_events = sort(as.numeric(DATASET$SDC$Event.Date),index.return = T)$ix
    
    DATASET$SDC$rand_feat <- feature_function(DATASET)
    
    # Now just do whatever we usually do for the feature results - whatever we need to get
    # Thresholds
    thetimes = DATASET$SDC$Event.Date
    thefeature = DATASET$SDC$rand_feat    
    thres = Reduce(cbind,lapply(1:length(thetimes), function(i){
      useonly = thetimes <= thetimes[i] & thetimes >= thetimes[i] - featurewindow & 1:length(thetimes) != i
      useonly = useonly & !is.na(thefeature)
      res = c(0,0)
      if (length(thefeature[useonly]) >= 5)
        res = c(quantile(thefeature[useonly], quantile_feature),quantile(thefeature[useonly], 1-quantile_feature))
      res
    }))
    feature_down = thres[1,]
    feature_up = thres[2,]
    names(feature_down) <- format(DATASET$SDC$Event.Date, "%Y%m")
    names(feature_up) <- format(DATASET$SDC$Event.Date, "%Y%m")
    tmp = feature_down
    feature_thresholds_down = sapply(sort(unique(names(tmp))), function(i) {
      mean(tmp[(names(tmp) == i)])
    })
    names(feature_thresholds_down)<- sort(unique(names(tmp)))
    tmp = feature_up
    feature_thresholds_up = sapply(sort(unique(names(tmp))), function(i) {
      mean(tmp[(names(tmp) == i)])
    })
    names(feature_thresholds_up)<- sort(unique(names(tmp)))
    # Classes
    Low_feature_events = which(scrub(thefeature) < feature_down & !is.na(thefeature) & feature_down !=0)
    High_feature_events = which(scrub(thefeature) > feature_up & !is.na(thefeature)  & feature_up != 0 )
    # the results
    cbind(
      car_table(DATASET$returns_by_event_monthly[,Low_feature_events], DATASET$SDC$Event.Date[Low_feature_events], Risk_Factors_Monthly)$results,
      car_table(DATASET$returns_by_event_monthly[,High_feature_events], DATASET$SDC$Event.Date[High_feature_events], Risk_Factors_Monthly)$results,
      calendar_table(DATASET$returns_by_event_monthly[,Low_feature_events], DATASET$SDC$Event.Date[Low_feature_events], Risk_Factors_Monthly)$results,
      calendar_table(DATASET$returns_by_event_monthly[,High_feature_events], DATASET$SDC$Event.Date[High_feature_events], Risk_Factors_Monthly)$results
    )
  })
}

###############################################################################################
#<<  R2 Randomization , eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
##### WE SHOULD UPDATE THIS WITH THE NEW R2 METHOD (FOR NOW WE DON"T NEED THIS...) ####################
feature_function_R2 <- function(DATASET){
  Betas_PB6M <- Betas_lm("Six.Month.Before","One.Day.Before",DATASET$Dates, DATASET$returns_by_event, Risk_Factors)
  ifelse(!is.na(Betas_PB6M["Rsq",]), Betas_PB6M["Rsq",], NA)
}
cat("\nR2 randomization...")
R2_randomization = get_feature_results_randomize(BUYBACK_DATA$DATASET,feature_function_R2,quantile_R2,R2window,randomization_iterations = randomization_iterations)
min_random_lowR2irat = which.min(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",1]))
max_random_lowR2irat = which.max(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",1]))
min_random_highR2irat = which.min(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",4]))
max_random_highR2irat = which.max(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",4]))
min_random_lowR2cal = which.min(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",7]))
max_random_lowR2cal = which.max(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",7]))
min_random_highR2cal = which.min(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",10]))
max_random_highR2cal = which.max(sapply(1:length(R2_randomization), function(i)R2_randomization[[i]]["+48",10]))
R2_IRATStableBB_random_min = cbind(
  R2_randomization[[min_random_lowR2irat]][,1:3],
  R2_randomization[[min_random_highR2irat]][,4:6],
  R2_randomization[[min_random_lowR2cal]][,7:9],
  R2_randomization[[min_random_highR2cal]][,10:12]
)
R2_IRATStableBB_random_max = cbind(
  R2_randomization[[max_random_lowR2irat]][,1:3],
  R2_randomization[[max_random_highR2irat]][,4:6],
  R2_randomization[[max_random_lowR2cal]][,7:9],
  R2_randomization[[max_random_highR2cal]][,10:12]
)
###############################################################################################
#<<  VOL Randomization , eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
##### WE SHOULD UPDATE THIS WITH THE NEW VOL METHOD (FOR NOW WE DON"T NEED THIS...) ####################
feature_function_VOL <- function(DATASET) -VOL_lm("Six.Month.Before","One.Day.Before",DATASET$Dates, DATASET$returns_by_event)
cat("\nVol randomization...")
VOL_randomization = get_feature_results_randomize(BUYBACK_DATA$DATASET,feature_function_VOL,quantile_VOL,VOLwindow,randomization_iterations = randomization_iterations)
min_random_lowvolirat = which.min(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",1]))
max_random_lowvolirat = which.max(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",1]))
min_random_highvolirat = which.min(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",4]))
max_random_highvolirat = which.max(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",4]))
min_random_lowvolcal = which.min(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",7]))
max_random_lowvolcal = which.max(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",7]))
min_random_highvolcal = which.min(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",10]))
max_random_highvolcal = which.max(sapply(1:length(VOL_randomization), function(i)VOL_randomization[[i]]["+48",10]))
VOL_IRATStableBB_random_min = cbind(
  VOL_randomization[[min_random_lowvolirat]][,1:3],
  VOL_randomization[[min_random_highvolirat]][,4:6],
  VOL_randomization[[min_random_lowvolcal]][,7:9],
  VOL_randomization[[min_random_highvolcal]][,10:12]
)
VOL_IRATStableBB_random_max = cbind(
  VOL_randomization[[max_random_lowvolirat]][,1:3],
  VOL_randomization[[max_random_highvolirat]][,4:6],
  VOL_randomization[[max_random_lowvolcal]][,7:9],
  VOL_randomization[[max_random_highvolcal]][,10:12]
)
###############################################################################################
#<<  LEV Randomization , eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
cat("\nLev randomization...")
LEV_randomization = get_feature_results_randomize(BUYBACK_DATA$DATASET,feature_function_LEV,quantile_LEV,LEVwindow,randomization_iterations = randomization_iterations)
min_random_lowLEVirat = which.min(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",1]))
max_random_lowLEVirat = which.max(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",1]))
min_random_highLEVirat = which.min(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",4]))
max_random_highLEVirat = which.max(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",4]))
min_random_lowLEVcal = which.min(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",7]))
max_random_lowLEVcal = which.max(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",7]))
min_random_highLEVcal = which.min(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",10]))
max_random_highLEVcal = which.max(sapply(1:length(LEV_randomization), function(i)LEV_randomization[[i]]["+48",10]))
LEV_IRATStableBB_random_min = cbind(
  LEV_randomization[[min_random_lowLEVirat]][,1:3],
  LEV_randomization[[min_random_highLEVirat]][,4:6],
  LEV_randomization[[min_random_lowLEVcal]][,7:9],
  LEV_randomization[[min_random_highLEVcal]][,10:12]
)
LEV_IRATStableBB_random_max = cbind(
  LEV_randomization[[max_random_lowLEVirat]][,1:3],
  LEV_randomization[[max_random_highLEVirat]][,4:6],
  LEV_randomization[[max_random_lowLEVcal]][,7:9],
  LEV_randomization[[max_random_highLEVcal]][,10:12]
)
###############################################################################################
#<<  EPSunc Randomization , eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
cat("\nEPSunc randomization...")
EPSunc_randomization = get_feature_results_randomize(BUYBACK_DATA$DATASET,feature_function_EPSunc,quantile_EPS,EPSwindow,randomization_iterations = randomization_iterations)
min_random_lowEPSuncirat = which.min(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",1]))
max_random_lowEPSuncirat = which.max(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",1]))
min_random_highEPSuncirat = which.min(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",4]))
max_random_highEPSuncirat = which.max(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",4]))
min_random_lowEPSunccal = which.min(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",7]))
max_random_lowEPSunccal = which.max(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",7]))
min_random_highEPSunccal = which.min(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",10]))
max_random_highEPSunccal = which.max(sapply(1:length(EPSunc_randomization), function(i)EPSunc_randomization[[i]]["+48",10]))
EPSunc_IRATStableBB_random_min = cbind(
  EPSunc_randomization[[min_random_lowEPSuncirat]][,1:3],
  EPSunc_randomization[[min_random_highEPSuncirat]][,4:6],
  EPSunc_randomization[[min_random_lowEPSunccal]][,7:9],
  EPSunc_randomization[[min_random_highEPSunccal]][,10:12]
)
EPSunc_IRATStableBB_random_max = cbind(
  EPSunc_randomization[[max_random_lowEPSuncirat]][,1:3],
  EPSunc_randomization[[max_random_highEPSuncirat]][,4:6],
  EPSunc_randomization[[max_random_lowEPSunccal]][,7:9],
  EPSunc_randomization[[max_random_highEPSunccal]][,10:12]
)

get_feature_results_randomize_EPSunc <- function(DATASET,randomization_iterations = randomization_iterations){  
  DATASET$SDC$Event.Date.Orig <- DATASET$SDC$Event.Date    
  lapply(1:randomization_iterations, function(randomiter){     
    cat(randomiter,",")
    all_dates = rownames(DATASET$returns_by_event)
    tmp = DATASET$SDC$Event.Date.Orig 
    for (i in 1:length(DATASET$SDC$Event.Date)){
      useonly = all_dates[DATASET$returns_by_event[,i]!=0]
      if (length(useonly) > 3000) { # just to require we have enough data for this company 
        useonly = useonly[1000:(length(useonly)-1000)] # just to avoid delists etc
        res = useonly[sample(1:length(useonly),1)]
      } else {
        res = useonly[sample(10:length(useonly),1)]
      }
      tmp[i]= as.Date(res,"%Y-%m-%d")
    }
    DATASET$SDC$Event.Date <- tmp
    # Now the EPS uncertainty result:
    pre_beats <- feature_function_EPSunc(DATASET)
    pre_eps_uncertainty = #Reduce(rbind,
      lapply(0:9, function(i){
        useonly = !is.na(pre_beats) & scrub(pre_beats) >= quantile(pre_beats[!is.na(pre_beats)],0.1*i) & pre_beats <= quantile(pre_beats[!is.na(pre_beats)],0.1*(i+1))
        x = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
        x#["+48",]  
      })
    #)
    pre_eps_uncertainty
  }
  )
}
#EPSunc_randomization2 <- get_feature_results_randomize_EPSunc(BUYBACK_DATA$DATASET,randomization_iterations = randomization_iterations)


###############################################################################################
###############################################################################################
#<< All industries analysis, eval = TRUE, echo=FALSE,message=FALSE,fig.pos='h',results='asis' >>=
###############################################################################################
###############################################################################################

Event_Industries <- suppressWarnings(scrub(as.numeric(BUYBACK_DATA$DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
Event_Industries<- sapply(Event_Industries, function(i){
  x=as.numeric(i)
  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
})
industry_tableBB = sort(table(Event_Industries), decreasing = TRUE)
industry_tableBB=industry_tableBB[-which(names(industry_tableBB)=="Strange")]
names(industry_tableBB)<- gsub("_"," ",names(industry_tableBB))

industry_BB <- lapply(which(industry_tableBB > min_industry_sampleBB), function(i){
  this_industry = which( !(Event_Industries == gsub(" ","_", names(industry_tableBB)[i])) ) # NOTICE THE "!"
  if (length(this_industry) != length(BUYBACK_DATA$DATASET$SDC$Industry) -industry_tableBB[i])
    stop("\nFunny industry problem\n")
  
  cbind(
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,this_industry], BUYBACK_DATA$DATASET$SDC$Event.Date[this_industry], Risk_Factors_Monthly)$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,this_industry], BUYBACK_DATA$DATASET$SDC$Event.Date[this_industry], Risk_Factors_Monthly)$results
  )
})

industry_BB_features <- Reduce(rbind,lapply(which(industry_tableBB > 100), function(i){
  this_industry = which(Event_Industries == gsub(" ","_", names(industry_tableBB)[i]))
  if (length(this_industry) != industry_tableBB[i])
    stop("\nFunny industry problem\n")
  
  res = matrix(
    c(
      100*length(intersect(this_industry,High_Idiosyncr_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,Low_Idiosyncr_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,High_VOL_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,Low_VOL_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,High_LEV_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,Low_LEV_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,High_EPS_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,Low_EPS_eventsBB))/(length(this_industry)),
      100*length(intersect(this_industry,which(company_subset_undervalued_bb)))/(length(this_industry)),
      100*length(intersect(this_industry,which(company_subset_overvalued_bb)))/(length(this_industry))
      
    )
    , nrow=1)
  colnames(res) <- c("H Idsync.","L Idsync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.","U/valued","O/valued")
  rownames(res)<-names(industry_tableBB)[i]
  res
}))

Event_Industries <- suppressWarnings(scrub(as.numeric(ISSUERS_DATA$DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
Event_Industries<- sapply(Event_Industries, function(i){
  x=as.numeric(i)
  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
})
industry_tableISS = sort(table(Event_Industries), decreasing = TRUE)
industry_tableISS=industry_tableISS[-which(names(industry_tableISS)=="Strange")]
names(industry_tableISS)<- gsub("_"," ",names(industry_tableISS))

rm("Event_Industries")

###############################################################################################
## Crisis 
###############################################################################################

crisis_events_BB = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(i) is.crisis(i,BEAR_YEARS, CRISIS_SLACK_pre,CRISIS_SLACK_post))
crisisBB = sum(crisis_events_BB)
nocrisisBB = sum(!crisis_events_BB)
IRATS_regimeBB  <- round(cbind(
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(!(crisis_events_BB))], BUYBACK_DATA$DATASET$SDC$Event.Date[which(!(crisis_events_BB))], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeBB) <- c("Crisis: CAR", "t-stat","p-value","Not Crisis: CAR", "t-stat","p-value")
rownames(IRATS_regimeBB)[nrow(IRATS_regimeBB)] <- "Observations"

crisis_events_ISS = sapply(ISSUERS_DATA$DATASET$SDC$Event.Date, function(i) is.crisis(i,BEAR_YEARS, CRISIS_SLACK_pre,CRISIS_SLACK_post))
crisisISS = sum(crisis_events_ISS)
nocrisisISS = sum(!crisis_events_ISS)
IRATS_regimeISS  <- round(cbind(
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS)], ISSUERS_DATA$DATASET$SDC$Event.Date[which((crisis_events_ISS))], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(!(crisis_events_ISS))], ISSUERS_DATA$DATASET$SDC$Event.Date[which(!(crisis_events_ISS))], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeISS) <- c("Crisis: CAR", "t-stat","p-value","Not Crisis: CAR", "t-stat","p-value")
rownames(IRATS_regimeISS)[nrow(IRATS_regimeISS)] <- "Observations"

#calendar
crisis_events_BB = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(i) is.crisis(i,BEAR_YEARS, CRISIS_SLACK_pre,CRISIS_SLACK_post))
crisisBB = sum(crisis_events_BB)
nocrisisBB = sum(!crisis_events_BB)
IRATS_regimeBB_cal  <- round(cbind(
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(!(crisis_events_BB))], BUYBACK_DATA$DATASET$SDC$Event.Date[which(!(crisis_events_BB))], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeBB_cal) <- c("Crisis: CAL", "t-stat","p-value","Not Crisis: CAL", "t-stat","p-value")
rownames(IRATS_regimeBB_cal)[nrow(IRATS_regimeBB_cal)] <- "Observations"

crisis_events_ISS = sapply(ISSUERS_DATA$DATASET$SDC$Event.Date, function(i) is.crisis(i,BEAR_YEARS, CRISIS_SLACK_pre,CRISIS_SLACK_post))
crisisISS = sum(crisis_events_ISS)
nocrisisISS = sum(!crisis_events_ISS)
IRATS_regimeISS_cal  <- round(cbind(
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS)], ISSUERS_DATA$DATASET$SDC$Event.Date[which((crisis_events_ISS))], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(!(crisis_events_ISS))], ISSUERS_DATA$DATASET$SDC$Event.Date[which(!(crisis_events_ISS))], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeISS_cal) <- c("Crisis: CAL", "t-stat","p-value","Not Crisis: CAL", "t-stat","p-value")
rownames(IRATS_regimeISS_cal)[nrow(IRATS_regimeISS_cal)] <- "Observations"


########################################################################################
### Idiosyncratic risk classification and regimes
########################################################################################

# buybacks
crisis_events_BB_High_Idiosync = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns < R2_downBB
crisis_events_BB_Low_Idiosync = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns > R2_upBB
nocrisis_events_BB_High_Idiosync = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns < R2_downBB
nocrisis_events_BB_Low_Idiosync = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns > R2_upBB

IRATS_regimeBB_R2  <- round(cbind(
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_Low_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_Low_Idiosync)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_High_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_High_Idiosync)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_Low_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_Low_Idiosync)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_High_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_High_Idiosync)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeBB_R2) <- c("Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value","Not Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value")
rownames(IRATS_regimeBB_R2)[nrow(IRATS_regimeBB_R2)] <- "Observations"

# issuers
crisis_events_ISS_High_Idiosync = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns < R2_downISS
crisis_events_ISS_Low_Idiosync = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns > R2_upISS
nocrisis_events_ISS_High_Idiosync = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns < R2_downISS
nocrisis_events_ISS_Low_Idiosync = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns > R2_upISS

IRATS_regimeISS_R2  <- round(cbind(
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_Low_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_Low_Idiosync)], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_High_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_High_Idiosync)], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_Low_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_Low_Idiosync)], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_High_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_High_Idiosync)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeISS_R2) <- c("Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value","Not Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value")
rownames(IRATS_regimeISS_R2)[nrow(IRATS_regimeISS_R2)] <- "Observations"

#calendar
# buybacks
crisis_events_BB_High_Idiosync = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns < R2_downBB
crisis_events_BB_Low_Idiosync = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns > R2_upBB
nocrisis_events_BB_High_Idiosync = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns < R2_downBB
nocrisis_events_BB_Low_Idiosync = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$Rsq_returns > R2_upBB

IRATS_regimeBB_R2_cal  <- round(cbind(
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_Low_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_Low_Idiosync)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_High_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_High_Idiosync)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_Low_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_Low_Idiosync)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_High_Idiosync)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_High_Idiosync)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeBB_R2_cal) <- c("Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value","Not Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value")
rownames(IRATS_regimeBB_R2_cal)[nrow(IRATS_regimeBB_R2_cal)] <- "Observations"

# issuers
crisis_events_ISS_High_Idiosync = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns < R2_downISS
crisis_events_ISS_Low_Idiosync = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns > R2_upISS
nocrisis_events_ISS_High_Idiosync = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns < R2_downISS
nocrisis_events_ISS_Low_Idiosync = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$Rsq_returns > R2_upISS

IRATS_regimeISS_R2_cal  <- round(cbind(
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_Low_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_Low_Idiosync)], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_High_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_High_Idiosync)], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_Low_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_Low_Idiosync)], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_High_Idiosync)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_High_Idiosync)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeISS_R2_cal) <- c("Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value","Not Crisis: Low Idiosync.", "t-stat","p-value","High Idiosync.","t-stat","p-value")
rownames(IRATS_regimeISS_R2_cal)[nrow(IRATS_regimeISS_R2_cal)] <- "Observations"


pnl_returns_events_highIbb <- apply(PNL_matrix_BB(start_date_event,end_date_event,High_Idiosyncr_eventsBB, BUYBACK_DATA$DATASET$DatesMonth,BUYBACK_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
pnl_returns_events_lowIbb <- apply(PNL_matrix_BB(start_date_event,end_date_event,Low_Idiosyncr_eventsBB, BUYBACK_DATA$DATASET$DatesMonth,BUYBACK_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
long_highI_short_risk_factors_BB = suppressWarnings(scrub(alpha_lm(pnl_returns_events_highIbb,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))
long_lowI_short_risk_factors_BB = suppressWarnings(scrub(alpha_lm(pnl_returns_events_lowIbb,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))

pnl_returns_events_highIiss <- apply(PNL_matrix_BB(start_date_event,end_date_event,High_Idiosyncr_eventsISS, ISSUERS_DATA$DATASET$DatesMonth,ISSUERS_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
pnl_returns_events_lowIiss <- apply(PNL_matrix_BB(start_date_event,end_date_event,Low_Idiosyncr_eventsISS, ISSUERS_DATA$DATASET$DatesMonth,ISSUERS_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
long_highI_short_risk_factors_ISS = suppressWarnings(scrub(alpha_lm(pnl_returns_events_highIiss,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))
long_lowI_short_risk_factors_ISS = suppressWarnings(scrub(alpha_lm(pnl_returns_events_lowIiss,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))


########################################################################################
### Volatility classification and regimes
########################################################################################

# buybacks
crisis_events_BB_Low_VOL = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol < VOL_downBB
crisis_events_BB_High_VOL = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol > VOL_upBB
nocrisis_events_BB_Low_VOL = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol < VOL_downBB
nocrisis_events_BB_High_VOL = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol > VOL_upBB

IRATS_regimeBB_VOL  <- round(cbind(
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_Low_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_Low_VOL)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_High_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_High_VOL)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_Low_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_Low_VOL)], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_High_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_High_VOL)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeBB_VOL) <- c("Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value","Not Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value")
rownames(IRATS_regimeBB_VOL)[nrow(IRATS_regimeBB_VOL)] <- "Observations"

# issuers
crisis_events_ISS_High_VOL = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol < VOL_downISS
crisis_events_ISS_Low_VOL = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol > VOL_upISS
nocrisis_events_ISS_High_VOL = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol < VOL_downISS
nocrisis_events_ISS_Low_VOL = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol > VOL_upISS

IRATS_regimeISS_VOL  <- round(cbind(
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_Low_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_Low_VOL)], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_High_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_High_VOL)], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_Low_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_Low_VOL)], Risk_Factors_Monthly)$results,
  car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_High_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_High_VOL)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeISS_VOL) <- c("Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value","Not Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value")
rownames(IRATS_regimeISS_VOL)[nrow(IRATS_regimeISS_VOL)] <- "Observations"

#calendar
# buybacks
crisis_events_BB_Low_VOL = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol < VOL_downBB
crisis_events_BB_High_VOL = crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol > VOL_upBB
nocrisis_events_BB_Low_VOL = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol < VOL_downBB
nocrisis_events_BB_High_VOL = !crisis_events_BB & BUYBACK_DATA$DATASET$SDC$pre_vol > VOL_upBB

IRATS_regimeBB_VOL_cal  <- round(cbind(
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_Low_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_Low_VOL)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_BB_High_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(crisis_events_BB_High_VOL)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_Low_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_Low_VOL)], Risk_Factors_Monthly)$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_BB_High_VOL)], BUYBACK_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_BB_High_VOL)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeBB_VOL_cal) <- c("Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value","Not Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value")
rownames(IRATS_regimeBB_VOL_cal)[nrow(IRATS_regimeBB_VOL_cal)] <- "Observations"

# issuers
crisis_events_ISS_High_VOL = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol < VOL_downISS
crisis_events_ISS_Low_VOL = crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol > VOL_upISS
nocrisis_events_ISS_High_VOL = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol < VOL_downISS
nocrisis_events_ISS_Low_VOL = !crisis_events_ISS & ISSUERS_DATA$DATASET$SDC$pre_vol > VOL_upISS

IRATS_regimeISS_VOL_cal  <- round(cbind(
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_Low_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_Low_VOL)], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(crisis_events_ISS_High_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(crisis_events_ISS_High_VOL)], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_Low_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_Low_VOL)], Risk_Factors_Monthly)$results,
  calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,which(nocrisis_events_ISS_High_VOL)], ISSUERS_DATA$DATASET$SDC$Event.Date[which(nocrisis_events_ISS_High_VOL)], Risk_Factors_Monthly)$results
),2)
colnames(IRATS_regimeISS_VOL_cal) <- c("Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value","Not Crisis: Low VOL.", "t-stat","p-value","High VOL.","t-stat","p-value")
rownames(IRATS_regimeISS_VOL_cal)[nrow(IRATS_regimeISS_VOL_cal)] <- "Observations"


pnl_returns_events_highVbb <- apply(PNL_matrix_BB(start_date_event,end_date_event,High_VOL_eventsBB, BUYBACK_DATA$DATASET$DatesMonth,BUYBACK_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
pnl_returns_events_lowVbb <- apply(PNL_matrix_BB(start_date_event,end_date_event,Low_VOL_eventsBB, BUYBACK_DATA$DATASET$DatesMonth,BUYBACK_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
long_highV_short_risk_factors_BB = suppressWarnings(scrub(alpha_lm(pnl_returns_events_highVbb,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))
long_lowV_short_risk_factors_BB = suppressWarnings(scrub(alpha_lm(pnl_returns_events_lowVbb,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))

pnl_returns_events_highViss <- apply(PNL_matrix_BB(start_date_event,end_date_event,High_VOL_eventsISS, ISSUERS_DATA$DATASET$DatesMonth,ISSUERS_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
pnl_returns_events_lowViss <- apply(PNL_matrix_BB(start_date_event,end_date_event,Low_VOL_eventsISS, ISSUERS_DATA$DATASET$DatesMonth,ISSUERS_DATA$DATASET$returns_by_event_monthly, event=1),1,non_zero_mean)
long_highV_short_risk_factors_ISS = suppressWarnings(scrub(alpha_lm(pnl_returns_events_highViss,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))
long_lowV_short_risk_factors_ISS = suppressWarnings(scrub(alpha_lm(pnl_returns_events_lowViss,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1)))

#####################################################################################################################
#####################################################################################################################
# Cross Relations
#####################################################################################################################
#####################################################################################################################


Under_IdioBB = rbind(
  c(100*length(intersect(which(company_subset_undervalued_bb),High_Idiosyncr_eventsBB ))/length(which(company_subset_undervalued_bb)), 100*length(intersect(which(company_subset_undervalued_bb),Low_Idiosyncr_eventsBB ))/length(which(company_subset_undervalued_bb)),
    100*length(intersect(which(company_subset_undervalued_bb),High_VOL_eventsBB ))/length(which(company_subset_undervalued_bb)), 100*length(intersect(which(company_subset_undervalued_bb),Low_VOL_eventsBB ))/length(which(company_subset_undervalued_bb)),
    100*length(intersect(which(company_subset_undervalued_bb),High_LEV_eventsBB ))/length(which(company_subset_undervalued_bb)), 100*length(intersect(which(company_subset_undervalued_bb),Low_LEV_eventsBB ))/length(which(company_subset_undervalued_bb)),
    100*length(intersect(which(company_subset_undervalued_bb),High_EPS_eventsBB ))/length(which(company_subset_undervalued_bb)), 100*length(intersect(which(company_subset_undervalued_bb),Low_EPS_eventsBB ))/length(which(company_subset_undervalued_bb))
  ),
  c(100*length(intersect(which(company_subset_overvalued_bb),High_Idiosyncr_eventsBB ))/length(which(company_subset_overvalued_bb)), 100*length(intersect(which(company_subset_overvalued_bb),Low_Idiosyncr_eventsBB ))/length(which(company_subset_overvalued_bb)),
    100*length(intersect(which(company_subset_overvalued_bb),High_VOL_eventsBB ))/length(which(company_subset_overvalued_bb)), 100*length(intersect(which(company_subset_overvalued_bb),Low_VOL_eventsBB ))/length(which(company_subset_overvalued_bb)),
    100*length(intersect(which(company_subset_overvalued_bb),High_LEV_eventsBB ))/length(which(company_subset_overvalued_bb)), 100*length(intersect(which(company_subset_overvalued_bb),Low_LEV_eventsBB ))/length(which(company_subset_overvalued_bb)),
    100*length(intersect(which(company_subset_overvalued_bb),High_EPS_eventsBB ))/length(which(company_subset_overvalued_bb)), 100*length(intersect(which(company_subset_overvalued_bb),Low_EPS_eventsBB ))/length(which(company_subset_overvalued_bb))
  ),
  ###
  c(100*length(intersect(High_Idiosyncr_eventsBB,High_Idiosyncr_eventsBB ))/length(High_Idiosyncr_eventsBB), 100*length(intersect(High_Idiosyncr_eventsBB,Low_Idiosyncr_eventsBB ))/length(High_Idiosyncr_eventsBB),
    100*length(intersect(High_Idiosyncr_eventsBB,High_VOL_eventsBB ))/length(High_Idiosyncr_eventsBB), 100*length(intersect(High_Idiosyncr_eventsBB,Low_VOL_eventsBB ))/length(High_Idiosyncr_eventsBB),
    100*length(intersect(High_Idiosyncr_eventsBB,High_LEV_eventsBB ))/length(High_Idiosyncr_eventsBB), 100*length(intersect(High_Idiosyncr_eventsBB,Low_LEV_eventsBB ))/length(High_Idiosyncr_eventsBB),
    100*length(intersect(High_Idiosyncr_eventsBB,High_EPS_eventsBB ))/length(High_Idiosyncr_eventsBB), 100*length(intersect(High_Idiosyncr_eventsBB,Low_EPS_eventsBB ))/length(High_Idiosyncr_eventsBB)
  ),
  c(100*length(intersect(Low_Idiosyncr_eventsBB,High_Idiosyncr_eventsBB ))/length(Low_Idiosyncr_eventsBB), 100*length(intersect(Low_Idiosyncr_eventsBB,Low_Idiosyncr_eventsBB ))/length(Low_Idiosyncr_eventsBB),
    100*length(intersect(Low_Idiosyncr_eventsBB,High_VOL_eventsBB ))/length(Low_Idiosyncr_eventsBB), 100*length(intersect(Low_Idiosyncr_eventsBB,Low_VOL_eventsBB ))/length(Low_Idiosyncr_eventsBB),
    100*length(intersect(Low_Idiosyncr_eventsBB,High_LEV_eventsBB ))/length(Low_Idiosyncr_eventsBB), 100*length(intersect(Low_Idiosyncr_eventsBB,Low_LEV_eventsBB ))/length(Low_Idiosyncr_eventsBB),
    100*length(intersect(Low_Idiosyncr_eventsBB,High_EPS_eventsBB ))/length(Low_Idiosyncr_eventsBB), 100*length(intersect(Low_Idiosyncr_eventsBB,Low_EPS_eventsBB ))/length(Low_Idiosyncr_eventsBB)
  ),
  ###  
  c(100*length(intersect(High_VOL_eventsBB,High_Idiosyncr_eventsBB ))/length(High_VOL_eventsBB), 100*length(intersect(High_VOL_eventsBB,Low_Idiosyncr_eventsBB ))/length(High_VOL_eventsBB),
    100*length(intersect(High_VOL_eventsBB,High_VOL_eventsBB ))/length(High_VOL_eventsBB), 100*length(intersect(High_VOL_eventsBB,Low_VOL_eventsBB ))/length(High_VOL_eventsBB),
    100*length(intersect(High_VOL_eventsBB,High_LEV_eventsBB ))/length(High_VOL_eventsBB), 100*length(intersect(High_VOL_eventsBB,Low_LEV_eventsBB ))/length(High_VOL_eventsBB),
    100*length(intersect(High_VOL_eventsBB,High_EPS_eventsBB ))/length(High_VOL_eventsBB), 100*length(intersect(High_VOL_eventsBB,Low_EPS_eventsBB ))/length(High_VOL_eventsBB)
  ),
  c(100*length(intersect(Low_VOL_eventsBB,High_Idiosyncr_eventsBB ))/length(Low_VOL_eventsBB), 100*length(intersect(Low_VOL_eventsBB,Low_Idiosyncr_eventsBB ))/length(Low_VOL_eventsBB),
    100*length(intersect(Low_VOL_eventsBB,High_VOL_eventsBB ))/length(Low_VOL_eventsBB), 100*length(intersect(Low_VOL_eventsBB,Low_VOL_eventsBB ))/length(Low_VOL_eventsBB),
    100*length(intersect(Low_VOL_eventsBB,High_LEV_eventsBB ))/length(Low_VOL_eventsBB), 100*length(intersect(Low_VOL_eventsBB,Low_LEV_eventsBB ))/length(Low_VOL_eventsBB),
    100*length(intersect(Low_VOL_eventsBB,High_EPS_eventsBB ))/length(Low_VOL_eventsBB), 100*length(intersect(Low_VOL_eventsBB,Low_EPS_eventsBB ))/length(Low_VOL_eventsBB)
  ),
  ###  
  c(100*length(intersect(High_LEV_eventsBB,High_Idiosyncr_eventsBB ))/length(High_LEV_eventsBB), 100*length(intersect(High_LEV_eventsBB,Low_Idiosyncr_eventsBB ))/length(High_LEV_eventsBB),
    100*length(intersect(High_LEV_eventsBB,High_VOL_eventsBB ))/length(High_LEV_eventsBB), 100*length(intersect(High_LEV_eventsBB,Low_VOL_eventsBB ))/length(High_LEV_eventsBB),
    100*length(intersect(High_LEV_eventsBB,High_LEV_eventsBB ))/length(High_LEV_eventsBB), 100*length(intersect(High_LEV_eventsBB,Low_LEV_eventsBB ))/length(High_LEV_eventsBB),
    100*length(intersect(High_LEV_eventsBB,High_EPS_eventsBB ))/length(High_LEV_eventsBB), 100*length(intersect(High_LEV_eventsBB,Low_LEV_eventsBB ))/length(High_LEV_eventsBB)
  ),
  c(100*length(intersect(Low_LEV_eventsBB,High_Idiosyncr_eventsBB ))/length(Low_LEV_eventsBB), 100*length(intersect(Low_LEV_eventsBB,Low_Idiosyncr_eventsBB ))/length(Low_LEV_eventsBB),
    100*length(intersect(Low_LEV_eventsBB,High_VOL_eventsBB ))/length(Low_LEV_eventsBB), 100*length(intersect(Low_LEV_eventsBB,Low_VOL_eventsBB ))/length(Low_LEV_eventsBB),
    100*length(intersect(Low_LEV_eventsBB,High_LEV_eventsBB ))/length(Low_LEV_eventsBB), 100*length(intersect(Low_LEV_eventsBB,Low_LEV_eventsBB ))/length(Low_LEV_eventsBB),
    100*length(intersect(Low_LEV_eventsBB,High_EPS_eventsBB ))/length(Low_LEV_eventsBB), 100*length(intersect(Low_LEV_eventsBB,Low_EPS_eventsBB ))/length(Low_LEV_eventsBB)
  )  
  
)
rownames(Under_IdioBB) <- c("Undervalued","Overvalued", "High Idiosync.", "Low Idiosync.","High Vol.", "Low Vol.","High Lev.","Low Lev." )
colnames(Under_IdioBB) <- c("H Idiosync.","L Idiosync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.")

Under_IdioISS = rbind(
  c(100*length(intersect(which(company_subset_undervalued_iss),High_Idiosyncr_eventsISS ))/length(which(company_subset_undervalued_iss)), 100*length(intersect(which(company_subset_undervalued_iss),Low_Idiosyncr_eventsISS ))/length(which(company_subset_undervalued_iss)),
    100*length(intersect(which(company_subset_undervalued_iss),High_VOL_eventsISS ))/length(which(company_subset_undervalued_iss)), 100*length(intersect(which(company_subset_undervalued_iss),Low_VOL_eventsISS ))/length(which(company_subset_undervalued_iss)),
    100*length(intersect(which(company_subset_undervalued_iss),High_LEV_eventsISS ))/length(which(company_subset_undervalued_iss)), 100*length(intersect(which(company_subset_undervalued_iss),Low_LEV_eventsISS ))/length(which(company_subset_undervalued_iss)),
    100*length(intersect(which(company_subset_undervalued_iss),High_EPS_eventsISS ))/length(which(company_subset_undervalued_iss)), 100*length(intersect(which(company_subset_undervalued_iss),Low_EPS_eventsISS ))/length(which(company_subset_undervalued_iss))
  ),
  c(100*length(intersect(which(company_subset_overvalued_iss),High_Idiosyncr_eventsISS ))/length(which(company_subset_overvalued_iss)), 100*length(intersect(which(company_subset_overvalued_iss),Low_Idiosyncr_eventsISS ))/length(which(company_subset_overvalued_iss)),
    100*length(intersect(which(company_subset_overvalued_iss),High_VOL_eventsISS ))/length(which(company_subset_overvalued_iss)), 100*length(intersect(which(company_subset_overvalued_iss),Low_VOL_eventsISS ))/length(which(company_subset_overvalued_iss)),
    100*length(intersect(which(company_subset_overvalued_iss),High_LEV_eventsISS ))/length(which(company_subset_overvalued_iss)), 100*length(intersect(which(company_subset_overvalued_iss),Low_LEV_eventsISS ))/length(which(company_subset_overvalued_iss)),
    100*length(intersect(which(company_subset_overvalued_iss),High_EPS_eventsISS ))/length(which(company_subset_overvalued_iss)), 100*length(intersect(which(company_subset_overvalued_iss),Low_EPS_eventsISS ))/length(which(company_subset_overvalued_iss))
  ),
  ###
  c(100*length(intersect(High_Idiosyncr_eventsISS,High_Idiosyncr_eventsISS ))/length(High_Idiosyncr_eventsISS), 100*length(intersect(High_Idiosyncr_eventsISS,Low_Idiosyncr_eventsISS ))/length(High_Idiosyncr_eventsISS),
    100*length(intersect(High_Idiosyncr_eventsISS,High_VOL_eventsISS ))/length(High_Idiosyncr_eventsISS), 100*length(intersect(High_Idiosyncr_eventsISS,Low_VOL_eventsISS ))/length(High_Idiosyncr_eventsISS),
    100*length(intersect(High_Idiosyncr_eventsISS,High_LEV_eventsISS ))/length(High_Idiosyncr_eventsISS), 100*length(intersect(High_Idiosyncr_eventsISS,Low_LEV_eventsISS ))/length(High_Idiosyncr_eventsISS),
    100*length(intersect(High_Idiosyncr_eventsISS,High_EPS_eventsISS ))/length(High_Idiosyncr_eventsISS), 100*length(intersect(High_Idiosyncr_eventsISS,Low_EPS_eventsISS ))/length(High_Idiosyncr_eventsISS)
  ),
  c(100*length(intersect(Low_Idiosyncr_eventsISS,High_Idiosyncr_eventsISS ))/length(Low_Idiosyncr_eventsISS), 100*length(intersect(Low_Idiosyncr_eventsISS,Low_Idiosyncr_eventsISS ))/length(Low_Idiosyncr_eventsISS),
    100*length(intersect(Low_Idiosyncr_eventsISS,High_VOL_eventsISS ))/length(Low_Idiosyncr_eventsISS), 100*length(intersect(Low_Idiosyncr_eventsISS,Low_VOL_eventsISS ))/length(Low_Idiosyncr_eventsISS),
    100*length(intersect(Low_Idiosyncr_eventsISS,High_LEV_eventsISS ))/length(Low_Idiosyncr_eventsISS), 100*length(intersect(Low_Idiosyncr_eventsISS,Low_LEV_eventsISS ))/length(Low_Idiosyncr_eventsISS),
    100*length(intersect(Low_Idiosyncr_eventsISS,High_EPS_eventsISS ))/length(Low_Idiosyncr_eventsISS), 100*length(intersect(Low_Idiosyncr_eventsISS,Low_EPS_eventsISS ))/length(Low_Idiosyncr_eventsISS)
  ),
  ###  
  c(100*length(intersect(High_VOL_eventsISS,High_Idiosyncr_eventsISS ))/length(High_VOL_eventsISS), 100*length(intersect(High_VOL_eventsISS,Low_Idiosyncr_eventsISS ))/length(High_VOL_eventsISS),
    100*length(intersect(High_VOL_eventsISS,High_VOL_eventsISS ))/length(High_VOL_eventsISS), 100*length(intersect(High_VOL_eventsISS,Low_VOL_eventsISS ))/length(High_VOL_eventsISS),
    100*length(intersect(High_VOL_eventsISS,High_LEV_eventsISS ))/length(High_VOL_eventsISS), 100*length(intersect(High_VOL_eventsISS,Low_LEV_eventsISS ))/length(High_VOL_eventsISS),
    100*length(intersect(High_VOL_eventsISS,High_EPS_eventsISS ))/length(High_VOL_eventsISS), 100*length(intersect(High_VOL_eventsISS,Low_EPS_eventsISS ))/length(High_VOL_eventsISS)
  ),
  c(100*length(intersect(Low_VOL_eventsISS,High_Idiosyncr_eventsISS ))/length(Low_VOL_eventsISS), 100*length(intersect(Low_VOL_eventsISS,Low_Idiosyncr_eventsISS ))/length(Low_VOL_eventsISS),
    100*length(intersect(Low_VOL_eventsISS,High_VOL_eventsISS ))/length(Low_VOL_eventsISS), 100*length(intersect(Low_VOL_eventsISS,Low_VOL_eventsISS ))/length(Low_VOL_eventsISS),
    100*length(intersect(Low_VOL_eventsISS,High_LEV_eventsISS ))/length(Low_VOL_eventsISS), 100*length(intersect(Low_VOL_eventsISS,Low_LEV_eventsISS ))/length(Low_VOL_eventsISS),
    100*length(intersect(Low_VOL_eventsISS,High_EPS_eventsISS ))/length(Low_VOL_eventsISS), 100*length(intersect(Low_VOL_eventsISS,Low_EPS_eventsISS ))/length(Low_VOL_eventsISS)
  ),
  ###  
  c(100*length(intersect(High_LEV_eventsISS,High_Idiosyncr_eventsISS ))/length(High_LEV_eventsISS), 100*length(intersect(High_LEV_eventsISS,Low_Idiosyncr_eventsISS ))/length(High_LEV_eventsISS),
    100*length(intersect(High_LEV_eventsISS,High_VOL_eventsISS ))/length(High_LEV_eventsISS), 100*length(intersect(High_LEV_eventsISS,Low_VOL_eventsISS ))/length(High_LEV_eventsISS),
    100*length(intersect(High_LEV_eventsISS,High_LEV_eventsISS ))/length(High_LEV_eventsISS), 100*length(intersect(High_LEV_eventsISS,Low_LEV_eventsISS ))/length(High_LEV_eventsISS),
    100*length(intersect(High_LEV_eventsISS,High_EPS_eventsISS ))/length(High_LEV_eventsISS), 100*length(intersect(High_LEV_eventsISS,Low_LEV_eventsISS ))/length(High_LEV_eventsISS)
  ),
  c(100*length(intersect(Low_LEV_eventsISS,High_Idiosyncr_eventsISS ))/length(Low_LEV_eventsISS), 100*length(intersect(Low_LEV_eventsISS,Low_Idiosyncr_eventsISS ))/length(Low_LEV_eventsISS),
    100*length(intersect(Low_LEV_eventsISS,High_VOL_eventsISS ))/length(Low_LEV_eventsISS), 100*length(intersect(Low_LEV_eventsISS,Low_VOL_eventsISS ))/length(Low_LEV_eventsISS),
    100*length(intersect(Low_LEV_eventsISS,High_LEV_eventsISS ))/length(Low_LEV_eventsISS), 100*length(intersect(Low_LEV_eventsISS,Low_LEV_eventsISS ))/length(Low_LEV_eventsISS),
    100*length(intersect(Low_LEV_eventsISS,High_EPS_eventsISS ))/length(Low_LEV_eventsISS), 100*length(intersect(Low_LEV_eventsISS,Low_EPS_eventsISS ))/length(Low_LEV_eventsISS)
  )  
  
)
rownames(Under_IdioISS) <- c("Undervalued","Overvalued", "High Idiosync.", "Low Idiosync.","High Vol.", "Low Vol.","High Lev.","Low Lev." )
colnames(Under_IdioISS) <- c("H Idiosync.","L Idiosync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.")

#####################################################################################################################
#####################################################################################################################
# NEW INDEX
#####################################################################################################################
#####################################################################################################################

tmp = BUYBACK_DATA$DATASET$SDC$BEME_used + BUYBACK_DATA$DATASET$SDC$Performance_used
company_subset_undervaluedNoSize_bb = ifelse(tmp <= quantile(tmp,0.2), 0, ifelse(tmp >= quantile(tmp,0.8),2,1))
rm("tmp")

# Buybacks 
DATASET = BUYBACK_DATA$DATASET
EUindex_bb = sapply(1:length(DATASET$SDC$Event.Date), function(i){
  ifelse(i %in% High_Idiosyncr_eventsBB, 2, ifelse(i %in% Low_Idiosyncr_eventsBB, 0, 1)) +
    ifelse(i %in% High_VOL_eventsBB, 2, ifelse(i %in% Low_VOL_eventsBB, 0, 1)) +
    #ifelse(i %in% High_EPS_eventsBB, 2, ifelse(i %in% Low_EPS_eventsBB, 0, 1)) +
    #ifelse(i %in% Low_LEV_eventsBB, 2, ifelse(i %in% High_LEV_eventsBB, 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
  #company_subset_undervaluedNoSize_bb[i]
})

EUindex_bb_nosize = sapply(1:length(DATASET$SDC$Event.Date), function(i){
  ifelse(i %in% High_Idiosyncr_eventsBB, 2, ifelse(i %in% Low_Idiosyncr_eventsBB, 0, 1)) +
    ifelse(i %in% High_VOL_eventsBB, 2, ifelse(i %in% Low_VOL_eventsBB, 0, 1)) +
    #ifelse(i %in% High_EPS_eventsBB, 2, ifelse(i %in% Low_EPS_eventsBB, 0, 1)) +
    #ifelse(i %in% Low_LEV_eventsBB, 2, ifelse(i %in% High_LEV_eventsBB, 0, 1)) +
    #ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
    company_subset_undervaluedNoSize_bb[i]
})

EU_long_bb = NULL
EU_Hedged_bb = NULL
EU_long48_bb = NULL
EU_Hedged48_bb = NULL
EU_IRATStable_bb = NULL
EU_CALtable_bb = NULL
for (i in 0:6){
  EU_events_now = which(EUindex_bb == i)
  EU = apply(PNL_matrix_BB(start_date_event,"One.Year.After", EU_events_now,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", EU_events_now,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU48m_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU_long_bb = cbind(EU_long_bb,EU)
  EU_Hedged_bb = cbind(EU_Hedged_bb,EU_hedged)
  EU_long48_bb = cbind(EU_long48_bb,EU48m)
  EU_Hedged48_bb = cbind(EU_Hedged48_bb, EU48m_hedged)
  EU_IRATStable_bb = cbind(EU_IRATStable_bb,car_table(DATASET$returns_by_event_monthly[,EU_events_now], DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly)$results)
  EU_CALtable_bb = cbind(EU_CALtable_bb,calendar_table(DATASET$returns_by_event_monthly[,EU_events_now], DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly)$results)
}
rm("EU","EU_hedged","EU48m", "EU48m_hedged")

##ROBUST OVER TIME
high_EU = (EUindex_bb %in% 5:6)
low_EU = (EUindex_bb %in% 0:1)

EIRATS_table_undervaluation_time_bb = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    #car_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    #car_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    car_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  #colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
  #                   "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  
  res    
}))
EIRATS_table_undervaluation_time_bb = round(EIRATS_table_undervaluation_time_bb,2)

ECAL_table_undervaluation_time_bb = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    #calendar_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    #calendar_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    calendar_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    calendar_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  #colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
  #                   "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  
  res    
}))
ECAL_table_undervaluation_time_bb = round(ECAL_table_undervaluation_time_bb,2)

high_EU_bb = high_EU
low_EU_bb = low_EU
High_EU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", high_EU_bb,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
High_EU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_EU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
Low_EU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", low_EU_bb,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
Low_EU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_EU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
High_EU_BB48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", high_EU_bb,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
High_EU_BB_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_EU_BB48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
Low_EU_BB48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", low_EU_bb,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
Low_EU_BB_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_EU_BB48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    

rm("DATASET","high_EU","low_EU")

###############
##### ISSUERS
DATASET = ISSUERS_DATA$DATASET
EUindex_iss = sapply(1:length(DATASET$SDC$Event.Date), function(i){
  ifelse(i %in% High_Idiosyncr_eventsISS, 2, ifelse(i %in% Low_Idiosyncr_eventsISS, 0, 1)) +
    ifelse(i %in% High_VOL_eventsISS, 2, ifelse(i %in% Low_VOL_eventsISS, 0, 1)) +
    ifelse(company_subset_overvalued_iss[i], 2, ifelse(company_subset_undervalued_iss[i], 0, 1)) ##"GOOD" MEANS UNDERVALUED!!! (buybacks terminology...)
})
EU_long_iss = NULL
EU_Hedged_iss = NULL
EU_long48_iss = NULL
EU_Hedged48_iss = NULL
EU_IRATStable_iss = NULL
EU_CALtable_iss = NULL
for (i in 0:6){
  EU_events_now = which(EUindex_iss == i)
  EU = apply(PNL_matrix_BB(start_date_event,"One.Year.After", EU_events_now,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", EU_events_now,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU48m_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU_long_iss = cbind(EU_long_iss,EU)
  EU_Hedged_iss = cbind(EU_Hedged_iss,EU_hedged)
  EU_long48_iss = cbind(EU_long48_iss,EU48m)
  EU_Hedged48_iss = cbind(EU_Hedged48_iss, EU48m_hedged)
  EU_IRATStable_iss = cbind(EU_IRATStable_iss,car_table(DATASET$returns_by_event_monthly[,EU_events_now], DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly)$results)
  EU_CALtable_iss = cbind(EU_CALtable_iss,calendar_table(DATASET$returns_by_event_monthly[,EU_events_now], DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly)$results)
}
rm("EU","EU_hedged","EU48m", "EU48m_hedged")
##ROBUST OVER TIME
DATASET = ISSUERS_DATA$DATASET
low_EU = (EUindex_iss %in% 0:1) ## "low_EU" MEANS UNDERVALUED!!! (buybacks terminology...)
high_EU = (EUindex_iss %in% 5:6)

EIRATS_table_undervaluation_time_iss = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    #car_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    #car_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    car_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  #colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
  #                   "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  
  res    
}))
EIRATS_table_undervaluation_time_iss = round(EIRATS_table_undervaluation_time_iss,2)

ECAL_table_undervaluation_time_iss = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    #calendar_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    #calendar_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    calendar_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    calendar_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  #colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
  #                   "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  
  res    
}))
ECAL_table_undervaluation_time_iss = round(ECAL_table_undervaluation_time_iss,2)

high_EU_iss = high_EU
low_EU_iss = low_EU
rm("DATASET","high_EU","low_EU")

########################################################################################################
# EU relations
########################################################################################################

all_fund_sources = unique(unlist(sapply(BUYBACK_DATA$DATASET$SDC$Source...of..Funds..Code, function(i) unlist(str_split(i,"\\+")))))
cash_funds = c("CR")
credit_funds = c("BL","BOR","CF","DS")
other_funds = setdiff(all_fund_sources,c(cash_funds,credit_funds))
all_purposes = unique(unlist(sapply(BUYBACK_DATA$DATASET$SDC$Purpose.Code, function(i) unlist(str_split(i,"\\+")))))
good_purpose = c("ESV","UVL","STP","ISV")
other_purpose = setdiff(all_purposes,c(good_purpose))

high_leverage = 0*EUindex_bb
high_leverage[High_LEV_eventsBB] <-1
low_leverage = 0*EUindex_bb
low_leverage[Low_LEV_eventsBB] <-1
Missed_EPS = (BUYBACK_DATA$DATASET$SDC$EPS.Value < BUYBACK_DATA$DATASET$SDC$EPS.Forecast)
Beat_EPS = (BUYBACK_DATA$DATASET$SDC$EPS.Value >= BUYBACK_DATA$DATASET$SDC$EPS.Forecast)
low_epsunc = 0*EUindex_bb
low_epsunc[Low_EPS_eventsBB] <-1
ISS_Later = ifelse(BUYBACK_DATA$DATASET$SDC$OtherlaterEvent!=0, "Yes", "No")
Credit = sapply(BUYBACK_DATA$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), credit_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(cash_funds,other_funds)))==0)
Cash = sapply(BUYBACK_DATA$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), cash_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(credit_funds,other_funds)))==0)
Good_purpose = sapply(BUYBACK_DATA$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), good_purpose))!=0 & length(intersect(unlist(str_split(i,"\\+")), other_purpose))==0)
Stock_Option_Plan = sapply(BUYBACK_DATA$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "STP"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV","UVL")))==0)
Undervalued = sapply(BUYBACK_DATA$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "UVL"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("STP")))==0 )
Enhance_Shareholder_Value = sapply(BUYBACK_DATA$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV")))!=0)
all_characteristics = cbind(low_leverage,high_leverage,Missed_EPS,Beat_EPS,ISS_Later,Cash,Good_purpose,Undervalued,Enhance_Shareholder_Value,Stock_Option_Plan)

EU_relations= t(apply(all_characteristics,2,function(r){
  x = table(EUindex_bb, r)
  x = matrix(round(100*x[,2]/(x[,1]+x[,2]),1),ncol=1)
  x
}))
colnames(EU_relations) <- paste("EU",0:(ncol(EU_relations)-1),sep="")
rownames(EU_relations) <- gsub("_"," ", rownames(EU_relations))
all_characteristics_continuous = cbind(BUYBACK_DATA$DATASET$SDC$Market.Cap, BUYBACK_DATA$DATASET$SDC$BEME_used, BUYBACK_DATA$DATASET$SDC$Event.Size)
EU_relations_continuous = t(apply(all_characteristics_continuous,2,function(r){
  sapply(sort(unique(EUindex_bb)), function(i) {
    useonly = which(EUindex_bb == i)
    mean(r[useonly][!is.na(r[useonly])])
  })
}))
rownames(EU_relations_continuous) <- c("Market Cap.", "BE/ME Score", "Percentage Shares")
EU_relations = rbind(EU_relations, round(EU_relations_continuous,2))

U_relations= t(apply(all_characteristics,2,function(r){
  x = table(BUYBACK_DATA$Valuation_Index, r)
  x = matrix(round(100*x[,2]/(x[,1]+x[,2]),1),ncol=1)
  x
}))
colnames(U_relations) <- paste("U",0:(ncol(U_relations)-1),sep="")
rownames(U_relations) <- gsub("_"," ", rownames(U_relations))


########################################################################################
## NOW SAVE ALL THESE VARIABLES SO THAT THE rnw RUNS FASTER
########################################################################################

# Keep all results ONLY IN THIS FILE just in case we need to rerun or add something... this is temporary
save(list = setdiff(ls(all = TRUE),initial_vars), file = "tmpfiles/bb_issuersALL.Rdata")
