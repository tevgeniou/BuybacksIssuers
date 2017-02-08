#  Copyright 2015, INSEAD
#  by T. Evgeniou, Enric Junque de Fortuny, Nick Nassuphis, Theo Vermaelen 
#  Dual licensed under the MIT or GPL Version 2 licenses.


####################################################################################
# These are one by one the tables in the .Rnw. 
####################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceLibraries/latex_code.R")
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")

rm("three_factor_model", "five_factor_model")

initial_vars = ls(all = TRUE) # takes time to save and load, so we save only what is needed at the end. 

load("../FinanceData/created_projects_datasets/BUYBACKSnew.Rdata")

if (continuous_valuation_index){
  BUYBACK_DATA$Valuation_Index = 
    ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$recent_performance_score), NA, (1-BUYBACK_DATA$DATASET$CRSP$recent_performance_score)) +
    ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap_score), NA, (1-BUYBACK_DATA$DATASET$CRSP$Market.Cap_score)) + 
    ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$BE.ME_score), NA, BUYBACK_DATA$DATASET$CRSP$BE.ME_score)
  
  ISSUERS_DATA$Valuation_Index =  # THIS IS AN OVERVAUATION INDEX: THE HIGHER, THE MORE OVERVALUED: High Perf + Low Size + Low B/M
    ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$recent_performance_score), NA, (ISSUERS_DATA$DATASET$CRSP$recent_performance_score)) +
    ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap_score), NA, (1-ISSUERS_DATA$DATASET$CRSP$Market.Cap_score)) + 
    ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$BE.ME_score), NA, (1-ISSUERS_DATA$DATASET$CRSP$BE.ME_score))
  
} else {
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
}


########################################################################################################
# First get the pre-announce betas and returns - used for cross-sectional analyses- for ALL events

if (0){ # It is slow so we can do it only when needed. Also, first manually do the basic filtering (for missing data, basic size/price/etc filters etc) in filter_bbissuers_data.R
  
  # Buybacks first
  BUYBACK_PreEvent_Factor_coeffs <- event_study_factor_coeffs(BUYBACK_DATA$DATASET$returns_by_event_monthly, BUYBACK_DATA$DATASET$SDC$Event.Date,BUYBACK_DATA$Risk_Factors_Monthly)
  ## Step 2: Calculate Monthly Estimated Risk-adjusted Return 
  factors_used_noRF = c("Delta","SMB","HML","RMW","CMA")
  Estimated_returns = Reduce(rbind,lapply(1:length(BUYBACK_PreEvent_Factor_coeffs), function(thestock){
    this_stock_data = BUYBACK_PreEvent_Factor_coeffs[[thestock]]
    apply(this_stock_data, 1, function(r)
      ifelse(!is.na(sum(r)), r["actual_ret"] - r["RF_ret"] - sum(r[factors_used_noRF]*r[paste(factors_used_noRF,"ret", sep="_")]), NA)
    )
  }))
  names(BUYBACK_PreEvent_Factor_coeffs) <- paste(BUYBACK_DATA$DATASET$SDC$permno, BUYBACK_DATA$DATASET$SDC$Event.Date, sep=" ")
  rownames(Estimated_returns) <- paste(BUYBACK_DATA$DATASET$SDC$permno, BUYBACK_DATA$DATASET$SDC$Event.Date, sep=" ")
  colnames(Estimated_returns)<- paste("Month", 1:ncol(Estimated_returns), sep=" ")
  save(BUYBACK_PreEvent_Factor_coeffs,Estimated_returns,file = "../FinanceData/created_projects_datasets/BUYBACKSnew_BSC1998_event_study_factor_coeffs.Rdata") #
  
  #############
  # Now issuers
  ISSUERS_PreEvent_Factor_coeffs <- event_study_factor_coeffs(ISSUERS_DATA$DATASET$returns_by_event_monthly, ISSUERS_DATA$DATASET$SDC$Event.Date,ISSUERS_DATA$Risk_Factors_Monthly)
  ## Step 2: Calculate Monthly Estimated Risk-adjusted Return 
  factors_used_noRF = c("Delta","SMB","HML","RMW","CMA")
  Estimated_returns = Reduce(rbind,lapply(1:length(ISSUERS_PreEvent_Factor_coeffs), function(thestock){
    this_stock_data = ISSUERS_PreEvent_Factor_coeffs[[thestock]]
    apply(this_stock_data, 1, function(r)
      ifelse(!is.na(sum(r)), r["actual_ret"] - r["RF_ret"] - sum(r[factors_used_noRF]*r[paste(factors_used_noRF,"ret", sep="_")]), NA)
    )
  }))
  names(ISSUERS_PreEvent_Factor_coeffs) <- paste(ISSUERS_DATA$DATASET$SDC$permno, ISSUERS_DATA$DATASET$SDC$Event.Date, sep=" ")
  rownames(Estimated_returns) <- paste(ISSUERS_DATA$DATASET$SDC$permno, ISSUERS_DATA$DATASET$SDC$Event.Date, sep=" ")
  colnames(Estimated_returns)<- paste("Month", 1:ncol(Estimated_returns), sep=" ")
  save(ISSUERS_PreEvent_Factor_coeffs,Estimated_returns,file = "../FinanceData/created_projects_datasets/ISSUERSnew_BSC1998_event_study_factor_coeffs.Rdata") #
}
############################################################################################################
# All the data filters are done in here
source("filter_bbissuers_data.R")

############################################################################################################

BUYBACK_DATA$DATASET$DatesMonth <- create_dates_month(BUYBACK_DATA$DATASET$SDC$Event.Date, rownames(BUYBACK_DATA$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(BUYBACK_DATA$DATASET$DatesMonth) <- BUYBACK_DATA$DATASET$SDC$permno

ISSUERS_DATA$DATASET$DatesMonth <- create_dates_month(ISSUERS_DATA$DATASET$SDC$Event.Date, rownames(ISSUERS_DATA$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(ISSUERS_DATA$DATASET$DatesMonth) <- ISSUERS_DATA$DATASET$SDC$permno

if (do.value.weight == 1){   
  per_month_events = table(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start = 1, end = 7))
  per_month_events = structure(as.numeric(per_month_events), .Names = names(per_month_events))
  per_month_events = sapply(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start = 1, end = 7), function(i) per_month_events[i])
  per_month_events = per_month_events*(length(per_month_events)/sum(per_month_events)) # make it also have total "number of events" being 1
  value.weights_bb = per_month_events*BUYBACK_DATA$DATASET$CRSP$Market.Cap
  value.weights_bb = BUYBACK_DATA$DATASET$CRSP$Market.Cap
  
  per_month_events = table(str_sub(ISSUERS_DATA$DATASET$SDC$Event.Date, start = 1, end = 7))
  per_month_events = structure(as.numeric(per_month_events), .Names = names(per_month_events))
  per_month_events = sapply(str_sub(ISSUERS_DATA$DATASET$SDC$Event.Date, start = 1, end = 7), function(i) per_month_events[i])
  per_month_events = per_month_events*(length(per_month_events)/sum(per_month_events)) # make it also have total "number of events" being 1
  value.weights_iss = per_month_events#*ISSUERS_DATA$DATASET$CRSP$Market.Cap
  value.weights_iss = ISSUERS_DATA$DATASET$CRSP$Market.Cap
} else {
  value.weights_bb = rep(1,length(BUYBACK_DATA$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
  value.weights_iss = rep(1,length(ISSUERS_DATA$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
}

#BUYBACK_DATA$DATASET$events_last_30days <- sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(i) sum(BUYBACK_DATA$DATASET$SDC$Event.Date < i & BUYBACK_DATA$DATASET$SDC$Event.Date > i-20))
#BUYBACK_DATA$DATASET$events_last_60days <- sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(i) sum(BUYBACK_DATA$DATASET$SDC$Event.Date < i & BUYBACK_DATA$DATASET$SDC$Event.Date > i-40))

########################################################################################################
# OTHER FACTORS
Risk_Factors_Monthly = BUYBACK_DATA$Risk_Factors_Monthly
Market_Monthly = BUYBACK_DATA$Market_Monthly

if (0){ # q-factors
  qfactors <- read.csv("dataset/indices_and_factors/qfactors.csv", sep=";", dec=".")
  rownames(qfactors) <- paste(qfactors[,1],ifelse(str_length(qfactors[,2]) == 2, qfactors[,2], paste("0",qfactors[,2], sep="")), sep="-")
  useonly = which(rownames(qfactors) %in% str_sub(rownames(Risk_Factors_Monthly), start = 1, end=7))
  qfactors = qfactors[useonly,]
  rownamesqfactors <- rownames(Risk_Factors_Monthly)[match(rownames(qfactors), str_sub(rownames(Risk_Factors_Monthly), start = 1, end=7))]
  qfactors = qfactors[,3:6]
  qfactors = apply(qfactors,2,function(r) as.numeric(str_replace(r,",",".")))
  qfactors = qfactors/100
  rownames(qfactors) <- rownamesqfactors
  qfactors = cbind(qfactors,Risk_Factors_Monthly[rownames(qfactors),c("SMB", "HML","RF","RMW","CMA")])
  Risk_Factors_Monthly = qfactors
  formula_used="(ri - RF) ~ MKT + HML + ME + I.A + ROE"
  qfactors_irats_MKT_HML_ME_IA_ROE = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly, BUYBACK_DATA$DATASET$SDC$Event.Date, qfactors, formula_used = formula_used)$results
  qfactors_irats_MKT_HML_ME_IA_ROE = qfactors_irats_MKT_HML_ME_IA_ROE[reported_times,]
}


if (1){ # MIspricing factors
  misprice_factors <- read.csv("dataset/indices_and_factors/M4.csv", sep=",", dec=".")
  rownames(misprice_factors) <- as.character(as.Date(paste(misprice_factors$YYYYMM, "01",sep=""), format = "%Y%m%d"))
  misprice_factors = misprice_factors[,2:6]
  useonly = which(str_sub(rownames(misprice_factors), start = 1, end=7) %in% str_sub(rownames(Risk_Factors_Monthly), start = 1, end=7))
  misprice_factors = misprice_factors[useonly,]
  sum(str_sub(rownames(misprice_factors), start = 1, end=7)  != str_sub(rownames(Risk_Factors_Monthly), start = 1, end=7))
  rownames(misprice_factors) <- rownames(Risk_Factors_Monthly)
  max(abs(misprice_factors$MKTRF[(misprice_factors$MKTRF != Risk_Factors_Monthly$Delta)]-Risk_Factors_Monthly$Delta[(misprice_factors$MKTRF != Risk_Factors_Monthly$Delta)]))
  
  names(misprice_factors)[which(names(misprice_factors)=="SMB")] <- "SMB2"
  misprice_factors = cbind(misprice_factors,Risk_Factors_Monthly[,c("HML","RMW","CMA","SMB")])
  names(misprice_factors)[which(names(misprice_factors)=="MKTRF")] <- "Delta"
  
  Risk_Factors_Monthly = misprice_factors
  three_factor_model="(ri - RF) ~ Delta + SMB2 + MGMT + PERF"
  five_factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA"
  
  misprice_factors_irats_MKT_HML_ME_IA_ROE = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly, BUYBACK_DATA$DATASET$SDC$Event.Date, Risk_Factors_Monthly, formula_used = three_factor_model)$results
  misprice_factors_irats_MKT_HML_ME_IA_ROE = misprice_factors_irats_MKT_HML_ME_IA_ROE[reported_times,]
  
}

############################################################################
# READ STAMBAUGH
############################################################################
if (0){ # This is slow
  Stambaugh <- read.csv("Misp_Score.csv", sep=",")
  Stambaugh$yyyymm <- paste(str_sub(Stambaugh$yyyymm,start=1,end=4),str_sub(Stambaugh$yyyymm,start=5,end=6), sep="-")
  StambaughBB <- sapply(seq_along(BUYBACK_DATA$DATASET$SDC$CUSIP), function(i){
    getid = which(Stambaugh$permno == BUYBACK_DATA$DATASET$SDC$permno[i] & Stambaugh$yyyymm == str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date[i], start=1, end=7))
    ifelse(length(getid)== 1, Stambaugh$MISP[getid], NA)
  })
  
  StambaughBB_lastmonth <- sapply(seq_along(BUYBACK_DATA$DATASET$SDC$CUSIP), function(i){
    getid = which(Stambaugh$permno == BUYBACK_DATA$DATASET$SDC$permno[i] & Stambaugh$yyyymm == str_sub(AddMonths(BUYBACK_DATA$DATASET$SDC$Event.Date[i], -1), start=1, end=7))
    ifelse(length(getid)== 1, Stambaugh$MISP[getid], NA)
  })
  
  StambaughISS <- sapply(seq_along(ISSUERS_DATA$DATASET$SDC$CUSIP), function(i){
    getid = which(Stambaugh$permno == ISSUERS_DATA$DATASET$SDC$permno[i] & Stambaugh$yyyymm == str_sub(ISSUERS_DATA$DATASET$SDC$Event.Date[i], start=1, end=7))
    ifelse(length(getid)== 1, Stambaugh$MISP[getid], NA)
  })
  
  StambaughISS_lastmonth  <- sapply(seq_along(ISSUERS_DATA$DATASET$SDC$CUSIP), function(i){
    getid = which(Stambaugh$permno == ISSUERS_DATA$DATASET$SDC$permno[i] & Stambaugh$yyyymm == str_sub(AddMonths(ISSUERS_DATA$DATASET$SDC$Event.Date[i],-1), start=1, end=7))
    ifelse(length(getid)== 1, Stambaugh$MISP[getid], NA)
  })
  
  
  save(StambaughBB,StambaughISS, StambaughBB_lastmonth, StambaughISS_lastmonth, file = "StambaughBB.Rdata")
} else {
  load("StambaughBB.Rdata")
  BUYBACK_DATA$DATASET$Stambaugh <- StambaughBB_lastmonth
  ISSUERS_DATA$DATASET$Stambaugh <- StambaughISS_lastmonth
}

High_STAM_eventsBB = !is.na(BUYBACK_DATA$DATASET$Stambaugh) & (BUYBACK_DATA$DATASET$Stambaugh > quantile(BUYBACK_DATA$DATASET$Stambaugh[!is.na(BUYBACK_DATA$DATASET$Stambaugh)], 1-quantile_simple))
Low_STAM_eventsBB  = !is.na(BUYBACK_DATA$DATASET$Stambaugh) & (BUYBACK_DATA$DATASET$Stambaugh < quantile(BUYBACK_DATA$DATASET$Stambaugh[!is.na(BUYBACK_DATA$DATASET$Stambaugh)], quantile_simple))

High_STAM_eventsISS = !is.na(ISSUERS_DATA$DATASET$Stambaugh) & (ISSUERS_DATA$DATASET$Stambaugh > quantile(ISSUERS_DATA$DATASET$Stambaugh[!is.na(ISSUERS_DATA$DATASET$Stambaugh)], 1-quantile_simple))
Low_STAM_eventsISS = !is.na(ISSUERS_DATA$DATASET$Stambaugh) & (ISSUERS_DATA$DATASET$Stambaugh < quantile(ISSUERS_DATA$DATASET$Stambaugh[!is.na(ISSUERS_DATA$DATASET$Stambaugh)], quantile_simple))

############################################################################
# PREPARE ALL VARIABLES
############################################################################

# Some event categoeries (all T/F vectors)
company_subset_undervalued_bb = BUYBACK_DATA$Valuation_Index > quantile(BUYBACK_DATA$Valuation_Index, 1-quantile_simple)
company_subset_overvalued_bb = BUYBACK_DATA$Valuation_Index < quantile(BUYBACK_DATA$Valuation_Index,quantile_simple)
High_perf_eventsBB = BUYBACK_DATA$DATASET$CRSP$recent_performance_score  > quantile(BUYBACK_DATA$DATASET$CRSP$recent_performance_score,1-quantile_simple)
Low_perf_eventsBB = BUYBACK_DATA$DATASET$CRSP$recent_performance_score  < quantile(BUYBACK_DATA$DATASET$CRSP$recent_performance_score,quantile_simple)
High_Size_eventsBB = BUYBACK_DATA$DATASET$CRSP$Market.Cap_score  > quantile(BUYBACK_DATA$DATASET$CRSP$Market.Cap_score,1-quantile_simple)
Low_Size_eventsBB = BUYBACK_DATA$DATASET$CRSP$Market.Cap_score  < quantile(BUYBACK_DATA$DATASET$CRSP$Market.Cap_score,quantile_simple)
High_BEME_eventsBB = BUYBACK_DATA$DATASET$CRSP$BE.ME_score  > quantile(BUYBACK_DATA$DATASET$CRSP$BE.ME_score,1-quantile_simple)
Low_BEME_eventsBB = BUYBACK_DATA$DATASET$CRSP$BE.ME_score  < quantile(BUYBACK_DATA$DATASET$CRSP$BE.ME_score,quantile_simple)
High_Idiosyncr_eventsBB = BUYBACK_DATA$DATASET$CRSP$Rsq_score < quantile(BUYBACK_DATA$DATASET$CRSP$Rsq_score, quantile_simple)
Low_Idiosyncr_eventsBB  = BUYBACK_DATA$DATASET$CRSP$Rsq_score > quantile(BUYBACK_DATA$DATASET$CRSP$Rsq_score, 1-quantile_simple)
High_IVOL_eventsBB = BUYBACK_DATA$DATASET$CRSP$IVOL_score > quantile(BUYBACK_DATA$DATASET$CRSP$IVOL_score, 1-quantile_simple)
Low_IVOL_eventsBB  = BUYBACK_DATA$DATASET$CRSP$IVOL_score < quantile(BUYBACK_DATA$DATASET$CRSP$IVOL_score, quantile_simple)
High_VOL_eventsBB = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score > quantile(BUYBACK_DATA$DATASET$CRSP$pre_vol_Score, 1-quantile_simple)
Low_VOL_eventsBB  = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score < quantile(BUYBACK_DATA$DATASET$CRSP$pre_vol_Score, quantile_simple)
High_marketbeta_eventsBB = BUYBACK_DATA$DATASET$CRSP$market_beta_score > quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, 1-quantile_simple)
Medium_marketbeta_eventsBB  = BUYBACK_DATA$DATASET$CRSP$market_beta_score >= quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, quantile_simple) & BUYBACK_DATA$DATASET$CRSP$market_beta_score <= quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, 1-quantile_simple)
Low_marketbeta_eventsBB  = BUYBACK_DATA$DATASET$CRSP$market_beta_score < quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, quantile_simple)
tmp = BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e
High_LEV_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_LEV_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
tmp = BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec_score
High_EPS_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_EPS_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
BUYBACK_DATA$EU_index = sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
    ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
})
high_EU_bb = BUYBACK_DATA$EU_index >= quantile(BUYBACK_DATA$EU_index,0.8)
low_EU_bb = BUYBACK_DATA$EU_index <= quantile(BUYBACK_DATA$EU_index,0.2)
buybacks.events.past2years = 1*(BUYBACK_DATA$DATASET$CRSP$buybacks_events_past2years !=0)
rm("tmp")
#Recommendation score: 1. Strong Buy, 2. Buy, 3. Hold, 4. Underperform, 5. Sell
downgraded_events = !is.na(BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec) > scrub(BUYBACK_DATA$DATASET$ibes$month_minus2$mean_rec)
not_downgraded_events = !is.na(BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec) <= scrub(BUYBACK_DATA$DATASET$ibes$month_minus2$mean_rec)
upgraded_events = !is.na(BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec) & !is.na(BUYBACK_DATA$DATASET$ibes$month_minus2$mean_rec) & scrub(BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec) < scrub(BUYBACK_DATA$DATASET$ibes$month_minus2$mean_rec)


#### Continuous variables now
StambaughBB = BUYBACK_DATA$DATASET$Stambaugh
Firm_size = BUYBACK_DATA$DATASET$CRSP$Market.Cap
Firm_size_score = BUYBACK_DATA$DATASET$CRSP$Market.Cap_score
Prior_R = BUYBACK_DATA$DATASET$CRSP$recent_performance
Prior_R_score = BUYBACK_DATA$DATASET$CRSP$recent_performance_score
BEME = BUYBACK_DATA$DATASET$CRSP$BE.ME
BEME_score = BUYBACK_DATA$DATASET$CRSP$BE.ME_score
U_index = BUYBACK_DATA$Valuation_Index
EU_index = BUYBACK_DATA$EU_index
Vol_raw = BUYBACK_DATA$DATASET$CRSP$pre_vol
Vol_raw_score = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score
Idiosyncratic = BUYBACK_DATA$DATASET$CRSP$IVOL
Idiosyncratic_score = BUYBACK_DATA$DATASET$CRSP$IVOL_score
One_m_Rsqr = 1-BUYBACK_DATA$DATASET$CRSP$Rsq
One_m_Rsqr_score = 1-BUYBACK_DATA$DATASET$CRSP$Rsq_score
market_beta = BUYBACK_DATA$DATASET$CRSP$market_beta
market_beta_score = BUYBACK_DATA$DATASET$CRSP$market_beta_score
Analyst_recommendation = (BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec_score)
Analyst_disagreement = (BUYBACK_DATA$DATASET$ibes$month_minus1$analyst_disagreement_score)
Analyst_coverage = (BUYBACK_DATA$DATASET$ibes$month_minus1$analyst_coverage)
Analyst_coverage_score = (BUYBACK_DATA$DATASET$ibes$month_minus1$analyst_coverage_score)
Event.Size = BUYBACK_DATA$DATASET$SDC$Event.Size
buybacks.events.past2years = 1*(BUYBACK_DATA$DATASET$CRSP$buybacks_events_past2years !=0)
Total.Payout = (BUYBACK_DATA$DATASET$CRSP$Total_Payout)
lagged.dividend.payout.ratio = BUYBACK_DATA$DATASET$CRSP$divident_payout_ratio
lagged.dividend.payout.ratio[scrub(lagged.dividend.payout.ratio) < 0 | scrub(lagged.dividend.payout.ratio) > 100] <- NA
lagged.dividend.payout.ratio = (lagged.dividend.payout.ratio) 
Leverage = (BUYBACK_DATA$DATASET$CRSP$leverage_d_over_d_plus_e)
operating.income = (BUYBACK_DATA$DATASET$CRSP$operating_income)
std.operating.income = (BUYBACK_DATA$DATASET$CRSP$std_operating_income)
non.operating.income = (BUYBACK_DATA$DATASET$CRSP$non_operating_income)
liquid.assets = (BUYBACK_DATA$DATASET$CRSP$liquid_assets)
price.earnings.ratio = (BUYBACK_DATA$DATASET$CRSP$price_earnings_ratio)
capital.expenditures = (BUYBACK_DATA$DATASET$CRSP$capital_expenditures)
profitability = (BUYBACK_DATA$DATASET$CRSP$profitability)
net_debt = (BUYBACK_DATA$DATASET$CRSP$net_debt)
tax_rate = (BUYBACK_DATA$DATASET$CRSP$tax_rate)
if (0){
  # Institutional - still not standard across projects
  Institutional = sapply(1:length(BUYBACK_DATA$DATASET$SDC$CUSIP), function(i) {
    tmp = BUYBACK_DATA$DATASET$institutional$Institutional.Ownership.Ratio.1_score[[i]]
    useonly = which(AddMonths(as.Date(paste(names(tmp),"01", sep="-")),1) < BUYBACK_DATA$DATASET$SDC$Event.Date[i])
    tmp = tmp[useonly]
    ifelse(sum(!is.na(tmp)), tail(tmp[!is.na(tmp)],1), NA)
  })
  Institutional[scrub(Institutional) >= 100] <- NA
  Institutional.number = sapply(1:length(BUYBACK_DATA$DATASET$SDC$CUSIP), function(i) {
    tmp = BUYBACK_DATA$DATASET$institutional$num.institutional.investors_score[[i]]
    useonly = which(AddMonths(as.Date(paste(names(tmp),"01", sep="-")),1) < BUYBACK_DATA$DATASET$SDC$Event.Date[i])
    tmp = tmp[useonly]
    ifelse(sum(!is.na(tmp)), tail(tmp[!is.na(tmp)],1), NA)
  })
  Institutional.number[Institutional >= 100] <- NA
  save(Institutional,Institutional.number, file = "Institutional.number.Rdata")
} else {
  load("Institutional.number.Rdata")
}
tmp = Institutional
High_inst_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_inst_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)

tmp = Analyst_coverage_score
High_an_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_an_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
rm("tmp")

tmp = Analyst_recommendation
High_anRec_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_anRec_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
rm("tmp")

tmp = Analyst_disagreement
High_anDis_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_simple)  & !is.na(tmp)
Low_anDis_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_simple)  & !is.na(tmp)
rm("tmp")


# THESE ARE THE VARIABLES WE USE IN THE DATA SUMMARY STATS
all_characteristics_continuous_summary = cbind(
  buybacks.events.past2years,
  Firm_size,
  100*Prior_R,
  BEME,
  U_index,
  EU_index,
  100*Vol_raw,
  One_m_Rsqr,
  StambaughBB,
  Event.Size,
  Analyst_coverage,
  Total.Payout,
  lagged.dividend.payout.ratio,
  Leverage,
  profitability,
  net_debt,
  tax_rate,
  operating.income,
  std.operating.income,
  non.operating.income,
  liquid.assets,
  price.earnings.ratio,
  capital.expenditures,
  Institutional,
  Institutional.number
)
colnames(all_characteristics_continuous_summary) <- c(
  "Announced Repurchace in Previous 2 Years (0/1)",
  "Market Cap. (Score)", 
  "Prior Returns (Score)",
  "BE/ME (Score)", 
  "U-index",
  "EU-index",
  "Volatility (Score)", 
  "One minus Rsq (Score)",
  "Mispricing Measure",
  "Percent Shares",
  "Analyst Coverage (Score)",
  "Total Payout in Event Year before Event",
  "Lag Dividend Payout Ratio",
  "Leverage",
  "Profitability (ROA)",
  "Net Debt",
  "Tax Rate",
  "Operating Income (Percent assets)", 
  "std Operating Income", 
  "Non-Operating Income (Percent assets)",
  "Liquid Assets (Percent assets)",
  "Price/Earnings Ratio",
  "Capital Expenditures (Percent assets)",
  "Institutional Holdings (Score)",
  "Number of Institutions (Score)"
)

## ISSUERS - HERE WE DON'T NEED AS MUCH SINCE WE DON'T REALLY DEAL WITH THESE

company_subset_undervalued_iss = ISSUERS_DATA$Valuation_Index > quantile(ISSUERS_DATA$Valuation_Index, 1-quantile_Uindex)
company_subset_overvalued_iss = ISSUERS_DATA$Valuation_Index < quantile(ISSUERS_DATA$Valuation_Index,quantile_Uindex)
High_Idiosyncr_eventsISS = ISSUERS_DATA$DATASET$CRSP$Rsq_score < quantile(ISSUERS_DATA$DATASET$CRSP$Rsq_score, quantile_R2)
Low_Idiosyncr_eventsISS  = ISSUERS_DATA$DATASET$CRSP$Rsq_score > quantile(ISSUERS_DATA$DATASET$CRSP$Rsq_score, 1-quantile_R2)
High_VOL_eventsISS = ISSUERS_DATA$DATASET$CRSP$pre_vol_Score > quantile(ISSUERS_DATA$DATASET$CRSP$pre_vol_Score, 1-quantile_VOL)
Low_VOL_eventsISS  = ISSUERS_DATA$DATASET$CRSP$pre_vol_Score < quantile(ISSUERS_DATA$DATASET$CRSP$pre_vol_Score, quantile_VOL)

EUindex_iss = sapply(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsISS[i], 2, ifelse(Low_Idiosyncr_eventsISS[i], 0, 1)) +
    ifelse(High_VOL_eventsISS[i], 2, ifelse(Low_VOL_eventsISS[i], 0, 1)) +
    ifelse(company_subset_undervalued_iss[i], 2, ifelse(company_subset_overvalued_iss[i], 0, 1))
})

EUindex_bb = sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
    ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
})


################################################################################################
# Descriptive Stats

non_na_mean <- function(x) { mean(x[!is.na(x)]) }
non_na_median <- function(x) { median(x[!is.na(x)]) }
non_na_sd <- function(x) { sd(x[!is.na(x)]) }
non_na_quantile <- function(x,q) { quantile(x[!is.na(x)],q) }

non_na_zero_mean <- function(x) { mean(x[!is.na(x) & scrub(x)!=0]) }
non_na_zero_median <- function(x) { median(x[!is.na(x)& scrub(x)!=0]) }
non_na_zero_sd <- function(x) { sd(x[!is.na(x) & scrub(x)!=0]) }
non_na_zero_quantile <- function(x,q) { quantile(x[!is.na(x)& scrub(x)!=0],q) }

Descriptive_stats_BB = rbind(
  c(non_na_zero_mean(BUYBACK_DATA$DATASET$CRSP$Market.Cap),non_na_zero_median(BUYBACK_DATA$DATASET$CRSP$Market.Cap), non_na_zero_sd(BUYBACK_DATA$DATASET$CRSP$Market.Cap), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$Market.Cap,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$Market.Cap,0.8)),
  100*c(non_na_zero_mean(BUYBACK_DATA$DATASET$CRSP$recent_performance),non_na_zero_median(BUYBACK_DATA$DATASET$CRSP$recent_performance), non_na_zero_sd(BUYBACK_DATA$DATASET$CRSP$recent_performance), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$recent_performance,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$recent_performance,0.8)),
  c(non_na_zero_mean(BUYBACK_DATA$DATASET$CRSP$BE.ME),non_na_zero_median(BUYBACK_DATA$DATASET$CRSP$BE.ME), non_na_zero_sd(BUYBACK_DATA$DATASET$CRSP$BE.ME), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$BE.ME,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$BE.ME,0.8)),
  100*c(non_na_zero_mean(BUYBACK_DATA$DATASET$CRSP$pre_vol),non_na_zero_median(BUYBACK_DATA$DATASET$CRSP$pre_vol), non_na_zero_sd(BUYBACK_DATA$DATASET$CRSP$pre_vol), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$pre_vol,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$pre_vol,0.8)),
  c(non_na_zero_mean(BUYBACK_DATA$DATASET$CRSP$Rsq),non_na_zero_median(BUYBACK_DATA$DATASET$CRSP$Rsq), non_na_zero_sd(BUYBACK_DATA$DATASET$CRSP$Rsq), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$Rsq,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$Rsq,0.8)),
  c(non_na_mean(BUYBACK_DATA$Valuation_Index),non_na_median(BUYBACK_DATA$Valuation_Index), non_na_sd(BUYBACK_DATA$Valuation_Index), non_na_quantile(BUYBACK_DATA$Valuation_Index,0.2), non_na_quantile(BUYBACK_DATA$Valuation_Index,0.8)),
  c(non_na_mean(EUindex_bb),non_na_median(EUindex_bb), non_na_sd(EUindex_bb), non_na_quantile(EUindex_bb,0.2), non_na_quantile(EUindex_bb,0.8)),
  c(non_na_mean(StambaughBB),non_na_median(StambaughBB), non_na_sd(StambaughBB), non_na_quantile(StambaughBB,0.2), non_na_quantile(StambaughBB,0.8)),
  c(non_na_zero_mean(BUYBACK_DATA$DATASET$SDC$Event.Size),non_na_zero_median(BUYBACK_DATA$DATASET$SDC$Event.Size), non_na_zero_sd(BUYBACK_DATA$DATASET$SDC$Event.Size), non_na_zero_quantile(BUYBACK_DATA$DATASET$SDC$Event.Size,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$SDC$Event.Size,0.8)),
  c(non_na_zero_mean(BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e),non_na_zero_median(BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e), non_na_zero_sd(BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e,0.2), non_na_zero_quantile(BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e,0.8))
)
rownames(Descriptive_stats_BB) <- c("Market Cap.", "Prior Returns","BE/ME", "Volatility", "$(1-R^2)$","U-index", "EU-index","Mispricing Measure","Percent Shares","Leverage")

Descriptive_stats_ISS = rbind(
  c(non_na_zero_mean(ISSUERS_DATA$DATASET$CRSP$Market.Cap),non_na_zero_median(ISSUERS_DATA$DATASET$CRSP$Market.Cap), non_na_zero_sd(ISSUERS_DATA$DATASET$CRSP$Market.Cap), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$Market.Cap,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$Market.Cap,0.8)),
  100*c(non_na_zero_mean(ISSUERS_DATA$DATASET$CRSP$recent_performance),non_na_zero_median(ISSUERS_DATA$DATASET$CRSP$recent_performance), non_na_zero_sd(ISSUERS_DATA$DATASET$CRSP$recent_performance), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$recent_performance,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$recent_performance,0.8)),
  c(non_na_zero_mean(ISSUERS_DATA$DATASET$CRSP$BE.ME),non_na_zero_median(ISSUERS_DATA$DATASET$CRSP$BE.ME), non_na_zero_sd(ISSUERS_DATA$DATASET$CRSP$BE.ME), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$BE.ME,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$BE.ME,0.8)),
  100*c(non_na_zero_mean(ISSUERS_DATA$DATASET$CRSP$pre_vol),non_na_zero_median(ISSUERS_DATA$DATASET$CRSP$pre_vol), non_na_zero_sd(ISSUERS_DATA$DATASET$CRSP$pre_vol), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$pre_vol,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$pre_vol,0.8)),
  c(non_na_zero_mean(ISSUERS_DATA$DATASET$CRSP$Rsq),non_na_zero_median(ISSUERS_DATA$DATASET$CRSP$Rsq), non_na_zero_sd(ISSUERS_DATA$DATASET$CRSP$Rsq), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$Rsq,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$Rsq,0.8)),
  c(non_na_zero_mean(ISSUERS_DATA$DATASET$Stambaugh),non_na_zero_median(ISSUERS_DATA$DATASET$Stambaugh), non_na_zero_sd(ISSUERS_DATA$DATASET$Stambaugh), non_na_zero_quantile(ISSUERS_DATA$DATASET$Stambaugh,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$Stambaugh,0.8)),
  #c(non_na_mean(ISSUERS_DATA$Valuation_Index),non_na_median(ISSUERS_DATA$Valuation_Index), non_na_sd(ISSUERS_DATA$Valuation_Index), non_na_quantile(ISSUERS_DATA$Valuation_Index,0.2), non_na_quantile(ISSUERS_DATA$Valuation_Index,0.8)),
  #c(non_na_mean(EUindex_iss),non_na_median(EUindex_iss), non_na_sd(EUindex_iss), non_na_quantile(EUindex_iss,0.2), non_na_quantile(EUindex_iss,0.8)),
  c(non_na_zero_mean(ISSUERS_DATA$DATASET$SDC$Event.Size),non_na_zero_median(ISSUERS_DATA$DATASET$SDC$Event.Size), non_na_zero_sd(ISSUERS_DATA$DATASET$SDC$Event.Size), non_na_zero_quantile(ISSUERS_DATA$DATASET$SDC$Event.Size,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$SDC$Event.Size,0.8)),
  c(non_na_zero_mean(ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e),non_na_zero_median(ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e), non_na_zero_sd(ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e,0.2), non_na_zero_quantile(ISSUERS_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e,0.8))
)
rownames(Descriptive_stats_ISS) <- c("Market Cap.", "Prior Returns","BE/ME", "Volatility", "$(1-R^2)$","Mispricing Measure",
                                     #"U-index", "EU-index",
                                     "Percent Shares","Leverage")

####################################################################################
#cbind(IRATS_table_all_bb,IRATS_table_all_bb_undervaluation)[reported_times,],
#cbind(CAL_table_all_bb,CAL_table_all_bb_undervaluation)[reported_times,],

useonly = 1:length(BUYBACK_DATA$DATASET$SDC$CUSIP)
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_all_bb = cbind(tmp3f,tmp5f)
colnames(IRATS_table_all_bb)[1] <- "CAR 3F"
colnames(IRATS_table_all_bb)[4] <- "CAR 5F"
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
CAL_table_all_bb = cbind(tmp3f, tmp5f)
colnames(CAL_table_all_bb)[1] <- "CAL 3F"
colnames(CAL_table_all_bb)[4] <- "CAL 5F"

# U index
useonly = which(company_subset_undervalued_bb)
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(company_subset_overvalued_bb)
Otmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
Otmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
Otmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

IRATS_table_all_bb_undervaluation = cbind(tmp3f,Otmp3f,tmp5f,Otmp5f)
CAL_table_all_bb_undervaluation = cbind(tmp3f2,Otmp3f2,tmp5f2,Otmp5f2)
colnames(IRATS_table_all_bb_undervaluation)[1] <- "U:CAR3F"
colnames(IRATS_table_all_bb_undervaluation)[4] <- "O:CAR3F"
colnames(IRATS_table_all_bb_undervaluation)[7] <- "U:CAR5F"
colnames(IRATS_table_all_bb_undervaluation)[10] <- "O:CAR5F"
colnames(CAL_table_all_bb_undervaluation)[1] <- "U:CAL3F"
colnames(CAL_table_all_bb_undervaluation)[4] <- "O:CAL3F"
colnames(CAL_table_all_bb_undervaluation)[7] <- "U:CAL5F"
colnames(CAL_table_all_bb_undervaluation)[10] <- "O:CAL5F"


####################################################################################
#cbind(CAL_table_all_bb_value,CAL_table_all_bb_undervaluation_value)[reported_times,],

useonly = 1:length(BUYBACK_DATA$DATASET$SDC$CUSIP)
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
CAL_table_all_bb_value = cbind(tmp3f, tmp5f)
colnames(CAL_table_all_bb_value)[1] <- "CAL 3F"
colnames(CAL_table_all_bb_value)[4] <- "CAL 5F"

useonly = which(company_subset_undervalued_bb)
tmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
tmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
useonly = which(company_subset_overvalued_bb)
Otmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
Otmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results

CAL_table_all_bb_undervaluation_value = cbind(tmp3f2,Otmp3f2,tmp5f2,Otmp5f2)
colnames(CAL_table_all_bb_undervaluation_value)[1] <- "U:CAL3F"
colnames(CAL_table_all_bb_undervaluation_value)[4] <- "O:CAL3F"
colnames(CAL_table_all_bb_undervaluation_value)[7] <- "U:CAL5F"
colnames(CAL_table_all_bb_undervaluation_value)[10] <- "O:CAL5F"

rm("useonly","tmp3f2","tmp5f2","Otmp3f2","Otmp5f2")

####################################################################################
#cbind(CAL_table_all_bb_valueL,CAL_table_all_bb_undervaluation_valueL)[reported_times,],

useonly_valueL = which(BUYBACK_DATA$DATASET$CRSP$Market.Cap_score < 0.75)

company_subset_undervalued_bb_valueL = BUYBACK_DATA$Valuation_Index[useonly_valueL] > quantile(BUYBACK_DATA$Valuation_Index[useonly_valueL], 1-quantile_Uindex)
company_subset_overvalued_bb_valueL = BUYBACK_DATA$Valuation_Index[useonly_valueL] < quantile(BUYBACK_DATA$Valuation_Index[useonly_valueL],quantile_Uindex)

useonly = useonly_valueL
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
CAL_table_all_bb_valueL = cbind(tmp3f, tmp5f)
colnames(CAL_table_all_bb_valueL)[1] <- "CAL 3F"
colnames(CAL_table_all_bb_valueL)[4] <- "CAL 5F"

useonly = useonly_valueL[which(company_subset_undervalued_bb_valueL)]
tmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
tmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
useonly = useonly_valueL[which(company_subset_overvalued_bb_valueL)]
Otmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
Otmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results

CAL_table_all_bb_undervaluation_valueL = cbind(tmp3f2,Otmp3f2,tmp5f2,Otmp5f2)
colnames(CAL_table_all_bb_undervaluation_valueL)[1] <- "U:CAL3F"
colnames(CAL_table_all_bb_undervaluation_valueL)[4] <- "O:CAL3F"
colnames(CAL_table_all_bb_undervaluation_valueL)[7] <- "U:CAL5F"
colnames(CAL_table_all_bb_undervaluation_valueL)[10] <- "O:CAL5F"

rm("useonly","useonly_valueL","tmp3f2","tmp5f2","Otmp3f2","Otmp5f2")

####################################################################################
# High/Low EU for buybacks now
useonly = which(EUindex_bb %in% 1:3)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(EUindex_bb %in% 4:6)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

EU_IRATStableBB = cbind(tmp11, tmp12)
EU_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(EU_IRATStableBB) <- c("Low EU: CAR", "t-stat","p-value","High EU: CAR", "t-stat","p-value")
colnames(EU_IRATStableBB_cal) <- c("Low EU: CAL", "t-stat","p-value","High EU: CAL", "t-stat","p-value")

####################################################################################
# Stambaugh for buybacks now
useonly = which(Low_STAM_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_STAM_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

Stam_IRATStableBB = cbind(tmp11, tmp12)
Stam_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(Stam_IRATStableBB) <- c("Low Mis.: CAR", "t-stat","p-value","High Mis.: CAR", "t-stat","p-value")
colnames(Stam_IRATStableBB_cal) <- c("Low Mis.: CAL", "t-stat","p-value","High Mis.: CAL", "t-stat","p-value")

####################################################################################
# Stambaugh vs VOL for buybacks now

useonly = which(Low_STAM_eventsBB & Low_VOL_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_STAM_eventsBB & Low_VOL_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_STAM_eventsBB & High_VOL_eventsBB)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_STAM_eventsBB & High_VOL_eventsBB)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

Stam_IRATStable_VolBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
Stam_IRATStable_VolBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(Stam_IRATStable_VolBB) <- c("Low Mis.: L. Vol CAR", "t-stat","p-value","H. Vol CAR", "t-stat","p-value", "High Mis.: L. Vol CAR", "t-stat","p-value","H. Vol CAR", "t-stat","p-value")
colnames(Stam_IRATStable_VolBB_cal) <- c("Low Mis.: L. Vol CAL", "t-stat","p-value","H. Vol CAL", "t-stat","p-value", "High Mis.: L. Vol CAL", "t-stat","p-value","H. Vol CAL", "t-stat","p-value")

####################################################################################
# Stambaugh vs EU now, double sorting
useonly = which(Low_STAM_eventsBB & EUindex_bb %in% 1:3)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_STAM_eventsBB & EUindex_bb %in% 1:3)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_STAM_eventsBB & EUindex_bb %in% 4:6)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_STAM_eventsBB & EUindex_bb %in% 4:6)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

Stam_IRATStable_EU_BB = cbind(tmp11,Otmp11,tmp12,Otmp12)
Stam_IRATStable_EU_BB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(Stam_IRATStable_EU_BB) <- c("Low Mis.: L. EU CAR", "t-stat","p-value","H. EU CAR", "t-stat","p-value", "High Mis.: L. EU CAR", "t-stat","p-value","H. EU CAR", "t-stat","p-value")
colnames(Stam_IRATStable_EU_BB_cal) <- c("Low Mis.: L. EU CAL", "t-stat","p-value","H. EU CAL", "t-stat","p-value", "High Mis.: L. EU CAL", "t-stat","p-value","H. EU CAL", "t-stat","p-value")

####################################################################################
#IRATS_table_all_iss[reported_times,],
#CAL_table_all_iss[reported_times,],
#useonly = which(ISSUERS_DATA$DATASET$SDC$Event.Date > "2001-01-01")

useonly = 1:length(ISSUERS_DATA$DATASET$SDC$CUSIP)
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_all_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_all_iss)[1] <- "CAR 3F"
colnames(IRATS_table_all_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_all_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_all_iss)[1] <- "CAL 3F"
colnames(CAL_table_all_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

# Issuers vol
useonly = High_VOL_eventsISS 
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_highvol_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_highvol_iss)[1] <- "CAR 3F"
colnames(IRATS_table_highvol_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_highvol_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_highvol_iss)[1] <- "CAL 3F"
colnames(CAL_table_highvol_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

useonly = Low_VOL_eventsISS #& (ISSUERS_DATA$DATASET$SDC$Event.Date < "2000-01-01")
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_lowvol_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_lowvol_iss)[1] <- "CAR 3F"
colnames(IRATS_table_lowvol_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_lowvol_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_lowvol_iss)[1] <- "CAL 3F"
colnames(CAL_table_lowvol_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

# Issuers ivol
useonly = High_Idiosyncr_eventsISS
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_highidio_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_highidio_iss)[1] <- "CAR 3F"
colnames(IRATS_table_highidio_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_highidio_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_highidio_iss)[1] <- "CAL 3F"
colnames(CAL_table_highidio_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

useonly = Low_Idiosyncr_eventsISS #& (ISSUERS_DATA$DATASET$SDC$Event.Date < "2000-01-01")
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_lowidio_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_lowidio_iss)[1] <- "CAR 3F"
colnames(IRATS_table_lowidio_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_lowidio_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_lowidio_iss)[1] <- "CAL 3F"
colnames(CAL_table_lowidio_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

# Stambaugh for issuers now
useonly = High_STAM_eventsISS 
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_highMM_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_highMM_iss)[1] <- "CAR 3F"
colnames(IRATS_table_highMM_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_highMM_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_highMM_iss)[1] <- "CAL 3F"
colnames(CAL_table_highMM_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

useonly = Low_STAM_eventsISS #& (ISSUERS_DATA$DATASET$SDC$Event.Date < "2000-01-01")
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_lowMM_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_lowMM_iss)[1] <- "CAR 3F"
colnames(IRATS_table_lowMM_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_lowMM_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_lowMM_iss)[1] <- "CAL 3F"
colnames(CAL_table_lowMM_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

# Stambaugh vs VOL for issuers now
useonly = which(Low_STAM_eventsISS & Low_VOL_eventsISS)
tmp11 = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
useonly = which(High_STAM_eventsISS & Low_VOL_eventsISS)
tmp12 = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
useonly = which(Low_STAM_eventsISS & High_VOL_eventsISS)
Otmp11 = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
useonly = which(High_STAM_eventsISS & High_VOL_eventsISS)
Otmp12 = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results

Stam_IRATStable_VolISS = cbind(tmp11,Otmp11,tmp12,Otmp12)
Stam_IRATStable_VolISS_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(Stam_IRATStable_VolISS) <- c("Low Mis.: L. Vol CAR", "t-stat","p-value","H. Vol CAR", "t-stat","p-value", "High Mis.: L. Vol CAR", "t-stat","p-value","H. Vol CAR", "t-stat","p-value")
colnames(Stam_IRATStable_VolISS_cal) <- c("Low Mis.: L. Vol CAL", "t-stat","p-value","H. Vol CAL", "t-stat","p-value", "High Mis.: L. Vol CAL", "t-stat","p-value","H. Vol CAL", "t-stat","p-value")

####################################################################################
#Exit_on_SEO_Abn_table[reported_times,],
#Exit_on_SEO_Abn_table_cal[reported_times,],
useonly = which(BUYBACK_DATA$DATASET$SDC$Otherlater != "2100-01-01" & BUYBACK_DATA$DATASET$CRSP$Market.Cap <= 1000)
tmp1 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(BUYBACK_DATA$DATASET$SDC$Otherlater == "2100-01-01")
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
Exit_on_SEO_Abn_table = cbind(tmp12,tmp1)
Exit_on_SEO_Abn_table_cal = cbind(tmp22,tmp2)
colnames(Exit_on_SEO_Abn_table)[1] <- "No SEO: CAR"
colnames(Exit_on_SEO_Abn_table)[4] <- "SEO: CAR"
colnames(Exit_on_SEO_Abn_table_cal)[1] <- "No SEO: CAL"
colnames(Exit_on_SEO_Abn_table_cal)[4] <- "SEO: CAL"

rm("tmp1","tmp2","tmp12","tmp22", "useonly")

####################################################################################
#tmp = BBtable_time[reported_times,c(4,5,6,10,11,12,16,17,18,22,23,24)]
#colnames(tmp) <- c("1985-1990: CAR","t-stat","p-value","1991-2000: CAR","t-stat","p-value","2001-2015: CAR","t-stat","p-value","2008-2015: CAR","t-stat","p-value")
#tmp2 = BBtable_time_cal[reported_times,c(4,5,6,10,11,12,16,17,18,22,23,24)]
#colnames(tmp2) <- c("1985-1990: CAL","t-stat","p-value","1991-2000: CAL","t-stat","p-value","2001-2014: CAL","t-stat","p-value","2008-2014: CAL","t-stat","p-value")

periodnow = (ISSUERS_DATA$DATASET$SDC$Event.Date >= "1980-01-01" & ISSUERS_DATA$DATASET$SDC$Event.Date <= "2017-01-01")

ISStable_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (ISSUERS_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & ISSUERS_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  res = cbind(
    car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,periodnow], ISSUERS_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,periodnow], ISSUERS_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"4FF", sep=" "),
                     "t-stat","p-value", "5FF", "t-stat","p-value") 
  rownames(res)[nrow(res)] <- "Observations"
  res    
}))

ISStable_time_cal = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (ISSUERS_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & ISSUERS_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  res = cbind(
    calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,periodnow], ISSUERS_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[periodnow])$results,
    calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,periodnow], ISSUERS_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model,value.weights = value.weights_iss[periodnow])$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"4FF", sep=" "),
                     "t-stat","p-value", "5FF", "t-stat","p-value") 
  rownames(res)[nrow(res)] <- "Observations"
  res    
}))

BBtable_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  res = cbind(
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                     "t-stat","p-value", "5FF", "t-stat","p-value") 
  rownames(res)[nrow(res)] <- "Observations"
  res    
}))

BBtable_time_cal = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  res = cbind(
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[periodnow])$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model,value.weights = value.weights_bb[periodnow])$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                     "t-stat","p-value", "5FF", "t-stat","p-value") 
  rownames(res)[nrow(res)] <- "Observations"
  res    
}))


####################################################################################
#one_table = cbind(BBtable_undertime[,7:12],BB_cal_table_undertime[,7:12],
#                  BBtable_undertime[,19:24],BB_cal_table_undertime[,19:24],
#                  BBtable_undertime[,31:36],BB_cal_table_undertime[,31:36],
#                  BBtable_undertime[,43:48],BB_cal_table_undertime[,43:48]
BBtable_undertime = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_undervalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_undervalued_bb & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_overvalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_overvalued_bb & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_undervalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_undervalued_bb & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_overvalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_overvalued_bb & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                     "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  res    
}))              

BB_cal_table_undertime = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_undervalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_undervalued_bb & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[company_subset_undervalued_bb & periodnow])$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_overvalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_overvalued_bb & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[company_subset_overvalued_bb & periodnow])$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_undervalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_undervalued_bb & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model,value.weights = value.weights_bb[company_subset_undervalued_bb & periodnow])$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,company_subset_overvalued_bb & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[company_subset_overvalued_bb & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model,value.weights = value.weights_bb[company_subset_overvalued_bb & periodnow])$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                     "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  rownames(res)[nrow(res)] <- "Observations" 
  res    
}))

####################################################################################
#tmp = Reduce(cbind,lapply(1:length(industry_BB), function(i) industry_BB[[i]][,1:3] ))
#tmp_cal = Reduce(cbind,lapply(1:length(industry_BB), function(i) industry_BB[[i]][,4:6] ))

Event_Industries = BUYBACK_DATA$DATASET$SDC$Industry
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
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,this_industry], BUYBACK_DATA$DATASET$SDC$Event.Date[this_industry], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,this_industry], BUYBACK_DATA$DATASET$SDC$Event.Date[this_industry], Risk_Factors_Monthly,value.weights = value.weights_bb[this_industry])$results
  )
})


####################################################################################
#pnl_matrix(remove_initialization_time(BUYBACK_DATA$long_all12mshort_risk_factors,min_date=FirstTrade)), 
all_events = 1:length(BUYBACK_DATA$DATASET$SDC$Event.Date)
events_all_12m = PNL_matrix_BB(start_date_event,"One.Year.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1)  
pnl_returns_events_all_12M <- apply(events_all_12m,1,function(r) non_zero_mean(scrub(r)))
long_all12mshort_risk_factors = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_12M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
rm("all_events")

####################################################################################
#tmp = industry_BB_features[,c(1:4,9:10)]

industry_BB_features <- Reduce(rbind,lapply(which(industry_tableBB > 100), function(i){
  this_industry = which(Event_Industries == gsub(" ","_", names(industry_tableBB)[i]))
  if (length(this_industry) != industry_tableBB[i])
    stop("\nFunny industry problem\n")
  
  res = matrix(
    c(
      100*length(intersect(this_industry,which(High_Idiosyncr_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(Low_Idiosyncr_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(High_VOL_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(Low_VOL_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(High_LEV_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(Low_LEV_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(High_EPS_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(Low_EPS_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(High_STAM_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(Low_STAM_eventsBB)))/(length(this_industry)),
      100*length(intersect(this_industry,which(company_subset_undervalued_bb)))/(length(this_industry)),
      100*length(intersect(this_industry,which(company_subset_overvalued_bb)))/(length(this_industry))
      
    )
    , nrow=1)
  colnames(res) <- c("H Idsync.","L Idsync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.","High Mispr.","Low Mispr.","U/valued","O/valued")
  rownames(res)<-names(industry_tableBB)[i]
  res
}))



####################################################################################
#cbind(R2_IRATStableBB,R2_IRATStable_underBB)[reported_times,],
#cbind(R2_IRATStableBB_cal,R2_IRATStable_underBB_cal)[reported_times,],

## Both 4 and 5f idiosync. BB

# BB vol
useonly = High_VOL_eventsBB
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_highvol_BB = cbind(tmp3f,tmp5f)
colnames(IRATS_table_highvol_BB)[1] <- "CAR 3F"
colnames(IRATS_table_highvol_BB)[4] <- "CAR 5F"
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
CAL_table_highvol_BB = cbind(tmp3f, tmp5f)
colnames(CAL_table_highvol_BB)[1] <- "CAL 3F"
colnames(CAL_table_highvol_BB)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

useonly = Low_VOL_eventsBB #& (BUYBACK_DATA$DATASET$SDC$Event.Date < "2000-01-01")
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_lowvol_BB = cbind(tmp3f,tmp5f)
colnames(IRATS_table_lowvol_BB)[1] <- "CAR 3F"
colnames(IRATS_table_lowvol_BB)[4] <- "CAR 5F"
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
CAL_table_lowvol_BB = cbind(tmp3f, tmp5f)
colnames(CAL_table_lowvol_BB)[1] <- "CAL 3F"
colnames(CAL_table_lowvol_BB)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

# BB ivol
useonly = High_Idiosyncr_eventsBB
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_highidio_BB = cbind(tmp3f,tmp5f)
colnames(IRATS_table_highidio_BB)[1] <- "CAR 3F"
colnames(IRATS_table_highidio_BB)[4] <- "CAR 5F"
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
CAL_table_highidio_BB = cbind(tmp3f, tmp5f)
colnames(CAL_table_highidio_BB)[1] <- "CAL 3F"
colnames(CAL_table_highidio_BB)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

useonly = Low_Idiosyncr_eventsBB #& (BUYBACK_DATA$DATASET$SDC$Event.Date < "2000-01-01")
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
IRATS_table_lowidio_BB = cbind(tmp3f,tmp5f)
colnames(IRATS_table_lowidio_BB)[1] <- "CAR 3F"
colnames(IRATS_table_lowidio_BB)[4] <- "CAR 5F"
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
CAL_table_lowidio_BB = cbind(tmp3f, tmp5f)
colnames(CAL_table_lowidio_BB)[1] <- "CAL 3F"
colnames(CAL_table_lowidio_BB)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

###OLDER SPLIT...

useonly = which(Low_Idiosyncr_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

R2_IRATStableBB = cbind(tmp11, tmp12)
R2_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(R2_IRATStableBB) <- c("Low Idiosync.: CAR", "t-stat","p-value","High Idiosync.: CAR", "t-stat","p-value")
colnames(R2_IRATStableBB_cal) <- c("Low Idiosync.: CAL", "t-stat","p-value","High Idiosync.: CAL", "t-stat","p-value")

useonly = which(Low_Idiosyncr_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_Idiosyncr_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

R2_IRATStable_underBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
R2_IRATStable_underBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(R2_IRATStable_underBB) <- c("Low Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(R2_IRATStable_underBB_cal) <- c("Low Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value", "High Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")

# The value weighted calendar now
useonly = which(High_Idiosyncr_eventsBB)
R2_IRATStableBB_calValue = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results

rm("tmp11","tmp12","tmp21","tmp22","Otmp11","Otmp12","Otmp21","Otmp22", "useonly")


####################################################################################
#cbind(VOL_IRATStableBB,VOL_IRATStable_underBB)[reported_times,],
#cbind(VOL_IRATStableBB_cal,VOL_IRATStable_underBB_cal)[reported_times,],

useonly = which(Low_VOL_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_VOL_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

VOL_IRATStableBB = cbind(tmp11, tmp12)
VOL_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(VOL_IRATStableBB) <- c("Low Vol: CAR", "t-stat","p-value","High Vol: CAR", "t-stat","p-value")
colnames(VOL_IRATStableBB_cal) <- c("Low Vol: CAL", "t-stat","p-value","High Vol: CAL", "t-stat","p-value")

useonly = which(Low_VOL_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_VOL_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_VOL_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_VOL_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

VOL_IRATStable_underBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
VOL_IRATStable_underBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(VOL_IRATStable_underBB) <- c("Low Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(VOL_IRATStable_underBB_cal) <- c("Low Vol: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value", "High Vol: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")

# The value weighted calendar now
useonly = which(High_VOL_eventsBB)
VOL_IRATStableBB_calValue = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results

rm("tmp11","tmp12","tmp21","tmp22","Otmp11","Otmp12","Otmp21","Otmp22", "useonly")

####################################################################################
#VOL_IRATStableBB_cal_value

useonly = which(Low_VOL_eventsBB)
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results
useonly = which(High_VOL_eventsBB)
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results

VOL_IRATStableBB_cal_value = cbind(tmp21, tmp22)
colnames(VOL_IRATStableBB_cal_value) <- c("Low Vol: CAL", "t-stat","p-value","High Vol: CAL", "t-stat","p-value")

rm("useonly","tmp21","tmp22")

####################################################################################
#cbind(R2_IRATStableBB,R2_IRATStable_underBB)[reported_times,],
#cbind(R2_IRATStableBB_cal,R2_IRATStable_underBB_cal)[reported_times,],

useonly = which(Low_IVOL_eventsBB )
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_IVOL_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

IVOL_IRATStableBB = cbind(tmp11, tmp12)
IVOL_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(IVOL_IRATStableBB) <- c("Low Idiosync.: CAR", "t-stat","p-value","High Idiosync.: CAR", "t-stat","p-value")
colnames(IVOL_IRATStableBB_cal) <- c("Low Idiosync.: CAL", "t-stat","p-value","High Idiosync.: CAL", "t-stat","p-value")

useonly = which(Low_Idiosyncr_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_Idiosyncr_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

IVOL_IRATStable_underBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
IVOL_IRATStable_underBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(IVOL_IRATStable_underBB) <- c("Low Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(IVOL_IRATStable_underBB_cal) <- c("Low Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value", "High Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")

# The value weighted calendar now
useonly = which(High_Idiosyncr_eventsBB)
IVOL_IRATStableBB_calValue = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[useonly])$results

rm("tmp11","tmp12","tmp21","tmp22","Otmp11","Otmp12","Otmp21","Otmp22", "useonly")

####################################################################################
#cbind(marketbeta_IRATStableBB,marketbeta_IRATStable_underBB)[reported_times,],
#cbind(marketbeta_IRATStableBB_cal,marketbeta_IRATStable_underBB_cal)[reported_times,],

useonly = which(Low_marketbeta_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Medium_marketbeta_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_marketbeta_eventsBB)
tmp13 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp23 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

marketbeta_IRATStableBB = cbind(tmp11, tmp12,tmp13)
marketbeta_IRATStableBB_cal = cbind(tmp21, tmp22,tmp23)
colnames(marketbeta_IRATStableBB) <- c("Low beta: CAR", "t-stat","p-value","Medium beta: CAR", "t-stat","p-value","High beta: CAR", "t-stat","p-value")
colnames(marketbeta_IRATStableBB_cal) <- c("Low beta: CAL", "t-stat","p-value","Medium beta: CAR", "t-stat","p-value","High beta: CAL", "t-stat","p-value")

rm("tmp11","tmp12","tmp13","tmp21","tmp22","tmp23", "useonly")

useonly = which(Low_marketbeta_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_marketbeta_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_marketbeta_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_marketbeta_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

marketbeta_IRATStable_underBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
marketbeta_IRATStable_underBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(marketbeta_IRATStable_underBB) <- c("Low beta: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High beta: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(marketbeta_IRATStable_underBB_cal) <- c("Low beta: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value", "High beta: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")

rm("tmp11","tmp12","tmp21","tmp22","Otmp11","Otmp12","Otmp21","Otmp22", "useonly")

####################################################################################
#round(Under_IdioBB[1:6,1:4],1), 

Under_IdioBB = rbind(
  c(100*sum(company_subset_undervalued_bb & High_Idiosyncr_eventsBB)/sum(company_subset_undervalued_bb), 100*sum(company_subset_undervalued_bb & Low_Idiosyncr_eventsBB)/sum(company_subset_undervalued_bb),
    100*sum(company_subset_undervalued_bb & High_VOL_eventsBB)/sum(company_subset_undervalued_bb), 100*sum(company_subset_undervalued_bb & Low_VOL_eventsBB)/sum(company_subset_undervalued_bb),
    100*sum(company_subset_undervalued_bb & High_LEV_eventsBB)/sum(company_subset_undervalued_bb), 100*sum(company_subset_undervalued_bb & Low_LEV_eventsBB)/sum(company_subset_undervalued_bb),
    100*sum(company_subset_undervalued_bb & High_EPS_eventsBB)/sum(company_subset_undervalued_bb), 100*sum(company_subset_undervalued_bb & Low_EPS_eventsBB)/sum(company_subset_undervalued_bb)
  ),
  c(100*sum(company_subset_overvalued_bb & High_Idiosyncr_eventsBB)/sum(company_subset_overvalued_bb), 100*sum(company_subset_overvalued_bb & Low_Idiosyncr_eventsBB)/sum(company_subset_overvalued_bb),
    100*sum(company_subset_overvalued_bb & High_VOL_eventsBB)/sum(company_subset_overvalued_bb), 100*sum(company_subset_overvalued_bb & Low_VOL_eventsBB)/sum(company_subset_overvalued_bb),
    100*sum(company_subset_overvalued_bb & High_LEV_eventsBB)/sum(company_subset_overvalued_bb), 100*sum(company_subset_overvalued_bb & Low_LEV_eventsBB)/sum(company_subset_overvalued_bb),
    100*sum(company_subset_overvalued_bb & High_EPS_eventsBB)/sum(company_subset_overvalued_bb), 100*sum(company_subset_overvalued_bb & Low_EPS_eventsBB)/sum(company_subset_overvalued_bb)
  ),
  ###
  c(100*sum(High_Idiosyncr_eventsBB & High_Idiosyncr_eventsBB)/sum(High_Idiosyncr_eventsBB), 100*sum(High_Idiosyncr_eventsBB & Low_Idiosyncr_eventsBB)/sum(High_Idiosyncr_eventsBB),
    100*sum(High_Idiosyncr_eventsBB & High_VOL_eventsBB)/sum(High_Idiosyncr_eventsBB), 100*sum(High_Idiosyncr_eventsBB & Low_VOL_eventsBB)/sum(High_Idiosyncr_eventsBB),
    100*sum(High_Idiosyncr_eventsBB & High_LEV_eventsBB)/sum(High_Idiosyncr_eventsBB), 100*sum(High_Idiosyncr_eventsBB & Low_LEV_eventsBB)/sum(High_Idiosyncr_eventsBB),
    100*sum(High_Idiosyncr_eventsBB & High_EPS_eventsBB)/sum(High_Idiosyncr_eventsBB), 100*sum(High_Idiosyncr_eventsBB & Low_EPS_eventsBB)/sum(High_Idiosyncr_eventsBB)
  ),
  c(100*sum(Low_Idiosyncr_eventsBB & High_Idiosyncr_eventsBB)/sum(Low_Idiosyncr_eventsBB), 100*sum(Low_Idiosyncr_eventsBB & Low_Idiosyncr_eventsBB)/sum(Low_Idiosyncr_eventsBB),
    100*sum(Low_Idiosyncr_eventsBB & High_VOL_eventsBB)/sum(Low_Idiosyncr_eventsBB), 100*sum(Low_Idiosyncr_eventsBB & Low_VOL_eventsBB)/sum(Low_Idiosyncr_eventsBB),
    100*sum(Low_Idiosyncr_eventsBB & High_LEV_eventsBB)/sum(Low_Idiosyncr_eventsBB), 100*sum(Low_Idiosyncr_eventsBB & Low_LEV_eventsBB)/sum(Low_Idiosyncr_eventsBB),
    100*sum(Low_Idiosyncr_eventsBB & High_EPS_eventsBB)/sum(Low_Idiosyncr_eventsBB), 100*sum(Low_Idiosyncr_eventsBB & Low_EPS_eventsBB)/sum(Low_Idiosyncr_eventsBB)
  ),
  ###  
  c(100*sum(High_VOL_eventsBB & High_Idiosyncr_eventsBB)/sum(High_VOL_eventsBB), 100*sum(High_VOL_eventsBB & Low_Idiosyncr_eventsBB)/sum(High_VOL_eventsBB),
    100*sum(High_VOL_eventsBB & High_VOL_eventsBB)/sum(High_VOL_eventsBB), 100*sum(High_VOL_eventsBB & Low_VOL_eventsBB)/sum(High_VOL_eventsBB),
    100*sum(High_VOL_eventsBB & High_LEV_eventsBB)/sum(High_VOL_eventsBB), 100*sum(High_VOL_eventsBB & Low_LEV_eventsBB)/sum(High_VOL_eventsBB),
    100*sum(High_VOL_eventsBB & High_EPS_eventsBB)/sum(High_VOL_eventsBB), 100*sum(High_VOL_eventsBB & Low_EPS_eventsBB)/sum(High_VOL_eventsBB)
  ),
  c(100*sum(Low_VOL_eventsBB & High_Idiosyncr_eventsBB)/sum(Low_VOL_eventsBB), 100*sum(Low_VOL_eventsBB & Low_Idiosyncr_eventsBB)/sum(Low_VOL_eventsBB),
    100*sum(Low_VOL_eventsBB & High_VOL_eventsBB)/sum(Low_VOL_eventsBB), 100*sum(Low_VOL_eventsBB & Low_VOL_eventsBB)/sum(Low_VOL_eventsBB),
    100*sum(Low_VOL_eventsBB & High_LEV_eventsBB)/sum(Low_VOL_eventsBB), 100*sum(Low_VOL_eventsBB & Low_LEV_eventsBB)/sum(Low_VOL_eventsBB),
    100*sum(Low_VOL_eventsBB & High_EPS_eventsBB)/sum(Low_VOL_eventsBB), 100*sum(Low_VOL_eventsBB & Low_EPS_eventsBB)/sum(Low_VOL_eventsBB)
  ),
  ###  
  c(100*sum(High_STAM_eventsBB & High_Idiosyncr_eventsBB)/sum(High_STAM_eventsBB), 100*sum(High_STAM_eventsBB & Low_Idiosyncr_eventsBB)/sum(High_STAM_eventsBB),
    100*sum(High_STAM_eventsBB & High_VOL_eventsBB)/sum(High_STAM_eventsBB), 100*sum(High_STAM_eventsBB & Low_VOL_eventsBB)/sum(High_STAM_eventsBB),
    100*sum(High_STAM_eventsBB & High_LEV_eventsBB)/sum(High_STAM_eventsBB), 100*sum(High_STAM_eventsBB & Low_LEV_eventsBB)/sum(High_STAM_eventsBB),
    100*sum(High_STAM_eventsBB & High_EPS_eventsBB)/sum(High_STAM_eventsBB), 100*sum(High_STAM_eventsBB & Low_STAM_eventsBB)/sum(High_STAM_eventsBB)
  ),
  c(100*sum(Low_STAM_eventsBB & High_Idiosyncr_eventsBB)/sum(Low_STAM_eventsBB), 100*sum(Low_STAM_eventsBB & Low_Idiosyncr_eventsBB)/sum(Low_STAM_eventsBB),
    100*sum(Low_STAM_eventsBB & High_VOL_eventsBB)/sum(Low_STAM_eventsBB), 100*sum(Low_STAM_eventsBB & Low_VOL_eventsBB)/sum(Low_STAM_eventsBB),
    100*sum(Low_STAM_eventsBB & High_LEV_eventsBB)/sum(Low_STAM_eventsBB), 100*sum(Low_STAM_eventsBB & Low_LEV_eventsBB)/sum(Low_STAM_eventsBB),
    100*sum(Low_STAM_eventsBB & High_EPS_eventsBB)/sum(Low_STAM_eventsBB), 100*sum(Low_STAM_eventsBB & Low_EPS_eventsBB)/sum(Low_STAM_eventsBB)
  ) ,
  ###  
  c(100*sum(High_LEV_eventsBB & High_Idiosyncr_eventsBB)/sum(High_LEV_eventsBB), 100*sum(High_LEV_eventsBB & Low_Idiosyncr_eventsBB)/sum(High_LEV_eventsBB),
    100*sum(High_LEV_eventsBB & High_VOL_eventsBB)/sum(High_LEV_eventsBB), 100*sum(High_LEV_eventsBB & Low_VOL_eventsBB)/sum(High_LEV_eventsBB),
    100*sum(High_LEV_eventsBB & High_LEV_eventsBB)/sum(High_LEV_eventsBB), 100*sum(High_LEV_eventsBB & Low_LEV_eventsBB)/sum(High_LEV_eventsBB),
    100*sum(High_LEV_eventsBB & High_EPS_eventsBB)/sum(High_LEV_eventsBB), 100*sum(High_LEV_eventsBB & Low_LEV_eventsBB)/sum(High_LEV_eventsBB)
  ),
  c(100*sum(Low_LEV_eventsBB & High_Idiosyncr_eventsBB)/sum(Low_LEV_eventsBB), 100*sum(Low_LEV_eventsBB & Low_Idiosyncr_eventsBB)/sum(Low_LEV_eventsBB),
    100*sum(Low_LEV_eventsBB & High_VOL_eventsBB)/sum(Low_LEV_eventsBB), 100*sum(Low_LEV_eventsBB & Low_VOL_eventsBB)/sum(Low_LEV_eventsBB),
    100*sum(Low_LEV_eventsBB & High_LEV_eventsBB)/sum(Low_LEV_eventsBB), 100*sum(Low_LEV_eventsBB & Low_LEV_eventsBB)/sum(Low_LEV_eventsBB),
    100*sum(Low_LEV_eventsBB & High_EPS_eventsBB)/sum(Low_LEV_eventsBB), 100*sum(Low_LEV_eventsBB & Low_EPS_eventsBB)/sum(Low_LEV_eventsBB)
  )  
  
)
rownames(Under_IdioBB) <- c("Undervalued","Overvalued", "High Idiosync.", "Low Idiosync.","High Vol.", "Low Vol.","High Mispr.", "Low Mispr.","High Lev.","Low Lev." )
colnames(Under_IdioBB) <- c("H Idiosync.","L Idiosync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.")


####################################################################################
#  round(Under_IdioBB_cor,2), 
EU_index_features = scrub(cbind(1-BUYBACK_DATA$DATASET$CRSP$Rsq_score, BUYBACK_DATA$DATASET$CRSP$pre_vol_Score,BUYBACK_DATA$Valuation_Index, BUYBACK_DATA$DATASET$Stambaugh))
colnames(EU_index_features) <- c("Idiosyncratic Score", "Volatility Score", "U-Index Score", "Mispricing Measure")
Under_IdioBB_cor = cor(EU_index_features)


####################################################################################
#round(EU_relations,1), 

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
Missed_EPS = (BUYBACK_DATA$DATASET$ibes$mean_rec_last_month_score < BUYBACK_DATA$DATASET$ibes$mean_rec_last_last_month_score)
Beat_EPS = (BUYBACK_DATA$DATASET$ibes$mean_rec_last_month_score >= BUYBACK_DATA$DATASET$ibes$mean_rec_last_last_month_score)
low_epsunc = 0*EUindex_bb
low_epsunc[Low_EPS_eventsBB] <-1
ISS_Later = ifelse((BUYBACK_DATA$DATASET$SDC$OtherlaterEvent != 0), "Yes", "No")
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
all_characteristics_continuous = cbind(BUYBACK_DATA$DATASET$CRSP$Market.Cap, BUYBACK_DATA$DATASET$CRSP$BE.ME_score, BUYBACK_DATA$DATASET$SDC$Event.Size,BUYBACK_DATA$DATASET$Stambaugh)
EU_relations_continuous = t(apply(all_characteristics_continuous,2,function(r){
  sapply(sort(unique(EUindex_bb)), function(i) {
    useonly = which(EUindex_bb == i)
    mean(r[useonly][!is.na(r[useonly])])
  })
}))
rownames(EU_relations_continuous) <- c("Market Cap.", "BE/ME Score", "Percentage Shares", "Mispricing Measure")
EU_relations = rbind(EU_relations, round(EU_relations_continuous,2))


####################################################################################
#EU_IRATStable_bb[reported_times,1:(3*4)],
#EU_IRATStable_bb[reported_times,13:(3*7)],

EU_long_bb = NULL
EU_Hedged_bb = NULL
EU_long48_bb = NULL
EU_Hedged48_bb = NULL
EU_IRATStable_bb = NULL
EU_CALtable_bb = NULL
for (i in 0:6){
  EU_events_now = which(EUindex_bb == i)
  EU = apply(PNL_matrix_BB(start_date_event,"One.Year.After", EU_events_now,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", EU_events_now,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU48m_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU_long_bb = cbind(EU_long_bb,EU)
  EU_Hedged_bb = cbind(EU_Hedged_bb,EU_hedged)
  EU_long48_bb = cbind(EU_long48_bb,EU48m)
  EU_Hedged48_bb = cbind(EU_Hedged48_bb, EU48m_hedged)
  EU_IRATStable_bb = cbind(EU_IRATStable_bb,car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,EU_events_now], BUYBACK_DATA$DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly,formula_used=five_factor_model)$results)
  EU_CALtable_bb = cbind(EU_CALtable_bb,calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,EU_events_now], BUYBACK_DATA$DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly,value.weights = value.weights_bb[EU_events_now])$results)
}
rm("EU","EU_hedged","EU48m", "EU48m_hedged","i")


####################################################################################
#EU_CALtable_bb_value[reported_times,1:(3*4)],
#EU_CALtable_bb_value[reported_times,13:(3*7)],

EU_CALtable_bb_value = NULL
for (i in 0:6){
  tmp = which(EUindex_bb %in% i)
  tmp_cal = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,tmp], BUYBACK_DATA$DATASET$SDC$Event.Date[tmp], Risk_Factors_Monthly,value.weights = BUYBACK_DATA$DATASET$CRSP$Market.Cap[tmp])$results
  EU_CALtable_bb_value = cbind(EU_CALtable_bb_value,tmp_cal)
}
rm("tmp","i","tmp_cal")

################################################################################
##ROBUST OVER TIME
high_EU = (EUindex_bb %in% 4:6)
low_EU = (EUindex_bb %in% 0:3)

EIRATS_table_undervaluation_time_bb = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    #car_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    #car_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,high_EU & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,low_EU & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
  )
  #colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
  #                   "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  
  res    
}))

ECAL_table_undervaluation_time_bb = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA$DATASET$SDC$Event.Date <= periods_considered[i,2])
  
  res = cbind(
    #calendar_table(DATASET$returns_by_event_monthly[,high_EU & periodnow], DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    #calendar_table(DATASET$returns_by_event_monthly[,low_EU & periodnow], DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,high_EU & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model,value.weights = value.weights_bb[high_EU & periodnow])$results,
    calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,low_EU & periodnow], BUYBACK_DATA$DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model,value.weights = value.weights_bb[low_EU & periodnow])$results
  )
  #colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
  #                   "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  
  res    
}))


####################################################################################
#EU_CALtable_bb[reported_times,1:(3*4)],
#EU_CALtable_bb[reported_times,13:(3*7)],

# Done above


####################################################################################
#EIRATS_table_undervaluation_time_bb[reported_times,1:(3*4)],
#EIRATS_table_undervaluation_time_bb[reported_times,13:(3*8)],

# Done above

####################################################################################
#ECAL_table_undervaluation_time_bb[reported_times,1:(3*4)],
#ECAL_table_undervaluation_time_bb[reported_times,13:(3*8)],

# Done above

####################################################################################
#tmp = table(valuation_index_bb)
valuation_index_bb = BUYBACK_DATA$Valuation_Index


####################################################################################
#IRATS_table_all_bb_plot = IRATS_table_all_bb
#IRATS_table_all_iss_plot = IRATS_table_all_iss
#IRATS_table_all_bb_undervaluation_plot = IRATS_table_all_bb_undervaluation

# Done above.

####################################################################################
#BB_with_ISS_later_total

tmp = exit_helper_rnw(BUYBACK_DATA,"Otherlater",holding_period_pnl = "Four.Years.After")
BB_with_ISS_later = tmp$exit_events
BB_with_noISS_later = tmp$noexit_events
BB_ISS_Exit_Hedged = tmp$pnl_Exit_Hedged
BB_ISS_NoExit_Hedged = tmp$pnl_NoExit_Hedged
BB_with_ISS_later_total = tmp$number_events
Exit_on_SEO_Abn_table = round(cbind(
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_noISS_later], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_ISS_later], Risk_Factors_Monthly,formula_used=five_factor_model)$results
),2)
colnames(Exit_on_SEO_Abn_table) <- c("No SEO: CAR", "t-stat", "p-value","SEO: CAR", "t-stat", "p-value")
rownames(Exit_on_SEO_Abn_table)[nrow(Exit_on_SEO_Abn_table)] <- "Observations"

Exit_on_SEO_Abn_table_cal = round(cbind(
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_noISS_later], Risk_Factors_Monthly,value.weights = value.weights_bb[BB_with_noISS_later])$results,
  calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_ISS_later], Risk_Factors_Monthly,value.weights = value.weights_bb[BB_with_ISS_later])$results
),2)
colnames(Exit_on_SEO_Abn_table_cal) <- c("No SEO: CAL", "t-stat", "p-value","SEO: CAL", "t-stat", "p-value")
rownames(Exit_on_SEO_Abn_table_cal)[nrow(Exit_on_SEO_Abn_table_cal)] <- "Observations"
rm("tmp")

####################################################################################
#Exit_on_SEO_Abn_table

# Done above.

####################################################################################
# Other variables used in the .Rnw

datasummaryBB = rbind(
  round(c(summary(BUYBACK_DATA$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA$DATASET$SDC$Event.Size) & BUYBACK_DATA$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(BUYBACK_DATA$DATASET$SDC$Event.Size[!is.na(BUYBACK_DATA$DATASET$SDC$Event.Size) & BUYBACK_DATA$DATASET$SDC$Event.Size!=0]),sum(is.na(BUYBACK_DATA$DATASET$SDC$Event.Size) | BUYBACK_DATA$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(BUYBACK_DATA$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap) & BUYBACK_DATA$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(BUYBACK_DATA$DATASET$CRSP$Market.Cap[!is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap) & BUYBACK_DATA$DATASET$CRSP$Market.Cap!=0]),sum(is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap) | BUYBACK_DATA$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(BUYBACK_DATA$DATASET$CRSP$BE.ME[BUYBACK_DATA$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA$DATASET$CRSP$BE.ME) & BUYBACK_DATA$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(BUYBACK_DATA$DATASET$CRSP$BE.ME[BUYBACK_DATA$DATASET$CRSP$BE.ME < 1e20 & !is.na(BUYBACK_DATA$DATASET$CRSP$BE.ME) & BUYBACK_DATA$DATASET$CRSP$BE.ME!=0]),sum(is.na(BUYBACK_DATA$DATASET$CRSP$BE.ME >= 1e20) | BUYBACK_DATA$DATASET$CRSP$BE.ME ==0 | BUYBACK_DATA$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryBB) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryBB)[ncol(datasummaryBB)-1] <- "std"
colnames(datasummaryBB)[ncol(datasummaryBB)] <- "Missing"

datasummaryISS = rbind(
  round(c(summary(ISSUERS_DATA$DATASET$SDC$Event.Size[!is.na(ISSUERS_DATA$DATASET$SDC$Event.Size) & ISSUERS_DATA$DATASET$SDC$Event.Size!=0])[c(1,3,4,6)], sd(ISSUERS_DATA$DATASET$SDC$Event.Size[!is.na(ISSUERS_DATA$DATASET$SDC$Event.Size) & ISSUERS_DATA$DATASET$SDC$Event.Size!=0]),sum(is.na(ISSUERS_DATA$DATASET$SDC$Event.Size) | ISSUERS_DATA$DATASET$SDC$Event.Size==0)),1),
  round(c(summary(ISSUERS_DATA$DATASET$CRSP$Market.Cap[!is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap) & ISSUERS_DATA$DATASET$CRSP$Market.Cap!=0])[c(1,3,4,6)], sd(ISSUERS_DATA$DATASET$CRSP$Market.Cap[!is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap) & ISSUERS_DATA$DATASET$CRSP$Market.Cap!=0]),sum(is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap) | ISSUERS_DATA$DATASET$CRSP$Market.Cap==0)),1),
  round(c(summary(ISSUERS_DATA$DATASET$CRSP$BE.ME[ISSUERS_DATA$DATASET$CRSP$BE.ME < 1e20 & !is.na(ISSUERS_DATA$DATASET$CRSP$BE.ME) & ISSUERS_DATA$DATASET$CRSP$BE.ME!=0])[c(1,3,4,6)], sd(ISSUERS_DATA$DATASET$CRSP$BE.ME[ISSUERS_DATA$DATASET$CRSP$BE.ME < 1e20 & !is.na(ISSUERS_DATA$DATASET$CRSP$BE.ME) & ISSUERS_DATA$DATASET$CRSP$BE.ME!=0]),sum(is.na(ISSUERS_DATA$DATASET$CRSP$BE.ME >= 1e20) | ISSUERS_DATA$DATASET$CRSP$BE.ME ==0 | ISSUERS_DATA$DATASET$CRSP$BE.ME==0)),1))
rownames(datasummaryISS) <- c("Percent authorized", "Market cap.", "BE/ME")
colnames(datasummaryISS)[ncol(datasummaryISS)-1] <- "std"
colnames(datasummaryISS)[ncol(datasummaryISS)] <- "Missing"


# Get all holding periods event returns
all_events = 1:length(BUYBACK_DATA$DATASET$SDC$Event.Date)
BUYBACK_DATA$events_all_1m = scrub(PNL_matrix_BB(start_date_event,"One.Month.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1))
BUYBACK_DATA$events_all_3m = scrub(PNL_matrix_BB(start_date_event,"Three.Month.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1)) 
BUYBACK_DATA$events_all_6m = scrub(PNL_matrix_BB(start_date_event,"Six.Month.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1))  
BUYBACK_DATA$events_all_12m = scrub(PNL_matrix_BB(start_date_event,"One.Year.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1)) 
BUYBACK_DATA$events_all_24m = scrub(PNL_matrix_BB(start_date_event,"Two.Years.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1))  
BUYBACK_DATA$events_all_36m = scrub(PNL_matrix_BB(start_date_event,"Three.Years.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1))  
BUYBACK_DATA$events_all_48m = scrub(PNL_matrix_BB(start_date_event,"Four.Years.After", all_events,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1))  

# now get all holding period pnls
BUYBACK_DATA$pnl_returns_events_all_1M =  apply(BUYBACK_DATA$events_all_1m,1,function(r) non_zero_mean(scrub(r)))
BUYBACK_DATA$pnl_returns_events_all_3M = apply(BUYBACK_DATA$events_all_3m,1,function(r) non_zero_mean(scrub(r)))
BUYBACK_DATA$pnl_returns_events_all_6M <- apply(BUYBACK_DATA$events_all_6m,1,function(r) non_zero_mean(scrub(r)))
BUYBACK_DATA$pnl_returns_events_all_12M <- apply(BUYBACK_DATA$events_all_12m,1,function(r) non_zero_mean(scrub(r)))
BUYBACK_DATA$pnl_returns_events_all_24M =  apply(BUYBACK_DATA$events_all_24m,1,function(r) non_zero_mean(scrub(r)))
BUYBACK_DATA$pnl_returns_events_all_36M = apply(BUYBACK_DATA$events_all_36m,1,function(r) non_zero_mean(scrub(r)))
BUYBACK_DATA$pnl_returns_events_all_48M <- apply(BUYBACK_DATA$events_all_48m,1,function(r) non_zero_mean(scrub(r)))

#... and hedge them to get the trading strategies  for ALL companies  for all holding periods
BUYBACK_DATA$long_all1mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_1M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
BUYBACK_DATA$long_all3mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_3M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
BUYBACK_DATA$long_all6mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_6M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
BUYBACK_DATA$long_all12mshort_risk_factors = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_12M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
BUYBACK_DATA$long_all24mshort_risk_factors = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_24M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
BUYBACK_DATA$long_all36mshort_risk_factors = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_36M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
BUYBACK_DATA$long_all48mshort_risk_factors = suppressWarnings(scrub(alpha_lm(BUYBACK_DATA$pnl_returns_events_all_48M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))

# ... and get the live events for different holding periods
BUYBACK_DATA$live_events1m  = apply(BUYBACK_DATA$events_all_1m!=0,1,sum)
BUYBACK_DATA$live_events3m  = apply(BUYBACK_DATA$events_all_3m!=0,1,sum)
BUYBACK_DATA$live_events6m  = apply(BUYBACK_DATA$events_all_6m!=0,1,sum)
BUYBACK_DATA$live_events12m  = apply(BUYBACK_DATA$events_all_12m!=0,1,sum)
BUYBACK_DATA$live_events24m  = apply(BUYBACK_DATA$events_all_24m!=0,1,sum)
BUYBACK_DATA$live_events36m  = apply(BUYBACK_DATA$events_all_36m!=0,1,sum)
BUYBACK_DATA$live_events48m  = apply(BUYBACK_DATA$events_all_48m!=0,1,sum)
rm("all_events")

tmp = get_pnl_results_stock_subset(BUYBACK_DATA$DATASET,High_Idiosyncr_eventsBB,Low_Idiosyncr_eventsBB,Risk_Factors_Monthly,pnl_hedge_factors)
High_Idiosyncr_BB_Hedged  = tmp$High_feature_Hedged; Low_Idiosyncr_BB_Hedged = tmp$Low_feature_Hedged
High_Idiosyncr_BB48m = tmp$High_feature48m; Low_Idiosyncr_BB48m  = tmp$Low_feature48m
High_Idiosyncr_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_Idiosyncr_BB_Hedged48m = tmp$Low_feature_Hedged48m

tmp = get_pnl_results_stock_subset(BUYBACK_DATA$DATASET,High_VOL_eventsBB,Low_VOL_eventsBB,Risk_Factors_Monthly,pnl_hedge_factors)
High_VOL_BB_Hedged  = tmp$High_feature_Hedged; Low_VOL_BB_Hedged = tmp$Low_feature_Hedged
High_VOL_BB48m = tmp$High_feature48m; Low_VOL_BB48m  = tmp$Low_feature48m
High_VOL_BB_Hedged48m  = tmp$High_feature_Hedged48m; Low_VOL_BB_Hedged48m = tmp$Low_feature_Hedged48m

high_EU_bb = high_EU
low_EU_bb = low_EU
High_EU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", high_EU_bb,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
High_EU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_EU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
Low_EU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", low_EU_bb,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
Low_EU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_EU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
High_EU_BB48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", high_EU_bb,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
High_EU_BB_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_EU_BB48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
Low_EU_BB48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", low_EU_bb,  BUYBACK_DATA$DATASET$DatesMonth, BUYBACK_DATA$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
Low_EU_BB_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_EU_BB48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    

rm("tmp")

### THESE WE DON'T USE ANY MORE
if (0){
  
  ########################################################################################################
  # Short term analysis
  
  load("../FinanceData/created_projects_datasets/BUYBACK_otherdata.Rdata")
  #logistic_data_all = logistic.data.all.yearly 
  #yearly.regression.variable = yearly.regression.variable.yearly
  Daily_Returns = Buyback_Daily_Returns[,paste(BUYBACK_DATA$DATASET$SDC$permno, BUYBACK_DATA$DATASET$SDC$Event.Date, sep="_")]
  rm("Buyback_Daily_Returns")
  
  ### VOL
  Low_project_eventsBB = Low_VOL_eventsBB & company_subset_undervalued_bb
  High_project_eventsBB = High_VOL_eventsBB & company_subset_undervalued_bb
  
  Low_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_varM10M1 = Low_VOL_varM10M1[!is.na(Low_VOL_varM10M1)]
  High_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),High_project_eventsBB], 2, sum)
  High_VOL_varM10M1 = High_VOL_varM10M1[!is.na(High_VOL_varM10M1)]
  Low_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),Low_project_eventsBB], 2, sum)
  Low_VOL_var01 = Low_VOL_var01[!is.na(Low_VOL_var01)]
  High_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),High_project_eventsBB], 2, sum)
  High_VOL_var01 = High_VOL_var01[!is.na(High_VOL_var01)]
  Low_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_var2_10 = Low_VOL_var2_10[!is.na(Low_VOL_var2_10)]
  High_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),High_project_eventsBB], 2, sum)
  High_VOL_var2_10 = High_VOL_var2_10[!is.na(High_VOL_var2_10)]
  
  tmpUnder = rbind(
    c(mean(Low_VOL_varM10M1),mean(High_VOL_varM10M1), mean(Low_VOL_varM10M1) - mean(High_VOL_varM10M1)),
    c(t.test(Low_VOL_varM10M1)$statistic,t.test(High_VOL_varM10M1)$statistic,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$statistic),
    c(t.test(Low_VOL_varM10M1)$p.value,t.test(High_VOL_varM10M1)$p.value,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$p.value),
    
    c(mean(Low_VOL_var01),mean(High_VOL_var01), mean(Low_VOL_var01) - mean(High_VOL_var01)),
    c(t.test(Low_VOL_var01)$statistic,t.test(High_VOL_var01)$statistic,t.test(Low_VOL_var01,High_VOL_var01)$statistic),
    c(t.test(Low_VOL_var01)$p.value,t.test(High_VOL_var01)$p.value,t.test(Low_VOL_var01,High_VOL_var01)$p.value),
    
    c(mean(Low_VOL_var2_10),mean(High_VOL_var2_10), mean(Low_VOL_var2_10) - mean(High_VOL_var2_10)),
    c(t.test(Low_VOL_var2_10)$statistic,t.test(High_VOL_var2_10)$statistic,t.test(Low_VOL_var2_10,High_VOL_var2_10)$statistic),
    c(t.test(Low_VOL_var2_10)$p.value,t.test(High_VOL_var2_10)$p.value,t.test(Low_VOL_var2_10,High_VOL_var2_10)$p.value)
  )
  rownames(tmpUnder) <- c("Days -10:-1","t-stat","p-value","Days 0:+1","t-stat","p-value","Days +2:+10","t-stat","p-value")
  colnames(tmpUnder) <- c("High U-index: Low Vol.","High Vol.", "Low-High Vol.")
  
  ##
  Low_project_eventsBB = Low_VOL_eventsBB & company_subset_overvalued_bb
  High_project_eventsBB = High_VOL_eventsBB & company_subset_overvalued_bb
  
  Low_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_varM10M1 = Low_VOL_varM10M1[!is.na(Low_VOL_varM10M1)]
  High_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),High_project_eventsBB], 2, sum)
  High_VOL_varM10M1 = High_VOL_varM10M1[!is.na(High_VOL_varM10M1)]
  Low_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),Low_project_eventsBB], 2, sum)
  Low_VOL_var01 = Low_VOL_var01[!is.na(Low_VOL_var01)]
  High_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),High_project_eventsBB], 2, sum)
  High_VOL_var01 = High_VOL_var01[!is.na(High_VOL_var01)]
  Low_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_var2_10 = Low_VOL_var2_10[!is.na(Low_VOL_var2_10)]
  High_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),High_project_eventsBB], 2, sum)
  High_VOL_var2_10 = High_VOL_var2_10[!is.na(High_VOL_var2_10)]
  
  tmpOver = rbind(
    c(mean(Low_VOL_varM10M1),mean(High_VOL_varM10M1), mean(Low_VOL_varM10M1) - mean(High_VOL_varM10M1)),
    c(t.test(Low_VOL_varM10M1)$statistic,t.test(High_VOL_varM10M1)$statistic,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$statistic),
    c(t.test(Low_VOL_varM10M1)$p.value,t.test(High_VOL_varM10M1)$p.value,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$p.value),
    
    c(mean(Low_VOL_var01),mean(High_VOL_var01), mean(Low_VOL_var01) - mean(High_VOL_var01)),
    c(t.test(Low_VOL_var01)$statistic,t.test(High_VOL_var01)$statistic,t.test(Low_VOL_var01,High_VOL_var01)$statistic),
    c(t.test(Low_VOL_var01)$p.value,t.test(High_VOL_var01)$p.value,t.test(Low_VOL_var01,High_VOL_var01)$p.value),
    
    c(mean(Low_VOL_var2_10),mean(High_VOL_var2_10), mean(Low_VOL_var2_10) - mean(High_VOL_var2_10)),
    c(t.test(Low_VOL_var2_10)$statistic,t.test(High_VOL_var2_10)$statistic,t.test(Low_VOL_var2_10,High_VOL_var2_10)$statistic),
    c(t.test(Low_VOL_var2_10)$p.value,t.test(High_VOL_var2_10)$p.value,t.test(Low_VOL_var2_10,High_VOL_var2_10)$p.value)
  )
  rownames(tmpOver) <- c("Days -10:-1","t-stat","p-value","Days 0:+1","t-stat","p-value","Days +2:+10","t-stat","p-value")
  colnames(tmpOver) <- c("Low U-index: Low Vol.","High Vol.", "Low-High Vol.")
  
  short_term_returns_VOL = cbind(tmpOver,tmpUnder)
  
  ### 1-R2
  Low_project_eventsBB = Low_Idiosyncr_eventsBB & company_subset_undervalued_bb
  High_project_eventsBB = High_Idiosyncr_eventsBB & company_subset_undervalued_bb
  
  Low_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_varM10M1 = Low_VOL_varM10M1[!is.na(Low_VOL_varM10M1)]
  High_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),High_project_eventsBB], 2, sum)
  High_VOL_varM10M1 = High_VOL_varM10M1[!is.na(High_VOL_varM10M1)]
  Low_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),Low_project_eventsBB], 2, sum)
  Low_VOL_var01 = Low_VOL_var01[!is.na(Low_VOL_var01)]
  High_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),High_project_eventsBB], 2, sum)
  High_VOL_var01 = High_VOL_var01[!is.na(High_VOL_var01)]
  Low_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_var2_10 = Low_VOL_var2_10[!is.na(Low_VOL_var2_10)]
  High_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),High_project_eventsBB], 2, sum)
  High_VOL_var2_10 = High_VOL_var2_10[!is.na(High_VOL_var2_10)]
  
  tmpUnder = rbind(
    c(mean(Low_VOL_varM10M1),mean(High_VOL_varM10M1), mean(Low_VOL_varM10M1) - mean(High_VOL_varM10M1)),
    c(t.test(Low_VOL_varM10M1)$statistic,t.test(High_VOL_varM10M1)$statistic,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$statistic),
    c(t.test(Low_VOL_varM10M1)$p.value,t.test(High_VOL_varM10M1)$p.value,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$p.value),
    
    c(mean(Low_VOL_var01),mean(High_VOL_var01), mean(Low_VOL_var01) - mean(High_VOL_var01)),
    c(t.test(Low_VOL_var01)$statistic,t.test(High_VOL_var01)$statistic,t.test(Low_VOL_var01,High_VOL_var01)$statistic),
    c(t.test(Low_VOL_var01)$p.value,t.test(High_VOL_var01)$p.value,t.test(Low_VOL_var01,High_VOL_var01)$p.value),
    
    c(mean(Low_VOL_var2_10),mean(High_VOL_var2_10), mean(Low_VOL_var2_10) - mean(High_VOL_var2_10)),
    c(t.test(Low_VOL_var2_10)$statistic,t.test(High_VOL_var2_10)$statistic,t.test(Low_VOL_var2_10,High_VOL_var2_10)$statistic),
    c(t.test(Low_VOL_var2_10)$p.value,t.test(High_VOL_var2_10)$p.value,t.test(Low_VOL_var2_10,High_VOL_var2_10)$p.value)
  )
  rownames(tmpUnder) <- c("Days -10:-1","t-stat","p-value","Days 0:+1","t-stat","p-value","Days +2:+10","t-stat","p-value")
  colnames(tmpUnder) <- c("High U-index: Low Idio.","High Idio.", "Low-High Idio.")
  
  ##
  Low_project_eventsBB = Low_Idiosyncr_eventsBB & company_subset_overvalued_bb
  High_project_eventsBB = High_Idiosyncr_eventsBB & company_subset_overvalued_bb
  
  Low_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_varM10M1 = Low_VOL_varM10M1[!is.na(Low_VOL_varM10M1)]
  High_VOL_varM10M1 = 100*apply(Daily_Returns[paste("-",10:1, sep=""),High_project_eventsBB], 2, sum)
  High_VOL_varM10M1 = High_VOL_varM10M1[!is.na(High_VOL_varM10M1)]
  Low_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),Low_project_eventsBB], 2, sum)
  Low_VOL_var01 = Low_VOL_var01[!is.na(Low_VOL_var01)]
  High_VOL_var01 = 100*apply(Daily_Returns[c("0","+1"),High_project_eventsBB], 2, sum)
  High_VOL_var01 = High_VOL_var01[!is.na(High_VOL_var01)]
  Low_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),Low_project_eventsBB], 2, sum)
  Low_VOL_var2_10 = Low_VOL_var2_10[!is.na(Low_VOL_var2_10)]
  High_VOL_var2_10 = 100*apply(Daily_Returns[paste("+",2:10,sep=""),High_project_eventsBB], 2, sum)
  High_VOL_var2_10 = High_VOL_var2_10[!is.na(High_VOL_var2_10)]
  
  tmpOver = rbind(
    c(mean(Low_VOL_varM10M1),mean(High_VOL_varM10M1), mean(Low_VOL_varM10M1) - mean(High_VOL_varM10M1)),
    c(t.test(Low_VOL_varM10M1)$statistic,t.test(High_VOL_varM10M1)$statistic,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$statistic),
    c(t.test(Low_VOL_varM10M1)$p.value,t.test(High_VOL_varM10M1)$p.value,t.test(Low_VOL_varM10M1,High_VOL_varM10M1)$p.value),
    
    c(mean(Low_VOL_var01),mean(High_VOL_var01), mean(Low_VOL_var01) - mean(High_VOL_var01)),
    c(t.test(Low_VOL_var01)$statistic,t.test(High_VOL_var01)$statistic,t.test(Low_VOL_var01,High_VOL_var01)$statistic),
    c(t.test(Low_VOL_var01)$p.value,t.test(High_VOL_var01)$p.value,t.test(Low_VOL_var01,High_VOL_var01)$p.value),
    
    c(mean(Low_VOL_var2_10),mean(High_VOL_var2_10), mean(Low_VOL_var2_10) - mean(High_VOL_var2_10)),
    c(t.test(Low_VOL_var2_10)$statistic,t.test(High_VOL_var2_10)$statistic,t.test(Low_VOL_var2_10,High_VOL_var2_10)$statistic),
    c(t.test(Low_VOL_var2_10)$p.value,t.test(High_VOL_var2_10)$p.value,t.test(Low_VOL_var2_10,High_VOL_var2_10)$p.value)
  )
  rownames(tmpOver) <- c("Days -10:-1","t-stat","p-value","Days 0:+1","t-stat","p-value","Days +2:+10","t-stat","p-value")
  colnames(tmpOver) <- c("Low U-index: Low Idio.","High Idio.", "Low-High Idio.")
  
  short_term_returns_Idiosync = cbind(tmpOver,tmpUnder)
  
  ###################################################
  # Cumulative short term
  
  Daily_Returns_ini = Daily_Returns
  
  Daily_Returns = apply(Daily_Returns_ini,2,function(r){
    tmp = cumsum(r)
    #tmp <- tmp - tmp[10]
    tmp
  })
  
  ## VOL
  Low_project_eventsBB = Low_VOL_eventsBB & company_subset_undervalued_bb
  High_project_eventsBB = High_VOL_eventsBB & company_subset_undervalued_bb
  
  low_project_short = t(apply(Daily_Returns[,Low_project_eventsBB], 1, function(r) c(100*mean(r[!is.na(r)]),t.test((r[!is.na(r)]))$statistic,t.test((r[!is.na(r)]))$p.value)))
  high_project_short = t(apply(Daily_Returns[,High_project_eventsBB], 1, function(r) c(100*mean(r[!is.na(r)]),t.test((r[!is.na(r)]))$statistic,t.test((r[!is.na(r)]))$p.value)))
  low_high_project_short = t(apply(Daily_Returns, 1, function(r) {
    lows = r[Low_project_eventsBB]
    highs = r[High_project_eventsBB]
    c(100*mean(lows[!is.na(lows)]) - 100*mean(highs[!is.na(highs)]), t.test(lows[!is.na(lows)], highs[!is.na(highs)])$statistic,t.test(lows[!is.na(lows)], highs[!is.na(highs)])$p.value)
  }))
  short_term_table = cbind(low_project_short,high_project_short,low_high_project_short)
  short_term_table = rbind(short_term_table, c(rep(sum(Low_project_eventsBB),3), rep(sum(High_project_eventsBB),3), c(rep(0,3))))
  rownames(short_term_table)[nrow(short_term_table)]<- "Observations"
  colnames(short_term_table) <- c("Low Vol.","t-stat", "p-value","High Vol.","t-stat", "p-value","Low-High Vol.","t-stat", "p-value")
  
  short_term_table_VOL = short_term_table
  short_term_table_VOL[which(apply(short_term_table,1,function(r) sum(is.na(r))!=0)),] <- rep(c(0,0,1),5)
  rm("short_term_table")
  
  #### 1-R2
  Low_project_eventsBB = Low_Idiosyncr_eventsBB & company_subset_undervalued_bb
  High_project_eventsBB = High_Idiosyncr_eventsBB & company_subset_undervalued_bb
  
  low_project_short = t(apply(Daily_Returns[,Low_project_eventsBB], 1, function(r) c(100*mean(r[!is.na(r)]),t.test((r[!is.na(r)]))$statistic,t.test((r[!is.na(r)]))$p.value)))
  high_project_short = t(apply(Daily_Returns[,High_project_eventsBB], 1, function(r) c(100*mean(r[!is.na(r)]),t.test((r[!is.na(r)]))$statistic,t.test((r[!is.na(r)]))$p.value)))
  low_high_project_short = t(apply(Daily_Returns, 1, function(r) {
    lows = r[Low_project_eventsBB]
    highs = r[High_project_eventsBB]
    c(100*mean(lows[!is.na(lows)]) - 100*mean(highs[!is.na(highs)]), t.test(lows[!is.na(lows)], highs[!is.na(highs)])$statistic,t.test(lows[!is.na(lows)], highs[!is.na(highs)])$p.value)
  }))
  short_term_table = cbind(low_project_short,high_project_short,low_high_project_short)
  short_term_table = rbind(short_term_table, c(rep(sum(Low_project_eventsBB),3), rep(sum(High_project_eventsBB),3), c(rep(0,3))))
  rownames(short_term_table)[nrow(short_term_table)]<- "Observations"
  colnames(short_term_table) <- c("Low Idio.","t-stat", "p-value","High Idio.","t-stat", "p-value","Low-High Idio.","t-stat", "p-value")
  
  short_term_table_Idiosync = short_term_table
  short_term_table_Idiosync[which(apply(short_term_table,1,function(r) sum(is.na(r))!=0)),] <- rep(c(0,0,1),5)
  rm("short_term_table")
  
  ###
  Daily_Returns = Daily_Returns_ini
  
  rm("Daily_Returns_ini","low_project_short","high_project_short","low_high_project_short","Low_project_eventsBB","High_project_eventsBB",
     "Low_VOL_varM10M1","High_VOL_varM10M1","Low_VOL_var01","High_VOL_var01","Low_VOL_var2_10","High_VOL_var2_10",
     "logistic.data.all.monthly", "logistic.data.all.yearly", "buybacks.events", "Daily_Returns")
  
  ################################################################################################################################
  # Logistic regression for buyback event prediction, AND the daily returns of the buyback events
  ################################################################################################################################
  
  library("sampleSelection")
  winsorize <- function(r) {r0 = r[!is.na(r)]; minval = quantile(r[!is.na(r)],0.01); maxval= quantile(r[!is.na(r)],0.99); r[!is.na(r)] <- ifelse(r[!is.na(r)] < minval | r[!is.na(r)] > maxval, NA, r[!is.na(r)]); r}
  
  load("../FinanceData/created_projects_datasets/BUYBACK_otherdata.Rdata")
  #logistic_data_all = logistic.data.all.yearly 
  #yearly.regression.variable = yearly.regression.variable.yearly
  logistic_data_all = logistic.data.all.monthly 
  yearly.regression.variable = yearly.regression.variable.monthly
  Daily_Returns = Buyback_Daily_Returns[,paste(BUYBACK_DATA$DATASET$SDC$permno, BUYBACK_DATA$DATASET$SDC$Event.Date, sep="_")]
  rm("logistic.data.all.yearly","logistic.data.all.monthly","buybacks.events","Buyback_Daily_Returns")
  
  
  load("../FinanceData/created_monthly_data/GLOBAL_MONTHLY_DATABASE.Rdata")
  GLOBAL_MONTHLY_DATABASE$volatility_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$volatility)
  GLOBAL_MONTHLY_DATABASE$volatility_month_score <- get_cross_section_score(GLOBAL_MONTHLY_DATABASE$volatility_month)
  logistic_data_all$VOL <- yearly.regression.variable(GLOBAL_MONTHLY_DATABASE$volatility)
  logistic_data_all$VOL_score <- yearly.regression.variable(GLOBAL_MONTHLY_DATABASE$volatility_score)
  logistic_data_all$VOL_month <- yearly.regression.variable(GLOBAL_MONTHLY_DATABASE$volatility_month)
  logistic_data_all$VOL_month_score <- yearly.regression.variable(GLOBAL_MONTHLY_DATABASE$volatility_month_score)
  
  rm("GLOBAL_MONTHLY_DATABASE")
  
  # THE SPECIFIC TO THIS PROJECT CHOICES
  logistic_IV_used_thisproject = 
    c("VOL_month_score",
      "year.dummies","industry.dummies.2digit"
    )
  
  
  # START FROM WHERE WE START HAVING BUYBACK DATA + 2 years (to get the previous buybacks)
  tmp = as.Date(paste(str_sub(rownames(logistic_data_all), start=7, end=13), "01",sep="-"))
  useonly = tmp >= (min(BUYBACK_DATA$DATASET$SDC$Event.Date) + 2*370)
  logistic_data_all = logistic_data_all[useonly,]
  
  winsorise_vars <- c(
    "size.all.score", 
    "MEBE.score", 
    "prior.returns.all.score",
    "Total.Payout", 
    "Total.Payout.last.year",
    "Leverage", 
    "profitability", 
    "operating.income", 
    "non.operating.income", 
    "std.operating.income", 
    "dividend.payout.ratio", 
    "liquid.assets", 
    "price.earnings.ratio", 
    "capital.expenditures", 
    "institutional.ratio.all", 
    "institutional.number.all"
  )
  
  if (length(winsorise_vars) > 0)
    logistic_data_all[,winsorise_vars] <- apply(logistic_data_all[,winsorise_vars], 2, winsorize)
  
  logistic_IV_used= c(
    "buybacks.events.past2years", 
    "size.all.score", 
    "MEBE.score", 
    "prior.returns.all.score",
    "Total.Payout", 
    "Total.Payout.last.year",
    "Leverage", 
    "profitability", 
    "operating.income", 
    "non.operating.income", 
    "std.operating.income", 
    "dividend.payout.ratio", 
    "liquid.assets", 
    "price.earnings.ratio", 
    "capital.expenditures", 
    "institutional.ratio.all", 
    "institutional.number.all",
    logistic_IV_used_thisproject
  )
  
  ###
  logistic_data = logistic_data_all[,c(logistic_IV_used,"buybacks.events.this.period")]
  logistic_data = logistic_data[apply(logistic_data,1,function(r) sum(is.na(r))) == 0,]
  names(logistic_data) <- str_replace_all(names(logistic_data), "\\.","")
  
  the_logistic_formula = paste("buybackseventsthisperiod ~ ", paste(str_replace_all(logistic_IV_used, "\\.",""), collapse = " + " ), sep=" ")
  the_logistic_formula = as.formula(the_logistic_formula)
  
  logistic_regression_buyback = glm(formula = the_logistic_formula, family = "binomial", data = logistic_data)
  logistic_regression_buyback = summary(logistic_regression_buyback)$coefficients
  logistic_regression_buyback = logistic_regression_buyback[,c(1,3,4)]
  colnames(logistic_regression_buyback) <- c("Coeff.","t-stat","p-value")
  
  probit_regression_buyback_glm = glm(formula = the_logistic_formula, family = binomial(link = "probit"), data = logistic_data)
  probit_regression_buyback = summary(probit_regression_buyback_glm)$coefficients
  probit_regression_buyback = probit_regression_buyback[,c(1,3,4)]
  colnames(probit_regression_buyback) <- c("Coeff.","t-stat","p-value")
  
  logistic_regression_buyback_project = logistic_regression_buyback[!str_detect(rownames(logistic_regression_buyback),"dummi"),]
  probit_regression_buyback_project = probit_regression_buyback[!str_detect(rownames(probit_regression_buyback),"dummi"),]
  
  rm("logistic_IV_used_thisproject","logistic_regression_buyback","probit_regression_buyback","the_logistic_formula","logistic_IV_used")
  
  
  ########################################################################################################
  # Cross sectional analyses
  ########################################################################################################
  # Get the predicted post-announce abnormal returns (as developed by "../BuybacksApril2016/bb_issuers_new.R")
  load("../FinanceData/created_projects_datasets/BUYBACKSnew_BSC1998_event_study_factor_coeffs.Rdata")
  events_used = paste(BUYBACK_DATA$DATASET$SDC$permno, BUYBACK_DATA$DATASET$SDC$Event.Date, sep=" ")
  Estimated_returns = 100*Estimated_returns[events_used,]
  BUYBACK_PreEvent_Factor_coeffs = BUYBACK_PreEvent_Factor_coeffs[events_used]
  rm("events_used","BUYBACK_PreEvent_Factor_coeffs") # we don't need for now BUYBACK_PreEvent_Factor_coeffs
  
  company_features_all = as.data.frame(all_characteristics_continuous_summary)
  
  ####################################
  # Some project specific parameters
  tmp = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score
  names(tmp) <- paste(str_sub(AddMonths(BUYBACK_DATA$DATASET$SDC$Event.Date,-1),start = 1, end=7), BUYBACK_DATA$DATASET$SDC$permno, sep=" ")
  tmp = tmp[rownames(company_features_all)]
  project.main.IV.variable = tmp
  company_features_all$project.main.IV.variable = project.main.IV.variable
  cross_regressions_variables_individual = NULL
  cross_regressions_variables_complete = NULL
  
  
  # Add dummies
  year_dummies_cross = sapply(BUYBACK_DATA$DATASET$SDC$Event.Date, function(i) str_sub(i, start=1,end=4))
  industry_dummies_cross = as.character(BUYBACK_DATA$DATASET$CRSP$SICH)
  industry_dummies_cross_2digit = ifelse(!is.na(BUYBACK_DATA$DATASET$CRSP$SICH), str_sub(as.character(BUYBACK_DATA$DATASET$CRSP$SICH), start=1,end=2), NA)
  company_features_all$year_dummies_cross = year_dummies_cross
  company_features_all$industry_dummies_cross = industry_dummies_cross
  company_features_all$industry_dummies_cross_2digit = industry_dummies_cross_2digit
  
  ## PROJECT SPECIFIC VARIABLES
  # NONE
  
  cross_regressions_variables_individual = c(
    "Market Cap. (Score)",
    "BE/ME (Score)",
    "Prior Returns (Score)",
    "U-index",
    "EU-index", 
    #"Volatility (Score)",
    "One minus Rsq (Score)",
    "Analyst Coverage (Score)", 
    "Percent Shares", 
    "Profitability (ROA)",
    "Net Debt",
    "Tax Rate",
    "Lag Dividend Payout Ratio",
    "Leverage",
    "Institutional Holdings (Score)",
    cross_regressions_variables_individual,
    "project.main.IV.variable"
  )
  
  cross_regressions_variables_complete = c(
    "Market Cap. (Score)",
    "BE/ME (Score)",
    "Prior Returns (Score)",
    #"Volatility (Score)",
    "One minus Rsq (Score)",
    "Analyst Coverage (Score)", 
    "Percent Shares", 
    "Profitability (ROA)",
    "Net Debt",
    "Tax Rate",
    "Lag Dividend Payout Ratio",
    "Leverage",
    "Institutional Holdings (Score)",
    cross_regressions_variables_complete,
    "project.main.IV.variable"
  )
  
  nomissing_allowed = c("alphaT", "project.main.IV.variable",cross_regressions_variables_complete)
  
  useonly = 1:length(BUYBACK_DATA$DATASET$SDC$CUSIP)
  
  company_features = company_features_all[,c(cross_regressions_variables_complete,"year_dummies_cross","industry_dummies_cross_2digit")]
  BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,],company_features[useonly,,drop=F],timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
  BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
  rownames(BSC1998_completemodel) <- c("Intercept",cross_regressions_variables_complete)
  rownames(BSC1998_completemodel)[which(rownames(BSC1998_completemodel) == "project.main.IV.variable")] <- "VOL"
  
  BSC1998_individual_regression = Reduce(rbind,lapply(cross_regressions_variables_individual, function(i){
    #cat(i,",")
    remove_vars_report = function(varname) !str_detect(varname,"dummies")
    BSC1998_coefficients <- BSC1998_event_study_coeffs(Estimated_returns[useonly,,drop=F],company_features_all[useonly,c(i,"year_dummies_cross","industry_dummies_cross_2digit"),drop=F], timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed,remove_vars_report = remove_vars_report)
    BSC1998_completemodel =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients)
    BSC1998_completemodel[2:nrow(BSC1998_completemodel),]
  }))
  rownames(BSC1998_individual_regression) <- cross_regressions_variables_individual
  rownames(BSC1998_individual_regression)[which(rownames(BSC1998_individual_regression) == "project.main.IV.variable")] <- VOL
  
  ########################################################################################
  ### HECKMAN
  ########################################################################################
  
  HeckmanIMR1 = invMillsRatio(probit_regression_buyback_glm)$IMR1
  names(HeckmanIMR1) <- rownames(logistic_data)
  rownames_second_stage_rename = paste(str_sub(rownames(company_features_all), start=9, end=13), str_sub(rownames(company_features_all), start=1,end=7), sep=" ")
  tmp = match(rownames_second_stage_rename,names(HeckmanIMR1))
  HeckmanIMR1_matched = ifelse(is.na(tmp), NA, HeckmanIMR1[tmp])
  
  company_features_heckman = company_features_all[,c(cross_regressions_variables_complete,"year_dummies_cross","industry_dummies_cross_2digit")]
  company_features_heckman$HeckmanIMR1 = HeckmanIMR1_matched
  company_features_heckman = company_features_heckman[!is.na(HeckmanIMR1_matched),]
  Estimated_returns_heckman <- Estimated_returns[!is.na(HeckmanIMR1_matched),]
  BSC1998_coefficients_heckman <- BSC1998_event_study_coeffs(Estimated_returns_heckman,company_features_heckman,timeperiods_requested = 1:48, square_features = NULL,nomissing_allowed)
  BSC1998_completemodel_heckman =BSC1998_coeffs_tmp_aggregator(BSC1998_coefficients_heckman)
  rownames(BSC1998_completemodel_heckman) <- c("Intercept", cross_regressions_variables_complete,"Heckman Inv. Mills")
  rownames(BSC1998_completemodel_heckman)[which(rownames(BSC1998_completemodel_heckman) == "project.main.IV.variable")] <- "VOL"
  
  rm("HeckmanIMR1","tmp","HeckmanIMR1_matched","Estimated_returns_heckman",
     "BSC1998_coefficients_heckman","company_features_heckman","rownames_second_stage_rename",
     "Estimated_returns", "logistic_data_all", "probit_regression_buyback_glm","useonly")
  
}
########################################################################################################
# Keep all results ONLY IN THIS FILE just in case we need to rerun or add something... this is temporary

save(list = setdiff(ls(all = TRUE),initial_vars), file = "bb_issuers_new.Rdata")


########################################################################################################
# Other temp tests

if (0){
  
  x=table(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start=1, end=7))
  useonly = which(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start=1,end=7) %in% names(x[(x > quantile(x,0.2)) & (x < quantile(x,0.8))]))
  useonly = which(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start=1,end=7) %in% names(x[x < quantile(x,0.4)]))
  for (iter in c(0.1*(1:9))){
    useonly = which(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start=1,end=7) %in% names(x[x <= quantile(x,iter)]))
    cat("\nBottom percentile:",(iter)*100,"- Number of events: ", length(useonly),"\n")
    car = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    calendar = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
    print(round(cbind(car[reported_times,], calendar[reported_times,]),2))
  }
  
  for (iter in c(0.1*(1:10))){
    useonly = which(str_sub(BUYBACK_DATA$DATASET$SDC$Event.Date, start=1,end=7) %in% names(x[x < quantile(x,iter) & x >= quantile(x,iter-0.1)]))
    cat("\nDecile:",(iter-0.1)*100, "to", (iter)*100,"- Number of events: ", length(useonly),"\n")
    cat("U-index:",round(mean(BUYBACK_DATA$Valuation_Index[useonly]),2), ", EU index:", round(mean(EU_index[useonly]),2))
  }
  
  buybacks_volume = rep(0,as.numeric(max(BUYBACK_DATA$DATASET$SDC$Event.Date) - min(BUYBACK_DATA$DATASET$SDC$Event.Date)))
  names(buybacks_volume) <- sapply(1:length(buybacks_volume), function(i) as.character(min(BUYBACK_DATA$DATASET$SDC$Event.Date)+i-1))
  tmp = match(as.character(BUYBACK_DATA$DATASET$SDC$Event.Date), names(buybacks_volume))
  for (iter in 1:length(tmp))
    buybacks_volume[tmp[iter]] <- buybacks_volume[tmp[iter]]+1
  buybacks_volume_roll = ms(buybacks_volume,20)
  names(buybacks_volume_roll) <- names(buybacks_volume) 
  pre_event_volume = buybacks_volume_roll[tmp-1]
  
  useonly = which(buybacks_volume_roll > 50)
  car = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
  calendar = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
  print(round(cbind(car[reported_times,], calendar[reported_times,]),2))
  
}
