
####################################################################################
# These are one by one the tables in the .Rnw. 
####################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

source("../FinanceLibraries/lib_helpers.R", chdir=TRUE)
source("../FinanceLibraries/latex_code.R")
source("../FinanceData/rawdata_fama_french/ff_industries_sic.R")
source("Paper_global_parameters.R")

initial_vars = ls(all = TRUE) # takes time to save and load, so we save only what is needed at the end. 

load("../FinanceData/created_projects_datasets/BUYBACKSnew.Rdata")

if (continuous_valuation_index){
  BUYBACK_DATA$Valuation_Index = 
  ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$recent_performance_score), NA, (1-BUYBACK_DATA$DATASET$CRSP$recent_performance_score)) +
  ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$Market.Cap_score), NA, (1-BUYBACK_DATA$DATASET$CRSP$Market.Cap_score)) + 
  ifelse(is.na(BUYBACK_DATA$DATASET$CRSP$BE.ME_score), NA, BUYBACK_DATA$DATASET$CRSP$BE.ME_score)
  
  ISSUERS_DATA$Valuation_Index = 
    ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$recent_performance_score), NA, (1-ISSUERS_DATA$DATASET$CRSP$recent_performance_score)) +
    ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$Market.Cap_score), NA, (1-ISSUERS_DATA$DATASET$CRSP$Market.Cap_score)) + 
    ifelse(is.na(ISSUERS_DATA$DATASET$CRSP$BE.ME_score), NA, ISSUERS_DATA$DATASET$CRSP$BE.ME_score)

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
############################################################################################################
# All the data filters are done in here
source("filter_bbissuers_data.R")

############################################################################################################

BUYBACK_DATA$DATASET$DatesMonth <- create_dates_month(BUYBACK_DATA$DATASET$SDC$Event.Date, rownames(BUYBACK_DATA$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(BUYBACK_DATA$DATASET$DatesMonth) <- BUYBACK_DATA$DATASET$SDC$permno

ISSUERS_DATA$DATASET$DatesMonth <- create_dates_month(ISSUERS_DATA$DATASET$SDC$Event.Date, rownames(ISSUERS_DATA$Risk_Factors_Monthly)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
colnames(ISSUERS_DATA$DATASET$DatesMonth) <- ISSUERS_DATA$DATASET$SDC$permno

Risk_Factors_Monthly = BUYBACK_DATA$Risk_Factors_Monthly
Market_Monthly = BUYBACK_DATA$Market_Monthly

if (do.value.weight == 1){   
  value.weights_bb = BUYBACK_DATA$DATASET$CRSP$Market.Cap
  value.weights_iss = ISSUERS_DATA$DATASET$CRSP$Market.Cap
} else {
  value.weights_bb = rep(1,length(BUYBACK_DATA$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
  value.weights_iss = rep(1,length(ISSUERS_DATA$DATASET$CRSP$Market.Cap)) # This is the first version of the paper - no value weighted calendar method
}

company_subset_undervalued_bb = BUYBACK_DATA$Valuation_Index > quantile(BUYBACK_DATA$Valuation_Index, 1-quantile_Uindex)
company_subset_overvalued_bb = BUYBACK_DATA$Valuation_Index < quantile(BUYBACK_DATA$Valuation_Index,quantile_Uindex)
High_Idiosyncr_eventsBB = BUYBACK_DATA$DATASET$CRSP$Rsq_score < quantile(BUYBACK_DATA$DATASET$CRSP$Rsq_score, quantile_R2)
Low_Idiosyncr_eventsBB  = BUYBACK_DATA$DATASET$CRSP$Rsq_score > quantile(BUYBACK_DATA$DATASET$CRSP$Rsq_score, 1-quantile_R2)
High_IVOL_eventsBB = BUYBACK_DATA$DATASET$CRSP$IVOL_score > quantile(BUYBACK_DATA$DATASET$CRSP$IVOL_score, 1-quantile_VOL)
Low_IVOL_eventsBB  = BUYBACK_DATA$DATASET$CRSP$IVOL_score < quantile(BUYBACK_DATA$DATASET$CRSP$IVOL_score, quantile_VOL)
High_VOL_eventsBB = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score > quantile(BUYBACK_DATA$DATASET$CRSP$pre_vol_Score, 1-quantile_VOL)
Low_VOL_eventsBB  = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score < quantile(BUYBACK_DATA$DATASET$CRSP$pre_vol_Score, quantile_VOL)
High_marketbeta_eventsBB = BUYBACK_DATA$DATASET$CRSP$market_beta_score > quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, 1-quantile_VOL)
Medium_marketbeta_eventsBB  = BUYBACK_DATA$DATASET$CRSP$market_beta_score >= quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, quantile_VOL) & BUYBACK_DATA$DATASET$CRSP$market_beta_score <= quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, 1-quantile_VOL)
Low_marketbeta_eventsBB  = BUYBACK_DATA$DATASET$CRSP$market_beta_score < quantile(BUYBACK_DATA$DATASET$CRSP$market_beta_score, quantile_VOL)
tmp = BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e
High_LEV_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_LEV)  & !is.na(tmp)
Low_LEV_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_LEV)  & !is.na(tmp)
tmp = BUYBACK_DATA$DATASET$ibes$month_minus1$mean_rec_score
High_EPS_eventsBB = scrub(tmp) > quantile(tmp[!is.na(tmp)], 1-quantile_EPS)  & !is.na(tmp)
Low_EPS_eventsBB  = scrub(tmp) < quantile(tmp[!is.na(tmp)], quantile_EPS)  & !is.na(tmp)
rm("tmp")


####################################################################################
#cbind(IRATS_table_all_bb,IRATS_table_all_bb_undervaluation)[reported_times,],
#cbind(CAL_table_all_bb,CAL_table_all_bb_undervaluation)[reported_times,],

useonly = 1:length(BUYBACK_DATA$DATASET$SDC$CUSIP)
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
IRATS_table_all_bb = cbind(tmp3f,tmp5f)
colnames(IRATS_table_all_bb)[1] <- "CAR 3F"
colnames(IRATS_table_all_bb)[4] <- "CAR 5F"
tmp3f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
CAL_table_all_bb = cbind(tmp3f, tmp5f)
colnames(CAL_table_all_bb)[1] <- "CAL 3F"
colnames(CAL_table_all_bb)[4] <- "CAL 5F"

useonly = which(company_subset_undervalued_bb)
tmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
tmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp5f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(company_subset_overvalued_bb)
Otmp3f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
Otmp3f2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_bb[useonly])$results
Otmp5f = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
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

rm("useonly","tmp3f","tmp3f2","tmp5f","tmp5f2","Otmp3f","Otmp3f2","Otmp5f","Otmp5f2")

####################################################################################
#IRATS_table_all_iss[reported_times,],
#CAL_table_all_iss[reported_times,],

useonly = 1:length(ISSUERS_DATA$DATASET$SDC$CUSIP)
tmp3f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model)$results
tmp5f = car_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
IRATS_table_all_iss = cbind(tmp3f,tmp5f)
colnames(IRATS_table_all_iss)[1] <- "CAR 3F"
colnames(IRATS_table_all_iss)[4] <- "CAR 5F"
tmp3f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,formula_used=three_factor_model,value.weights = value.weights_iss[useonly])$results
tmp5f = calendar_table(ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly], ISSUERS_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_iss[useonly])$results
CAL_table_all_iss = cbind(tmp3f, tmp5f)
colnames(CAL_table_all_iss)[1] <- "CAL 3F"
colnames(CAL_table_all_iss)[4] <- "CAL 5F"
rm("tmp3f","tmp5f")

####################################################################################
#Exit_on_SEO_Abn_table[reported_times,],
#Exit_on_SEO_Abn_table_cal[reported_times,],
useonly = which(BUYBACK_DATA$DATASET$SDC$Otherlater != "2100-01-01")
tmp1 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp2 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(BUYBACK_DATA$DATASET$SDC$Otherlater == "2100-01-01")
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
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
    car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,this_industry], BUYBACK_DATA$DATASET$SDC$Event.Date[this_industry], Risk_Factors_Monthly)$results,
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
      100*length(intersect(this_industry,which(company_subset_undervalued_bb)))/(length(this_industry)),
      100*length(intersect(this_industry,which(company_subset_overvalued_bb)))/(length(this_industry))
      
    )
    , nrow=1)
  colnames(res) <- c("H Idsync.","L Idsync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.","U/valued","O/valued")
  rownames(res)<-names(industry_tableBB)[i]
  res
}))



####################################################################################
#cbind(R2_IRATStableBB,R2_IRATStable_underBB)[reported_times,],
#cbind(R2_IRATStableBB_cal,R2_IRATStable_underBB_cal)[reported_times,],

useonly = which(Low_Idiosyncr_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

R2_IRATStableBB = cbind(tmp11, tmp12)
R2_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(R2_IRATStableBB) <- c("Low Idiosync.: CAR", "t-stat","p-value","High Idiosync.: CAR", "t-stat","p-value")
colnames(R2_IRATStableBB_cal) <- c("Low Idiosync.: CAL", "t-stat","p-value","High Idiosync.: CAL", "t-stat","p-value")

useonly = which(Low_Idiosyncr_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_Idiosyncr_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_Idiosyncr_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

R2_IRATStable_underBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
R2_IRATStable_underBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(R2_IRATStable_underBB) <- c("Low Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Idiosync.: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(R2_IRATStable_underBB_cal) <- c("Low Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value", "High Idiosync.: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")

rm("tmp11","tmp12","tmp21","tmp22","Otmp11","Otmp12","Otmp21","Otmp22", "useonly")

####################################################################################
#cbind(VOL_IRATStableBB,VOL_IRATStable_underBB)[reported_times,],
#cbind(VOL_IRATStableBB_cal,VOL_IRATStable_underBB_cal)[reported_times,],

useonly = which(Low_VOL_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_VOL_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

VOL_IRATStableBB = cbind(tmp11, tmp12)
VOL_IRATStableBB_cal = cbind(tmp21, tmp22)
colnames(VOL_IRATStableBB) <- c("Low Vol: CAR", "t-stat","p-value","High Vol: CAR", "t-stat","p-value")
colnames(VOL_IRATStableBB_cal) <- c("Low Vol: CAL", "t-stat","p-value","High Vol: CAL", "t-stat","p-value")

useonly = which(Low_VOL_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_VOL_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_VOL_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_VOL_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
Otmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

VOL_IRATStable_underBB = cbind(tmp11,Otmp11,tmp12,Otmp12)
VOL_IRATStable_underBB_cal = cbind(tmp21,Otmp21,tmp22,Otmp22)
colnames(VOL_IRATStable_underBB) <- c("Low Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High Vol: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
colnames(VOL_IRATStable_underBB_cal) <- c("Low Vol: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value", "High Vol: U CAL", "t-stat","p-value","O CAL", "t-stat","p-value")

rm("tmp11","tmp12","tmp21","tmp22","Otmp11","Otmp12","Otmp21","Otmp22", "useonly")


####################################################################################
#cbind(marketbeta_IRATStableBB,marketbeta_IRATStable_underBB)[reported_times,],
#cbind(marketbeta_IRATStableBB_cal,marketbeta_IRATStable_underBB_cal)[reported_times,],

useonly = which(Low_marketbeta_eventsBB)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Medium_marketbeta_eventsBB)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_marketbeta_eventsBB)
tmp13 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp23 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results

marketbeta_IRATStableBB = cbind(tmp11, tmp12,tmp13)
marketbeta_IRATStableBB_cal = cbind(tmp21, tmp22,tmp23)
colnames(marketbeta_IRATStableBB) <- c("Low beta: CAR", "t-stat","p-value","Medium beta: CAR", "t-stat","p-value","High beta: CAR", "t-stat","p-value")
colnames(marketbeta_IRATStableBB_cal) <- c("Low beta: CAL", "t-stat","p-value","Medium beta: CAR", "t-stat","p-value","High beta: CAL", "t-stat","p-value")

rm("tmp11","tmp12","tmp13","tmp21","tmp22","tmp23", "useonly")

useonly = which(Low_marketbeta_eventsBB & company_subset_undervalued_bb)
tmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_marketbeta_eventsBB & company_subset_undervalued_bb)
tmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
tmp22 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(Low_marketbeta_eventsBB & company_subset_overvalued_bb)
Otmp11 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
Otmp21 = calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly,value.weights = value.weights_bb[useonly])$results
useonly = which(High_marketbeta_eventsBB & company_subset_overvalued_bb)
Otmp12 = car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly], BUYBACK_DATA$DATASET$SDC$Event.Date[useonly], Risk_Factors_Monthly)$results
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
rownames(Under_IdioBB) <- c("Undervalued","Overvalued", "High Idiosync.", "Low Idiosync.","High Vol.", "Low Vol.","High Lev.","Low Lev." )
colnames(Under_IdioBB) <- c("H Idiosync.","L Idiosync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.")


####################################################################################
#  round(Under_IdioBB_cor,2), 
EU_index_features = scrub(cbind(1-BUYBACK_DATA$DATASET$CRSP$Rsq_score, BUYBACK_DATA$DATASET$CRSP$pre_vol_Score,BUYBACK_DATA$Valuation_Index))
colnames(EU_index_features) <- c("Idiosyncratic Score", "Volatility Score", "U-Index Score")
Under_IdioBB_cor = cor(EU_index_features)


####################################################################################
#round(EU_relations,1), 
EUindex_bb = sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
  ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
    ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
    ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
})

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
all_characteristics_continuous = cbind(BUYBACK_DATA$DATASET$CRSP$Market.Cap, BUYBACK_DATA$DATASET$CRSP$BE.ME_score, BUYBACK_DATA$DATASET$SDC$Event.Size)
EU_relations_continuous = t(apply(all_characteristics_continuous,2,function(r){
  sapply(sort(unique(EUindex_bb)), function(i) {
    useonly = which(EUindex_bb == i)
    mean(r[useonly][!is.na(r[useonly])])
  })
}))
rownames(EU_relations_continuous) <- c("Market Cap.", "BE/ME Score", "Percentage Shares")
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
  EU_IRATStable_bb = cbind(EU_IRATStable_bb,car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,EU_events_now], BUYBACK_DATA$DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly)$results)
  EU_CALtable_bb = cbind(EU_CALtable_bb,calendar_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,EU_events_now], BUYBACK_DATA$DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly,value.weights = value.weights_bb[EU_events_now])$results)
}
rm("EU","EU_hedged","EU48m", "EU48m_hedged","i")

##ROBUST OVER TIME
high_EU = (EUindex_bb %in% 5:6)
low_EU = (EUindex_bb %in% 0:1)

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
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_noISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_noISS_later], Risk_Factors_Monthly)$results,
  car_table(BUYBACK_DATA$DATASET$returns_by_event_monthly[,BB_with_ISS_later], BUYBACK_DATA$DATASET$SDC$Event.Date[BB_with_ISS_later], Risk_Factors_Monthly)$results
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

########################################################################################################
########################################################################################################
# Keep all results ONLY IN THIS FILE just in case we need to rerun or add something... this is temporary

save(list = setdiff(ls(all = TRUE),initial_vars), file = "bb_issuers_new.Rdata")


