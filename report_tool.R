#disable for debugging
#load("bb_issuersALL.Rdata")
report_months_cal = c(as.numeric(reported_times[1:(length(reported_times)-1)]),1)
report_months_car = sort(c((-3:24)*2,1))
reported_times_plots <-  c("-6","-4","-2","0","+1","+2","+4","+6","+8","+10","+12","+14","+16","+18","+20","+22","+24","+26","+28","+30","+32","+34","+36","+38","+40","+42","+44","+46","+48")  

#general variables and overwriting 
useonly_bb = which(BUYBACK_DATA$DATASET$SDC$Event.Date >= input$startdate & BUYBACK_DATA$DATASET$SDC$Event.Date <= input$enddate & scrub(BUYBACK_DATA$DATASET$SDC$Market.Cap) >= input$market_cap_min & scrub(BUYBACK_DATA$DATASET$SDC$Market.Cap) <= input$market_cap_max) 
useonly_is = which(ISSUERS_DATA$DATASET$SDC$Event.Date >= input$startdate & ISSUERS_DATA$DATASET$SDC$Event.Date <= input$enddate & scrub(ISSUERS_DATA$DATASET$SDC$Market.Cap) >= input$market_cap_min & scrub(ISSUERS_DATA$DATASET$SDC$Market.Cap) <= input$market_cap_max) 

ret_bb <- BUYBACK_DATA$DATASET$returns_by_event_monthly[,useonly_bb]
sdc_bb <- BUYBACK_DATA$DATASET$SDC[useonly_bb,]
subset_undervalued_bb <- company_subset_undervalued_bb[useonly_bb]
subset_overvalued_bb <- company_subset_overvalued_bb[useonly_bb]
datesmonth_bb <- BUYBACK_DATA$DATASET$DatesMonth[,useonly_bb]
EUindex_bb2 <- EUindex_bb[useonly_bb]

ret_is <- ISSUERS_DATA$DATASET$returns_by_event_monthly[,useonly_is]
sdc_is <- ISSUERS_DATA$DATASET$SDC[useonly_is,]
company_subset_undervalued_is <- company_subset_undervalued_iss[useonly_is]
company_subset_overvalued_is <- company_subset_overvalued_iss[useonly_is]
datesmonth_is <- ISSUERS_DATA$DATASET$DatesMonth[,useonly_is]

if(0) { #remove periods: not done now
  valididx <- NULL
  for(i in 1:dim(periods_considered)[1]) {
    if(periods_considered[i,1] >= input$startdate && periods_considered[i,2] <= input$enddate) {
      valididx <- c(valididx,i)
    }
  }
  periods_considered <- periods_considered[valididx,]
}
#****************************
# overwrite variables
BUYBACK_DATA2 <- BUYBACK_DATA
BUYBACK_DATA2$DATASET$SDC <- sdc_bb
BUYBACK_DATA2$DATASET$DatesMonth <- datesmonth_bb
BUYBACK_DATA2$DATASET$returns_by_event_monthly <- ret_bb
BUYBACK_DATA2$Valuation_Index <- BUYBACK_DATA$Valuation_Index[useonly_bb]

ISSUERS_DATA2 <- ISSUERS_DATA
ISSUERS_DATA2$DATASET$SDC <- sdc_is
ISSUERS_DATA2$DATASET$DatesMonth <- datesmonth_is
ISSUERS_DATA2$DATASET$returns_by_event_monthly <- ret_is
ISSUERS_DATA2$Valuation_Index <- ISSUERS_DATA$Valuation_Index[useonly_is]
 
######################
# CACHE CAR TABLE
CACHE3 <- car_table_cache(ret_bb,sdc_bb$Event.Date,Risk_Factors_Monthly,formula_used = three_factor_model)
CACHE5 <- car_table_cache(ret_bb,sdc_bb$Event.Date,Risk_Factors_Monthly,formula_used = five_factor_model)

CACHE3IS <- car_table_cache(ret_is,sdc_is$Event.Date,Risk_Factors_Monthly,formula_used = three_factor_model)
CACHE5IS <- car_table_cache(ret_is,sdc_is$Event.Date,Risk_Factors_Monthly,formula_used = five_factor_model)


#****************************
# Table modification 1
IRATS_table_all_3f = car_table_cached(CACHE3,allmonths=report_months_car)
IRATS_table_all_5f = car_table_cached(CACHE5,allmonths=report_months_car)
IRATS_table_all    = round(cbind(IRATS_table_all_3f$results, IRATS_table_all_5f$results),2)
colnames(IRATS_table_all) <- c("CAR 3F", "t-stat", "p-value", "CAR 5F", "t-stat", "p-value")  
IRATS_table_all_bb = IRATS_table_all

IRATS_table_undervaluation_undervalued3f = car_table_cached(CACHE3,subset_undervalued_bb,report_months_car)
IRATS_table_undervaluation_overvalued3f = car_table_cached(CACHE3,subset_overvalued_bb,report_months_car)
IRATS_table_undervaluation_undervalued5f = car_table_cached(CACHE5,subset_undervalued_bb,report_months_car)
IRATS_table_undervaluation_overvalued5f = car_table_cached(CACHE5,subset_overvalued_bb,report_months_car)
IRATS_table_undervaluation = round(cbind(
  IRATS_table_undervaluation_undervalued3f$results,
  IRATS_table_undervaluation_overvalued3f$results,
  IRATS_table_undervaluation_undervalued5f$results,
  IRATS_table_undervaluation_overvalued5f$results
),2)
colnames(IRATS_table_undervaluation) <- c("U:CAR3F", "t-stat", "p-value", "O:CAR3F", "t-stat", "p-value","U:CAR5F", "t-stat", "p-value", "O:CAR5F", "t-stat", "p-value")
IRATS_table_all_bb_undervaluation <- IRATS_table_undervaluation

CAL_table_all_3f = calendar_table2(ret_bb, sdc_bb$Event.Date, Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)
CAL_table_all_5f = calendar_table2(ret_bb, sdc_bb$Event.Date, Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)
CAL_table_all = round(cbind(CAL_table_all_3f$results, CAL_table_all_5f$results),2)
colnames(CAL_table_all) <- c("CAL 3F", "t-stat", "p-value", "CAL 5F", "t-stat", "p-value")  
CAL_table_all_bb <- CAL_table_all

CAL_table_undervaluation_undervalued3f = calendar_table2(ret_bb[,subset_undervalued_bb], sdc_bb$Event.Date[subset_undervalued_bb], Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)
CAL_table_undervaluation_overvalued3f = calendar_table2(ret_bb[,subset_overvalued_bb], sdc_bb$Event.Date[subset_overvalued_bb], Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)
CAL_table_undervaluation_undervalued5f = calendar_table2(ret_bb[,subset_undervalued_bb], sdc_bb$Event.Date[subset_undervalued_bb], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)
CAL_table_undervaluation_overvalued5f = calendar_table2(ret_bb[,subset_overvalued_bb], sdc_bb$Event.Date[subset_overvalued_bb], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)
CAL_table_undervaluation = round(cbind(
  CAL_table_undervaluation_undervalued3f$results,
  CAL_table_undervaluation_overvalued3f$results,
  CAL_table_undervaluation_undervalued5f$results,
  CAL_table_undervaluation_overvalued5f$results
),2)
colnames(CAL_table_undervaluation) <- c("U:CAL3F", "t-stat", "p-value", "O:CAL3F", "t-stat", "p-value","U:CAL5F", "t-stat", "p-value", "O:CAL5F", "t-stat", "p-value")
CAL_table_all_bb_undervaluation <- CAL_table_undervaluation


#****************************
# Table modification 2
IRATS_table_all_3f = car_table_cached(CACHE3IS,allmonths = report_months_car)
IRATS_table_all_5f = car_table_cached(CACHE5IS,allmonths = report_months_car)
IRATS_table_all = round(cbind(IRATS_table_all_3f$results, IRATS_table_all_5f$results),2)
colnames(IRATS_table_all) <- c("CAR 3F", "t-stat", "p-value", "CAR 5F", "t-stat", "p-value")  
IRATS_table_all_iss <- IRATS_table_all

CAL_table_all_3f = calendar_table2(ret_is, sdc_is$Event.Date, Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)
CAL_table_all_5f = calendar_table2(ret_is, sdc_is$Event.Date, Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)
CAL_table_all = round(cbind(CAL_table_all_3f$results, CAL_table_all_5f$results),2)
colnames(CAL_table_all) <- c("CAL 3F", "t-stat", "p-value", "CAL 5F", "t-stat", "p-value")  
CAL_table_all_iss   <- CAL_table_all


#****************************
# Table modification 3
tmp = exit_helper_rnw(BUYBACK_DATA2,"Otherlater",holding_period_pnl = "Four.Years.After")
BB_with_ISS_later = tmp$exit_events
BB_with_noISS_later = tmp$noexit_events
BB_ISS_Exit_Hedged = tmp$pnl_Exit_Hedged
BB_ISS_NoExit_Hedged = tmp$pnl_NoExit_Hedged
BB_with_ISS_later_total = tmp$number_events
Exit_on_SEO_Abn_table = round(cbind(
  car_table_cached(CACHE5,BB_with_noISS_later,report_months_car)$results,
  car_table_cached(CACHE5,BB_with_ISS_later,report_months_car)$results
),2)
colnames(Exit_on_SEO_Abn_table) <- c("No SEO: CAR", "t-stat", "p-value","SEO: CAR", "t-stat", "p-value")
rownames(Exit_on_SEO_Abn_table)[nrow(Exit_on_SEO_Abn_table)] <- "Observations"


#****************************
# Table modification 4 - needs to use original buyback data I think
ISS_for_BB_later = unique(BUYBACK_DATA$DATASET$SDC$OtherlaterEvent[which(BUYBACK_DATA$DATASET$SDC$OtherlaterEvent!=0)])
ISS_for_noBB_later = setdiff(1:length(ISSUERS_DATA$DATASET$SDC$Event.Date), ISS_for_BB_later)

ISS_for_BB_later = intersect(ISS_for_BB_later,useonly_is)
ISS_for_noBB_later = intersect(ISS_for_noBB_later,useonly_is)

if(length(ISS_for_BB_later) > 0 & length(ISS_for_noBB_later) > 0) {
  ISS_for_BB_Abn_table = round(cbind(
    car_table_cached(CACHE5IS,ISS_for_BB_later,report_months_car)$results,
    car_table_cached(CACHE5IS,ISS_for_noBB_later,report_months_car)$results
  ),2)
  colnames(ISS_for_BB_Abn_table) <- c("Buyback SEO: CAR", "t-stat", "p-value","Other SEO: CAR", "t-stat", "p-value")
  rownames(ISS_for_BB_Abn_table)[nrow(ISS_for_BB_Abn_table)] <- "Observations"
} else {
  ISS_for_BB_Abn_table <- NULL
}


#****************************
# Table modification 5
IRATS_table_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (sdc_bb$Event.Date >= periods_considered[i,1] & sdc_bb$Event.Date <= periods_considered[i,2])
  res = cbind(
    car_table_cached(CACHE3,which(periodnow),report_months_car)$results,
    car_table_cached(CACHE5,which(periodnow),report_months_car)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                     "t-stat","p-value", "5FF", "t-stat","p-value") 
  rownames(res)[nrow(res)] <- "Observations"
  res    
}))
IRATS_table_time = round(IRATS_table_time,2)
BBtable_time <- IRATS_table_time

CAL_table_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (sdc_bb$Event.Date >= periods_considered[i,1] & sdc_bb$Event.Date <= periods_considered[i,2])
  res = cbind(
    calendar_table2(ret_bb[,periodnow], sdc_bb$Event.Date[periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)$results,
    calendar_table2(ret_bb[,periodnow], sdc_bb$Event.Date[periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                     "t-stat","p-value", "5FF", "t-stat","p-value") 
  rownames(res)[nrow(res)] <- "Observations"
  res    
}))
CAL_table_time = round(CAL_table_time,2)
BBtable_time_cal <- CAL_table_time

#****************************
# Table modification 6
IRATS_table_undervaluation_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (sdc_bb$Event.Date >= periods_considered[i,1] & sdc_bb$Event.Date <= periods_considered[i,2])
  res = cbind(
    car_table_cached(CACHE3,subset_undervalued_bb & periodnow,report_months_car)$results,
    car_table_cached(CACHE3,subset_overvalued_bb & periodnow,report_months_car)$results,
    car_table_cached(CACHE5,subset_undervalued_bb & periodnow,report_months_car)$results,
    car_table_cached(CACHE5,subset_overvalued_bb & periodnow,report_months_car)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                     "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  res    
}))
IRATS_table_undervaluation_time = round(IRATS_table_undervaluation_time,2)
BBtable_undertime <- IRATS_table_undervaluation_time


CAL_table_undervaluation_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow =(sdc_bb$Event.Date >= periods_considered[i,1] & sdc_bb$Event.Date <= periods_considered[i,2])
  res = cbind(
    calendar_table2(ret_bb[,subset_undervalued_bb & periodnow], sdc_bb$Event.Date[subset_undervalued_bb & periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)$results,
    calendar_table2(ret_bb[,subset_overvalued_bb & periodnow], sdc_bb$Event.Date[subset_overvalued_bb & periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=three_factor_model)$results,
    calendar_table2(ret_bb[,subset_undervalued_bb & periodnow], sdc_bb$Event.Date[subset_undervalued_bb & periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)$results,
    calendar_table2(ret_bb[,subset_overvalued_bb & periodnow], sdc_bb$Event.Date[subset_overvalued_bb & periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                     "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
  rownames(res)[nrow(res)] <- "Observations" 
  res    
}))
CAL_table_undervaluation_time = round(CAL_table_undervaluation_time,2)
BB_cal_table_undertime <- CAL_table_undervaluation_time


#****************************
# Table modification 7
BUYBACK_DATA2$long_all12mshort_risk_factors <- BUYBACK_DATA2$long_all12mshort_risk_factors[names(BUYBACK_DATA2$long_all12mshort_risk_factors) >= input$startdate & names(BUYBACK_DATA2$long_all12mshort_risk_factors) <= input$enddate]

#****************************
# Table modification 8
tmp = get_feature_results(BUYBACK_DATA2$DATASET,"minus_Rsq_returns", subset_undervalued_bb, subset_overvalued_bb, quantile_R2,R2window, method="Simple")

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


#****************************
# Table modification 9 
tmp = get_feature_results(BUYBACK_DATA2$DATASET,"pre_vol_Score", subset_undervalued_bb, subset_overvalued_bb, quantile_VOL,VOLwindow, method="Simple")

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

#****************************
# Table modification 10
BUYBACK_DATA2$DATASET$SDC$pre_lev <- feature_function_LEV(BUYBACK_DATA2$DATASET)
tmp = get_feature_results(BUYBACK_DATA2$DATASET,"pre_lev", subset_undervalued_bb, subset_overvalued_bb, quantile_LEV,LEVwindow)
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

BUYBACK_DATA2$DATASET$SDC$EPS_unc <- feature_function_EPSunc(BUYBACK_DATA2$DATASET)
tmp = get_feature_results(BUYBACK_DATA2$DATASET,"EPS_unc", subset_undervalued_bb, subset_overvalued_bb, quantile_EPS,EPSwindow)
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

Under_IdioBB2 = rbind(
  c(100*length(intersect(which(subset_undervalued_bb),High_Idiosyncr_eventsBB ))/length(which(subset_undervalued_bb)), 100*length(intersect(which(subset_undervalued_bb),Low_Idiosyncr_eventsBB ))/length(which(subset_undervalued_bb)),
    100*length(intersect(which(subset_undervalued_bb),High_VOL_eventsBB ))/length(which(subset_undervalued_bb)), 100*length(intersect(which(subset_undervalued_bb),Low_VOL_eventsBB ))/length(which(subset_undervalued_bb)),
    100*length(intersect(which(subset_undervalued_bb),High_LEV_eventsBB ))/length(which(subset_undervalued_bb)), 100*length(intersect(which(subset_undervalued_bb),Low_LEV_eventsBB ))/length(which(subset_undervalued_bb)),
    100*length(intersect(which(subset_undervalued_bb),High_EPS_eventsBB ))/length(which(subset_undervalued_bb)), 100*length(intersect(which(subset_undervalued_bb),Low_EPS_eventsBB ))/length(which(subset_undervalued_bb))
  ),
  c(100*length(intersect(which(subset_overvalued_bb),High_Idiosyncr_eventsBB ))/length(which(subset_overvalued_bb)), 100*length(intersect(which(subset_overvalued_bb),Low_Idiosyncr_eventsBB ))/length(which(subset_overvalued_bb)),
    100*length(intersect(which(subset_overvalued_bb),High_VOL_eventsBB ))/length(which(subset_overvalued_bb)), 100*length(intersect(which(subset_overvalued_bb),Low_VOL_eventsBB ))/length(which(subset_overvalued_bb)),
    100*length(intersect(which(subset_overvalued_bb),High_LEV_eventsBB ))/length(which(subset_overvalued_bb)), 100*length(intersect(which(subset_overvalued_bb),Low_LEV_eventsBB ))/length(which(subset_overvalued_bb)),
    100*length(intersect(which(subset_overvalued_bb),High_EPS_eventsBB ))/length(which(subset_overvalued_bb)), 100*length(intersect(which(subset_overvalued_bb),Low_EPS_eventsBB ))/length(which(subset_overvalued_bb))
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
rownames(Under_IdioBB2) <- c("Undervalued","Overvalued", "High Idiosync.", "Low Idiosync.","High Vol.", "Low Vol.","High Lev.","Low Lev." )
colnames(Under_IdioBB2) <- c("H Idiosync.","L Idiosync.", "H Vol.","L Vol.","H Lev.","L Lev.","H EPS unc.",  "L EPS unc.")

#****************************
# Table modification 12
VOL_IRATStableBB
all_fund_sources = unique(unlist(sapply(BUYBACK_DATA2$DATASET$SDC$Source...of..Funds..Code, function(i) unlist(str_split(i,"\\+")))))
cash_funds = c("CR")
credit_funds = c("BL","BOR","CF","DS")
other_funds = setdiff(all_fund_sources,c(cash_funds,credit_funds))
all_purposes = unique(unlist(sapply(BUYBACK_DATA2$DATASET$SDC$Purpose.Code, function(i) unlist(str_split(i,"\\+")))))
good_purpose = c("ESV","UVL","STP","ISV")
other_purpose = setdiff(all_purposes,c(good_purpose))

high_leverage = 0*EUindex_bb2
high_leverage[High_LEV_eventsBB] <-1
low_leverage = 0*EUindex_bb2
low_leverage[Low_LEV_eventsBB] <-1
Missed_EPS = (BUYBACK_DATA2$DATASET$SDC$EPS.Value < BUYBACK_DATA2$DATASET$SDC$EPS.Forecast)
Beat_EPS = (BUYBACK_DATA2$DATASET$SDC$EPS.Value >= BUYBACK_DATA2$DATASET$SDC$EPS.Forecast)
low_epsunc = 0*EUindex_bb2
low_epsunc[Low_EPS_eventsBB] <-1
ISS_Later = ifelse(BUYBACK_DATA2$DATASET$SDC$OtherlaterEvent!=0, "Yes", "No")
Credit = sapply(BUYBACK_DATA2$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), credit_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(cash_funds,other_funds)))==0)
Cash = sapply(BUYBACK_DATA2$DATASET$SDC$Source...of..Funds..Code, function(i) length(intersect(unlist(str_split(i,"\\+")), cash_funds))!=0 & length(intersect(unlist(str_split(i,"\\+")), c(credit_funds,other_funds)))==0)
Good_purpose = sapply(BUYBACK_DATA2$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), good_purpose))!=0 & length(intersect(unlist(str_split(i,"\\+")), other_purpose))==0)
Stock_Option_Plan = sapply(BUYBACK_DATA2$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "STP"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV","UVL")))==0)
Undervalued = sapply(BUYBACK_DATA2$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), "UVL"))!=0 & length(intersect(unlist(str_split(i,"\\+")), c("STP")))==0 )
Enhance_Shareholder_Value = sapply(BUYBACK_DATA2$DATASET$SDC$Purpose.Code, function(i) length(intersect(unlist(str_split(i,"\\+")), c("ESV","ISV")))!=0)
all_characteristics = cbind(low_leverage,high_leverage,Missed_EPS,Beat_EPS,ISS_Later,Cash,Good_purpose,Undervalued,Enhance_Shareholder_Value,Stock_Option_Plan)

EU_relations= t(apply(all_characteristics,2,function(r){
  x = table(EUindex_bb2, r)
  x = matrix(round(100*x[,2]/(x[,1]+x[,2]),1),ncol=1)
  x
}))
colnames(EU_relations) <- paste("EU",0:(ncol(EU_relations)-1),sep="")
rownames(EU_relations) <- gsub("_"," ", rownames(EU_relations))
all_characteristics_continuous = cbind(BUYBACK_DATA2$DATASET$SDC$Market.Cap, BUYBACK_DATA2$DATASET$SDC$BEME_used, BUYBACK_DATA2$DATASET$SDC$Event.Size)
EU_relations_continuous = t(apply(all_characteristics_continuous,2,function(r){
  sapply(sort(unique(EUindex_bb2)), function(i) {
    useonly = which(EUindex_bb2 == i)
    mean(r[useonly][!is.na(r[useonly])])
  })
}))
rownames(EU_relations_continuous) <- c("Market Cap.", "BE/ME Score", "Percentage Shares")
EU_relations = rbind(EU_relations, round(EU_relations_continuous,2))

U_relations= t(apply(all_characteristics,2,function(r){
  x = table(BUYBACK_DATA2$Valuation_Index, r)
  x = matrix(round(100*x[,2]/(x[,1]+x[,2]),1),ncol=1)
  x
}))
colnames(U_relations) <- paste("U",0:(ncol(U_relations)-1),sep="")
rownames(U_relations) <- gsub("_"," ", rownames(U_relations))


#****************************
# Table modification 13
EU_long_bb = NULL
EU_Hedged_bb = NULL
EU_long48_bb = NULL
EU_Hedged48_bb = NULL
EU_IRATStable_bb = NULL
EU_CALtable_bb = NULL
for (i in 0:6){
  EU_events_now = which(EUindex_bb2 == i)
  EU = apply(PNL_matrix_BB(start_date_event,"One.Year.After", EU_events_now,  BUYBACK_DATA2$DATASET$DatesMonth, BUYBACK_DATA2$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", EU_events_now,  BUYBACK_DATA2$DATASET$DatesMonth, BUYBACK_DATA2$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  EU48m_hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(EU48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  EU_long_bb = cbind(EU_long_bb,EU)
  EU_Hedged_bb = cbind(EU_Hedged_bb,EU_hedged)
  EU_long48_bb = cbind(EU_long48_bb,EU48m)
  EU_Hedged48_bb = cbind(EU_Hedged48_bb, EU48m_hedged)
  EU_IRATStable_bb = cbind(EU_IRATStable_bb,car_table_cached(CACHE5,EU_events_now,report_months_car)$results)
  EU_CALtable_bb = cbind(EU_CALtable_bb,calendar_table2(BUYBACK_DATA2$DATASET$returns_by_event_monthly[,EU_events_now], BUYBACK_DATA2$DATASET$SDC$Event.Date[EU_events_now], Risk_Factors_Monthly,report_months_cal)$results)
}
rm("EU","EU_hedged","EU48m", "EU48m_hedged")


#****************************
# Table modification 15
high_EU = (EUindex_bb2 %in% 5:6)
low_EU = (EUindex_bb2 %in% 0:1)
EIRATS_table_undervaluation_time_bb = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA2$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA2$DATASET$SDC$Event.Date <= periods_considered[i,2])
  res = cbind(
    car_table_cached(CACHE5,which(high_EU & periodnow),report_months_car)$results,
    car_table_cached(CACHE5,which(low_EU & periodnow),report_months_car)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  res    
}))
EIRATS_table_undervaluation_time_bb = round(EIRATS_table_undervaluation_time_bb,2)


#****************************
# Table modification 16
ECAL_table_undervaluation_time_bb = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
  periodnow = (BUYBACK_DATA2$DATASET$SDC$Event.Date >= periods_considered[i,1] & BUYBACK_DATA2$DATASET$SDC$Event.Date <= periods_considered[i,2])
  res = cbind(
    calendar_table2(BUYBACK_DATA2$DATASET$returns_by_event_monthly[,high_EU & periodnow], BUYBACK_DATA2$DATASET$SDC$Event.Date[high_EU & periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)$results,
    calendar_table2(BUYBACK_DATA2$DATASET$returns_by_event_monthly[,low_EU & periodnow], BUYBACK_DATA2$DATASET$SDC$Event.Date[low_EU & periodnow], Risk_Factors_Monthly,report_months_cal,formula_used=five_factor_model)$results
  )
  colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 5FF", sep=" "),
                     "t-stat","p-value", "O 5FF", "t-stat","p-value") 
  res    
}))
ECAL_table_undervaluation_time_bb = round(ECAL_table_undervaluation_time_bb,2)

#****************************
# Table modification 17
high_EU_bb = high_EU
low_EU_bb = low_EU
High_EU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", high_EU_bb,  BUYBACK_DATA2$DATASET$DatesMonth, BUYBACK_DATA2$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
High_EU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_EU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    


############################################# FIGURES #########################################
valuation_index_bb <- valuation_index_bb[useonly_bb]
