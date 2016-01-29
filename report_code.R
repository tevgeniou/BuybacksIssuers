
# This file is run only in order to "customize" the over/under valuation events - the rest are all done in report_data.R

reportcode_data<-new.env()

with(reportcode_data,{
  
  DATASET = report_data$DATASET     
  DATASET$Betas_PB6M  = report_data$Betas_PB6M    
  cleanup = DATASET$cleanup # to avoid losing info when we remove data. we add back at the end of the file
  # just add some names 
  DATASET$SDC$event_name = paste(DATASET$SDC$CUSIP, DATASET$SDC$Event.Date, sep="_")
  colnames(DATASET$Betas_PB6M) <- DATASET$SDC$event_name 
  
  #### FIRST THE BUSINESS DECISIONS CLEANUP
  # Now remove all events for which there was at least another one less than MINIMUM_PERIOD_SINCE_LAST_EVENT before this event
  recent_events <- which(sapply(1:length(DATASET$SDC$Event.Date), function(ev){
    all_permno_events = which(DATASET$SDC$permno == DATASET$SDC$permno[ev])
    sum((DATASET$SDC$Event.Date[ev] - DATASET$SDC$Event.Date[all_permno_events] < MINIMUM_PERIOD_SINCE_LAST_EVENT ) &
          (DATASET$SDC$Event.Date[ev] - DATASET$SDC$Event.Date[all_permno_events] > 0)) !=0     
  }))
  with_recent_event = length(unique(recent_events))
  cleanup$BIZ_have_previous_recent_event <- with_recent_event 
  
  to_remove = unique(recent_events)
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
  rm("recent_events")
  
  # DO THE "BUSINESS" FILTERING FIRST
  # NOTE: we still keep events for which IBES data is missing. we will remove those only when we do the IBES data based analysis in the paper
  
  DATASET$cleanup <- cleanup #need to put this back and take it again before the biz cleanup!
  DATASET = business_data_removal(DATASET)
  cleanup <- DATASET$cleanup
  
  ### And now the dataset-idiosyncratic business cleaning here
  
  # Buybacks
  if (DATASET$name == "buybacks"){
    Technique_filter =  DATASET$SDC$Tech..nique.Code %in% BB_allowed_techniques # OP, OPNG, and ""    
    cleanup$BIZ_allowed_techniques <- sum(!Technique_filter) 
    DATASET$cleanup <- cleanup #need to put this back and take it again before the biz cleanup!    
    to_remove =  which(!(Technique_filter))    
  }
  
  # Issuers 
  if (DATASET$name == "issuers"){
    Technique_filter =  DATASET$SDC$Offering.Technique %in% ISS_allowed_techniques    
    cleanup$BIZ_allowed_techniques <- sum(!Technique_filter) 
    DATASET$cleanup <- cleanup #need to put this back and take it again before the biz cleanup!    
    to_remove =  which(!(Technique_filter))     
  }
  
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
  
  ########################################################################################################################
  #Now the results
  
  DATASET$SDC$Event_Industry <- suppressWarnings(scrub(as.numeric(DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed  
  DATASET_all = DATASET
  
  ########################################################################################################################
  #FIRST ALL EXCEPT FINANCIALS AND UTILITIES (Whatever we excluded)
  
  to_remove = which(!(DATASET$SDC$Event_Industry %in% INDUSTRY_USED))
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
  
  # Calculate some basic variables
  Betas_Summary <- (my_summary(t(DATASET$Betas_PB6M)))
  
  # Get all holding periods event returns
  all_events = 1:length(DATASET$SDC$Event.Date)
  events_all_1m = PNL_matrix_BB(start_date_event,"One.Month.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  events_all_3m = PNL_matrix_BB(start_date_event,"Three.Month.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  events_all_6m = PNL_matrix_BB(start_date_event,"Six.Month.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  events_all_12m = PNL_matrix_BB(start_date_event,"One.Year.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  events_all_24m = PNL_matrix_BB(start_date_event,"Two.Years.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  events_all_36m = PNL_matrix_BB(start_date_event,"Three.Years.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  events_all_48m = PNL_matrix_BB(start_date_event,"Four.Years.After", all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  
  # now get all holding period pnls
  pnl_returns_events_all_1M =  apply(events_all_1m,1,non_zero_mean)
  pnl_returns_events_all_3M = apply(events_all_3m,1,non_zero_mean)
  pnl_returns_events_all_6M <- apply(events_all_6m,1,non_zero_mean)
  pnl_returns_events_all_12M <- apply(events_all_12m,1,non_zero_mean)
  pnl_returns_events_all_24M =  apply(events_all_24m,1,non_zero_mean)
  pnl_returns_events_all_36M = apply(events_all_36m,1,non_zero_mean)
  pnl_returns_events_all_48M <- apply(events_all_48m,1,non_zero_mean)
  
  #... and hedge them to get the trading strategies  for ALL companies  for all holding periods
  long_all1mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_1M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_all3mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_3M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_all6mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_6M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_all12mshort_risk_factors = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_12M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_all24mshort_risk_factors = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_24M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_all36mshort_risk_factors = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_36M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_all48mshort_risk_factors = suppressWarnings(scrub(alpha_lm(pnl_returns_events_all_48M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  
  # ... and get the live events for different holding periods
  live_events1m  = apply(events_all_1m!=0,1,sum)
  live_events3m  = apply(events_all_3m!=0,1,sum)
  live_events6m  = apply(events_all_6m!=0,1,sum)
  live_events12m  = apply(events_all_12m!=0,1,sum)
  live_events24m  = apply(events_all_24m!=0,1,sum)
  live_events36m  = apply(events_all_36m!=0,1,sum)
  live_events48m  = apply(events_all_48m!=0,1,sum)
  
  # Get the U-index deals now  
  x=DATASET$returns_by_event_monthly
  msx = apply(x,2,function(r) shift(ms(r,11),2))
  rownames(msx) <- str_sub(rownames(x),start=1,end=7)
  DATASET$SDC$prior_returns_absolute <- sapply(1:length(DATASET$SDC$Event.Date), function(i)
    100*msx[which(rownames(msx) == str_sub(DATASET$SDC$Event.Date[i],start=1,end=7)),i]
  )  
  returns_breakpoint_years_months = as.numeric(format(as.Date(rownames(returnsbreakpoints)),"%Y%m"))
  SDC_years_months = as.numeric(format(DATASET$SDC$Event.Date,"%Y%m"))
  for (i in 1:nrow(DATASET$SDC)){
    month_we_care = which(returns_breakpoint_years_months == SDC_years_months[i])#-1 # We don't use -1 since the FF breakpoints go till 2 months before
    returnsthres = returnsbreakpoints[month_we_care,]
    DATASET$SDC$returns_quantile[i] = ifelse(DATASET$SDC$prior_returns_absolute[i] < min(returnsthres), 1, tail(which(returnsthres <= DATASET$SDC$prior_returns_absolute[i]),1))
  }  
  
  ##### The indices now
  if (DATASET$name == "buybacks"){
    tmp = DATASET$SDC$returns_quantile
    Performance_used = ifelse(tmp %in% 1:4, 5, ifelse(tmp %in% 5:8, 4, ifelse(tmp %in% 9:12, 3, ifelse(tmp %in% 13:16, 2, 1))))  
    tmp = DATASET$SDC$BEME_quantile
    BEME_used = ceiling(tmp/4) 
    #BEME_used = ifelse(tmp >= quantile(tmp,0.8), 5, ifelse(tmp >= quantile(tmp,0.6), 4, ifelse(tmp >= quantile(tmp,0.4), 3, ifelse(tmp >= quantile(tmp,0.2), 2, 1))))
    tmp = DATASET$SDC$ME_quantile
    Size_used = ifelse(tmp %in% 1:4, 5, ifelse(tmp %in% 5:8, 4, ifelse(tmp %in% 9:12, 3, ifelse(tmp %in% 13:16, 2, 1))))  
    
    Valuation_Index = Size_used + BEME_used + Performance_used
    #Split up dataset in good company subset and bad company subset: "GOOD" MEANS UNDERVALUED!!! (buybacks terminology...)
    company_subset_undervalued =  Valuation_Index >= quantile(Valuation_Index,0.8)
    company_subset_overvalued  = Valuation_Index <= quantile(Valuation_Index,0.2)
    rest_of_companies = rep(1,length(Performance_used)) - (company_subset_undervalued | company_subset_overvalued)
  } 
  
  if (DATASET$name == "issuers"){
    tmp = DATASET$SDC$returns_quantile
    Performance_used = ifelse(tmp >= quantile(tmp,0.8), 5, ifelse(tmp >= quantile(tmp,0.6), 4, ifelse(tmp >= quantile(tmp,0.4), 3, ifelse(tmp >= quantile(tmp,0.2), 2, 1))))  
    tmp = DATASET$SDC$BEME_quantile
    BEME_used = ifelse(tmp <= quantile(tmp,0.2), 5, ifelse(tmp <= quantile(tmp,0.4), 4, ifelse(tmp <= quantile(tmp,0.6), 3, ifelse(tmp <= quantile(tmp,0.8), 2, 1))))  
    tmp = DATASET$SDC$ME_quantile
    Size_used = ifelse(tmp <= quantile(tmp,0.2), 5, ifelse(tmp <= quantile(tmp,0.4), 4, ifelse(tmp <= quantile(tmp,0.6), 3, ifelse(tmp <= quantile(tmp,0.8), 2, 1))))  
    
    Valuation_Index = Size_used + BEME_used + Performance_used
    #Split up dataset in good company subset and bad company subset "GOOD" MEANS UNDERVALUED!!! (buybacks terminology...)
    company_subset_undervalued =  Valuation_Index <= quantile(Valuation_Index,0.2)
    company_subset_overvalued  = Valuation_Index >= quantile(Valuation_Index,0.8)
    rest_of_companies = rep(1,length(Performance_used)) - (company_subset_undervalued | company_subset_overvalued)
  }
  rm("x","msx" ,"tmp","returns_breakpoint_years_months","SDC_years_months","month_we_care","returnsthres")
  
  DATASET$SDC$Performance_used <- Performance_used
  DATASET$SDC$BEME_used <- BEME_used
  DATASET$SDC$Size_used <- Size_used
  
  #Betas of good and bad companies only
  Betas_undervalued <- DATASET$Betas_PB6M[,company_subset_undervalued]
  Betas_overvalued <- DATASET$Betas_PB6M[,company_subset_overvalued]
  Betas_undervalued_summary <- my_summary(t(Betas_undervalued))
  Betas_overvalued_summary <- my_summary(t(Betas_overvalued))
  
  ## Trading strategies based on the over/under-valuation index  
  pnl_returns_events_undervalued = apply(PNL_matrix_BB(start_date_event,end_date_event, which(company_subset_undervalued),  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)  
  pnl_returns_events_overvalued = apply(PNL_matrix_BB(start_date_event,end_date_event, which(company_subset_overvalued),  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)  
  long_undervaluedshort_risk_factors  = suppressWarnings(scrub(alpha_lm(pnl_returns_events_undervalued,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  long_overvaluedshort_risk_factors  = suppressWarnings(scrub(alpha_lm(pnl_returns_events_overvalued,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  
  ### IRATS tables now
  IRATS_table_all_3f = car_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=three_factor_model)
  IRATS_table_all_5f = car_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=five_factor_model)
  IRATS_table_all = round(cbind(IRATS_table_all_3f$results, IRATS_table_all_5f$results),2)
  colnames(IRATS_table_all) <- c("CAR 3F", "t-stat", "p-value", "CAR 5F", "t-stat", "p-value")  
  
  IRATS_table_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    res = cbind(
      car_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                       "t-stat","p-value", "5FF", "t-stat","p-value") 
    rownames(res)[nrow(res)] <- "Observations"
    res    
  }))
  IRATS_table_time = round(IRATS_table_time,2)
  
  IRATS_table_undervaluation_undervalued3f = car_table(DATASET$returns_by_event_monthly[,company_subset_undervalued], DATASET$SDC$Event.Date[company_subset_undervalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  IRATS_table_undervaluation_overvalued3f = car_table(DATASET$returns_by_event_monthly[,company_subset_overvalued], DATASET$SDC$Event.Date[company_subset_overvalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  IRATS_table_undervaluation_undervalued5f = car_table(DATASET$returns_by_event_monthly[,company_subset_undervalued], DATASET$SDC$Event.Date[company_subset_undervalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  IRATS_table_undervaluation_overvalued5f = car_table(DATASET$returns_by_event_monthly[,company_subset_overvalued], DATASET$SDC$Event.Date[company_subset_overvalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  IRATS_table_undervaluation = round(cbind(
    IRATS_table_undervaluation_undervalued3f$results,
    IRATS_table_undervaluation_overvalued3f$results,
    IRATS_table_undervaluation_undervalued5f$results,
    IRATS_table_undervaluation_overvalued5f$results
  ),2)
  colnames(IRATS_table_undervaluation) <- c("U:CAR3F", "t-stat", "p-value", "O:CAR3F", "t-stat", "p-value","U:CAR5F", "t-stat", "p-value", "O:CAR5F", "t-stat", "p-value")
  
  IRATS_table_undervaluation_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    
    res = cbind(
      car_table(DATASET$returns_by_event_monthly[,company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                       "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
    
    res    
  }))
  IRATS_table_undervaluation_time = round(IRATS_table_undervaluation_time,2)
  
  ### Calendar tables now
  CAL_table_all_3f = calendar_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=three_factor_model)
  CAL_table_all_5f = calendar_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=five_factor_model)
  CAL_table_all = round(cbind(CAL_table_all_3f$results, CAL_table_all_5f$results),2)
  colnames(CAL_table_all) <- c("CAL 3F", "t-stat", "p-value", "CAL 5F", "t-stat", "p-value")  
  
  CAL_table_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    res = cbind(
      calendar_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                       "t-stat","p-value", "5FF", "t-stat","p-value") 
    rownames(res)[nrow(res)] <- "Observations"
    res    
  }))
  CAL_table_time = round(CAL_table_time,2)
  
  CAL_table_undervaluation_undervalued3f = calendar_table(DATASET$returns_by_event_monthly[,company_subset_undervalued], DATASET$SDC$Event.Date[company_subset_undervalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  CAL_table_undervaluation_overvalued3f = calendar_table(DATASET$returns_by_event_monthly[,company_subset_overvalued], DATASET$SDC$Event.Date[company_subset_overvalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  CAL_table_undervaluation_undervalued5f = calendar_table(DATASET$returns_by_event_monthly[,company_subset_undervalued], DATASET$SDC$Event.Date[company_subset_undervalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  CAL_table_undervaluation_overvalued5f = calendar_table(DATASET$returns_by_event_monthly[,company_subset_overvalued], DATASET$SDC$Event.Date[company_subset_overvalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  CAL_table_undervaluation = round(cbind(
    CAL_table_undervaluation_undervalued3f$results,
    CAL_table_undervaluation_overvalued3f$results,
    CAL_table_undervaluation_undervalued5f$results,
    CAL_table_undervaluation_overvalued5f$results
  ),2)
  colnames(CAL_table_undervaluation) <- c("U:CAL3F", "t-stat", "p-value", "O:CAL3F", "t-stat", "p-value","U:CAL5F", "t-stat", "p-value", "O:CAL5F", "t-stat", "p-value")
  
  CAL_table_undervaluation_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    
    res = cbind(
      calendar_table(DATASET$returns_by_event_monthly[,company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                       "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
    rownames(res)[nrow(res)] <- "Observations" 
    
    res    
  }))
  CAL_table_undervaluation_time = round(CAL_table_undervaluation_time,2)
  
  DATASET_paper = DATASET # THIS IS WHAT WE WILL HAVE IN THE PAPER!!!!!
  
  ########################################################################################################################
  ########################################################################################################################
  # NOW ONLY THE FINANCIALS 
  DATASET = DATASET_all # get all back again
  
  finance_deals_dates = DATASET$SDC$Event.Date[which((DATASET$SDC$Event_Industry %in% INDUSTRY_FINANCIALS))]
  
  to_remove = which(!(DATASET$SDC$Event_Industry %in% INDUSTRY_FINANCIALS))
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
  # Get all holding periods event returns
  finance_all_events = 1:length(DATASET$SDC$Event.Date)
  finance_events_all_1m = PNL_matrix_BB(start_date_event,"One.Month.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  finance_events_all_3m = PNL_matrix_BB(start_date_event,"Three.Month.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  finance_events_all_6m = PNL_matrix_BB(start_date_event,"Six.Month.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  finance_events_all_12m = PNL_matrix_BB(start_date_event,"One.Year.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  finance_events_all_24m = PNL_matrix_BB(start_date_event,"Two.Years.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  finance_events_all_36m = PNL_matrix_BB(start_date_event,"Three.Years.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  finance_events_all_48m = PNL_matrix_BB(start_date_event,"Four.Years.After", finance_all_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1)  
  
  # now get all holding period pnls
  finance_pnl_returns_events_all_1M =  apply(finance_events_all_1m,1,non_zero_mean)
  finance_pnl_returns_events_all_3M = apply(finance_events_all_3m,1,non_zero_mean)
  finance_pnl_returns_events_all_6M <- apply(finance_events_all_6m,1,non_zero_mean)
  finance_pnl_returns_events_all_12M <- apply(finance_events_all_12m,1,non_zero_mean)
  finance_pnl_returns_events_all_24M =  apply(finance_events_all_24m,1,non_zero_mean)
  finance_pnl_returns_events_all_36M = apply(finance_events_all_36m,1,non_zero_mean)
  finance_pnl_returns_events_all_48M <- apply(finance_events_all_48m,1,non_zero_mean)
  
  #... and hedge them to get the trading strategies  for ALL companies  for all holding periods
  finance_long_all1mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_1M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_all3mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_3M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_all6mshort_risk_factors  = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_6M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_all12mshort_risk_factors = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_12M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_all24mshort_risk_factors = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_24M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_all36mshort_risk_factors = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_36M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_all48mshort_risk_factors = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_all_48M,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  
  # ... and get the live events for different holding periods
  finance_live_events1m  = apply(finance_events_all_1m!=0,1,sum)
  finance_live_events3m  = apply(finance_events_all_3m!=0,1,sum)
  finance_live_events6m  = apply(finance_events_all_6m!=0,1,sum)
  finance_live_events12m  = apply(finance_events_all_12m!=0,1,sum)
  finance_live_events24m  = apply(finance_events_all_24m!=0,1,sum)
  finance_live_events36m  = apply(finance_events_all_36m!=0,1,sum)
  finance_live_events48m  = apply(finance_events_all_48m!=0,1,sum)
  
  # Get the U-index deals now  
  x=DATASET$returns_by_event_monthly
  msx = apply(x,2,function(r) shift(ms(r,11),2))
  rownames(msx) <- str_sub(rownames(x),start=1,end=7)
  DATASET$SDC$prior_returns_absolute <- sapply(1:length(DATASET$SDC$Event.Date), function(i)
    100*msx[which(rownames(msx) == str_sub(DATASET$SDC$Event.Date[i],start=1,end=7)),i]
  )  
  returns_breakpoint_years_months = as.numeric(format(as.Date(rownames(returnsbreakpoints)),"%Y%m"))
  SDC_years_months = as.numeric(format(DATASET$SDC$Event.Date,"%Y%m"))
  for (i in 1:nrow(DATASET$SDC)){
    month_we_care = which(returns_breakpoint_years_months == SDC_years_months[i])#-1 # We don't use -1 since the FF breakpoints go till 2 months before
    returnsthres = returnsbreakpoints[month_we_care,]
    DATASET$SDC$returns_quantile[i] = ifelse(DATASET$SDC$prior_returns_absolute[i] < min(returnsthres), 1, tail(which(returnsthres <= DATASET$SDC$prior_returns_absolute[i]),1))
  }  
  
  
  ##### The indices now
  if (DATASET$name == "buybacks"){
    tmp = DATASET$SDC$returns_quantile
    finance_Performance_used = ifelse(tmp %in% 1:4, 5, ifelse(tmp %in% 5:8, 4, ifelse(tmp %in% 9:12, 3, ifelse(tmp %in% 13:16, 2, 1))))  
    tmp = DATASET$SDC$BEME_quantile
    finance_BEME_used = ceiling(tmp/4) 
    tmp = DATASET$SDC$ME_quantile
    finance_Size_used = ifelse(tmp %in% 1:4, 5, ifelse(tmp %in% 5:8, 4, ifelse(tmp %in% 9:12, 3, ifelse(tmp %in% 13:16, 2, 1))))  

    finance_Valuation_Index = finance_Size_used + finance_BEME_used + finance_Performance_used
    #Split up dataset in good company subset and bad company subset "GOOD" MEANS UNDERVALUED!!! (buybacks terminology...)
    finance_company_subset_undervalued =  finance_Valuation_Index >= quantile(finance_Valuation_Index,0.8)
    finance_company_subset_overvalued  = finance_Valuation_Index <= quantile(finance_Valuation_Index,0.2)
    # If we want to use the same thresholds as for the other companies
    #finance_company_subset_undervalued =  finance_Valuation_Index >= quantile(Valuation_Index,0.8)
    #finance_company_subset_overvalued  = finance_Valuation_Index <= quantile(Valuation_Index,0.2)
    finance_rest_of_companies = rep(1,length(finance_Performance_used)) - (finance_company_subset_undervalued | finance_company_subset_overvalued)
  } 
  
  if (DATASET$name == "issuers"){
    tmp = DATASET$SDC$returns_quantile
    finance_Performance_used = ifelse(tmp >= quantile(tmp,0.8), 5, ifelse(tmp >= quantile(tmp,0.6), 4, ifelse(tmp >= quantile(tmp,0.4), 3, ifelse(tmp >= quantile(tmp,0.2), 2, 1))))  
    tmp = DATASET$SDC$BEME_quantile
    finance_BEME_used = ifelse(tmp <= quantile(tmp,0.2), 5, ifelse(tmp <= quantile(tmp,0.4), 4, ifelse(tmp <= quantile(tmp,0.6), 3, ifelse(tmp <= quantile(tmp,0.8), 2, 1))))  
    tmp = DATASET$SDC$ME_quantile
    finance_Size_used = ifelse(tmp <= quantile(tmp,0.2), 5, ifelse(tmp <= quantile(tmp,0.4), 4, ifelse(tmp <= quantile(tmp,0.6), 3, ifelse(tmp <= quantile(tmp,0.8), 2, 1))))  
    
    finance_Valuation_Index = finance_Size_used + finance_BEME_used + finance_Performance_used
    #Split up dataset in good company subset and bad company subset "GOOD" MEANS UNDERVALUED!!! (buybacks terminology...)
    finance_company_subset_undervalued =  finance_Valuation_Index <= quantile(finance_Valuation_Index,0.2)
    finance_company_subset_overvalued  = finance_Valuation_Index >= quantile(finance_Valuation_Index,0.8)
    # If we want to use the same thresholds as for the other companies
    #finance_company_subset_undervalued =  finance_Valuation_Index >= quantile(Valuation_Index,0.8)
    #finance_company_subset_overvalued  = finance_Valuation_Index <= quantile(Valuation_Index,0.2)
    finance_rest_of_companies = rep(1,length(finance_Performance_used)) - (finance_company_subset_undervalued | finance_company_subset_overvalued)
  }
  rm("x","msx" ,"tmp","returns_breakpoint_years_months","SDC_years_months","month_we_care","returnsthres")
  
  ## Trading strategies based on the over/under-valuation index  
  finance_pnl_returns_events_undervalued = apply(PNL_matrix_BB(start_date_event,end_date_event, which(finance_company_subset_undervalued),  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)  
  finance_pnl_returns_events_overvalued = apply(PNL_matrix_BB(start_date_event,end_date_event, which(finance_company_subset_overvalued),  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)  
  finance_long_undervaluedshort_risk_factors  = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_undervalued,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  finance_long_overvaluedshort_risk_factors  = suppressWarnings(scrub(alpha_lm(finance_pnl_returns_events_overvalued,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months, trade = 1)))
  
  ### IRATS tables now
  # FOR FINANCE WE ALSO DO THE SPECIAL HEDGE WITH ADDING THE FINANCE SECTOR
  finance_IRATS_table_all_3f = car_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=three_factor_model)
  finance_IRATS_table_all_5f = car_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=five_factor_model)
  finance_IRATS_table_all_6f = car_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=five_factor_model_finance)
  finance_IRATS_table_all = round(cbind(finance_IRATS_table_all_3f$results, finance_IRATS_table_all_5f$results,finance_IRATS_table_all_6f$results),2)
  colnames(finance_IRATS_table_all) <- c("CAR 3F", "t-stat", "p-value", "CAR 5F", "t-stat", "p-value", "CAR 5F+Fin", "t-stat", "p-value")  
  
  finance_IRATS_table_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    res = cbind(
      car_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model_finance)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                       "t-stat","p-value", "5FF", "t-stat","p-value", "CAR 5F+Fin", "t-stat", "p-value") 
    rownames(res)[nrow(res)] <- "Observations"
    res    
  }))
  finance_IRATS_table_time = round(finance_IRATS_table_time,2)
  
  finance_IRATS_table_undervaluation_undervalued3f = car_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued], DATASET$SDC$Event.Date[finance_company_subset_undervalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  finance_IRATS_table_undervaluation_overvalued3f = car_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued], DATASET$SDC$Event.Date[finance_company_subset_overvalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  finance_IRATS_table_undervaluation_undervalued5f = car_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued], DATASET$SDC$Event.Date[finance_company_subset_undervalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  finance_IRATS_table_undervaluation_overvalued5f = car_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued], DATASET$SDC$Event.Date[finance_company_subset_overvalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  finance_IRATS_table_undervaluation_undervalued5ff = car_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued], DATASET$SDC$Event.Date[finance_company_subset_undervalued], Risk_Factors_Monthly,formula_used=five_factor_model_finance)
  finance_IRATS_table_undervaluation_overvalued5ff = car_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued], DATASET$SDC$Event.Date[finance_company_subset_overvalued], Risk_Factors_Monthly,formula_used=five_factor_model_finance)
  finance_IRATS_table_undervaluation = round(cbind(
    finance_IRATS_table_undervaluation_undervalued3f$results,
    finance_IRATS_table_undervaluation_overvalued3f$results,
    finance_IRATS_table_undervaluation_undervalued5f$results,
    finance_IRATS_table_undervaluation_overvalued5f$results,
    finance_IRATS_table_undervaluation_undervalued5ff$results,
    finance_IRATS_table_undervaluation_overvalued5ff$results
  ),2)
  colnames(finance_IRATS_table_undervaluation) <- c("U:CAR3F", "t-stat", "p-value", "O:CAR3F", "t-stat", "p-value","U:CAR5F", "t-stat", "p-value", "O:CAR5F", "t-stat", "p-value","U:CAR5F+Fin", "t-stat", "p-value", "O:CAR5F+Fin", "t-stat", "p-value")
  
  finance_IRATS_table_undervaluation_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    
    res = cbind(
      car_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
      car_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                       "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
    
    res    
  }))
  finance_IRATS_table_undervaluation_time = round(finance_IRATS_table_undervaluation_time,2)
  
  ### Calendar tables now
  finance_CAL_table_all_3f = calendar_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=three_factor_model)
  finance_CAL_table_all_5f = calendar_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=five_factor_model)
  finance_CAL_table_all_6f = calendar_table(DATASET$returns_by_event_monthly, DATASET$SDC$Event.Date, Risk_Factors_Monthly,formula_used=five_factor_model_finance)
  finance_CAL_table_all = round(cbind(finance_CAL_table_all_3f$results, finance_CAL_table_all_5f$results,finance_CAL_table_all_6f$results),2)
  colnames(finance_CAL_table_all) <- c("CAL 3F", "t-stat", "p-value", "CAL 5F", "t-stat", "p-value","CAL 5F+Fin", "t-stat", "p-value")  
  
  finance_CAL_table_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    res = cbind(
      calendar_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,periodnow], DATASET$SDC$Event.Date[periodnow], Risk_Factors_Monthly,formula_used=five_factor_model_finance)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"3FF", sep=" "),
                       "t-stat","p-value", "5FF", "t-stat","p-value", "5FF+Fin", "t-stat","p-value") 
    rownames(res)[nrow(res)] <- "Observations"
    res    
  }))
  finance_CAL_table_time = round(finance_CAL_table_time,2)
  
  finance_CAL_table_undervaluation_undervalued3f = calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued], DATASET$SDC$Event.Date[finance_company_subset_undervalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  finance_CAL_table_undervaluation_overvalued3f = calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued], DATASET$SDC$Event.Date[finance_company_subset_overvalued], Risk_Factors_Monthly,formula_used=three_factor_model)
  finance_CAL_table_undervaluation_undervalued5f = calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued], DATASET$SDC$Event.Date[finance_company_subset_undervalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  finance_CAL_table_undervaluation_overvalued5f = calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued], DATASET$SDC$Event.Date[finance_company_subset_overvalued], Risk_Factors_Monthly,formula_used=five_factor_model)
  finance_CAL_table_undervaluation_undervalued5ff = calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued], DATASET$SDC$Event.Date[finance_company_subset_undervalued], Risk_Factors_Monthly,formula_used=five_factor_model_finance)
  finance_CAL_table_undervaluation_overvalued5ff = calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued], DATASET$SDC$Event.Date[finance_company_subset_overvalued], Risk_Factors_Monthly,formula_used=five_factor_model_finance)
  finance_CAL_table_undervaluation = round(cbind(
    finance_CAL_table_undervaluation_undervalued3f$results,
    finance_CAL_table_undervaluation_overvalued3f$results,
    finance_CAL_table_undervaluation_undervalued5f$results,
    finance_CAL_table_undervaluation_overvalued5f$results,
    finance_CAL_table_undervaluation_undervalued5ff$results,
    finance_CAL_table_undervaluation_overvalued5ff$results
  ),2)
  colnames(finance_CAL_table_undervaluation) <- c("U:CAL3F", "t-stat", "p-value", "O:CAL3F", "t-stat", "p-value","U:CAL5F", "t-stat", "p-value", "O:CAL5F", "t-stat", "p-value","U:CAR5F+Fin", "t-stat", "p-value", "O:CAR5F+Fin", "t-stat", "p-value")
  
  finance_CAL_table_undervaluation_time = Reduce(cbind, lapply(1:nrow(periods_considered), function(i){
    periodnow = (DATASET$SDC$Event.Date >= periods_considered[i,1] & DATASET$SDC$Event.Date <= periods_considered[i,2])
    res = cbind(
      calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=three_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_undervalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_undervalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results,
      calendar_table(DATASET$returns_by_event_monthly[,finance_company_subset_overvalued & periodnow], DATASET$SDC$Event.Date[finance_company_subset_overvalued & periodnow], Risk_Factors_Monthly,formula_used=five_factor_model)$results
    )
    colnames(res) <- c(paste(paste(str_sub(periods_considered[i,1],start = 1, end = 4), str_sub(periods_considered[i,2],start = 1, end = 4), sep= "-"),"U 3FF", sep=" "),
                       "t-stat","p-value", "O 3FF", "t-stat","p-value", "U 5FF","t-stat","p-value"," O 5FF","t-stat"," p-value") 
    rownames(res)[nrow(res)] <- "Observations" 
    
    res    
  }))
  finance_CAL_table_undervaluation_time = round(finance_CAL_table_undervaluation_time,2)
  
  ######################################################
  # Just get back the whole dataset for now
  DATASET = DATASET_paper; rm("DATASET_all", "DATASET_paper") # THIS IS WHAT WE WILL HAVE IN THE PAPER!!!!!
  
})

### SAVE NOW
save(reportcode_data, file = paste("tmpfiles",filename_to_save, sep="/"))
