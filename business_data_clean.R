
business_data_removal <- function(events){
  
  cleanup = events$cleanup # to avoid losing info when we remove data. we add back at the end of the file
  
  BIZ_initial_data = nrow(events$SDC)
  
  No_filter = rep(T,length(events$SDC$Event.Date))
  Basic_filter =  No_filter # !is.na(events$SDC$ME_quantile) & !is.na(events$SDC$BEME_quantile)  
  Period_filter <- (as.Date(events$SDC$Event.Date) >= First) & (as.Date(events$SDC$Event.Date) <= Last)
  
  Penny_stock_filter = ifelse(events$SDC$Event.Date < "1995-01-01", scrub(events$SDC$Closing.Price) >= penny_stock_price_old, scrub(events$SDC$Closing.Price) >= penny_stock_price_recent) 
  
  Event_Industry <- suppressWarnings(scrub(as.numeric(events$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
  Industry_filter = Event_Industry %in% Industries
  US_only = (events$SDC$Currency %in% good_currencies)
  
  major_markets_only = sapply(events$SDC$Stock.Exchange, function(i) sum(str_split(i, "\\+")[[1]] %in% major_markets)>0)
  TOTAL_FILTER_basic = No_filter & Basic_filter & Period_filter & Penny_stock_filter  & Industry_filter & US_only & major_markets_only
  
  Market_cap_filter =  (scrub(events$SDC$Market.Cap) >= MIN_SIZE) & (scrub(events$SDC$Market.Cap) <= MAX_SIZE)
  ME_quantile_filter    =  scrub(events$SDC$ME_quantile)   >= MIN_ME_quantile   & scrub(events$SDC$ME_quantile)    <= MAX_ME_quantile  # & !is.na(events$SDC$ME_quantile)
  BEME_quantile_filter  =  scrub(events$SDC$BEME_quantile) >= MIN_BEME_quantile & scrub(events$SDC$BEME_quantile)  <= MAX_BEME_quantile #& !is.na(events$SDC$BEME_quantile)   
  #Leverage_filter = (scrub(events$SDC$lt/events$SDC$at) > 0.5)*(!is.na(events$SDC$lt/events$SDC$at))
  EventSize_filter = (events$SDC$Event.Size >= MIN_EVENT_SIZE) & (events$SDC$Event.Size <= MAX_EVENT_SIZE)
  
  # Remove announcements with less than days_live live days
  all_dates = as.Date(rownames(events$returns_by_event))
  event_dates = sapply(events$SDC$Event.Date, function(i) which(all_dates == i))
  live_days = sapply(1:ncol(events$returns_by_event), function(i) sum(events$returns_by_event[1:event_dates[i],i]!=0))
  small_live_days = (live_days >= days_live)
  
  # If we want to remove any "returns outliers" 
  returns_outliers = rep(T, length(events$SDC$Event.Date))
  if (remove_returns_outliers){
    returns_outliers <- (apply(events$returns_by_event,2,sd)!=0) & (apply(abs(events$returns_by_event),2,max) < MAX_DR_abs)
  } 
  TOTAL_FILTER_complex = Market_cap_filter & ME_quantile_filter & BEME_quantile_filter & EventSize_filter & small_live_days & returns_outliers
  
  ###
  TOTAL_FILTER = TOTAL_FILTER_basic & TOTAL_FILTER_complex
  
  # NOW REMOVE ALL THESE DEALS
  to_remove = which(!TOTAL_FILTER)
  if (length(to_remove) > 0){
    # just in alphabetic order not to forget any    
    events$Abn_returns <- events$Abn_returns[,-to_remove]
    events$Betas_PB6M <- events$Betas_PB6M[,-to_remove]
    events$Dates <- events$Dates[,-to_remove]
    events$DatesMonth <- events$DatesMonth[,-to_remove]
    events$SDC <- events$SDC[-to_remove,]
    for(field in ls(events$compustat_data))  events$compustat_data[[field]] <- events$compustat_data[[field]][-to_remove]
    for(field in ls(events$ibes))  events$ibes[[field]] <- events$ibes[[field]][-to_remove]
    events$returns_by_event <- events$returns_by_event[,-to_remove]
    events$returns_by_event_monthly <- events$returns_by_event_monthly[,-to_remove]    
  }
  
  ### 
  # Save the data removal variables in the cleanup and return events
  cleanup$biz_clean$initial_data <- BIZ_initial_data
  cleanup$biz_clean$total_removed = length(to_remove)
  cleanup$biz_clean$Basic_filter <- sum(!Basic_filter)
  cleanup$biz_clean$Period_filter <- sum(!Period_filter)
  cleanup$biz_clean$Penny_stock_filter <- sum(!Penny_stock_filter)
  cleanup$biz_clean$Industry_filter <- sum(!Industry_filter)
  cleanup$biz_clean$US_only <- sum(!US_only)
  cleanup$biz_clean$major_markets_only <- sum(!major_markets_only)
  cleanup$biz_clean$Market_cap_filter <- sum(!Market_cap_filter)
  cleanup$biz_clean$ME_quantile_filter <- sum(!ME_quantile_filter)
  cleanup$biz_clean$BEME_quantile_filter <- sum(!BEME_quantile_filter)
  cleanup$biz_clean$EventSize_filter <- sum(!EventSize_filter)
  cleanup$biz_clean$small_live_days <- sum(!small_live_days)
  cleanup$biz_clean$returns_outliers <- sum(!returns_outliers)
  
  events$cleanup = cleanup
  
  events
}

