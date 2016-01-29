

report_data<-new.env()

# THIS CODE USES A DATASET TO GENERATE ALL NECESSARY SLOW TO COMPUTE DATA FOR THE .Rnw AND SAVE THEM IN A FILE WITH THE NAME OF THE DATASET
with(report_data,{
  
  cleanup = DATASET$cleanup # to avoid losing info when we remove data. we add back at the end of the file

  # Add the Dates and order in time
  DATASET$Dates <- create_dates(as.Date(DATASET$SDC$Event.Date)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
  colnames(DATASET$Dates) <- DATASET$SDC$permno
  ordered_events = sort(as.numeric(DATASET$SDC$Event.Date),index.return = T)$ix
  if (length(unique(diff(ordered_events)))!=1)
    stop(paste("The time order was messed up somewhere"), DATASET$name, sep=" ")
  rm("ordered_events")
  
  DATASET$DatesMonth <- create_dates_month(as.Date(DATASET$SDC$Event.Date)) # We don't need this any more, can simplify to only get the dates needed... it's ok for now, as it is now slow
  colnames(DATASET$DatesMonth) <- DATASET$SDC$permno
  
  ####### Move on with calculating what the .Rnw will later need - whatever is "slow"
  
  ########################################################################################################################
  ### Idiosyncratic risk and Abnormal performance calculation
  ########################################################################################################################
  Betas_PB6M <- Betas_lm("Six.Month.Before","One.Day.Before",DATASET$Dates, DATASET$returns_by_event, Risk_Factors)
  no_beta_available = which(apply(Betas_PB6M,2,function(r) (sum(is.na(r)) !=0)))
  cleanup$REPORT_no_beta_available = length(no_beta_available) # last cleanup we do!
  
  DATASET$SDC$Rsq_returns <- Betas_PB6M["Rsq",]
  Betas_PB6M  <- Betas_PB6M[setdiff(colnames(Risk_Factors),"RF"),]  
  
  DATASET$SDC$pre_vol <- VOL_lm("Six.Month.Before","One.Day.Before",DATASET$Dates, DATASET$returns_by_event)
  too_much_vol = which(DATASET$SDC$pre_vol > max_vol_outlier)
  cleanup$REPORT_too_much_vol = length(too_much_vol) # last cleanup we do!
  
  to_remove = unique(c(no_beta_available,too_much_vol))
  
  if (length(to_remove)>0){
    DATASET$Dates <- DATASET$Dates[,-to_remove]
    DATASET$SDC <- DATASET$SDC[-to_remove,]
    DATASET$returns_by_event <- DATASET$returns_by_event[,-to_remove]
    DATASET$returns_by_event_monthly <- DATASET$returns_by_event_monthly[,-to_remove]    
    for(field in ls(DATASET$compustat_data))  DATASET$compustat_data[[field]] <- DATASET$compustat_data[[field]][-to_remove]
    for(field in ls(DATASET$ibes))  DATASET$ibes[[field]] <- DATASET$ibes[[field]][-to_remove]
    Betas_PB6M = Betas_PB6M[,-to_remove,drop=F]
  }
  
  RFfield    <- which(colnames(Risk_Factors) == "RF")
  factors     <- as.matrix(Risk_Factors[,colnames(Risk_Factors)!="RF"], drop=F)  
  Abn_returns <- DATASET$returns_by_event - factors %*% Betas_PB6M[colnames(factors),]  #to be sure that factors and Betas have same ordering of factors here
  if (length(RFfield)==1){
    Risk_Factors_mat = as.matrix(Risk_Factors[,"RF",drop=F])%*%matrix(rep(1,ncol(Abn_returns)), nrow=1)
    Abn_returns = Abn_returns -Risk_Factors_mat
  }
  colnames(Abn_returns) <- colnames(DATASET$returns_by_event)
  rownames(Abn_returns) <- rownames(DATASET$returns_by_event)
  
  DATASET$Abn_returns <- Abn_returns
  rm("RFfield", "factors", "Abn_returns","to_remove","no_beta_available","Risk_Factors_mat")
  DATASET$cleanup <- cleanup # add it back
})

save(report_data, file = paste("tmpfiles",filename_to_save, sep="/"))





