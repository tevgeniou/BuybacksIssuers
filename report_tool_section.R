#overwrite some variables that we need
if(!exists("BUYBACK_DATA2")) {
  BUYBACK_DATA2 <- BUYBACK_DATA
}
useonly_bb = which(BUYBACK_DATA2$DATASET$SDC$Event.Date >= input$startdate & BUYBACK_DATA2$DATASET$SDC$Event.Date <= input$enddate & scrub(BUYBACK_DATA2$DATASET$SDC$Market.Cap) >= input$market_cap_min & scrub(BUYBACK_DATA2$DATASET$SDC$Market.Cap) <= input$market_cap_max) 
useonly_is = which(ISSUERS_DATA$DATASET$SDC$Event.Date >= input$startdate & ISSUERS_DATA$DATASET$SDC$Event.Date <= input$enddate & scrub(ISSUERS_DATA$DATASET$SDC$Market.Cap) >= input$market_cap_min & scrub(ISSUERS_DATA$DATASET$SDC$Market.Cap) <= input$market_cap_max) 
subset_undervalued_bb <- company_subset_undervalued_bb[useonly_bb]
subset_overvalued_bb <- company_subset_overvalued_bb[useonly_bb]

BUYBACK_DATA$DATASET$SDC$Pre.Vol.Score <- BUYBACK_DATA$DATASET$SDC$pre_vol_Score
BUYBACK_DATA2$DATASET$SDC$Pre.Vol.Score <- BUYBACK_DATA2$DATASET$SDC$pre_vol_Score

###########################################################################
# HELPER FUNCTIONS FOR GENERATING SECTIONS
#
#
generate_section <- function(variable_name,reverse,quantile_used_ind) {
  if(is.null(variable_name)) {
    return(NULL) 
  }
  DS <- BUYBACK_DATA2$DATASET
  
  #do analysis first, add text later
  thesign = ifelse(reverse, -1, +1)
  thefeature = thesign*DS$SDC[[which(names(DS$SDC) == variable_name)]]  
  High_feature_events = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
  Low_feature_events = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
  
  #Test example: car table
  IRATStableBB = round(cbind(
    car_table_cached(CACHE5,Low_feature_events,allmonths = report_months_car)$results,
    car_table_cached(CACHE5,High_feature_events,allmonths = report_months_car)$results
  ),2)
  colnames(IRATStableBB) <- c("Low: CAR", "t-stat","p-value", "High: CAR", "t-stat","p-value")
  
  IRATStableBB_cal = round(cbind(
    calendar_table2(DS$returns_by_event_monthly[,Low_feature_events], DS$SDC$Event.Date[Low_feature_events], Risk_Factors_Monthly,report_months_cal)$results,
    calendar_table2(DS$returns_by_event_monthly[,High_feature_events], DS$SDC$Event.Date[High_feature_events], Risk_Factors_Monthly,report_months_cal)$results
  ),2)
  colnames(IRATStableBB_cal) <- c("Low: CAL", "t-stat","p-value", "High: CAL", "t-stat","p-value")
  
  #numbers for the under table
  tmp = get_feature_results(BUYBACK_DATA2$DATASET,variable_name, subset_undervalued_bb, subset_overvalued_bb, quantile_used_ind,180, method="Simple")
  IRATStable_underBB = tmp$feature_IRATStable_under;
  IRATStable_underBB_cal = tmp$feature_IRATStable_under_cal
  colnames(IRATStable_underBB) <- c("Low: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value", "High: U CAR", "t-stat","p-value","O CAR", "t-stat","p-value")
  colnames(IRATStableBB_cal) <- c("Low: CAL", "t-stat","p-value","High: CAL", "t-stat","p-value")
  High_VOL_BB_Hedged  = tmp$High_feature_Hedged; Low_VOL_BB_Hedged = tmp$Low_feature_Hedged
  
  #text generation
  cat("\\subsection{",variable_name,"}")
  cat("For each event we measure their pre-announce ",variable_name,". We define two 
      types of events: low ",variable_name," and high ",variable_name," events, 
      depending on whether the variable was in the top or bottom quantile of the ",variable_name,"of
      all CRSP companies in the pre-event month. In total we have ", sum(High_feature_events), " low ",variable_name," and ",sum(Low_feature_events),  " high ",variable_name," events. 
  ")
  #Table~\\ref{tbl:industriesAdditional} shows the 
  #percentage of high and low ",variable_name," events across all industries for which we have at 
  #least 100 buyback events in our sample.
  
  cat("Table~\\ref{tbl:",variable_name,"} shows the IRATS and Calendar Time abnormal returns for high
       and low ",variable_name," buybacks events-companies. Focusing on IRATS, high ",variable_name," 
       buyback stocks earn ",IRATStableBB["+48","High: CAR"],"$\\%$  after 48 months (t=" ,IRATStableBB["+48",5] ,"), while low ",variable_name," 
       buyback stocks earn ",IRATStableBB["+48","Low: CAR"],"$\\%$ (t=" ,IRATStableBB["+48",2] ,"). 
       The results using the Calendar Time method confirm these findings.\\footnote{We also calculated the returns of a 
       hedged strategy similar to Figure~\\ref{fig:ReturnsBB} (Panel~B). Starting in ", firstyear, " we form a portfolio of all stocks that announced a 
       buyback during the previous $N$ months and hold the stock for $N$ months. High ",variable_name, " companies earn cumulative excess returns of  ",
      round(tail(cumsum(100*High_VOL_BB_Hedged),1),1), "$\\%$ (", round(tail(cumsum(100*High_VOL_BB_Hedged48m),1),1), "$\\%$) for the 12 (48) month holding strategy, 
       compared to the ", round(tail(cumsum(100*Low_VOL_BB_Hedged),1),1), "$\\%$ (", round(tail(cumsum(100*Low_VOL_BB_Hedged48m),1),1), "$\\%$) 
       of the corresponding low ", variable_name," sample.}
  ",sep="")
  cat("Table~\\ref{tbl:under",variable_name,"} also tests whether the U-index is valid for high ",variable_name," 
       events, as well as whether adjusting for ",variable_name," improves the predictive power of the U-index. 
       Regardless of the time horizon and the event study method, the U-index works for high ",variable_name," 
       companies. After 48 months, based on the IRATS methods, high U-index high ",variable_name," companies earn ",
      IRATStable_underBB["+48","High: U CAR"],"$\\%$ (t=",IRATStable_underBB["+48",8],"). Low U-index 
       high ",variable_name," companies earn ",IRATStable_underBB["+48",10],"$\\%$, (t=",IRATStable_underBB["+48",11],").
       The Calendar Time results provide the same picture. Figure~\\ref{fig:pastallVOL} summarizes the results 
       for the total sample and the high and low U-index sample.
  ",sep="")
  #TODO: still need to add those figures
  
  
  #tables that will be returned and outputted in the appendix
  car_table <- printLatexTable2(
    IRATStableBB[reported_times,],
    IRATStableBB_cal[reported_times,],
    title=paste("Analyzing",variable_name),
    title1 = "Panel A: IRATs Cumulative Abnormal Returns",
    title2 = "Panel B: Calendar Method Monthly Abnormal Returns",
    caption="The table presents the long-run abnormal returns for firms in the high and low section respectively. The following regression is run each event month~$j$:
\\begin{eqnarray*}
(R_{i,t} - R_{f,t}) &=& a_j + b_j (R_{m,t} - R_{f,t}) + c_j {SMB}_t + d_j {HML}_t + e_t {RMW}_t + f_t {CMA}_t + \\epsilon_{i,t},
\\end{eqnarray*}
where $R_{i,t}$ is the monthly return on security $i$ in the calendar month $t$ that corresponds to the event month $j$, with $j = 0$ being the month of the repurchase announcement. $R_{f,t}$ and $R_{m,t}$ are the risk-free rate and the return on the equally weighted CRSP index, respectively. ${SMB}_t$, ${HML_t}$, ${RMW}_t$, ${CMA}_t$ are the monthly returns on the size, book-to-market factor, profitability factor and investment factor in month $t$, respectively. The numbers reported are sums of the intercepts of cross-sectional regressions over the relevant event-time-periods expressed in percentage terms. The standard error (denominator of the $t$-statistic) for a window is the square root of the sum of the squares of the monthly standard errors. Panel~B reports monthly average abnormal returns (AR) of equally weighted Calendar Time portfolios using the \\cite{FamaFrench2015a} five-factor model. In this method, event firms that have announced an open market buyback in the last calendar months form the basis of the calendar month portfolio. A single time-series regression is run with the excess returns of the calendar portfolio as the dependent variable and the returns of five factors as the independent variables. The significance levels are indicated by * and ** and correspond to a significance level of $5\\%$ and $1\\%$ respectively, using a two-tailed test.",
    
    label = paste("tbl:",variable_name,sep=""),
    bigtitleontop=T,
    titleontop=T,
    scale=1,
    lastSpecial=T,
    dorotate=F,
    returntext=T
  )
  #vol under table
  under_table <- printLatexTable2(
    cbind(IRATStableBB[reported_times,],IRATStable_underBB[reported_times,]),
    cbind(IRATStableBB_cal[reported_times,],IRATStable_underBB_cal[reported_times,]),
    columns= c("Low","High.","Low/High","Low/Low U-ind.","High/High U-ind.","High/Low U-ind."),
    title=paste("Buyback for Low and High ",variable_name," and  for Low and and High U-index companies"),
    title1 = "Panel A: IRATs Cumulative Abnormal Returns",
    title2 = "Panel B: Calendar Method Monthly Abnormal Returns",
    caption="This table presents the long-term abnormal return after open market repurchase announcements from the announcement date until $t$ months after, for low and high sections and  for low and  high U-index companies.  Regardless of event study method and time horizon, the U-index works only for idiosyncratic companies. Panel~A reports monthly cumulative average abnormal returns (CAR) in percent using Ibbotson's (1975) returns across time and security (IRATS) method combined with the \\cite{FamaFrench2015a} five-factor model for the sample of firms that announced an open market share repurchase plus various subsamples. The following regression is run each event month~$j$:
\\begin{eqnarray*}
(R_{i,t} - R_{f,t}) &=& a_j + b_j (R_{m,t} - R_{f,t}) + c_j {SMB}_t + d_j {HML}_t + e_t {RMW}_t + f_t {CMA}_t + \\epsilon_{i,t},
\\end{eqnarray*}
where $R_{i,t}$ is the monthly return on security $i$ in the calendar month $t$ that corresponds to the event month $j$, with $j = 0$ being the month of the repurchase announcement. $R_{f,t}$ and $R_{m,t}$ are the risk-free rate and the return on the equally weighted CRSP index, respectively. ${SMB}_t$, ${HML_t}$, ${RMW}_t$, ${CMA}_t$ are the monthly returns on the size, book-to-market factor, profitability factor and investment factor in month $t$, respectively. The numbers reported are sums of the intercepts of cross-sectional regressions over the relevant event-time-periods expressed in percentage terms. The standard error (denominator of the $t$-statistic) for a window is the square root of the sum of the squares of the monthly standard errors. Panel~B reports monthly average abnormal returns (AR) of equally weighted Calendar Time portfolios using the \\cite{FamaFrench2015a} five-factor model. In this method, event firms that have announced an open market buyback in the last calendar months form the basis of the calendar month portfolio. A single time-series regression is run with the excess returns of the calendar portfolio as the dependent variable and the returns of five factors as the independent variables. The significance levels are indicated by * and ** and correspond to a significance level of $5\\%$ and $1\\%$ respectively, using a two-tailed test.",
    label = paste("tbl:under",variable_name,sep=""),
    bigtitleontop=T,
    titleontop=T,
    metric1 = "CAR",
    metric2 = "AR",
    scale=1,
    lastSpecial=T,
    dorotate=T,
    returntext=T
  )
  return(
    list(
      "car_table" = car_table,
      "High" = High_feature_events,
      "Low" = Low_feature_events,
      "under_table" = under_table,
      "name" = variable_name,
      "reverse_sign" = reverse
    )
  )
}

#uses a couple of (global) variables from report_tool.R: report_months_car,report_months_cal,CACHE5,BUYBACK_DATA2
generate_U_section <- function(sec1,sec2,sec3,sec4,sec5,sec6,quantile_used_ind,quantile_used_all){
  #### ANALYSIS/CREATION OF CEU-INDEX
  DS <- BUYBACK_DATA2$DATASET
  
  thesign = ifelse(sec1$reverse_sign, -1, +1)
  thefeature = thesign*DS$SDC[[which(names(DS$SDC) == sec1$name)]]  
  High_feature_events1 = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
  Low_feature_events1 = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
  
  High_feature_events2 <- High_feature_events3 <- High_feature_events4 <- High_feature_events5 <- High_feature_events6 <- rep(FALSE,length(High_feature_events1))
  Low_feature_events2 <- Low_feature_events3 <- Low_feature_events4 <- Low_feature_events5 <- Low_feature_events6 <- 
    rep(FALSE,length(Low_feature_events1))
  
  feature_used_names= gsub("\\.", " ",sec1$name) 
  
  if (!is.null(sec2$name)){
    thesign = ifelse(sec2$reverse_sign, -1, +1)
    thefeature = thesign*DS$SDC[[which(names(DS$SDC) == sec2$name)]]  
    High_feature_events2 = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
    Low_feature_events2 = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
    feature_used_names = paste(feature_used_names,gsub("\\.", " ",sec2$name),sep=", ")  
  }
  if (!is.null(sec3$name)){
    thesign = ifelse(sec3$reverse_sign, -1, +1)
    thefeature = thesign*DS$SDC[[which(names(DS$SDC) == sec3$name)]]  
    High_feature_events3 = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
    Low_feature_events3 = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
    feature_used_names = paste(feature_used_names,gsub("\\.", " ",sec3$name),sep=", ")  
  }
  if (!is.null(sec4$name)){
    thesign = ifelse(sec4$reverse_sign, -1, +1)
    thefeature = thesign*DS$SDC[[which(names(DS$SDC) == sec4$name)]]  
    High_feature_events4 = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
    Low_feature_events4 = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
    feature_used_names = paste(feature_used_names,gsub("\\.", " ",sec4$name),sep=", ")  
  }
  if (!is.null(sec5$name)){
    thesign = ifelse(sec5$reverse_sign, -1, +1)
    thefeature = thesign*DS$SDC[[which(names(DS$SDC) == sec5$name)]]   
    High_feature_events5 = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
    Low_feature_events5 = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
    feature_used_names = paste(feature_used_names,gsub("\\.", " ",sec5$name),sep=", ")  
  }
  if (!is.null(sec6$name)){
    thesign = ifelse(sec6$reverse_sign, -1, +1)
    thefeature = thesign*DS$SDC[[which(names(DS$SDC) == sec6$name)]]  
    High_feature_events6 = (scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_ind)) & !is.na(thefeature))
    Low_feature_events6 = (scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_ind)) & !is.na(thefeature))
    feature_used_names = paste(feature_used_names,gsub("\\.", " ",sec6$name),sep=", ")  
  }
  
  Index_score = 
    2*High_feature_events1 + 1*(!High_feature_events1 & !Low_feature_events1) + 
    ifelse(is.null(sec2$name),0,2*High_feature_events2 + 1*(!High_feature_events2 & !Low_feature_events2)) + 
    ifelse(is.null(sec3$name),0,2*High_feature_events3 + 1*(!High_feature_events3 & !Low_feature_events3)) + 
    ifelse(is.null(sec4$name),0,2*High_feature_events4 + 1*(!High_feature_events4 & !Low_feature_events4)) + 
    ifelse(is.null(sec5$name),0,2*High_feature_events5 + 1*(!High_feature_events5 & !Low_feature_events5)) + 
    ifelse(is.null(sec6$name),0,2*High_feature_events6 + 1*(!High_feature_events6 & !Low_feature_events6)) 
  
  TOPQ = quantile(Index_score)["75%"]
  BOTQ = quantile(Index_score)["25%"]
  thefeature = Index_score
  High_feature_events = thefeature > TOPQ #(scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_all)) & !is.na(thefeature))
  Low_feature_events = thefeature < BOTQ #(scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_all)) & !is.na(thefeature))
  if (sum(High_feature_events) == 0)
    High_feature_events = thefeature >= TOPQ #(scrub(thefeature) >= quantile(thefeature[!is.na(thefeature)],1-as.numeric(quantile_used_all)) & !is.na(thefeature))
  if (sum(Low_feature_events) == 0)
    Low_feature_events = thefeature <= BOTQ #(scrub(thefeature) <= quantile(thefeature[!is.na(thefeature)],as.numeric(quantile_used_all)) & !is.na(thefeature))
  res = round(cbind(
    car_table_cached(CACHE5,Low_feature_events,allmonths=report_months_car)$results,
    car_table_cached(CACHE5,High_feature_events,allmonths=report_months_car)$results
  ),2)[reported_times,]
  colnames(res) <- c("Low: CAR", "t-stat","p-value", "High: CAR", "t-stat","p-value")
  
  res_cal = round(cbind(
    calendar_table2(DS$returns_by_event_monthly[,Low_feature_events], DS$SDC$Event.Date[Low_feature_events], Risk_Factors_Monthly,report_months_cal)$results,
    calendar_table2(DS$returns_by_event_monthly[,High_feature_events], DS$SDC$Event.Date[High_feature_events], Risk_Factors_Monthly,report_months_cal)$results
  ),2)[reported_times,]
  colnames(res_cal) <- c("Low: CAL", "t-stat","p-value", "High: CAL", "t-stat","p-value")
  
  #under_idio_bb2 generic construction
  nameslist <- list()
  sec7 <- list()
  High_feature_events7 <- subset_undervalued_bb
  Low_feature_events7 <- subset_overvalued_bb
  sec7$name <- "Valued"
  thefactors <- c(F,F,F,F,F,F,T)
  for(i in 1:7) {
    sec <- eval(parse(text=paste("sec",i,sep="")))
    if(!is.null(sec)) {
      thefactors[i] <- T
      nameslist <- c(nameslist,paste("Low",sec$name),paste("High",sec$name))
    }
  }
  thefactors <- which(thefactors)
  Under_IdioBB2 <- matrix(nrow = 2*length(thefactors),ncol=2*length(thefactors))
  for(i in 1:(length(thefactors))) {
    #First rows: factors vs eachother
    Highi <- eval(parse(text=paste("High_feature_events",thefactors[i],sep="")))
    Lowi <- eval(parse(text=paste("Low_feature_events",thefactors[i],sep="")))
    idxi <- 2*(i-1)+1
    for(j in 1:length(thefactors)) {
      Highj <- eval(parse(text=paste("High_feature_events",thefactors[j],sep="")))
      Lowj <- eval(parse(text=paste("Low_feature_events",thefactors[j],sep="")))
      idxj <- 2*(j-1)+1
      Under_IdioBB2[idxi,idxj]    <- 100*length(intersect(which(Lowi),Highj))/length(which(Lowi))
      Under_IdioBB2[idxi,idxj+1]  <- 100*length(intersect(which(Lowi),Lowj))/length(which(Lowi))
      Under_IdioBB2[idxi+1,idxj]  <- 100*length(intersect(which(Highi),Highj))/length(which(Highi))
      Under_IdioBB2[idxi+1,idxj+1]<- 100*length(intersect(which(Highi),Lowj))/length(which(Highi))
    }
  }
  rownames(Under_IdioBB2) <- nameslist
  colnames(Under_IdioBB2) <- nameslist
  
  ##Under idio BB corr generic construction
  nameslist <- list("U Index")
  EU_index_features <- BUYBACK_DATA2$Valuation_Index
  for(i in 1:((length(thefactors)-1))) {
    sec <- eval(parse(text=paste("sec",thefactors[i],sep="")))
    EU_index_features <- cbind(EU_index_features,DS$SDC[sec$name])
    nameslist <- c(nameslist,paste(sec$name))
  }
  Under_IdioBB_cor2 = cor(EU_index_features)
  colnames(Under_IdioBB_cor2) <- nameslist
  rownames(Under_IdioBB_cor2) <- nameslist
  
  #### TABLE GENERATION
  #some car table
  car_table <- printLatexTable2(
    res[reported_times,],
    res_cal[reported_times,],
    title=paste("Customized Enhanced U-index"),
    title1 = "Panel A: IRATs Cumulative Abnormal Returns",
    title2 = "Panel B: Calendar Method Monthly Abnormal Returns",
    caption="The table presents the long-run abnormal returns for firms after issue announcements with and without a subsequent buyback announcement on the full sample for different periods in time. Panel~A reports monthly cumulative average abnormal returns (CAR) in percent using Ibbotson's (1975) returns across time and security (IRATS) method combined with the \\cite{FamaFrench2015a} five-factor model for the sample of firms that announced an open market share repurchase plus various subsamples. The following regression is run each event month~$j$:
\\begin{eqnarray*}
(R_{i,t} - R_{f,t}) &=& a_j + b_j (R_{m,t} - R_{f,t}) + c_j {SMB}_t + d_j {HML}_t + e_t {RMW}_t + f_t {CMA}_t + \\epsilon_{i,t},
\\end{eqnarray*}
where $R_{i,t}$ is the monthly return on security $i$ in the calendar month $t$ that corresponds to the event month $j$, with $j = 0$ being the month of the repurchase announcement. $R_{f,t}$ and $R_{m,t}$ are the risk-free rate and the return on the equally weighted CRSP index, respectively. ${SMB}_t$, ${HML_t}$, ${RMW}_t$, ${CMA}_t$ are the monthly returns on the size, book-to-market factor, profitability factor and investment factor in month $t$, respectively. The numbers reported are sums of the intercepts of cross-sectional regressions over the relevant event-time-periods expressed in percentage terms. The standard error (denominator of the $t$-statistic) for a window is the square root of the sum of the squares of the monthly standard errors. Panel~B reports monthly average abnormal returns (AR) of equally weighted Calendar Time portfolios using the \\cite{FamaFrench2015a} five-factor model. In this method, event firms that have announced an open market buyback in the last calendar months form the basis of the calendar month portfolio. A single time-series regression is run with the excess returns of the calendar portfolio as the dependent variable and the returns of five factors as the independent variables. The significance levels are indicated by * and ** and correspond to a significance level of $5\\%$ and $1\\%$ respectively, using a two-tailed test.",
    
    label = "tbl:EEUindex",
    bigtitleontop=T,
    titleontop=T,
    scale=1,
    lastSpecial=T,
    dorotate=F,
    returntext=T
  )
  #CEU_IRATStable_bb / CEU_CALtable_bb
  CEU_IRATStable_bb <- NULL
  CEU_CALtable_bb <- NULL
  for (i in sort(unique(Index_score))){
    CEU_events_now = which(Index_score == i)
    CEU_IRATStable_bb = cbind(CEU_IRATStable_bb,car_table_cached(CACHE5,CEU_events_now,allmonths=report_months_car)$results)
    CEU_CALtable_bb = cbind(CEU_CALtable_bb,calendar_table2(DS$returns_by_event_monthly[,CEU_events_now], DS$SDC$Event.Date[CEU_events_now], Risk_Factors_Monthly,report_months_cal)$results)
  }
  if(length(unique(Index_score)) < 4) {
    CEU_IRATStable_bb_latex <- printLatexTable(
      CEU_IRATStable_bb[reported_times,1:(3*length(unique(Index_score)))],
      columns = paste("CEU-index",unique(Index_score)),
      title="Buyback announcements IRATS for all EU-index Values",
      caption=paste("IRATS five factor cumulative abnormal returns after open market repurchase announcements for each Customised Enhanced Undervaluation Index value from 0 to ", max(Index_score),".  We calculate the CEU-index simply as the sum of 0, 1, 2, numbers for low, middle, and high for the selected components of the CEU-Index. For each CEU-index value, we report the monthly cumulative average abnormal returns (CAR) in percent using Ibbotson's (1975) returns across time and security (IRATS) method combined with the \\cite{FamaFrench2015a} five-factor model for the sample of firms that announced an open market share repurchase plus various subsamples. The following regression is run each event month~$j$:
  \\begin{eqnarray*}
  (R_{i,t} - R_{f,t}) &=& a_j + b_j (R_{m,t} - R_{f,t}) + c_j {SMB}_t + d_j {HML}_t + e_t {RMW}_t + f_t {CMA}_t + \\epsilon_{i,t},
  \\end{eqnarray*}
  where $R_{i,t}$ is the monthly return on security $i$ in the calendar month $t$ that corresponds to the event month $j$, with $j = 0$ being the month of the repurchase announcement. $R_{f,t}$ and $R_{m,t}$ are the risk-free rate and the return on the equally weighted CRSP index, respectively. ${SMB}_t$, ${HML_t}$, ${RMW}_t$, ${CMA}_t$ are the monthly returns on the size, book-to-market factor, profitability factor and investment factor in month $t$, respectively. The numbers reported are sums of the intercepts of cross-sectional regressions over the relevant event-time-periods expressed in percentage terms. The standard error (denominator of the $t$-statistic) for a window is the square root of the sum of the squares of the monthly standard errors.", sep=""),
      label = "tbl:CEUunderBB",
      titleontop=T,lastSpecial=T,returntext=T,metric = "CAR"
    )
    CEU_CALtable_bb_latex <- printLatexTable(
      CEU_CALtable_bb[reported_times,1:(3*length(unique(Index_score)))],
      columns = paste("CEU-index",unique(Index_score)),
      title="Buyback announcements Calendar Time for all EU-index Values",
      caption=paste("IRATS five factor cumulative abnormal returns after open market repurchase announcements for each Customised Enhanced Undervaluation Index value from 0 to ", max(Index_score),".  We calculate the EU-index simply as the sum of three numbers:  high Peyer and Vermaelen (2009) U-index terms get a score of 2, low get a 0; high idiosyncratic terms get a score of 2, low get a 0; and high volatility terms get a score of 2, low get a 0.  Firms that get neither 0 nor 2 (hence are in the middle of the range) get a score of 1 for each of these 3 scores. For each EU-index value, we report the monthly average abnormal returns (AR) of equally weighted Calendar Time portfolios using the \\cite{FamaFrench2015a} five-factor model. In this method, event firms that have announced an open market buyback in the last calendar months form the basis of the calendar month portfolio. A single time-series regression is run with the excess returns of the calendar portfolio as the dependent variable and the returns of five factors (the difference between the risk-free rate and the return on the equally weighted CRSP index, the monthly return on the size, book-to-market factor, profitability factor and investment factor in month) as the independent variables. The significance levels are indicated by * and ** and correspond to a significance level of $5\\%$ and $1\\%$ respectively, using a two-tailed test.", sep=""),
      label = "tbl:CEUunderBBcal",metric="AR",lastSpecial=T,returntext=T
    )    
  } else {
    CEU_IRATStable_bb_latex <- printLatexTable2(
      CEU_IRATStable_bb[reported_times,1:(3*4)],
      CEU_IRATStable_bb[reported_times,13:(3*length(unique(Index_score)))],
      columns = paste("CEU-index",unique(Index_score)[1:4]),
      columns2 = paste("CEU-index",unique(Index_score)[5:length(unique(Index_score))]),
      title="Buyback announcements IRATS for all EU-index Values",
      caption=paste("IRATS five factor cumulative abnormal returns after open market repurchase announcements for each Customised Enhanced Undervaluation Index value from 0 to ", max(Index_score),".  We calculate the CEU-index simply as the sum of 0, 1, 2, numbers for low, middle, and high for the selected components of the CEU-Index. For each CEU-index value, we report the monthly cumulative average abnormal returns (CAR) in percent using Ibbotson's (1975) returns across time and security (IRATS) method combined with the \\cite{FamaFrench2015a} five-factor model for the sample of firms that announced an open market share repurchase plus various subsamples. The following regression is run each event month~$j$:
  \\begin{eqnarray*}
                    (R_{i,t} - R_{f,t}) &=& a_j + b_j (R_{m,t} - R_{f,t}) + c_j {SMB}_t + d_j {HML}_t + e_t {RMW}_t + f_t {CMA}_t + \\epsilon_{i,t},
                    \\end{eqnarray*}
                    where $R_{i,t}$ is the monthly return on security $i$ in the calendar month $t$ that corresponds to the event month $j$, with $j = 0$ being the month of the repurchase announcement. $R_{f,t}$ and $R_{m,t}$ are the risk-free rate and the return on the equally weighted CRSP index, respectively. ${SMB}_t$, ${HML_t}$, ${RMW}_t$, ${CMA}_t$ are the monthly returns on the size, book-to-market factor, profitability factor and investment factor in month $t$, respectively. The numbers reported are sums of the intercepts of cross-sectional regressions over the relevant event-time-periods expressed in percentage terms. The standard error (denominator of the $t$-statistic) for a window is the square root of the sum of the squares of the monthly standard errors.", sep=""),
      label = "tbl:CEUunderBB",
      bigtitleontop=T,titleontop=T, metric1="CAR",metric2="CAR",scale=1,lastSpecial=T,dorotate=T,returntext=T
    )
    CEU_CALtable_bb_latex <- printLatexTable2(
      CEU_CALtable_bb[reported_times,1:(3*4)],
      CEU_CALtable_bb[reported_times,13:(3*length(unique(Index_score)))],
      columns = paste("CEU-index",unique(Index_score)[1:4]),
      columns2 = paste("CEU-index",unique(Index_score)[5:length(unique(Index_score))]),
      title="Buyback announcements Calendar Time for all EU-index Values",
      caption=paste("IRATS five factor cumulative abnormal returns after open market repurchase announcements for each Customised Enhanced Undervaluation Index value from 0 to ", max(Index_score),".  We calculate the EU-index simply as the sum of three numbers:  high Peyer and Vermaelen (2009) U-index terms get a score of 2, low get a 0; high idiosyncratic terms get a score of 2, low get a 0; and high volatility terms get a score of 2, low get a 0.  Firms that get neither 0 nor 2 (hence are in the middle of the range) get a score of 1 for each of these 3 scores. For each EU-index value, we report the monthly average abnormal returns (AR) of equally weighted Calendar Time portfolios using the \\cite{FamaFrench2015a} five-factor model. In this method, event firms that have announced an open market buyback in the last calendar months form the basis of the calendar month portfolio. A single time-series regression is run with the excess returns of the calendar portfolio as the dependent variable and the returns of five factors (the difference between the risk-free rate and the return on the equally weighted CRSP index, the monthly return on the size, book-to-market factor, profitability factor and investment factor in month) as the independent variables. The significance levels are indicated by * and ** and correspond to a significance level of $5\\%$ and $1\\%$ respectively, using a two-tailed test.", sep=""),
      label = "tbl:CEUunderBBcal",
      bigtitleontop=T,titleontop=T,metric1="AR",metric2="AR",scale=1,lastSpecial=T,dorotate=T,returntext=T
    )
  }
  
  #### TEXT GENERATION
  cat("\\subsection{Customized Enhanced U-Index}")
  #TODO: all these numbers need to come from our current analysis
  cat(paste("In this section we analyze a Customized  Enhanced U-Index (CEU-Index), comprised of all the  
         event/firm characteristics selected (",feature_used_names ,"), following the same process as for the 
         EU-Index above (e.g. adding 0, 1, or 2 for each of the 
         selected characteristics, depending on whether the firm was in the middle, bottom, or top percentile of the all CRSP firms). 
         High and low CEU-Index events are defined so that roughly the top/bottom 25$\\%$ 
         of the events belong in these two categories. We report the results of this CEU-Index as done for the EU-Index 
         in Section \\ref{sec:newindex}.
         Figure~\\ref{fig:CEundervaluationindex} shows the distribution of the CEU-index. 
          The index has a symmetric distribution with a mean of ", round(mean(Index_score),2), ". 
            ", sep="")
      #Table~\\ref{tbl:CEUrelations} shows how the CEU-index relates to a number of firm characteristics. All variables are as in Table~\\ref{tbl:EUrelations}.  
  )
  
  # NEED TO UPDATE ALL NUMBERS AND FIGURES/TABLES HERE. Note that we don't use Under_IdioBB2 and Under_IdioBB_cor2 now
  # CHANGE THESE, TOO:
  #High_CEU_BB_Hedged = 0; Low_CEU_BB_Hedged = 0; High_CEU_BB_Hedged48m = 0; Low_CEU_BB_Hedged48m = 0 
  
  high_CEU_bb = High_feature_events
  low_CEU_bb = Low_feature_events
  High_CEU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", high_CEU_bb,  DS$DatesMonth, DS$returns_by_event_monthly,event=1),1,non_zero_mean)
  High_CEU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_CEU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  Low_CEU_BB = apply(PNL_matrix_BB(start_date_event,"One.Year.After", low_CEU_bb,  DS$DatesMonth, DS$returns_by_event_monthly,event=1),1,non_zero_mean)
  Low_CEU_BB_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_CEU_BB,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  High_CEU_BB48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", high_CEU_bb,  DS$DatesMonth, DS$returns_by_event_monthly,event=1),1,non_zero_mean)
  High_CEU_BB_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_CEU_BB48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  Low_CEU_BB48m = apply(PNL_matrix_BB(start_date_event,"Four.Years.After", low_CEU_bb,  DS$DatesMonth, DS$returns_by_event_monthly,event=1),1,non_zero_mean)
  Low_CEU_BB_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_CEU_BB48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)    
  
  cat("Tables~\\ref{tbl:CEUunderBB} and~\\ref{tbl:CEUunderBBcal} show respectively the IRATS and Calendar Time monthly abnormal 
      returns for all values of the CEU-index. Focusing on IRATS, as the CEU-index increases, the long term abnormal returns change 
      (from ", round(res["+48",1],1),"$\\%$ to ", round(res["+48",ncol(res)-2],1),"$\\%$). 
      Figure~\\ref{fig:CEpastall} show the same results over time for each CEU-index.\\footnote{We also calculated the returns of a hedged strategy similar to 
      Figure~\\ref{fig:ReturnsBB} (Panel~B). Starting in ", firstyear," we form a portfolio of all stocks 
      that announced a buyback during the previous $N$ months and hold the stock for $N$ months. 
      The 12-month holding period high CEU-index portfolio has average annual abnormal returns 
      of ", round(pnl_stats(High_CEU_BB_Hedged)[1],1), "$\\%$, while the low CEU-index one earns ", 
      round(pnl_stats(Low_CEU_BB_Hedged)[1],1), "$\\%$ annual excess returns. For the 48-month holding 
      periods the high and low EU-index portfolio earn annual excess returns of  respectively, ", 
      round(pnl_stats(High_CEU_BB_Hedged48m)[1],1),"$\\%$ and ", round(pnl_stats(Low_CEU_BB_Hedged48m)[1],1), 
      "$\\%$.}",sep="")
  
  return(list(
    "Index_score" = Index_score,
    "car_table" = car_table,
    "Under_IdioBB2" = Under_IdioBB2,
    "Under_IdioBB_cor2" = Under_IdioBB_cor2,
    "CEU_IRATStable_bb" = CEU_IRATStable_bb,
    "CEU_IRATStable_bb_latex" = CEU_IRATStable_bb_latex,
    "CEU_CALtable_bb_latex" = CEU_CALtable_bb_latex
  ))
  
}


generate_industry_table <- function(sec1,sec2,sec3,sec4,sec5,sec6) {
  #Event_Industries <- suppressWarnings(scrub(as.numeric(BUYBACK_DATA2$DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
  #Event_Industries<- sapply(Event_Industries, function(i){
  #  x=as.numeric(i)
  #  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  #  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
  #})
  
  Event_Industries <- BUYBACK_DATA2$DATASET$SDC$Industry
  
  industry_tableBB = sort(table(Event_Industries), decreasing = TRUE)
  industry_tableBB=industry_tableBB[-which(names(industry_tableBB)=="Strange")]
  names(industry_tableBB)<- gsub("_"," ",names(industry_tableBB))
  
  industry_BB_features <- Reduce(rbind,lapply(which(industry_tableBB > 100), function(i){
    this_industry = which(Event_Industries == gsub(" ","_", names(industry_tableBB)[i]))
    if (length(this_industry) != industry_tableBB[i])
      stop("\nFunny industry problem\n")
    res = matrix(
      c(
        ifelse(is.null(sec1),-Inf,100*length(intersect(this_industry,which(sec1$High)))/(length(this_industry))),
        ifelse(is.null(sec1),-Inf,100*length(intersect(this_industry,which(sec1$Low)))/(length(this_industry))),
        ifelse(is.null(sec2),-Inf,100*length(intersect(this_industry,which(sec2$High)))/(length(this_industry))),
        ifelse(is.null(sec2),-Inf,100*length(intersect(this_industry,which(sec2$Low)))/(length(this_industry))),
        ifelse(is.null(sec3),-Inf,100*length(intersect(this_industry,which(sec3$High)))/(length(this_industry))),
        ifelse(is.null(sec3),-Inf,100*length(intersect(this_industry,which(sec3$Low)))/(length(this_industry))),
        ifelse(is.null(sec4),-Inf,100*length(intersect(this_industry,which(sec4$High)))/(length(this_industry))),
        ifelse(is.null(sec4),-Inf,100*length(intersect(this_industry,which(sec4$Low)))/(length(this_industry))),
        ifelse(is.null(sec5),-Inf,100*length(intersect(this_industry,which(sec5$High)))/(length(this_industry))),
        ifelse(is.null(sec5),-Inf,100*length(intersect(this_industry,which(sec5$Low)))/(length(this_industry))),
        ifelse(is.null(sec6),-Inf,100*length(intersect(this_industry,which(sec6$High)))/(length(this_industry))),
        ifelse(is.null(sec6),-Inf,100*length(intersect(this_industry,which(sec6$Low)))/(length(this_industry))),
        #TODO add the "new" U valuation index here
        100*length(intersect(this_industry,which(subset_undervalued_bb)))/(length(this_industry)),
        100*length(intersect(this_industry,which(subset_overvalued_bb)))/(length(this_industry))
      )
      , nrow=1)
    res <- res[-which(res==-Inf)]
    res
  }))
  nameslist <- list()
  for(i in 1:6) {
    sec <- eval(parse(text=paste("sec",i,sep="")))
    if(!is.null(sec)) {
      nameslist <- c(nameslist,paste("High",sec$name),paste("Low",sec$name))
    }
  }
  #industry_BB_features <- as.numeric(industry_BB_features)
  colnames(industry_BB_features) <- c(unlist(nameslist),"U/valued","O/valued")
  rownames(industry_BB_features)<-names(industry_tableBB)[which(industry_tableBB > 100)]
  return(industry_BB_features)
}