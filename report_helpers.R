car_table_cache <- function(returns,Event.Date,Risk_Factors_Monthly,min_window = -6, max_window = 48,formula_used="(ri - RF) ~ Delta + SMB + HML + RMW + CMA") {
  factors_used = setdiff(unlist(str_split(gsub("~", ",", gsub("\\-", ",", gsub("\\+", ",", gsub("\\)", "",gsub("\\(", "",formula_used))))), " , ")),"ri")
  
  if (sum(!(factors_used %in% colnames(Risk_Factors_Monthly))))
    stop(paste("car_table misses the risk factors: ",factors_used[!(factors_used %in% colnames(Risk_Factors_Monthly))]))
  
  allmonths = min_window:max_window 
  
  factors_used_noRF = setdiff(factors_used, "RF")
  if (ncol(returns) < length(factors_used_noRF) + 1){ 
    results = matrix(0,nrow=length(allmonths), ncol = 3)
    results = rbind(results, rep(ncol(returns)))
    betas = matrix(0,nrow=length(allmonths), ncol = length(factors_used_noRF))
    betasstderr = matrix(0,nrow=length(allmonths), ncol = length(factors_used_noRF)) 
    colnames(results) <- c("CAL","t-stat","p-value")
    rownames(results) <- c(ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),"Observations")
    colnames(results) <- c("CAR","t-stat","p-value")
    colnames(betas) <- factors_used_noRF
    colnames(betasstderr) <- factors_used_noRF
    all_results = list(results = results, betas = betas, betasstderr = betasstderr)
    return(all_results) 
  }
  
  #Step 1: Build an event matrix, where all the events are aligned by event month as opposed to calendar month
  Event.Date = paste(str_sub(Event.Date,start=1,end=7), "01", sep="-") # just in case
  Row.Date <- as.Date(rownames(returns))
  firstHit = sapply(Event.Date, function(i) ifelse(i > tail(Row.Date,1), length(Row.Date)+1, ifelse(i < head(Row.Date,1), 0, which(Row.Date == i))))
  form  = as.formula(formula_used)
  Risk_Factors_Monthly = Risk_Factors_Monthly[,factors_used]
  if (!("RF" %in% factors_used)){
    Risk_Factors_Monthly = cbind(Risk_Factors_Monthly,matrix(0,nrow=nrow(Risk_Factors_Monthly)))
    colnames(Risk_Factors_Monthly)[ncol(Risk_Factors_Monthly)] <- "RF"
  }
  
  #Build event_month x company x factor matrix
  EVENT_ALIGNED <- array(0, c(length(allmonths), ncol(returns), ncol(Risk_Factors_Monthly)+1)) 
  for(ev in 1:length(Event.Date)) {
    starting      = max(1,min(nrow(returns),firstHit[ev] + min_window))
    ending        = max(1,min(nrow(returns),firstHit[ev] + max_window))
    EVENT_ALIGNED[(1 + (starting - (firstHit[ev] + min_window))):(length(allmonths) - (firstHit[ev] + max_window-ending)),ev,] <- as.matrix(cbind(Risk_Factors_Monthly[starting:ending,], returns[starting:ending,ev]))
  }
  dimnames(EVENT_ALIGNED) <- list( ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),
                                   c(paste(colnames(returns),Event.Date)),
                                   c(colnames(Risk_Factors_Monthly),"ri"))
  return(list(
    "EVENT_ALIGNED" = EVENT_ALIGNED,
    "Event.Date"    = Event.Date,
    "Row.Date"      = Row.Date,
    "Risk_Factors_Monthly" = Risk_Factors_Monthly,
    "form"          = form,
    "factors_used_noRF" = factors_used_noRF,
    "min_window"    = min_window,
    "max_window"    = max_window
  ))
}
#ignore allmonths for now, I don't think this can work
car_table_cached <- function(cache,thesubset=NULL) {
  if(is.null(thesubset)) 
    thesubset = 1:length(cache$Event.Date)
  #if(!is.null(allmonths)) {
  #monthindex = ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths)
  #} else {
  monthindex = rownames(cache$EVENT_ALIGNED)
  min_window = min(1,cache$min_window)
  max_window = max(-1,cache$max_window)
  allmonths = min_window:max_window 
  #}
  EVENT_ALIGNED        <- cache$EVENT_ALIGNED[monthindex,thesubset,]
  Event.Date           <- cache$Event.Date[thesubset]
  Row.Date             <- cache$Row.Date
  form                 <- cache$form
  factors_used_noRF    <- cache$factors_used_noRF
  Risk_Factors_Monthly <- cache$Risk_Factors_Monthly
  
  #Step2: now for each month, calculate the CAR. need to include month 0 and set that one to 0  
  alphas <- rep(0,length(allmonths))
  betas  <- array(0,c(length(allmonths),ncol(Risk_Factors_Monthly)-1))
  betasstderr <- array(0,c(length(allmonths),ncol(Risk_Factors_Monthly)-1))
  stderr <- rep(0,length(allmonths))
  dfs    <- as.integer(rep(0,length(allmonths)))
  
  Event.Date_number = as.numeric(as.Date(Event.Date))
  Row.Date_number = as.numeric(Row.Date)
  
  for (i in 1:length(allmonths)) {
    if (allmonths[i] !=0) { #we do not consider the month of the event
      #tmpdate = AddMonths(Event.Date,allmonths[i])    
      #ret <- EVENT_ALIGNED[i,(tmpdate <= tail(rownames(returns),1)) & (tmpdate >= head(rownames(returns),1)),]
      hitnow = sapply(Event.Date_number, function(j) (which(Row.Date_number == j) + allmonths[i]))
      ret <- EVENT_ALIGNED[i,(hitnow <= length(Row.Date_number)) & (hitnow >= 1),]
      ret <- ret[ret[,"ri"]!=0,]  # WE NEED THIS HERE!!!!!
      #ret <- ret[ret[,"ri"] <1,]  # WE NEED THIS HERE!!!!!
      if (nrow(ret) > ncol(ret)){
        model = fastLm(form,data=data.frame(ret,row.names = NULL))
        alphas[i] = summary(model)$coefficients[1,"Estimate"] 
        betas[i,] <- summary(model)$coefficients[2:nrow(summary(model)$coefficients), "Estimate"]
        betasstderr[i,] <- summary(model)$coefficients[2:nrow(summary(model)$coefficients), "StdErr"]    
        stderr[i] = coef(summary(model))[1, "StdErr"]
        dfs[i] = df.residual(model)
      } else{
        alphas[i] = 0
        betas[i,] <- 0
        betasstderr[i,] <- 0  
        stderr[i] = 0
        dfs[i] = 0
      }
    }
  }
  
  #summary CAR output: aggregate results for all windows
  results <- array(0,c(length(allmonths)+1,3))
  if (0 %in% allmonths){
    for (i in 1:(which(allmonths==0)-1)){
      thealpha = sum(alphas[(which(allmonths==0)-1):i]) # this is i:1 since we want to end up with a 0 at event month - 1 (and at event month)
      thestderr = sqrt(sum(stderr[(which(allmonths==0)-1):i]*stderr[(which(allmonths==0)-1):i])) # this is 1:i since we want to add them all from the oldest month
      tstat = ifelse(thestderr, thealpha / thestderr, 0) 
      results[i,] <- c( 
        thealpha,  
        tstat,
        2 * pt(abs(tstat), df = dfs[i], lower.tail = FALSE)    #pvalue
      )
    }
    #results[1:(which(allmonths==0)-1),1] <- -results[1:(which(allmonths==0)-1),1] # AS WE NEED TO START FROM HIGH AND END TO 0! - we do this only for the plots, so we fix it there only
    for (i in (which(allmonths==0)+1):length(allmonths)){
      thealpha = sum(alphas[(which(allmonths==0)+1):i])
      thestderr = sqrt(sum(stderr[(which(allmonths==0)+1):i]*stderr[(which(allmonths==0)+1):i]))
      tstat = ifelse(thestderr, thealpha / thestderr, 0) 
      results[i,] <- c( 
        thealpha,  
        tstat,
        2 * pt(abs(tstat), df = dfs[i], lower.tail = FALSE)    #pvalue
      )
    }
  } else {
    for (i in 1:length(allmonths)){
      thealpha = sum(alphas[1:i])
      thestderr = sqrt(sum(stderr[1:i]*stderr[1:i]))
      tstat = ifelse(thestderr, thealpha / thestderr, 0) 
      results[i,] <- c( 
        thealpha,  
        tstat,
        2 * pt(abs(tstat), df = dfs[i], lower.tail = FALSE)    #pvalue
      )
    }
  }
  results[,1] = 100*results[,1]
  rownames(results) <- c(ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),"Observations")
  colnames(results) <- c("CAR","t-stat","p-value")
  results[nrow(results),] <- rep(length(Event.Date),ncol(results)) 
  colnames(betas) <- factors_used_noRF
  colnames(betasstderr) <- factors_used_noRF
  all_results = list(results = results, betas = betas, betasstderr = betasstderr)
  return(all_results)
}










#Builds a calendar table like PV2009. Returns need to be monthly, same for the risk factors
calendar_table2 <- function(returns,Event.Date,Risk_Factors_Monthly,allmonths = NULL,min_window = -6, max_window = 48,formula_used="(ri - RF) ~ Delta + SMB + HML + RMW + CMA") {
  #data check
  if (class(Risk_Factors_Monthly) != "data.frame") {
    dates <- names(Risk_Factors_Monthly)
    Risk_Factors_Monthly = data.frame(market=Risk_Factors_Monthly)
    rownames(Risk_Factors_Monthly) <- dates
  }
  factors_used = setdiff(unlist(str_split(gsub("~", ",", gsub("\\-", ",", gsub("\\+", ",", gsub("\\)", "",gsub("\\(", "",formula_used))))), " , ")),"ri")
  factors_used_noRF = setdiff(factors_used, "RF")
  
  if(is.null(allmonths)) {
    min_window = min(1,min_window)
    max_window = max(-1,max_window)
    allmonths = min_window:max_window
  }
  
  
  if (sum(!(factors_used %in% colnames(Risk_Factors_Monthly))))
    stop(paste("calendar_table misses the risk factors: ",factors_used[!(factors_used %in% colnames(Risk_Factors_Monthly))]))
  
  if (ncol(returns) < length(factors_used_noRF) + 1){
    results = matrix(0,nrow=length(allmonths), ncol = 3)
    results = rbind(results, rep(ncol(returns),3))
    betas = matrix(0,nrow=length(allmonths), ncol = length(factors_used_noRF))
    betasstderr = matrix(0,nrow=length(allmonths), ncol = length(factors_used_noRF)) 
    colnames(results) <- c("CAL","t-stat","p-value")
    rownames(results) <- c(ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),"Observations")
    colnames(results) <- c("CAR","t-stat","p-value")
    colnames(betas) <- factors_used_noRF
    colnames(betasstderr) <- factors_used_noRF
    all_results = list(results = results, betas = betas, betasstderr = betasstderr)
    return(all_results) 
  }
  
  Row.Date <- as.Date(rownames(returns))
  form  = as.formula(formula_used)
  Event.Date = paste(str_sub(Event.Date,start=1,end=7), "01", sep="-") # just in case
  Risk_Factors_Monthly = Risk_Factors_Monthly[,factors_used]
  if (!("RF" %in% factors_used)){
    Risk_Factors_Monthly = cbind(Risk_Factors_Monthly,matrix(0,nrow=nrow(Risk_Factors_Monthly)))
    colnames(Risk_Factors_Monthly)[ncol(Risk_Factors_Monthly)] <- "RF"
  }
  
  alphas <- rep(0,length(allmonths))
  betas <- array(0,c(length(allmonths),ncol(Risk_Factors_Monthly)-1))
  betasstderr <- array(0,c(length(allmonths),ncol(Risk_Factors_Monthly)-1))
  stderr <- rep(0,length(allmonths))
  dfs <- as.integer(rep(0,length(allmonths)))
  
  Event.Date_number = as.numeric(as.Date(Event.Date))
  Row.Date_number = as.numeric(Row.Date)
  #Now create aggregate returns based on the window and calculate the model
  results <- array(0,c(length(allmonths)+1,3))
  for (i in 1:length(allmonths)) {
    #cat(i,",")
    w = allmonths[i]
    hitnow = sapply(Event.Date_number, function(j) Row.Date_number[min(length(Row.Date_number),max(1,(which(Row.Date_number == j) + w)))])
    ret <- returns
    if (w > 0)
      for(j in 1:length(Event.Date)) 
        ret[ Row.Date_number <= Event.Date_number[j] | Row.Date_number > hitnow[j],j ] <- 0
    if (w < 0)
      for(j in 1:length(Event.Date)) 
        ret[ Row.Date_number >= Event.Date_number[j] | Row.Date_number < hitnow[j],j ] <- 0
    ri <- apply(ret,1, function(x) non_zero_mean(scrub(x)))
    ri <- ri[ head(which(ri !=0),1) : tail(which(ri!=0),1)]
    
    data  = Risk_Factors_Monthly
    data  = data[rownames(data) >= min(names(ri)) & rownames(data) <= max(names(ri)),]
    data$ri = ri
    form  = as.formula(form)
    model = fastLm(form,data=data)
    
    results[i,] <- c( 
      summary(model)$coefficients[1,"Estimate"]*100,
      summary(model)$coefficients[1,"t.value"],
      summary(model)$coefficients[1,"p.value"]
    )
    betas[i,] <- summary(model)$coefficients[2:nrow(summary(model)$coefficients), "Estimate"]
    betasstderr[i,] <- summary(model)$coefficients[2:nrow(summary(model)$coefficients), "StdErr"]    
  }
  
  rownames(results) <- c(ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),"Observations")
  colnames(results) <- c("CAL","t-stat","p-value")
  results[nrow(results),] <- rep(length(Event.Date),ncol(results)) 
  colnames(betas) <- factors_used_noRF
  colnames(betasstderr) <- factors_used_noRF
  all_results = list(results = results, betas = betas, betasstderr = betasstderr)
  return(all_results)
}
