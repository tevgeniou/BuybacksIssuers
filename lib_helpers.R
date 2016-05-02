# Required R libraries (need to be installed - it can take a few minutes the first time you run the project)

# installs all necessary libraries from CRAN
get_libraries <- function(filenames_list) { 
  lapply(filenames_list,function(thelibrary){    
    if (do.call(require,list(thelibrary)) == FALSE) 
      do.call(install.packages,list(thelibrary)) 
    do.call(library,list(thelibrary))
  })
}


libraries_used=c("stringr","gtools","foreign","reshape2","digest","timeDate","devtools","knitr","graphics",
                 "grDevices","xtable","sqldf","stargazer","data.table",
                 "Hmisc","vegan","fpc","GPArotation","FactoMineR","cluster",
                 "psych","stringr","googleVis", "png","ggplot2","googleVis", "gridExtra","RcppArmadillo","xts","DescTools")

get_libraries(libraries_used)
Rcpp::sourceCpp('lib_helpers.cpp', embeddedR=FALSE)

options(stringsAsFactors=FALSE)


# Summary table
my_summary <- function(x){
  nas = sum(is.na(x))
  x=x[!is.na(x)]
  res = c(mean(x), median(x),min(x),quantile(x,0.1), quantile(x,0.25), quantile(x,0,95), quantile(x,0.9), max(x), sd(x), length(x))
  names(res) <- c("mean", "p50", "min", "p10", "p25", "p75","p90","max","sd", "N")
  res
}

non_na_mean <- function(x) { mean(x[!is.na(x) & x != 0]) }
non_na_sd <- function(x) { sd(x[!is.na(x) & x != 0]) }

remove_initialization_time <- function(x,min_date=NULL) { # Note: Added also min_date as an argument!! THis way we remove data from any given date!
  if (!is.null(min_date)){
    res = x[as.Date(names(x)) >= as.Date(min_date)]
  } else {
    if (x[1] != 0){
      res = x
    } else {
      if (sum(x!=0) != 0){
        #res = tail(res, -head(which(diff(res)!=0),1))
        #names(res) <- tail(names(res), -head(which(diff(res)!=0),1))
        
        tmp <- names(x)
        ending <- -head(which(diff(x)!=0),1)
        res = tail(x, ending)
        names(res) <- tail(tmp, ending)
      }
    }
  }
  res
}

yeardays<-function(x){
  fd<-as.integer(as.Date(head(names(x),1),format="%Y-%m-%d"))
  ld<-as.integer(as.Date(tail(names(x),1),format="%Y-%m-%d"))
  365.25*length(x)/(ld-fd)
}

vol_pa<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE()) {
  if(is.null(names(x))){ good_days <- TRUE }else { good_days <- isBizday(as.timeDate(names(x)), holidays, wday = 1:5) }
  x1<-drop(x[exclude_zero & good_days])
  sqrt(yeardays(x1))*sd(x1)
}

sharpe<-function(x,exclude_zero=(x!=0), holidays = holidayNYSE() ) {
  if(is.null(names(x))){ good_days <- TRUE }else { good_days <- isBizday(as.timeDate(names(x)), holidays, wday = 1:5) }
  x1<-drop(x[exclude_zero & good_days])
  round(yeardays(x1)*mean(x1)/vol_pa(x1),digits=2)   # annualized
}

bps<-function(x,exclude_zero=(x!=0))round(10000*mean(drop(x[exclude_zero])),digits=2)

drawdown<-function(x)round(100*max(cummax(cumsum(x))-cumsum(x)),digits=2)

pnl_stats<-function(x, show_tr = FALSE, show_gr = FALSE){
  if(class(x)=="matrix")if(ncol(x)>1)x<-x[,1]
  ret<-c(Ret=round(100*yeardays(x)*mean(x),digits=1),Vol=round(100*vol_pa(x),digits=1),Sharpe=round(sharpe(x),2),DD=round(drawdown(x),1))
  ret
}

pnl_plot<-function(x,...){
  pargs<-as.list(match.call(expand.dots=TRUE))
  if(!"ylab" %in% names(pargs)) ylab<-deparse(substitute(x)) else ylab<-pargs$ylab
  if(!"main" %in% names(pargs)) main<-paste(names(pnl_stats(x)),pnl_stats(x),sep=":",collapse=" ") else main<-pargs$main
  plot_arguments<-c(list(x=cumsum(x*100),type="l",ylab=ylab,cex.main = 0.9, main=main,axes=FALSE),pargs[setdiff(names(pargs),c("","x","ylab","main"))])
  do.call(plot,plot_arguments)
  if(!is.null(names(x))){
    axis(1,at=seq(1,length(x),length.out=5),labels=names(x)[seq(1,length(x),length.out=5)])
    axis(2)
  } else { axis(1); axis(2)}
}

pnl_matrix<-function(perf, digits = 2){
  
  month_map<-c("01"="Jan","02"="Feb","03"="Mar","04"="Apr","05"="May","06"="Jun","07"="Jul","08"="Aug","09"="Sep","10"="Oct","11"="Nov","12"="Dec")
  perf_dates<-structure(do.call(rbind,strsplit(names(perf),"-")),dimnames=list(names(perf),c("Year","Month","Day")))
  perf_dates[,"Month"]<-month_map[perf_dates[,"Month"]]
  perf_years  <- sort(unique(perf_dates[,"Year"]))
  perf_months <- month_map
  res<-structure(
    outer(perf_years,perf_months,function(i_vec,j_vec)mapply(function(i,j){
      perf_ndx <- perf_dates[,"Year"]==i & perf_dates[,"Month"]==j
      if(sum(perf_ndx)==0)return(NA)
      prod(perf[perf_ndx]+1)-1
    },i_vec,j_vec)),
    dimnames=list(perf_years,perf_months)
  )
  round(cbind(res,Year=apply(res,1,function(r)prod(r[!is.na(r)]+1)-1))*100,digits=2)
}

show_stats<-function(x)paste(names(pnl_stats(x)),pnl_stats(x),sep=":",collapse=" \\textbar ")

###

# if number of non-zeroes of x is less than n, return 0, else return the mean of the non-zero entries 
non_zero_mean<-function(x,n=1)ifelse(sum(scrub(x)!=0)<n,0,mean(scrub(x)[scrub(x)!=0]))
# moving average of previous n elements : 0 for first n-1 elements
ma<-function(x,n,f=identity){res<-as.numeric(filter(f(x),rep(1/n,n),method="convolution",sides=1,circular=FALSE)); ifelse(is.na(res),0,res)}
# moving sum of previous n elements : 0 for first n-1 elements
ms<-function(x,n,f=identity){res<-as.numeric(filter(f(x),rep(1,n),method="convolution",sides=1,circular=FALSE)); ifelse(is.na(res),0,res)}
# shift forward if n +ve , backward if n -ve.
moving_average<-function(n)function(x)ma(x,n)
moving_sum<-function(n)function(x)ms(x,n)

shift<-function(a,n=1,filler=0){
  x<-switch(class(a),matrix=a,matrix(a,ncol=1,dimnames=list(names(a),NULL)))
  if(n==0)return(x)
  if(n>0){
    rbind(matrix(filler,ncol=ncol(x),nrow=n),head(x,-n)) 
  } else {
    rbind(tail(x,n),matrix(filler,ncol=ncol(x),nrow=abs(n)))
  }
}
# replace non-finite elements in x with zeroes
scrub<-function(x){
  if(length(x)==0)return(0)
  x[which(!is.finite(x))]<-0
  x
}

# Fill NAs with earlier values

fill_NA_previous <- function(x){
  if (sum(!is.na(x)) == 0)
    return(x)
  non_na = which(!is.na(x))
  if (length(non_na) == 1)
    return(c(rep(x[non_na], non_na-1), x[non_na:length(x)]))
  if (!(1 %in% non_na))  non_na = c(1,non_na)
  c(unlist(lapply(1:(length(non_na)-1), function(i) rep(x[non_na[i]],non_na[i+1] - non_na[i]))), x[tail(non_na,1):length(x)])
}

######

# roll function over multiple matrix rows, in parallel. mapply for matrices.
multiroll_fun<-function(fun,w,...,MoreArgs=NULL,verbose=identity){
  data<-list(...)
  if(length(data)==0)stop("multiroll: data required")
  if(length(data)!=length(formals(fun)))stop("multiroll: fun arg count differs from data length")
  if(!all(unlist(lapply(data,class))=="matrix"))stop("multiroll: only matrix")
  if( length(unique(unlist(lapply(data,nrow))))!=1 )stop("multiroll: all matrices must have same rows")
  p<-nrow(data[[1]])
  ndx<-t(matrix(c(0,w-1),nrow=2,ncol=p-w+1)+t(matrix(1:(p-w+1),ncol=2,nrow=p-w+1)))
  res<-t(simplify2array(apply(ndx,1,function(r){
    window_res<-do.call(fun,c(lapply(data,function(m)m[r[1]:r[2],,drop=FALSE]),MoreArgs))
    if(class(verbose)=="function")cat(r[2]," ",verbose(window_res),"\n")
    window_res
  })))
  if(class(res)=="matrix")rownames(res)<-rownames(data[[1]])[ndx[,2]] else names(res)<-rownames(data[[1]])[ndx[,2]]
  res
}

# apply function to matrix, matrix result
row_apply<-function(m,f,...){
  if(class(m)!="matrix")return(NULL)
  if(!any(class(f)%in%c("function","standardGeneric")))return(NULL)
  mcol<-ncol(m)
  res<-t(apply(m,1,function(r){
    row_res<-drop(unlist(f(r,...)))
    if(length(row_res)==1)return(rep(row_res,mcol))
    if(length(row_res)==mcol)return(row_res)
    return(rep(0,length(r)))
  }))
  dimnames(res)<-dimnames(m)
  res
}

# apply function to matrix, matrix result
col_apply<-function(m,f,...){
  if(class(m)!="matrix")return(NULL)
  if(!any(class(f)%in%c("function","standardGeneric")))return(NULL)
  mrow<-nrow(m)
  res<-apply(m,2,function(r){
    row_res<-drop(unlist(f(r,...)))
    if(length(row_res)==1)return(rep(row_res,mrow))
    if(length(row_res)==mrow)return(row_res)
    return(rep(0,length(r)))
  })
  dimnames(res)<-dimnames(m)
  res
}

"%-%"<-row_apply
"%|%"<-col_apply

################################################################################################################
################################################################################################################

# data is a daysXsecurities matrix of dayly returns.
# the %|% operator applies a function column-wise
# moving_average(window) returns a function that does it.
rolling_variance<-function(data,window){
  average_square <- (data*data)%|%moving_average(window)
  average_move <- data%|%moving_average(window)
  average_square - average_move*average_move
}

# Deals with 0s and NAs:
rolling_variance_nonzero<-function(data,window){
  data2 = (data*data)
  average_non_zero <- (data!=0)%|%moving_sum(window)
  average_square <- data2%|%moving_sum(window)
  average_move <- data%|%moving_sum(window)
  average_square = ifelse(average_non_zero, average_square/average_non_zero, 0)
  average_move = ifelse(average_non_zero, average_move/average_non_zero, 0)
  average_square - average_move*average_move
}

rolling_covariance<-function(security,hedge,window){
  comovement <- (security*hedge)%|%moving_average(window)
  product_of_means <- (security%|%moving_average(window))*(hedge%|%moving_average(window))
  comovement-product_of_means
}

rolling_correlation<-function(security1,security2,window){
  covariance<-rolling_covariance(security1,security2,window)
  variance1<-rolling_variance(security1,window)
  variance2<-rolling_variance(security2,window)
  ifelse(variance1>0&variance2>0,covariance/sqrt(variance1*variance2),0)
}

rolling_beta<-function(security,hedge,window){
  variance<-rolling_variance(hedge,window)
  covariance<-rolling_covariance(security,hedge,window)
  ifelse(variance>0,covariance/variance,0)
}

alpha_ff <- function(ri,RiskFactors,Risk_Factors_Equation) {
  data  = RiskFactors
  data$ri = ri
  form  = as.formula(Risk_Factors_Equation)
  model = lm(form,data=data)
  alpha = summary(model)$coefficients[1]
}

################################################################################################################
################################################################################################################
# DATASET CREATION RELATED FUNCTIONS
################################################################################################################
################################################################################################################


get_cross_section_score <- function(therawdata, therawdata_used=NULL){
  datacol = ncol(therawdata)
  data_used = therawdata
  if (!is.null(therawdata_used))
    data_used = cbind(therawdata,therawdata_used)
  tmp = t(apply(data_used,1,function(r){
    r_scored = scrub(r[1:datacol]) # Note: we don't score NAs and 0s
    r_ecdf = r_scored
    if (!is.null(therawdata_used))
      r_ecdf = scrub(r[(datacol+1):length(r)]) # Note: we don't use NAs and 0s
    if (sum(r_ecdf !=0)){ 
      score_fun = ecdf(r_ecdf[r_ecdf!=0])
      res = ifelse(r_scored!=0, score_fun(r_scored),NA)
    } else {
      res = r_scored*NA
    }
    res
  }))
  rownames(tmp) <- rownames(therawdata)
  tmp
}


################################################################################################################
################################################################################################################
# Simple analyses
################################################################################################################
################################################################################################################


# Returns the alpha, based on a running regression
# Riskfactors could be any (date x factor) vector or data frame and will be used in an lm:
#   lm("return ~ alpha + beta x factors")
# You can also specify a special "RF" factor in the df which will automatically go in the left-hand side:
#   lm("(return - RF) ~ factors)s
alpha_lm <- function(ri,Riskfactors,hedge_days, trade = 0) {
  #data check
  if (class(Riskfactors) != "data.frame") {
    dates <- names(Riskfactors)
    Riskfactors = data.frame(market=Riskfactors)
    rownames(Riskfactors) <- dates
  }
  
  #create the right formula
  RFfield    <- which(colnames(Riskfactors) == "RF")
  NotRFfield <- colnames(Riskfactors)[setdiff(1:ncol(Riskfactors),RFfield)]
  if(length(RFfield) == 1) {
    form <- as.formula(paste("(rj - RF) ~ ",paste(NotRFfield,collapse = "+")))
  } else {
    form <- as.formula(paste("rj ~ ",paste(NotRFfield,collapse = "+")))
  }
  #do a running coefficient estimation
  runcoeff <- function(rj) {
    data    = Riskfactors[rownames(Riskfactors) >= min(names(rj)) & rownames(Riskfactors) <= max(names(rj)),,drop=F]
    data$rj = rj
    model = fastLm(form,data=data)
    return(summary(model)$coefficients[,1])
  }
  coeff <- running(ri,fun=runcoeff,width=hedge_days,allow.fewer = T)
  coeffcorrection <- shift(t(coeff[NotRFfield,]),trade) * Riskfactors[,NotRFfield]
  if(dim(coeffcorrection)[1] !=1)
    coeffcorrection <- apply(coeffcorrection,1,sum)
  alpha <- (ri - coeffcorrection)
  if(length(RFfield) == 1)
    alpha <- alpha - Riskfactors[,RFfield]
  alpha <- alpha * (coeffcorrection!=0)*(ri!=0)
  return(alpha)
}

beta_expost <- function(ri,Riskfactors) {
  #data check
  if (class(Riskfactors) != "data.frame") {
    dates <- names(Riskfactors)
    Riskfactors = data.frame(market=Riskfactors)
    rownames(Riskfactors) <- dates
  }
  
  useonly = intersect(names(ri), rownames(Riskfactors))
  Riskfactors = Riskfactors[useonly,]
  ri = ri[useonly]
  
  #create the right formula
  RFfield    <- which(colnames(Riskfactors) == "RF")
  NotRFfield <- colnames(Riskfactors)[setdiff(1:ncol(Riskfactors),RFfield)]
  if(length(RFfield) == 1) {
    form <- as.formula(paste("(ri - RF) ~ ",paste(NotRFfield,collapse = "+")))
  } else {
    form <- as.formula(paste("ri ~ ",paste(NotRFfield,collapse = "+")))
  }
  
  data    = Riskfactors
  data$ri = ri
  model = fastLm(form,data=data)
  return(summary(model)$coefficients)
}

################################################################################################################
################################################################################################################
# Econometric methods
################################################################################################################
################################################################################################################


############################
#Builds a car table like PV2009. Returns need to be monthly, same for the risk factors
############################
car_table <- function(returns,Event.Date,Risk_Factors_Monthly,min_window = -6, max_window = 48,formula_used="(ri - RF) ~ Delta + SMB + HML + RMW + CMA",value.weights = 1) {
  #data check
  if (class(Risk_Factors_Monthly) != "data.frame") {
    dates <- names(Risk_Factors_Monthly)
    Risk_Factors_Monthly = data.frame(market=Risk_Factors_Monthly)
    rownames(Risk_Factors_Monthly) <- dates
  }
  # Make all data have monthly dates on first of month:
  rownames(returns) <- paste(str_sub(rownames(Risk_Factors_Monthly), start=1,end=7),"01",sep="-")
  rownames(Risk_Factors_Monthly) <- paste(str_sub(rownames(Risk_Factors_Monthly), start=1,end=7),"01",sep="-")
  
  ###
  factors_used = setdiff(unlist(str_split(gsub("~", ",", gsub("\\-", ",", gsub("\\+", ",", gsub("\\)", "",gsub("\\(", "",formula_used))))), " , ")),"ri")
  min_window = min(1,min_window)
  max_window = max(-1,max_window)
  allmonths = min_window:max_window
  if (sum(!(factors_used %in% colnames(Risk_Factors_Monthly))))
    stop(paste("car_table misses the risk factors: ",factors_used[!(factors_used %in% colnames(Risk_Factors_Monthly))]))
  
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
  Row.Date_number = as.numeric(Row.Date)
  Event.Date_number = as.numeric(as.Date(Event.Date))
  firstday = as.numeric(as.Date(head(rownames(returns),1)))
  lastday = as.numeric(as.Date(tail(rownames(returns),1)))
  
  firstHit = match(Event.Date_number,Row.Date_number)
  form  = as.formula(formula_used)
  Risk_Factors_Monthly = Risk_Factors_Monthly[,factors_used]
  if (!("RF" %in% factors_used)){
    Risk_Factors_Monthly = cbind(Risk_Factors_Monthly,matrix(0,nrow=nrow(Risk_Factors_Monthly)))
    colnames(Risk_Factors_Monthly)[ncol(Risk_Factors_Monthly)] <- "RF"
  }
  
  #Build event_month x company x factor matrix
  EVENT_ALIGNED <- array(0, c(length(allmonths), ncol(returns), ncol(Risk_Factors_Monthly)+1)) 
  starting      = pmax(1,pmin(nrow(returns),firstHit + min_window))
  ending        = pmax(1,pmin(nrow(returns),firstHit + max_window))
  # THIS IS THE SLOW PART
  for(ev in 1:length(Event.Date)) {
    EVENT_ALIGNED[(1 + (starting[ev] - (firstHit[ev] + min_window))):(length(allmonths) - (firstHit[ev] + max_window-ending[ev])),ev,] <- as.matrix(cbind(Risk_Factors_Monthly[starting[ev]:ending[ev],], returns[starting[ev]:ending[ev],ev]))
  }
  dimnames(EVENT_ALIGNED) <- list( ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),
                                   c(paste(colnames(returns),Event.Date_number)),
                                   c(colnames(Risk_Factors_Monthly),"ri"))
  
  #Step2: now for each month, calculate the CAR. need to include month 0 and set that one to 0  
  alphas <- rep(0,length(allmonths))
  betas <- array(0,c(length(allmonths),ncol(Risk_Factors_Monthly)-1))
  betasstderr <- array(0,c(length(allmonths),ncol(Risk_Factors_Monthly)-1))
  stderr <- rep(0,length(allmonths))
  dfs <- as.integer(rep(0,length(allmonths)))
  event_alphas = matrix(0,nrow = length(Event.Date_number), ncol = length(allmonths))
  
  match_ini =  match(Event.Date_number, Row.Date_number)
  for (i in 1:length(allmonths)) {
    if (allmonths[i] !=0) { #we do not consider the month of the event
      #tmpdate = AddMonths(Event.Date,allmonths[i])    
      #ret <- EVENT_ALIGNED[i,(tmpdate <= tail(rownames(returns),1)) & (tmpdate >= head(rownames(returns),1)),]
      hitnow = match_ini + allmonths[i]
      ret <- EVENT_ALIGNED[i,,]
      non_zeros = which(ret[,"ri"]!=0 & (hitnow <= length(Row.Date_number)) & (hitnow >= 1))
      ret <- ret[non_zeros,]  # WE NEED THIS HERE!!!!!
      #ret <- ret[ret[,"ri"] <1,]  # WE NEED THIS HERE!!!!!
      if (nrow(ret) > ncol(ret)){
        model = fastLm(form,data=data.frame(ret,row.names = NULL))
        alphas[i] = summary(model)$coefficients[1,"Estimate"] 
        the_betas = summary(model)$coefficients[2:nrow(summary(model)$coefficients), "Estimate"]
        betas[i,] <- the_betas
        betasstderr[i,] <- summary(model)$coefficients[2:nrow(summary(model)$coefficients), "StdErr"]    
        stderr[i] = coef(summary(model))[1, "StdErr"]
        dfs[i] = df.residual(model)
        event_alphas[non_zeros,i] <- ret[,"ri"] - ret[,"RF"] - ret[,names(the_betas)]%*%matrix(the_betas,ncol=1)
      } else{
        alphas[i] = 0
        betas[i,] <- 0
        betasstderr[i,] <- 0  
        stderr[i] = 0
        dfs[i] = 0
        event_alphas[non_zeros,i] <- 0
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
  results[nrow(results),] <- rep(length(Event.Date_number),ncol(results)) 
  colnames(betas) <- factors_used_noRF
  colnames(betasstderr) <- factors_used_noRF
  all_results = list(results = results, betas = betas, betasstderr = betasstderr,event_alphas = event_alphas)
  return(all_results)
}


############################
#Builds a calendar table like PV2009. Returns need to be monthly, same for the risk factors
############################
calendar_table <- function(returns,Event.Date, Risk_Factors_Monthly,min_window = -6, max_window = 48,formula_used="(ri - RF) ~ Delta + SMB + HML + RMW + CMA",value.weights = 1) {
  #data check
  if (class(Risk_Factors_Monthly) != "data.frame") {
    dates <- names(Risk_Factors_Monthly)
    Risk_Factors_Monthly = data.frame(market=Risk_Factors_Monthly)
    rownames(Risk_Factors_Monthly) <- dates
  }
  # Make all data have monthly dates on first of month:
  rownames(returns) <- paste(str_sub(rownames(Risk_Factors_Monthly), start=1,end=7),"01",sep="-")
  rownames(Risk_Factors_Monthly) <- paste(str_sub(rownames(Risk_Factors_Monthly), start=1,end=7),"01",sep="-")
  
  ### 
  factors_used = setdiff(unlist(str_split(gsub("~", ",", gsub("\\-", ",", gsub("\\+", ",", gsub("\\)", "",gsub("\\(", "",formula_used))))), " , ")),"ri")
  factors_used_noRF = setdiff(factors_used, "RF")
  min_window = min(1,min_window)
  max_window = max(-1,max_window)
  allmonths = min_window:max_window
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
  max_index = length(Row.Date_number)
  start_match = match(Event.Date_number,Row.Date_number)
  
  for (i in 1:length(allmonths)) {
    w = allmonths[i]
    hitnow = Row.Date_number[pmax(1,pmin(start_match+w,max_index))]
    ret <- returns
    if (w > 0) ret <- calendar_table_helper1(ret, Row.Date_number, Event.Date_number, hitnow)
    #for(j in 1:length(Event.Date)) 
    #   ret[ Row.Date_number <= Event.Date_number[j] | Row.Date_number > hitnow[j],j ] <- 0
    if (w < 0) ret <- calendar_table_helper2(ret, Row.Date_number, Event.Date_number, hitnow)
    # for(j in 1:length(Event.Date)) 
    #   ret[ Row.Date_number >= Event.Date_number[j] | Row.Date_number < hitnow[j],j ] <- 0
    
    ri <- row_weights(ret, value.weights)
    
    if (sum(ri!=0) > 10) { ### For special cases...
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
  }
  
  rownames(results) <- c(ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths),"Observations")
  colnames(results) <- c("CAL","t-stat","p-value")
  results[nrow(results),] <- rep(length(Event.Date),ncol(results)) 
  colnames(betas) <- factors_used_noRF
  colnames(betasstderr) <- factors_used_noRF
  all_results = list(results = results, betas = betas, betasstderr = betasstderr)
  return(all_results)
}

############################
#Builds a stock specific regression like Brennan, Chordia and Subrahmanyam (1998)
############################
# Split in two parts as the first one is more generic 

# PART I OF BSC1998_event_study
event_study_returns_estimates <- function(returns,Event.Date,company_features, Risk_Factors_Monthly,formula_used="(ri - RF) ~ Delta + SMB + HML + RMW + CMA", rolling_window=60, min_window = -6, max_window = 48, min_data = 30){
  # assumes returns has one column per event, so number of columns equal to Event.Date. Rows are months
  # Keep track also of the risk and stock returns the next month - where we will do the predictions
  factors_used = setdiff(unlist(str_split(gsub("~", ",", gsub("\\-", ",", gsub("\\+", ",", gsub("\\)", "",gsub("\\(", "",formula_used))))), " , ")),"ri")
  if (sum(!(factors_used %in% colnames(Risk_Factors_Monthly))))
    stop(paste("BSC1998_event_study misses the risk factors: ",factors_used[!(factors_used %in% colnames(Risk_Factors_Monthly))]))
  
  factors_used_noRF = setdiff(factors_used, "RF")
  number_of_factors = length(factors_used_noRF)
  form  = as.formula(formula_used)
  Risk_Factors_Monthly = Risk_Factors_Monthly[,factors_used]
  if (!("RF" %in% factors_used)){
    Risk_Factors_Monthly = cbind(Risk_Factors_Monthly,matrix(0,nrow=nrow(Risk_Factors_Monthly)))
    colnames(Risk_Factors_Monthly)[ncol(Risk_Factors_Monthly)] <- "RF"
  }
  
  Event.Date_month = str_sub(Event.Date,start=1,end=7)
  returns_month = str_sub(rownames(returns), start=1,end=7)
  
  ## Step 1: Estimate Factor Loadings, starting min_window months before the event announcement (so month 0 is the event month)
  lapply(1:length(Event.Date), function(i){
    if (i%%50 == 1) cat(i,",")
    stock_returns = structure(returns[,i], .Names = returns_month)
    event_row = which(returns_month == Event.Date_month[1])
    t(Reduce(cbind,lapply((min_window-1):(max_window-1), function(themonth){ # Note we use the "-1" here as we will be using the estimates till the previous month. We may want to use -2 here to be 100% safe. to check.
      #cat(themonth,",")
      res = structure(rep(NA,number_of_factors + (number_of_factors+1) + 1), .Names = c(factors_used_noRF, paste(factors_used,"ret", sep="_"), "actual_ret"))
      period_used = min(length(returns_month), max(1,event_row+themonth - rolling_window)):min(length(returns_month), max(1,event_row+themonth -1))
      period_to_predict = min(length(returns_month), max(1,event_row+themonth))
      if (length(period_used) > min_data & tail(period_used,1) < period_to_predict){ # in case we are at the last month available for this stocks
        ret = cbind(Risk_Factors_Monthly,scrub(returns[,i]))[period_used,] # Ignore NAs and 0s
        colnames(ret)[ncol(ret)]<- "ri"
        if (sum(ret[,"ri"]!=0) >= min_data){
          ret <- ret[ret[,"ri"]!=0,] 
          model = fastLm(form,data=data.frame(ret,row.names = NULL))
          thebetas = summary(model)$coefficients[2:nrow(summary(model)$coefficients), "Estimate"]
          res = c(thebetas,as.numeric(Risk_Factors_Monthly[period_to_predict,]), returns[period_to_predict,i])
          names(res) <- c(factors_used_noRF, paste(factors_used,"ret", sep="_"), "actual_ret")
        }
      }
      res
    })))
  })
  
}
# PART II OF BSC1998_event_study
BSC1998_event_study_coeffs <- function(factor_loadings,returns,Event.Date,company_features, Risk_Factors_Monthly,formula_used="(ri - RF) ~ Delta + SMB + HML + RMW + CMA", rolling_window=60, min_window = -6, max_window = 48, min_data = 30){
  # assumes returns has one column per event, so number of columns equal to Event.Date. Rows are months
  # Assumes factor_loadings comes from BSC1998_event_study_returns_estimates using the exact same inputs!
  
  # Keep track also of the risk and stock returns the next month - where we will do the predictions
  factors_used = setdiff(unlist(str_split(gsub("~", ",", gsub("\\-", ",", gsub("\\+", ",", gsub("\\)", "",gsub("\\(", "",formula_used))))), " , ")),"ri")
  if (sum(!(factors_used %in% colnames(Risk_Factors_Monthly))))
    stop(paste("BSC1998_event_study misses the risk factors: ",factors_used[!(factors_used %in% colnames(Risk_Factors_Monthly))]))
  
  factors_used_noRF = setdiff(factors_used, "RF")
  number_of_factors = length(factors_used_noRF)
  form  = as.formula(formula_used)
  Risk_Factors_Monthly = Risk_Factors_Monthly[,factors_used]
  if (!("RF" %in% factors_used)){
    Risk_Factors_Monthly = cbind(Risk_Factors_Monthly,matrix(0,nrow=nrow(Risk_Factors_Monthly)))
    colnames(Risk_Factors_Monthly)[ncol(Risk_Factors_Monthly)] <- "RF"
  }
  
  Event.Date_month = str_sub(Event.Date,start=1,end=7)
  returns_month = str_sub(rownames(returns), start=1,end=7)
  
  ## Step 1: Estimate Factor Loadings, starting min_window months before the event announcement (so month 0 is the event month)
  # Assumes these are inputs!
  
  ## Step 2: Calculate Monthly Estimated Risk-adjusted Return 
  months_used = min_window:max_window
  Estimated_returns = Reduce(rbind,lapply(1:length(factor_loadings), function(thestock){
    this_stock_data = factor_loadings[[thestock]]
    apply(this_stock_data, 1, function(r)
      ifelse(!is.na(sum(r)), r["actual_ret"] - r["RF_ret"] - sum(r[factors_used_noRF]*r[paste(factors_used_noRF,"ret", sep="_")]), NA)
    )
  }))
  
  ## Step 3: Run Cross-Section Regression in Each Post-Event Month, from 1-48 months
  C_mt_coefficients = Reduce(cbind,lapply(1:ncol(Estimated_returns), function(month){
    month_returns = Estimated_returns[,month]
    useonly = which(!is.na(month_returns))
    res = rep(NA, ncol(company_features)+1)
    if (length(useonly) > ncol(company_features) + 1){
      cross_sectional_data = cbind(company_features[useonly,], month_returns[useonly])
      colnames(cross_sectional_data) <- c(paste("Ind", 1:ncol(company_features), sep=""), "ret")
      cross_setional_form = as.formula(paste("ret", str_c(paste("Ind", 1:ncol(company_features), sep=""), collapse=" + "), sep=" ~ "))
      model = fastLm(cross_setional_form,data=data.frame(cross_sectional_data,row.names = NULL))
      res = summary(model)$coefficients[,1]
    }
    res
  }))
  
  colnames(C_mt_coefficients) <- months_used
  if (!is.null(colnames(company_features)))
    rownames(C_mt_coefficients) <- c("alpha_intercept", colnames(company_features))
  C_mt_coefficients
  ## Step 4: Aggregate  C_mt_coefficients over 48 Post-Event Months: Time-Series Average of C_mt_coefficients (This can be done outside, for whatever months one needs)
}



################################################################################################################
#####################################################################################
### ADDITIONAL KEY CODE FROM BUYBACKS/ISSUERS PROJECT
#####################################################################################
################################################################################################################

#Normal performance Function
Performance <- function(date_to_start, date_to_end, Dates, returns_used, event){
  Start_Date <- Dates[which(rownames(Dates) == date_to_start),]
  End_Date <- Dates[which(rownames(Dates) == date_to_end),]
  
  if (event)
    res = Reduce(cbind,lapply(1:length(Start_Date), function(i) 252*non_zero_mean(returns_used[rownames(returns_used) >= Start_Date[i] & rownames(returns_used) <= End_Date[i],i])))
  if (!event)
    res = Reduce(cbind,lapply(1:length(End_Date), function(i) 252*non_zero_mean(returns_used[(rownames(returns_used) > End_Date[i] | rownames(returns_used) < Start_Date[i]),i])))
  res
}

#PNL Matrix function (for any subset of companies to use)
PNL_matrix_BB <- function(date_to_start, date_to_end, company_subset, Dates, returns_used, event=1){
  returns_used <- returns_used[,company_subset, drop=F]
  Start_Date <- Dates[which(rownames(Dates) == date_to_start),company_subset]
  End_Date <- Dates[which(rownames(Dates) == date_to_end),company_subset]
  
  dates_used = rownames(returns_used)
  if (event){
    dates_matrix= mapply(function(s,e){
      dates_used >= s & dates_used <= e
    },Start_Date, End_Date)
    res = returns_used*dates_matrix  
  }
  if (!event){
    dates_matrix = Reduce(cbind,lapply(1:length(Start_Date), function(i) (dates_used > End_Date[i]))) 
    res = returns_used*dates_matrix  
  }
  res
}


Betas <- function(x,y,Dates, EVENT_RETURNS,Riskfactors){
  if (class(Riskfactors) != "matrix")
    Riskfactors = matrix(Riskfactors,ncol=1)
  Start_Date <- Dates[which(rownames(Dates) == x),]
  End_Date <- Dates[which(rownames(Dates) == y),]
  non_zeros <- which(apply(Riskfactors!=0,1,sum)!=0) 
  Riskfactors <- Riskfactors[non_zeros,,drop=F]
  tmp_matrix <- EVENT_RETURNS[non_zeros,]
  res = Reduce(cbind,lapply(1:length(Start_Date), function(i){
    thebetas = rep(0,ncol(Riskfactors))
    returns_tmp = tmp_matrix[rownames(tmp_matrix) >= Start_Date[i] & rownames(tmp_matrix) <= End_Date[i],i]
    RiskF_tmp = Riskfactors[which(rownames(tmp_matrix) >= Start_Date[i] & rownames(tmp_matrix) <= End_Date[i]),,drop=F]
    individualVariance = apply(RiskF_tmp,2,sd)
    useonly_nonzero_factors = which(individualVariance !=0 )
    if (length(useonly_nonzero_factors) > 0){
      RiskF_tmp_nonzero = RiskF_tmp[,useonly_nonzero_factors,drop=F]
      # REPLACE THESE THREE LINES WITH R'S OLS... to be sure/robust...
      thecovariance = cov(returns_tmp, RiskF_tmp_nonzero)
      thevariance = var(RiskF_tmp_nonzero)
      thebetas[useonly_nonzero_factors] <- solve(thevariance + 0*diag(ncol(thevariance)))%*%matrix(thecovariance,ncol=1) # maybe add a small diagonal for stability?
    }
    thebetas
  }))
  rownames(res)<- colnames(Riskfactors)
  res
}

Betas_lm <- function(x,y,Dates, EVENT_RETURNS,Risk_Factors){
  #data check
  if (class(Risk_Factors) != "data.frame") {
    dates <- names(Risk_Factors)
    Risk_Factors = data.frame(market=Risk_Factors)
    rownames(Risk_Factors) <- dates
  }
  
  if (sum(sapply(c("Delta" ,"SMB",   "HML",   "RMW",  "CMA",   "RF"), function(i) !(i %in% colnames(Risk_Factors))))!=0)
    stop("Beta_lm function has wrong risk factors")
  if (sum(sapply(colnames(Risk_Factors), function(i) !(i %in% c("Delta" ,"SMB",   "HML",   "RMW",  "CMA",   "RF"))))!=0)
    stop("Beta_lm function has wrong risk factors")
  
  #create the right formula for lm
  RFfield    <- which(colnames(Risk_Factors) == "RF")
  NotRFfield <- colnames(Risk_Factors)[setdiff(1:ncol(Risk_Factors),RFfield)]
  if(length(RFfield) == 1) {
    form <- as.formula(paste("(rj - RF) ~ ",paste(NotRFfield,collapse = "+")))
  } else {
    form <- as.formula(paste("rj ~ ",paste(NotRFfield,collapse = "+")))
  }
  
  #Step 1: create the return data
  Start_Date <- Dates[which(rownames(Dates) == x),]
  End_Date <- Dates[which(rownames(Dates) == y),]
  non_zeros <- which(apply(Risk_Factors!=0,1,sum)!=0) 
  Risk_Factors <- Risk_Factors[non_zeros,,drop=F]
  tmp_matrix <- EVENT_RETURNS[non_zeros,]
  
  #Step 2: estimation of betas uses the following function
  betafunc <- function(i){
    stats   = rep(0,length(NotRFfield)+3)
    rj      = tmp_matrix[rownames(tmp_matrix) >= Start_Date[i] & rownames(tmp_matrix) <= End_Date[i],i]
    data    = Risk_Factors[rownames(Risk_Factors) >= Start_Date[i] & rownames(Risk_Factors) <= End_Date[i],,drop=F]
    data$rj = rj
    
    model   = fastLm(form,data=data)
    coeff   = summary(model)$coefficients[,1]
    stats[1:length(coeff)]  = coeff                     #intercept, betas
    stats[length(coeff)+1] = ifelse(length(!is.na(model$residuals)) > 2, sd(model$residual[!is.na(model$residuals)]), 0)
    stats[length(stats)]    = summary(model)$r.squared  #R^2
    
    return(stats)
  }
  
  #Step 3: apply the function and format the data nicely
  res = Reduce(cbind,lapply(1:length(Start_Date), betafunc))
  rownames(res)<- c("alpha",NotRFfield,"IVOL","Rsq")
  res
}

VOL_lm <- function(x,y,Dates, EVENT_RETURNS){
  Start_Date <- Dates[which(rownames(Dates) == x),]
  End_Date <- Dates[which(rownames(Dates) == y),]
  volfunc <- function(i) {
    tmp = EVENT_RETURNS[rownames(EVENT_RETURNS) >= Start_Date[i] & rownames(EVENT_RETURNS) <= End_Date[i],i]
    ifelse(sum(tmp!=0) > 3, sd(tmp[tmp!=0]),0) 
  }
  sapply(1:length(Start_Date), function(i) volfunc(i))
}

RET_lm <- function(x,y,Dates, EVENT_RETURNS){
  Start_Date <- Dates[which(rownames(Dates) == x),]
  End_Date <- Dates[which(rownames(Dates) == y),]
  sapply(1:length(Start_Date), function(i) sum(EVENT_RETURNS[rownames(EVENT_RETURNS) >= Start_Date[i] & rownames(EVENT_RETURNS) <= End_Date[i],i]))
}

create_dates <- function(Event_Date) {
  Trading.Day = Event_Date + trading_day_after_announce
  One.Day.Before <- Event_Date - 1
  Five.Day.Before <- Event_Date - 5
  One.Month.Before <- One.Day.Before - 30
  Three.Month.Before <- One.Day.Before - 90
  Six.Month.Before <- One.Day.Before - 180
  One.Year.Before <- One.Day.Before - 360
  Two.Years.Before <- One.Day.Before - 2*360
  One.Month.After <- 30 + Trading.Day 
  Three.Month.After <- 90 + Trading.Day 
  Six.Month.After <- 180 + Trading.Day
  One.Year.After <- 365 + Trading.Day
  Two.Years.After <- 2*365 + Trading.Day
  Three.Years.After <- 3*365 + Trading.Day
  Four.Years.After <- 4*365 + Trading.Day
  Dates <- data.frame(Event_Date,One.Day.Before,Five.Day.Before,One.Month.Before,Three.Month.Before,Six.Month.Before,One.Year.Before,Two.Years.Before,Trading.Day,One.Month.After,Three.Month.After,Six.Month.After,One.Year.After,Two.Years.After,Three.Years.After,Four.Years.After)
  colnames(Dates) <- c("Event.Date","One.Day.Before","Five.Day.Before","One.Month.Before","Three.Month.Before","Six.Month.Before","One.Year.Before","Two.Years.Before","Trading.Day","One.Month.After","Three.Month.After","Six.Month.After","One.Year.After","Two.Years.After","Three.Years.After","Four.Years.After")
  Dates <- t(Dates)
  return(Dates)
}

create_dates_month <- function(Event_Date, allmonths) {
  last_available = tail(allmonths,1)
  allmonths = sort(allmonths) # just in case
  allmonths = str_sub(allmonths, start=1,end=7)
  tmp = match(str_sub(Event_Date, start = 1, end = 7), str_sub(allmonths, start=1,end=7))
  
  Trading.Day = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1, length(allmonths))], "01",sep="-"), last_available)
  One.Month.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+1, length(allmonths))], "01",sep="-"), last_available)
  Three.Month.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+3, length(allmonths))], "01",sep="-"), last_available)
  Six.Month.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+6, length(allmonths))], "01",sep="-"), last_available)
  One.Year.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+12, length(allmonths))], "01",sep="-"), last_available)
  Two.Years.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+24, length(allmonths))], "01",sep="-"), last_available)
  Three.Years.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+36, length(allmonths))], "01",sep="-"), last_available)
  Four.Years.After = ifelse(!is.na(tmp), paste(allmonths[pmin(tmp+1+48, length(allmonths))], "01",sep="-"), last_available)
  Dates <- data.frame(Trading.Day,One.Month.After,Three.Month.After,Six.Month.After,One.Year.After,Two.Years.After,Three.Years.After,Four.Years.After)
  colnames(Dates) <- c("Trading.Day","One.Month.After","Three.Month.After","Six.Month.After","One.Year.After","Two.Years.After","Three.Years.After","Four.Years.After")
  Dates <- t(Dates)
  return(Dates)
}

is.crisis <- function(thedate, crisis_years, slack_pre=0, slack_post = 0)
  sum(sapply(1:length(crisis_years), function(i) thedate >= AddMonths(crisis_years[[i]][1], - slack_pre*12) & thedate <= AddMonths(crisis_years[[i]][2], slack_post*12)))!=0

plot_crisis_dates <- function(all_ret_values, monthly = 1){
  if (class(all_ret_values) == "matrix" | class(all_ret_values) == "data.frame"){
    thenames = rownames(all_ret_values) 
  } else {
    thenames = names(all_ret_values)
  }  
  abline(h=0)
  for (i in 1:length(BEAR_YEARS)){
    if (!monthly){
      abline(v=which.min(abs(as.Date(thenames) - BEAR_YEARS[[i]][1])),lwd=3, col= "black", lty="solid")
      abline(v=which.min(abs(as.Date(thenames) - BEAR_YEARS[[i]][2])),lwd=3, col= "red", lty="solid")    
    } else {
      abline(v=which.min(abs(as.numeric(format(as.Date(thenames), "%Y%m")) - as.numeric(format(as.Date(BEAR_YEARS[[i]][1]), "%Y%m")))),lwd=3, col= "black", lty="solid")
      abline(v=which.min(abs(as.numeric(format(as.Date(thenames), "%Y%m")) - as.numeric(format(as.Date(BEAR_YEARS[[i]][2]), "%Y%m")))),lwd=3, col= "red", lty="solid")
    }
  }
  #for (i in 1:length(RECESSION_YEARS)){
  #  abline(v=which.min(abs(as.Date(thenames) - RECESSION_YEARS[[i]][1])),lwd=1, col= "black", lty="dotted")
  #  abline(v=which.min(abs(as.Date(thenames) - RECESSION_YEARS[[i]][2])),lwd=1, col= "red", lty="dotted")    
  #}
}

exit_helper_rnw <- function(event,exit_signal_name,holding_period_pnl = "Four.Years.After",pnl_hedge_factors_used=pnl_hedge_factors){
  exit_date = event$DATASET$SDC[[which(names(event$DATASET$SDC) == exit_signal_name)]]
  event_returns_monthly_exit = event$DATASET$returns_by_event_monthly
  # Set to 0 all returns starting the month after the SEO event: we exit then, hence we don't need these returns
  exit_events = which(exit_date != "2100-01-01" & exit_date != "1900-01-01")
  noexit_events = which(exit_date == "2100-01-01" & exit_date != "1900-01-01")
  all_dates_monthly = as.Date(rownames(event_returns_monthly_exit))
  first_day_next_month = sapply(exit_date,function(i) AddMonths(as.Date(timeFirstDayInMonth(i)),1))
  for (i in exit_events)
    event_returns_monthly_exit[all_dates_monthly >= first_day_next_month[i],i] <- 0  
  number_events_exit = table(format(event$DATASET$SDC$Event.Date[exit_events], "%Y"))
  number_events_noexit = table(format(event$DATASET$SDC$Event.Date[noexit_events], "%Y"))
  years_used = sort(union(names(number_events_exit), names(number_events_noexit)))
  number_events_noexit_align = structure(rep(0,length(years_used)), .Names = years_used)
  number_events_exit_align = structure(rep(0,length(years_used)), .Names = years_used)
  number_events_noexit_align[names(number_events_noexit)] <- number_events_noexit
  number_events_exit_align[names(number_events_exit)] <- number_events_exit
  number_events_exit = number_events_exit_align; rm("number_events_exit_align")
  number_events_noexit = number_events_noexit_align; rm("number_events_noexit_align")
  number_events = ifelse(number_events_noexit + number_events_exit !=0, 100*(number_events_exit/(number_events_noexit+number_events_exit)), 100) 
  pnl_Exit =  apply(PNL_matrix_BB(start_date_event,holding_period_pnl, exit_events,  event$DATASET$DatesMonth, event_returns_monthly_exit,event=1),1,non_zero_mean) 
  pnl_noExit =  apply(PNL_matrix_BB(start_date_event,holding_period_pnl, exit_events,  event$DATASET$DatesMonth, event$DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)   
  pnl_Exit_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(pnl_Exit,Risk_Factors_Monthly[,pnl_hedge_factors_used],hedge_months, trade =1))),min_date=FirstTrade)
  pnl_NoExit_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(pnl_noExit,Risk_Factors_Monthly[,pnl_hedge_factors_used],hedge_months, trade =1))),min_date=FirstTrade)
  
  return(list(exit_events = exit_events, noexit_events = noexit_events,
              pnl_Exit_Hedged = pnl_Exit_Hedged, pnl_NoExit_Hedged = pnl_NoExit_Hedged, 
              number_events = number_events 
  ))
}

get_pnl_results_stock_subset <- function(DATASET,High_feature_events,Low_feature_events,Risk_Factors_Monthly,pnl_hedge_factors){
  High_feature <- apply(PNL_matrix_BB(start_date_event,"One.Year.After", High_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  Low_feature <- apply(PNL_matrix_BB(start_date_event,"One.Year.After", Low_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  High_feature_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_feature,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)
  Low_feature_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_feature,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)  
  High_feature48m <- apply(PNL_matrix_BB(start_date_event,"Four.Years.After", High_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  Low_feature48m <- apply(PNL_matrix_BB(start_date_event,"Four.Years.After", Low_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  High_feature_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_feature48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)
  Low_feature_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_feature48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)
  
  list(
    High_feature_Hedged   = High_feature_Hedged,
    Low_feature_Hedged    = Low_feature_Hedged,
    High_feature48m       = High_feature48m,
    Low_feature48m        = Low_feature48m,
    High_feature_Hedged48m  = High_feature_Hedged48m,
    Low_feature_Hedged48m   = Low_feature_Hedged48m
  ) 
}

get_feature_results <- function(DATASET,feature_name, company_subset_undervalued,company_subset_overvalued,quantile_feature,featurewindow, method="Complex"){
  
  if (method == "Simple"){
    thefeature = DATASET$boardex[[which(names(DATASET$boardex) == feature_name)]]  
    #High_feature_events = which(scrub(thefeature) > 1-quantile_feature & !is.na(thefeature))
    #Low_feature_events = which(scrub(thefeature) < quantile_feature & !is.na(thefeature))
    High_feature_events = which(scrub(thefeature) > quantile(thefeature[!is.na(thefeature)],1-quantile_feature) & !is.na(thefeature))
    Low_feature_events = which(scrub(thefeature) < quantile(thefeature[!is.na(thefeature)],quantile_feature) & !is.na(thefeature))
  } else {
    # Thresholds
    thetimes = DATASET$SDC$Event.Date
    thefeature = DATASET$boardex[[which(names(DATASET$boardex) == feature_name)]]  
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
    
    #################################################################
    # Classes
    ## NEED TO DECIDE THE >= or just > here!
    High_feature_events = which(scrub(thefeature) > feature_up & !is.na(thefeature)  & feature_up != 0 )
    Low_feature_events = which(scrub(thefeature) <= feature_down & !is.na(thefeature))
    #################################################################
  }
  
  #Portfolios
  High_feature <- apply(PNL_matrix_BB(start_date_event,"One.Year.After", High_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  Low_feature <- apply(PNL_matrix_BB(start_date_event,"One.Year.After", Low_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  High_feature_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_feature,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)
  Low_feature_Hedged = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_feature,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)  
  High_feature48m <- apply(PNL_matrix_BB(start_date_event,"Four.Years.After", High_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  Low_feature48m <- apply(PNL_matrix_BB(start_date_event,"Four.Years.After", Low_feature_events,  DATASET$DatesMonth, DATASET$returns_by_event_monthly,event=1),1,non_zero_mean)
  High_feature_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(High_feature48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)
  Low_feature_Hedged48m = remove_initialization_time(suppressWarnings(scrub(alpha_lm(Low_feature48m,Risk_Factors_Monthly[,pnl_hedge_factors],hedge_months,trade=1))),min_date=FirstTrade)
  #IRATS
  feature_IRATStable = round(cbind(
    car_table(DATASET$returns_by_event_monthly[,Low_feature_events], DATASET$SDC$Event.Date[Low_feature_events], Risk_Factors_Monthly)$results,
    car_table(DATASET$returns_by_event_monthly[,High_feature_events], DATASET$SDC$Event.Date[High_feature_events], Risk_Factors_Monthly)$results
  ),3)
  feature_IRATStable_under = round(cbind(
    car_table(DATASET$returns_by_event_monthly[,intersect(Low_feature_events, which(company_subset_undervalued))], DATASET$SDC$Event.Date[intersect(Low_feature_events, which(company_subset_undervalued))], Risk_Factors_Monthly)$results,
    car_table(DATASET$returns_by_event_monthly[,intersect(Low_feature_events, which(company_subset_overvalued))], DATASET$SDC$Event.Date[intersect(Low_feature_events, which(company_subset_overvalued))], Risk_Factors_Monthly)$results,
    car_table(DATASET$returns_by_event_monthly[,intersect(High_feature_events, which(company_subset_undervalued))], DATASET$SDC$Event.Date[intersect(High_feature_events, which(company_subset_undervalued))], Risk_Factors_Monthly)$results,
    car_table(DATASET$returns_by_event_monthly[,intersect(High_feature_events, which(company_subset_overvalued))], DATASET$SDC$Event.Date[intersect(High_feature_events, which(company_subset_overvalued))], Risk_Factors_Monthly)$results
  ),3)
  #calendar
  feature_IRATStable_cal = round(cbind(
    calendar_table(DATASET$returns_by_event_monthly[,Low_feature_events], DATASET$SDC$Event.Date[Low_feature_events], Risk_Factors_Monthly)$results,
    calendar_table(DATASET$returns_by_event_monthly[,High_feature_events], DATASET$SDC$Event.Date[High_feature_events], Risk_Factors_Monthly)$results
  ),3)
  feature_IRATStable_under_cal = round(cbind(
    calendar_table(DATASET$returns_by_event_monthly[,intersect(Low_feature_events, which(company_subset_undervalued))], DATASET$SDC$Event.Date[intersect(Low_feature_events, which(company_subset_undervalued))], Risk_Factors_Monthly)$results,
    calendar_table(DATASET$returns_by_event_monthly[,intersect(Low_feature_events, which(company_subset_overvalued))], DATASET$SDC$Event.Date[intersect(Low_feature_events, which(company_subset_overvalued))], Risk_Factors_Monthly)$results,
    calendar_table(DATASET$returns_by_event_monthly[,intersect(High_feature_events, which(company_subset_undervalued))], DATASET$SDC$Event.Date[intersect(High_feature_events, which(company_subset_undervalued))], Risk_Factors_Monthly)$results,
    calendar_table(DATASET$returns_by_event_monthly[,intersect(High_feature_events, which(company_subset_overvalued))], DATASET$SDC$Event.Date[intersect(High_feature_events, which(company_subset_overvalued))], Risk_Factors_Monthly)$results
  ),3)
  list(
    High_feature_events    = High_feature_events,
    Low_feature_events     = Low_feature_events,
    High_feature          = High_feature,
    Low_feature           = Low_feature,
    High_feature_Hedged   = High_feature_Hedged,
    Low_feature_Hedged    = Low_feature_Hedged,
    High_feature48m       = High_feature48m,
    Low_feature48m        = Low_feature48m,
    High_feature_Hedged48m  = High_feature_Hedged48m,
    Low_feature_Hedged48m   = Low_feature_Hedged48m,
    feature_IRATStable       = feature_IRATStable,
    feature_IRATStable_cal   = feature_IRATStable_cal,
    feature_IRATStable_under =feature_IRATStable_under,
    feature_IRATStable_under_cal = feature_IRATStable_under_cal
  )
}


