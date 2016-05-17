
################################################################
# INPUT FILES
################################################################

# Needs these 2 csv files (from SDC - see README)
file_in_repurchases_dataset  = "rawdata_events_buybacks/sdc2016/sdc_output_buybacks_repurchases.csv"
file_in_MnA_dataset  = "rawdata_events_buybacks/sdc2016/sdc_output_buybacks_ma.csv"

################################################################
# OUTPUT FILE
################################################################

# Passes this file to dataset/buybacks/standardise_data.R
file_out_for_standardize_data  = "rawdata_events_buybacks/sdc2016/sdc_output_buybacks.csv"
#all_buybacks_orig <- read.csv(file_out_for_standardize_data,header=T,stringsAsFactors = F,sep=",")


################################################################

buybacks_repurchases <- read.csv(file_in_repurchases_dataset,header=T,stringsAsFactors = F,sep=";", dec=",")
buybacks_ma <- read.csv(file_in_MnA_dataset,header=T,stringsAsFactors = F,sep=";", dec=",")

# just in case there is an empty column
buybacks_repurchases$X <- NULL
buybacks_ma$X <- NULL

cleanup = list()
cleanup$Standardise = list()

cleanup$Standardise$initial_MA = length(buybacks_ma$Acquiror..CUSIP)
cleanup$Standardise$initial_repurchase = length(buybacks_repurchases$Initial.Auth..Date)

####################################################################################
# First clean up the M&A data.  ALWAYS CHECK HOW MANY EVENTS ARE DROPPED. IF TOO MANY, INVESTIGATE...
####################################################################################

# Remove duplicate events
buybacks_ma$tmp1 <- sapply(1:length(buybacks_ma$X.Deal.Number), function(i) 
  paste(tolower(buybacks_ma$Acquiror..CUSIP[i]),
        buybacks_ma$X..Date.Announced[i],
        collapse = ""))
cleanup_duplicate = duplicated(buybacks_ma$tmp1) | duplicated(buybacks_ma$tmp1, fromLast = TRUE) | duplicated(buybacks_ma$X.Deal.Number) | duplicated(buybacks_ma$X.Deal.Number, fromLast = TRUE)
cleanup$Standardise$MAduplicate = sum(cleanup_duplicate)
buybacks_ma$tmp1 <- NULL
# Remove missing dates events
the_dates_ma = as.Date(buybacks_ma$X..Date.Announced,format="%d/%m/%y")
buybacks_ma$Event.Date <- the_dates_ma
buybacks_ma$X..Date.Announced <- NULL
cleanup_date = is.na(the_dates_ma)
cleanup$Standardise$MAno_date = sum(cleanup_date)
# Remove cases where Acquiror cusip is missing (it may be ok not to have the target cusip)
cleanup_noAcqCusip = is.na(is.na(buybacks_ma$Acquiror..CUSIP))
cleanup$Standardise$MAnoAcqCusip = sum(cleanup_noAcqCusip)
## only keep cases where the acquiror and target cusip are the same
useonly_cusip = ifelse(!is.na(buybacks_ma$Acquiror..CUSIP), buybacks_ma$Acquiror..CUSIP == buybacks_ma$Target.CUSIP, T)
cleanup$Standardise$MAdifference_cusip = sum(!useonly_cusip)
## only keep cases where the acquiror and target cusip have the same ticker, just in case
useonly_ticker = ifelse(!is.na(buybacks_ma$Acquiror.Primary.Ticker.Symbol) & !is.na(buybacks_ma$Target.Primary.Ticker.Symbol),
                 buybacks_ma$Acquiror.Primary.Ticker.Symbol == buybacks_ma$Target.Primary.Ticker.Symbol, T)
cleanup$Standardise$MAdifference_ticker = sum(!useonly_ticker)
## to be sure, also check the industries
useonly_industry = ifelse(!is.na(buybacks_ma$Acquiror.Primary...SIC...Code) & !is.na(buybacks_ma$Target.Primary...SIC..Code),
                 buybacks_ma$Acquiror.Primary...SIC...Code == buybacks_ma$Target.Primary...SIC..Code, T)
cleanup$Standardise$MAdifference_industry = sum(!useonly_industry)
## to be sure, also check the share prices the day before the event
useonly_price = ifelse(!is.na(buybacks_ma$Acq.Closing.Price.1.Day.Prior.to..Ann...) & !is.na(buybacks_ma$Target.Share.Price.1.Day.Prior.to.Announcement....),
                 buybacks_ma$Acq.Closing.Price.1.Day.Prior.to..Ann... == buybacks_ma$Target.Share.Price.1.Day.Prior.to.Announcement...., T)
cleanup$Standardise$MAdifference_closingprice = sum(!useonly_price)
# Only keep the Cash only deals
cashonly = sapply(buybacks_ma$Consid..eration.Offered, function(i){
  all_considerations = unique(unlist(str_split(i,"\n")))
  sum(!(all_considerations %in% c("CASH","CASHONLY")))  == 0
})
cleanup$Standardise$MAnon_cashonly = sum(!cashonly)
# just remove anything that has an "attitude". It should be "Not Appl.". 
NoAttitude_only = buybacks_ma$Attitude != "Not Appl."
cleanup$Standardise$MAattitude = sum(NoAttitude_only)

# NOW remove
to_remove = (cleanup_duplicate | NoAttitude_only | !cashonly | !useonly_price | !useonly_industry | !useonly_ticker | !useonly_cusip | cleanup_noAcqCusip | cleanup_date)
buybacks_ma = buybacks_ma[!to_remove, ]
cleanup$Standardise$MAinitial_removed = sum(to_remove)

# Now clean up a bit the names of the data, following the convention of standardize_data.R
# and also making the names be the same across the two buyback datasets
buybacks_ma$X6.CUSIP <- buybacks_ma$Acquiror..CUSIP
buybacks_ma$Acquiror..CUSIP <- NULL; buybacks_ma$Target.CUSIP <- NULL
buybacks_ma$Ticker.Symbol <- buybacks_ma$Acquiror.Primary.Ticker.Symbol
buybacks_ma$Acquiror.Primary.Ticker.Symbol <- NULL; buybacks_ma$Target.Primary.Ticker.Symbol <- NULL
buybacks_ma$Industry <- buybacks_ma$Acquiror.Primary...SIC...Code
buybacks_ma$Acquiror.Primary...SIC...Code <- NULL; buybacks_ma$Target.Primary...SIC..Code <- NULL
buybacks_ma$Closing.Price <- buybacks_ma$Acq.Closing.Price.1.Day.Prior.to..Ann...
buybacks_ma$Acq.Closing.Price.1.Day.Prior.to..Ann... <- NULL; buybacks_ma$Target.Share.Price.1.Day.Prior.to.Announcement.... <- NULL
buybacks_ma$Consid..eration.Offered <- NULL
buybacks_ma$Attitude <- NULL
buybacks_ma$Event.Size <- scrub(buybacks_ma$X....sought)
buybacks_ma$X....sought <- NULL
buybacks_ma$Market.Cap <- suppressWarnings(scrub(as.numeric(gsub(",",".",buybacks_ma$Firm.Value...mil.))))
buybacks_ma$Firm.Value...mil. <- NULL
buybacks_ma$Stock.Exchange <- buybacks_ma$Acquiror.Stock.Exchange..Code.
buybacks_ma$Acquiror.Stock.Exchange..Code. <- NULL
buybacks_ma$Deal.Number.MA <- buybacks_ma$X.Deal.Number
buybacks_ma$X.Deal.Number <- NULL
buybacks_ma$Purpose.Code <- buybacks_ma$Purpose..Code
buybacks_ma$Purpose..Code <- NULL
buybacks_ma$Event.Size.Dollar <- scrub(buybacks_ma$Shares.Auth...mil.)
buybacks_ma$Shares.Auth...mil. <- NULL

###########################################################################
### clean repurchases now
###########################################################################

# First the cleanup
# Remove duplicate events
# Remove duplicate events
buybacks_repurchases$tmp1 <- sapply(1:length(buybacks_repurchases$X8.Digit.CUSIP), function(i) 
  paste(tolower(str_sub(buybacks_repurchases$X8.Digit.CUSIP[i], 1, 6)), # Need to use the 6-cusip here!! we merge using that later, so we need to avoid later duplicates
        buybacks_repurchases$Initial.Auth..Date[i],
        collapse = ""))
cleanup_duplicate = duplicated(buybacks_repurchases$tmp1) | duplicated(buybacks_repurchases$tmp1, fromLast = TRUE) | duplicated(buybacks_repurchases$Deal.Number) | duplicated(buybacks_repurchases$Deal.Number, fromLast = TRUE)
cleanup$Standardise$Repurchaseduplicate = sum(cleanup_duplicate)
buybacks_repurchases$tmp1 <- NULL
# NA dates
the_dates_repurchases = as.Date(buybacks_repurchases$Initial.Auth..Date,format="%d/%m/%Y")
buybacks_repurchases$Event.Date <- the_dates_repurchases
buybacks_repurchases$Initial.Auth..Date <- NULL
cleanup$Standardise$Repurchaseno_date = sum(is.na(the_dates_repurchases))
#NA CUSIPS
cleanup$Standardise$Repurchaseno_cusip = sum(is.na(buybacks_repurchases$X8.Digit.CUSIP))

to_remove = cleanup_duplicate | is.na(the_dates_repurchases) | is.na(buybacks_repurchases$X8.Digit.CUSIP) 
buybacks_repurchases = buybacks_repurchases[!to_remove,]
cleanup$Standardise$Repurchaseinitial_removed = sum(to_remove)

# Now the renaming
buybacks_repurchases$CUSIP <- buybacks_repurchases$X8.Digit.CUSIP
buybacks_repurchases$X8.Digit.CUSIP <- NULL
buybacks_repurchases$Closing.Price <- scrub(as.numeric(gsub(",",".",buybacks_repurchases$Market.Pr..1.Day.Prior.to.Initial.Auth..Date)))
buybacks_repurchases$Market.Pr..1.Day.Prior.to.Initial.Auth..Date <- NULL
buybacks_repurchases$Market.Cap <-suppressWarnings(scrub(as.numeric(gsub(",",".",buybacks_repurchases$Market.Cap..Init..Auth..Date...mil.))))
buybacks_repurchases$Market.Cap..Init..Auth..Date...mil. <- NULL
buybacks_repurchases$Event.Size <- scrub(as.numeric(gsub(",",".",buybacks_repurchases$Pct.of.Shs.Authorized.At.Initial.Auth..Date)))
buybacks_repurchases$Pct.of.Shs.Authorized.At.Initial.Auth..Date<-NULL
buybacks_repurchases$Industry <- buybacks_repurchases$Primary.SIC.Code
buybacks_repurchases$Primary.SIC.Code <- NULL
buybacks_repurchases$Stock.Exchange <- buybacks_repurchases$Primary.Stock.Exchange.Code
buybacks_repurchases$Primary.Stock.Exchange.Code <- NULL
buybacks_repurchases$Currency <- buybacks_repurchases$Currency...Code
buybacks_repurchases$Currency...Code <- NULL
buybacks_repurchases$Pct..Auth..Shares.Out. <- scrub(buybacks_repurchases$Pct..Auth..Shares.Out.)
buybacks_repurchases$Purpose.Code <- buybacks_repurchases$Pur..pose.Code
buybacks_repurchases$Pur..pose.Code <- NULL
buybacks_repurchases$Deal.Number.Repurchases <- buybacks_repurchases$Deal.Number
buybacks_repurchases$Deal.Number <- NULL

###########################################################################
## Now merge the two datasets 
###########################################################################

# buybacks_repurchases Cusips are 8-digit, while the M&A ones are 6 digit. so use only the 6. Note that acquiror and target cusips are the same by now 
# make all lower case in case there are inconsistencies
buybacks_repurchases$X6.CUSIP.match <- sapply(tolower(buybacks_repurchases$CUSIP), function(i) str_sub(i,1,6))
buybacks_ma$X6.CUSIP.match <- tolower(buybacks_ma$X6.CUSIP)
##########
# First remove common events that disagree in either Industry or Ticker
buybacks_ma$tmp <- sapply(1:length(buybacks_ma$X6.CUSIP.match), function(i) 
  paste(buybacks_ma$X6.CUSIP.match[i],
        buybacks_ma$Event.Date[i],
        buybacks_ma$Industry[i],
        buybacks_ma$Ticker.Symbol[i],                 
       collapse = ""))
buybacks_repurchases$tmp <- sapply(1:length(buybacks_repurchases$X6.CUSIP.match), function(i) 
  paste(buybacks_repurchases$X6.CUSIP.match[i],
        buybacks_repurchases$Event.Date[i],
        buybacks_repurchases$Industry[i],
        buybacks_repurchases$Ticker.Symbol[i],                 
        collapse = ""))
buybacks_ma$tmp1 <- sapply(1:length(buybacks_ma$X6.CUSIP.match), function(i) 
  paste(buybacks_ma$X6.CUSIP.match[i],
        buybacks_ma$Event.Date[i],
        collapse = ""))
buybacks_repurchases$tmp1 <- sapply(1:length(buybacks_repurchases$X6.CUSIP.match), function(i) 
  paste(buybacks_repurchases$X6.CUSIP.match[i],
        buybacks_repurchases$Event.Date[i],
        collapse = ""))
#check
sum(duplicated(buybacks_ma$tmp1)) + sum(duplicated(buybacks_repurchases$tmp1)) 

ma_merge_disagree = 
  (buybacks_ma$tmp1 %in% intersect(buybacks_ma$tmp1, buybacks_repurchases$tmp1)) & 
  !(buybacks_ma$tmp %in% intersect(buybacks_ma$tmp, buybacks_repurchases$tmp)) 
repurchase_merge_disagree = 
  (buybacks_repurchases$tmp1 %in% intersect(buybacks_ma$tmp1, buybacks_repurchases$tmp1)) & 
  !(buybacks_repurchases$tmp %in% intersect(buybacks_ma$tmp, buybacks_repurchases$tmp)) 
buybacks_repurchases$tmp <- buybacks_repurchases$tmp1 <- buybacks_ma$tmp <- buybacks_ma$tmp1 <- NULL
cleanup$Standardise$merge_duplicates = sum(ma_merge_disagree)

buybacks_ma = buybacks_ma[!ma_merge_disagree,]
buybacks_repurchases = buybacks_repurchases[!repurchase_merge_disagree,]

# Now ready to join them, step by step
# Step 1: get the common ones, only
all_buybacks <- merge(buybacks_ma,buybacks_repurchases , by=c("Event.Date","X6.CUSIP.match"))
# before removing "bad deals", find the deals from MA and repurchases that were not joined, 
# so we join them later (and avoid adding back the deals we will clean now)
only_MA_deals = setdiff(buybacks_ma$Deal.Number.MA, all_buybacks$Deal.Number.MA)
only_repurchase_deals = setdiff(buybacks_repurchases$Deal.Number.Repurchases, all_buybacks$Deal.Number.Repurchases)
# Now remove some important inconsistencies
# first clean some duplicate matches
pre_merge_count = length(all_buybacks$Event.Date)
cleanup$Standardise$initial_overlap = pre_merge_count

# THESE ARE JUST TO DOUBLE CHECK (in case we missed any lower case or other problems)
bad_merge_duplicateMA = duplicated(all_buybacks$Deal.Number.MA) | duplicated(all_buybacks$Deal.Number.MA, fromLast = TRUE)  
bad_merge_duplicatedrepurchase =  duplicated(all_buybacks$Deal.Number.Repurchases) | duplicated(all_buybacks$Deal.Number.Repurchases, fromLast = TRUE)
sum(bad_merge_duplicateMA | bad_merge_duplicatedrepurchase)
bad_merge_industry = all_buybacks$Industry.x != all_buybacks$Industry.y
bad_merge_ticker = all_buybacks$Ticker.Symbol.x != all_buybacks$Ticker.Symbol.y
to_remove = bad_merge_industry | bad_merge_ticker
sum(to_remove)

# clean the names now
all_buybacks$Industry <- all_buybacks$Industry.x
all_buybacks$Industry.x <- all_buybacks$Industry.y <- NULL
all_buybacks$Ticker.Symbol <- all_buybacks$Ticker.Symbol.x
all_buybacks$Ticker.Symbol.x <- all_buybacks$Ticker.Symbol.y <- NULL
# Now these are not exactly the same.... not clear why, but we merge them using as default the repurchases ones. 
all_buybacks$Closing.Price <- ifelse(all_buybacks$Closing.Price.y!=0, all_buybacks$Closing.Price.y, all_buybacks$Closing.Price.x)
all_buybacks$Closing.Price.x <- all_buybacks$Closing.Price.y <- NULL
all_buybacks$Market.Cap <- ifelse(all_buybacks$Market.Cap.y!=0, all_buybacks$Market.Cap.y, all_buybacks$Market.Cap.x)
all_buybacks$Market.Cap.x <- all_buybacks$Market.Cap.y <- NULL
all_buybacks$Event.Size <- ifelse(all_buybacks$Event.Size.y!=0, all_buybacks$Event.Size.y, all_buybacks$Event.Size.x)
all_buybacks$Event.Size.x <- all_buybacks$Event.Size.y <- NULL
all_buybacks$Purpose.Code <- ifelse(all_buybacks$Purpose.Code.y != "", all_buybacks$Purpose.Code.y, all_buybacks$Purpose.Code.x)
all_buybacks$Purpose.Code.x <- all_buybacks$Purpose.Code.y <- NULL
all_buybacks$Stock.Exchange <- ifelse(all_buybacks$Stock.Exchange.y != "", all_buybacks$Stock.Exchange.y, all_buybacks$Stock.Exchange.x)
all_buybacks$Stock.Exchange.x <- all_buybacks$Stock.Exchange.y <- NULL

#### Now add the remaining ones. First populate the missing columns with default missing values
all_buybacks$X6.CUSIP.match <- NULL # we don't need this any more
buybacks_repurchases$X6.CUSIP.match <- NULL
buybacks_ma$X6.CUSIP.match <- NULL
# repurchases first
buybacks_repurchases$Deal.Number.MA <- 0 # it is numeric
buybacks_repurchases$Event.Size.Dollar <- 0 # it is numeric
buybacks_repurchases$X6.CUSIP <- sapply(buybacks_repurchases$CUSIP, function(i) str_sub(i,1,6)) # same as before, but no tolower
# MA now
buybacks_ma$Name <- "" # character
buybacks_ma$Currency <- "US" # character - HERE WE GOT ONLY $US DEALS, SO WE ARE OK!
buybacks_ma$CUSIP <- "" # character
buybacks_ma$Deal.Number.Repurchases <- 0 # it is numeric
buybacks_ma$Pct..Auth..Shares.Out. <- 0
buybacks_ma$Tech..nique.Code <- "" # character
buybacks_ma$Trans..Type.Code <- "" # character
buybacks_ma$Source...of..Funds..Code <- "" # character
# Now append what is missing after sorting their names
colnames(all_buybacks) <- names(all_buybacks)
colnames(buybacks_ma) <- names(buybacks_ma)
colnames(buybacks_repurchases) <- names(buybacks_repurchases)
all_buybacks = all_buybacks[, sort(names(all_buybacks))]
buybacks_ma = buybacks_ma[, sort(names(buybacks_ma))]
buybacks_repurchases = buybacks_repurchases[, sort(names(buybacks_repurchases))]
# check
sum(names(all_buybacks)!=names(buybacks_ma))
sum(names(all_buybacks)!=names(buybacks_repurchases))

# check:
length(intersect(only_MA_deals, all_buybacks$Deal.Number.MA))
length(intersect(only_repurchase_deals, all_buybacks$Deal.Number.Repurchases))
length(intersect(only_MA_deals, only_repurchase_deals))

both_databases = length(all_buybacks$Event.Date)
# For each of the buybacks datasets now add events we need to add to all_buybacks
all_buybacks = rbind(all_buybacks, buybacks_ma[buybacks_ma$Deal.Number.MA %in% only_MA_deals, ])
all_buybacks = rbind(all_buybacks, buybacks_repurchases[buybacks_repurchases$Deal.Number.Repurchases %in% only_repurchase_deals, ])

cleanup$Standardise$only_MA_deals = length(only_MA_deals)
cleanup$Standardise$only_repurchase_deals = length(only_repurchase_deals)
cleanup$Standardise$overlap_deals = both_databases
cleanup$Standardise$final_merge_data= length(all_buybacks$Event.Date)
unlist(cleanup$Standardise) # ALL SHOULD MATCH NOW!!!
###########################

# Finally, since we are all the way here, we just add whatever is missing for the "standardized" data
all_buybacks$Closing.Price <- scrub(as.numeric(gsub(",",".",all_buybacks$Closing.Price)))
all_buybacks$Adj.Closing.Price <- 0*all_buybacks$Closing.Price
# WE DO NOT USE THE P/B FROM SDC (which anyway is not available for buybacks) AS THAT IS CONTEMPORANEOUS WITH THE EVENT - WE NEED THE ONE 2 YEARS BEFORE THE EVENT
all_buybacks$P.B <- rep(NA,length(all_buybacks$Closing.Price))
all_buybacks = all_buybacks[sort(as.numeric(all_buybacks$Event.Date), index.return=TRUE)$ix,]
# Just remove "\n" characters from some of the features
# let's see which ones
for (iter in 1:ncol(all_buybacks)){
  #cat(", ", iter,",")
  if (class(all_buybacks[,iter]) == "character"){
    tmp1 = all_buybacks[,iter]
    tmp1[is.na(tmp1)] <- ""
    if (sum(sapply(tmp1, function(i) str_detect(i,"\n")))){
      #cat(names(all_buybacks)[iter])
      tmp <- sapply(tmp1, function(i) gsub("\n", "+",i))
      names(tmp) <- NULL
      all_buybacks[,iter] <- tmp    
    }
  }
}

# Finally save it.
write.table(all_buybacks,file_out_for_standardize_data,row.names=F, col.names=T, quote = T, sep=";")

##############################################################################
# These are some tests based on manually extracted buybacks in Nov-Dec 2014
##############################################################################

if (0){
  
  length(all_buybacks$CUSIP)
  sum(all_buybacks$Deal.Number.MA != 0 & all_buybacks$Deal.Number.Repurchases == 0)
  sum(all_buybacks$Deal.Number.MA == 0 & all_buybacks$Deal.Number.Repurchases != 0)
  sum(all_buybacks$Deal.Number.MA !=0 & all_buybacks$Deal.Number.Repurchases != 0)
  sum(all_buybacks$Deal.Number.MA != 0 & all_buybacks$Deal.Number.Repurchases == 0) + sum(all_buybacks$Deal.Number.MA == 0 & all_buybacks$Deal.Number.Repurchases != 0) + sum(all_buybacks$Deal.Number.MA !=0 & all_buybacks$Deal.Number.Repurchases != 0)
  sum(all_buybacks$Deal.Number.MA == 0 & all_buybacks$Deal.Number.Repurchases != 0 & all_buybacks$Tech..nique.Code %in% c("OP"))
  
  Alberto_1998_2010 = 11096 
  all_buybacks_1998_2010 = all_buybacks[all_buybacks$Event.Date >= "1998-01-01" & all_buybacks$Event.Date < "2011-01-01",]
  length(all_buybacks_1998_2010$CUSIP)
  sum(all_buybacks_1998_2010$Deal.Number.MA != 0 & all_buybacks_1998_2010$Deal.Number.Repurchases == 0)
  sum(all_buybacks_1998_2010$Deal.Number.MA == 0 & all_buybacks_1998_2010$Deal.Number.Repurchases != 0)
  sum(all_buybacks_1998_2010$Deal.Number.MA !=0 & all_buybacks_1998_2010$Deal.Number.Repurchases != 0)
  sum(all_buybacks_1998_2010$Deal.Number.MA != 0 & all_buybacks_1998_2010$Deal.Number.Repurchases == 0) + sum(all_buybacks_1998_2010$Deal.Number.MA == 0 & all_buybacks_1998_2010$Deal.Number.Repurchases != 0) + sum(all_buybacks_1998_2010$Deal.Number.MA !=0 & all_buybacks_1998_2010$Deal.Number.Repurchases != 0)
  sum(all_buybacks_1998_2010$Deal.Number.MA == 0 & all_buybacks_1998_2010$Deal.Number.Repurchases != 0 & all_buybacks_1998_2010$Tech..nique.Code %in% c("OP"))
  
  
  Novbb_file  = "dataset/buybacks/sdc/November2014.csv"
  Decbb_file  = "dataset/buybacks/sdc/December2014.csv"
  
  Novbb <- read.csv(Novbb_file,header=T,stringsAsFactors = F,sep=",")
  Decbb <- read.csv(Decbb_file,header=T,stringsAsFactors = F,sep=",")
  
  length(c(Novbb[,1], Decbb[,1]))
  theo_tickers = unique(c(Novbb[,1], Decbb[,1]))
  length(theo_tickers)
  length(intersect(all_buybacks$Ticker.Symbol, theo_tickers))
  
  all_buybacks_theo_all = all_buybacks[all_buybacks$Ticker.Symbol %in% theo_tickers,]
  all_buybacks_theo_NovDec2014 = all_buybacks[all_buybacks$Ticker.Symbol %in% theo_tickers & all_buybacks$Event.Date > "2014-10-31" & all_buybacks$Event.Date < "2015-01-01",]
  all_buybacks_theo_Fall2014 = all_buybacks[all_buybacks$Ticker.Symbol %in% theo_tickers & all_buybacks$Event.Date > "2014-08-31" & all_buybacks$Event.Date < "2015-01-01",]
  nrow(all_buybacks_theo_all)
  nrow(all_buybacks_theo_Fall2014)
  nrow(all_buybacks_theo_NovDec2014)
  
  nrow(all_buybacks_theo_NovDec2014)
  sum(all_buybacks_theo_NovDec2014$Deal.Number.MA != 0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases == 0)
  sum(all_buybacks_theo_NovDec2014$Deal.Number.MA == 0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases != 0)
  sum(all_buybacks_theo_NovDec2014$Deal.Number.MA !=0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases != 0)
  sum(all_buybacks_theo_NovDec2014$Deal.Number.MA != 0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases == 0) + sum(all_buybacks_theo_NovDec2014$Deal.Number.MA == 0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases != 0) + sum(all_buybacks_theo_NovDec2014$Deal.Number.MA !=0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases != 0)
  sum(all_buybacks_theo_NovDec2014$Deal.Number.MA == 0 & all_buybacks_theo_NovDec2014$Deal.Number.Repurchases != 0 & all_buybacks_theo_NovDec2014$Tech..nique.Code %in% c("OP"))
  
}

