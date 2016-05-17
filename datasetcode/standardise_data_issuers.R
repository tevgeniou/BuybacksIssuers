#standardizes the SDC issuers data

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
source("lib_helpers.R", chdir=TRUE)

# In case we need to change these...
FirstTrade = "1900-01-01"
AbsoluteMinimumDate =  "1900-01-01"
Last = Today()

ISSUERS_DATA = list()

################################################################

SDC <- read.csv("rawdata_events_issuers/sdc_output_issuers.csv",header=T,stringsAsFactors = F,sep=";")
#Make sure we have: Event.Date, CUSIP, and all other standardised data characteristics as described in README in the datasets directory
the_dates = as.Date(SDC$X.Filing...Date,format="%d/%m/%y",origin="19700101")
inside_range = ifelse(!is.na(the_dates), the_dates >= FirstTrade & the_dates <= Last, T)
SDC = SDC[inside_range,]

the_dates = as.Date(SDC$X.Filing...Date,format="%d/%m/%y",origin="19700101")
SDC_NODATE = (is.na(the_dates))  
SDC_NOCUSIP = (SDC$X9.Digit.CUSIP == "")  

to_keep = !SDC_NODATE & !SDC_NOCUSIP
total_orig = nrow(SDC)
SDC = SDC[to_keep,]
total_sdc_clean = nrow(SDC)

SDC$X <- NULL # just in case there was any empty column in the .csv from SDC

# We do no more filtering at this stage. All filtering will be done at the business decision level. For now we get all the data. 
#useonly = (SDC$IPO.Flag..Y.N. %in% c("No") & SDC$Issue.Type %in% c("FO")
#           #& as.Date(SDC$X.Filing...Date,format="%d/%m/%y",origin="19700101") >= First
#           )
#SDC = SDC[useonly,]

################################################################
# THE REQUIRED FIELDS FIRST
################################################################

# THE REQUIRED FIELDS FIRST
SDCstandardised <- data.frame(Event.Date = as.Date(SDC$X.Filing...Date,format="%d/%m/%y",origin="19700101"))
SDCstandardised$CUSIP <- sapply(SDC$X9.Digit.CUSIP,function(x) substr(x,1,8))
SDCstandardised$Ticker.Symbol <- SDC$Ticker.Symbol
#SDCstandardised$Name <- SDC$Issuer
SDCstandardised$Industry <- SDC$Main.SIC.Code
SDCstandardised$Currency <- SDC$Cur..rency 
SDCstandardised$Stock.Exchange <- SDC$Primary.Exchange.Where.Issuer.s.Stock.Trades
SDCstandardised$Event.Size <- scrub(SDC$Deal.Size..as.Pct.of.Market.Cap)
#SDCstandardised$Closing.Price <- suppressWarnings(scrub(SDC$Stock.Price.1.Day..Prior.to.Filing.Date))
SDCstandardised$Closing.Price <- 0
# We assume the SDC price was NOT adjusted. We get this from CRSP
SDCstandardised$Adj.Closing.Price <- 0*SDCstandardised$Closing.Price
SDCstandardised$Market.Cap <- suppressWarnings(scrub(as.numeric(gsub(" ","",SDC$Market.Value.Before.Offer....mil.))))
SDCstandardised$Deal.Number <- SDC$X.Deal.Number
SDCstandardised$P.B <- scrub(SDC$Price..Book.Value.Before.Offer) # we also get this in case we want to use it instead of the BE.ME we later create using Compustat

# NOW FIELDS THAT ARE SPECIFIC TO THIS DATASET
SDCstandardised$Issue.Type <- SDC$Issue.Type
SDCstandardised$Issue.Date <-  as.Date(SDC$Issue.Date,format="%d/%m/%y",origin="19700101")
SDCstandardised$Trade.Date <-  as.Date(SDC$Trade.Date,format="%d/%m/%y",origin="19700101")
SDCstandardised$Offering.Technique <- SDC$Offering.Technique
SDCstandardised$SEC.File.Number <- SDC$SEC.File.Number
SDCstandardised$SEC..Regis...tration...Fee <- SDC$X..SEC..Regis...tration...Fee

SDC = SDCstandardised
SDC$missingCUSIP8 <- ifelse(SDC$CUSIP == "", "1","0")  # here we have them all. save as characters to make it simple for python later
SDC$X6.CUSIP <- sapply(SDC$CUSIP, function(i) str_sub(i,start=1, end=6)) 

# Remove any remaining problematic cusips
clean_cusip8 <- (str_count(SDC$CUSIP) == 8)
clean_cusip6 <- (str_count(SDC$X6.CUSIP) == 6)
SDC = SDC[clean_cusip8 & clean_cusip6,]

# add any remaining variables that will be filled later by compustat and crsp
SDC$Closing.Price.lastyear   <- 0*SDC$Closing.Price
SDC$Shares.Out               <- 0*SDC$Closing.Price

SDC$ME_usedfor_BEME        <- rep(NA,length(SDC$Closing.Price))
SDC$BE_used                <- rep(NA,length(SDC$Closing.Price))
SDC$BE.ME                  <- rep(NA,length(SDC$Closing.Price))
SDC$BEME_quantile          <- rep(NA,length(SDC$Closing.Price))
SDC$ME_quantile            <- rep(NA,length(SDC$Closing.Price))
SDC$ME.Breakpoints_used    <- lapply(1:length(SDC$Closing.Price), function(i) NULL)
SDC$BE.ME.Breakpoints_used <- lapply(1:length(SDC$Closing.Price), function(i) NULL)
SDC$Market.Cap_usedSource  <- as.character(0*SDC$Closing.Price)

############################################################################
### NOW DEAL WITH DUPLICATED CUSIPxEVENT.DATE PAIRS!!!!

# First remove SEO events for which the Issue or Trade date is BEFORE the Event Date
DATES_ISSUES = sum(!((SDC$Event.Date <= SDC$Issue.Date) & (SDC$Event.Date <= SDC$Trade.Date)))
SDC = SDC[(SDC$Event.Date <= SDC$Issue.Date) & (SDC$Event.Date <= SDC$Trade.Date),]

# Next find duplicates WITHOUT considering the Issue and Trade dates (which can be different across) and remove those
duplicate_events = duplicated(paste(SDC$CUSIP, SDC$Event.Date,SDC$Issue.Date, SDC$Trade.Date,sep=""))
duplicate_events = duplicate_events + duplicated(paste(SDC$CUSIP,  SDC$Event.Date,SDC$Issue.Date, SDC$Trade.Date,sep=""), fromLast = TRUE)
BAD_DUPLICATES = sum(duplicate_events)
SDC = SDC[!duplicate_events,]

#Now for the remaining duplicated events, keep the event with the earlier Issue date
# Sort over Issue time first 
SDC = SDC[sort(as.numeric(SDC$Issue.Date), index.return=TRUE)$ix,]
duplicate_events = duplicated(paste(SDC$CUSIP, SDC$Event.Date,sep=""))
ISSUE_DATES_DUPLICATES = sum(duplicate_events)
SDC = SDC[!duplicate_events,]

############################################################################

# Just remove "\n" characters from some of the features
# let's see which ones
for (iter in 1:ncol(SDC)){
  #cat(", ", iter,",")
  if (class(SDC[,iter]) == "character"){
    tmp1 = SDC[,iter]
    tmp1[is.na(tmp1)] <- ""
    if (sum(sapply(tmp1, function(i) str_detect(i,"\n")))){
      #cat(names(SDC)[iter])
      tmp <- sapply(tmp1, function(i) gsub("\n", "+",i))
      names(tmp) <- NULL
      SDC[,iter] <- tmp    
    }
  }
}

# Finally Sort over time before saving
SDC = SDC[sort(as.numeric(SDC$Event.Date), index.return=TRUE)$ix,]

# Add the clean up numbers
cleanup = list()
cleanup$Standardise = list()
cleanup$Standardise$SDC_initial_data = total_orig
cleanup$Standardise$SDC_NODATE      = sum(SDC_NODATE)
cleanup$Standardise$SDC_NOCUSIP     = sum(SDC_NOCUSIP)
cleanup$Standardise$clean_cusip8    = sum(!clean_cusip8)
cleanup$Standardise$clean_cusip6    = sum(!clean_cusip6)
cleanup$Standardise$SDC_standardize_removal = total_orig- nrow(SDC) 

cleanup$Standardise$ISSUE_DATES_DUPLICATES = ISSUE_DATES_DUPLICATES
cleanup$Standardise$BAD_DUPLICATES  = BAD_DUPLICATES
cleanup$Standardise$DATES_ISSUES    = DATES_ISSUES


ISSUERS_DATA$DATASET$SDC <- SDC
ISSUERS_DATA$cleanupSDC = cleanup

#######################################################
##### DONE WITH THE BUYABCK DATABASE, SAVE IT FOR NOW 

save(ISSUERS_DATA, file = "created_issuers_data/GLOBAL_ISSUERS.Rdata")



