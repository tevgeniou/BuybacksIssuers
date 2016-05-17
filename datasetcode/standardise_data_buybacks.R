#standardizes the SDC buyback data

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
source("lib_helpers.R", chdir=TRUE)

# In case we need to change these...
FirstTrade = "1900-01-01"
AbsoluteMinimumDate =  "1900-01-01"
Last = Today()

BUYBACK_DATA = list()

################################################################

# For  buybacks we do all the standarization when we merge the M&A and repurchases data!
source('rawdata_events_buybacks/sdc2016/buybacks_rawdata_combine.R')
#all_buybacks, cleanup
SDC <- all_buybacks
rm("all_buybacks")


################################################################
### SOME DATA CLEAN
################################################################

the_dates = SDC$Event.Date
inside_range = ifelse(!is.na(the_dates), the_dates >= FirstTrade & the_dates <= Last, T)
SDC = SDC[inside_range,]

the_dates = SDC$Event.Date
SDC_NODATE = (is.na(the_dates))  
SDC_tooearly <- rep(F,length(SDC_NODATE))
SDC_tooearly[!(is.na(the_dates))] <- the_dates[!(is.na(the_dates))] < AbsoluteMinimumDate
SDC_NOCUSIP = (SDC$X6.CUSIP == "")  # Note that we check only the 6 digit CUSIP here, as the buybacks M&A have only that

to_keep = !SDC_NODATE & !SDC_tooearly & !SDC_NOCUSIP
total_orig = nrow(SDC)
SDC = SDC[to_keep,]
total_sdc_clean = nrow(SDC)

SDC$X <- NULL # just in case there was any empty columns left (should not be as they are removed in buybacks_rawdata_combine.R)

# We do no more filtering at this stage. All filtering will be done at the business decision level. For now we get all the data. 
#useonly = (SDC$Tech..nique.Code %in% c("OP") & SDC$Trans..Type.Code %in% c("AU") 
           #& as.Date(SDC$Initial.Auth..Date,format="%d/%m/%Y") >= First
#           )
#SDC = SDC[useonly,]

### the "standarization" is already done by buybacks_rawdata_combine.R for the buybacks....

# HERE IS WHERE WE APPEND 10 AT THE END OF THE 6-DIGIT CUSIPS AS THE DEFAULT CUSIP, NOTING WHETHER IT WAS ORIGINALLY MISSING
SDC$missingCUSIP8 <- ifelse(SDC$CUSIP == "", "1","0") # save as characters to make it simple for python later
SDC$CUSIP = ifelse(SDC$CUSIP == "", paste(SDC$X6.CUSIP,"10",sep=""), SDC$CUSIP)

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

# For this data we have very few duplicated events, so we just remove them
duplicate_events = duplicated(paste(SDC$CUSIP, SDC$Event.Date,sep=""))
duplicate_events = duplicate_events + duplicated(paste(SDC$CUSIP, SDC$Event.Date,sep=""), fromLast = TRUE)
DUPLICATE_EVENTS_REMOVED = sum(duplicate_events!=0)
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
cleanup$Standardise$SDC_initial_data = total_orig
cleanup$Standardise$SDC_NODATE      = sum(SDC_NODATE)
cleanup$Standardise$SDC_NOCUSIP     = sum(SDC_NOCUSIP)
cleanup$Standardise$SDC_tooearly    = sum(SDC_tooearly)
cleanup$Standardise$clean_cusip8    = sum(!clean_cusip8)
cleanup$Standardise$clean_cusip6    = sum(!clean_cusip6)
cleanup$Standardise$SDC_standardize_removal = total_orig- nrow(SDC) 

BUYBACK_DATA$DATASET$SDC <- SDC
BUYBACK_DATA$cleanupSDC = cleanup

save(BUYBACK_DATA, file = "created_buyback_data/GLOBAL_BUYBACK.Rdata")



