
######################################################################## 
######################################################################## 
## FIRST PART (See README)
######################################################################## 
######################################################################## 

#################################################### 
# Input files
#################################################### 

# Get the usual file where all data is saved at each step:
# global_output_file: see create_dataset.R
load(global_output_file) # always get the data from the previous step, add new data, and save back to it again

# Assumes this file exists (see README)
# THIS IS THE MASTER CRSP-COMPUSTAT UNIVERSE PAIRS FILE FROM CRSP/COMPUSTAT MERGED (can be used for whatever data)
# See README on how to get it from CRSP/COMPUSTAT Merged
crsp_compustat_gvkey_pairs <- read.csv("dataset/crsp_compustat_gvkey_pairs/crsp_compustat_gvkey_pairs.csv",header=T,stringsAsFactors = F,sep=",")

# This file has all the FF and market data, already done by get_ff_and_market_data.R - so it should be already in memory.
load("dataset/indices_and_factors/market_ff_data.Rdata")

#################################################### 
# Output files:
#################################################### 

# this will be used to manually get the data from Compustat. This file is not used again after getting the data from Compustat
file_out_gvkey = paste("dataset/",datasetname,"/compustat/gvkeys_for_compustat_",datasetname,".csv",sep="")

#################################################### 
# CLEAN UP IN THIS FILE:
# We remove the SDC events for which we do not find any GVKEY

#################################################### 
# Match SDC permnos to the gvkeys, also appending the GVKEYs to the SDC data

# First clean up crsp_compustat_gvkey_pairs
# keep always the last available
crsp_compustat_gvkey_pairs = crsp_compustat_gvkey_pairs[sort(crsp_compustat_gvkey_pairs[,"datadate"], index.return=T)$ix,]
to_remove = is.na(crsp_compustat_gvkey_pairs[,"revt"]) | 
  is.na(crsp_compustat_gvkey_pairs[,"at"]) | 
  scrub(crsp_compustat_gvkey_pairs[,"at"]) <= 0 | 
  scrub(crsp_compustat_gvkey_pairs[,"revt"]) <= 0 | 
  crsp_compustat_gvkey_pairs[,"LINKPRIM"] %in% c("J","N")
cleanup$compustat$GVKEYremoves = length(unique(crsp_compustat_gvkey_pairs[to_remove,"LPERMNO"]))
cleanup$compustat$GVKEYnotremoves = length(unique(crsp_compustat_gvkey_pairs[!to_remove,"LPERMNO"]))
crsp_compustat_gvkey_pairs = crsp_compustat_gvkey_pairs[!to_remove,]

# simplify first
tmp = paste(crsp_compustat_gvkey_pairs[,"GVKEY"], crsp_compustat_gvkey_pairs[,"LPERMNO"])
crsp_compustat_gvkey_pairs = crsp_compustat_gvkey_pairs[!duplicated(tmp, fromLast= TRUE) ,] # most recent

# Remove PERMNOs with duplicate GVKEYs now - if any are left
tmp=sapply(unique(crsp_compustat_gvkey_pairs[,"LPERMNO"]), function(i) length(unique(crsp_compustat_gvkey_pairs[crsp_compustat_gvkey_pairs[,"LPERMNO"] == i,"GVKEY"])))
duplicates= crsp_compustat_gvkey_pairs[,"LPERMNO"] %in% unique(crsp_compustat_gvkey_pairs[,"LPERMNO"])[which(tmp>1)]
cleanup$compustat$DuplicateGVKEYs = sum(duplicates)
crsp_compustat_gvkey_pairs = crsp_compustat_gvkey_pairs[!duplicates,]

# Match now the SDC permnos with the Compustat GVKEYs
idx = match(SDC$permno,crsp_compustat_gvkey_pairs[,"LPERMNO"])
SDC$GVKEY = ifelse(!is.na(idx) & !is.na(SDC$permno), crsp_compustat_gvkey_pairs[,"GVKEY"][idx], NA)
rm("idx", "tmp", "duplicates", "crsp_compustat_gvkey_pairs")

cleanup$compustat$NO_GVKEY_remove = sum(is.na(SDC$GVKEY))
SDC = SDC[!is.na(SDC$GVKEY),]
permno_gvkey_pairs  = cbind(SDC$permno, SDC$GVKEY)
colnames(permno_gvkey_pairs) <- c("PERMNO", "GVKEY")
cat("\n*** SUMMARY of GVKEY matching: A total of ", sum(!is.na(SDC$GVKEY)), "events got a GVKEY,",cleanup$compustat$NO_GVKEY, "did not\n")

# save the unique GVKEYs found to this csv file that will be used to get 
# the CRSP/Compustat Merged data, as descibed in README
write.table(data.frame(GVKEY = unique(permno_gvkey_pairs[,"GVKEY"])),file_out_gvkey,row.names=F, col.names=F, quote = F, sep=",")

######################################################################## 
######################################################################## 
## SECOND PART: merge the Compustat data with the existing SDC data so far - See README
######################################################################## 
######################################################################## 

# Now we need to use the file_out_gvkey file to get the Compustat data manually, as described in README.
# Once the Compustat data are in 
# datasetname/compustat/compustat_fundamental_annual.csv and 
# datasetname/compustat/compustat_pension_annual .csv
# we can now get the final Compustat data that will be used by merge_data.R. 
# Note that the final Compustat data identify companies with the CRSP PERMNOs, hence we only need this one 
# file for Compustat in merge_dataset.R

if (!file.exists(paste("dataset/",datasetname, "/compustat/compustat_fundamental_annual.csv",sep="")) | 
      !file.exists(paste("dataset/",datasetname, "/compustat/compustat_pension_annual.csv",sep="")))
  stop("Compustat .csv Data from CRSP/Compustat and Compustat (see README) are not in the x/compustat directory...")

# we now combine all Compustat data using these 2 files created manually:
fundamental_annual <- read.csv(paste("dataset/",datasetname, "/compustat/compustat_fundamental_annual.csv",sep=""), header=T,stringsAsFactors = F,sep=",")
pension_annual     <- read.csv(paste("dataset/",datasetname, "/compustat/compustat_pension_annual.csv",sep=""), header=T,stringsAsFactors = F,sep=",")
#plus this matrix permno_gvkey_pairs to do some tests # see above: this has the PERMNO-GVKEY pairs

# just not to confuse them later
colnames(fundamental_annual)[which(colnames(fundamental_annual) == "LPERMNO")] <- "PERMNO"
colnames(pension_annual) <- paste(colnames(pension_annual), "pension", sep=".")
# just replace these with the same names as those used in fundamental_annual 
colnames(pension_annual)[which(colnames(pension_annual) == "gvkey.pension")] <- "GVKEY"
colnames(pension_annual)[which(colnames(pension_annual) == "datadate.pension")] <- "datadate"

# We don't need the rows where no pension data is available, but not the other way around
all_compustat_data <- merge(fundamental_annual,pension_annual, by=c("GVKEY","datadate"), all.x=TRUE, all.y = FALSE)

# just make sure all the LPERMNOs are in the PERMNOs with the correct GVKEYS. 
# WHY ARE SOME NOT THE SAME?! Not clear. we remove these
tmp_test1 = paste(all_compustat_data[,"PERMNO"], all_compustat_data[,"GVKEY"])
tmp_test2 = paste(permno_gvkey_pairs[,"PERMNO"], permno_gvkey_pairs[,"GVKEY"])

all_compustat_data = all_compustat_data[tmp_test1 %in% tmp_test2, ]

# Some formatting
all_compustat_data$datadate <- as.Date(as.character(all_compustat_data$datadate), format="%Y%m%d")

#cat("\nAfter all the Compustat data merging, we find ",table(-GVKEY_initial[GVKEY_initial <= 0]), "cases where the GVKEYs don't match across all 3 CRSP/Compustat and Compustat Datasets used, which we remove")
cat("\n\nThe Compustat data have ", nrow(all_compustat_data), "rows, each with ",ncol(all_compustat_data), "features\n\n")

###########################################################################
# We now augment the data with the BE/ME ratios 
###########################################################################

# Get the variables for the equation first:
# http://www.crsp.com/products/documentation/annual-data-industrial
compu.at           = all_compustat_data$at
compu.ceq          = all_compustat_data$ceq
compu.dlc          = all_compustat_data$dlc
compu.dltt         = all_compustat_data$dltt
compu.lt           = all_compustat_data$lt
compu.pstk         = all_compustat_data$pstk
compu.pstkl        = all_compustat_data$pstkl
compu.pstkrv       = all_compustat_data$pstkrv
compu.seq          = all_compustat_data$seq
compu.txditc       = all_compustat_data$txditc
compu.prba.pension = all_compustat_data$prba.pension

# The BE/ME equation now  
preferred_stock_all = ifelse(!is.na(compu.pstkl), compu.pstkl, ifelse(!is.na(compu.pstkrv), compu.pstkrv, compu.pstk))
cat("We start with ", sum(is.na(preferred_stock_all)), " missing preferred stock data, ", sum((preferred_stock_all[!is.na(preferred_stock_all)]==0)),
    " with 0 value, and ", sum((preferred_stock_all[!is.na(preferred_stock_all)]!=0)),
    "with non-zero values out of the total (across all months) of ", 
    length(preferred_stock_all), "ones, and ", sum(preferred_stock_all[!is.na(preferred_stock_all)] < 0),
    "negative ones that we set to 'missing'\n\n")
if (sum(scrub(preferred_stock_all)<0) !=0){
  cat("\nWe set the ", sum(scrub(preferred_stock_all)<0), "negative ones to NA\n")
  preferred_stock_all[which(scrub(preferred_stock_all) < 0)] <- NA
}

BE_used_all  = 
  ifelse(!is.na(compu.seq),compu.seq, ifelse(!is.na(compu.ceq+compu.pstk), compu.ceq+compu.pstk, compu.at - compu.lt)) -
  preferred_stock_all + 
  scrub(compu.txditc)  - scrub(compu.prba.pension) # NOTE: use "scrub" here as THESE WE ADD IF AVAILABLE ONLY!! ELSE WE HAVE MANY NAs
# add dates names to them, so we can refer to them later
names(BE_used_all) <- all_compustat_data$datadate

cat("We now have with ", sum(is.na(BE_used_all)), " missing BE data, ", 
    sum((BE_used_all[!is.na(BE_used_all)]==0))," with 0 value, and",
    sum((BE_used_all[!is.na(BE_used_all)]!=0)), 
    "with non-zero values out of the total (across all months) of ", 
    length(BE_used_all), "ones, and ", sum(BE_used_all[!is.na(BE_used_all)] < 0), 
    "negative ones (that we will fix later in merge_dataset.R following the steps at Ivo Welch's Website (see README))\n\n")

# These are all the available BEs...
idx_Compustat_availBE = !is.na(BE_used_all)

if (0){ # just an intermediate check
  check_matches = sapply(1:nrow(SDC), function(i){
    permnos = all_compustat_data$PERMNO == SDC$permno[i]
    predates = all_compustat_data$datadate < SDC$Event.Date[i]
    ifelse(scrub(sum(permnos & predates & idx_Compustat_availBE)), 1,0)
  })
  cat("**These are from", length(unique(all_compustat_data$PERMNO[which(idx_Compustat_availBE)])),"unique Compustat PERMNOs, ",
      length(intersect(unique(SDC$permno), unique(all_compustat_data$PERMNO[which(idx_Compustat_availBE)])))," unique PERMNOS in the SDC data that appear in ",
      sum(SDC$permno %in% unique(all_compustat_data$PERMNO[which(idx_Compustat_availBE)])), " unique\n SDC events, for which we have ",
      sum(check_matches)," SDC events for which we have any BE data available whatsoever before the Event Date\n\n")
}

##########################################################################################
# now get the appropriate available BE and the appropriately dated BE/ME and ME breakpoints
# that will later compare with in merge_dataset.r (where we will also get the market cap using the CRSP data)
# See README file
##########################################################################################

compustat_months = as.numeric(format(all_compustat_data$datadate,"%m"))
compustat_years  = as.numeric(format(all_compustat_data$datadate,"%Y"))
compustat_years_months = as.numeric(format(all_compustat_data$datadate,"%Y%m"))

SDC_months = as.numeric(format(SDC$Event.Date,"%m"))
SDC_years  = as.numeric(format(SDC$Event.Date,"%Y"))
SDC_years_months = as.numeric(format(SDC$Event.Date,"%Y%m"))

BEME_breakpoint_years      = as.numeric(format(as.Date(rownames(BE.MEbreakpoints)),"%Y"))
ME_breakpoint_years_months = as.numeric(format(as.Date(rownames(MEbreakpoints)),"%Y%m"))

# WE NOW APPEND THE COMPUSTAT DATA TO THE SDC ONES: We append the following Compustat Data:
# BE, BE.ME.Breakpoints_used, and ME.Breakpoints_used appropriately lagged, as well as "Compustat_Lag"-months lag other Compustat features
#SDC_orig = SDC # for debugging

year_we_care1 = SDC_years - 1*(SDC_months > 6) - 2*(SDC_months <= 6) 
year_we_care2 = SDC_years - 0*(SDC_months > 6) - 1*(SDC_months <= 6) 
all_dates = as.Date(names(BE_used_all))
if (Compustat_Lag!=0){
  Clag_year_months = as.numeric(format(AddMonths(SDC$Event.Date, -Compustat_Lag),"%Y%m"))  
} else {
  Clag_year_months = SDC_years_months    
}
SDC$preferred_stock <- NA # just keep track of this, too

compustat_data = list()
compustat_data[["at"]] = list(); compustat_data[["ceq"]] = list(); compustat_data[["dlc"]] = list()
compustat_data[["dltt"]] = list(); compustat_data[["lt"]] = list(); compustat_data[["pstk"]] = list()
compustat_data[["pstkl"]] = list(); compustat_data[["pstkrv"]] = list(); compustat_data[["seq"]] = list()
compustat_data[["txditc"]] = list(); compustat_data[["prba.pension"]] = list();compustat_data[["dates"]] = list();

for (iter in 1:nrow(SDC)){
  # if (iter%%1000 == 0) cat(iter,",")
  # now get all data available for this permno for the year we care about (depending on whether 
  # the event month is after June or not) 
  idx_Compustat_years = (compustat_years == year_we_care1[iter])
  # first get all compustat data for this permno 
  idx_Compustat_permnos = (all_compustat_data$PERMNO == SDC$permno[iter])             
  considered_BEvalues   = (idx_Compustat_availBE & idx_Compustat_years & idx_Compustat_permnos)  
  ####  GET THE BE DATA WHEN AVAILABLE 
  if (sum(considered_BEvalues) != 0) {
    considered_BEvalues = which(considered_BEvalues) #which is costly, so we do it only when needed
    # Get the very last available considered BE
    SDC$BE_used[iter] <- BE_used_all[considered_BEvalues[which.max(all_dates[considered_BEvalues])]] 
    SDC$preferred_stock[iter] <- preferred_stock_all[considered_BEvalues[which.max(all_dates[considered_BEvalues])]] # just keep track of this
    #### NOW GET THE BE/ME BREAKPOINT DATA (YEARLY)  - ONLY KEEP THE ROW OF BE.MEbreakpoints... 
    SDC$BE.ME.Breakpoints_used[iter] <- rownames(BE.MEbreakpoints)[which(BEME_breakpoint_years==year_we_care2[iter])]
    #### NOW GET THE ME BREAKPOINT DATA (MONTHLY) - previous month
    month_we_care = which(ME_breakpoint_years_months == SDC_years_months[iter])-1
    SDC$ME.Breakpoints_used[iter] <- rownames(MEbreakpoints)[month_we_care]

    # Finally get ALL other compustat data until Compustat_Lag (e.g. 1 - see global params) months before the SDC event
    compustat_years_months_this_permno = sort(compustat_years_months[idx_Compustat_permnos], decreasing = TRUE) # from most recent to older...
    pre_event_available  = compustat_years_months_this_permno[Clag_year_months[iter] > compustat_years_months_this_permno]
    idx_Compustat_recent = idx_Compustat_permnos & (compustat_years_months %in% pre_event_available)
    #just get all for now, and time it in bb_issuers.R later on!
    idx_Compustat_recent = idx_Compustat_permnos #& (compustat_years_months %in% pre_event_available)
    if (sum(idx_Compustat_recent) != 0){
      compustat_data$at[[iter]]            = all_compustat_data$at[idx_Compustat_recent]
      compustat_data$ceq[[iter]]           = all_compustat_data$ceq[idx_Compustat_recent]
      compustat_data$dlc[[iter]]           = all_compustat_data$dlc[idx_Compustat_recent]
      compustat_data$dltt[[iter]]          = all_compustat_data$dltt[idx_Compustat_recent]
      compustat_data$lt[[iter]]            = all_compustat_data$lt[idx_Compustat_recent]    
      compustat_data$pstk[[iter]]          = all_compustat_data$pstk[idx_Compustat_recent]
      compustat_data$pstkl[[iter]]         = all_compustat_data$pstkl[idx_Compustat_recent]
      compustat_data$pstkrv[[iter]]        = all_compustat_data$pstkrv[idx_Compustat_recent]
      compustat_data$seq[[iter]]           = all_compustat_data$seq[idx_Compustat_recent]
      compustat_data$txditc[[iter]]        = all_compustat_data$txditc[idx_Compustat_recent]
      compustat_data$prba.pension[[iter]]  = all_compustat_data$prba.pension[idx_Compustat_recent]
      compustat_data$dates[[iter]]         = all_compustat_data$datadate[idx_Compustat_recent]
    }      
  }      
}
cat("\n\n*** RESULTS after coupling the Compustat data in SDC: we have ", sum(is.na(SDC$BE_used)), " events with missing BE data, ", sum(scrub(SDC$BE_used)<0), 
    "with negative ones (to fix in merge_dataset.R following the steps at Ivo Welch's Website), and ", sum(!is.na(SDC$BE_used))," with non missing ones, out of the total of ", 
    nrow(SDC),"SDC events\n\n")

###########################################################################
#Save the new SDC data again, with the compustat data this time - which will be added later and treated like the ibes data
save(SDC,compustat_data,cleanup,file = global_output_file)


