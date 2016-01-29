
#datasetname = "issuers" 
#datasetname = "buybacks" 

# Variable "cleanup" is gathering all data cleaning information for EACH step, indicated by the name of the step 
# (so we can easily track back where we removed any data)

# This is the file where each step reads and writes to. It is first created by standardise_data.R below
global_output_file= paste("dataset/",datasetname,".Rdata",sep="")

############################################################
cat("\n\n*******\n\nGetting the Market and Fama-French data \n") 
# it is ok, this is done for each dataset, for consistency 
# across dataset: it is the same data in all cases anyway.
############################################################

# This assumes the S&P daily and monthly returns as well as the FF data are in the files
# defined as inputs in get_ff_and_market_data.R (see README)
source("dataset/process_code/get_ff_and_market_data.R")

############################################################
#standardize the SDC (or other input) data, generates tmp_files/sdc_output_X_standardised.csv
############################################################

# This assumes the SDC data are in 
# x/sdc/sdc_output_x.csv (2 files for the buybacks, one from "Repurchaces" 
# and one from "US Targets" of the SDC database as described in README)
# It also removes any events for which either the CUSIP or the Event Date is missing

cat("\n\n******\n\nRunning standardise\n cleanup$Standardize should have all data cleaning info at the end of this stage\n")
source(paste("dataset/", datasetname, "/standardise_data.R", sep=""))

############################################################
cat("\n\n**********\n\nGetting the CRSP data - NOTE!!! requires manual interventions, descibed in get_crsp_data.R and Steps 2 and 3 in README: Have you done them?\n")
############################################################

#cat("\nFrom the ",sum(SDC$missingCUSIP8 == "0")," 8-digits CUSIPs available, out of the ",length(SDC$missingCUSIP8)," events, ", sum(str_sub(SDC$CUSIP[SDC$missingCUSIP8 == "0"],start=7,end=8) == "10"), " end with a 10, ", sum(str_sub(SDC$CUSIP[SDC$missingCUSIP8 == "0"],start=7,end=8) == "11"), " end with an 11, and  ", sum(!(str_sub(SDC$CUSIP[SDC$missingCUSIP8 == "0"],start=7,end=8) %in% c("10","11"))), "with other digits\n")
source("dataset/process_code/get_crsp_data.R")

############################################################
cat("\n\n**********\n\nGetting the Compustat data - NOTE!!! requires manual interventions, descibed in get_compustat_data.R and Step 4 in README: Have you done them?\n")
cat("\n Here we also merge the Compustat data into SDC\n\n")
############################################################

source("dataset/process_code/get_compustat_data.R")
if (!file.exists(paste("dataset/",datasetname, "/compustat/compustat_fundamental_annual.csv",sep="")) |
      !file.exists(paste("dataset/",datasetname, "/compustat/compustat_pension_annual.csv",sep="")) )
  stop("Compustat data have not been downloaded from CRSP/Compustat and Compustat, as explained in Step 4 of README")

############################################################
cat("\n\n**********\n\nGetting the IBES data - NOTE!!! requires manual intervention (Step 5 in README): Have you done them?\n\n")
############################################################

file_for_IBES <- paste("dataset/",datasetname,"/ibes/cusip_for_ibes.csv",sep="")

ibes_cusip_permno_from_python <- read.csv(paste("dataset/tmp_files/crsp_sdc_link_",datasetname,".csv", sep=""))
write.table(data.frame(cusip = unique(ibes_cusip_permno_from_python[,"cusip"])),file_for_IBES,row.names=F, col.names=F, quote = F, sep=",")

if (!file.exists(paste("dataset/",datasetname, "/ibes/ibes_all.csv",sep="")))
  stop("IBES data have not been downloaded from IBES, as explained in Step 5 of README")

############################################################
# merge all data (SDC,CRSP, Compustat, IBES). All data are assumed 
# to be saved at this stage, in the files needed at the begining of merge_dataset.R
############################################################

cat("\n\n\n**********\n\nRunning merge\n\n")
source("dataset/process_code/merge_dataset.R")

############################################################
# scrub the dataset
############################################################

cat("\n\n**********\n\nRunning scrub\n\n")
source("dataset/process_code/scrub_dataset.R")

############################################################
# The final data that will be used by the .Rnw are now save (by scrub_dataset.R) in:
# paste("dataset/",datasetname,".Rdata",sep="")

