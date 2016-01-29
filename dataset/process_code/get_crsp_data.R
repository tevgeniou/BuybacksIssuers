
# Most of this file is manual or python based. See Steps 2 and 3 of README

#################################################### 
# Input files
#################################################### 

# Get the usual file where all data is saved at each step:
# global_output_file: see create_dataset.R
load(global_output_file) # always get the data from the previous step, add new data, and save back to it again

# Assumes this file exists (See README)
# THIS IS THE MASTER CRSP UNIVERSE PAIRS FILE FROM CRSP (can be used for whatever data)
# "dataset/crsp_permno_cusip_pairs/crsp_permno_cusip_pairs.csv"

#################################################### 
# Output files:
#################################################### 

# Python generates these files (do not remove them as Python is slow): 
# "x/returns/permno_for_crsp_"+dataset+".csv", used to manually get the data from CRSP (see README)
crsp_sdc_link_file = paste("dataset/tmp_files/crsp_sdc_link_",datasetname,".csv",sep="") # used below to append the permnos in the SDC data

# These are from CRSP manually as described in Step 3 in README, which are then used by merge_dataset.R
#file_in_CRSP_daily = paste("dataset/",datasetname,"/returns/crsp_returns_",datasetname,".csv",sep="")
#file_in_CRSP_monthly = paste("dataset/",datasetname,"/returns/crsp_returns_",datasetname,"_monthly.csv",sep="")

# This is a temporary file, not used again outside get_crsp_data.R
# It is used by python, and then removed - see below
tmp_file = paste("dataset/tmp_files/sdc_",datasetname,"_cusips_for_python.csv",sep="") 

#################################################### 
# CLEAN UP IN THIS FILE:

# We remove any data with missing PERMNOs

#################################################### 

# We find PERMNOS using the 8-digit CUSIPS. If SDC had only 6-digit CUSIP we append them with a 10 (see standardise_data.R and step 3 of README)
#cat("SDC dimensions: ", dim(SDC),", missing 8-digit CUSIPs: ",sum(SDC$CUSIP == ""), ", CUSIPs with non-8 digits: ",sum(str_count(SDC$CUSIP)!=8),", missing 6-digit CUSIPs: ", sum(SDC$X6.CUSIP == ""), ", X6.CUSIPs with non-6 digits: ",sum(str_count(SDC$X6.CUSIP)!=6)) #just to see where we are now.


# This tmp_file is used by python. 
write.table(data.frame(date=SDC$Event.Date, cusip=SDC$CUSIP, cusip6=SDC$X6.CUSIP, missing8=SDC$missingCUSIP8),tmp_file,row.names=F, col.names=T, quote = F, sep=",")

# Python is only used here. It generates these 2 files:
# "../tmp_files/crsp_sdc_link_"+dataset+".csv" - used below to append the permnos to the SDC data
# "../"+dataset+"/returns/permno_for_crsp_"+dataset+".csv" - used to manually get the returns from CRSP (see step 3 in README)
# Run these two commands in python (in a shell)
#$ python cusiptopermno.py issuers cusip_digits_used_version (cusip_digits_used_version is 1 by default)
#$ python cusiptopermno.py buybacks cusip_digits_used_version (cusip_digits_used_version is 1 by default)
# or this from R (not recommended, and untested in the new version) system(paste("python dataset/process_code/cusiptopermno.py", datasetname, cusip_digits_used_version))

# Buybacks: Finished with a total of 18299 valid matches, 0 doubles and 5892 empty results.
# Issuers:  Finished with a total of 6958 valid matches, 0 doubles and 2151 empty results.

# BY THIS POINT THE CRSP DATA SHOULD HAVE BEEN ALREADY SAVED IN  x/returns/crsp_returns.csv and -  x/returns/crsp_returns_monthly.csv

# Now append the permnos to the SDC data and save it back to the global_output_file again
# Use the date-cusip-permnos created by python - check if they exist, else exit
# needs to manually run python, as explained there. If not, these files will be missing later
if (!file.exists(crsp_sdc_link_file))
  stop("Have not run Python yet: Python output files are missing")

crsp_sdc_link = read.csv(crsp_sdc_link_file,header = TRUE, stringsAsFactors= FALSE, check.names=FALSE, dec=".")
crsp_sdc_link$date <- as.Date(as.character(crsp_sdc_link$date),format="%Y%m%d",origin="19700101")

# Just add permnos and also check for any funny duplicates: they should not exist! 
tmp1 = paste(SDC$Event.Date, SDC$CUSIP)
tmp2 = paste(crsp_sdc_link[,"date"], crsp_sdc_link[,"cusip"])
idx = match(tmp1,tmp2)
#duplicates = sapply(tmp1, function(i) length(unique(crsp_sdc_link[,"permno"][tmp2==i])))
#duplicate_finds = sum(duplicates > 1)
#if (duplicate_finds!=0)
#  stop("\n**WE DO WE HAVE DUPLICATE EVENT-CUSIP PAIRS? This shouldn't happen if the python code works correctly.**\n")
SDC$permno = ifelse(!is.na(idx) , crsp_sdc_link[,"permno"][idx], NA)
rm("tmp1", "tmp2", "idx")

cleanup$get_crsp$NOPERMNO_remove <- sum(is.na(SDC$permno))
cat("\n\n*** SUMMARY: Appended", sum(!is.na(SDC$permno)), "PERMNOs to the SDC Data; ", sum(is.na(SDC$permno)), "SDC events have no PERMNO and are removed\n\n")
SDC = SDC[!is.na(SDC$permno),]

# These files needs to be generated manually from CRSP, as explained in Step 3 in README
if (!file.exists(paste("dataset/",datasetname,"/returns/crsp_returns_monthly.csv",sep="")) |
      !file.exists(paste("dataset/",datasetname,"/returns/crsp_returns.csv",sep="")))
  stop("CRSP data have not been downloaded from CRSP, as explained in Step 3 of README")

# Now clean up the tmp file - but not those created by python (which is slow to run and is manual)
file.remove(tmp_file)

#####
#save the new SDC data back to the global_output_file for the next steps to use:
save(SDC,cleanup,file = global_output_file)

