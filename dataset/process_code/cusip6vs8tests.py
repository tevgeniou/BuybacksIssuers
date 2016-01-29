#!/usr/bin/python
import sys

if(len(sys.argv) < 2):
	print "Usage: python cusiptopermno.py [buybacks|issuers]"
	exit()
dataset = str(sys.argv[1])

#Read the readme first!
import pandas as pd
import numpy as np
import timeit

# SEE README

# Do 4 versions (UPDATE BASED ON README) of matching and report the results:
# Version 1:  using the 8-digit CUSIPs for all cases 
# Version 2: using the 6-digits CUSIPs for all cases 
#         Record duplicates for the 6-digit cases, as well as how many 10s and 11s there were for these duplicates.
# Version 3: use the 8-digit cusips when they are available (see "missing8"), and the 6-digit CUSIPs for the other cases. 
#         Record duplicates for the 6-digit cases, as well as how many 10s and 11s there were for these duplicates.

# Enric: TO CHECK THESE 3 VERSIONS BELOW. THE FIRST 2 VERSIONS SHOULD BE FINE, BUT GOOD TO CHECK. 
# ALSO, LET'S COUNT HOW MANY OF THE CRPS NCUSIPS HAVE 10 AND HOW MANY HAVE 11 AT THE END - so we say "we used the majority", 
# which i guess it is 10 like for the SDC 8-digit CUSIPS

# For buybacks the results are as follows (to update once the tests are confirmed):
# Version 1: 
# Finished with a total of 18738 valid matches, 0 doubles and 5429 empty results.
# Version 2:
# Finished with a total of 19987 valid matches, 285 doubles and 3895 empty results.
# (seems the 6 digits ones find more... but how come only 285 duplicates!! this is less than 18738 - 19987. 
# Also, how many of these are 10 and how many are 11 at the end?!)
# Version 3: (haven't run it yet, needs fixing)
# 

# For issuers the results are as follows (to update once the tests are confirmed):
# Version 1: 
# Finished with a total of 7562 valid matches, 0 doubles and 2651 empty results.
# Version 2:
# Finished with a total of 7868 valid matches, 31 doubles and 2314 empty results.
# (seems the 6 digits ones find more... but how come only 31 duplicates!! this is less than 7868 - 7562
# Also, how many of these are 10 and how many are 11 at the end?!)
# Version 3: (haven't run it yet, needs fixing)
# 

# Based on the results of Versions 1 and 2 (so far - need to add version 3!), 
# currently cusiptopermno.py uses the most "conservative" version 1 (all are 8-digits CUSIPs)
# as not too many more cases are lost and we are more sure we have the right data 
# (the only "caveat" is that we added 10 and not 11 at the end of the original 6-digits cusips we had,
# which we can resolve by using version 3 and removing all duplicates...)

####################################################################################
# FIRST USE ALL 8-DIGIT CUSIPS ONLY (e.g. including those where we append a "10" at the end of the 6-digits CUSIPs)
####################################################################################

print "Version 1: RUNNING THE 8-CUSIPS VERSION NOW."

sdc = pd.read_csv("../tmp_files/sdc_"+dataset+"_cusips_python_tests.csv")
sdc.date = sdc.date.map(lambda x: int(x.replace("-","")))
crsp = pd.read_csv("../crsp_permno_cusip_pairs/crsp_permno_cusip_pairs.csv",usecols=["PERMNO","date","NCUSIP","SHRCD"])

#prefilter to speed up calculations afterwards: drop dates we're not interested in
print "Prefiltering database."
start_time = timeit.default_timer()
udates = np.unique(sdc.date)
crsp = crsp[crsp.date.isin(udates)]

#prefilter to speed up calculations afterwards: drop cusips we're not interested in
start_time = timeit.default_timer()
ucusips = np.unique(sdc.cusip)
crsp = crsp[crsp.NCUSIP.isin(ucusips)]
timeit.default_timer() - start_time

#Check how many non-10s we have
valid = sdc.cusip.map(lambda x:str(x)[6:8]) == '10'
print "There were "+str(valid.value_counts()[False])+" of the 8-digits CUSIPS that didn't end in 10"

#Caclulate codes
print "Matching (Date,PERMNO) to PERMNO."
DATE = list()
PERMNO = list()
CUSIP = list()
err_double = 0
err_empty = 0

bad_DATE = list()
bad_CUSIP = list()
for d,c in zip(sdc.date,sdc.cusip):
    tmp = crsp[crsp.date == d]
    p = tmp.PERMNO[tmp.NCUSIP == c]
    if(p.shape[0] == 1):
	PERMNO.append(int(p))
        DATE.append(d)
	CUSIP.append(c)
    else:
        if(p.shape[0] == 0):
            #print \Couldn't find CUSIP \+str(c) + \ date \+str(d)+\\
            err_empty = err_empty +1
        else:
            #print \Double match CUSIP \ +str(c)+ \ date \+str(d)+\\
            err_double = err_double+1
        bad_DATE.append(d)
        bad_CUSIP.append(c)
print "Finished with a total of "+str(len(PERMNO))+" valid matches, "+str(err_double)+" doubles and " +str(err_empty)+" empty results."

print "Done with the 8-digits CUSIPs!"

####################################################################################
# NOW USE ONLY THE 6-DIGIT CUSIPS - FOR ALL (including those for which we have the 8-digits one) 
####################################################################################

print "Version 2: RUNNING THE 6-CUSIPS VERSION NOW."
# we use cusip6 instead of cusip everywhere in this version 

sdc = pd.read_csv("../tmp_files/sdc_"+dataset+"_cusips_python_tests.csv")
sdc.date = sdc.date.map(lambda x: int(x.replace("-","")))
crsp = pd.read_csv("../crsp_permno_cusip_pairs/crsp_permno_cusip_pairs.csv",usecols=["PERMNO","date","NCUSIP","SHRCD"])

# now we use only the 6-digits, hence we also get those only from CRSP
crsp.NCUSIP = crsp.NCUSIP.map(lambda x: str(x)[0:6])

#prefilter to speed up calculations afterwards: drop dates we're not interested in
print "Prefiltering database."
start_time = timeit.default_timer()
udates = np.unique(sdc.date)
crsp = crsp[crsp.date.isin(udates)]

#prefilter to speed up calculations afterwards: drop cusips we're not interested in
start_time = timeit.default_timer()
ucusips = np.unique(sdc.cusip6)
crsp = crsp[crsp.NCUSIP.isin(ucusips)]
timeit.default_timer() - start_time

#Caclulate codes
print "Matching (Date,PERMNO) to PERMNO."
DATE = list()
PERMNO = list()
CUSIP = list()
err_double = 0
err_empty = 0

bad_DATE = list()
bad_CUSIP = list()
for d,c in zip(sdc.date,sdc.cusip6):
    tmp = crsp[crsp.date == d]
    p = tmp.PERMNO[tmp.NCUSIP == c]
    if(p.shape[0] == 1):
  PERMNO.append(int(p))
        DATE.append(d)
	CUSIP.append(c)
    else:
        if(p.shape[0] == 0):
            #print \Couldn't find CUSIP \+str(c) + \ date \+str(d)+\\
            err_empty = err_empty +1
        else:
            #print \Double match CUSIP \ +str(c)+ \ date \+str(d)+\\
            err_double = err_double+1
        bad_DATE.append(d)
        bad_CUSIP.append(c)
print "Finished with a total of "+str(len(PERMNO))+" valid matches, "+str(err_double)+" doubles and " +str(err_empty)+" empty results."

print "Done with the 6-digits CUSIPs"

####################################################################################
# NOW USE THE 8-DIGIT CUSIPS WHENEVER AVAILABLE, AND THE 6-DIGIT CUSIPS FOR THE REST (as indicated by "missing8")
####################################################################################

# THIS ONE NEEDS FIXING, TO USE THE missing8 VARIABLE.....

print "Version 3: RUNNING THE 8-CUSIPS WHEN AVAILABLE AND 6-CUSIPS OTHERWISE VERSION NOW."

sdc = pd.read_csv("../tmp_files/sdc_"+dataset+"_cusips_python_tests.csv")
sdc.date = sdc.date.map(lambda x: int(x.replace("-","")))
crsp = pd.read_csv("../crsp_permno_cusip_pairs/crsp_permno_cusip_pairs.csv",usecols=["PERMNO","date","NCUSIP","SHRCD"])

#crsp.NCUSIP = crsp.NCUSIP.map(lambda x: str(x)[0:6])

#prefilter to speed up calculations afterwards: drop dates we're not interested in
print "Prefiltering database."
start_time = timeit.default_timer()
udates = np.unique(sdc.date)
crsp = crsp[crsp.date.isin(udates)]

#prefilter to speed up calculations afterwards: drop cusips we're not interested in
start_time = timeit.default_timer()
ucusips = np.unique(sdc.cusip)
crsp = crsp[crsp.NCUSIP.isin(ucusips)]
timeit.default_timer() - start_time

#Caclulate codes
print "Matching (Date,PERMNO) to PERMNO."
DATE = list()
PERMNO = list()
CUSIP = list()
err_double = 0
err_empty = 0

bad_DATE = list()
bad_CUSIP = list()
for d,c in zip(sdc.date,sdc.cusip):
    tmp = crsp[crsp.date == d]
    p = tmp.PERMNO[tmp.NCUSIP == c]
    if(p.shape[0] == 1):
  PERMNO.append(int(p))
        DATE.append(d)
  CUSIP.append(c)
    else:
        if(p.shape[0] == 0):
            #print \Couldn't find CUSIP \+str(c) + \ date \+str(d)+\\
            err_empty = err_empty +1
        else:
            #print \Double match CUSIP \ +str(c)+ \ date \+str(d)+\\
            err_double = err_double+1
        bad_DATE.append(d)
        bad_CUSIP.append(c)
print "Finished with a total of "+str(len(PERMNO))+" valid matches, "+str(err_double)+" doubles and " +str(err_empty)+" empty results."

print "Done with the 8 and 6-digits CUSIPs version"
