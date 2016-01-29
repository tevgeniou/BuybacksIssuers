#!/usr/bin/python
#Read the readme first!
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" INPUT FILES
"
" ../crsp_permno_cusip_pairs/crsp_permno_cusip_pairs.csv (from CRSP, see Step 2 of README)
" ../tmp_files/sdc_x_cusips_for_python.csv (from get_crsp_data.R)
"
" OUTPUT FILES
"
" ../tmp_files/crsp_sdc_link_x.csv" (to be used by merge_dataset.R - step 6 of README)
"  x/permno_for_crsp_x.csv" (to be used to get CRSP returns data - Step 3 of README)
" ../tmp_files/python_log_x_y.csv (with x the dataset and y the mode)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

import sys
import pandas as pd
import numpy as np
import timeit

mode = 1
if(len(sys.argv) < 2 or (sys.argv[1] !="buybacks" and sys.argv[1] != "issuers" )):
	print "Usage: python cusiptopermno.py [buybacks|issuers] [1-4]"
	print "\tMode 1: use C8 when available, append 10 to C6 otherwise"
	print "\tMode 2: use C8 when available, use C6 when not"
	print "\tMode 3: use only C6 digits for all"
	print "\tMode 4: same as mode 2, but remove C8 that don't end on 10/11"	
	exit()
elif(len(sys.argv) > 2):
	mode = int(sys.argv[2])
dataset = str(sys.argv[1])

print "Processing dataset "+dataset+" in mode "+str(mode)+"."
print "Reading PERMNO-CUSIP-DATE universe database."
crsp = pd.read_csv("../crsp_permno_cusip_pairs/crsp_permno_cusip_pairs.csv",usecols=["PERMNO","date","NCUSIP"])
sdc = pd.read_csv("../tmp_files/sdc_"+dataset+"_cusips_for_python.csv")
sdc.date = sdc.date.map(lambda x: int(str(x).replace("-","")))

flog = open("../tmp_files/python_log_"+dataset+"_"+str(mode)+".txt", 'w')
print >> flog, 'Universe size :',str(np.shape(crsp))
print >> flog, 'SDC data size :',str(np.shape(sdc))

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

# Useful for matching in mode 2,3
crsp["NCUSIP6"] = crsp.NCUSIP.map(lambda x: str(x)[0:6])
lastdigit = sdc.cusip.map(lambda x:str(x)[6:8])
sdc["valid8"]   = (lastdigit == '10') | (lastdigit == '11')
print >> flog, 'SDC invalid CUSIP8 (not 10/11):',str(sum(sdc.valid8 == 0))


#Caclulate codes
print "Matching (Date,PERMNO) to PERMNO."
DATE = list()
PERMNO = list()
CUSIP = list()
err_double = 0
err_empty = 0

bad_DATE = list()
bad_CUSIP = list()
#d,c6,c8,missing8,valid8 = zip(sdc.date,sdc.cusip6,sdc.cusip,sdc.missing8,sdc.valid8)[1]
for d,c6,c8,missing8,valid8 in zip(sdc.date,sdc.cusip6,sdc.cusip,sdc.missing8,sdc.valid8):
    tmp = crsp[crsp.date == d]

    #the exact matching depends on which mode is being used
    cusip = c8
    found = True
    if(mode == 1):
        p = tmp.PERMNO[tmp.NCUSIP == c8]
    elif(mode == 2):
	if(missing8):
            p = tmp.PERMNO[tmp.NCUSIP == c8]
        else:
            cusip = c6
            p = tmp.PERMNO[tmp.NCUSIP6 == c6]
    elif(mode == 3):
        cusip = c6
        p = tmp.PERMNO[tmp.NCUSIP6 == c6]
    elif(mode == 4):
        if(valid8):
            p = tmp.PERMNO[tmp.NCUSIP == c8]
	else:
	    found = False
    else:
	print "No mode selected"
	exit()
    if(found and p.shape[0] == 1):
	PERMNO.append(int(p))
        DATE.append(d)
	CUSIP.append(cusip)
    else:
        if(not found or p.shape[0] == 0):
            err_empty = err_empty +1
        else:
            err_double = err_double+1
        #bad_DATE.append(d)
        #bad_CUSIP.append(cusip)
print "Finished with a total of "+str(len(PERMNO))+" valid matches, "+str(err_double)+" doubles and " +str(err_empty)+" empty results."

print >> flog, 'Valid CUSIP matches:',str(len(PERMNO))
print >> flog, 'Doubles:',str(err_double)
print >> flog, 'Empty lookup:',str(err_empty)

print "Saving files."
out = pd.DataFrame()
out["date"] = DATE
out["cusip"] = CUSIP
out["permno"] = PERMNO
upermno = pd.DataFrame(np.unique(out["permno"]))

######
## Write the outpu csv files now

out.to_csv("../tmp_files/crsp_sdc_link_"+dataset+".csv")
print "../tmp_files/crsp_sdc_link_"+dataset+".csv saved."
upermno.to_csv("../"+dataset+"/returns/permno_for_crsp_"+dataset+".csv",index=False)
print "../"+dataset+"/returns/permno_for_crsp_"+dataset+".csv saved"

flog.close()
print "Done!"

