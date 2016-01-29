import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

################################################################
# Code to compute quantile breakpoints
#
if 1:
	sourcefile = "../universe/crsp_returns.csv"
	outputfile = "../tmp_files/breakpoints_vol.csv"

	#Step 0: Read in the returns and convert to a matrix
	#cd "/home/enric/Dropbox/Research/Active Research/BuybacksIssuers/dataset/process_code" #for testing
	df = pd.read_csv(sourcefile,na_values=["B","C"],usecols=["PERMNO","date","RET"])
	df['RET']  = df['RET'].astype(np.float)
	df['date'] = pd.to_datetime(df.date,format="%Y%m%d")
	df = df.pivot('date','PERMNO','RET')
	#df = df.ix[:,df.columns[1:100]]

	print "The universe contains",df.shape[0],"recorded days for each of the ",df.shape[1],"companies."

	#Step 1: define breakpoints as last days of month and stop at 2013-12-31
	breakpoints = pd.date_range('31/1/1985', periods=30*12, freq="M")
	Q = np.array(range(5,100,5))/100.
	quantiles   = np.zeros([breakpoints.shape[0],len(Q)])

	#Step 2: for each of the breakpoints, calculate sd vector and extract breakpoints for the previous 6 months
	for i in range(0,len(breakpoints)):
	    b = breakpoints[i]
	    #6*21 = 6 months, -1 day = 30/29th of the month
	    window = (df.index >= (b - pd.DateOffset(21))) & (df.index <= (b - pd.DateOffset(1)))
	    #calculate the volatility as the SD per company over it's non NA entries
	    vol    = (df[window].apply(np.std)).dropna()
	    quantiles[i] = vol.quantile(Q)

	#Step 3: save the quantiles to file
	output = pd.DataFrame(quantiles)
	output.index = breakpoints
	output.to_csv(outputfile)

################################################################
# Code to compute quantile membership for buybacks
#
sourcefile = "../buybacks/returns/crsp_returns.csv"
breakfile = "../tmp_files/breakpoints_vol.csv"
outputfile = "../tmp_files/breakpoints_vol_buybacks.csv"

#for testing
sourcefile = "../universe/crsp_returns.csv"
outputfile = "../tmp_files/breakpoints_vol_universe.csv"

#Step 0: Read the saved quantile data and such
breakpoints = pd.date_range('31/1/1985', periods=30*12, freq="M")
quantiles = pd.read_csv(breakfile,index_col=0).as_matrix()

#Step 1: Read in the returns and convert to a matrix
returns = pd.read_csv(sourcefile,na_values=["B","C"],usecols=["PERMNO","date","RET"])
returns['RET']  = returns['RET'].astype(np.float)
returns['date'] = pd.to_datetime(returns.date,format="%Y%m%d")
returns = returns.pivot('date','PERMNO','RET')

print "The buybacks dataset contains",returns.shape[0],"recorded days for each of the ",returns.shape[1],"companies."

#Step 2: Calculate in which percentile each company falls
company_q = np.zeros([breakpoints.shape[0],returns.shape[1]])
for i in range(0,len(breakpoints)):
    b = breakpoints[i]
    window = (returns.index >= b - pd.DateOffset(6*21)) & (returns.index <= (b - pd.DateOffset(1)))
    vol    = (returns[window].apply(np.std))
    #assign correct quantile to each company by assign an ever increasing quantile as long as it passes the pct check
    company_q[i] = 1 #start at quantile 1
    for j in range(0,len(quantiles[i])):
      company_q[i][np.where(vol > quantiles[i][j])] = j+2 #+2 to make quanties go from 2-20 for R
    company_q[i][np.where(np.isnan(vol))] = np.NaN

#Step 3: save to disk
output = pd.DataFrame(company_q)
output.index = breakpoints
output = output.fillna(-1)
output = output.astype(int)
output.columns = returns.columns
#output[output == -1] = np.NaN
output.to_csv(outputfile,)

#Quick checks
if 0:
  plt.plot(np.cumsum(comp_ret))
  fname = "/home/enric/Dropbox/Research/Active Research/BuybacksIssuers/dataset/buybacks/sdc/  sdc_output_buybacks.csv"
  df2 = pd.read_csv(fname,usecols=["CUSIP"],sep=";")

  #2000 drop here
  len(np.intersect1d(np.unique(df2.CUSIP),ucusip))
  #There should be 5K events dropping
  ucusip = np.unique(df.NCUSIP)
  tmp =  [ (u in ucusip)  for u in df2.CUSIP]
  np.sum(tmp)
