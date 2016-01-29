import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

sourcefile = "../universe/crsp_returns.csv"
outputfile = "../tmp_files/breakpoints_vol.csv"

#Step 0: Read in the returns and convert to a matrix
print "Reading the universe data."
#cd "/home/enric/Dropbox/Research/Active Research/BuybacksIssuers/dataset/process_code/" #for testing
df = pd.read_csv(sourcefile,na_values=["B","C"],usecols=["PERMNO","date","RET","NCUSIP"])
df['RET']  = df['RET'].astype(np.float)
df['date'] = pd.to_datetime(df.date,format="%Y%m%d")
df = df.pivot('date','PERMNO','RET')

print "The universe contains",df.shape[0],"recorded days for each of the ",df.shape[1],"companies."

#Step 1: define breakpoints as last days of month and stop at 2013-12-31
breakpoints = pd.date_range('31/1/1986', periods=28*12, freq="M")
Q           = np.array(range(5,100,5))/100.
quantiles   = np.zeros([breakpoints.shape[0],len(Q)])

print "Calculating breakpoints ..."
#Step 2: for each of the breakpoints, calculate sd vector and extract breakpoints for the previous 6 months
for i in range(0,len(breakpoints)):
    b = breakpoints[i]
    #6*21 = 6 months, -1 day = 30/29th of the month
    window = (df.index >= b - pd.DateOffset(6*21)) & (df.index <= (b - pd.DateOffset(1)))
    #calculate the volatility as the SD per company over it's non NA entries
    vol    = (df[window].apply(np.std)).dropna()
    quantiles[i] = vol.quantile(Q)

#Step 3: save the quantiles to file
output = pd.DataFrame(quantiles)
output.index = breakpoints
output.to_csv(outputfile)

print "Done, quitting!"

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
