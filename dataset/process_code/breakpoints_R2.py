import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import statsmodels.api as sm
import time

#cd "/home/enric/Dropbox/Research/Active Research/BuybacksIssuers/dataset/process_code" #for testing

################################################################
# Code to compute quantile breakpoints
#

sourcefile = "../universe/crsp_returns.csv"
factorfile = "../indices_and_factors/ff_research_data_5_factors_2x3_daily.csv"
outputfile = "../tmp_files/matrix_R2.csv"

#Step 0A: Read in the returns and convert to a matrix
df = pd.read_csv(sourcefile,na_values=["B","C"],usecols=["PERMNO","date","RET"])
df['RET']  = df['RET'].astype(np.float)
df['date'] = pd.to_datetime(df.date,format="%Y%m%d")
df = df.pivot('date','PERMNO','RET')

#TESTING ONLY:
#df = df.ix[:,df.columns[0:1000]]

print "The universe contains",df.shape[0],"recorded days for each of the ",df.shape[1],"companies."

#Step 0B: read the FF factors data
factors = pd.read_csv(factorfile,sep=r"\s*",engine="python",index_col="date")
factors.index= pd.to_datetime(factors.index,format="%Y%m%d")
factors = sm.add_constant(factors)

#Step 1: define breakpoints as last days of month and stop at 2013-12-31
breakpoints = pd.date_range('31/1/1985', periods=30*12, freq="M")
#breakpoints = pd.date_range('31/1/2000', periods=15*12, freq="M") #TESTING ONLY

#Create & align X/y vector
nz_sum = lambda x : np.sum(x[~pd.isnull(x)])
y = df.apply(nz_sum,1)
X = sm.add_constant(factors)
idx = np.intersect1d(y.index,X.index)
y = y[idx]
X = X.ix[idx,]
df = df.ix[idx,]

#Calculation 1: find R2 for the sum of the universe as check
def find_R2(breakpoint):
	window = (y.index >= (breakpoint - pd.DateOffset(6*21))) & (y.index <= (breakpoint - pd.DateOffset(1)))
	yi = (y[window] - X.RF[window])
	Xi = X.ix[window,["Delta","SMB","HML","RMW","CMA"]]
	model = sm.OLS(yi, Xi)
	R2 = model.fit().rsquared
	return R2
R2 = [ find_R2(breakpoint) for breakpoint in breakpoints]
print np.mean(R2) #.93 this is OK

#clear up some memory
factors = None
y = None


#Calculation 2: find R2 for each of the individual companies throughout time
R2 = np.zeros([breakpoints.shape[0],df.shape[1]])
nz_sum = lambda x : np.sum(x[~pd.isnull(x)])
def find_one_R2(company,window):
	yi = (df.ix[window,df.columns[company]] - X.RF[window])
	Xi = X.ix[window,["Delta","SMB","HML","RMW","CMA"]]
	goodidx  = ~pd.isnull(yi) #ignore nan
	if(np.sum(goodidx) > 21): #only use if we have more than 1 month of valid data
		model = sm.OLS(yi, Xi)
		R2 = model.fit().rsquared
		return R2
	else:
		return np.nan

tstart = time.time()		
for i in range(0,breakpoints.shape[0]):
	breakpoint = breakpoints[i]
	window     = (X.index >= (breakpoint - pd.DateOffset(6*21))) & (X.index <= (breakpoint - pd.DateOffset(1)))
	R2[i,:] = [ find_one_R2(x, window) for x in range(0,df.shape[1])]
	
	#some timing information
	tdelta = round((time.time() - tstart)/60.,0)
	ttogo  = round((tdelta*1.0 / (i+1))* (breakpoints.shape[0]-(i+1)),0)
	print str(round(i *100.0/ breakpoints.shape[0],2)) +"%, "+str(tdelta)+"m passed, "+str(ttogo)+"m to go"

#Step 3: save the quantiles to file
output = pd.DataFrame(R2)
output.index = breakpoints
output.to_csv(outputfile)
