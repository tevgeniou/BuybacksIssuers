
##################################################################
# INPUT files needed

# From CRSP:
# 1. dataset/indices_and_factors/snp500.csv
# 2. dataset/indices_and_factors/snp500_monthly.csv

# From http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html 
# 1. dataset/indices_and_factors/ff_research_data_5_factors_2x3_daily.CSV
# 2. dataset/indices_and_factors/ff_research_data_5_factors_2x3.CSV
# 3. dataset/indices_and_factors/me_breakpoints.csv
# 4. dataset/indices_and_factors/be_me_breakpoints.csv

##################################################################
# OUTPUT files 

#1. dataset/indices_and_factors/market_ff_data.Rdata
##################################################################

##################################################################
# Market for trading days only 
##################################################################

#Daily index data using CRSP, S.P.500 only, did not double check these with the rest of the code!
Indices_Daily <- read.csv("dataset/indices_and_factors/snp500.csv", header=TRUE, sep=",", dec=".")
rownames(Indices_Daily) <- as.Date(as.character(Indices_Daily$DATE),format="%Y%m%d")
# These are the trading days we will use throughout
trading.days <- rownames(Indices_Daily)[isBizday(as.timeDate(rownames(Indices_Daily)), holidayNYSE(), wday = 1:5)]
trading.days = setdiff(trading.days,c("2001-09-11", "2012-10-29" ))
Indices_Daily = Indices_Daily[trading.days,]
Market_Daily <- structure(Indices_Daily$sprtrn, .Names = rownames(Indices_Daily) )

#Monthly version of indices: S.P.500 only
Indices_Monthly <- read.csv("dataset/indices_and_factors/snp500_monthly.csv", header=TRUE, sep=",", dec=".")
rownames(Indices_Monthly) <- as.Date(as.character(Indices_Monthly$DATE),format="%Y%m%d")
Market_Monthly <- structure(Indices_Monthly$sprtrn, .Names = rownames(Indices_Monthly)) 

####################################################################################
# Fama-french risk factors, see alpha_lm in lib_helpers.R for the correct format!
####################################################################################

Risk_Factors = read.table("dataset/indices_and_factors/ff_research_data_5_factors_2x3_daily.csv",header=TRUE)
row.names(Risk_Factors) <- as.Date(as.character(Risk_Factors$date), "%Y%m%d")
Risk_Factors$date <- NULL
Risk_Factors <- Risk_Factors/100 # 1 is 100%

# Fama-french monthly risk factors, used a.o. in lib_helpers.R car_table()
ff_monthly = read.table("dataset/indices_and_factors/ff_research_data_5_factors_2x3.csv",header=TRUE)
row.names(ff_monthly) <- as.Date(paste(ff_monthly$date,"01",sep=""),format="%Y%m%d")
ff_monthly$date <- NULL
Risk_Factors_Monthly <- ff_monthly
Risk_Factors_Monthly <- Risk_Factors_Monthly/100

### breakpoints
returnsbreakpoints = read.table("dataset/indices_and_factors/returns_breakpoints.csv",header=FALSE, sep=",")
# Make them all be June 30, as that is when we get the BE/ME from the companies to compare with the breakpoints
rownames(returnsbreakpoints) <- as.Date(paste(returnsbreakpoints[,1],"01",sep=""), format="%Y%m%d")
returnsbreakpoints <- returnsbreakpoints[,3:ncol(returnsbreakpoints)] # remove the date and # of companies <=0 and > 0 columns now
colnames(returnsbreakpoints) <- paste("quantilefive",1:ncol(returnsbreakpoints), sep=".")

MEbreakpoints = read.table("dataset/indices_and_factors/me_breakpoints.csv",header=FALSE, sep=",")
row.names(MEbreakpoints) <- as.Date(paste(MEbreakpoints[,1],"01",sep=""), "%Y%m%d")
MEbreakpoints <- MEbreakpoints[,3:ncol(MEbreakpoints)] # remove the date and # of companies columns now
colnames(MEbreakpoints) <- paste("quantilefive",1:ncol(MEbreakpoints), sep=".")

BE.MEbreakpoints = read.table("dataset/indices_and_factors/be_me_breakpoints.csv",header=FALSE, sep=",")
# Make them all be June 30, as that is when we get the BE/ME from the companies to compare with the breakpoints
row.names(BE.MEbreakpoints) <- as.Date(paste(BE.MEbreakpoints[,1],"0630",sep=""), format="%Y%m%d")
BE.MEbreakpoints <- BE.MEbreakpoints[,4:ncol(BE.MEbreakpoints)] # remove the date and # of companies <=0 and > 0 columns now
colnames(BE.MEbreakpoints) <- paste("quantilefive",1:ncol(BE.MEbreakpoints), sep=".")

# Industry returns from FF
industry_monthly = read.table("dataset/indices_and_factors/industryvalueweighted.csv",header=TRUE,sep=",")
row.names(industry_monthly) <- as.Date(paste(industry_monthly[,1],"01",sep=""),format="%Y%m%d")
industry_monthly = industry_monthly[,2:50]/100
industry_monthly[abs(industry_monthly) > 0.99] <- 0
Finance_factor = matrix(apply(industry_monthly[,c("Banks", "Fin")],1,non_zero_mean), ncol=1)
colnames(Finance_factor) <- "Finance"
industry_monthly = cbind(industry_monthly,Finance_factor)

mom_monthly = read.table("dataset/indices_and_factors/ff_momentum.csv",header=TRUE,sep=",")
row.names(mom_monthly) <- as.Date(paste(mom_monthly[,1],"01",sep=""),format="%Y%m%d")
mom_monthly = mom_monthly[,2:2,drop=F]/100


### NOTE: WE DATE ALIGN ALL DATA IN load_data.R. BUT:
#### JUST DO SOME FIRST DATE ALIGNMENT HERE FOR THE MONTHLY DATA

useonly = Reduce(intersect,list(str_sub(names(Market_Monthly),start=1,end=7), str_sub(rownames(Risk_Factors_Monthly),start=1,end=7), str_sub(rownames(MEbreakpoints),start=1,end=7),str_sub(rownames(industry_monthly),start=1,end=7), str_sub(rownames(mom_monthly),start=1,end=7)))
Market_Monthly = Market_Monthly[which(str_sub(names(Market_Monthly),start=1,end=7) %in% useonly)]
Risk_Factors_Monthly = Risk_Factors_Monthly[which(str_sub(rownames(Risk_Factors_Monthly),start=1,end=7) %in% useonly),,drop=F]
returnsbreakpoints = returnsbreakpoints[which(str_sub(rownames(returnsbreakpoints),start=1,end=7) %in% useonly),,drop=F]
MEbreakpoints = MEbreakpoints[which(str_sub(rownames(MEbreakpoints),start=1,end=7) %in% useonly),,drop=F]
industry_monthly = industry_monthly[which(str_sub(rownames(industry_monthly),start=1,end=7) %in% useonly),,drop=F]
mom_monthly = mom_monthly[which(str_sub(rownames(mom_monthly),start=1,end=7) %in% useonly),,drop=F]

# NOW ADD ALL POSSIBLE RISK FACTORS HERE
Risk_Factors_Monthly = cbind(Risk_Factors_Monthly, mom_monthly, industry_monthly)

# Now save all this data
save(Market_Daily,Market_Monthly,Risk_Factors,Risk_Factors_Monthly,returnsbreakpoints, MEbreakpoints,BE.MEbreakpoints, mom_monthly,industry_monthly,file="dataset/indices_and_factors/market_ff_data.Rdata")
