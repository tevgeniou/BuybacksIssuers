############################################################################################ 
# All global parameters of the project
############################################################################################ 

##########################################################################################
### Parameters for the "business_data_clean" of the data

# business_data_clean.R parameters
First = as.Date("1976-01-01")
Last = as.Date("2014-12-31")
# BUT THIS IS THE FIRST DAY WE TRADE ANY PORTFOLIOS IN THE PORTFOLIOS SECTION: 
# we need some time to get enough live evnents and to hedge
FirstTrade = as.Date("1985-01-01")
# Dates used for the events we consider
AbsoluteMinimumDate = as.Date("1976-01-01")

periods_considered = rbind(
  c("1985-01-01", "1990-12-31"),
  c("1991-01-01", "2000-12-31"),
  c("2001-01-01", "2014-12-31"),
  c("2008-01-01", "2014-12-31")
)
finance_period = "2001-01-01"

penny_stock_price_recent = 3 # penny stock threshold price - post 1990
penny_stock_price_old = 1 # penny stock threshold price - pre 1990
good_currencies = c("US")
major_markets = c("N", "NM", "NY", "NK",      "New York", "Nasdaq", "NYSE Amex", "American")

MIN_SIZE = 0 #micro caps are those with a market cap <300m
MAX_SIZE = 1e20 #already in million (e6)
MIN_ME_quantile = 0 # the minimum ff ME quantile
MAX_ME_quantile = 20 # the maximum ff ME quantile
MIN_BEME_quantile = 0 # the minimum ff BE/ME quantile
MAX_BEME_quantile = 20 # the maximum ff BE/ME quantile
MIN_EVENT_SIZE = 0 # Required minimum "event size" (e.g. % of shares repurchased/issued)
MAX_EVENT_SIZE = 50 # not allow more than this percent of shares for the event

days_live = 0 # minimum number of days a stock needs to be live before announcement date
remove_returns_outliers = 0 # remove the reutrns outliers?
MAX_DR_abs = 100 # Max absolute daily return to consider a stock is an outliers
max_vol_outlier = 100000.1 # max daily standard deviation in 6 months before announcement

MINIMUM_PERIOD_SINCE_LAST_EVENT = 0# we require that there were at least that many days before the previous event

CROSS_EVENTS_SINCE_LAST_EVENT = 4*365 # we require that there were at most that many days before the previous event
minCROSS_EVENTS_SINCE_LAST_EVENT = 0 # we require that there were at least that many days before the previous event

BB_allowed_techniques = c("OP", "OPNG","") # Which Buybacks techniques are allowed (note that "" is needed to keep those for 
# which we do not have the technique - e.g. all buyback events from the SDC M&A database)
#       OP OPDA OPNG OPOL 
#5341 6922    1 6314    4 
ISS_allowed_techniques = c("FIRMCOMMITMENT+NEGOTIATEDSALE", "NEGOTIATEDSALE+FIRMCOMMITMENT",
                           "FIRMCOMMITMENT", "AUCTION+NEGOTIATEDSALE", "NEGOTIATEDSALE",
                           "FIRMCOMMITMENT+FIRMCOMMITMENT+NEGOTIATEDSALE",
                           "PLACEMENT","FIRMCOMMITMENT+AUCTION",
                           "FIRMCOMMITMENT+NEGOTIATEDSALE+OFFERSAL",
                           "NEGOTIATEDSALE+OFFERSALE+FIRMCOMMITMENT"
                           ) # Which SEO techniques are allowed 

##############################################################################

IBES_PRE_ANOUNCE_DATES = 1 # we require for any IBES announcement/forecast to be at least that many days before the event 
# This is used in merge_dataset.R, hence it needs to be rerun if this parameters is modified

##########################################################################################
# U-index parameters

Performance_to_use = "Absolute" # "Absolute" or "Abnormal" what to use for the pre-event performance
Performance_to_use_pre_event = "Six.Month.Before" # number of days to use pre-event for the performance

quantile_R2 = 0.2 # quantile for high and low idiosyncratic risk companies
quantile_VOL = 0.2 # quantile for high and low idiosyncratic risk companies
quantile_LEV = 0.25 # quantile for high and low idiosyncratic risk companies
quantile_EPS = 0.25 # quantile for high and low idiosyncratic risk companies
R2window = 180 # rolling window to get the R2 thresholds
VOLwindow = 180 # rolling window to get the R2 thresholds
LEVwindow = 180 # rolling window to get the R2 thresholds
EPSwindow = 180 # rolling window to get the R2 thresholds

years_across_cross_events = 4 # for BB then ISS or vice verca years.

randomizationiterations = 10 # how many times to run the volatility randomization test

##########################################################################################
# Industries and industry filter

All = 1:9999
source("dataset/indices_and_factors/ff_industries_sic.R")
Finance1 = 6000:6799
# Select the industries to use
Industries = All
INDUSTRY_USED = setdiff(All,union(union(FF_industries$Trading, FF_industries$Banking),FF_industries$Utilities)) # what industry to check in the sector sensitivity part of the paper (only used there)
INDUSTRY_FINANCIALS = union(FF_industries$Trading, FF_industries$Banking) # what industry to check in the sector sensitivity part of the paper (only used there)

min_industry_sampleBB = 300

finance_three_factor_model = "(ri - RF) ~ Finance + SMB + HML"
finance_five_factor_model = "(ri - RF) ~ Finance + SMB + HML + RMW + CMA"
finance_five_factor_model2 = "(ri - RF) ~ Delta + Finance + SMB + HML + RMW + CMA"
three_factor_model = "(ri - RF) ~ Delta + SMB + HML"
four_factor_model = "(ri - RF) ~ Delta + SMB + HML + Mom"
five_factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA"
six_factor_model = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA + Mom"
three_factor_model_finance = "(ri - RF) ~ Delta + SMB + HML + Finance"
four_factor_model_finance = "(ri - RF) ~ Delta + SMB + HML + Mom + Finance"
five_factor_model_finance = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA + Finance"
six_factor_model_finance = "(ri - RF) ~ Delta + SMB + HML + RMW + CMA + Mom + Finance"

reported_times = c("-6","+12","+24","+36","+48","Observations")
reported_times_post= c("+12","+24","+36","+48","Observations")
allmonths = (-6):48
reported_times_plots = ifelse(allmonths > 0, paste("+",allmonths,sep=""), allmonths)
plot_tickers= c(which(reported_times_plots=="-6"),which(reported_times_plots=="0"),which(reported_times_plots=="+12"),which(reported_times_plots=="+24"),which(reported_times_plots=="+36"),which(reported_times_plots=="+48"))
plot_xlabels = c("-6","0","+12","+24","+36","+48")

pnl_hedge_factors = c("RF","Delta","SMB","HML","RMW","CMA") # for the "strategy" hedge

##########################################################################################
# Parameters for trading and P&L performances
#Trading.Day = Event_Date + trading_day_after_announce
#One.Day.Before <- Event_Date - 1
#One.Month.Before <- One.Day.Before - 30
#Three.Month.Before <- One.Day.Before - 90
#Six.Month.Before <- One.Day.Before - 180
#One.Month.After <- 30 + Trading.Day 
#Three.Month.After <- 90 + Trading.Day 
#Six.Month.After <- 180 + Trading.Day
#One.Year.After <- 365 + Trading.Day
#Two.Years.After <- 2*365 + Trading.Day
#Three.Years.After <- 3*365 + Trading.Day
#Four.Years.After <- 4*365 + Trading.Day

trading_day_after_announce = 1 
start_date_event = "Trading.Day" # see above. this needs some cleanup as we don't need all these dates any more. but ok for now, as it is not slow to do is as is. keep it as is for future options
end_date_event = "Three.Month.After"
holding_period_text = gsub("\\.", " ", end_date_event)
hedge_days = 250
hedge_months = 18
rolling_vol_days = 12 # For the vo-based combination of issuers and buybacks portfolios
prior_performance_period_days = 180
# Just for the .rnw....
Performance_to_use_text = ifelse(Performance_to_use == "Abnormal", "5-factor abnormal", "absolute")

###

RECESSION_YEARS = list( #http://www.nber.org/cycles.html
  nineties = as.Date(c("1990-07-01", "1991-03-01")),
  dotcom = as.Date(c("2001-03-01", "2001-11-01")),
  financial = as.Date(c("2007-12-01", "2009-07-01")))

BEAR_YEARS = list( #http://www.gold-eagle.com/article/history-us-bear-bull-markets-1929
#  dotcom1 = as.Date(c("2000-03-24", "2001-09-21")),
#  dotcom2 = as.Date(c("2002-01-04", "2002-10-09")),
#  financial1 = as.Date(c("2007-10-09", "2008-11-20")),
#  financial2 = as.Date(c("2009-01-06", "2009-03-09"))
  dotcom = as.Date(c("2000-03-24", "2002-10-09")),
  financial1 = as.Date(c("2007-10-09", "2009-03-09"))
)

BEAR_YEARS_text = list( #http://www.gold-eagle.com/article/history-us-bear-bull-markets-1929
  #  dotcom1 = as.Date(c("2000-03-24", "2001-09-21")),
  #  dotcom2 = as.Date(c("2002-01-04", "2002-10-09")),
  #  financial1 = as.Date(c("2007-10-09", "2008-11-20")),
  #  financial2 = as.Date(c("2009-01-06", "2009-03-09"))
  dotcom = c("March 2000", "October 2002"),
  financial1 = c("October 2007", "March 2009")
)

CRISIS_SLACK_pre = 0 # in case we want to add some window around the recession years
CRISIS_SLACK_post = 0 # in case we want to add some window around the recession years

Compustat_Lag = 1 # number of months before the event that we would like to get various OTHER Compustat data from

