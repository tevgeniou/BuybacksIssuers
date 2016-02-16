
##########################################################################################
# Creates a small subset of the data only used for the interactive tool - to load faster
##########################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch

source("lib_helpers.R") 
source("latex_code.R")
source("dataset/indices_and_factors/ff_industries_sic.R")
source("business_data_clean.R")
source("Paper_global_parameters.R")
initial_vars = ls(all = TRUE) # takes time to save and load!!! so we save only what is needed at the end. 

load("tmpfiles/bb_issuersALL.Rdata")
rm("buybacks_ma", "buybacks_repurchases", "CRSP_prc", "CRSP_returns_monthly","CRSP_returns",
   "CRSP_prc_monthly","idx_Compustat_availBE","LEV_randomization",
   "crsp_sdc_link", "CRSP_shrout_monthly","fundamental_annual",
   "ibes","ibes_data","ibes_eps",
   #"ibes_sIs",
   "ibes_cusip_permno_from_python","Indices_Daily",
   "BE.MEbreakpoints","BEMEthres","all_compustat_data","all_characteristics",
   "MEbreakpoints","MEthres","mom_monthly","noBBdelistings_all","pension_annual",
   "returns_by_event","returns_by_event_monthly","SDCstandardised","SDC",
   "backfilled","BE_used_all","compustat_data","dataset","Market_Daily","previous",
   "R2_randomization","tmp_test1","VOL_randomization","x","EPSunc_randomization")


Betas_PB6M <- Betas_lm("Six.Month.Before","One.Day.Before",BUYBACK_DATA$DATASET$Dates, BUYBACK_DATA$DATASET$returns_by_event, Risk_Factors)
rm("Risk_Factors")

BUYBACK_DATA$DATASET$SDC$ivol <- Betas_PB6M["IVOL",]
BUYBACK_DATA$DATASET$SDC$marketbeta <- Betas_PB6M["Delta",]
BUYBACK_DATA$DATASET$SDC$SMBbeta <- Betas_PB6M["SMB",]
BUYBACK_DATA$DATASET$SDC$HMLbeta <- Betas_PB6M["HML",]
BUYBACK_DATA$DATASET$SDC$RMWbeta <- Betas_PB6M["RMW",]
BUYBACK_DATA$DATASET$SDC$CMAbeta <- Betas_PB6M["CMA",]
BUYBACK_DATA$DATASET$SDC$alpha <- Betas_PB6M["alpha",]
BUYBACK_DATA$DATASET$SDC$one_minus_Rsq_returns <- 1-Betas_PB6M["Rsq",]

Event_Industries <- suppressWarnings(scrub(as.numeric(BUYBACK_DATA$DATASET$SDC$Industry))) # 9 industries are 499A or 619A or 619B.. they are removed
BUYBACK_DATA$DATASET$SDC$Industry<- sapply(Event_Industries, function(i){
  x=as.numeric(i)
  tmp = sapply(1:length(FF_industries), function(j) x %in% FF_industries[[j]])
  ifelse(sum(tmp!=0), names(FF_industries)[which(tmp!=0)], "Strange")
})


BUYBACK_DATA_TOOL<-new.env()
with(BUYBACK_DATA_TOOL,{
  
  DATASET = list()
  DATASET$returns_by_event_monthly  = BUYBACK_DATA$DATASET$returns_by_event_monthly
  DATASET$DatesMonth = BUYBACK_DATA$DATASET$DatesMonth
  DATASET$ibes = BUYBACK_DATA$DATASET$ibes
  DATASET$compustat_data = BUYBACK_DATA$DATASET$compustat_data
  
  SDC = data.frame(
    
    Otherlater = BUYBACK_DATA$DATASET$SDC$Otherlater,
    OtherlaterEvent = BUYBACK_DATA$DATASET$SDC$OtherlaterEvent,
    Source...of..Funds..Code = BUYBACK_DATA$DATASET$SDC$Source...of..Funds..Code,
    Purpose.Code = BUYBACK_DATA$DATASET$SDC$Purpose.Code,
    
    Prior.Returns.Score = BUYBACK_DATA$DATASET$SDC$Performance_used, 
    BEME_used = BUYBACK_DATA$DATASET$SDC$BEME_used,# Just used this with a different in report.rnw 
    BE.ME.Score = BUYBACK_DATA$DATASET$SDC$BEME_used,
    Size.Score = BUYBACK_DATA$DATASET$SDC$Size_used,
    Valuation.Index = BUYBACK_DATA$Valuation_Index,
    
    Event.Size = BUYBACK_DATA$DATASET$SDC$Event.Size,
    Event.Date = BUYBACK_DATA$DATASET$SDC$Event.Date,
    Year = format(BUYBACK_DATA$DATASET$SDC$Event.Date,"%Y"),
    Industry = BUYBACK_DATA$DATASET$SDC$Industry,
    Market.Cap = BUYBACK_DATA$DATASET$SDC$Market.Cap,
    Percent.Shares = BUYBACK_DATA$DATASET$SDC$Event.Size,
    Purpose.Code = BUYBACK_DATA$DATASET$SDC$Purpose.Code,
    Source.of.funds = BUYBACK_DATA$DATASET$SDC$Source...of..Funds..Code,
    
    Market.beta = BUYBACK_DATA$DATASET$SDC$marketbeta,
    SMB.beta = BUYBACK_DATA$DATASET$SDC$SMBbeta,
    HML.beta = BUYBACK_DATA$DATASET$SDC$HMLbeta,
    RMW.beta = BUYBACK_DATA$DATASET$SDC$RMWbeta,
    CMA.beta = BUYBACK_DATA$DATASET$SDC$CMAbeta,
    
    minus_Rsq_returns = BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns,
    Rsq_returns = BUYBACK_DATA$DATASET$SDC$Rsq_returns,
    one.minus.Rsq.Score = BUYBACK_DATA$DATASET$SDC$minus_Rsq_returns,
    pre_vol = BUYBACK_DATA$DATASET$SDC$pre_vol,
    pre_vol_Score = BUYBACK_DATA$DATASET$SDC$pre_vol_Score, # Just used this with a different in report.rnw 
    pre.vol.Score = BUYBACK_DATA$DATASET$SDC$pre_vol_Score,
    ivol = 100*BUYBACK_DATA$DATASET$SDC$ivol,
    
    pre_lev = BUYBACK_DATA$DATASET$SDC$pre_lev, # Just used this with a different in report.rnw
    leverage.ratio = BUYBACK_DATA$DATASET$SDC$pre_lev,
    
    EPS_unc = BUYBACK_DATA$DATASET$SDC$EPS_unc,
    EPS.Value = BUYBACK_DATA$DATASET$SDC$EPS.Value,
    EPS.Forecast = BUYBACK_DATA$DATASET$SDC$EPS.Forecast,
    
    company_subset_undervalued = BUYBACK_DATA$company_subset_undervalued,
    company_subset_overvalued = BUYBACK_DATA$company_subset_overvalued,
    
    ivol = 100*BUYBACK_DATA$DATASET$SDC$ivol,
    
    Issue.later = BUYBACK_DATA$DATASET$SDC$Otherlater,
    Issue.before = BUYBACK_DATA$DATASET$SDC$Otherbefore
    
  )
  
  DATASET$SDC = SDC
  rm("SDC")
  
})
BUYBACK_DATA$DATASET = BUYBACK_DATA_TOOL$DATASET
rm("BUYBACK_DATA_TOOL")

###
ISSUERS_DATA_TOOL<-new.env()
with(ISSUERS_DATA_TOOL,{
  finance_Size_used = ISSUERS_DATA$finance_Size_used
  Valuation_Index = ISSUERS_DATA$Valuation_Index
  DATASET = list()
  DATASET$returns_by_event_monthly  = ISSUERS_DATA$DATASET$returns_by_event_monthly
  DATASET$DatesMonth = ISSUERS_DATA$DATASET$DatesMonth
  
  SDC = data.frame(
    Otherlater = ISSUERS_DATA$DATASET$SDC$Otherlater,
    Event.Date = ISSUERS_DATA$DATASET$SDC$Event.Date,
    
    Market.Cap = ISSUERS_DATA$DATASET$SDC$Market.Cap
  )
  DATASET$SDC = SDC
  rm("SDC")
})
ISSUERS_DATA = ISSUERS_DATA_TOOL
rm("ISSUERS_DATA_TOOL")

###

save(list = setdiff(ls(all = TRUE),initial_vars), file = "bb_issuersTOOL.Rdata")


