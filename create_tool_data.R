#  Copyright 2015, INSEAD
#  by T. Evgeniou, Enric Junque de Fortuny, Nick Nassuphis, Theo Vermaelen 
#  Dual licensed under the MIT or GPL Version 2 licenses.


##########################################################################################
# Creates a small subset of the data only used for the interactive tool - to load faster
##########################################################################################

rm(list=ls()) # Clean up the memory, if we want to rerun from scratch
load("bb_issuers_new.Rdata")

BUYBACK_DATA_TOOL = data.frame(
  
  Prior.Returns.Score = BUYBACK_DATA$DATASET$CRSP$recent_performance_score,
  BE.ME.Score = BUYBACK_DATA$DATASET$CRSP$BE.ME_score,
  Size.Score = BUYBACK_DATA$DATASET$CRSP$Market.Cap_score,
  Valuation.Index = BUYBACK_DATA$Valuation_Index,
  EU.Index = sapply(1:length(BUYBACK_DATA$DATASET$SDC$Event.Date), function(i){
    ifelse(High_Idiosyncr_eventsBB[i], 2, ifelse(Low_Idiosyncr_eventsBB[i], 0, 1)) +
      ifelse(High_VOL_eventsBB[i], 2, ifelse(Low_VOL_eventsBB[i], 0, 1)) +
      ifelse(company_subset_undervalued_bb[i], 2, ifelse(company_subset_overvalued_bb[i], 0, 1))
  }),
  Market.Cap = BUYBACK_DATA$DATASET$CRSP$Market.Cap,
  Percent.Shares = BUYBACK_DATA$DATASET$SDC$Event.Size,
  Market.beta.Score = BUYBACK_DATA$DATASET$CRSP$Market.Cap_score,
  one.minus.Rsq.Score = 1-BUYBACK_DATA$DATASET$CRSP$Rsq_score,
  pre.vol.Score = BUYBACK_DATA$DATASET$CRSP$pre_vol_Score,
  ivol.Score = BUYBACK_DATA$DATASET$CRSP$IVOL_score,
  leverage.ratio = BUYBACK_DATA$DATASET$CRSP$leverage_lt_over_lt_plus_e,
  Event.Date = BUYBACK_DATA$DATASET$SDC$Event.Date,
  Event.Size = BUYBACK_DATA$DATASET$SDC$Event.Size,
  Industry_name = BUYBACK_DATA$DATASET$SDC$Industry_name
  
)
Risk_Factors_Monthly <- BUYBACK_DATA$Risk_Factors_Monthly
returns_by_event_monthly <- BUYBACK_DATA$DATASET$returns_by_event_monthly
DatesMonth = BUYBACK_DATA$DATASET$DatesMonth

save(BUYBACK_DATA_TOOL,Risk_Factors_Monthly,returns_by_event_monthly,DatesMonth, file = "bb_issuersTOOL.Rdata")


