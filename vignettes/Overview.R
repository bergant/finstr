## ----, echo=FALSE, results='hide', message=FALSE-------------------------
library(dplyr)
library(tidyr)
library(finstr)
data(xbrl_data_aapl2013)
data(xbrl_data_aapl2014)

## ----xbrl_parse_min, eval=FALSE, echo=TRUE-------------------------------
#  library(finstr)
#  # parse XBRL (Apple 10-K report)
#  xbrl_url2014 <-
#    "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"
#  xbrl_url2013 <-
#    "http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml"
#  xbrl_data_aapl2014 <- xbrl_parse_min(xbrl_url2014)
#  xbrl_data_aapl2013 <- xbrl_parse_min(xbrl_url2013)

## ----xbrl_get_statements-------------------------------------------------
st2013 <- xbrl_get_statements(xbrl_data_aapl2013)
st2014 <- xbrl_get_statements(xbrl_data_aapl2014)
st2014

## ----statement-----------------------------------------------------------
balance_sheet2013 <- st2013$StatementOfFinancialPositionClassified
balance_sheet2014 <- st2014$StatementOfFinancialPositionClassified
balance_sheet2014

## ----relations-----------------------------------------------------------
get_relations(balance_sheet2014)

## ----elements------------------------------------------------------------
get_elements(balance_sheet2014, parent_id = "LiabilitiesCurrent", as_data_frame = T)

## ----merge_statement-----------------------------------------------------
balance_sheet <- merge( balance_sheet2013, balance_sheet2014 )

## ----merge_statements----------------------------------------------------
# merge all statements
st_all <- merge( st2013, st2014 )
# check if statement of income is merged:
balance_sheet <- st_all$StatementOfFinancialPositionClassified

## ----expose1-------------------------------------------------------------
library(dplyr)

balance_sheet %>%
  expose("Assets",
         "Liabilities",
         "CommintmentsAndContingencies",
         "StockholdersEquity")
  

## ----expose2-------------------------------------------------------------
balance_sheet %>%
  expose("Assets",
         Liabilities = c("Liabilities", 
                         "CommintmentsAndContingencies",
                         "StockholdersEquity"))

## ----other1--------------------------------------------------------------
balance_sheet %>%
  expose("AssetsCurrent",
         NonCurrentAssets = other("Assets"),
         Liabilities = other())

## ----without1------------------------------------------------------------
balance_sheet %>%
  expose(
    NonCurrentAssets = "Assets" %without% "AssetsCurrent",
    CurrentAssets = other("Assets")
  )

## ----without2------------------------------------------------------------
balance_sheet %>%
  expose( 
    TangibleAssets =
      "Assets" %without% c("Goodwill","IntangibleAssetsNetExcludingGoodwill"),
    IntangibleAssets = other("Assets")
  ) 

## ----current_ratio-------------------------------------------------------
library(dplyr)

balance_sheet %>%
  expose("AssetsCurrent", "LiabilitiesCurrent") %>%
  mutate(CurrentRatio = AssetsCurrent / LiabilitiesCurrent) %>%
  select(endDate, CurrentRatio)


## ----DaysSalesOutstanding------------------------------------------------

balance_sheet %>%
  inner_join( st_all$StatementOfIncome, by = "endDate") %>%
  mutate( 
    AccountReceivableLast = lag(AccountsReceivableNetCurrent),
    AccountReceivableAvg = (AccountReceivableLast+AccountsReceivableNetCurrent)/2,
    DaysSalesOutstanding = AccountReceivableAvg / SalesRevenueNet * 365 
  ) %>%
  select(endDate, DaysSalesOutstanding) %>%
  na.omit()


