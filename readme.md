# finstr - Financial Statements in R




**Warning: finstr package is in development. 
Please use with caution.**

The purpose of finstr package is to use financial statements 
data in more structured form and process.
For now it is offering:

1. Data structure for financial statements in tidy and usable format
2. Function to merge two reporting periods in single object
3. Some helper functions to help explore and manipulate the data in the 
structure

The idea in long term is to create an environment for reproducible financial 
statement analysis. With existing packages like XBRL for XBRL parsing, 
dplyr for data manipulation and knitr for reproducible research, this 
shouldn't be a long journey.


## Get data
Use XBRL package to parse XBRL files. For example:

```r
library(XBRL)
# parse XBRL (Apple 10-K report)
xbrl_url2014 <- 
  "http://edgar.sec.gov/Archives/edgar/data/320193/000119312514383437/aapl-20140927.xml"
xbrl_url2013 <- 
  "http://edgar.sec.gov/Archives/edgar/data/320193/000119312513416534/aapl-20130928.xml"
xbrl_data_aapl2014 <- xbrlDoAll(xbrl_url2014)
xbrl_data_aapl2013 <- xbrlDoAll(xbrl_url2013)
```

## Prepare statements
With `xbrl_get_statements` convert XBRL data to *statements* object. 

```r
library(finstr)

st2013 <- xbrl_get_statements(xbrl_data_aapl2013)
st2014 <- xbrl_get_statements(xbrl_data_aapl2014)
st2014
```

```
## Financial statements repository
##                                              From         To Rows Columns
## StatementOfIncome                      2012-09-29 2014-09-27    3      15
## StatementOfOtherComprehensiveIncome    2012-09-29 2014-09-27    3      14
## StatementOfFinancialPositionClassified 2013-09-28 2014-09-27    2      33
## StatementOfCashFlowsIndirect           2012-09-29 2014-09-27    3      33
```

Statements object is a list of 
several statement objects (ballance sheets, income and cash 
flow statements) which are data frames with elements as columns and periods
as rows. 
To get a single *statement* use *statements* object as a regular R list:

```r
balance_sheet2013 <- st2013$StatementOfFinancialPositionClassified
balance_sheet2014 <- st2014$StatementOfFinancialPositionClassified
balance_sheet2014
```

```
## Financial statement: 2 observations from 2013-09-28 to 2014-09-27 
## Numbers in  000000 
##                                                 2014-09-27 2013-09-28
## Assets                                          231839     207000    
## AssetsCurrent                                    68531      73286    
## CashAndCashEquivalentsAtCarryingValue            13844      14259    
## AvailableForSaleSecuritiesCurrent                11233      26287    
## AccountsReceivableNetCurrent                     17460      13102    
## InventoryNet                                      2111       1764    
## DeferredTaxAssetsNetCurrent                       4318       3453    
## NontradeReceivablesCurrent                        9759       7539    
## OtherAssetsCurrent                                9806       6882    
## AvailableForSaleSecuritiesNoncurrent            130162     106215    
## PropertyPlantAndEquipmentNet                     20624      16597    
## Goodwill                                          4616       1577    
## IntangibleAssetsNetExcludingGoodwill              4142       4179    
## OtherAssetsNoncurrent                             3764       5146    
## LiabilitiesAndStockholdersEquity                231839     207000    
## Liabilities                                     120292      83451    
## LiabilitiesCurrent                               63448      43658    
## AccountsPayableCurrent                           30196      22367    
## AccruedLiabilitiesCurrent                        18453      13856    
## DeferredRevenueCurrent                            8491       7435    
## CommercialPaper                                   6308          0    
## DeferredRevenueNoncurrent                         3031       2625    
## LongTermDebt                                     28987      16960    
## OtherLiabilitiesNoncurrent                       24826      20208    
## CommitmentsAndContingencies                          0          0    
## StockholdersEquity                              111547     123549    
## CommonStocksIncludingAdditionalPaidInCapital     23313      19764    
## RetainedEarningsAccumulatedDeficit               87152     104256    
## AccumulatedOtherComprehensiveIncomeLossNetOfTax   1082       -471
```


Information about hierarchical structure of elements (concepts) is stored 
as an attribute to the statement object.

To see the calculation hierarchy of elements use `get_elements`: 

```r
get_elements(balance_sheet2014)
```

```
## Assets 
## ..AssetsCurrent 
## ....CashAndCashEquivalentsAtCarryingValue 
## ....AvailableForSaleSecuritiesCurrent 
## ....AccountsReceivableNetCurrent 
## ....InventoryNet 
## ....DeferredTaxAssetsNetCurrent 
## ....NontradeReceivablesCurrent 
## ....OtherAssetsCurrent 
## ..AvailableForSaleSecuritiesNoncurrent 
## ..PropertyPlantAndEquipmentNet 
## ..Goodwill 
## ..IntangibleAssetsNetExcludingGoodwill 
## ..OtherAssetsNoncurrent 
## LiabilitiesAndStockholdersEquity 
## ..Liabilities 
## ....LiabilitiesCurrent 
## ......AccountsPayableCurrent 
## ......AccruedLiabilitiesCurrent 
## ......DeferredRevenueCurrent 
## ......CommercialPaper 
## ....DeferredRevenueNoncurrent 
## ....LongTermDebt 
## ....OtherLiabilitiesNoncurrent 
## ..CommitmentsAndContingencies 
## ..StockholdersEquity 
## ....CommonStocksIncludingAdditionalPaidInCapital 
## ....RetainedEarningsAccumulatedDeficit 
## ....AccumulatedOtherComprehensiveIncomeLossNetOfTax
```

To query the fundamental elements from higher order elements use
`get_elements`:

```r
get_elements(balance_sheet2014, parent_id = "LiabilitiesCurrent")
```

```
## ....LiabilitiesCurrent 
## ......AccountsPayableCurrent 
## ......AccruedLiabilitiesCurrent 
## ......DeferredRevenueCurrent 
## ......CommercialPaper
```

## Merge statements from different periods
Use `merge` function to create single financial statement data from two 
statements. 

```r
balance_sheet <- merge( balance_sheet2013, balance_sheet2014 )
```

The structure of merged balance sheets may differ because of changes in XBRL
taxonomy. 
Function `merge` takes care of it by expanding the element 
hierarchy to fit both statements. 
The values of any missing elements is set to 0.

To merge all statements from *statements* object use merge on statements objects:

```r
# merge all statements
st_all <- merge( st2013, st2014 )
# check if statement of income is merged:
balance_sheet <- st_all$StatementOfFinancialPositionClassified
```

## Calculate new values and ratios
Statement object (in our case `balance_sheet`) is also a data frame object.
With elements (or concepts) as columns and time periods as rows.
It is possible then to use statement as a data frame.

Lets calculate current ratio which is defined by

$$ Current Ratio = \frac{Current Assets}{Current Liabilities} $$


```r
library(dplyr)

balance_sheet %>%
  mutate(CurrentRatio = AssetsCurrent / LiabilitiesCurrent) %>%
  select(endDate, CurrentRatio)
```

```
##      endDate CurrentRatio
## 1 2012-09-29     1.495849
## 2 2013-09-28     1.678639
## 3 2014-09-27     1.080113
```

By using `calculate` function we can achieve the same result with
less verbose language. Lets calculate now two ratios:


```r
library(dplyr)

balance_sheet %>% calculate(
  
    Current_Ratio = AssetsCurrent / LiabilitiesCurrent,
    
    Quick_Ratio =  
      ( CashAndCashEquivalentsAtCarryingValue + 
          AvailableForSaleSecuritiesCurrent +
          AccountsReceivableNetCurrent
        ) / LiabilitiesCurrent
    
)
```

```
##      endDate Current_Ratio Quick_Ratio
## 1 2012-09-29      1.495849    1.039360
## 2 2013-09-28      1.678639    1.228824
## 3 2014-09-27      1.080113    0.670423
```


If we need a period average value we can use a `lag` function.
For example, to calculate *DSO* (days sales outstanding) over longer periods
the average of account receivable is compared to net sales.

We will use the formula for yearly preiods:

$$ DSO = \frac{Average Accounts Receivable}{Sales Revenue} \times 365 $$

In this case we need to connect two type of statements: balance sheets and
income statements. With matching reporting periods it can be accomplished 
with joining two data frames:


```r
balance_sheet %>%
  inner_join( st_all$StatementOfIncome, by = "endDate") %>%
  calculate(
    .AccountReceivableLast = lag(AccountsReceivableNetCurrent),
    .AccountReceivableAvg = (.AccountReceivableLast + AccountsReceivableNetCurrent)/2,
    DaysSalesOutstanding = .AccountReceivableAvg / SalesRevenueNet * 365 
  )
```

```
##      endDate DaysSalesOutstanding
## 1 2012-09-29                   NA
## 2 2013-09-28             25.66169
## 3 2014-09-27             30.51268
```

The leading dot instructs the calculate function to hide the value. In our case
only DaysSalesOutstanding is selected in final result.
