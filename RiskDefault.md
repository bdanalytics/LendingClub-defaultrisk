# LendingClub DefaultRisk: not.fully.paid classification
bdanalytics  

**  **    
**Date: (Thu) Mar 26, 2015**    

# Introduction:  

Data: LendingClub.com
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/loans.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size <- 0.30               # > 0 & < 1
glb_split_sample.seed <- 144               # or any integer 

glb_predct_var <- "not.fully.paid"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- NULL                # or NULL

glb_exclude_vars_as_features <- union(glb_id_vars, ".rnorm")     # or NULL                      
# List chrs (convert into factors if it's a valid feature); num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("purpose")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_mice_complete.seed <- 144               # or any integer
glb_is_regression <- FALSE; glb_is_classification <- TRUE

glb_mdl <- glb_sel_mdl <- glb_dmy_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    #url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/loans.csv", 
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/loans_imputed.csv",     
    comment="glb_entity_df", force_header=TRUE,
    print_diagn=(glb_is_separate_newent_dataset | 
                !glb_split_entity_newent_datasets))
```

```
## [1] "Downloading file ./data/loans_imputed.csv from https://courses.edx.org/c4x/MITx/15.071x_2/asset/loans_imputed.csv..."
## [1] "Reading file ./data/loans_imputed.csv..."
## [1] "dimensions of data in ./data/loans_imputed.csv: 9,578 rows x 14 cols"
```

```r
print(table(glb_entity_df$not.fully.paid))
```

```
## 
##    0    1 
## 8045 1533
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url="<newdt_url>", 
        comment="glb_newent_df", force_header=TRUE, print_diagn=TRUE)
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_entity_df, parse(text=glb_split_newdata_condition)))
            glb_entity_df <- do.call("subset", 
                list(glb_entity_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_entity_df[, glb_predct_var], 
                                      SplitRatio=(1-glb_split_newdata_size))
                glb_newent_df <- glb_entity_df[!split, ] 
                glb_entity_df <- glb_entity_df[split ,]
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_entity_df)
        str(glb_entity_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##    credit.policy            purpose int.rate installment log.annual.inc
## 2              1        credit_card   0.1071      228.22      11.082143
## 3              1 debt_consolidation   0.1357      366.86      10.373491
## 10             1 debt_consolidation   0.1221       84.12      10.203592
## 12             1 debt_consolidation   0.1324      253.58      11.835009
## 21             1          all_other   0.0800      188.02      11.225243
## 28             1 debt_consolidation   0.1375      255.43       9.998798
##      dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 2  14.29  707          2760.000     33623       76.7              0
## 3  11.63  682          4710.000      3511       25.6              1
## 10 10.00  707          2730.042      5630       23.0              1
## 12  9.16  662          4298.000      5122       18.2              2
## 21 16.08  772          4888.958     29797       23.2              1
## 28 14.29  662          1318.958      4175       51.5              0
##    delinq.2yrs pub.rec not.fully.paid
## 2            0       0              0
## 3            0       0              0
## 10           0       0              0
## 12           1       0              0
## 21           0       0              0
## 28           1       0              0
##      credit.policy            purpose int.rate installment log.annual.inc
## 2993             1        educational   0.1600      112.51       9.903488
## 3276             1     major_purchase   0.1253      160.64      10.126631
## 3529             1     major_purchase   0.1126      312.21      10.693398
## 4707             1 debt_consolidation   0.1392      546.20      11.608236
## 5409             1     small_business   0.1600      421.89      10.896739
## 7452             1        credit_card   0.1136      526.58      10.714418
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 2993  6.30  677          1259.958      1715       39.9              0
## 3276 12.53  692          2850.000     19939       69.7              2
## 3529 19.04  757          7049.958       336        4.7              1
## 4707 15.36  687          3060.000     23839       59.3              1
## 5409 11.20  702          3945.958      5418       33.9              3
## 7452  8.83  727          3457.042     12142       62.3              0
##      delinq.2yrs pub.rec not.fully.paid
## 2993           0       0              0
## 3276           0       0              0
## 3529           0       0              0
## 4707           2       0              0
## 5409           0       0              1
## 7452           0       0              1
##      credit.policy   purpose int.rate installment log.annual.inc   dti
## 9561             0 all_other   0.1867      547.36       11.40756 15.76
## 9565             0 all_other   0.1385      511.56       12.32386 12.33
## 9568             0 all_other   0.1311      101.24       10.96820  8.23
## 9569             0 all_other   0.1979       37.06       10.64542 22.17
## 9571             0 all_other   0.1671      113.63       10.64542 28.06
## 9575             0 all_other   0.1253      257.70       11.14186  0.21
##      fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 9561  667         10050.042     13255       88.4              7
## 9565  687          6420.042    385489       51.2              4
## 9568  687          2790.042      1514       13.8              5
## 9569  667          5916.000     28854       59.8              6
## 9571  672          3210.042     25759       63.8              5
## 9575  722          4380.000       184        1.1              5
##      delinq.2yrs pub.rec not.fully.paid
## 9561           0       0              0
## 9565           0       0              0
## 9568           0       0              0
## 9569           0       1              0
## 9571           0       0              1
## 9575           0       0              1
## 'data.frame':	2873 obs. of  14 variables:
##  $ credit.policy    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ purpose          : chr  "credit_card" "debt_consolidation" "debt_consolidation" "debt_consolidation" ...
##  $ int.rate         : num  0.107 0.136 0.122 0.132 0.08 ...
##  $ installment      : num  228.2 366.9 84.1 253.6 188 ...
##  $ log.annual.inc   : num  11.1 10.4 10.2 11.8 11.2 ...
##  $ dti              : num  14.29 11.63 10 9.16 16.08 ...
##  $ fico             : int  707 682 707 662 772 662 772 797 712 682 ...
##  $ days.with.cr.line: num  2760 4710 2730 4298 4889 ...
##  $ revol.bal        : int  33623 3511 5630 5122 29797 4175 3660 6844 3534 43039 ...
##  $ revol.util       : num  76.7 25.6 23 18.2 23.2 51.5 6.8 14.4 54.4 93.4 ...
##  $ inq.last.6mths   : int  0 1 1 2 1 0 0 0 0 3 ...
##  $ delinq.2yrs      : int  0 0 0 1 0 1 0 0 0 0 ...
##  $ pub.rec          : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ not.fully.paid   : int  0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_newent_df"
##   credit.policy            purpose int.rate installment log.annual.inc
## 1             1 debt_consolidation   0.1189      829.10       11.35041
## 4             1 debt_consolidation   0.1008      162.34       11.35041
## 5             1        credit_card   0.1426      102.92       11.29973
## 6             1        credit_card   0.0788      125.13       11.90497
## 7             1 debt_consolidation   0.1496      194.02       10.71442
## 8             1          all_other   0.1114      131.22       11.00210
##     dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 1 19.48  737          5639.958     28854       52.1              0
## 4  8.10  712          2699.958     33667       73.2              1
## 5 14.97  667          4066.000      4740       39.5              0
## 6 16.98  727          6120.042     50807       51.0              0
## 7  4.00  667          3180.042      3839       76.8              0
## 8 11.08  722          5116.000     24220       68.6              0
##   delinq.2yrs pub.rec not.fully.paid
## 1           0       0              0
## 4           0       0              0
## 5           1       0              0
## 6           0       0              0
## 7           0       1              1
## 8           0       0              1
##      credit.policy            purpose int.rate installment log.annual.inc
## 237              1 debt_consolidation   0.1280      554.37       11.03489
## 397              1 debt_consolidation   0.0951      592.70       10.34174
## 1693             1 debt_consolidation   0.1178      551.28       11.22524
## 3125             1          all_other   0.1253      334.67       10.81818
## 3953             1   home_improvement   0.1284      282.40       11.93819
## 9010             0          all_other   0.1411       85.58       10.25766
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 237  19.22  677         3060.0417     17641       69.5              2
## 397  16.49  747         4260.0417     25946       37.9              0
## 1693 11.47  707         3630.0000     17565       53.9              0
## 3125  3.37  717         2626.0000      8158       31.5              0
## 3953  0.39  712         8159.9583      5714       28.4              3
## 9010  7.37  732          779.9583         0        0.0              0
##      delinq.2yrs pub.rec not.fully.paid
## 237            0       0              0
## 397            0       0              0
## 1693           0       0              0
## 3125           0       0              0
## 3953           0       0              0
## 9010           0       0              0
##      credit.policy            purpose int.rate installment log.annual.inc
## 9572             0          all_other   0.1568      161.01       11.22524
## 9573             0 debt_consolidation   0.1565       69.98       10.11047
## 9574             0          all_other   0.1461      344.76       12.18075
## 9576             0 debt_consolidation   0.1071       97.81       10.59663
## 9577             0   home_improvement   0.1600      351.58       10.81978
## 9578             0 debt_consolidation   0.1392      853.43       11.26446
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 9572  8.00  677          7230.000      6909       29.2              4
## 9573  7.02  662          8190.042      2999       39.5              6
## 9574 10.39  672         10474.000    215372       82.1              2
## 9576 13.09  687          3450.042     10036       82.9              8
## 9577 19.18  692          1800.000         0        3.2              5
## 9578 16.28  732          4740.000     37879       57.0              6
##      delinq.2yrs pub.rec not.fully.paid
## 9572           0       1              1
## 9573           0       0              1
## 9574           0       0              1
## 9576           0       0              1
## 9577           0       0              1
## 9578           0       0              1
## 'data.frame':	6705 obs. of  14 variables:
##  $ credit.policy    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ purpose          : chr  "debt_consolidation" "debt_consolidation" "credit_card" "credit_card" ...
##  $ int.rate         : num  0.1189 0.1008 0.1426 0.0788 0.1496 ...
##  $ installment      : num  829 162 103 125 194 ...
##  $ log.annual.inc   : num  11.4 11.4 11.3 11.9 10.7 ...
##  $ dti              : num  19.5 8.1 15 17 4 ...
##  $ fico             : int  737 712 667 727 667 722 682 677 767 747 ...
##  $ days.with.cr.line: num  5640 2700 4066 6120 3180 ...
##  $ revol.bal        : int  28854 33667 4740 50807 3839 24220 69909 13846 6068 3021 ...
##  $ revol.util       : num  52.1 73.2 39.5 51 76.8 68.6 51.1 71 16.7 4.8 ...
##  $ inq.last.6mths   : int  0 1 0 0 0 0 1 2 0 0 ...
##  $ delinq.2yrs      : int  0 0 1 0 0 0 0 0 0 1 ...
##  $ pub.rec          : int  0 0 0 0 1 0 0 1 0 0 ...
##  $ not.fully.paid   : int  0 0 0 0 1 1 0 0 0 0 ...
##  - attr(*, "comment")= chr "glb_entity_df"
```

```r
script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# credit.policy: 1 if the customer meets the credit underwriting criteria of LendingClub.com, and 0 otherwise.
# purpose: The purpose of the loan (takes values "credit_card", "debt_consolidation", "educational", "major_purchase", "small_business", and "all_other").
# int.rate: The interest rate of the loan, as a proportion (a rate of 11% would be stored as 0.11). Borrowers judged by LendingClub.com to be more risky are assigned higher interest rates.
# installment: The monthly installments ($) owed by the borrower if the loan is funded.
# log.annual.inc: The natural log of the self-reported annual income of the borrower.
# dti: The debt-to-income ratio of the borrower (amount of debt divided by annual income).
# fico: The FICO credit score of the borrower.
# days.with.cr.line: The number of days the borrower has had a credit line.
# revol.bal: The borrower's revolving balance (amount unpaid at the end of the credit card billing cycle).
# revol.util: The borrower's revolving line utilization rate (the amount of the credit line used relative to total credit available).
# inq.last.6mths: The borrower's number of inquiries by creditors in the last 6 months.
# delinq.2yrs: The number of times the borrower had been 30+ days past due on a payment in the past 2 years.
# pub.rec: The borrower's number of derogatory public records (bankruptcy filings, tax liens, or judgments).

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

        purpose.fctr=factor(purpose, 
                    as.factor(union(obs_df$purpose, obs_twin_df$purpose))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>"), 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>),        
        .rnorm=rnorm(1)
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_newent_df)
```

```
## Loading required package: plyr
```

```
##  credit.policy      purpose             int.rate       installment    
##  Min.   :0.0000   Length:6705        Min.   :0.0600   Min.   : 15.76  
##  1st Qu.:1.0000   Class :character   1st Qu.:0.1039   1st Qu.:163.77  
##  Median :1.0000   Mode  :character   Median :0.1229   Median :269.31  
##  Mean   :0.8051                      Mean   :0.1227   Mean   :319.99  
##  3rd Qu.:1.0000                      3rd Qu.:0.1411   3rd Qu.:436.44  
##  Max.   :1.0000                      Max.   :0.2164   Max.   :940.14  
##                                                                       
##  log.annual.inc        dti             fico       days.with.cr.line
##  Min.   : 7.548   Min.   : 0.00   Min.   :612.0   Min.   :  180    
##  1st Qu.:10.556   1st Qu.: 7.23   1st Qu.:682.0   1st Qu.: 2820    
##  Median :10.918   Median :12.59   Median :707.0   Median : 4114    
##  Mean   :10.934   Mean   :12.55   Mean   :710.9   Mean   : 4590    
##  3rd Qu.:11.301   3rd Qu.:17.80   3rd Qu.:737.0   3rd Qu.: 5760    
##  Max.   :14.528   Max.   :29.95   Max.   :827.0   Max.   :17616    
##                                                                    
##    revol.bal        revol.util     inq.last.6mths    delinq.2yrs     
##  Min.   :     0   Min.   :  0.00   Min.   : 0.000   Min.   : 0.0000  
##  1st Qu.:  3084   1st Qu.: 22.20   1st Qu.: 0.000   1st Qu.: 0.0000  
##  Median :  8535   Median : 46.10   Median : 1.000   Median : 0.0000  
##  Mean   : 16792   Mean   : 46.71   Mean   : 1.578   Mean   : 0.1651  
##  3rd Qu.: 18107   3rd Qu.: 71.20   3rd Qu.: 2.000   3rd Qu.: 0.0000  
##  Max.   :952013   Max.   :119.00   Max.   :33.000   Max.   :11.0000  
##                                                                      
##     pub.rec        not.fully.paid             purpose.fctr 
##  Min.   :0.00000   Min.   :0.00   debt_consolidation:2751  
##  1st Qu.:0.00000   1st Qu.:0.00   credit_card       : 851  
##  Median :0.00000   Median :0.00   all_other         :1643  
##  Mean   :0.06413   Mean   :0.16   home_improvement  : 443  
##  3rd Qu.:0.00000   3rd Qu.:0.00   small_business    : 435  
##  Max.   :5.00000   Max.   :1.00   major_purchase    : 332  
##                                   educational       : 250  
##      .rnorm      
##  Min.   :-2.101  
##  1st Qu.:-2.101  
##  Median :-2.101  
##  Mean   :-2.101  
##  3rd Qu.:-2.101  
##  Max.   :-2.101  
##                  
##     credit.policy           purpose          int.rate       installment 
##                 0                 0                 0                 0 
##    log.annual.inc               dti              fico days.with.cr.line 
##                 0                 0                 0                 0 
##         revol.bal        revol.util    inq.last.6mths       delinq.2yrs 
##                 0                 0                 0                 0 
##           pub.rec    not.fully.paid      purpose.fctr            .rnorm 
##                 0                 0                 0                 0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df, glb_entity_df)
```

```
##  credit.policy      purpose             int.rate       installment    
##  Min.   :0.0000   Length:2873        Min.   :0.0600   Min.   : 15.67  
##  1st Qu.:1.0000   Class :character   1st Qu.:0.1028   1st Qu.:163.57  
##  Median :1.0000   Mode  :character   Median :0.1221   Median :267.74  
##  Mean   :0.8047                      Mean   :0.1225   Mean   :316.99  
##  3rd Qu.:1.0000                      3rd Qu.:0.1393   3rd Qu.:421.89  
##  Max.   :1.0000                      Max.   :0.2121   Max.   :926.83  
##                                                                       
##  log.annual.inc        dti             fico       days.with.cr.line
##  Min.   : 8.102   Min.   : 0.00   Min.   :612.0   Min.   :  179    
##  1st Qu.:10.560   1st Qu.: 7.16   1st Qu.:682.0   1st Qu.: 2795    
##  Median :10.933   Median :12.85   Median :707.0   Median : 4140    
##  Mean   :10.928   Mean   :12.75   Mean   :710.8   Mean   : 4494    
##  3rd Qu.:11.290   3rd Qu.:18.30   3rd Qu.:737.0   3rd Qu.: 5670    
##  Max.   :13.459   Max.   :29.96   Max.   :822.0   Max.   :17640    
##                                                                    
##    revol.bal         revol.util     inq.last.6mths    delinq.2yrs     
##  Min.   :      0   Min.   :  0.00   Min.   : 0.000   Min.   : 0.0000  
##  1st Qu.:   3362   1st Qu.: 23.40   1st Qu.: 0.000   1st Qu.: 0.0000  
##  Median :   8712   Median : 46.90   Median : 1.000   Median : 0.0000  
##  Mean   :  17198   Mean   : 47.02   Mean   : 1.576   Mean   : 0.1605  
##  3rd Qu.:  18728   3rd Qu.: 70.40   3rd Qu.: 2.000   3rd Qu.: 0.0000  
##  Max.   :1207359   Max.   :108.80   Max.   :24.000   Max.   :13.0000  
##                                                                       
##     pub.rec        not.fully.paid               purpose.fctr 
##  Min.   :0.00000   Min.   :0.0000   credit_card       : 411  
##  1st Qu.:0.00000   1st Qu.:0.0000   debt_consolidation:1206  
##  Median :0.00000   Median :0.0000   all_other         : 688  
##  Mean   :0.05743   Mean   :0.1601   small_business    : 184  
##  3rd Qu.:0.00000   3rd Qu.:0.0000   home_improvement  : 186  
##  Max.   :3.00000   Max.   :1.0000   major_purchase    : 105  
##                                     educational       :  93  
##      .rnorm      
##  Min.   :-1.288  
##  1st Qu.:-1.288  
##  Median :-1.288  
##  Mean   :-1.288  
##  3rd Qu.:-1.288  
##  Max.   :-1.288  
##                  
##     credit.policy           purpose          int.rate       installment 
##                 0                 0                 0                 0 
##    log.annual.inc               dti              fico days.with.cr.line 
##                 0                 0                 0                 0 
##         revol.bal        revol.util    inq.last.6mths       delinq.2yrs 
##                 0                 0                 0                 0 
##           pub.rec    not.fully.paid      purpose.fctr            .rnorm 
##                 0                 0                 0                 0
```

```r
#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_newent_df
# Check for glb_newent_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(table(sign(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(mycreate_xtab(glb_entity_df, <col1_name>))
# print(mycreate_xtab(glb_entity_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>.NA, glb_entity_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](RiskDefault_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>"))

script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_predct_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
        union_df$.rownames <- rownames(union_df)
        union_df <- orderBy(~.rownames, union_df)
        
        imp_entity_df <- myimport_data(
            url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/loans_imputed.csv", 
            comment="imp_entity_df", force_header=TRUE, print_diagn=TRUE)
        print(all.equal(subset(union_df, select=-c(.src, .rownames, .rnorm)), 
                        imp_entity_df))
        
        # Partition again
        glb_entity_df <<- subset(union_df, .src == "entity", select=-c(.src, .rownames))
        comment(glb_entity_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-c(.src, .rownames))
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if ((sum(sapply(names(glb_entity_df), 
                function(col) sum(is.na(glb_entity_df[, col])))) > 0) | 
    (sum(sapply(names(glb_newent_df), 
                function(col) sum(is.na(glb_newent_df[, col])))) > 0))
    glb_impute_missing_data(glb_entity_df, glb_newent_df)

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_newent_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- 
    myselect_features(glb_entity_df, glb_exclude_vars_as_features, glb_predct_var))
```

```
##                                  id        cor.y   cor.y.abs
## credit.policy         credit.policy -0.163108283 0.163108283
## int.rate                   int.rate  0.155449912 0.155449912
## inq.last.6mths       inq.last.6mths  0.153796715 0.153796715
## fico                           fico -0.153338871 0.153338871
## revol.util               revol.util  0.081111895 0.081111895
## pub.rec                     pub.rec  0.055223796 0.055223796
## revol.bal                 revol.bal  0.049656648 0.049656648
## dti                             dti  0.048904794 0.048904794
## installment             installment  0.047121296 0.047121296
## log.annual.inc       log.annual.inc -0.041998649 0.041998649
## days.with.cr.line days.with.cr.line -0.038840650 0.038840650
## delinq.2yrs             delinq.2yrs  0.007991196 0.007991196
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, merge(glb_feats_df, 
          mydelete_cor_features(glb_feats_df, glb_entity_df, glb_predct_var, 
                                glb_exclude_vars_as_features), 
          all.x=TRUE)))
```

```
## Loading required package: reshape2
```

```
##                   credit.policy    int.rate inq.last.6mths        fico
## credit.policy        1.00000000 -0.29545210   -0.545759226  0.34625859
## int.rate            -0.29545210  1.00000000    0.208373695 -0.71165905
## inq.last.6mths      -0.54575923  0.20837370    1.000000000 -0.20251619
## fico                 0.34625859 -0.71165905   -0.202516194  1.00000000
## revol.util          -0.10566551  0.46772224   -0.005086417 -0.54199015
## pub.rec             -0.04968784  0.09710638    0.071054569 -0.14934986
## revol.bal           -0.20127800  0.10902802    0.009921288 -0.01959978
## dti                 -0.09632966  0.22543784    0.035745635 -0.24610542
## installment          0.04507119  0.27687748   -0.013331367  0.08694803
## log.annual.inc       0.02529379  0.06296215    0.014122390  0.10444220
## days.with.cr.line    0.09626052 -0.11809783   -0.050309146  0.26060273
## delinq.2yrs         -0.07855665  0.15807244    0.024965059 -0.21462576
##                     revol.util      pub.rec    revol.bal          dti
## credit.policy     -0.105665510 -0.049687836 -0.201278003 -0.096329661
## int.rate           0.467722241  0.097106381  0.109028020  0.225437841
## inq.last.6mths    -0.005086417  0.071054569  0.009921288  0.035745635
## fico              -0.541990148 -0.149349858 -0.019599776 -0.246105419
## revol.util         1.000000000  0.068353177  0.222304285  0.333444875
## pub.rec            0.068353177  1.000000000 -0.038971079 -0.001328395
## revol.bal          0.222304285 -0.038971079  1.000000000  0.211858561
## dti                0.333444875 -0.001328395  0.211858561  1.000000000
## installment        0.086528671 -0.035134141  0.256658744  0.044582352
## log.annual.inc     0.068528198  0.027376219  0.385320139 -0.055794740
## days.with.cr.line -0.027697224  0.071062321  0.235884320  0.049702260
## delinq.2yrs       -0.052657796  0.020272805 -0.034005286 -0.006986651
##                     installment log.annual.inc days.with.cr.line
## credit.policy      0.0450711933     0.02529379        0.09626052
## int.rate           0.2768774814     0.06296215       -0.11809783
## inq.last.6mths    -0.0133313674     0.01412239       -0.05030915
## fico               0.0869480340     0.10444220        0.26060273
## revol.util         0.0865286707     0.06852820       -0.02769722
## pub.rec           -0.0351341411     0.02737622        0.07106232
## revol.bal          0.2566587438     0.38532014        0.23588432
## dti                0.0445823515    -0.05579474        0.04970226
## installment        1.0000000000     0.44393932        0.18068037
## log.annual.inc     0.4439393176     1.00000000        0.33424156
## days.with.cr.line  0.1806803749     0.33424156        1.00000000
## delinq.2yrs       -0.0002032654     0.03310201        0.09363960
##                     delinq.2yrs
## credit.policy     -0.0785566545
## int.rate           0.1580724446
## inq.last.6mths     0.0249650592
## fico              -0.2146257580
## revol.util        -0.0526577960
## pub.rec            0.0202728046
## revol.bal         -0.0340052864
## dti               -0.0069866513
## installment       -0.0002032654
## log.annual.inc     0.0331020066
## days.with.cr.line  0.0936396008
## delinq.2yrs        1.0000000000
##                   credit.policy   int.rate inq.last.6mths       fico
## credit.policy        0.00000000 0.29545210    0.545759226 0.34625859
## int.rate             0.29545210 0.00000000    0.208373695 0.71165905
## inq.last.6mths       0.54575923 0.20837370    0.000000000 0.20251619
## fico                 0.34625859 0.71165905    0.202516194 0.00000000
## revol.util           0.10566551 0.46772224    0.005086417 0.54199015
## pub.rec              0.04968784 0.09710638    0.071054569 0.14934986
## revol.bal            0.20127800 0.10902802    0.009921288 0.01959978
## dti                  0.09632966 0.22543784    0.035745635 0.24610542
## installment          0.04507119 0.27687748    0.013331367 0.08694803
## log.annual.inc       0.02529379 0.06296215    0.014122390 0.10444220
## days.with.cr.line    0.09626052 0.11809783    0.050309146 0.26060273
## delinq.2yrs          0.07855665 0.15807244    0.024965059 0.21462576
##                    revol.util     pub.rec   revol.bal         dti
## credit.policy     0.105665510 0.049687836 0.201278003 0.096329661
## int.rate          0.467722241 0.097106381 0.109028020 0.225437841
## inq.last.6mths    0.005086417 0.071054569 0.009921288 0.035745635
## fico              0.541990148 0.149349858 0.019599776 0.246105419
## revol.util        0.000000000 0.068353177 0.222304285 0.333444875
## pub.rec           0.068353177 0.000000000 0.038971079 0.001328395
## revol.bal         0.222304285 0.038971079 0.000000000 0.211858561
## dti               0.333444875 0.001328395 0.211858561 0.000000000
## installment       0.086528671 0.035134141 0.256658744 0.044582352
## log.annual.inc    0.068528198 0.027376219 0.385320139 0.055794740
## days.with.cr.line 0.027697224 0.071062321 0.235884320 0.049702260
## delinq.2yrs       0.052657796 0.020272805 0.034005286 0.006986651
##                    installment log.annual.inc days.with.cr.line
## credit.policy     0.0450711933     0.02529379        0.09626052
## int.rate          0.2768774814     0.06296215        0.11809783
## inq.last.6mths    0.0133313674     0.01412239        0.05030915
## fico              0.0869480340     0.10444220        0.26060273
## revol.util        0.0865286707     0.06852820        0.02769722
## pub.rec           0.0351341411     0.02737622        0.07106232
## revol.bal         0.2566587438     0.38532014        0.23588432
## dti               0.0445823515     0.05579474        0.04970226
## installment       0.0000000000     0.44393932        0.18068037
## log.annual.inc    0.4439393176     0.00000000        0.33424156
## days.with.cr.line 0.1806803749     0.33424156        0.00000000
## delinq.2yrs       0.0002032654     0.03310201        0.09363960
##                    delinq.2yrs
## credit.policy     0.0785566545
## int.rate          0.1580724446
## inq.last.6mths    0.0249650592
## fico              0.2146257580
## revol.util        0.0526577960
## pub.rec           0.0202728046
## revol.bal         0.0340052864
## dti               0.0069866513
## installment       0.0002032654
## log.annual.inc    0.0331020066
## days.with.cr.line 0.0936396008
## delinq.2yrs       0.0000000000
## [1] "cor(int.rate, fico)=-0.7117"
```

![](RiskDefault_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(not.fully.paid, int.rate)=0.1554"
## [1] "cor(not.fully.paid, fico)=-0.1533"
```

```
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is >=1000, so using gam with formula: y ~ s(x, bs = "cs"). Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(glb_feats_df, glb_entity_df,
## glb_predct_var, : Dropping fico as a feature
```

![](RiskDefault_files/figure-html/remove_correlated_features-2.png) 

```
##                                  id        cor.y   cor.y.abs
## credit.policy         credit.policy -0.163108283 0.163108283
## int.rate                   int.rate  0.155449912 0.155449912
## inq.last.6mths       inq.last.6mths  0.153796715 0.153796715
## revol.util               revol.util  0.081111895 0.081111895
## pub.rec                     pub.rec  0.055223796 0.055223796
## revol.bal                 revol.bal  0.049656648 0.049656648
## dti                             dti  0.048904794 0.048904794
## installment             installment  0.047121296 0.047121296
## log.annual.inc       log.annual.inc -0.041998649 0.041998649
## days.with.cr.line days.with.cr.line -0.038840650 0.038840650
## delinq.2yrs             delinq.2yrs  0.007991196 0.007991196
##                   credit.policy    int.rate inq.last.6mths   revol.util
## credit.policy        1.00000000 -0.29545210   -0.545759226 -0.105665510
## int.rate            -0.29545210  1.00000000    0.208373695  0.467722241
## inq.last.6mths      -0.54575923  0.20837370    1.000000000 -0.005086417
## revol.util          -0.10566551  0.46772224   -0.005086417  1.000000000
## pub.rec             -0.04968784  0.09710638    0.071054569  0.068353177
## revol.bal           -0.20127800  0.10902802    0.009921288  0.222304285
## dti                 -0.09632966  0.22543784    0.035745635  0.333444875
## installment          0.04507119  0.27687748   -0.013331367  0.086528671
## log.annual.inc       0.02529379  0.06296215    0.014122390  0.068528198
## days.with.cr.line    0.09626052 -0.11809783   -0.050309146 -0.027697224
## delinq.2yrs         -0.07855665  0.15807244    0.024965059 -0.052657796
##                        pub.rec    revol.bal          dti   installment
## credit.policy     -0.049687836 -0.201278003 -0.096329661  0.0450711933
## int.rate           0.097106381  0.109028020  0.225437841  0.2768774814
## inq.last.6mths     0.071054569  0.009921288  0.035745635 -0.0133313674
## revol.util         0.068353177  0.222304285  0.333444875  0.0865286707
## pub.rec            1.000000000 -0.038971079 -0.001328395 -0.0351341411
## revol.bal         -0.038971079  1.000000000  0.211858561  0.2566587438
## dti               -0.001328395  0.211858561  1.000000000  0.0445823515
## installment       -0.035134141  0.256658744  0.044582352  1.0000000000
## log.annual.inc     0.027376219  0.385320139 -0.055794740  0.4439393176
## days.with.cr.line  0.071062321  0.235884320  0.049702260  0.1806803749
## delinq.2yrs        0.020272805 -0.034005286 -0.006986651 -0.0002032654
##                   log.annual.inc days.with.cr.line   delinq.2yrs
## credit.policy         0.02529379        0.09626052 -0.0785566545
## int.rate              0.06296215       -0.11809783  0.1580724446
## inq.last.6mths        0.01412239       -0.05030915  0.0249650592
## revol.util            0.06852820       -0.02769722 -0.0526577960
## pub.rec               0.02737622        0.07106232  0.0202728046
## revol.bal             0.38532014        0.23588432 -0.0340052864
## dti                  -0.05579474        0.04970226 -0.0069866513
## installment           0.44393932        0.18068037 -0.0002032654
## log.annual.inc        1.00000000        0.33424156  0.0331020066
## days.with.cr.line     0.33424156        1.00000000  0.0936396008
## delinq.2yrs           0.03310201        0.09363960  1.0000000000
##                   credit.policy   int.rate inq.last.6mths  revol.util
## credit.policy        0.00000000 0.29545210    0.545759226 0.105665510
## int.rate             0.29545210 0.00000000    0.208373695 0.467722241
## inq.last.6mths       0.54575923 0.20837370    0.000000000 0.005086417
## revol.util           0.10566551 0.46772224    0.005086417 0.000000000
## pub.rec              0.04968784 0.09710638    0.071054569 0.068353177
## revol.bal            0.20127800 0.10902802    0.009921288 0.222304285
## dti                  0.09632966 0.22543784    0.035745635 0.333444875
## installment          0.04507119 0.27687748    0.013331367 0.086528671
## log.annual.inc       0.02529379 0.06296215    0.014122390 0.068528198
## days.with.cr.line    0.09626052 0.11809783    0.050309146 0.027697224
## delinq.2yrs          0.07855665 0.15807244    0.024965059 0.052657796
##                       pub.rec   revol.bal         dti  installment
## credit.policy     0.049687836 0.201278003 0.096329661 0.0450711933
## int.rate          0.097106381 0.109028020 0.225437841 0.2768774814
## inq.last.6mths    0.071054569 0.009921288 0.035745635 0.0133313674
## revol.util        0.068353177 0.222304285 0.333444875 0.0865286707
## pub.rec           0.000000000 0.038971079 0.001328395 0.0351341411
## revol.bal         0.038971079 0.000000000 0.211858561 0.2566587438
## dti               0.001328395 0.211858561 0.000000000 0.0445823515
## installment       0.035134141 0.256658744 0.044582352 0.0000000000
## log.annual.inc    0.027376219 0.385320139 0.055794740 0.4439393176
## days.with.cr.line 0.071062321 0.235884320 0.049702260 0.1806803749
## delinq.2yrs       0.020272805 0.034005286 0.006986651 0.0002032654
##                   log.annual.inc days.with.cr.line  delinq.2yrs
## credit.policy         0.02529379        0.09626052 0.0785566545
## int.rate              0.06296215        0.11809783 0.1580724446
## inq.last.6mths        0.01412239        0.05030915 0.0249650592
## revol.util            0.06852820        0.02769722 0.0526577960
## pub.rec               0.02737622        0.07106232 0.0202728046
## revol.bal             0.38532014        0.23588432 0.0340052864
## dti                   0.05579474        0.04970226 0.0069866513
## installment           0.44393932        0.18068037 0.0002032654
## log.annual.inc        0.00000000        0.33424156 0.0331020066
## days.with.cr.line     0.33424156        0.00000000 0.0936396008
## delinq.2yrs           0.03310201        0.09363960 0.0000000000
##                   id        cor.y   cor.y.abs cor.low
## 8           int.rate  0.155449912 0.155449912       1
## 6     inq.last.6mths  0.153796715 0.153796715       1
## 12        revol.util  0.081111895 0.081111895       1
## 10           pub.rec  0.055223796 0.055223796       1
## 11         revol.bal  0.049656648 0.049656648       1
## 4                dti  0.048904794 0.048904794       1
## 7        installment  0.047121296 0.047121296       1
## 3        delinq.2yrs  0.007991196 0.007991196       1
## 2  days.with.cr.line -0.038840650 0.038840650       1
## 9     log.annual.inc -0.041998649 0.041998649       1
## 5               fico -0.153338871 0.153338871      NA
## 1      credit.policy -0.163108283 0.163108283       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Add dummy model - random variable
#   Potential Enhancements:
#       For classifiers, it shd generate proba/outcomes that mimics the freq
#           distribution of glb_predct_var values; Right now it always generates
#           0 (most frequent ?)
ret_lst <- myrun_mdl_fn(indep_vars_vctr=".rnorm",
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5906  -0.5906  -0.5906  -0.5906   1.9144  
## 
## Coefficients: (1 not defined because of singularities)
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.65801    0.03331  -49.78   <2e-16 ***
## .rnorm            NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5896.6  on 6704  degrees of freedom
## AIC: 5898.6
## 
## Number of Fisher Scoring iterations: 3
## 
##    feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 1 .rnorm  6705       NA       NA           NA 49880.87      NA 5898.636
##   auc.fit auc.OOB
## 1     0.5     0.5
```

```r
glb_dmy_mdl <- glb_mdl

# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        lcl_predct_var=glb_predct_var, 
                        lcl_predct_var_name=glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0547  -0.6271  -0.5442  -0.4361   2.2914  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -3.6726     0.1688  -21.76   <2e-16 ***
## int.rate     15.9214     1.2702   12.54   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5734.8  on 6703  degrees of freedom
## AIC: 5738.8
## 
## Number of Fisher Scoring iterations: 4
## 
##      feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit
## 2 int.rate  6705       NA       NA           NA 53742.06      NA 5738.833
## 1   .rnorm  6705       NA       NA           NA 49880.87      NA 5898.636
##     auc.fit   auc.OOB
## 2 0.6186226 0.6239081
## 1 0.5000000 0.5000000
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, 
              subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                        glb_predct_var, glb_predct_var_name,
                            fit_df=glb_entity_df, OOB_df=glb_newent_df)    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0322  -0.6321  -0.5297  -0.4255   2.3260  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -3.01504    0.22292 -13.525  < 2e-16 ***
## int.rate      43.98527    6.34227   6.935 4.05e-12 ***
## int.rate:fico -0.04750    0.01053  -4.511 6.44e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5713.6  on 6702  degrees of freedom
## AIC: 5719.6
## 
## Number of Fisher Scoring iterations: 4
## 
##                     feats n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit
## 3 int.rate, int.rate:fico  6705       NA       NA           NA 54279.05
## 2                int.rate  6705       NA       NA           NA 53742.06
## 1                  .rnorm  6705       NA       NA           NA 49880.87
##   SSE.OOB  AIC.fit   auc.fit   auc.OOB
## 3      NA 5719.640 0.6260227 0.6246982
## 2      NA 5738.833 0.6186226 0.6239081
## 1      NA 5898.636 0.5000000 0.5000000
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, 
                                               cor.low == 1)[, "id"],
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1439  -0.6091  -0.5043  -0.3993   2.3907  
## 
## Coefficients:
##                     Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        1.647e+00  7.482e-01   2.201 0.027723 *  
## int.rate           9.159e+00  1.617e+00   5.665 1.47e-08 ***
## inq.last.6mths     8.617e-02  1.583e-02   5.444 5.20e-08 ***
## revol.util         2.764e-03  1.396e-03   1.980 0.047679 *  
## pub.rec            3.779e-01  1.136e-01   3.327 0.000879 ***
## revol.bal          3.325e-06  1.143e-06   2.908 0.003634 ** 
## dti               -4.290e-04  5.360e-03  -0.080 0.936196    
## installment        8.410e-04  1.932e-04   4.353 1.34e-05 ***
## delinq.2yrs       -2.301e-02  6.209e-02  -0.371 0.710911    
## days.with.cr.line -1.378e-05  1.543e-05  -0.894 0.371535    
## log.annual.inc    -4.329e-01  7.033e-02  -6.155 7.49e-10 ***
## credit.policy     -4.356e-01  9.877e-02  -4.410 1.03e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5566.6  on 6693  degrees of freedom
## AIC: 5590.6
## 
## Number of Fisher Scoring iterations: 4
## 
##                                                                                                                                       feats
## 4 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, delinq.2yrs, days.with.cr.line, log.annual.inc, credit.policy
## 3                                                                                                                   int.rate, int.rate:fico
## 2                                                                                                                                  int.rate
## 1                                                                                                                                    .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 4  6705       NA       NA           NA 56195.59      NA 5590.636 0.6727112
## 3  6705       NA       NA           NA 54279.05      NA 5719.640 0.6260227
## 2  6705       NA       NA           NA 53742.06      NA 5738.833 0.6186226
## 1  6705       NA       NA           NA 49880.87      NA 5898.636 0.5000000
##     auc.OOB
## 4 0.6630219
## 3 0.6246982
## 2 0.6239081
## 1 0.5000000
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
    union(glb_predct_var, glb_exclude_vars_as_features)),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2049  -0.6205  -0.4951  -0.3606   2.6397  
## 
## Coefficients:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   8.866e+00  1.549e+00   5.722 1.05e-08 ***
## credit.policy                -3.368e-01  1.011e-01  -3.332 0.000861 ***
## int.rate                      6.110e-01  2.085e+00   0.293 0.769446    
## installment                   1.275e-03  2.092e-04   6.093 1.11e-09 ***
## log.annual.inc               -4.337e-01  7.148e-02  -6.067 1.30e-09 ***
## dti                           4.638e-03  5.502e-03   0.843 0.399288    
## fico                         -9.317e-03  1.710e-03  -5.448 5.08e-08 ***
## days.with.cr.line             2.371e-06  1.588e-05   0.149 0.881343    
## revol.bal                     3.085e-06  1.168e-06   2.641 0.008273 ** 
## revol.util                    1.839e-03  1.535e-03   1.199 0.230722    
## inq.last.6mths                8.437e-02  1.600e-02   5.275 1.33e-07 ***
## delinq.2yrs                  -8.320e-02  6.561e-02  -1.268 0.204762    
## pub.rec                       3.300e-01  1.139e-01   2.898 0.003756 ** 
## purpose.fctrcredit_card      -2.929e-01  1.259e-01  -2.326 0.019998 *  
## purpose.fctrall_other         3.212e-01  9.183e-02   3.498 0.000469 ***
## purpose.fctrhome_improvement  4.939e-01  1.439e-01   3.432 0.000599 ***
## purpose.fctrsmall_business    7.333e-01  1.356e-01   5.406 6.46e-08 ***
## purpose.fctrmajor_purchase   -1.618e-01  1.997e-01  -0.810 0.417760    
## purpose.fctreducational       4.559e-01  1.732e-01   2.633 0.008466 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5485.2  on 6686  degrees of freedom
## AIC: 5523.2
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                                                                                                                           feats
## 5 credit.policy, int.rate, installment, log.annual.inc, dti, fico, days.with.cr.line, revol.bal, revol.util, inq.last.6mths, delinq.2yrs, pub.rec, purpose.fctr
## 4                     int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, delinq.2yrs, days.with.cr.line, log.annual.inc, credit.policy
## 3                                                                                                                                       int.rate, int.rate:fico
## 2                                                                                                                                                      int.rate
## 1                                                                                                                                                        .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 5  6705       NA       NA           NA 61192.29      NA 5523.212 0.6861578
## 4  6705       NA       NA           NA 56195.59      NA 5590.636 0.6727112
## 3  6705       NA       NA           NA 54279.05      NA 5719.640 0.6260227
## 2  6705       NA       NA           NA 53742.06      NA 5738.833 0.6186226
## 1  6705       NA       NA           NA 49880.87      NA 5898.636 0.5000000
##     auc.OOB
## 5 0.6720995
## 4 0.6630219
## 3 0.6246982
## 2 0.6239081
## 1 0.5000000
```

```r
glb_sel_mdl <- glb_all_X_mdl <- glb_mdl

# User specified - easier to exclude features
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(names(glb_entity_df), 
#     union(union(glb_predct_var, glb_exclude_vars_as_features), c("<feat1_name>", "<feat2_name>"))),
#                         glb_predct_var, glb_predct_var_name,
#                         fit_df=glb_entity_df, OOB_df=glb_newent_df)

# User specified - easier to include features
ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("int.rate"),
                        glb_predct_var, glb_predct_var_name,
                        fit_df=glb_entity_df, OOB_df=glb_newent_df)
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0547  -0.6271  -0.5442  -0.4361   2.2914  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -3.6726     0.1688  -21.76   <2e-16 ***
## int.rate     15.9214     1.2702   12.54   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5734.8  on 6703  degrees of freedom
## AIC: 5738.8
## 
## Number of Fisher Scoring iterations: 4
## 
##                                                                                                                                                           feats
## 5 credit.policy, int.rate, installment, log.annual.inc, dti, fico, days.with.cr.line, revol.bal, revol.util, inq.last.6mths, delinq.2yrs, pub.rec, purpose.fctr
## 4                     int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, delinq.2yrs, days.with.cr.line, log.annual.inc, credit.policy
## 3                                                                                                                                       int.rate, int.rate:fico
## 2                                                                                                                                                      int.rate
## 6                                                                                                                                                      int.rate
## 1                                                                                                                                                        .rnorm
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 5  6705       NA       NA           NA 61192.29      NA 5523.212 0.6861578
## 4  6705       NA       NA           NA 56195.59      NA 5590.636 0.6727112
## 3  6705       NA       NA           NA 54279.05      NA 5719.640 0.6260227
## 2  6705       NA       NA           NA 53742.06      NA 5738.833 0.6186226
## 6  6705       NA       NA           NA 53742.06      NA 5738.833 0.6186226
## 1  6705       NA       NA           NA 49880.87      NA 5898.636 0.5000000
##     auc.OOB
## 5 0.6720995
## 4 0.6630219
## 3 0.6246982
## 2 0.6239081
## 6 0.6239081
## 1 0.5000000
```

```r
glb_int.rate_X_mdl <- glb_mdl

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
if (glb_is_regression)
    print(myplot_scatter(plot_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))

if (glb_is_classification) {
    # Lower AIC is better
    plot_models_df[, "inv.AIC.fit"] <- 1.0 / plot_models_df[, "AIC.fit"] 
    print(myplot_scatter(plot_models_df, "inv.AIC.fit", "auc.OOB") + 
          geom_text(aes(label=feats.label), data=plot_models_df, color="NavyBlue", 
                    size=3.5, angle=45))
}
```

![](RiskDefault_files/figure-html/run_models-1.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats(lcl_sel_mdl=glb_sel_mdl, 
                                          lcl_entity_df=glb_entity_df))
```

```
##                                  id         Pr.z
## installment             installment 1.108957e-09
## log.annual.inc       log.annual.inc 1.304203e-09
## fico                           fico 5.082004e-08
## purpose.fctr           purpose.fctr 6.455536e-08
## inq.last.6mths       inq.last.6mths 1.330224e-07
## credit.policy         credit.policy 8.607114e-04
## pub.rec                     pub.rec 3.756271e-03
## revol.bal                 revol.bal 8.273096e-03
## delinq.2yrs             delinq.2yrs 2.047623e-01
## revol.util               revol.util 2.307221e-01
## dti                             dti 3.992879e-01
## int.rate                   int.rate 7.694460e-01
## days.with.cr.line days.with.cr.line 8.813429e-01
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_entity_df[, glb_predct_var_name] - glb_entity_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_entity_df)))                             
}    

if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id,
                        glb_predct_var, glb_predct_var_name, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
#     print(glb_models_df[nrow(glb_models_df), ])
    glb_entity_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_entity_df, type="response")

    require(ROCR)
    ROCRpred <- prediction(glb_entity_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_entity_df[, glb_predct_var])
    ROCRperf <- performance(ROCRpred, "tpr", "fpr")
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2,1.7))
    
    thresholds_df <- data.frame(threshold=seq(0.0, 1.0, 0.1))
    thresholds_df$f.score <- sapply(1:nrow(thresholds_df), function(row_ix) 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     thresholds_df[row_ix, "threshold"], 
                                     glb_predct_var, glb_predct_var_name))
    print(thresholds_df)
    print(myplot_line(thresholds_df, "threshold", "f.score"))
    
    glb_clf_proba_threshold <- thresholds_df[which.max(thresholds_df$f.score), 
                                             "threshold"]
    # This should change to maximize f.score.OOB ???
    print(sprintf("Classifier Probability Threshold: %0.4f to maximize f.score.fit",
                  glb_clf_proba_threshold))

    glb_clf_proba_threshold <- 0.5
    print(sprintf("Classifier Probability Threshold: %0.4f to H/w spec",
                  glb_clf_proba_threshold))

    glb_entity_df[, glb_predct_var_name] <- 
        (glb_entity_df[, paste0(glb_predct_var_name, ".proba")] >= 
             glb_clf_proba_threshold) * 1.0
    print(mycreate_xtab(glb_entity_df, c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score=%0.4f", 
        mycompute_classifier_f.score(glb_sel_mdl, glb_entity_df, 
                                     glb_clf_proba_threshold, 
                                     glb_predct_var, glb_predct_var_name)))    
}    
```

```
## 
## Call:
## glm(formula = reformulate(indep_vars_vctr, response = lcl_predct_var), 
##     family = "binomial", data = fit_df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2049  -0.6205  -0.4951  -0.3606   2.6397  
## 
## Coefficients:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                   8.866e+00  1.549e+00   5.722 1.05e-08 ***
## installment                   1.275e-03  2.092e-04   6.093 1.11e-09 ***
## log.annual.inc               -4.337e-01  7.148e-02  -6.067 1.30e-09 ***
## fico                         -9.317e-03  1.710e-03  -5.448 5.08e-08 ***
## purpose.fctrcredit_card      -2.929e-01  1.259e-01  -2.326 0.019998 *  
## purpose.fctrall_other         3.212e-01  9.183e-02   3.498 0.000469 ***
## purpose.fctrhome_improvement  4.939e-01  1.439e-01   3.432 0.000599 ***
## purpose.fctrsmall_business    7.333e-01  1.356e-01   5.406 6.46e-08 ***
## purpose.fctrmajor_purchase   -1.618e-01  1.997e-01  -0.810 0.417760    
## purpose.fctreducational       4.559e-01  1.732e-01   2.633 0.008466 ** 
## inq.last.6mths                8.437e-02  1.600e-02   5.275 1.33e-07 ***
## credit.policy                -3.368e-01  1.011e-01  -3.332 0.000861 ***
## pub.rec                       3.300e-01  1.139e-01   2.898 0.003756 ** 
## revol.bal                     3.085e-06  1.168e-06   2.641 0.008273 ** 
## delinq.2yrs                  -8.320e-02  6.561e-02  -1.268 0.204762    
## revol.util                    1.839e-03  1.535e-03   1.199 0.230722    
## dti                           4.638e-03  5.502e-03   0.843 0.399288    
## int.rate                      6.110e-01  2.085e+00   0.293 0.769446    
## days.with.cr.line             2.371e-06  1.588e-05   0.149 0.881343    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5485.2  on 6686  degrees of freedom
## AIC: 5523.2
## 
## Number of Fisher Scoring iterations: 5
## 
##                                                                                                                                                           feats
## 5 credit.policy, int.rate, installment, log.annual.inc, dti, fico, days.with.cr.line, revol.bal, revol.util, inq.last.6mths, delinq.2yrs, pub.rec, purpose.fctr
## 4                     int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, delinq.2yrs, days.with.cr.line, log.annual.inc, credit.policy
## 3                                                                                                                                       int.rate, int.rate:fico
## 2                                                                                                                                                      int.rate
## 6                                                                                                                                                      int.rate
## 1                                                                                                                                                        .rnorm
## 7 installment, log.annual.inc, fico, purpose.fctr, inq.last.6mths, credit.policy, pub.rec, revol.bal, delinq.2yrs, revol.util, dti, int.rate, days.with.cr.line
##   n.fit R.sq.fit R.sq.OOB Adj.R.sq.fit  SSE.fit SSE.OOB  AIC.fit   auc.fit
## 5  6705       NA       NA           NA 61192.29      NA 5523.212 0.6861578
## 4  6705       NA       NA           NA 56195.59      NA 5590.636 0.6727112
## 3  6705       NA       NA           NA 54279.05      NA 5719.640 0.6260227
## 2  6705       NA       NA           NA 53742.06      NA 5738.833 0.6186226
## 6  6705       NA       NA           NA 53742.06      NA 5738.833 0.6186226
## 1  6705       NA       NA           NA 49880.87      NA 5898.636 0.5000000
## 7  6705       NA       NA           NA 61192.29      NA 5523.212 0.6861578
##     auc.OOB
## 5 0.6720995
## 4 0.6630219
## 3 0.6246982
## 2 0.6239081
## 6 0.6239081
## 1 0.5000000
## 7        NA
```

![](RiskDefault_files/figure-html/fit_training.all-1.png) 

```
##    threshold     f.score
## 1        0.0 0.275906403
## 2        0.1 0.321780496
## 3        0.2 0.352339181
## 4        0.3 0.251400124
## 5        0.4 0.133971292
## 6        0.5 0.056387665
## 7        0.6 0.018315018
## 8        0.7 0.003703704
## 9        0.8 0.003710575
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](RiskDefault_files/figure-html/fit_training.all-2.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
## [1] "Classifier Probability Threshold: 0.5000 to H/w spec"
##   not.fully.paid not.fully.paid.predict.0 not.fully.paid.predict.1
## 1              0                     5602                       30
## 2              1                     1041                       32
## [1] "f.score=0.0564"
```

```r
print(glb_feats_df <- mymerge_feats_Pr.z(glb_feats_df, glb_sel_mdl, glb_entity_df))
```

```
##                   id        cor.y   cor.y.abs cor.low         Pr.z
## 7        installment  0.047121296 0.047121296       1 1.108957e-09
## 9     log.annual.inc -0.041998649 0.041998649       1 1.304203e-09
## 5               fico -0.153338871 0.153338871      NA 5.082004e-08
## 11      purpose.fctr           NA          NA      NA 6.455536e-08
## 6     inq.last.6mths  0.153796715 0.153796715       1 1.330224e-07
## 1      credit.policy -0.163108283 0.163108283       1 8.607114e-04
## 10           pub.rec  0.055223796 0.055223796       1 3.756271e-03
## 12         revol.bal  0.049656648 0.049656648       1 8.273096e-03
## 3        delinq.2yrs  0.007991196 0.007991196       1 2.047623e-01
## 13        revol.util  0.081111895 0.081111895       1 2.307221e-01
## 4                dti  0.048904794 0.048904794       1 3.992879e-01
## 8           int.rate  0.155449912 0.155449912       1 7.694460e-01
## 2  days.with.cr.line -0.038840650 0.038840650       1 8.813429e-01
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name)
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)) == 0)
            warning("No coefficients in selected model are statistically significant")
        else print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1],
                    glb_predct_var, glb_predct_var_name, glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_entity_df)
```

![](RiskDefault_files/figure-html/fit_training.all-3.png) ![](RiskDefault_files/figure-html/fit_training.all-4.png) ![](RiskDefault_files/figure-html/fit_training.all-5.png) ![](RiskDefault_files/figure-html/fit_training.all-6.png) ![](RiskDefault_files/figure-html/fit_training.all-7.png) ![](RiskDefault_files/figure-html/fit_training.all-8.png) ![](RiskDefault_files/figure-html/fit_training.all-9.png) ![](RiskDefault_files/figure-html/fit_training.all-10.png) 

```
##    credit.policy            purpose int.rate installment log.annual.inc
## 7              1 debt_consolidation   0.1496      194.02      10.714418
## 8              1          all_other   0.1114      131.22      11.002100
## 58             1        credit_card   0.1343      678.08      11.884489
## 66             1          all_other   0.1059       32.55      10.433822
## 90             1     small_business   0.0964      642.02      11.472103
## 91             1     small_business   0.1154      104.76       9.615805
##      dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 7   4.00  667          3180.042      3839       76.8              0
## 8  11.08  722          5116.000     24220       68.6              0
## 58 10.15  682          4209.958     41674       74.1              0
## 66 14.47  687          1110.000      4485       36.9              1
## 90  2.86  737          2190.000     10296       42.5              0
## 91 12.24  672          1289.958      2867       55.1              3
##    delinq.2yrs pub.rec not.fully.paid       purpose.fctr    .rnorm
## 7            0       1              1 debt_consolidation -2.101394
## 8            0       0              1          all_other -2.101394
## 58           0       0              1        credit_card -2.101394
## 66           0       0              1          all_other -2.101394
## 90           0       0              1     small_business -2.101394
## 91           0       0              1     small_business -2.101394
##    not.fully.paid.predict.proba not.fully.paid.predict not.fully.paid.fctr
## 7                     0.1848420                      0                   1
## 8                     0.1045200                      0                   1
## 58                    0.1188397                      0                   1
## 66                    0.1501818                      0                   1
## 90                    0.1717049                      0                   1
## 91                    0.3678482                      0                   1
##    not.fully.paid.predict.accurate .label
## 7                            FALSE     .7
## 8                            FALSE     .8
## 58                           FALSE    .58
## 66                           FALSE    .66
## 90                           FALSE    .90
## 91                           FALSE    .91
##      credit.policy            purpose int.rate installment log.annual.inc
## 2385             1        credit_card   0.1148      197.82       10.93311
## 5157             1 debt_consolidation   0.1183      659.37       11.06664
## 8069             0 debt_consolidation   0.1312      270.02       10.18112
## 8358             0          all_other   0.1324      169.05       10.23996
## 8623             0          all_other   0.1134      210.56       10.64542
## 9301             0 debt_consolidation   0.1531      292.45       11.22524
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 2385 11.16  712          2367.958     18513       60.1              2
## 5157  8.18  737          6946.000     27095       67.1              2
## 8069 11.32  657          1410.000      3919       37.7             15
## 8358 19.54  667          3960.042     13542       56.4              5
## 8623  5.60  682          1199.958      4713       61.2              6
## 9301 13.44  677          7169.958      6837       26.7              7
##      delinq.2yrs pub.rec not.fully.paid       purpose.fctr    .rnorm
## 2385           0       0              1        credit_card -2.101394
## 5157           0       0              1 debt_consolidation -2.101394
## 8069           0       0              0 debt_consolidation -2.101394
## 8358           1       0              1          all_other -2.101394
## 8623           0       0              1          all_other -2.101394
## 9301           0       0              1 debt_consolidation -2.101394
##      not.fully.paid.predict.proba not.fully.paid.predict
## 2385                   0.08146414                      0
## 5157                   0.14261512                      0
## 8069                   0.53910254                      1
## 8358                   0.35723096                      0
## 8623                   0.31351788                      0
## 9301                   0.24914598                      0
##      not.fully.paid.fctr not.fully.paid.predict.accurate .label
## 2385                   1                           FALSE  .2385
## 5157                   1                           FALSE  .5157
## 8069                   0                           FALSE  .8069
## 8358                   1                           FALSE  .8358
## 8623                   1                           FALSE  .8623
## 9301                   1                           FALSE  .9301
##      credit.policy            purpose int.rate installment log.annual.inc
## 9572             0          all_other   0.1568      161.01       11.22524
## 9573             0 debt_consolidation   0.1565       69.98       10.11047
## 9574             0          all_other   0.1461      344.76       12.18075
## 9576             0 debt_consolidation   0.1071       97.81       10.59663
## 9577             0   home_improvement   0.1600      351.58       10.81978
## 9578             0 debt_consolidation   0.1392      853.43       11.26446
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 9572  8.00  677          7230.000      6909       29.2              4
## 9573  7.02  662          8190.042      2999       39.5              6
## 9574 10.39  672         10474.000    215372       82.1              2
## 9576 13.09  687          3450.042     10036       82.9              8
## 9577 19.18  692          1800.000         0        3.2              5
## 9578 16.28  732          4740.000     37879       57.0              6
##      delinq.2yrs pub.rec not.fully.paid       purpose.fctr    .rnorm
## 9572           0       1              1          all_other -2.101394
## 9573           0       0              1 debt_consolidation -2.101394
## 9574           0       0              1          all_other -2.101394
## 9576           0       0              1 debt_consolidation -2.101394
## 9577           0       0              1   home_improvement -2.101394
## 9578           0       0              1 debt_consolidation -2.101394
##      not.fully.paid.predict.proba not.fully.paid.predict
## 9572                    0.2909692                      0
## 9573                    0.2970257                      0
## 9574                    0.3164259                      0
## 9576                    0.2664379                      0
## 9577                    0.3289883                      0
## 9578                    0.2991222                      0
##      not.fully.paid.fctr not.fully.paid.predict.accurate .label
## 9572                   1                           FALSE  .9572
## 9573                   1                           FALSE  .9573
## 9574                   1                           FALSE  .9574
## 9576                   1                           FALSE  .9576
## 9577                   1                           FALSE  .9577
## 9578                   1                           FALSE  .9578
```

![](RiskDefault_files/figure-html/fit_training.all-11.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_newent_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_newent_df, type="response")

if (glb_is_classification) {
    # Compute selected model predictions
    glb_newent_df[, paste0(glb_predct_var_name, ".proba")] <- 
        predict(glb_sel_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, glb_predct_var_name] <- 
        (predict(glb_sel_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0

    # Compute dummy model predictions
    glb_newent_df[, paste0(glb_predct_var, ".preddmy.proba")] <- 
        predict(glb_dmy_mdl, newdata=glb_newent_df, type="response")
    glb_newent_df[, paste0(glb_predct_var, ".preddmy")] <- 
        (predict(glb_dmy_mdl, newdata=glb_newent_df, type="response") >= 
            glb_clf_proba_threshold) * 1.0
}
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```r
myprint_df(glb_newent_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    not.fully.paid not.fully.paid.predict
## 2               0                      0
## 3               0                      0
## 10              0                      0
## 12              0                      0
## 21              0                      0
## 28              0                      0
##      not.fully.paid not.fully.paid.predict
## 4191              0                      0
## 4952              0                      0
## 5086              0                      0
## 6114              0                      0
## 7795              1                      0
## 8552              1                      0
##      not.fully.paid not.fully.paid.predict
## 9561              0                      0
## 9565              0                      0
## 9568              0                      0
## 9569              0                      0
## 9571              1                      0
## 9575              1                      0
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_newent_df[, glb_predct_var_name] - 
                        glb_newent_df[, glb_predct_var]) ^ 2) / nrow(glb_newent_df)) ^ 0.5))                        
    print(myplot_scatter(glb_newent_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
    glb_newent_df[, paste0(glb_predct_var_name, ".err")] <- 
        abs(glb_newent_df[, glb_predct_var_name] - glb_newent_df[, glb_predct_var])
    print(head(orderBy(reformulate(c("-", paste0(glb_predct_var_name, ".err"))), 
                       glb_newent_df)))                                                      

#     glb_newent_df[, "<Output Pred variable>"] <- func(glb_newent_df[, glb_pred_var_name])                         
}                         

if (glb_is_classification) {
    ROCRpred <- prediction(glb_newent_df[, paste0(glb_predct_var_name, ".proba")],
                           glb_newent_df[, glb_predct_var])
    print(sprintf("auc=%0.4f", auc <- as.numeric(performance(ROCRpred, "auc")@y.values)))   
    
    print(sprintf("probability threshold=%0.4f", glb_clf_proba_threshold))
    print(newent_conf_df <- mycreate_xtab(glb_newent_df, 
                                        c(glb_predct_var, glb_predct_var_name)))
    print(sprintf("f.score.sel=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_sel_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=glb_predct_var_name)))
    print(sprintf("sensitivity=%0.4f", newent_conf_df[2, 3] / 
                      (newent_conf_df[2, 3] + newent_conf_df[2, 2])))
    print(sprintf("specificity=%0.4f", newent_conf_df[1, 2] / 
                      (newent_conf_df[1, 2] + newent_conf_df[1, 3])))
    print(sprintf("accuracy=%0.4f", (newent_conf_df[1, 2] + newent_conf_df[2, 3]) / 
                      (newent_conf_df[1, 2] + newent_conf_df[2, 3] + 
                       newent_conf_df[1, 3] + newent_conf_df[2, 2])))
    
    print(mycreate_xtab(glb_newent_df, c(glb_predct_var, paste0(glb_predct_var, ".preddmy"))))
    print(sprintf("f.score.dmy=%0.4f", 
        mycompute_classifier_f.score(mdl=glb_dmy_mdl, obs_df=glb_newent_df, 
                                     proba_threshold=glb_clf_proba_threshold, 
                                     lcl_predct_var=glb_predct_var, 
                                     lcl_predct_var_name=paste0(glb_predct_var, ".preddmy"))))
}    
```

```
## [1] "auc=0.6721"
## [1] "probability threshold=0.5000"
##   not.fully.paid not.fully.paid.predict.0 not.fully.paid.predict.1
## 1              0                     2400                       13
## 2              1                      457                        3
## [1] "f.score.sel=0.0126"
## [1] "sensitivity=0.0065"
## [1] "specificity=0.9946"
## [1] "accuracy=0.8364"
##   not.fully.paid not.fully.paid.preddmy.0
## 1              0                     2413
## 2              1                      460
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## [1] "f.score.dmy=0.0000"
```

```r
glb_analytics_diag_plots(glb_newent_df)
```

![](RiskDefault_files/figure-html/predict_newdata-1.png) ![](RiskDefault_files/figure-html/predict_newdata-2.png) ![](RiskDefault_files/figure-html/predict_newdata-3.png) ![](RiskDefault_files/figure-html/predict_newdata-4.png) ![](RiskDefault_files/figure-html/predict_newdata-5.png) ![](RiskDefault_files/figure-html/predict_newdata-6.png) ![](RiskDefault_files/figure-html/predict_newdata-7.png) ![](RiskDefault_files/figure-html/predict_newdata-8.png) 

```
##     credit.policy            purpose int.rate installment log.annual.inc
## 74              1     small_business   0.1501      225.37      12.269047
## 75              1   home_improvement   0.0964       80.26      11.225243
## 76              1   home_improvement   0.1280       84.00      10.373491
## 77              1 debt_consolidation   0.1122      507.46      10.596635
## 131             1     small_business   0.1217      166.48       9.210340
## 135             1 debt_consolidation   0.1249       70.25       9.433484
##       dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 74   6.45  677          6240.000     56411       75.3              0
## 75  20.00  772          4920.958       269        3.8              3
## 76  20.00  682          5492.000      3408       35.1              1
## 77  13.50  712          5368.958      6513       34.3              3
## 131  0.00  662          2250.000       875       54.7              2
## 135 14.40  662          1469.000       458       91.6              1
##     delinq.2yrs pub.rec not.fully.paid       purpose.fctr    .rnorm
## 74            0       0              1     small_business -1.287818
## 75            0       0              1   home_improvement -1.287818
## 76            0       0              1   home_improvement -1.287818
## 77            0       1              1 debt_consolidation -1.287818
## 131           1       0              1     small_business -1.287818
## 135           0       0              1 debt_consolidation -1.287818
##     not.fully.paid.predict.proba not.fully.paid.predict
## 74                    0.16388102                      0
## 75                    0.07508575                      0
## 76                    0.20113113                      0
## 77                    0.22392812                      0
## 131                   0.39678684                      0
## 135                   0.22456394                      0
##     not.fully.paid.preddmy.proba not.fully.paid.preddmy
## 74                     0.1600298                      0
## 75                     0.1600298                      0
## 76                     0.1600298                      0
## 77                     0.1600298                      0
## 131                    0.1600298                      0
## 135                    0.1600298                      0
##     not.fully.paid.fctr not.fully.paid.predict.accurate .label
## 74                    1                           FALSE    .74
## 75                    1                           FALSE    .75
## 76                    1                           FALSE    .76
## 77                    1                           FALSE    .77
## 131                   1                           FALSE   .131
## 135                   1                           FALSE   .135
##      credit.policy            purpose int.rate installment log.annual.inc
## 999              1 debt_consolidation   0.1008      171.22       11.30220
## 1363             1   home_improvement   0.1020      242.71       11.27663
## 1917             1          all_other   0.1253      127.18       11.00210
## 4641             1   home_improvement   0.1670      710.03       11.26446
## 6889             1   home_improvement   0.1183      331.34       11.28978
## 8600             0          all_other   0.1513       41.68       10.59663
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 999  14.79  712          6900.000     66235      97.40              0
## 1363 11.99  732          4686.958     42703      40.40              3
## 1917 20.14  687          4140.042     27759      71.70              1
## 4641 24.60  677          1748.958         0      49.63              1
## 6889 10.59  722          6422.042      6490       4.90              3
## 8600 24.81  657          3870.000      7809      85.80              5
##      delinq.2yrs pub.rec not.fully.paid       purpose.fctr    .rnorm
## 999            0       0              1 debt_consolidation -1.287818
## 1363           0       0              1   home_improvement -1.287818
## 1917           0       0              1          all_other -1.287818
## 4641           0       0              1   home_improvement -1.287818
## 6889           0       0              1   home_improvement -1.287818
## 8600           1       1              1          all_other -1.287818
##      not.fully.paid.predict.proba not.fully.paid.predict
## 999                    0.09469428                      0
## 1363                   0.14311524                      0
## 1917                   0.15737990                      0
## 4641                   0.29555428                      0
## 6889                   0.14695887                      0
## 8600                   0.39911449                      0
##      not.fully.paid.preddmy.proba not.fully.paid.preddmy
## 999                     0.1600298                      0
## 1363                    0.1600298                      0
## 1917                    0.1600298                      0
## 4641                    0.1600298                      0
## 6889                    0.1600298                      0
## 8600                    0.1600298                      0
##      not.fully.paid.fctr not.fully.paid.predict.accurate .label
## 999                    1                           FALSE   .999
## 1363                   1                           FALSE  .1363
## 1917                   1                           FALSE  .1917
## 4641                   1                           FALSE  .4641
## 6889                   1                           FALSE  .6889
## 8600                   1                           FALSE  .8600
##      credit.policy          purpose int.rate installment log.annual.inc
## 9474             0        all_other   0.1496      138.58        9.92329
## 9484             0        all_other   0.1774      432.26       10.27643
## 9514             0      credit_card   0.1253      384.87       11.00210
## 9549             0 home_improvement   0.1607       87.99       10.77896
## 9571             0        all_other   0.1671      113.63       10.64542
## 9575             0        all_other   0.1253      257.70       11.14186
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 9474  4.76  672          3180.042      2381       37.2              5
## 9484 11.36  682          5671.042     13328       93.9              7
## 9514 11.56  722          3614.000     25088       75.6              5
## 9549 14.20  667          4080.000      1530       36.4              7
## 9571 28.06  672          3210.042     25759       63.8              5
## 9575  0.21  722          4380.000       184        1.1              5
##      delinq.2yrs pub.rec not.fully.paid     purpose.fctr    .rnorm
## 9474           0       0              1        all_other -1.287818
## 9484           0       1              0        all_other -1.287818
## 9514           0       0              1      credit_card -1.287818
## 9549           0       0              1 home_improvement -1.287818
## 9571           0       0              1        all_other -1.287818
## 9575           0       0              1        all_other -1.287818
##      not.fully.paid.predict.proba not.fully.paid.predict
## 9474                    0.3584886                      0
## 9484                    0.5588186                      1
## 9514                    0.1601204                      0
## 9549                    0.3587328                      0
## 9571                    0.3346938                      0
## 9575                    0.1778953                      0
##      not.fully.paid.preddmy.proba not.fully.paid.preddmy
## 9474                    0.1600298                      0
## 9484                    0.1600298                      0
## 9514                    0.1600298                      0
## 9549                    0.1600298                      0
## 9571                    0.1600298                      0
## 9575                    0.1600298                      0
##      not.fully.paid.fctr not.fully.paid.predict.accurate .label
## 9474                   1                           FALSE  .9474
## 9484                   0                           FALSE  .9484
## 9514                   1                           FALSE  .9514
## 9549                   1                           FALSE  .9549
## 9571                   1                           FALSE  .9571
## 9575                   1                           FALSE  .9575
```

![](RiskDefault_files/figure-html/predict_newdata-9.png) 

```r
glb_newent_df$profit <- exp(glb_newent_df$int.rate*3) - 1      # all loans are for 3 years
glb_newent_df$profit[glb_newent_df$not.fully.paid == 1] <- -1
print(max(glb_newent_df$profit))
```

```
## [1] 0.8894769
```

```r
print(mean(glb_newent_df$profit))
```

```
## [1] 0.2094136
```

```r
glb_newent_high_interest_df <- subset(glb_newent_df, int.rate >= 0.15)
print(mean(glb_newent_high_interest_df$profit))
```

```
## [1] 0.2251015
```

```r
print(table(glb_newent_high_interest_df$not.fully.paid))
```

```
## 
##   0   1 
## 327 110
```

```r
print(risk_cutoff <- sort(glb_newent_high_interest_df$not.fully.paid.predict.proba, 
                    decreasing=FALSE)[100])
```

```
## [1] 0.1763305
```

```r
glb_newent_high_interest_sel_df <- 
    subset(glb_newent_high_interest_df, 
           not.fully.paid.predict.proba <= risk_cutoff)
print(sum(glb_newent_high_interest_sel_df$profit))
```

```
## [1] 31.27825
```

```r
print(table(glb_newent_high_interest_sel_df$not.fully.paid))
```

```
## 
##  0  1 
## 81 19
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.2 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] ROCR_1.0-6      gplots_2.16.0   mgcv_1.8-4      nlme_3.1-120   
##  [5] reshape2_1.4.1  plyr_1.8.1      caTools_1.17.1  doBy_4.5-13    
##  [9] survival_2.38-1 ggplot2_1.0.1  
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6       colorspace_1.2-6   digest_0.6.8      
##  [4] evaluate_0.5.5     formatR_1.0        gdata_2.13.3      
##  [7] grid_3.1.3         gtable_0.1.2       gtools_3.4.1      
## [10] htmltools_0.2.6    KernSmooth_2.23-14 knitr_1.9         
## [13] labeling_0.3       lattice_0.20-30    MASS_7.3-39       
## [16] Matrix_1.1-5       munsell_0.4.2      proto_0.3-10      
## [19] Rcpp_0.11.5        rmarkdown_0.5.1    scales_0.2.4      
## [22] splines_3.1.3      stringr_0.6.2      tools_3.1.3       
## [25] yaml_2.1.13
```
