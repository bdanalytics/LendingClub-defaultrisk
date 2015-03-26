# LendingClub DefaultRisk: not.fully.paid classification:: template2
bdanalytics  

**  **    
**Date: (Mon) Jun 29, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/loans.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
First run:
    <glb_sel_mdl_id>: Leaderboard: <accuracy>
        newobs_tbl=[0=, 1=]; submit_filename=
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=; opt.prob.threshold.OOB=
            <feat1>=<imp>; <feat1>=<imp>; <feat1>=<imp>; 
            <txt.feat1>=<imp>; <txt.feat1>=<imp>; <txt.feat1>=<imp>; 

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
# glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/loans.csv"
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/loans_imputed.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "template2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- FALSE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
    glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
    glb_split_sample.seed <- 144               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- TRUE # or TRUE or FALSE

glb_rsp_var_raw <- "not.fully.paid"

# for classification, the response variable has to be a factor
glb_rsp_var <- "notfullypaid.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
#     return(log(raw))
    ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA))
```

```
## [1] Y    Y    N    N    <NA>
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
#     return(exp(var))
    as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, NA)))
```

```
## [1]  1  1  0  0 NA
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# not.fully.paid: indicates that the loan was not paid back in full (the borrower either defaulted or the loan was "charged off," meaning the borrower was deemed unlikely to ever pay it back).
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

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- NULL # or c("<var1>")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

# Derived features
glb_derive_lst <- NULL;

# Add logs of numerics that are not distributed normally ->  do automatically ???

glb_derive_lst[["purpose.fctr"]] <- list(
#     mapfn=function(Rasmussen) { return(ifelse(sign(Rasmussen) >= 0, 1, 0)) }
    mapfn=function(purpose) { return(relevel(as.factor(purpose), ref="all_other")) }
        , args=c("purpose"))
#     mapfn=function(Week) { return(substr(Week, 1, 10)) }
#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }
#     , args=c("raw"))
#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))

# # If glb_allobs_df is not sorted in the desired manner
#     mapfn=function(Week) { return(coredata(lag(zoo(orderBy(~Week, glb_allobs_df)$ILI), -2, na.pad=TRUE))) }
#     mapfn=function(ILI) { return(coredata(lag(zoo(ILI), -2, na.pad=TRUE))) }
#     mapfn=function(ILI.2.lag) { return(log(ILI.2.lag)) }

# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]

glb_derive_vars <- names(glb_derive_lst)
# tst <- "purpose.fctr"; args_lst <- NULL; for (arg in glb_derive_lst[[tst]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; print(head(args_lst[[arg]])); print(head(drv_vals <- do.call(glb_derive_lst[[tst]]$mapfn, args_lst))); 
# print(which_ix <- which(args_lst[[arg]] == 0.75)); print(drv_vals[which_ix]); 

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("purpose") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # c("int.rate")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](LendingClub_template2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor    bgn end elapsed
## 1 import.data          1          0 14.262  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/loans_imputed.csv..."
## [1] "dimensions of data in ./data/loans_imputed.csv: 9,578 rows x 14 cols"
##   credit.policy            purpose int.rate installment log.annual.inc
## 1             1 debt_consolidation   0.1189      829.10       11.35041
## 2             1        credit_card   0.1071      228.22       11.08214
## 3             1 debt_consolidation   0.1357      366.86       10.37349
## 4             1 debt_consolidation   0.1008      162.34       11.35041
## 5             1        credit_card   0.1426      102.92       11.29973
## 6             1        credit_card   0.0788      125.13       11.90497
##     dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 1 19.48  737          5639.958     28854       52.1              0
## 2 14.29  707          2760.000     33623       76.7              0
## 3 11.63  682          4710.000      3511       25.6              1
## 4  8.10  712          2699.958     33667       73.2              1
## 5 14.97  667          4066.000      4740       39.5              0
## 6 16.98  727          6120.042     50807       51.0              0
##   delinq.2yrs pub.rec not.fully.paid
## 1           0       0              0
## 2           0       0              0
## 3           0       0              0
## 4           0       0              0
## 5           1       0              0
## 6           0       0              0
##      credit.policy            purpose int.rate installment log.annual.inc
## 331              1          all_other   0.0743       62.15       11.12715
## 1459             1     small_business   0.1493      346.32       11.31447
## 4878             1          all_other   0.1600       98.45       10.41619
## 6970             1 debt_consolidation   0.1565      848.42       11.38584
## 7043             1          all_other   0.0988      483.16       11.09741
## 9478             0 debt_consolidation   0.1357      254.77       10.53210
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 331   9.99  767          8190.042      2475        5.4              1
## 1459  8.68  682          5339.958         9        0.0              0
## 4878 14.77  662          1319.958       339       12.1              2
## 6970 15.72  687          3810.042     31710       93.5              0
## 7043  7.69  752          5400.042      8816       23.8              0
## 9478 13.02  687          2130.000      9451       33.3              4
##      delinq.2yrs pub.rec not.fully.paid
## 331            1       0              0
## 1459           0       0              0
## 4878           0       0              1
## 6970           0       1              0
## 7043           0       0              0
## 9478           0       0              1
##      credit.policy            purpose int.rate installment log.annual.inc
## 9573             0 debt_consolidation   0.1565       69.98       10.11047
## 9574             0          all_other   0.1461      344.76       12.18075
## 9575             0          all_other   0.1253      257.70       11.14186
## 9576             0 debt_consolidation   0.1071       97.81       10.59663
## 9577             0   home_improvement   0.1600      351.58       10.81978
## 9578             0 debt_consolidation   0.1392      853.43       11.26446
##        dti fico days.with.cr.line revol.bal revol.util inq.last.6mths
## 9573  7.02  662          8190.042      2999       39.5              6
## 9574 10.39  672         10474.000    215372       82.1              2
## 9575  0.21  722          4380.000       184        1.1              5
## 9576 13.09  687          3450.042     10036       82.9              8
## 9577 19.18  692          1800.000         0        3.2              5
## 9578 16.28  732          4740.000     37879       57.0              6
##      delinq.2yrs pub.rec not.fully.paid
## 9573           0       0              1
## 9574           0       0              1
## 9575           0       0              1
## 9576           0       0              1
## 9577           0       0              1
## 9578           0       0              1
## 'data.frame':	9578 obs. of  14 variables:
##  $ credit.policy    : int  1 1 1 1 1 1 1 1 1 1 ...
##  $ purpose          : chr  "debt_consolidation" "credit_card" "debt_consolidation" "debt_consolidation" ...
##  $ int.rate         : num  0.119 0.107 0.136 0.101 0.143 ...
##  $ installment      : num  829 228 367 162 103 ...
##  $ log.annual.inc   : num  11.4 11.1 10.4 11.4 11.3 ...
##  $ dti              : num  19.5 14.3 11.6 8.1 15 ...
##  $ fico             : int  737 707 682 712 667 727 667 722 682 707 ...
##  $ days.with.cr.line: num  5640 2760 4710 2700 4066 ...
##  $ revol.bal        : int  28854 33623 3511 33667 4740 50807 3839 24220 69909 5630 ...
##  $ revol.util       : num  52.1 76.7 25.6 73.2 39.5 51 76.8 68.6 51.1 23 ...
##  $ inq.last.6mths   : int  0 0 1 1 0 0 0 0 1 1 ...
##  $ delinq.2yrs      : int  0 0 0 0 1 0 0 0 0 0 ...
##  $ pub.rec          : int  0 0 0 0 0 0 1 0 0 0 ...
##  $ not.fully.paid   : int  0 0 0 0 0 0 1 1 0 0 ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
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
##  - attr(*, "comment")= chr "glb_newobs_df"
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
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
```

```
## Warning: using .rownames as identifiers for observations
```

```r
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 1  import.data          1          0 14.262 14.949   0.687
## 2 inspect.data          2          0 14.950     NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](LendingClub_template2_files/figure-html/inspect.data-1.png) 

```
##       not.fully.paid.0 not.fully.paid.1
## Test              2413              460
## Train             5632             1073
##       not.fully.paid.0 not.fully.paid.1
## Test         0.8398886        0.1601114
## Train        0.8399702        0.1600298
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##  credit.policy            dti      revol.bal     revol.util inq.last.6mths 
##           1868             89            321            297           3637 
##    delinq.2yrs        pub.rec not.fully.paid 
##           8458           9019           8045 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   purpose .rownames 
##         0         0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   not.fully.paid notfullypaid.fctr   .n
## 1              0                 N 8045
## 2              1                 Y 1533
```

![](LendingClub_template2_files/figure-html/inspect.data-2.png) 

```
##       notfullypaid.fctr.N notfullypaid.fctr.Y
## Test                 2413                 460
## Train                5632                1073
##       notfullypaid.fctr.N notfullypaid.fctr.Y
## Test            0.8398886           0.1601114
## Train           0.8399702           0.1600298
```

```r
# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: credit.policy"
```

![](LendingClub_template2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: int.rate"
```

![](LendingClub_template2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: installment"
```

![](LendingClub_template2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: log.annual.inc"
```

![](LendingClub_template2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: dti"
```

![](LendingClub_template2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: fico"
```

![](LendingClub_template2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: days.with.cr.line"
```

![](LendingClub_template2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: revol.bal"
```

![](LendingClub_template2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: revol.util"
```

![](LendingClub_template2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: inq.last.6mths"
```

![](LendingClub_template2_files/figure-html/inspect.data-12.png) 

```
## [1] "feat: delinq.2yrs"
```

![](LendingClub_template2_files/figure-html/inspect.data-13.png) 

```
## [1] "feat: pub.rec"
```

![](LendingClub_template2_files/figure-html/inspect.data-14.png) 

```
## [1] "feat: .rnorm"
```

![](LendingClub_template2_files/figure-html/inspect.data-15.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0 14.950 24.222   9.272
## 3   scrub.data          2          1 24.223     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##  credit.policy            dti      revol.bal     revol.util inq.last.6mths 
##           1868             89            321            297           3637 
##    delinq.2yrs        pub.rec not.fully.paid 
##           8458           9019           8045 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   purpose .rownames 
##         0         0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 24.223 26.155   1.932
## 4 transform.data          2          2 26.155     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

```
## [1] "Creating new feature: purpose.fctr..."
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 4   transform.data          2          2 26.155 26.218   0.063
## 5 extract.features          3          0 26.218     NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 26.225  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

#stop(here"); sav_allobs_df <- glb_allobs_df #; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    for (sfx in c("", ".POSIX"))
        glb_exclude_vars_as_features <- 
            union(glb_exclude_vars_as_features, 
                    paste(glb_date_vars, sfx, sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last2 <- as.numeric(merge(z-lag(z, -2), b, all=TRUE)); last2[is.na(last2)] <- 0
        glb_allobs_df[, paste0(feat, ".last2.log")] <- log(1 + last2)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last2.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last2.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}
rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn   end
## 1                extract.features_bgn          1          0 26.225 26.24
## 2 extract.features_factorize.str.vars          2          0 26.240    NA
##   elapsed
## 1   0.015
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##     purpose        .src   .rownames 
##   "purpose"      ".src" ".rownames"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}

if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 26.240 26.258
## 3                extract.features_end          3          0 26.259     NA
##   elapsed
## 2   0.018
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 26.240 26.258
## 1                extract.features_bgn          1          0 26.225 26.240
##   elapsed duration
## 2   0.018    0.018
## 1   0.015    0.015
## [1] "Total Elapsed Time: 26.258 secs"
```

![](LendingClub_template2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](LendingClub_template2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 26.218 27.547   1.329
## 6     cluster.data          4          0 27.548     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 27.548 27.896   0.348
## 7 manage.missing.data          4          1 27.897     NA      NA
```

```r
# If mice crashes with error: Error in get(as.character(FUN), mode = "function", envir = envir) : object 'State' of mode 'function' was not found
#   consider excluding 'State' as a feature

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##  credit.policy            dti      revol.bal     revol.util inq.last.6mths 
##           1868             89            321            297           3637 
##    delinq.2yrs        pub.rec not.fully.paid 
##           8458           9019           8045 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   purpose .rownames 
##         0         0
```

```r
# glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col],
                                                       inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    
    # complete(mice()) changes attributes of factors even though values don't change
    for (col in ret_vars) {
        if (inherits(out_impent_df[, col], "factor")) {
            if (identical(as.numeric(out_impent_df[, col]), 
                          as.numeric(inp_impent_df[, col])))
                ret_vars <- setdiff(ret_vars, col)
        }
    }
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
```

```
## named integer(0)
```

```r
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
##  credit.policy            dti      revol.bal     revol.util inq.last.6mths 
##           1868             89            321            297           3637 
##    delinq.2yrs        pub.rec not.fully.paid 
##           8458           9019           8045 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##   purpose .rownames 
##         0         0
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Last call for data modifications 
#stop(here") # sav_allobs_df <- glb_allobs_df
# glb_allobs_df[(glb_allobs_df$PropR == 0.75) & (glb_allobs_df$State == "Hawaii"), "PropR.fctr"] <- "N"

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 7 manage.missing.data          4          1 27.897 27.994   0.097
## 8     select.features          5          0 27.995     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                  id        cor.y exclude.as.feat
## not.fully.paid       not.fully.paid  1.000000000               1
## credit.policy         credit.policy -0.163108283               0
## int.rate                   int.rate  0.155449912               0
## inq.last.6mths       inq.last.6mths  0.153796715               0
## fico                           fico -0.153338871               0
## revol.util               revol.util  0.081111895               0
## pub.rec                     pub.rec  0.055223796               0
## revol.bal                 revol.bal  0.049656648               0
## dti                             dti  0.048904794               0
## installment             installment  0.047121296               0
## log.annual.inc       log.annual.inc -0.041998649               0
## days.with.cr.line days.with.cr.line -0.038840650               0
## purpose.fctr           purpose.fctr  0.037421545               0
## delinq.2yrs             delinq.2yrs  0.007991196               0
## .rnorm                       .rnorm  0.001175492               0
##                     cor.y.abs
## not.fully.paid    1.000000000
## credit.policy     0.163108283
## int.rate          0.155449912
## inq.last.6mths    0.153796715
## fico              0.153338871
## revol.util        0.081111895
## pub.rec           0.055223796
## revol.bal         0.049656648
## dti               0.048904794
## installment       0.047121296
## log.annual.inc    0.041998649
## days.with.cr.line 0.038840650
## purpose.fctr      0.037421545
## delinq.2yrs       0.007991196
## .rnorm            0.001175492
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## [1] "cor(fico, int.rate)=-0.7117"
## [1] "cor(notfullypaid.fctr, fico)=-0.1533"
## [1] "cor(notfullypaid.fctr, int.rate)=0.1554"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified fico as highly correlated with int.rate
```

```
##                   id        cor.y exclude.as.feat   cor.y.abs cor.high.X
## 11    not.fully.paid  1.000000000               1 1.000000000       <NA>
## 9           int.rate  0.155449912               0 0.155449912       <NA>
## 7     inq.last.6mths  0.153796715               0 0.153796715       <NA>
## 15        revol.util  0.081111895               0 0.081111895       <NA>
## 12           pub.rec  0.055223796               0 0.055223796       <NA>
## 14         revol.bal  0.049656648               0 0.049656648       <NA>
## 5                dti  0.048904794               0 0.048904794       <NA>
## 8        installment  0.047121296               0 0.047121296       <NA>
## 13      purpose.fctr  0.037421545               0 0.037421545       <NA>
## 4        delinq.2yrs  0.007991196               0 0.007991196       <NA>
## 1             .rnorm  0.001175492               0 0.001175492       <NA>
## 3  days.with.cr.line -0.038840650               0 0.038840650       <NA>
## 10    log.annual.inc -0.041998649               0 0.041998649       <NA>
## 6               fico -0.153338871               0 0.153338871   int.rate
## 2      credit.policy -0.163108283               0 0.163108283       <NA>
##    freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 11  5.248835    0.02982849   FALSE FALSE    FALSE            FALSE
## 9   1.235000    3.62416107   FALSE FALSE    FALSE            FALSE
## 7   1.499709    0.41759881   FALSE FALSE    FALSE            FALSE
## 15 12.058824   15.25727069   FALSE FALSE    FALSE            FALSE
## 12 16.237113    0.08948546   FALSE FALSE    FALSE            FALSE
## 14 28.375000   85.86129754   FALSE FALSE    FALSE            FALSE
## 5   4.500000   35.27218494   FALSE FALSE    FALSE            FALSE
## 8   1.166667   55.68978374   FALSE FALSE    FALSE            FALSE
## 13  1.674376    0.10439970   FALSE FALSE    FALSE            FALSE
## 4  10.555160    0.14914243   FALSE FALSE    FALSE            FALSE
## 1   1.000000  100.00000000   FALSE FALSE    FALSE            FALSE
## 3   1.090909   32.25950783   FALSE FALSE    FALSE            FALSE
## 10  1.213873   22.47576435   FALSE FALSE    FALSE            FALSE
## 6   1.068493    0.65622670   FALSE FALSE    FALSE            FALSE
## 2   4.130069    0.02982849   FALSE FALSE    FALSE            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning: Removed 5 rows containing missing values (geom_point).
```

```
## Warning: Removed 5 rows containing missing values (geom_point).
```

```
## Warning: Removed 5 rows containing missing values (geom_point).
```

![](LendingClub_template2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
## named integer(0)
## [1] "numeric data w/ 0s in : "
##  credit.policy            dti      revol.bal     revol.util inq.last.6mths 
##           1868             89            321            297           3637 
##    delinq.2yrs        pub.rec not.fully.paid 
##           8458           9019           8045 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##   purpose .rownames 
##         0         0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 27.995 29.112   1.117
## 9 partition.data.training          6          0 29.112     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for notfullypaid.fctr; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 15 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                                  id exclude.as.feat rsp_var
## notfullypaid.fctr notfullypaid.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                                  id cor.y exclude.as.feat cor.y.abs
## 11                   not.fully.paid     1            TRUE         1
## notfullypaid.fctr notfullypaid.fctr    NA            TRUE        NA
##                   cor.high.X freqRatio percentUnique zeroVar   nzv
## 11                      <NA>  5.248835    0.02982849   FALSE FALSE
## notfullypaid.fctr       <NA>        NA            NA      NA    NA
##                   myNearZV is.cor.y.abs.low interaction.feat rsp_var_raw
## 11                   FALSE            FALSE               NA        TRUE
## notfullypaid.fctr       NA               NA               NA          NA
##                   rsp_var
## 11                     NA
## notfullypaid.fctr    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 9578   20
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 6705   19
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 6705   19
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 2873   19
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 2873   19
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 29.112 29.423   0.311
## 10              fit.models          7          0 29.424     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[glb_feats_df$id == max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_feats_df$id == glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a higher correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl(model_id="Baseline", 
                         model_method="mybaseln_classfr",
                        indep_vars_vctr=glb_Baseline_mdl_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8399702 0.1600298 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
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
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8399702 0.1600298
## 2 0.8399702 0.1600298
## 3 0.8399702 0.1600298
## 4 0.8399702 0.1600298
## 5 0.8399702 0.1600298
## 6 0.8399702 0.1600298
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.MFO.myMFO_classfr.N
## 1                 N                                          5632
## 2                 Y                                          1073
##          Prediction
## Reference    N    Y
##         N 5632    0
##         Y 1073    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.399702e-01   0.000000e+00   8.309724e-01   8.486712e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   5.081492e-01  6.633506e-235 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8399702 0.1600298
## 2 0.8399702 0.1600298
## 3 0.8399702 0.1600298
## 4 0.8399702 0.1600298
## 5 0.8399702 0.1600298
## 6 0.8399702 0.1600298
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.MFO.myMFO_classfr.N
## 1                 N                                          2413
## 2                 Y                                           460
##          Prediction
## Reference    N    Y
##         N 2413    0
##         Y  460    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.398886e-01   0.000000e+00   8.259605e-01   8.531205e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   5.124443e-01  1.308142e-101 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.351                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8399702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8309724             0.8486712             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8398886
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8259605             0.8531205             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](LendingClub_template2_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.2759064
## 2        0.1 0.2759064
## 3        0.2 0.1707090
## 4        0.3 0.1707090
## 5        0.4 0.1707090
## 6        0.5 0.1707090
## 7        0.6 0.1707090
## 8        0.7 0.1707090
## 9        0.8 0.1707090
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Random.myrandom_classfr.Y
## 1                 N                                                5632
## 2                 Y                                                1073
##          Prediction
## Reference    N    Y
##         N    0 5632
##         Y    0 1073
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1600298      0.0000000      0.1513288      0.1690276      0.8399702 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](LendingClub_template2_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.2760276
## 2        0.1 0.2760276
## 3        0.2 0.1772973
## 4        0.3 0.1772973
## 5        0.4 0.1772973
## 6        0.5 0.1772973
## 7        0.6 0.1772973
## 8        0.7 0.1772973
## 9        0.8 0.1772973
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Random.myrandom_classfr.Y
## 1                 N                                                2413
## 2                 Y                                                 460
##          Prediction
## Reference    N    Y
##         N    0 2413
##         Y    0  460
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1601114      0.0000000      0.1468795      0.1740395      0.8398886 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.234                 0.002   0.5064397
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1       0.2759064        0.1600298
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1513288             0.1690276             0   0.5097686
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.2760276        0.1601114
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.1468795             0.1740395             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: credit.policy, int.rate"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.000466 on full training set
```

```
## Loading required package: rpart.plot
```

![](LendingClub_template2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 6705 
## 
##             CP nsplit rel error
## 1 0.0004659832      0         1
## 
## Node number 1: 6705 observations
##   predicted class=N  expected loss=0.1600298  P(node) =1
##     class counts:  5632  1073
##    probabilities: 0.840 0.160 
## 
## n= 6705 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 6705 1073 N (0.8399702 0.1600298) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1                 N                                             5632
## 2                 Y                                             1073
##          Prediction
## Reference    N    Y
##         N 5632    0
##         Y 1073    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.399702e-01   0.000000e+00   8.309724e-01   8.486712e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   5.081492e-01  6.633506e-235 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1                 N                                             2413
## 2                 Y                                              460
##          Prediction
## Reference    N    Y
##         N 2413    0
##         Y  460    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.398886e-01   0.000000e+00   8.259605e-01   8.531205e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   5.124443e-01  1.308142e-101 
##               model_id model_method                   feats
## 1 Max.cor.Y.cv.0.rpart        rpart credit.policy, int.rate
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.799                  0.03
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.8399702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8309724             0.8486712             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8398886
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8259605             0.8531205             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: credit.policy, int.rate"
## Fitting cp = 0 on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 6705 
## 
##             CP nsplit rel error
## 1 0.0004659832      0 1.0000000
## 2 0.0003106555     14 0.9934762
## 3 0.0002329916     23 0.9906803
## 4 0.0001331381     27 0.9897484
## 5 0.0000000000     34 0.9888164
## 
## Variable importance
##      int.rate credit.policy 
##            53            47 
## 
## Node number 1: 6705 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.1600298  P(node) =1
##     class counts:  5632  1073
##    probabilities: 0.840 0.160 
##   left son=2 (5398 obs) right son=3 (1307 obs)
##   Primary splits:
##       credit.policy < 0.5     to the right, improve=47.95629, (0 missing)
##       int.rate      < 0.09325 to the left,  improve=29.81654, (0 missing)
##   Surrogate splits:
##       int.rate < 0.1727  to the left,  agree=0.811, adj=0.033, (0 split)
## 
## Node number 2: 5398 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.1306039  P(node) =0.8050708
##     class counts:  4693   705
##    probabilities: 0.869 0.131 
##   left son=4 (1107 obs) right son=5 (4291 obs)
##   Primary splits:
##       int.rate < 0.09325 to the left,  improve=16.25838, (0 missing)
## 
## Node number 3: 1307 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.2815608  P(node) =0.1949292
##     class counts:   939   368
##    probabilities: 0.718 0.282 
##   left son=6 (67 obs) right son=7 (1240 obs)
##   Primary splits:
##       int.rate < 0.09355 to the left,  improve=3.713938, (0 missing)
## 
## Node number 4: 1107 observations
##   predicted class=N  expected loss=0.05420054  P(node) =0.1651007
##     class counts:  1047    60
##    probabilities: 0.946 0.054 
## 
## Node number 5: 4291 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.1503146  P(node) =0.6399702
##     class counts:  3646   645
##    probabilities: 0.850 0.150 
##   left son=10 (3943 obs) right son=11 (348 obs)
##   Primary splits:
##       int.rate < 0.15665 to the left,  improve=4.795618, (0 missing)
## 
## Node number 6: 67 observations
##   predicted class=N  expected loss=0.119403  P(node) =0.009992543
##     class counts:    59     8
##    probabilities: 0.881 0.119 
## 
## Node number 7: 1240 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.2903226  P(node) =0.1849366
##     class counts:   880   360
##    probabilities: 0.710 0.290 
##   left son=14 (929 obs) right son=15 (311 obs)
##   Primary splits:
##       int.rate < 0.1561  to the left,  improve=1.613355, (0 missing)
## 
## Node number 10: 3943 observations
##   predicted class=N  expected loss=0.1432919  P(node) =0.5880686
##     class counts:  3378   565
##    probabilities: 0.857 0.143 
## 
## Node number 11: 348 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.2298851  P(node) =0.05190157
##     class counts:   268    80
##    probabilities: 0.770 0.230 
##   left son=22 (324 obs) right son=23 (24 obs)
##   Primary splits:
##       int.rate < 0.19145 to the left,  improve=0.5517241, (0 missing)
## 
## Node number 14: 929 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.2755651  P(node) =0.1385533
##     class counts:   673   256
##    probabilities: 0.724 0.276 
##   left son=28 (59 obs) right son=29 (870 obs)
##   Primary splits:
##       int.rate < 0.1528  to the right, improve=1.417729, (0 missing)
## 
## Node number 15: 311 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.3344051  P(node) =0.0463833
##     class counts:   207   104
##    probabilities: 0.666 0.334 
##   left son=30 (284 obs) right son=31 (27 obs)
##   Primary splits:
##       int.rate < 0.1569  to the right, improve=1.27915, (0 missing)
## 
## Node number 22: 324 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.2222222  P(node) =0.04832215
##     class counts:   252    72
##    probabilities: 0.778 0.222 
##   left son=44 (13 obs) right son=45 (311 obs)
##   Primary splits:
##       int.rate < 0.1875  to the right, improve=0.5718526, (0 missing)
## 
## Node number 23: 24 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.003579418
##     class counts:    16     8
##    probabilities: 0.667 0.333 
## 
## Node number 28: 59 observations
##   predicted class=N  expected loss=0.1694915  P(node) =0.008799403
##     class counts:    49    10
##    probabilities: 0.831 0.169 
## 
## Node number 29: 870 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.2827586  P(node) =0.1297539
##     class counts:   624   246
##    probabilities: 0.717 0.283 
##   left son=58 (381 obs) right son=59 (489 obs)
##   Primary splits:
##       int.rate < 0.1352  to the right, improve=0.7119456, (0 missing)
## 
## Node number 30: 284 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.3204225  P(node) =0.04235645
##     class counts:   193    91
##    probabilities: 0.680 0.320 
##   left son=60 (147 obs) right son=61 (137 obs)
##   Primary splits:
##       int.rate < 0.1731  to the left,  improve=0.4745977, (0 missing)
## 
## Node number 31: 27 observations
##   predicted class=N  expected loss=0.4814815  P(node) =0.004026846
##     class counts:    14    13
##    probabilities: 0.519 0.481 
## 
## Node number 44: 13 observations
##   predicted class=N  expected loss=0.07692308  P(node) =0.001938852
##     class counts:    12     1
##    probabilities: 0.923 0.077 
## 
## Node number 45: 311 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.2282958  P(node) =0.0463833
##     class counts:   240    71
##    probabilities: 0.772 0.228 
##   left son=90 (138 obs) right son=91 (173 obs)
##   Primary splits:
##       int.rate < 0.16325 to the left,  improve=0.5287139, (0 missing)
## 
## Node number 58: 381 observations
##   predicted class=N  expected loss=0.2598425  P(node) =0.05682327
##     class counts:   282    99
##    probabilities: 0.740 0.260 
## 
## Node number 59: 489 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.3006135  P(node) =0.07293065
##     class counts:   342   147
##    probabilities: 0.699 0.301 
##   left son=118 (204 obs) right son=119 (285 obs)
##   Primary splits:
##       int.rate < 0.11875 to the left,  improve=0.902604, (0 missing)
## 
## Node number 60: 147 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.292517  P(node) =0.02192394
##     class counts:   104    43
##    probabilities: 0.707 0.293 
##   left son=120 (32 obs) right son=121 (115 obs)
##   Primary splits:
##       int.rate < 0.1677  to the right, improve=2.295711, (0 missing)
## 
## Node number 61: 137 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.350365  P(node) =0.02043251
##     class counts:    89    48
##    probabilities: 0.650 0.350 
##   left son=122 (111 obs) right son=123 (26 obs)
##   Primary splits:
##       int.rate < 0.17585 to the right, improve=1.437036, (0 missing)
## 
## Node number 90: 138 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.1956522  P(node) =0.02058166
##     class counts:   111    27
##    probabilities: 0.804 0.196 
##   left son=180 (19 obs) right son=181 (119 obs)
##   Primary splits:
##       int.rate < 0.1614  to the right, improve=1.686883, (0 missing)
## 
## Node number 91: 173 observations
##   predicted class=N  expected loss=0.2543353  P(node) =0.02580164
##     class counts:   129    44
##    probabilities: 0.746 0.254 
## 
## Node number 118: 204 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.2647059  P(node) =0.03042506
##     class counts:   150    54
##    probabilities: 0.735 0.265 
##   left son=236 (49 obs) right son=237 (155 obs)
##   Primary splits:
##       int.rate < 0.1135  to the right, improve=0.8469194, (0 missing)
## 
## Node number 119: 285 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.3263158  P(node) =0.04250559
##     class counts:   192    93
##    probabilities: 0.674 0.326 
##   left son=238 (221 obs) right son=239 (64 obs)
##   Primary splits:
##       int.rate < 0.1245  to the right, improve=1.054698, (0 missing)
## 
## Node number 120: 32 observations
##   predicted class=N  expected loss=0.125  P(node) =0.004772558
##     class counts:    28     4
##    probabilities: 0.875 0.125 
## 
## Node number 121: 115 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.3391304  P(node) =0.01715138
##     class counts:    76    39
##    probabilities: 0.661 0.339 
##   left son=242 (81 obs) right son=243 (34 obs)
##   Primary splits:
##       int.rate < 0.16375 to the left,  improve=0.5093366, (0 missing)
## 
## Node number 122: 111 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.3153153  P(node) =0.01655481
##     class counts:    76    35
##    probabilities: 0.685 0.315 
##   left son=244 (12 obs) right son=245 (99 obs)
##   Primary splits:
##       int.rate < 0.178   to the left,  improve=0.5945946, (0 missing)
## 
## Node number 123: 26 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.5  P(node) =0.003877703
##     class counts:    13    13
##    probabilities: 0.500 0.500 
##   left son=246 (13 obs) right son=247 (13 obs)
##   Primary splits:
##       int.rate < 0.174   to the left,  improve=0.6923077, (0 missing)
## 
## Node number 180: 19 observations
##   predicted class=N  expected loss=0  P(node) =0.002833706
##     class counts:    19     0
##    probabilities: 1.000 0.000 
## 
## Node number 181: 119 observations,    complexity param=0.0001331381
##   predicted class=N  expected loss=0.2268908  P(node) =0.01774795
##     class counts:    92    27
##    probabilities: 0.773 0.227 
##   left son=362 (112 obs) right son=363 (7 obs)
##   Primary splits:
##       int.rate < 0.1604  to the left,  improve=1.765756, (0 missing)
## 
## Node number 236: 49 observations
##   predicted class=N  expected loss=0.1836735  P(node) =0.007307979
##     class counts:    40     9
##    probabilities: 0.816 0.184 
## 
## Node number 237: 155 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.2903226  P(node) =0.02311708
##     class counts:   110    45
##    probabilities: 0.710 0.290 
##   left son=474 (144 obs) right son=475 (11 obs)
##   Primary splits:
##       int.rate < 0.1131  to the left,  improve=1.541422, (0 missing)
## 
## Node number 238: 221 observations,    complexity param=0.0002329916
##   predicted class=N  expected loss=0.3031674  P(node) =0.03296048
##     class counts:   154    67
##    probabilities: 0.697 0.303 
##   left son=476 (103 obs) right son=477 (118 obs)
##   Primary splits:
##       int.rate < 0.12895 to the left,  improve=0.6495495, (0 missing)
## 
## Node number 239: 64 observations,    complexity param=0.0004659832
##   predicted class=N  expected loss=0.40625  P(node) =0.009545116
##     class counts:    38    26
##    probabilities: 0.594 0.406 
##   left son=478 (47 obs) right son=479 (17 obs)
##   Primary splits:
##       int.rate < 0.12255 to the left,  improve=1.533323, (0 missing)
## 
## Node number 242: 81 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.308642  P(node) =0.01208054
##     class counts:    56    25
##    probabilities: 0.691 0.309 
##   left son=484 (8 obs) right son=485 (73 obs)
##   Primary splits:
##       int.rate < 0.163   to the right, improve=0.5987232, (0 missing)
## 
## Node number 243: 34 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.4117647  P(node) =0.005070843
##     class counts:    20    14
##    probabilities: 0.588 0.412 
##   left son=486 (12 obs) right son=487 (22 obs)
##   Primary splits:
##       int.rate < 0.1664  to the right, improve=0.9705882, (0 missing)
## 
## Node number 244: 12 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.001789709
##     class counts:    10     2
##    probabilities: 0.833 0.167 
## 
## Node number 245: 99 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.3333333  P(node) =0.0147651
##     class counts:    66    33
##    probabilities: 0.667 0.333 
##   left son=490 (76 obs) right son=491 (23 obs)
##   Primary splits:
##       int.rate < 0.1825  to the right, improve=0.6167048, (0 missing)
## 
## Node number 246: 13 observations
##   predicted class=N  expected loss=0.3846154  P(node) =0.001938852
##     class counts:     8     5
##    probabilities: 0.615 0.385 
## 
## Node number 247: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.001938852
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 362: 112 observations
##   predicted class=N  expected loss=0.2053571  P(node) =0.01670395
##     class counts:    89    23
##    probabilities: 0.795 0.205 
## 
## Node number 363: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001043997
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 474: 144 observations
##   predicted class=N  expected loss=0.2708333  P(node) =0.02147651
##     class counts:   105    39
##    probabilities: 0.729 0.271 
## 
## Node number 475: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.001640567
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 476: 103 observations
##   predicted class=N  expected loss=0.2621359  P(node) =0.01536167
##     class counts:    76    27
##    probabilities: 0.738 0.262 
## 
## Node number 477: 118 observations,    complexity param=0.0002329916
##   predicted class=N  expected loss=0.3389831  P(node) =0.01759881
##     class counts:    78    40
##    probabilities: 0.661 0.339 
##   left son=954 (68 obs) right son=955 (50 obs)
##   Primary splits:
##       int.rate < 0.13195 to the right, improve=0.2919442, (0 missing)
## 
## Node number 478: 47 observations
##   predicted class=N  expected loss=0.3404255  P(node) =0.007009694
##     class counts:    31    16
##    probabilities: 0.660 0.340 
## 
## Node number 479: 17 observations
##   predicted class=Y  expected loss=0.4117647  P(node) =0.002535421
##     class counts:     7    10
##    probabilities: 0.412 0.588 
## 
## Node number 484: 8 observations
##   predicted class=N  expected loss=0.125  P(node) =0.001193139
##     class counts:     7     1
##    probabilities: 0.875 0.125 
## 
## Node number 485: 73 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.3287671  P(node) =0.0108874
##     class counts:    49    24
##    probabilities: 0.671 0.329 
##   left son=970 (66 obs) right son=971 (7 obs)
##   Primary splits:
##       int.rate < 0.16215 to the left,  improve=0.9118188, (0 missing)
## 
## Node number 486: 12 observations
##   predicted class=N  expected loss=0.25  P(node) =0.001789709
##     class counts:     9     3
##    probabilities: 0.750 0.250 
## 
## Node number 487: 22 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.5  P(node) =0.003281133
##     class counts:    11    11
##    probabilities: 0.500 0.500 
##   left son=974 (9 obs) right son=975 (13 obs)
##   Primary splits:
##       int.rate < 0.16455 to the left,  improve=0.09401709, (0 missing)
## 
## Node number 490: 76 observations
##   predicted class=N  expected loss=0.3026316  P(node) =0.01133482
##     class counts:    53    23
##    probabilities: 0.697 0.303 
## 
## Node number 491: 23 observations,    complexity param=0.0003106555
##   predicted class=N  expected loss=0.4347826  P(node) =0.003430276
##     class counts:    13    10
##    probabilities: 0.565 0.435 
##   left son=982 (14 obs) right son=983 (9 obs)
##   Primary splits:
##       int.rate < 0.18005 to the left,  improve=0.431332, (0 missing)
## 
## Node number 954: 68 observations,    complexity param=0.0002329916
##   predicted class=N  expected loss=0.3088235  P(node) =0.01014169
##     class counts:    47    21
##    probabilities: 0.691 0.309 
##   left son=1908 (29 obs) right son=1909 (39 obs)
##   Primary splits:
##       int.rate < 0.1327  to the left,  improve=0.4600042, (0 missing)
## 
## Node number 955: 50 observations
##   predicted class=N  expected loss=0.38  P(node) =0.007457122
##     class counts:    31    19
##    probabilities: 0.620 0.380 
## 
## Node number 970: 66 observations
##   predicted class=N  expected loss=0.3030303  P(node) =0.0098434
##     class counts:    46    20
##    probabilities: 0.697 0.303 
## 
## Node number 971: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001043997
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 974: 9 observations
##   predicted class=N  expected loss=0.4444444  P(node) =0.001342282
##     class counts:     5     4
##    probabilities: 0.556 0.444 
## 
## Node number 975: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.001938852
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 982: 14 observations
##   predicted class=N  expected loss=0.3571429  P(node) =0.002087994
##     class counts:     9     5
##    probabilities: 0.643 0.357 
## 
## Node number 983: 9 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.001342282
##     class counts:     4     5
##    probabilities: 0.444 0.556 
## 
## Node number 1908: 29 observations
##   predicted class=N  expected loss=0.2413793  P(node) =0.00432513
##     class counts:    22     7
##    probabilities: 0.759 0.241 
## 
## Node number 1909: 39 observations,    complexity param=0.0002329916
##   predicted class=N  expected loss=0.3589744  P(node) =0.005816555
##     class counts:    25    14
##    probabilities: 0.641 0.359 
##   left son=3818 (28 obs) right son=3819 (11 obs)
##   Primary splits:
##       int.rate < 0.13395 to the right, improve=1.065601, (0 missing)
## 
## Node number 3818: 28 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.004175988
##     class counts:    20     8
##    probabilities: 0.714 0.286 
## 
## Node number 3819: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.001640567
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## n= 6705 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##    1) root 6705 1073 N (0.83997017 0.16002983)  
##      2) credit.policy>=0.5 5398  705 N (0.86939607 0.13060393)  
##        4) int.rate< 0.09325 1107   60 N (0.94579946 0.05420054) *
##        5) int.rate>=0.09325 4291  645 N (0.84968539 0.15031461)  
##         10) int.rate< 0.15665 3943  565 N (0.85670809 0.14329191) *
##         11) int.rate>=0.15665 348   80 N (0.77011494 0.22988506)  
##           22) int.rate< 0.19145 324   72 N (0.77777778 0.22222222)  
##             44) int.rate>=0.1875 13    1 N (0.92307692 0.07692308) *
##             45) int.rate< 0.1875 311   71 N (0.77170418 0.22829582)  
##               90) int.rate< 0.16325 138   27 N (0.80434783 0.19565217)  
##                180) int.rate>=0.1614 19    0 N (1.00000000 0.00000000) *
##                181) int.rate< 0.1614 119   27 N (0.77310924 0.22689076)  
##                  362) int.rate< 0.1604 112   23 N (0.79464286 0.20535714) *
##                  363) int.rate>=0.1604 7    3 Y (0.42857143 0.57142857) *
##               91) int.rate>=0.16325 173   44 N (0.74566474 0.25433526) *
##           23) int.rate>=0.19145 24    8 N (0.66666667 0.33333333) *
##      3) credit.policy< 0.5 1307  368 N (0.71843917 0.28156083)  
##        6) int.rate< 0.09355 67    8 N (0.88059701 0.11940299) *
##        7) int.rate>=0.09355 1240  360 N (0.70967742 0.29032258)  
##         14) int.rate< 0.1561 929  256 N (0.72443488 0.27556512)  
##           28) int.rate>=0.1528 59   10 N (0.83050847 0.16949153) *
##           29) int.rate< 0.1528 870  246 N (0.71724138 0.28275862)  
##             58) int.rate>=0.1352 381   99 N (0.74015748 0.25984252) *
##             59) int.rate< 0.1352 489  147 N (0.69938650 0.30061350)  
##              118) int.rate< 0.11875 204   54 N (0.73529412 0.26470588)  
##                236) int.rate>=0.1135 49    9 N (0.81632653 0.18367347) *
##                237) int.rate< 0.1135 155   45 N (0.70967742 0.29032258)  
##                  474) int.rate< 0.1131 144   39 N (0.72916667 0.27083333) *
##                  475) int.rate>=0.1131 11    5 Y (0.45454545 0.54545455) *
##              119) int.rate>=0.11875 285   93 N (0.67368421 0.32631579)  
##                238) int.rate>=0.1245 221   67 N (0.69683258 0.30316742)  
##                  476) int.rate< 0.12895 103   27 N (0.73786408 0.26213592) *
##                  477) int.rate>=0.12895 118   40 N (0.66101695 0.33898305)  
##                    954) int.rate>=0.13195 68   21 N (0.69117647 0.30882353)  
##                     1908) int.rate< 0.1327 29    7 N (0.75862069 0.24137931) *
##                     1909) int.rate>=0.1327 39   14 N (0.64102564 0.35897436)  
##                       3818) int.rate>=0.13395 28    8 N (0.71428571 0.28571429) *
##                       3819) int.rate< 0.13395 11    5 Y (0.45454545 0.54545455) *
##                    955) int.rate< 0.13195 50   19 N (0.62000000 0.38000000) *
##                239) int.rate< 0.1245 64   26 N (0.59375000 0.40625000)  
##                  478) int.rate< 0.12255 47   16 N (0.65957447 0.34042553) *
##                  479) int.rate>=0.12255 17    7 Y (0.41176471 0.58823529) *
##         15) int.rate>=0.1561 311  104 N (0.66559486 0.33440514)  
##           30) int.rate>=0.1569 284   91 N (0.67957746 0.32042254)  
##             60) int.rate< 0.1731 147   43 N (0.70748299 0.29251701)  
##              120) int.rate>=0.1677 32    4 N (0.87500000 0.12500000) *
##              121) int.rate< 0.1677 115   39 N (0.66086957 0.33913043)  
##                242) int.rate< 0.16375 81   25 N (0.69135802 0.30864198)  
##                  484) int.rate>=0.163 8    1 N (0.87500000 0.12500000) *
##                  485) int.rate< 0.163 73   24 N (0.67123288 0.32876712)  
##                    970) int.rate< 0.16215 66   20 N (0.69696970 0.30303030) *
##                    971) int.rate>=0.16215 7    3 Y (0.42857143 0.57142857) *
##                243) int.rate>=0.16375 34   14 N (0.58823529 0.41176471)  
##                  486) int.rate>=0.1664 12    3 N (0.75000000 0.25000000) *
##                  487) int.rate< 0.1664 22   11 N (0.50000000 0.50000000)  
##                    974) int.rate< 0.16455 9    4 N (0.55555556 0.44444444) *
##                    975) int.rate>=0.16455 13    6 Y (0.46153846 0.53846154) *
##             61) int.rate>=0.1731 137   48 N (0.64963504 0.35036496)  
##              122) int.rate>=0.17585 111   35 N (0.68468468 0.31531532)  
##                244) int.rate< 0.178 12    2 N (0.83333333 0.16666667) *
##                245) int.rate>=0.178 99   33 N (0.66666667 0.33333333)  
##                  490) int.rate>=0.1825 76   23 N (0.69736842 0.30263158) *
##                  491) int.rate< 0.1825 23   10 N (0.56521739 0.43478261)  
##                    982) int.rate< 0.18005 14    5 N (0.64285714 0.35714286) *
##                    983) int.rate>=0.18005 9    4 Y (0.44444444 0.55555556) *
##              123) int.rate< 0.17585 26   13 N (0.50000000 0.50000000)  
##                246) int.rate< 0.174 13    5 N (0.61538462 0.38461538) *
##                247) int.rate>=0.174 13    5 Y (0.38461538 0.61538462) *
##           31) int.rate< 0.1569 27   13 N (0.51851852 0.48148148) *
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-7.png) 

```
##    threshold    f.score
## 1        0.0 0.27590640
## 2        0.1 0.30486519
## 3        0.2 0.33454840
## 4        0.3 0.21923336
## 5        0.4 0.11194653
## 6        0.5 0.08613264
## 7        0.6 0.01473297
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1                 N                                                  4649
## 2                 Y                                                   660
##   notfullypaid.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                   983
## 2                                                   413
##          Prediction
## Reference    N    Y
##         N 4649  983
##         Y  660  413
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.549590e-01   1.875172e-01   7.444798e-01   7.652160e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.958256e-15 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-9.png) 

```
##    threshold    f.score
## 1        0.0 0.27602760
## 2        0.1 0.30350195
## 3        0.2 0.29318394
## 4        0.3 0.14218009
## 5        0.4 0.04339250
## 6        0.5 0.04056795
## 7        0.6 0.01287554
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1                 N                                                   475
## 2                 Y                                                    31
##   notfullypaid.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                  1938
## 2                                                   429
##          Prediction
## Reference    N    Y
##         N  475 1938
##         Y   31  429
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.31465367     0.04834758     0.29769496     0.33199069     0.83988862 
## AccuracyPValue  McnemarPValue 
##     1.00000000     0.00000000 
##                    model_id model_method                   feats
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart credit.policy, int.rate
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.476                 0.017
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.648915                    0.2       0.3345484         0.754959
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7444798              0.765216     0.1875172   0.6197157
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3035019        0.3146537
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.297695             0.3319907    0.04834758
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: credit.policy, int.rate"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.000466 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](LendingClub_template2_files/figure-html/fit.models_0-11.png) ![](LendingClub_template2_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 6705 
## 
##             CP nsplit rel error
## 1 0.0004659832      0         1
## 
## Node number 1: 6705 observations
##   predicted class=N  expected loss=0.1600298  P(node) =1
##     class counts:  5632  1073
##    probabilities: 0.840 0.160 
## 
## n= 6705 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 6705 1073 N (0.8399702 0.1600298) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.rpart.N
## 1                 N                                        5632
## 2                 Y                                        1073
##          Prediction
## Reference    N    Y
##         N 5632    0
##         Y 1073    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.399702e-01   0.000000e+00   8.309724e-01   8.486712e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   5.081492e-01  6.633506e-235 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.rpart.N
## 1                 N                                        2413
## 2                 Y                                         460
##          Prediction
## Reference    N    Y
##         N 2413    0
##         Y  460    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.398886e-01   0.000000e+00   8.259605e-01   8.531205e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   5.124443e-01  1.308142e-101 
##          model_id model_method                   feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart credit.policy, int.rate               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.325                 0.023         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8340045
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8309724             0.8486712    0.00186345         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8398886
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8259605             0.8531205             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.005712369     0.002541372
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: credit.policy, int.rate"
## Aggregating results
## Fitting final model on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_0-13.png) ![](LendingClub_template2_files/figure-html/fit.models_0-14.png) ![](LendingClub_template2_files/figure-html/fit.models_0-15.png) ![](LendingClub_template2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1522  -0.5956  -0.5187  -0.4298   2.2810  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)    -2.6619     0.1995 -13.341   <2e-16 ***
## credit.policy  -0.7274     0.0779  -9.338   <2e-16 ***
## int.rate       12.2697     1.3321   9.211   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5651.4  on 6702  degrees of freedom
## AIC: 5657.4
## 
## Number of Fisher Scoring iterations: 4
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-17.png) 

```
##    threshold    f.score
## 1        0.0 0.27590640
## 2        0.1 0.30436127
## 3        0.2 0.32250203
## 4        0.3 0.18477554
## 5        0.4 0.03174603
## 6        0.5 0.00000000
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.glm.N
## 1                 N                                      4640
## 2                 Y                                       676
##   notfullypaid.fctr.predict.Max.cor.Y.glm.Y
## 1                                       992
## 2                                       397
##          Prediction
## Reference    N    Y
##         N 4640  992
##         Y  676  397
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.512304e-01   1.732084e-01   7.407005e-01   7.615414e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.230790e-14 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-19.png) 

```
##    threshold    f.score
## 1        0.0 0.27602760
## 2        0.1 0.31179263
## 3        0.2 0.31127679
## 4        0.3 0.17177914
## 5        0.4 0.02880658
## 6        0.5 0.00000000
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Max.cor.Y.glm.N
## 1                 N                                       561
## 2                 Y                                        33
##   notfullypaid.fctr.predict.Max.cor.Y.glm.Y
## 1                                      1852
## 2                                       427
##          Prediction
## Reference    N    Y
##         N  561 1852
##         Y   33  427
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.3438914      0.0618214      0.3265145      0.3615868      0.8398886 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
##        model_id model_method                   feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm credit.policy, int.rate               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.109                 0.043    0.650489
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2        0.322502        0.8399702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7407005             0.7615414   0.002512178     0.65265
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3117926        0.3438914
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.3265145             0.3615868     0.0618214    5657.364
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0002583223     0.004351221
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: credit.policy, int.rate, credit.policy:int.rate"
## Aggregating results
## Fitting final model on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_0-21.png) ![](LendingClub_template2_files/figure-html/fit.models_0-22.png) ![](LendingClub_template2_files/figure-html/fit.models_0-23.png) ![](LendingClub_template2_files/figure-html/fit.models_0-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.9701  -0.6108  -0.5133  -0.4046   2.3515  
## 
## Coefficients:
##                          Estimate Std. Error z value Pr(>|z|)    
## (Intercept)               -1.6561     0.3368  -4.917 8.78e-07 ***
## credit.policy             -2.1343     0.3952  -5.401 6.63e-08 ***
## int.rate                   5.1539     2.3594   2.184 0.028933 *  
## `credit.policy:int.rate`  10.3152     2.8552   3.613 0.000303 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5638.4  on 6701  degrees of freedom
## AIC: 5646.4
## 
## Number of Fisher Scoring iterations: 4
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-25.png) 

```
##    threshold   f.score
## 1        0.0 0.2759064
## 2        0.1 0.3070765
## 3        0.2 0.3257821
## 4        0.3 0.1438849
## 5        0.4 0.0000000
## 6        0.5 0.0000000
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Interact.High.cor.Y.glm.N
## 1                 N                                                4377
## 2                 Y                                                 620
##   notfullypaid.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                                1255
## 2                                                 453
##          Prediction
## Reference    N    Y
##         N 4377 1255
##         Y  620  453
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.203579e-01   1.608253e-01   7.094462e-01   7.310778e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.524319e-48 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-27.png) 

```
##    threshold   f.score
## 1        0.0 0.2760276
## 2        0.1 0.3151561
## 3        0.2 0.3286540
## 4        0.3 0.1505017
## 5        0.4 0.0000000
## 6        0.5 0.0000000
## 7        0.6 0.0000000
## 8        0.7 0.0000000
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Interact.High.cor.Y.glm.N
## 1                 N                                                1861
## 2                 Y                                                 261
##   notfullypaid.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                                 552
## 2                                                 199
##          Prediction
## Reference    N    Y
##         N 1861  552
##         Y  261  199
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.170205e-01   1.622984e-01   7.001600e-01   7.334379e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.678547e-24 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                             feats max.nTuningRuns
## 1 credit.policy, int.rate, credit.policy:int.rate               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.074                 0.038   0.6500808
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.3257821        0.8399702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7094462             0.7310778             0   0.6530158
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2        0.328654        0.7170205
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1               0.70016             0.7334379     0.1622984    5646.416
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0002583223               0
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, credit.policy"
## Aggregating results
## Fitting final model on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_0-29.png) ![](LendingClub_template2_files/figure-html/fit.models_0-30.png) ![](LendingClub_template2_files/figure-html/fit.models_0-31.png) ![](LendingClub_template2_files/figure-html/fit.models_0-32.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1127  -0.6100  -0.4965  -0.3827   2.5104  
## 
## Coefficients:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     1.810e+00  7.605e-01   2.380 0.017300 *  
## int.rate                        7.455e+00  1.648e+00   4.523 6.11e-06 ***
## inq.last.6mths                  8.383e-02  1.578e-02   5.312 1.08e-07 ***
## revol.util                      4.745e-03  1.439e-03   3.298 0.000975 ***
## pub.rec                         4.085e-01  1.135e-01   3.600 0.000319 ***
## revol.bal                       2.748e-06  1.151e-06   2.388 0.016936 *  
## dti                             5.035e-03  5.461e-03   0.922 0.356470    
## installment                     9.157e-04  1.977e-04   4.631 3.63e-06 ***
## purpose.fctrcredit_card        -5.880e-01  1.342e-01  -4.381 1.18e-05 ***
## purpose.fctrdebt_consolidation -2.922e-01  9.164e-02  -3.188 0.001431 ** 
## purpose.fctreducational         1.491e-01  1.749e-01   0.852 0.393962    
## purpose.fctrhome_improvement    1.564e-01  1.472e-01   1.062 0.288234    
## purpose.fctrmajor_purchase     -5.126e-01  2.005e-01  -2.557 0.010570 *  
## purpose.fctrsmall_business      2.941e-01  1.400e-01   2.101 0.035680 *  
## delinq.2yrs                    -1.768e-03  6.244e-02  -0.028 0.977410    
## .rnorm                          1.813e-02  3.433e-02   0.528 0.597363    
## days.with.cr.line              -1.761e-05  1.556e-05  -1.132 0.257742    
## log.annual.inc                 -4.273e-01  7.130e-02  -5.993 2.06e-09 ***
## credit.policy                  -4.492e-01  9.909e-02  -4.533 5.82e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5515.4  on 6686  degrees of freedom
## AIC: 5553.4
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-33.png) 

```
##    threshold     f.score
## 1        0.0 0.275906403
## 2        0.1 0.320656726
## 3        0.2 0.358897990
## 4        0.3 0.241826033
## 5        0.4 0.131641554
## 6        0.5 0.047914818
## 7        0.6 0.018348624
## 8        0.7 0.003703704
## 9        0.8 0.001858736
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-34.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Low.cor.X.glm.N
## 1                 N                                      4501
## 2                 Y                                       591
##   notfullypaid.fctr.predict.Low.cor.X.glm.Y
## 1                                      1131
## 2                                       482
##          Prediction
## Reference    N    Y
##         N 4501 1131
##         Y  591  482
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.431767e-01   2.063579e-01   7.325407e-01   7.536009e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.414554e-38 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_0-35.png) 

```
##    threshold     f.score
## 1        0.0 0.276027603
## 2        0.1 0.318896129
## 3        0.2 0.324710080
## 4        0.3 0.247023810
## 5        0.4 0.089719626
## 6        0.5 0.033264033
## 7        0.6 0.008583691
## 8        0.7 0.004338395
## 9        0.8 0.004338395
## 10       0.9 0.004338395
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Low.cor.X.glm.N
## 1                 N                                      1934
## 2                 Y                                       278
##   notfullypaid.fctr.predict.Low.cor.X.glm.Y
## 1                                       479
## 2                                       182
##          Prediction
## Reference    N    Y
##         N 1934  479
##         Y  278  182
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.365124e-01   1.675215e-01   7.199944e-01   7.525473e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   3.618271e-13 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                             feats
## 1 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, credit.policy
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       1.15                 0.099
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.6822388                    0.2        0.358898        0.8386279
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7325407             0.7536009    0.02648994   0.6688742
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3247101        0.7365124
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7199944             0.7525473     0.1675215    5553.354
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0006834565      0.01795044
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 29.424 63.786  34.362
## 11 fit.models          7          1 63.786     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor  bgn end elapsed
## 1 fit.models_1_bgn          1          0 71.3  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 71.300 71.316   0.016
## 2 fit.models_1_glm          2          0 71.317     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy"
## Aggregating results
## Fitting final model on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_1-1.png) ![](LendingClub_template2_files/figure-html/fit.models_1-2.png) ![](LendingClub_template2_files/figure-html/fit.models_1-3.png) ![](LendingClub_template2_files/figure-html/fit.models_1-4.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2080  -0.6201  -0.4954  -0.3609   2.6575  
## 
## Coefficients:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     9.203e+00  1.554e+00   5.920 3.21e-09 ***
## int.rate                        6.009e-01  2.084e+00   0.288 0.773111    
## inq.last.6mths                  8.421e-02  1.600e-02   5.265 1.40e-07 ***
## revol.util                      1.839e-03  1.535e-03   1.198 0.230757    
## pub.rec                         3.337e-01  1.140e-01   2.927 0.003419 ** 
## revol.bal                       3.096e-06  1.169e-06   2.648 0.008087 ** 
## dti                             4.574e-03  5.504e-03   0.831 0.405884    
## installment                     1.277e-03  2.092e-04   6.101 1.05e-09 ***
## purpose.fctrcredit_card        -6.160e-01  1.345e-01  -4.580 4.65e-06 ***
## purpose.fctrdebt_consolidation -3.225e-01  9.186e-02  -3.510 0.000448 ***
## purpose.fctreducational         1.341e-01  1.753e-01   0.765 0.444539    
## purpose.fctrhome_improvement    1.725e-01  1.480e-01   1.166 0.243664    
## purpose.fctrmajor_purchase     -4.846e-01  2.009e-01  -2.412 0.015857 *  
## purpose.fctrsmall_business      4.093e-01  1.419e-01   2.883 0.003934 ** 
## delinq.2yrs                    -8.385e-02  6.562e-02  -1.278 0.201308    
## .rnorm                          2.071e-02  3.442e-02   0.602 0.547436    
## days.with.cr.line               2.432e-06  1.588e-05   0.153 0.878327    
## log.annual.inc                 -4.341e-01  7.150e-02  -6.071 1.27e-09 ***
## fico                           -9.329e-03  1.710e-03  -5.456 4.87e-08 ***
## credit.policy                  -3.384e-01  1.011e-01  -3.347 0.000818 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5484.8  on 6685  degrees of freedom
## AIC: 5524.8
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-5.png) 

```
##    threshold     f.score
## 1        0.0 0.275906403
## 2        0.1 0.321307012
## 3        0.2 0.352384419
## 4        0.3 0.248292986
## 5        0.4 0.138535032
## 6        0.5 0.053003534
## 7        0.6 0.018315018
## 8        0.7 0.003703704
## 9        0.8 0.003714020
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-6.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.glm.N
## 1                 N                                  4442
## 2                 Y                                   589
##   notfullypaid.fctr.predict.All.X.glm.Y
## 1                                  1190
## 2                                   484
##          Prediction
## Reference    N    Y
##         N 4442 1190
##         Y  589  484
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.346756e-01   1.954668e-01   7.239327e-01   7.452141e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   6.377500e-46 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-7.png) 

```
##    threshold     f.score
## 1        0.0 0.276027603
## 2        0.1 0.321584158
## 3        0.2 0.340535869
## 4        0.3 0.241430700
## 5        0.4 0.106870229
## 6        0.5 0.016771488
## 7        0.6 0.008602151
## 8        0.7 0.004329004
## 9        0.8 0.004338395
## 10       0.9 0.004338395
## 11       1.0 0.000000000
```

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.glm.N
## 1                 N                                  1913
## 2                 Y                                   263
##   notfullypaid.fctr.predict.All.X.glm.Y
## 1                                   500
## 2                                   197
##          Prediction
## Reference    N    Y
##         N 1913  500
##         Y  263  197
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.344239e-01   1.829127e-01   7.178674e-01   7.505017e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.299084e-17 
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                                                                   feats
## 1 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.224                 0.115
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.6862765                    0.2       0.3523844         0.838777
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7239327             0.7452141    0.03380809   0.6714373
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3405359        0.7344239
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7178674             0.7505017     0.1829127     5524.85
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0005166445      0.02298986
##                   label step_major step_minor    bgn    end elapsed
## 2      fit.models_1_glm          2          0 71.317 81.918  10.601
## 3 fit.models_1_bayesglm          3          0 81.919     NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/HW3_LendingClub_DefaultRisk
```

![](LendingClub_template2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2060  -0.6200  -0.4958  -0.3614   2.6557  
## 
## Coefficients:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     9.142e+00  1.549e+00   5.904 3.56e-09 ***
## int.rate                        6.588e-01  2.077e+00   0.317 0.751115    
## inq.last.6mths                  8.412e-02  1.597e-02   5.267 1.38e-07 ***
## revol.util                      1.841e-03  1.532e-03   1.202 0.229341    
## pub.rec                         3.334e-01  1.139e-01   2.927 0.003427 ** 
## revol.bal                       3.090e-06  1.167e-06   2.647 0.008113 ** 
## dti                             4.560e-03  5.497e-03   0.830 0.406769    
## installment                     1.270e-03  2.088e-04   6.084 1.17e-09 ***
## purpose.fctrcredit_card        -6.123e-01  1.339e-01  -4.572 4.82e-06 ***
## purpose.fctrdebt_consolidation -3.206e-01  9.153e-02  -3.502 0.000461 ***
## purpose.fctreducational         1.343e-01  1.744e-01   0.770 0.441227    
## purpose.fctrhome_improvement    1.723e-01  1.474e-01   1.169 0.242254    
## purpose.fctrmajor_purchase     -4.787e-01  1.993e-01  -2.402 0.016293 *  
## purpose.fctrsmall_business      4.079e-01  1.414e-01   2.886 0.003906 ** 
## delinq.2yrs                    -8.345e-02  6.553e-02  -1.274 0.202827    
## .rnorm                          2.062e-02  3.439e-02   0.600 0.548806    
## days.with.cr.line               2.298e-06  1.586e-05   0.145 0.884813    
## log.annual.inc                 -4.326e-01  7.137e-02  -6.060 1.36e-09 ***
## fico                           -9.273e-03  1.703e-03  -5.446 5.16e-08 ***
## credit.policy                  -3.385e-01  1.009e-01  -3.355 0.000794 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 5896.6  on 6704  degrees of freedom
## Residual deviance: 5484.9  on 6685  degrees of freedom
## AIC: 5524.9
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-9.png) 

```
##    threshold     f.score
## 1        0.0 0.275906403
## 2        0.1 0.321659300
## 3        0.2 0.352512746
## 4        0.3 0.248447205
## 5        0.4 0.138645418
## 6        0.5 0.053003534
## 7        0.6 0.018315018
## 8        0.7 0.003703704
## 9        0.8 0.003714020
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.bayesglm.N
## 1                 N                                       4443
## 2                 Y                                        589
##   notfullypaid.fctr.predict.All.X.bayesglm.Y
## 1                                       1189
## 2                                        484
##          Prediction
## Reference    N    Y
##         N 4443 1189
##         Y  589  484
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.348248e-01   1.956717e-01   7.240836e-01   7.453613e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   8.452137e-46 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-11.png) 

```
##    threshold     f.score
## 1        0.0 0.276027603
## 2        0.1 0.321329640
## 3        0.2 0.340830450
## 4        0.3 0.241430700
## 5        0.4 0.103448276
## 6        0.5 0.016806723
## 7        0.6 0.008602151
## 8        0.7 0.004329004
## 9        0.8 0.004338395
## 10       0.9 0.004338395
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.bayesglm.N
## 1                 N                                       1914
## 2                 Y                                        263
##   notfullypaid.fctr.predict.All.X.bayesglm.Y
## 1                                        499
## 2                                        197
##          Prediction
## Reference    N    Y
##         N 1914  499
##         Y  263  197
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.347720e-01   1.833891e-01   7.182218e-01   7.508427e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.692683e-17 
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                                                                   feats
## 1 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      2.396                 0.194
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.6862813                    0.2       0.3525127         0.838777
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7240836             0.7453613    0.03258134   0.6715671
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3408304         0.734772
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7182218             0.7508427     0.1833891    5524.854
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0009313942      0.02580696
##                   label step_major step_minor    bgn    end elapsed
## 3 fit.models_1_bayesglm          3          0 81.919 91.265   9.346
## 4    fit.models_1_rpart          4          0 91.266     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00261 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](LendingClub_template2_files/figure-html/fit.models_1-13.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 6705 
## 
##            CP nsplit rel error
## 1 0.002609506      0         1
## 
## Node number 1: 6705 observations
##   predicted class=N  expected loss=0.1600298  P(node) =1
##     class counts:  5632  1073
##    probabilities: 0.840 0.160 
## 
## n= 6705 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 6705 1073 N (0.8399702 0.1600298) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.no.rnorm.rpart.N
## 1                 N                                             5632
## 2                 Y                                             1073
##          Prediction
## Reference    N    Y
##         N 5632    0
##         Y 1073    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.399702e-01   0.000000e+00   8.309724e-01   8.486712e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   5.081492e-01  6.633506e-235 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.no.rnorm.rpart.N
## 1                 N                                             2413
## 2                 Y                                              460
##          Prediction
## Reference    N    Y
##         N 2413    0
##         Y  460    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.398886e-01   0.000000e+00   8.259605e-01   8.531205e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   5.124443e-01  1.308142e-101 
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                                                           feats
## 1 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      2.154                 0.133
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1         0.5                    0.5               0        0.8326622
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8309724             0.8486712    0.05577392         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8398886
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8259605             0.8531205             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007526892      0.02004867
##                label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_rpart          4          0 91.266 95.123   3.857
## 5    fit.models_1_rf          5          0 95.123     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](LendingClub_template2_files/figure-html/fit.models_1-14.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](LendingClub_template2_files/figure-html/fit.models_1-15.png) ![](LendingClub_template2_files/figure-html/fit.models_1-16.png) 

```
##                 Length Class      Mode     
## call                4  -none-     call     
## type                1  -none-     character
## predicted        6705  factor     numeric  
## err.rate         1500  -none-     numeric  
## confusion           6  -none-     numeric  
## votes           13410  matrix     numeric  
## oob.times        6705  -none-     numeric  
## classes             2  -none-     character
## importance         18  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y                6705  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames             18  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           2  -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-17.png) 

```
##    threshold    f.score
## 1        0.0 0.27590640
## 2        0.1 0.79569892
## 3        0.2 0.98609824
## 4        0.3 0.92967581
## 5        0.4 0.76382488
## 6        0.5 0.50452962
## 7        0.6 0.20268007
## 8        0.7 0.03119266
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.no.rnorm.rf.N
## 1                 N                                          5611
## 2                 Y                                             9
##   notfullypaid.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                            21
## 2                                          1064
##          Prediction
## Reference    N    Y
##         N 5611   21
##         Y    9 1064
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.99552573     0.98343214     0.99361881     0.99697925     0.83997017 
## AccuracyPValue  McnemarPValue 
##     0.00000000     0.04460972 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-19.png) 

```
##    threshold    f.score
## 1        0.0 0.27602760
## 2        0.1 0.35046729
## 3        0.2 0.30109890
## 4        0.3 0.16206897
## 5        0.4 0.02505219
## 6        0.5 0.00000000
## 7        0.6 0.00000000
## 8        0.7 0.00000000
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-20.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.All.X.no.rnorm.rf.N
## 1                 N                                          1461
## 2                 Y                                           160
##   notfullypaid.fctr.predict.All.X.no.rnorm.rf.Y
## 1                                           952
## 2                                           300
##          Prediction
## Reference    N    Y
##         N 1461  952
##         Y  160  300
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   6.129481e-01   1.518450e-01   5.948558e-01   6.308100e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00  2.215029e-124 
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                                                           feats
## 1 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     46.277                11.265
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9998986                    0.2       0.9860982        0.8392245
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9936188             0.9969793   0.002279195   0.6711995
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3504673        0.6129481
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.5948558               0.63081      0.151845
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0005166445     0.003325232
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
require(gdata) # needed for trim
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```r
model_id <- "Csm.1";
indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats)
                        , 1)[, "feats"]
indep_vars_vctr <- trim(unlist(strsplit(indep_vars_vctr, "[,]")))
indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

for (method in c("glm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: Csm.1.glm"
## [1] "    indep_vars: int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy"
## Aggregating results
## Fitting final model on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_1-21.png) ![](LendingClub_template2_files/figure-html/fit.models_1-22.png) ![](LendingClub_template2_files/figure-html/fit.models_1-23.png) ![](LendingClub_template2_files/figure-html/fit.models_1-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2049  -0.6205  -0.4951  -0.3606   2.6397  
## 
## Coefficients:
##                                  Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                     9.187e+00  1.554e+00   5.910 3.42e-09 ***
## int.rate                        6.110e-01  2.085e+00   0.293 0.769446    
## inq.last.6mths                  8.437e-02  1.600e-02   5.275 1.33e-07 ***
## revol.util                      1.839e-03  1.535e-03   1.199 0.230722    
## pub.rec                         3.300e-01  1.139e-01   2.898 0.003756 ** 
## revol.bal                       3.085e-06  1.168e-06   2.641 0.008273 ** 
## dti                             4.638e-03  5.502e-03   0.843 0.399288    
## installment                     1.275e-03  2.092e-04   6.093 1.11e-09 ***
## purpose.fctrcredit_card        -6.141e-01  1.344e-01  -4.568 4.93e-06 ***
## purpose.fctrdebt_consolidation -3.212e-01  9.183e-02  -3.498 0.000469 ***
## purpose.fctreducational         1.347e-01  1.753e-01   0.768 0.442201    
## purpose.fctrhome_improvement    1.727e-01  1.480e-01   1.167 0.243135    
## purpose.fctrmajor_purchase     -4.830e-01  2.009e-01  -2.404 0.016203 *  
## purpose.fctrsmall_business      4.120e-01  1.419e-01   2.905 0.003678 ** 
## delinq.2yrs                    -8.320e-02  6.561e-02  -1.268 0.204762    
## days.with.cr.line               2.371e-06  1.588e-05   0.149 0.881343    
## log.annual.inc                 -4.337e-01  7.148e-02  -6.067 1.30e-09 ***
## fico                           -9.317e-03  1.710e-03  -5.448 5.08e-08 ***
## credit.policy                  -3.368e-01  1.011e-01  -3.332 0.000861 ***
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
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-25.png) 

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

![](LendingClub_template2_files/figure-html/fit.models_1-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Csm.1.glm.N
## 1                 N                                  4451
## 2                 Y                                   591
##   notfullypaid.fctr.predict.Csm.1.glm.Y
## 1                                  1181
## 2                                   482
##          Prediction
## Reference    N    Y
##         N 4451 1181
##         Y  591  482
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.357196e-01   1.959127e-01   7.249895e-01   7.462443e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.741491e-44 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-27.png) 

```
##    threshold     f.score
## 1        0.0 0.276027603
## 2        0.1 0.321075524
## 3        0.2 0.340277778
## 4        0.3 0.241791045
## 5        0.4 0.099616858
## 6        0.5 0.012605042
## 7        0.6 0.008602151
## 8        0.7 0.004329004
## 9        0.8 0.004338395
## 10       0.9 0.004338395
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Csm.1.glm.N
## 1                 N                                  1917
## 2                 Y                                   264
##   notfullypaid.fctr.predict.Csm.1.glm.Y
## 1                                   496
## 2                                   196
##          Prediction
## Reference    N    Y
##         N 1917  496
##         Y  264  196
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.354682e-01   1.831522e-01   7.189308e-01   7.515246e-01   8.398886e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   5.326571e-17 
##    model_id model_method
## 1 Csm.1.glm          glm
##                                                                                                                                                           feats
## 1 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.256                 0.103
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.6861578                    0.2       0.3523392        0.8389262
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7249895             0.7462443    0.03525053   0.6720995
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3402778        0.7354682
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7189308             0.7515246     0.1831522    5523.212
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0004474273      0.02494381
##                                importance
## installment                     100.00000
## log.annual.inc                   99.56249
## fico                             89.15702
## inq.last.6mths                   86.23313
## purpose.fctrcredit_card          74.33701
## purpose.fctrdebt_consolidation   56.33937
```

```r
    # easier to include features
model_id <- "Bsl.1"; indep_vars_vctr <- c("int.rate")
for (method in c("glm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: Bsl.1.glm"
## [1] "    indep_vars: int.rate"
## Aggregating results
## Fitting final model on full training set
```

![](LendingClub_template2_files/figure-html/fit.models_1-29.png) ![](LendingClub_template2_files/figure-html/fit.models_1-30.png) ![](LendingClub_template2_files/figure-html/fit.models_1-31.png) ![](LendingClub_template2_files/figure-html/fit.models_1-32.png) 

```
## 
## Call:
## NULL
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
## [1] "    calling mypredict_mdl for fit:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-33.png) 

```
##    threshold     f.score
## 1        0.0 0.275906403
## 2        0.1 0.300488961
## 3        0.2 0.266825966
## 4        0.3 0.080971660
## 5        0.4 0.009199632
## 6        0.5 0.000000000
## 7        0.6 0.000000000
## 8        0.7 0.000000000
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-34.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Bsl.1.glm.N
## 1                 N                                   970
## 2                 Y                                    59
##   notfullypaid.fctr.predict.Bsl.1.glm.Y
## 1                                  4662
## 2                                  1014
##          Prediction
## Reference    N    Y
##         N  970 4662
##         Y   59 1014
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.29589858     0.04284809     0.28498927     0.30698565     0.83997017 
## AccuracyPValue  McnemarPValue 
##     1.00000000     0.00000000 
## [1] "    calling mypredict_mdl for OOB:"
```

![](LendingClub_template2_files/figure-html/fit.models_1-35.png) 

```
##    threshold     f.score
## 1        0.0 0.276027603
## 2        0.1 0.302123216
## 3        0.2 0.273408240
## 4        0.3 0.083175803
## 5        0.4 0.004319654
## 6        0.5 0.000000000
## 7        0.6 0.000000000
## 8        0.7 0.000000000
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](LendingClub_template2_files/figure-html/fit.models_1-36.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   notfullypaid.fctr notfullypaid.fctr.predict.Bsl.1.glm.N
## 1                 N                                   434
## 2                 Y                                    26
##   notfullypaid.fctr.predict.Bsl.1.glm.Y
## 1                                  1979
## 2                                   434
##          Prediction
## Reference    N    Y
##         N  434 1979
##         Y   26  434
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##     0.30212322     0.04537559     0.28536727     0.31928310     0.83988862 
## AccuracyPValue  McnemarPValue 
##     1.00000000     0.00000000 
##    model_id model_method    feats max.nTuningRuns
## 1 Bsl.1.glm          glm int.rate               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.529                 0.033   0.6186226
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1        0.300489        0.8399702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.2849893             0.3069857             0   0.6239081
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.3021232        0.3021232
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.2853673             0.3192831    0.04537559    5738.833
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0002583223               0
##   importance
## 1        NaN
```

```r
# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
## Csm.1.glm                                 Csm.1.glm              glm
## Bsl.1.glm                                 Bsl.1.glm              glm
##                                                                                                                                                                                           feats
## MFO.myMFO_classfr                                                                                                                                                                        .rnorm
## Random.myrandom_classfr                                                                                                                                                                  .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                    credit.policy, int.rate
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                               credit.policy, int.rate
## Max.cor.Y.rpart                                                                                                                                                         credit.policy, int.rate
## Max.cor.Y.glm                                                                                                                                                           credit.policy, int.rate
## Interact.High.cor.Y.glm                                                                                                                         credit.policy, int.rate, credit.policy:int.rate
## Low.cor.X.glm                   int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, credit.policy
## All.X.glm                 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy
## All.X.bayesglm            int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy
## All.X.no.rnorm.rpart              int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
## All.X.no.rnorm.rf                 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
## Csm.1.glm                         int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
## Bsl.1.glm                                                                                                                                                                              int.rate
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.myMFO_classfr                       0                      0.351
## Random.myrandom_classfr                 0                      0.234
## Max.cor.Y.cv.0.rpart                    0                      0.799
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.476
## Max.cor.Y.rpart                         3                      1.325
## Max.cor.Y.glm                           1                      1.109
## Interact.High.cor.Y.glm                 1                      1.074
## Low.cor.X.glm                           1                      1.150
## All.X.glm                               1                      1.224
## All.X.bayesglm                          1                      2.396
## All.X.no.rnorm.rpart                    3                      2.154
## All.X.no.rnorm.rf                       3                     46.277
## Csm.1.glm                               1                      1.256
## Bsl.1.glm                               1                      1.529
##                           min.elapsedtime.final max.auc.fit
## MFO.myMFO_classfr                         0.003   0.5000000
## Random.myrandom_classfr                   0.002   0.5064397
## Max.cor.Y.cv.0.rpart                      0.030   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart                 0.017   0.6489150
## Max.cor.Y.rpart                           0.023   0.5000000
## Max.cor.Y.glm                             0.043   0.6504890
## Interact.High.cor.Y.glm                   0.038   0.6500808
## Low.cor.X.glm                             0.099   0.6822388
## All.X.glm                                 0.115   0.6862765
## All.X.bayesglm                            0.194   0.6862813
## All.X.no.rnorm.rpart                      0.133   0.5000000
## All.X.no.rnorm.rf                        11.265   0.9998986
## Csm.1.glm                                 0.103   0.6861578
## Bsl.1.glm                                 0.033   0.6186226
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2759064
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.2       0.3345484
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3225020
## Interact.High.cor.Y.glm                      0.2       0.3257821
## Low.cor.X.glm                                0.2       0.3588980
## All.X.glm                                    0.2       0.3523844
## All.X.bayesglm                               0.2       0.3525127
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.2       0.9860982
## Csm.1.glm                                    0.2       0.3523392
## Bsl.1.glm                                    0.1       0.3004890
##                           max.Accuracy.fit max.AccuracyLower.fit
## MFO.myMFO_classfr                0.8399702             0.8309724
## Random.myrandom_classfr          0.1600298             0.1513288
## Max.cor.Y.cv.0.rpart             0.8399702             0.8309724
## Max.cor.Y.cv.0.cp.0.rpart        0.7549590             0.7444798
## Max.cor.Y.rpart                  0.8340045             0.8309724
## Max.cor.Y.glm                    0.8399702             0.7407005
## Interact.High.cor.Y.glm          0.8399702             0.7094462
## Low.cor.X.glm                    0.8386279             0.7325407
## All.X.glm                        0.8387770             0.7239327
## All.X.bayesglm                   0.8387770             0.7240836
## All.X.no.rnorm.rpart             0.8326622             0.8309724
## All.X.no.rnorm.rf                0.8392245             0.9936188
## Csm.1.glm                        0.8389262             0.7249895
## Bsl.1.glm                        0.8399702             0.2849893
##                           max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                     0.8486712   0.000000000   0.5000000
## Random.myrandom_classfr               0.1690276   0.000000000   0.5097686
## Max.cor.Y.cv.0.rpart                  0.8486712   0.000000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart             0.7652160   0.187517226   0.6197157
## Max.cor.Y.rpart                       0.8486712   0.001863450   0.5000000
## Max.cor.Y.glm                         0.7615414   0.002512178   0.6526500
## Interact.High.cor.Y.glm               0.7310778   0.000000000   0.6530158
## Low.cor.X.glm                         0.7536009   0.026489942   0.6688742
## All.X.glm                             0.7452141   0.033808089   0.6714373
## All.X.bayesglm                        0.7453613   0.032581340   0.6715671
## All.X.no.rnorm.rpart                  0.8486712   0.055773920   0.5000000
## All.X.no.rnorm.rf                     0.9969793   0.002279195   0.6711995
## Csm.1.glm                             0.7462443   0.035250530   0.6720995
## Bsl.1.glm                             0.3069857   0.000000000   0.6239081
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2760276
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.1       0.3035019
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.1       0.3117926
## Interact.High.cor.Y.glm                      0.2       0.3286540
## Low.cor.X.glm                                0.2       0.3247101
## All.X.glm                                    0.2       0.3405359
## All.X.bayesglm                               0.2       0.3408304
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.1       0.3504673
## Csm.1.glm                                    0.2       0.3402778
## Bsl.1.glm                                    0.1       0.3021232
##                           max.Accuracy.OOB max.AccuracyLower.OOB
## MFO.myMFO_classfr                0.8398886             0.8259605
## Random.myrandom_classfr          0.1601114             0.1468795
## Max.cor.Y.cv.0.rpart             0.8398886             0.8259605
## Max.cor.Y.cv.0.cp.0.rpart        0.3146537             0.2976950
## Max.cor.Y.rpart                  0.8398886             0.8259605
## Max.cor.Y.glm                    0.3438914             0.3265145
## Interact.High.cor.Y.glm          0.7170205             0.7001600
## Low.cor.X.glm                    0.7365124             0.7199944
## All.X.glm                        0.7344239             0.7178674
## All.X.bayesglm                   0.7347720             0.7182218
## All.X.no.rnorm.rpart             0.8398886             0.8259605
## All.X.no.rnorm.rf                0.6129481             0.5948558
## Csm.1.glm                        0.7354682             0.7189308
## Bsl.1.glm                        0.3021232             0.2853673
##                           max.AccuracyUpper.OOB max.Kappa.OOB
## MFO.myMFO_classfr                     0.8531205    0.00000000
## Random.myrandom_classfr               0.1740395    0.00000000
## Max.cor.Y.cv.0.rpart                  0.8531205    0.00000000
## Max.cor.Y.cv.0.cp.0.rpart             0.3319907    0.04834758
## Max.cor.Y.rpart                       0.8531205    0.00000000
## Max.cor.Y.glm                         0.3615868    0.06182140
## Interact.High.cor.Y.glm               0.7334379    0.16229845
## Low.cor.X.glm                         0.7525473    0.16752146
## All.X.glm                             0.7505017    0.18291269
## All.X.bayesglm                        0.7508427    0.18338911
## All.X.no.rnorm.rpart                  0.8531205    0.00000000
## All.X.no.rnorm.rf                     0.6308100    0.15184499
## Csm.1.glm                             0.7515246    0.18315217
## Bsl.1.glm                             0.3192831    0.04537559
##                           max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## MFO.myMFO_classfr                         NA              NA          NA
## Random.myrandom_classfr                   NA              NA          NA
## Max.cor.Y.cv.0.rpart                      NA              NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA              NA          NA
## Max.cor.Y.rpart                 0.0057123693     0.002541372          NA
## Max.cor.Y.glm                   0.0002583223     0.004351221    5657.364
## Interact.High.cor.Y.glm         0.0002583223     0.000000000    5646.416
## Low.cor.X.glm                   0.0006834565     0.017950443    5553.354
## All.X.glm                       0.0005166445     0.022989856    5524.850
## All.X.bayesglm                  0.0009313942     0.025806962    5524.854
## All.X.no.rnorm.rpart            0.0075268921     0.020048670          NA
## All.X.no.rnorm.rf               0.0005166445     0.003325232          NA
## Csm.1.glm                       0.0004474273     0.024943812    5523.212
## Bsl.1.glm                       0.0002583223     0.000000000    5738.833
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor     bgn     end elapsed
## 5  fit.models_1_rf          5          0  95.123 162.128  67.005
## 6 fit.models_1_end          6          0 162.128      NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 11 fit.models          7          1  63.786 162.135  98.349
## 12 fit.models          7          2 162.136      NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                                            model_id     model_method
## MFO.myMFO_classfr                 MFO.myMFO_classfr    myMFO_classfr
## Random.myrandom_classfr     Random.myrandom_classfr myrandom_classfr
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart            rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart            rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart            rpart
## Max.cor.Y.glm                         Max.cor.Y.glm              glm
## Interact.High.cor.Y.glm     Interact.High.cor.Y.glm              glm
## Low.cor.X.glm                         Low.cor.X.glm              glm
## All.X.glm                                 All.X.glm              glm
## All.X.bayesglm                       All.X.bayesglm         bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart            rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf               rf
## Csm.1.glm                                 Csm.1.glm              glm
## Bsl.1.glm                                 Bsl.1.glm              glm
##                                                                                                                                                                                           feats
## MFO.myMFO_classfr                                                                                                                                                                        .rnorm
## Random.myrandom_classfr                                                                                                                                                                  .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                                                    credit.policy, int.rate
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                                               credit.policy, int.rate
## Max.cor.Y.rpart                                                                                                                                                         credit.policy, int.rate
## Max.cor.Y.glm                                                                                                                                                           credit.policy, int.rate
## Interact.High.cor.Y.glm                                                                                                                         credit.policy, int.rate, credit.policy:int.rate
## Low.cor.X.glm                   int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, credit.policy
## All.X.glm                 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy
## All.X.bayesglm            int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, .rnorm, days.with.cr.line, log.annual.inc, fico, credit.policy
## All.X.no.rnorm.rpart              int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
## All.X.no.rnorm.rf                 int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
## Csm.1.glm                         int.rate, inq.last.6mths, revol.util, pub.rec, revol.bal, dti, installment, purpose.fctr, delinq.2yrs, days.with.cr.line, log.annual.inc, fico, credit.policy
## Bsl.1.glm                                                                                                                                                                              int.rate
##                           max.nTuningRuns max.auc.fit
## MFO.myMFO_classfr                       0   0.5000000
## Random.myrandom_classfr                 0   0.5064397
## Max.cor.Y.cv.0.rpart                    0   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.6489150
## Max.cor.Y.rpart                         3   0.5000000
## Max.cor.Y.glm                           1   0.6504890
## Interact.High.cor.Y.glm                 1   0.6500808
## Low.cor.X.glm                           1   0.6822388
## All.X.glm                               1   0.6862765
## All.X.bayesglm                          1   0.6862813
## All.X.no.rnorm.rpart                    3   0.5000000
## All.X.no.rnorm.rf                       3   0.9998986
## Csm.1.glm                               1   0.6861578
## Bsl.1.glm                               1   0.6186226
##                           opt.prob.threshold.fit max.f.score.fit
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2759064
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.2       0.3345484
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.2       0.3225020
## Interact.High.cor.Y.glm                      0.2       0.3257821
## Low.cor.X.glm                                0.2       0.3588980
## All.X.glm                                    0.2       0.3523844
## All.X.bayesglm                               0.2       0.3525127
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.2       0.9860982
## Csm.1.glm                                    0.2       0.3523392
## Bsl.1.glm                                    0.1       0.3004890
##                           max.Accuracy.fit max.Kappa.fit max.auc.OOB
## MFO.myMFO_classfr                0.8399702   0.000000000   0.5000000
## Random.myrandom_classfr          0.1600298   0.000000000   0.5097686
## Max.cor.Y.cv.0.rpart             0.8399702   0.000000000   0.5000000
## Max.cor.Y.cv.0.cp.0.rpart        0.7549590   0.187517226   0.6197157
## Max.cor.Y.rpart                  0.8340045   0.001863450   0.5000000
## Max.cor.Y.glm                    0.8399702   0.002512178   0.6526500
## Interact.High.cor.Y.glm          0.8399702   0.000000000   0.6530158
## Low.cor.X.glm                    0.8386279   0.026489942   0.6688742
## All.X.glm                        0.8387770   0.033808089   0.6714373
## All.X.bayesglm                   0.8387770   0.032581340   0.6715671
## All.X.no.rnorm.rpart             0.8326622   0.055773920   0.5000000
## All.X.no.rnorm.rf                0.8392245   0.002279195   0.6711995
## Csm.1.glm                        0.8389262   0.035250530   0.6720995
## Bsl.1.glm                        0.8399702   0.000000000   0.6239081
##                           opt.prob.threshold.OOB max.f.score.OOB
## MFO.myMFO_classfr                            0.5       0.0000000
## Random.myrandom_classfr                      0.1       0.2760276
## Max.cor.Y.cv.0.rpart                         0.5       0.0000000
## Max.cor.Y.cv.0.cp.0.rpart                    0.1       0.3035019
## Max.cor.Y.rpart                              0.5       0.0000000
## Max.cor.Y.glm                                0.1       0.3117926
## Interact.High.cor.Y.glm                      0.2       0.3286540
## Low.cor.X.glm                                0.2       0.3247101
## All.X.glm                                    0.2       0.3405359
## All.X.bayesglm                               0.2       0.3408304
## All.X.no.rnorm.rpart                         0.5       0.0000000
## All.X.no.rnorm.rf                            0.1       0.3504673
## Csm.1.glm                                    0.2       0.3402778
## Bsl.1.glm                                    0.1       0.3021232
##                           max.Accuracy.OOB max.Kappa.OOB
## MFO.myMFO_classfr                0.8398886    0.00000000
## Random.myrandom_classfr          0.1601114    0.00000000
## Max.cor.Y.cv.0.rpart             0.8398886    0.00000000
## Max.cor.Y.cv.0.cp.0.rpart        0.3146537    0.04834758
## Max.cor.Y.rpart                  0.8398886    0.00000000
## Max.cor.Y.glm                    0.3438914    0.06182140
## Interact.High.cor.Y.glm          0.7170205    0.16229845
## Low.cor.X.glm                    0.7365124    0.16752146
## All.X.glm                        0.7344239    0.18291269
## All.X.bayesglm                   0.7347720    0.18338911
## All.X.no.rnorm.rpart             0.8398886    0.00000000
## All.X.no.rnorm.rf                0.6129481    0.15184499
## Csm.1.glm                        0.7354682    0.18315217
## Bsl.1.glm                        0.3021232    0.04537559
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.myMFO_classfr                         2.84900285          333.33333333
## Random.myrandom_classfr                   4.27350427          500.00000000
## Max.cor.Y.cv.0.rpart                      1.25156446           33.33333333
## Max.cor.Y.cv.0.cp.0.rpart                 2.10084034           58.82352941
## Max.cor.Y.rpart                           0.75471698           43.47826087
## Max.cor.Y.glm                             0.90171326           23.25581395
## Interact.High.cor.Y.glm                   0.93109870           26.31578947
## Low.cor.X.glm                             0.86956522           10.10101010
## All.X.glm                                 0.81699346            8.69565217
## All.X.bayesglm                            0.41736227            5.15463918
## All.X.no.rnorm.rpart                      0.46425255            7.51879699
## All.X.no.rnorm.rf                         0.02160901            0.08877053
## Csm.1.glm                                 0.79617834            9.70873786
## Bsl.1.glm                                 0.65402224           30.30303030
##                            inv.aic.fit
## MFO.myMFO_classfr                   NA
## Random.myrandom_classfr             NA
## Max.cor.Y.cv.0.rpart                NA
## Max.cor.Y.cv.0.cp.0.rpart           NA
## Max.cor.Y.rpart                     NA
## Max.cor.Y.glm             0.0001767608
## Interact.High.cor.Y.glm   0.0001771035
## Low.cor.X.glm             0.0001800714
## All.X.glm                 0.0001810004
## All.X.bayesglm            0.0001810003
## All.X.no.rnorm.rpart                NA
## All.X.no.rnorm.rf                   NA
## Csm.1.glm                 0.0001810541
## Bsl.1.glm                 0.0001742515
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 3 rows containing missing values (geom_path).
```

```
## Warning: Removed 117 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually if you must have them.
```

![](LendingClub_template2_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](LendingClub_template2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 1          MFO.myMFO_classfr        0.8398886   0.5000000    0.00000000
## 3       Max.cor.Y.cv.0.rpart        0.8398886   0.5000000    0.00000000
## 5            Max.cor.Y.rpart        0.8398886   0.5000000    0.00000000
## 11      All.X.no.rnorm.rpart        0.8398886   0.5000000    0.00000000
## 8              Low.cor.X.glm        0.7365124   0.6688742    0.16752146
## 13                 Csm.1.glm        0.7354682   0.6720995    0.18315217
## 10            All.X.bayesglm        0.7347720   0.6715671    0.18338911
## 9                  All.X.glm        0.7344239   0.6714373    0.18291269
## 7    Interact.High.cor.Y.glm        0.7170205   0.6530158    0.16229845
## 12         All.X.no.rnorm.rf        0.6129481   0.6711995    0.15184499
## 6              Max.cor.Y.glm        0.3438914   0.6526500    0.06182140
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.3146537   0.6197157    0.04834758
## 14                 Bsl.1.glm        0.3021232   0.6239081    0.04537559
## 2    Random.myrandom_classfr        0.1601114   0.5097686    0.00000000
##    min.aic.fit opt.prob.threshold.OOB
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 8     5553.354                    0.2
## 13    5523.212                    0.2
## 10    5524.854                    0.2
## 9     5524.850                    0.2
## 7     5646.416                    0.2
## 12          NA                    0.1
## 6     5657.364                    0.1
## 4           NA                    0.1
## 14    5738.833                    0.1
## 2           NA                    0.1
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually if you must have them.
```

```
## Warning: Removed 50 rows containing missing values (geom_point).
```

```
## Warning: Removed 7 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually if you must have them.
```

![](LendingClub_template2_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: MFO.myMFO_classfr"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
#     if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
#         warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
#         glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
#     }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

```
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8399702 0.1600298
## 2 0.8399702 0.1600298
## 3 0.8399702 0.1600298
## 4 0.8399702 0.1600298
## 5 0.8399702 0.1600298
## 6 0.8399702 0.1600298
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); #sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
```

```
## [1] "in MFO.Classifier$varImp"
##        Overall
## .rnorm       0
```

```r
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##        importance MFO.myMFO_classfr.importance
## .rnorm        NaN                          NaN
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](LendingClub_template2_files/figure-html/fit.models_2-4.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames notfullypaid.fctr
## 9575      9575                 Y
## 999        999                 Y
## 2            2                 N
## 7027      7027                 N
## 8412      8412                 N
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.prob
## 9575                                        0.1600298
## 999                                         0.1600298
## 2                                           0.1600298
## 7027                                        0.1600298
## 8412                                        0.1600298
##      notfullypaid.fctr.predict.MFO.myMFO_classfr
## 9575                                           N
## 999                                            N
## 2                                              N
## 7027                                           N
## 8412                                           N
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.accurate
## 9575                                                FALSE
## 999                                                 FALSE
## 2                                                    TRUE
## 7027                                                 TRUE
## 8412                                                 TRUE
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.error .label
## 9575                                        -0.3399702   9575
## 999                                         -0.3399702    999
## 2                                            0.0000000      2
## 7027                                         0.0000000   7027
## 8412                                         0.0000000   8412
## [1] "Inaccurate: "
##      .rownames notfullypaid.fctr
## 1050      1050                 Y
## 1075      1075                 Y
## 1107      1107                 Y
## 1133      1133                 Y
## 1134      1134                 Y
## 1168      1168                 Y
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.prob
## 1050                                        0.1600298
## 1075                                        0.1600298
## 1107                                        0.1600298
## 1133                                        0.1600298
## 1134                                        0.1600298
## 1168                                        0.1600298
##      notfullypaid.fctr.predict.MFO.myMFO_classfr
## 1050                                           N
## 1075                                           N
## 1107                                           N
## 1133                                           N
## 1134                                           N
## 1168                                           N
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.accurate
## 1050                                                FALSE
## 1075                                                FALSE
## 1107                                                FALSE
## 1133                                                FALSE
## 1134                                                FALSE
## 1168                                                FALSE
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.error
## 1050                                        -0.3399702
## 1075                                        -0.3399702
## 1107                                        -0.3399702
## 1133                                        -0.3399702
## 1134                                        -0.3399702
## 1168                                        -0.3399702
##      .rownames notfullypaid.fctr
## 1524      1524                 Y
## 6024      6024                 Y
## 6032      6032                 Y
## 8489      8489                 Y
## 8502      8502                 Y
## 9549      9549                 Y
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.prob
## 1524                                        0.1600298
## 6024                                        0.1600298
## 6032                                        0.1600298
## 8489                                        0.1600298
## 8502                                        0.1600298
## 9549                                        0.1600298
##      notfullypaid.fctr.predict.MFO.myMFO_classfr
## 1524                                           N
## 6024                                           N
## 6032                                           N
## 8489                                           N
## 8502                                           N
## 9549                                           N
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.accurate
## 1524                                                FALSE
## 6024                                                FALSE
## 6032                                                FALSE
## 8489                                                FALSE
## 8502                                                FALSE
## 9549                                                FALSE
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.error
## 1524                                        -0.3399702
## 6024                                        -0.3399702
## 6032                                        -0.3399702
## 8489                                        -0.3399702
## 8502                                        -0.3399702
## 9549                                        -0.3399702
##      .rownames notfullypaid.fctr
## 9571      9571                 Y
## 9575      9575                 Y
## 963        963                 Y
## 974        974                 Y
## 984        984                 Y
## 999        999                 Y
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.prob
## 9571                                        0.1600298
## 9575                                        0.1600298
## 963                                         0.1600298
## 974                                         0.1600298
## 984                                         0.1600298
## 999                                         0.1600298
##      notfullypaid.fctr.predict.MFO.myMFO_classfr
## 9571                                           N
## 9575                                           N
## 963                                            N
## 974                                            N
## 984                                            N
## 999                                            N
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.accurate
## 9571                                                FALSE
## 9575                                                FALSE
## 963                                                 FALSE
## 974                                                 FALSE
## 984                                                 FALSE
## 999                                                 FALSE
##      notfullypaid.fctr.predict.MFO.myMFO_classfr.error
## 9571                                        -0.3399702
## 9575                                        -0.3399702
## 963                                         -0.3399702
## 974                                         -0.3399702
## 984                                         -0.3399702
## 999                                         -0.3399702
```

![](LendingClub_template2_files/figure-html/fit.models_2-5.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor     bgn     end elapsed
## 12 fit.models          7          2 162.136 191.212  29.076
## 13 fit.models          7          3 191.213      NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "notfullypaid.fctr.predict.MFO.myMFO_classfr.prob"    
## [2] "notfullypaid.fctr.predict.MFO.myMFO_classfr"         
## [3] "notfullypaid.fctr.predict.MFO.myMFO_classfr.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](LendingClub_template2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 13        fit.models          7          3 191.213 204.744  13.531
## 14 fit.data.training          8          0 204.744      NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## [1] "fitting model: Final.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Aggregating results
## Fitting final model on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8399702 0.1600298 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8399702 0.1600298
## 2 0.8399702 0.1600298
## 3 0.8399702 0.1600298
## 4 0.8399702 0.1600298
## 5 0.8399702 0.1600298
## 6 0.8399702 0.1600298
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   notfullypaid.fctr notfullypaid.fctr.predict.Final.myMFO_classfr.N
## 1                 N                                            5632
## 2                 Y                                            1073
##          Prediction
## Reference    N    Y
##         N 5632    0
##         Y 1073    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.399702e-01   0.000000e+00   8.309724e-01   8.486712e-01   8.399702e-01 
## AccuracyPValue  McnemarPValue 
##   5.081492e-01  6.633506e-235
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

```
##              model_id  model_method  feats max.nTuningRuns
## 1 Final.myMFO_classfr myMFO_classfr .rnorm               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.045                 0.004         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8399702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.8309724             0.8486712             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1       0.0002583223               0
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 14 fit.data.training          8          0 204.744 206.944     2.2
## 15 fit.data.training          8          1 206.945      NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnobs_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.5
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8399702 0.1600298
## 2 0.8399702 0.1600298
## 3 0.8399702 0.1600298
## 4 0.8399702 0.1600298
## 5 0.8399702 0.1600298
## 6 0.8399702 0.1600298
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
```

```
## [1] "in MFO.Classifier$varImp"
##        Overall
## .rnorm       0
```

```r
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##        MFO.myMFO_classfr.importance importance
## .rnorm                          NaN        NaN
##        Final.myMFO_classfr.importance
## .rnorm                            NaN
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](LendingClub_template2_files/figure-html/fit.data.training_1-1.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames notfullypaid.fctr
## 3612      3612                 Y
## 9578      9578                 Y
## 1            1                 N
## 1204      1204                 N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 3612                                          0.1600298
## 9578                                          0.1600298
## 1                                             0.1600298
## 1204                                          0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 3612                                             N
## 9578                                             N
## 1                                                N
## 1204                                             N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 3612                                                  FALSE
## 9578                                                  FALSE
## 1                                                      TRUE
## 1204                                                   TRUE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error .label
## 3612                                          -0.3399702   3612
## 9578                                          -0.3399702   9578
## 1                                              0.0000000      1
## 1204                                           0.0000000   1204
## [1] "Inaccurate: "
##      .rownames notfullypaid.fctr
## 1005      1005                 Y
## 1011      1011                 Y
## 1014      1014                 Y
## 1018      1018                 Y
## 1024      1024                 Y
## 103        103                 Y
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 1005                                          0.1600298
## 1011                                          0.1600298
## 1014                                          0.1600298
## 1018                                          0.1600298
## 1024                                          0.1600298
## 103                                           0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 1005                                             N
## 1011                                             N
## 1014                                             N
## 1018                                             N
## 1024                                             N
## 103                                              N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 1005                                                  FALSE
## 1011                                                  FALSE
## 1014                                                  FALSE
## 1018                                                  FALSE
## 1024                                                  FALSE
## 103                                                   FALSE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error
## 1005                                          -0.3399702
## 1011                                          -0.3399702
## 1014                                          -0.3399702
## 1018                                          -0.3399702
## 1024                                          -0.3399702
## 103                                           -0.3399702
##      .rownames notfullypaid.fctr
## 1175      1175                 Y
## 4875      4875                 Y
## 545        545                 Y
## 623        623                 Y
## 6649      6649                 Y
## 8377      8377                 Y
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 1175                                          0.1600298
## 4875                                          0.1600298
## 545                                           0.1600298
## 623                                           0.1600298
## 6649                                          0.1600298
## 8377                                          0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 1175                                             N
## 4875                                             N
## 545                                              N
## 623                                              N
## 6649                                             N
## 8377                                             N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 1175                                                  FALSE
## 4875                                                  FALSE
## 545                                                   FALSE
## 623                                                   FALSE
## 6649                                                  FALSE
## 8377                                                  FALSE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error
## 1175                                          -0.3399702
## 4875                                          -0.3399702
## 545                                           -0.3399702
## 623                                           -0.3399702
## 6649                                          -0.3399702
## 8377                                          -0.3399702
##      .rownames notfullypaid.fctr
## 9577      9577                 Y
## 9578      9578                 Y
## 972        972                 Y
## 980        980                 Y
## 986        986                 Y
## 994        994                 Y
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 9577                                          0.1600298
## 9578                                          0.1600298
## 972                                           0.1600298
## 980                                           0.1600298
## 986                                           0.1600298
## 994                                           0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 9577                                             N
## 9578                                             N
## 972                                              N
## 980                                              N
## 986                                              N
## 994                                              N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 9577                                                  FALSE
## 9578                                                  FALSE
## 972                                                   FALSE
## 980                                                   FALSE
## 986                                                   FALSE
## 994                                                   FALSE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error
## 9577                                          -0.3399702
## 9578                                          -0.3399702
## 972                                           -0.3399702
## 980                                           -0.3399702
## 986                                           -0.3399702
## 994                                           -0.3399702
```

![](LendingClub_template2_files/figure-html/fit.data.training_1-2.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "notfullypaid.fctr.predict.Final.myMFO_classfr.prob"
## [2] "notfullypaid.fctr.predict.Final.myMFO_classfr"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](LendingClub_template2_files/figure-html/fit.data.training_1-3.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor     bgn     end elapsed
## 15 fit.data.training          8          1 206.945 240.063  33.118
## 16  predict.data.new          9          0 240.064      NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newobs_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.5
```

```
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8399702 0.1600298
## 2 0.8399702 0.1600298
## 3 0.8399702 0.1600298
## 4 0.8399702 0.1600298
## 5 0.8399702 0.1600298
## 6 0.8399702 0.1600298
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](LendingClub_template2_files/figure-html/predict.data.new-1.png) 

```
## [1] "Min/Max Boundaries: "
##      .rownames notfullypaid.fctr
## 9575      9575                 Y
## 999        999                 Y
## 2            2                 N
## 7027      7027                 N
## 8412      8412                 N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 9575                                          0.1600298
## 999                                           0.1600298
## 2                                             0.1600298
## 7027                                          0.1600298
## 8412                                          0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 9575                                             N
## 999                                              N
## 2                                                N
## 7027                                             N
## 8412                                             N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 9575                                                  FALSE
## 999                                                   FALSE
## 2                                                      TRUE
## 7027                                                   TRUE
## 8412                                                   TRUE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error .label
## 9575                                          -0.3399702   9575
## 999                                           -0.3399702    999
## 2                                              0.0000000      2
## 7027                                           0.0000000   7027
## 8412                                           0.0000000   8412
## [1] "Inaccurate: "
##      .rownames notfullypaid.fctr
## 1050      1050                 Y
## 1075      1075                 Y
## 1107      1107                 Y
## 1133      1133                 Y
## 1134      1134                 Y
## 1168      1168                 Y
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 1050                                          0.1600298
## 1075                                          0.1600298
## 1107                                          0.1600298
## 1133                                          0.1600298
## 1134                                          0.1600298
## 1168                                          0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 1050                                             N
## 1075                                             N
## 1107                                             N
## 1133                                             N
## 1134                                             N
## 1168                                             N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 1050                                                  FALSE
## 1075                                                  FALSE
## 1107                                                  FALSE
## 1133                                                  FALSE
## 1134                                                  FALSE
## 1168                                                  FALSE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error
## 1050                                          -0.3399702
## 1075                                          -0.3399702
## 1107                                          -0.3399702
## 1133                                          -0.3399702
## 1134                                          -0.3399702
## 1168                                          -0.3399702
##      .rownames notfullypaid.fctr
## 649        649                 Y
## 7314      7314                 Y
## 7929      7929                 Y
## 8130      8130                 Y
## 8239      8239                 Y
## 8784      8784                 Y
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 649                                           0.1600298
## 7314                                          0.1600298
## 7929                                          0.1600298
## 8130                                          0.1600298
## 8239                                          0.1600298
## 8784                                          0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 649                                              N
## 7314                                             N
## 7929                                             N
## 8130                                             N
## 8239                                             N
## 8784                                             N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 649                                                   FALSE
## 7314                                                  FALSE
## 7929                                                  FALSE
## 8130                                                  FALSE
## 8239                                                  FALSE
## 8784                                                  FALSE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error
## 649                                           -0.3399702
## 7314                                          -0.3399702
## 7929                                          -0.3399702
## 8130                                          -0.3399702
## 8239                                          -0.3399702
## 8784                                          -0.3399702
##      .rownames notfullypaid.fctr
## 9571      9571                 Y
## 9575      9575                 Y
## 963        963                 Y
## 974        974                 Y
## 984        984                 Y
## 999        999                 Y
##      notfullypaid.fctr.predict.Final.myMFO_classfr.prob
## 9571                                          0.1600298
## 9575                                          0.1600298
## 963                                           0.1600298
## 974                                           0.1600298
## 984                                           0.1600298
## 999                                           0.1600298
##      notfullypaid.fctr.predict.Final.myMFO_classfr
## 9571                                             N
## 9575                                             N
## 963                                              N
## 974                                              N
## 984                                              N
## 999                                              N
##      notfullypaid.fctr.predict.Final.myMFO_classfr.accurate
## 9571                                                  FALSE
## 9575                                                  FALSE
## 963                                                   FALSE
## 974                                                   FALSE
## 984                                                   FALSE
## 999                                                   FALSE
##      notfullypaid.fctr.predict.Final.myMFO_classfr.error
## 9571                                          -0.3399702
## 9575                                          -0.3399702
## 963                                           -0.3399702
## 974                                           -0.3399702
## 984                                           -0.3399702
## 999                                           -0.3399702
```

![](LendingClub_template2_files/figure-html/predict.data.new-2.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
```

```
## [1] 0.5
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: MFO.myMFO_classfr"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.myMFO_classfr"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 6705   19
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 1          MFO.myMFO_classfr        0.8398886   0.5000000    0.00000000
## 3       Max.cor.Y.cv.0.rpart        0.8398886   0.5000000    0.00000000
## 5            Max.cor.Y.rpart        0.8398886   0.5000000    0.00000000
## 11      All.X.no.rnorm.rpart        0.8398886   0.5000000    0.00000000
## 8              Low.cor.X.glm        0.7365124   0.6688742    0.16752146
## 13                 Csm.1.glm        0.7354682   0.6720995    0.18315217
## 10            All.X.bayesglm        0.7347720   0.6715671    0.18338911
## 9                  All.X.glm        0.7344239   0.6714373    0.18291269
## 7    Interact.High.cor.Y.glm        0.7170205   0.6530158    0.16229845
## 12         All.X.no.rnorm.rf        0.6129481   0.6711995    0.15184499
## 6              Max.cor.Y.glm        0.3438914   0.6526500    0.06182140
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.3146537   0.6197157    0.04834758
## 14                 Bsl.1.glm        0.3021232   0.6239081    0.04537559
## 2    Random.myrandom_classfr        0.1601114   0.5097686    0.00000000
##    min.aic.fit opt.prob.threshold.OOB
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 11          NA                    0.5
## 8     5553.354                    0.2
## 13    5523.212                    0.2
## 10    5524.854                    0.2
## 9     5524.850                    0.2
## 7     5646.416                    0.2
## 12          NA                    0.1
## 6     5657.364                    0.1
## 4           NA                    0.1
## 14    5738.833                    0.1
## 2           NA                    0.1
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    
```

```
## [1] "MFO.myMFO_classfr OOB confusion matrix & accuracy: "
##          Prediction
## Reference    N    Y
##         N 2413    0
##         Y  460    0
## [1] "Final.myMFO_classfr new confusion matrix & accuracy: "
##          Prediction
## Reference    N    Y
##         N 2413    0
##         Y  460    0
```

```r
dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##        MFO.myMFO_classfr.importance importance
## .rnorm                          NaN        NaN
##        Final.myMFO_classfr.importance
## .rnorm                            NaN
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor     bgn     end elapsed
## 16     predict.data.new          9          0 240.064 280.312  40.248
## 17 display.session.info         10          0 280.313      NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor     bgn     end elapsed
## 11              fit.models          7          1  63.786 162.135  98.349
## 16        predict.data.new          9          0 240.064 280.312  40.248
## 10              fit.models          7          0  29.424  63.786  34.362
## 15       fit.data.training          8          1 206.945 240.063  33.118
## 12              fit.models          7          2 162.136 191.212  29.076
## 13              fit.models          7          3 191.213 204.744  13.531
## 2             inspect.data          2          0  14.950  24.222   9.272
## 14       fit.data.training          8          0 204.744 206.944   2.200
## 3               scrub.data          2          1  24.223  26.155   1.932
## 5         extract.features          3          0  26.218  27.547   1.329
## 8          select.features          5          0  27.995  29.112   1.117
## 1              import.data          1          0  14.262  14.949   0.687
## 6             cluster.data          4          0  27.548  27.896   0.348
## 9  partition.data.training          6          0  29.112  29.423   0.311
## 7      manage.missing.data          4          1  27.897  27.994   0.097
## 4           transform.data          2          2  26.155  26.218   0.063
##    duration
## 11   98.349
## 16   40.248
## 10   34.362
## 15   33.118
## 12   29.076
## 13   13.531
## 2     9.272
## 14    2.200
## 3     1.932
## 5     1.329
## 8     1.117
## 1     0.687
## 6     0.348
## 9     0.311
## 7     0.097
## 4     0.063
## [1] "Total Elapsed Time: 280.312 secs"
```

![](LendingClub_template2_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.1 (2015-06-18)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
##  [1] tcltk     grid      parallel  stats     graphics  grDevices utils    
##  [8] datasets  methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-8          Matrix_1.2-1        MASS_7.3-41        
##  [7] rpart.plot_1.5.2    rpart_4.1-9         ROCR_1.0-7         
## [10] gplots_2.17.0       dplyr_0.4.2         plyr_1.8.3         
## [13] sqldf_0.4-10        RSQLite_1.0.0       DBI_0.3.1          
## [16] gsubfn_0.6-6        proto_0.3-10        reshape2_1.4.1     
## [19] caTools_1.17.1      doMC_1.3.3          iterators_1.0.7    
## [22] foreach_1.4.2       doBy_4.5-13         survival_2.38-2    
## [25] caret_6.0-47        ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.6         class_7.3-12        gtools_3.5.0       
##  [4] assertthat_0.1      digest_0.6.8        R6_2.0.1           
##  [7] BradleyTerry2_1.0-6 chron_2.3-47        coda_0.17-1        
## [10] evaluate_0.7        e1071_1.6-4         lazyeval_0.1.10    
## [13] minqa_1.2.4         SparseM_1.6         car_2.0-25         
## [16] nloptr_1.0.4        rmarkdown_0.7       labeling_0.3       
## [19] splines_3.2.1       stringr_1.0.0       munsell_0.4.2      
## [22] compiler_3.2.1      mgcv_1.8-6          htmltools_0.2.6    
## [25] nnet_7.3-9          codetools_0.2-11    brglm_0.5-9        
## [28] bitops_1.0-6        nlme_3.1-120        gtable_0.1.2       
## [31] magrittr_1.5        formatR_1.2         scales_0.2.5       
## [34] KernSmooth_2.23-14  stringi_0.5-5       RColorBrewer_1.1-2 
## [37] tools_3.2.1         abind_1.4-3         pbkrtest_0.4-2     
## [40] yaml_2.1.13         colorspace_1.2-6    knitr_1.10.5       
## [43] quantreg_5.11
```
