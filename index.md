# Exercise Analysis
Chuck Stietzel  
10/30/2017  



## Synopsis
People are integrating technology into their lives in new ways.  At the forefront is the Quantified Self Movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. Most device applications try to identify the type of activity one is performing (e.g. Walking), the quantity of it (e.g. 200 steps), and/or monitoring certain body responses (e.g. heart rate).  This study attempts to determine how well one is performing a designated activity. Data collected from accelerometers on the belt, forearm, arm, and dumbbell of 6 participants is used. Each was asked to perform a dumbbell lift once correctly and then five different ways incorrectly.

A three classification models were investigated to correctly identify how an exercise was being performed (eClass).  A partitioning scheme based on the eClass was performed that allocated 75% of the data for model training and 25% for model selection.  A Random Forest model using 31 predictors was selected as the final model and achieved an out-of-sample classification accuracy of 99.25% on the validation set.

## Preparing the Data
### Loading the Data Set
A number of columns from the original data set were eliminated.  The row index user name, time stamps and window information were discarded.  These columns are not useful in the context of this study.  A prediction of valid exercise technique will be attained from the instantaneous sensor readings.  The activity classifications were loaded in as factors with levels [ABCDE].

Many columns had a large percentage of missing data (divide by zero values were also converted to NA).  Any column that had less than 90% valid measurements was discarded.  Of the remaining 52 variables that passed this threshold, each were 100% populated.


```r
getexdata <- function (datafile) {
  exdata_raw <- read_csv(datafile, na = c("NA", "#DIV/0!"), col_types = 
                         cols(
                           X1 = col_skip(), ##col_integer(),
                           user_name = col_skip(), ##col_character(),
                           raw_timestamp_part_1 = col_skip(), ##col_integer(),
                           raw_timestamp_part_2 = col_skip(), ##col_integer(),
                           cvtd_timestamp = col_skip(), ##col_character(),
                           new_window = col_skip(), ##col_character(),
                           num_window = col_skip(), ##col_integer(),
                           roll_belt = col_double(),
                           pitch_belt = col_double(),
                           yaw_belt = col_double(),
                           total_accel_belt = col_double(),
                           kurtosis_roll_belt = col_double(),
                           kurtosis_picth_belt = col_double(),
                           kurtosis_yaw_belt = col_double(),
                           skewness_roll_belt = col_double(),
                           skewness_roll_belt.1 = col_double(),
                           skewness_yaw_belt = col_double(),
                           max_roll_belt = col_double(),
                           max_picth_belt = col_double(),
                           max_yaw_belt = col_double(),
                           min_roll_belt = col_double(),
                           min_pitch_belt = col_double(),
                           min_yaw_belt = col_double(),
                           amplitude_roll_belt = col_double(),
                           amplitude_pitch_belt = col_double(),
                           amplitude_yaw_belt = col_double(),
                           var_total_accel_belt = col_double(),
                           avg_roll_belt = col_double(),
                           stddev_roll_belt = col_double(),
                           var_roll_belt = col_double(),
                           avg_pitch_belt = col_double(),
                           stddev_pitch_belt = col_double(),
                           var_pitch_belt = col_double(),
                           avg_yaw_belt = col_double(),
                           stddev_yaw_belt = col_double(),
                           var_yaw_belt = col_double(),
                           gyros_belt_x = col_double(),
                           gyros_belt_y = col_double(),
                           gyros_belt_z = col_double(),
                           accel_belt_x = col_double(),
                           accel_belt_y = col_double(),
                           accel_belt_z = col_double(),
                           magnet_belt_x = col_double(),
                           magnet_belt_y = col_double(),
                           magnet_belt_z = col_double(),
                           roll_arm = col_double(),
                           pitch_arm = col_double(),
                           yaw_arm = col_double(),
                           total_accel_arm = col_double(),
                           var_accel_arm = col_double(),
                           avg_roll_arm = col_double(),
                           stddev_roll_arm = col_double(),
                           var_roll_arm = col_double(),
                           avg_pitch_arm = col_double(),
                           stddev_pitch_arm = col_double(),
                           var_pitch_arm = col_double(),
                           avg_yaw_arm = col_double(),
                           stddev_yaw_arm = col_double(),
                           var_yaw_arm = col_double(),
                           gyros_arm_x = col_double(),
                           gyros_arm_y = col_double(),
                           gyros_arm_z = col_double(),
                           accel_arm_x = col_double(),
                           accel_arm_y = col_double(),
                           accel_arm_z = col_double(),
                           magnet_arm_x = col_double(),
                           magnet_arm_y = col_double(),
                           magnet_arm_z = col_double(),
                           kurtosis_roll_arm = col_double(),
                           kurtosis_picth_arm = col_double(),
                           kurtosis_yaw_arm = col_double(),
                           skewness_roll_arm = col_double(),
                           skewness_pitch_arm = col_double(),
                           skewness_yaw_arm = col_double(),
                           max_roll_arm = col_double(),
                           max_picth_arm = col_double(),
                           max_yaw_arm = col_double(),
                           min_roll_arm = col_double(),
                           min_pitch_arm = col_double(),
                           min_yaw_arm = col_double(),
                           amplitude_roll_arm = col_double(),
                           amplitude_pitch_arm = col_double(),
                           amplitude_yaw_arm = col_double(),
                           roll_dumbbell = col_double(),
                           pitch_dumbbell = col_double(),
                           yaw_dumbbell = col_double(),
                           kurtosis_roll_dumbbell = col_double(),
                           kurtosis_picth_dumbbell = col_double(),
                           kurtosis_yaw_dumbbell = col_double(),
                           skewness_roll_dumbbell = col_double(),
                           skewness_pitch_dumbbell = col_double(),
                           skewness_yaw_dumbbell = col_double(),
                           max_roll_dumbbell = col_double(),
                           max_picth_dumbbell = col_double(),
                           max_yaw_dumbbell = col_double(),
                           min_roll_dumbbell = col_double(),
                           min_pitch_dumbbell = col_double(),
                           min_yaw_dumbbell = col_double(),
                           amplitude_roll_dumbbell = col_double(),
                           amplitude_pitch_dumbbell = col_double(),
                           amplitude_yaw_dumbbell = col_double(),
                           total_accel_dumbbell = col_double(),
                           var_accel_dumbbell = col_double(),
                           avg_roll_dumbbell = col_double(),
                           stddev_roll_dumbbell = col_double(),
                           var_roll_dumbbell = col_double(),
                           avg_pitch_dumbbell = col_double(),
                           stddev_pitch_dumbbell = col_double(),
                           var_pitch_dumbbell = col_double(),
                           avg_yaw_dumbbell = col_double(),
                           stddev_yaw_dumbbell = col_double(),
                           var_yaw_dumbbell = col_double(),
                           gyros_dumbbell_x = col_double(),
                           gyros_dumbbell_y = col_double(),
                           gyros_dumbbell_z = col_double(),
                           accel_dumbbell_x = col_double(),
                           accel_dumbbell_y = col_double(),
                           accel_dumbbell_z = col_double(),
                           magnet_dumbbell_x = col_double(),
                           magnet_dumbbell_y = col_double(),
                           magnet_dumbbell_z = col_double(),
                           roll_forearm = col_double(),
                           pitch_forearm = col_double(),
                           yaw_forearm = col_double(),
                           kurtosis_roll_forearm = col_double(),
                           kurtosis_picth_forearm = col_double(),
                           kurtosis_yaw_forearm = col_double(),
                           skewness_roll_forearm = col_double(),
                           skewness_pitch_forearm = col_double(),
                           skewness_yaw_forearm = col_double(),
                           max_roll_forearm = col_double(),
                           max_picth_forearm = col_double(),
                           max_yaw_forearm = col_double(),
                           min_roll_forearm = col_double(),
                           min_pitch_forearm = col_double(),
                           min_yaw_forearm = col_double(),
                           amplitude_roll_forearm = col_double(),
                           amplitude_pitch_forearm = col_double(),
                           amplitude_yaw_forearm = col_double(),
                           total_accel_forearm = col_double(),
                           var_accel_forearm = col_double(),
                           avg_roll_forearm = col_double(),
                           stddev_roll_forearm = col_double(),
                           var_roll_forearm = col_double(),
                           avg_pitch_forearm = col_double(),
                           stddev_pitch_forearm = col_double(),
                           var_pitch_forearm = col_double(),
                           avg_yaw_forearm = col_double(),
                           stddev_yaw_forearm = col_double(),
                           var_yaw_forearm = col_double(),
                           gyros_forearm_x = col_double(),
                           gyros_forearm_y = col_double(),
                           gyros_forearm_z = col_double(),
                           accel_forearm_x = col_double(),
                           accel_forearm_y = col_double(),
                           accel_forearm_z = col_double(),
                           magnet_forearm_x = col_double(),
                           magnet_forearm_y = col_double(),
                           magnet_forearm_z = col_double(),
                           classe = col_factor(c("A", "B", "C", "D", "E"))
                         ))
  ## Calculate the % of valid values for each column
  poppct <- sapply(exdata_raw, function (x) mean(!is.na(x)))
  exdata_raw[,poppct > 0.90]
}

exdata <- getexdata("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")

dim(exdata)
```

```
## [1] 19622    53
```

### Feature Selection
The final data set has 52 predictors despite ignoring nearly 2/3 of the original predictors due to lack of data. The correlation of the remaining predictors was evaluated.  Many exhibited a high degree of correlation.  In an effort to further reduce the dimensionality of the data, any variables that exhibited correlations of 0.75 or more were removed from the final predictor set. This cutoff eliminated 21 predictors leaving 31 for model training.

```r
rm.pred <- findCorrelation(cor(exdata[,1:52]), cutoff=.75)
length(rm.pred)
```

```
## [1] 21
```

### Training and Valiation Sets
With nearly 20,000 observations available, a training set using 75% of the observations provides ample data for estimating the parameters of the various models. The remaining 25% was used as the validation set for out of sample testing. The model with the highest accuracy on the validation set will be selected as the final model.


```r
set.seed(1234)
inTrain <- createDataPartition(y=exdata$classe, p=0.75, list=FALSE)
extrain <- exdata[inTrain, -rm.pred]; extest <- exdata[-inTrain, -rm.pred]
dim(extrain)
```

```
## [1] 14718    32
```

## Model Selection 
### Tuning & Cross Validation
Three different model types were tested for this analysis: Linear Discriminant, Boosted Tree, and Random Forest. The *caret* package's *train* function was used to tune each model with default tuning parameters. The optimal parameters for each model were determined by optimizing for classification accuracy using 10-fold cross validation. 


```r
fitControl <- trainControl(method = "cv", number = 10, allowParallel = TRUE)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
## Linear Discriminant Analysis
set.seed(234)
lda.fit <- train(classe ~ ., method="lda", data=extrain, trControl = fitControl, verbose=FALSE)
## Booosted Tree
set.seed(234)
gbm.fit <- train(classe ~ ., method="gbm", data=extrain, trControl = fitControl, verbose=FALSE)
## Random Forest
set.seed(234)
rf.fit <- train(classe ~ ., method="rf", data=extrain, trControl = fitControl, verbose=FALSE)
stopCluster(cluster)
registerDoSEQ()
```

## Results and Conclusion


```r
mdlstats <- summary(resamples(list(lda=lda.fit,gbm=gbm.fit, rf=rf.fit)))

lda.conf <- confusionMatrix(extest$classe, predict(lda.fit, extest))
gbm.conf <- confusionMatrix(extest$classe, predict(gbm.fit, extest))
rf.conf <- confusionMatrix(extest$classe, predict(rf.fit, extest))

mdlcomp <- cbind(rbind(lda.conf$overall[1], gbm.conf$overall[1], rf.conf$overall[1]),
                 mdlstats$statistics$Accuracy[,c(1,3,6)])

kable(mdlcomp, format = "html", digits = rep(4, 4),
      col.names = c("Accuracy", "Min", "Median", "Max")) %>% 
  kable_styling(full_width = F, c("bordered")) %>%
    add_header_above(c("Model" = 1, "Test Set" = 1, "Training Set Accuracy" = 3)) %>%
  column_spec(1:5, width = "1.0in")
```

<table class="table table-bordered" style="width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:center; border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;" colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Model</div></th>
<th style="text-align:center; border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;" colspan="1"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Test Set</div></th>
<th style="text-align:center; border-bottom:hidden; padding-bottom:0; padding-left:3px;padding-right:3px;" colspan="3"><div style="border-bottom: 1px solid #ddd; padding-bottom: 5px;">Training Set Accuracy</div></th>
</tr>
<tr>
<th style="text-align:left;">   </th>
   <th style="text-align:right;"> Accuracy </th>
   <th style="text-align:right;"> Min </th>
   <th style="text-align:right;"> Median </th>
   <th style="text-align:right;"> Max </th>
  </tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;width: 1.0in; "> lda </td>
   <td style="text-align:right;width: 1.0in; "> 0.5901 </td>
   <td style="text-align:right;width: 1.0in; "> 0.5693 </td>
   <td style="text-align:right;width: 1.0in; "> 0.5838 </td>
   <td style="text-align:right;width: 1.0in; "> 0.5883 </td>
  </tr>
<tr>
<td style="text-align:left;width: 1.0in; "> gbm </td>
   <td style="text-align:right;width: 1.0in; "> 0.9511 </td>
   <td style="text-align:right;width: 1.0in; "> 0.9368 </td>
   <td style="text-align:right;width: 1.0in; "> 0.9443 </td>
   <td style="text-align:right;width: 1.0in; "> 0.9531 </td>
  </tr>
<tr>
<td style="text-align:left;width: 1.0in; "> rf </td>
   <td style="text-align:right;width: 1.0in; "> 0.9925 </td>
   <td style="text-align:right;width: 1.0in; "> 0.9891 </td>
   <td style="text-align:right;width: 1.0in; "> 0.9912 </td>
   <td style="text-align:right;width: 1.0in; "> 0.9939 </td>
  </tr>
</tbody>
</table>

The table above shows a summary of performance accuracy on the in-sample model training and on the held-out validation/test set.  The min, max and median classification accuracy statistics are reported for the 10-fold re-samples. The linear discriminant model performed very poorly both in and out-of-sample; not achieving 60% accuracy.  Boosted tree did far better by recording an accuracy of 95% on the held-out test set. However, in-sample results were as low as 92%. The random forest model achieved better than 99% accuracy on both in and out-of-sample tests.  Based on these accuracy results, the random forest model constructed would be able to reasonably predict the eClass of the dumbbell lift. 
