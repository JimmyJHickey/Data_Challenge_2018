##########################
##  Code for Jimmy
##  Byron Smith
##  7/19/2018
##
##  This code is written to demonstrate a risk score using logistic regression
##  as well as for Cox regression.

rm(list=ls())
library(survival)
library(pROC) # For C-statistic

data(lung) # From the survival package.
##  This data set looks at survival of patients in a lung cancer study.



##  First, we look at logistic regression.
##  We could ask, what is the risk of a 60 year old male patient dying
##  within the first year of the study.  To do this, we:
##  1) Take all patients that died within one year or made it to 1 year.
##  2) Run a logistic regression on this data set.
##  3) Predict the probability of death of the new patient.

# Step 1
lung2 <- lung[which(lung$time>365 | lung$status==2),]
lung2$status <- lung2$status*as.numeric(lung2$time<=365)

# Step 2
logistic.fit <- glm((status==2)~age+sex, data=lung2, family="binomial")
summary(logistic.fit)

# Step 3
predict(logistic.fit, newdata=data.frame(age=60, sex=1), type="response")
# 0.7000969 This is the risk!!

##  Therefore, the risk of a 60 year old male patient dying within the first year is
##  predicted to be 70%.

##  For further model validation you can calculate the area under the ROC curve:
auc(lung2$status, predict(logistic.fit))
# 0.6041 which isn't great...

##########################################
##  Now let's try with Cox regression!
##########################################

cox.fit <- coxph(Surv(time, status==2)~age+sex, data=lung)
summary(cox.fit)

sf1 <- survfit(cox.fit, newdata=data.frame(age=60, sex=1))
1-summary(sf1, times=365)$surv
# 0.6447738 This is the risk using survival analysis!

##  The equivalent to the area under the ROC curve for survival analysis
##  is called the concordance:
summary(cox.fit)$concordance
# C = 0.60285300 which is pretty similar.