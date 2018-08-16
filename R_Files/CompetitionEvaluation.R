#####################
##  Competition Evaluation
##  8/15/2018
##  Byron Smith
##
##  This code is written to demonstrate using cross-validation
##  and methods of evaluation of the models.

rm(list=ls())
library(survival)

set.seed(347934)

indir <- "C:/Users/m150170/Documents/Data_Challenge_2018-master/"
outdir <- "C:/Users/m150170/Documents/Data_Challenge_2018-master/"


data1 <- read.csv(paste0(indir, "TrainingLPs.csv"))
data2 <- read.csv(paste0(indir, "Training_Clinical.csv"))
data3 <- cbind(data2, data1[,-1])

##  The evaluations are
##  1) Survival concordance and
##  2) Calibration.
concordances.holdout <- NULL # We want these as close to 1 as possible
calibration.ps.holdout <- NULL # We want these as close to 1 as possible as well!
concordances.10fold <- rep(0, 10)
calibration.ps.10fold <- rep(0, 10)


# Holdout -----------------------------------------------------------------

training.obs <- sample(1:300, 200, replace=F)
test.obs <- (1:300)[-training.obs]

##  Define the training and test variables
y <- Surv(data3$Survival.time[training.obs], data3$deadstatus.event[training.obs])
x <- data3[training.obs,13:34]
y.test <- Surv(data3$Survival.time[test.obs], data3$deadstatus.event[test.obs])
x.test <- data3[test.obs,13:34]

##  Train the model on the 2/3 data
fit1 <- coxph(y~., data=x, iter=0, init=rep(1, ncol(x)))

##  Predict the model on the new data
preds <- predict(fit1, newdata=data3[test.obs, 13:34], type='lp')
concordances.holdout <- survConcordance(y.test~preds)$concordance # The concordance

##  Calculate the calibration
fit0 <- coxph(y.test~preds)
p <- log(predict(fit0, newdata=data.frame(preds), type="expected"))
lp <- predict(fit0, newdata=data.frame(preds), type="lp")
logbase <- p-lp
group <- cut(lp, c(-Inf, quantile(p, 1:9/10), Inf))
fit3 <- glm(data3$deadstatus.event[test.obs] ~ -1 + group + offset(p), family=poisson)

calibration.ps.holdout <- anova(fit3, test="Chisq")$`Pr(>Chi)`[2] # The calibration

# Ten fold CV -------------------------------------------------------------

random.ordering <- sample(1:300)

for(ii in 1:10){
  test.obs <- random.ordering[((ii-1)*30+1):(ii*30)]
  training.obs <- (1:300)[-test.obs]
  
  ##  Define the training and test variables
  y <- Surv(data3$Survival.time[training.obs], data3$deadstatus.event[training.obs])
  x <- data3[training.obs,13:34]
  y.test <- Surv(data3$Survival.time[test.obs], data3$deadstatus.event[test.obs])
  x.test <- data3[test.obs,13:34]
  
  ##  Train the model on the 2/3 data
  fit1 <- coxph(y~., data=x, iter=0, init=rep(1, ncol(x)))
  
  ##  Predict the model on the new data
  preds <- predict(fit1, newdata=data3[test.obs, 13:34], type='lp')
  concordances.10fold[ii] <- survConcordance(y.test~preds)$concordance # The concordance
  
  ##  Calculate the calibration
  fit0 <- coxph(y.test~preds)
  p <- log(predict(fit0, newdata=data.frame(preds), type="expected"))
  lp <- predict(fit0, newdata=data.frame(preds), type="lp")
  logbase <- p-lp
  group <- cut(lp, c(-Inf, quantile(p, 1:9/10), Inf))
  fit3 <- glm(data3$deadstatus.event[test.obs] ~ -1 + group + offset(p), family=poisson)
  
  calibration.ps.10fold[ii] <- anova(fit3, test="Chisq")$`Pr(>Chi)`[2] # The calibration
  
  
}

mean(concordances.10fold)
mean(calibration.ps.10fold)










