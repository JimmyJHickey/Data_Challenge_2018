########################
# Jimmy Hickey
# 2018/08/16
# Analysis of clinical variables

library(survival)
library(glmnet)
library(randomForestSRC)


set.seed(347934)

training = read.csv("../data/Training_clinical.csv")
test = read.csv("../data/Test_clinical.csv")

# 70% training 30% validation
training.obs = sample(1:nrow(training), 2/3 * nrow(training), replace=F)
test.obs = (1:nrow(training))[-training.obs]

# Split out training and test sets

x = training[training.obs,3:9]
x.test = training[test.obs,3:9]

y = Surv(time = training$Survival.time[training.obs],
         event = training$deadstatus.event[training.obs])
y2 = training[training.obs, 10:11]
y.test = Surv(time = training$Survival.time[test.obs], 
              event = training$deadstatus.event[test.obs])

training_set = training[training.obs, 3:11]
test_set = training[test.obs, 3:11]
test_data = test[,3:9]

# Let's try some random forests!
rf1 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set[,-6],
                 nodedepth = 4, ntree = 1000, mtry = 3)
# error rate 44.37%
rf1_pred = predict(rf1, newdata=test_set, type='lp')$predicted
# test set error rate 45.78%

rf2 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set[,-6],
            nodedepth = 6, ntree = 1000, mtry = 3)
# error rate 43.96%
rf2_pred = predict(rf2, newdata=test_set, type='lp')$predicted
# test set error rate 45.97%

rf3 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set[,-6],
            nodedepth = 6, ntree = 1000)
# error rate 44.79%
rf3_pred = predict(rf3, newdata=test_set, type='lp')$predicted
# test set error rate 45.75%


rf4 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set[,-6],
            nodedepth = 10, ntree = 100, mtry = 5)
# error rate 42.8%
rf4_pred = predict(rf4, newdata=test_set, type='lp')$predicted
# test set error rate 47.57%


rf5 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set[,-6],
           nodedepth = 5, ntree = 100)
# error rate 44.59%
rf5_pred = predict(rf5, newdata=test_set, type='lp')$predicted
# test set error rate 45.89%

rf1pred_training = predict(rf1, newdata=training[,c(-1,-2,-8,-10,-11,-12,-13)], type='lp')$predicted
rf2pred_training = predict(rf2, newdata=training[,c(-1,-2,-8,-10,-11,-12,-13)], type='lp')$predicted
rf3pred_training = predict(rf3, newdata=training[,c(-1,-2,-8,-10,-11,-12,-13)], type='lp')$predicted
rf4pred_training = predict(rf4, newdata=training[,c(-1,-2,-8,-10,-11,-12,-13)], type='lp')$predicted
rf5pred_training = predict(rf5, newdata=training[,c(-1,-2,-8,-10,-11,-12,-13)], type='lp')$predicted




rf_preds_train = rbind(c("RF1", "RF2", "RF3", "RF4", "RF5"), cbind(
  rf1pred_training,
  rf2pred_training,
  rf3pred_training,
  rf4pred_training,
  rf5pred_training))

write.csv(rf_preds_train, file="../data/RF_preds_train.csv")

rf1pred_test = predict(rf1, newdata=test_data[,-6], type='lp')$predicted
rf2pred_test = predict(rf2, newdata=test_data[,-6], type='lp')$predicted
rf3pred_test = predict(rf3, newdata=test_data[,-6], type='lp')$predicted
rf4pred_test = predict(rf4, newdata=test_data[,-6], type='lp')$predicted
rf5pred_test = predict(rf5, newdata=test_data[,-6], type='lp')$predicted


rf_preds_test = rbind(c("RF1", "RF2", "RF3", "RF4", "RF5"), cbind(
  rf1pred_test,
  rf2pred_test,
  rf3pred_test,
  rf4pred_test,
  rf5pred_test))

write.csv(rf_preds_test, file="../data/RF_preds_test.csv")

# Trying some penalized!
x_matrix = model.matrix(~age + Clinical.T.Stage + Clinical.N.Stage +
                          Clinical.M.Stage + Overall.Stage + gender,
                        training[training.obs,])




get_lambda1 = cv.glmnet(x=x_matrix, y = y, family = "cox", alpha = 0)

lasso1 = glmnet(x_matrix, y, family="cox", alpha=1, lambda = get_lambda1$lambda.min)
coefs1 = as.numeric(coef(lasso1))  

# all the coefficients for ridge and lasso ended up being ~0, so we scrapped it



## ENSEMBLE

rf_data_train = rf_preds_train[-1,]
rf_data_test = rf_preds_test[-1,]

train_lps = read.csv("../data/TrainingLPs.csv")[,-1]
test_lps = read.csv("../data/TestLPs.csv")[,-1]


train_ensemble_data = cbind(training$Survival.time, training$deadstatus.event,
                            train_lps, rf_data_train)

test_ensemble_data = cbind(test_lps, rf_data_test)


# names
names(train_ensemble_data)[1] = "Survival.time"
names(train_ensemble_data)[2] = "deadstatus.event"
names(train_ensemble_data)[25] = "rf1pred"
names(train_ensemble_data)[26] = "rf2pred"
names(train_ensemble_data)[27] = "rf3pred"
names(train_ensemble_data)[28] = "rf4pred"
names(train_ensemble_data)[29] = "rf5pred"
names(train_ensemble_data)

train_ensemble_data[,25] = as.numeric(train_ensemble_data[,25])
train_ensemble_data[,26] = as.numeric(train_ensemble_data[,26])
train_ensemble_data[,27] = as.numeric(train_ensemble_data[,27])
train_ensemble_data[,28] = as.numeric(train_ensemble_data[,28])
train_ensemble_data[,29] = as.numeric(train_ensemble_data[,29])

names(test_ensemble_data)[23] = "rf1pred"
names(test_ensemble_data)[24] = "rf2pred"
names(test_ensemble_data)[25] = "rf3pred"
names(test_ensemble_data)[26] = "rf4pred"
names(test_ensemble_data)[27] = "rf5pred"
names(test_ensemble_data)

test_ensemble_data[,23] = as.numeric(test_ensemble_data[,23])
test_ensemble_data[,24] = as.numeric(test_ensemble_data[,24])
test_ensemble_data[,25] = as.numeric(test_ensemble_data[,25])
test_ensemble_data[,26] = as.numeric(test_ensemble_data[,26])
test_ensemble_data[,27] = as.numeric(test_ensemble_data[,27])


ensemble_rf = rfsrc(Surv(Survival.time, deadstatus.event)~., 
                    data = train_ensemble_data,
                    ntree = 1000)

ensemble_rf_predicted = predict(ensemble_rf, newdata=train_ensemble_data, type='lp')$predicted

ensemble_rf_test_predicted = predict(ensemble_rf, newdata=test_ensemble_data, type='lp')$predicted




ensemble_train_x_matrix = as.matrix(train_ensemble_data[,c(-1,-2)])
y = Surv(train_ensemble_data$Survival.time, train_ensemble_data$deadstatus.event)

get_lambda = cv.glmnet(x=ensemble_train_x_matrix, y = y, family = "cox", alpha = 1)
plot(get_lambda)
get_lambda$lambda.min
get_lambda$lambda.1se

lasso = glmnet(ensemble_train_x_matrix, y, family="cox", alpha=1, lambda = get_lambda$lambda.1se)
coefs = as.numeric(lasso$beta) 
coefs

ensemble_lasso_fit = coxph(y~., data=as.data.frame(ensemble_train_x_matrix), 
                           init = coefs, iter = 0)
summary(ensemble_lasso_fit)

ensemble_test_lp = predict(ensemble_lasso_fit, newdata = test_ensemble_data, type='lp')
View(ensemble_test_lp)


write.csv(ensemble_test_lp, file = "../data/ensemble_test_lp.csv")


# risks at 1, 2, 3 years
times = c(1,2,3) * 365.25

survival_fit = survfit(ensemble_lasso_fit, newdata = test_ensemble_data)
risks = summary(survival_fit, times=times)$surv
risks = t(risks)
risks = as.data.frame(risks)
names(risks) = c("one_year_risk", "two_year_risk", "three_year_risk")

write.csv(risks, file = "../data/test_risk.csv")
