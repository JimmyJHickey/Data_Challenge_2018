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
rf1 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
                 nodedepth = 4, ntree = 1000, mtry = 3)
# error rate 46.39%
predict(rf1, newdata=test_set, type='lp')
# test set error rate 47.59%

rf2 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
            nodedepth = 6, ntree = 1000, mtry = 3)
# error rate 44.76%
predict(rf2, newdata=test_set, type='lp')
# test set error rate 47.45%

rf3 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
            nodedepth = 6, ntree = 1000)
# error rate 42.53%
predict(rf3, newdata=test_set, type='lp')
# test set error rate 47.48%


rf4 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
            nodedepth = 10, ntree = 100, mtry = 5)
# error rate 44.18%
predict(rf4, newdata=test_set, type='lp')
# test set error rate 49.18%


rf5 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
           nodedepth = 5, ntree = 100)
# error rate 45.23%
predict(rf5, newdata=test_set, type='lp')
# test set error rate 48.28%



rf1pred = predict(rf1, newdata=test_data, type='lp')
rf2pred = predict(rf2, newdata=test_data, type='lp')
rf3pred = predict(rf3, newdata=test_data, type='lp')
rf4pred = predict(rf4, newdata=test_data, type='lp')
rf5pred = predict(rf5, newdata=test_data, type='lp')

# Trying some penalized!
# x_matrix = model.matrix(~age + Clinical.T.Stage + Clinical.N.Stage + 
#                           Clinical.M.Stage + Overall.Stage + Histology + gender,
#                         training[training.obs,])
# 
# #### Matrix is different dimension because of NAs =[
# 
# lasso1 = glmnet(x_matrix, y, family="cox", alpha=1)

