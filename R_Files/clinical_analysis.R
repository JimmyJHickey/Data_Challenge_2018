########################3
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


# Let's try some random forests!
rf1 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
                 nodedepth = 4, ntree = 1000, mtry = 3)
# error rate 46.39%

rf2 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
            nodedepth = 6, ntree = 1000, mtry = 3)
# error rate 44.76%

rf3 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
            nodedepth = 6, ntree = 1000)
# error rate 45.35%

rf4 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
            nodedepth = 10, ntree = 1000, mtry = 5)
# error rate 44.18%

rf5 = rfsrc(Surv(Survival.time, deadstatus.event)~., data = training_set,
           nodedepth = 5, ntree = 100)
# error rate 45.23%




# Trying some penalized!
x_matrix = model.matrix(~age + Clinical.T.Stage + Clinical.N.Stage + 
                          Clinical.M.Stage + Overall.Stage + Histology + gender,
                        training[training.obs,])

#### Matrix is different dimension because of NAs =[

lasso1 = glmnet(x_matrix, y, family="cox", alpha=1)

