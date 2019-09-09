remove(bike)
head(bike)
fit0=lm(trips~.,data = bike)
summary(fit0)
fit0.0=lm(trips~ASSAULT+BATTERY+BURGLARY+CRIMINAL_TRESPASS+DECEPTIVE_PRACTICEHOMICIDE+NARCOTICS+ROBBERY+THEFT)

#full model
fit.full = lm(trips~ASSAULT+BATTERY+BURGLARY+CRIMINAL_TRESPASS+DECEPTIVE_PRACTICE+HOMICIDE+NARCOTICS+ROBBERY+THEFT+PARK_AREA_ACRES+CTA_BUS_STATIONS+CTA_TRAIN_STATIONS+BIKE_ROUTES+Limited_Business_License+Retail_Food_Establishment+CAPACITY+PER_CAPITA_INCOME+POPULATION_SQ_MILE+CBD+MINORITY+EDU, bike, subset = train)
summary(fit.full)
yhat=predict(fit.full, bike[!train,])
mean((bike$trips[!train] - yhat)^2) 


x = model.matrix(trips ~ ASSAULT+BATTERY+BURGLARY+CRIMINAL_TRESPASS+DECEPTIVE_PRACTICE+HOMICIDE+NARCOTICS+ROBBERY+THEFT+PARK_AREA_ACRES+CTA_BUS_STATIONS+CTA_TRAIN_STATIONS+BIKE_ROUTES+Limited_Business_License+Retail_Food_Establishment+CAPACITY+PER_CAPITA_INCOME+POPULATION_SQ_MILE+CBD+MINORITY+EDU, bike)
fit.ridge = glmnet(x[train,], bike_new$trips[train], alpha=0)
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], bike_new$trips[train], alpha=0) # find optimal lambda fit.cv$lambda.min # optimal value of lambda
plot(fit.ridge) # plot MSE vs. log(lambda)
fit.cv$lambda.min
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,]) # find yhat for best model mean((college$Apps[!train] - yhat)^2)
mean((bike_new$trips[!train] - yhat)^2)

cor(bike)
###Measure CBD
fit1 = glm(CBD ~ PARK_AREA_ACRES + CTA_BUS_STATIONS + CTA_TRAIN_STATIONS + BIKE_ROUTES + Limited_Business_License +
             Retail_Food_Establishment + PER_CAPITA_INCOME + POPULATION_SQ_MILE 
           + MINORITY + EDU, family= binomial(link=logit), data=bike)
summary(fit1)
vif(fit1)

###create CBD positive & negative
bike$zroutes=scale(bike$BIKE_ROUTES)
bike$zlicense=scale(bike$Limited_Business_License)
bike$zfood=scale(bike$Retail_Food_Establishment)
bike$CBD_POSITIVE=(bike_zroutes+bike_zlicense+bike_zfood)/3
bike$zbus = scale(bike$CTA_BUS_STATIONS)
bike$ztrain = scale (bike$CTA_TRAIN_STATIONS)
bike$CBD_NEGATIVE = (bike$zbus + bike$ztrain)/2 #now we have park_area, cta negative, cta positive, capacity, income, population, minority, edu
head(bike)
bike$CBD_AVGNEGATIVE = (bike$CTA_BUS_STATIONS+bike$CTA_TRAIN_STATIONS)/2
bike$CBD_AVGPOSITIVE= (bike$BIKE_ROUTES+bike$Limited_Business_License+bike$Retail_Food_Establishment)/3

####measure crime
###correlation measure
cor(bike[,13:21], bike$trips)
#assault, battery, trespass, deceptive, robbery, theft positive, other negative
bike$zassault = scale(bike$ASSAULT)
bike$zbattery = scale(bike$BATTERY)
bike$zburglary = scale(bike$BURGLARY)
bike$ztrespass = scale(bike$CRIMINAL_TRESPASS)
bike$zdeceptive = scale(bike$DECEPTIVE_PRACTICE)
bike$zhomicide = scale(bike$HOMICIDE)
bike$znarcotics = scale(bike$NARCOTICS)
bike$zrobbery = scale(bike$ROBBERY)
bike$ztheft = scale(bike$THEFT)
bike$CRIME_POSITIVE = (bike$zassault+bike$zbattery+bike$ztrespass+bike$zdeceptive+bike$zrobbery+bike$ztheft)/6
bike$CRIME_NEGATIVE = (bike$zburglary+bike$zhomicide+bike$znarcotics)/3
bike$CRIME_POSITIVE_NEW = (bike$zbattery+bike$ztrespass+bike$zdeceptive+bike$zrobbery+bike$ztheft)/5
bike$CRIME_NEGATIVE_NEW = bike$znarcotics
bike$CRIME_AVGPOSITIVE = (bike$BATTERY+bike$CRIMINAL_TRESPASS+bike$DECEPTIVE_PRACTICE+bike$ROBBERY+bike$THEFT)/5
bike$CRIME_AVGNEGATIVE = bike$NARCOTICS
head(bike)

###set train and test
set.seed(12345)
train = runif(nrow(bike))<.5

###model testing
fit2 = lm(trips~CRIME_POSITIVE+CRIME_NEGATIVE+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+PER_CAPITA_INCOME+POPULATION_SQ_MILE+MINORITY+EDU, 
          data = bike, subset = train)
summary(fit2)
yhat=predict(fit2, bike[!train,])
vif(fit2)
mean((bike$trips[!train] - yhat)^2) #0.266
fit3 = lm(trips~CRIME_POSITIVE+CRIME_NEGATIVE+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY, 
          data = bike, subset = train)
summary(fit3)
vif(fit3)
yhat=predict(fit3, bike[!train,])
mean((bike$trips[!train] - yhat)^2) #0.2697
fit4 = lm(trips~CRIME_POSITIVE+CRIME_NEGATIVE+CTA_TRAIN_STATIONS+CTA_BUS_STATIONS+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY+EDU, 
              data = bike, subset = train)
summary(fit4)
yhat=predict(fit4, bike[!train,])
mean((bike$trips[!train] - yhat)^2) 
fit5 = lm(trips~weighted_positive+weighted_negative+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY+EDU, 
          data = bike, subset = train)
summary(fit5)
yhat=predict(fit5, bike[!train,])
mean((bike$trips[!train] - yhat)^2) 
fit6 = lm(trips~CRIME_AVGPOSITIVE+CRIME_AVGNEGATIVE+CBD_AVGNEGATIVE+CBD_AVGPOSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY, 
          data = bike, subset = train)
summary(fit6)
yhat=predict(fit6, bike[!train,])
mean((bike$trips[!train] - yhat)^2) 
fit7 = lm(trips~CRIME_AVGPOSITIVE+CRIME_AVGNEGATIVE+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY, 
          data = bike, subset = train)
summary(fit7)
yhat=predict(fit7, bike[!train,])
mean((bike$trips[!train] - yhat)^2) 
fit8 = lm(trips~CRIME_POSITIVE_NEW+CRIME_NEGATIVE_NEW+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY, 
          data = bike, subset = train)
summary(fit8)
yhat=predict(fit8, bike[!train,])
mean((bike$trips[!train] - yhat)^2) 
#forward
fit0=lm(trips~1, bike)
fit.forward=step(fit0, scope = ~CRIME_POSITIVE+CRIME_NEGATIVE+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY,
                 data = bike, subset = train)
yhat=predict(fit.forward, bike[!train,])
mean((bike$trips[!train] - yhat)^2)#0.2609
vif(fit.forward)

#ridge
x = model.matrix(trips ~ CRIME_POSITIVE+CRIME_NEGATIVE+CBD_NEGATIVE+CBD_POSITIVE+PARK_AREA_ACRES+CAPACITY+MINORITY+EDU, bike)
fit.ridge = glmnet(x[train,], bike$trips[train], alpha=0)
plot(fit.ridge, xvar="lambda")
fit.cv = cv.glmnet(x[train,], bike$trips[train], alpha=0) 
plot(fit.ridge) 
fit.cv$lambda.min
yhat = predict(fit.ridge, s=fit.cv$lambda.min, newx=x[!train,]) 
mean((bike$trips[!train] - yhat)^2) #0.264 with demographic variables and 0.2624 without

#lasso
fit.lasso = glmnet(x[train,], bike$trips[train], alpha=1) 
plot(fit.lasso, xvar="lambda")
fit.cv = cv.glmnet(x[train,], bike$trips[train], alpha=1) 
plot(fit.lasso) 
fit.cv$lambda.min
yhat = predict(fit.lasso, s=fit.cv$lambda.min, newx=x[!train,]) 
mean((bike$trips[!train] - yhat)^2) #0.2617

