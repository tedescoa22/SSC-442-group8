library(plyr)
library(tidyverse)
ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",", na.strings=c(""," ", "NA"))

# Exercise 1 part 1

#dropping OverallCond and OverallQual
dropped <- -c(1,18, 19,27)
ames = ameslist[dropped]
Ames <- ames[sapply(ames, is.numeric)]

reg = lm(SalePrice ~ 1, data = Ames)

#Not sure if FitAll is needed
FitAll = lm(SalePrice ~ ., data=Ames)
formula(FitAll)
FitStart = lm(SalePrice)
step(reg, direction = "forward", 
     scope=(~MSSubClass + LotFrontage + LotArea + MasVnrArea + YearBuilt + YearRemodAdd + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF))
#+ X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea +
#BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces + GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea
#+MiscVal + MoSold + YrSold)


step(FitAll,ddirection = "forward", scope = formula(FitAll))

# Exercise 1 part 2
forward_selc <- step(reg, direction = "forward", 
     scope=~MSSubClass + LotArea +  YearBuilt + YearRemodAdd + BsmtFinSF1 +  BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF +  GrLivArea + 
       FullBath +  HalfBath + BedroomAbvGr + KitchenAbvGr +  TotRmsAbvGrd + Fireplaces )

reg <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces, data = Ames)





#SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars +
#KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd +
#Fireplaces

# Excercis 1 part 3

rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}


get_complexity = function(model) {
  length(coef(model)) - 1
}

reg1 <- lm(SalePrice ~ GrLivArea  , data = Ames)
reg2 <- lm(SalePrice ~ GrLivArea + YearBuilt  , data = Ames)
reg3 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF  , data = Ames)
reg4 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars, data = Ames)
reg5 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr, data = Ames)
reg6 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr + BedroomAbvGr, data = Ames)
reg7 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd, data = Ames)
reg8 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd +  BsmtFinSF1, data = Ames)
reg9 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd +  BsmtFinSF1  + YearRemodAdd, data = Ames)
reg10 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces  , data = Ames)
reg11 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass , data = Ames)
reg12 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea  , data = Ames)
reg13 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea + ScreenPorch, data = Ames)
reg14 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea + ScreenPorch + WoodDeckSF , data = Ames)
reg15 <- lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea + ScreenPorch + WoodDeckSF + LotArea , data = Ames)

act = Ames$SalePrice

pred1 = reg1$fitted.values
pred2 = reg2$fitted.values
pred3 = reg3$fitted.values
pred4 = reg4$fitted.values
pred5 = reg5$fitted.values
pred6 = reg6$fitted.values
pred7 = reg7$fitted.values
pred8 = reg8$fitted.values
pred9 = reg9$fitted.values
pred10 = reg10$fitted.values
pred11 = reg11$fitted.values
pred12 = reg12$fitted.values
pred13 = reg13$fitted.values
pred14 = reg14$fitted.values
pred15 = reg15$fitted.values


complex1 = get_complexity(reg1)
complex2 = get_complexity(reg2)
complex3 = get_complexity(reg3)
complex4 = get_complexity(reg4)
complex5 = get_complexity(reg5)
complex6 = get_complexity(reg6)
complex7 = get_complexity(reg7)
complex8 = get_complexity(reg8)
complex9 = get_complexity(reg9)
complex10 = get_complexity(reg10)
complex11 = get_complexity(reg11)
complex12 = get_complexity(reg12)
complex13 = get_complexity(reg13)
complex14 = get_complexity(reg14)
complex15 = get_complexity(reg15)





RMSE1 <- rmse(act,pred1)
RMSE2 <- rmse(act,pred2)
RMSE3 <- rmse(act,pred3)
RMSE4 <- rmse(act,pred4)
RMSE5 <- rmse(act,pred5)
RMSE6 <- rmse(act,pred6)
RMSE7 <- rmse(act,pred7)
RMSE8 <- rmse(act,pred8)
RMSE9 <- rmse(act,pred9)
RMSE10 <- rmse(act,pred10)
RMSE11 <- rmse(act,pred11)
RMSE12 <- rmse(act,pred12)
RMSE13 <- rmse(act,pred13)
RMSE14 <- rmse(act,pred14)
RMSE15 <- rmse(act,pred15)

Complex = c(complex1,complex2,complex3,complex4,complex5,complex6,complex7,complex8,complex9,complex10,complex11,complex12,complex13,complex14,complex15)
RMSE= c(RMSE1, RMSE2,RMSE3,RMSE4,RMSE5,RMSE6,RMSE7,RMSE8,RMSE9,RMSE10,RMSE11,RMSE12,RMSE13,RMSE14,RMSE15)
plot(Complex, RMSE)









set.seed(9)
num_obs = nrow(Ames)

train_index = sample(num_obs, size = trunc(0.50 * num_obs))
train_data = Ames[train_index, ]
test_data = Ames[-train_index, ]



fit_0 = lm(SalePrice ~ 1, data = train_data)
get_complexity(fit_0)

# train RMSE
sqrt(mean((train_data$SalePrice - predict(fit_0, train_data)) ^ 2))
# test RMSE
sqrt(mean((test_data$SalePrice - predict(fit_0, test_data)) ^ 2))

# train RMSE
rmse(actual = train_data$SalePrice, predicted = predict(fit_0, train_data))
# test RMSE
rmse(actual = test_data$SalePrice, predicted = predict(fit_0, test_data))


get_rmse = function(model, data, response) {
  rmse(actual = subset(data, select = response, drop = TRUE),
       predicted = predict(model, data))
}


get_rmse(model = fit_0, data = train_data, response = "SalePrice") # train RMSE
get_rmse(model = fit_0, data = test_data, response = "SalePrice") # test RMSE

fit_1 = lm(SalePrice ~ GrLivArea , data = train_data)
fit_2 = lm(SalePrice ~ GrLivArea + YearBuilt , data = train_data)
fit_3 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF , data = train_data)
fit_4 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars , data = train_data)
fit_5 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr , data = train_data)


model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5)


train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)


# This is the same as the apply command above

test_rmse = c(get_rmse(fit_1, test_data, "SalePrice"),
              get_rmse(fit_2, test_data, "SalePrice"),
              get_rmse(fit_3, test_data, "SalePrice"),
              get_rmse(fit_4, test_data, "SalePrice"),
              get_rmse(fit_5, test_data, "SalePrice"))

plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")


# Exercise 2 part 1

fit_1 = lm(SalePrice ~ GrLivArea , data = train_data)
fit_2 = lm(SalePrice ~ GrLivArea + YearBuilt , data = train_data)
fit_3 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF , data = train_data)
fit_4 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars , data = train_data)
fit_5 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF +GarageCars +KitchenAbvGr , data = train_data)
fit_6 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr ,data = train_data)
fit_7 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd ,data = train_data)
fit_8 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 ,data = train_data)
fit_9 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd ,data = train_data)
fit_10 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces  ,data = train_data)
fit_11 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass , data = train_data)
fit_12 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea , data = train_data)
fit_13 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea + ScreenPorch , data = train_data)
fit_14 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea + ScreenPorch + WoodDeckSF , data = train_data)
fit_15 = lm(SalePrice ~ GrLivArea + YearBuilt + TotalBsmtSF + GarageCars + KitchenAbvGr + BedroomAbvGr + TotRmsAbvGrd + BsmtFinSF1 + YearRemodAdd + Fireplaces + MSSubClass + PoolArea + ScreenPorch + WoodDeckSF + LotArea , data = train_data)



model_list = list(fit_1, fit_2, fit_3, fit_4, fit_5,fit_6,fit_7,fit_8,fit_9,fit_10,fit_11,fit_12,fit_13,fit_14,fit_15)


train_rmse = sapply(model_list, get_rmse, data = train_data, response = "SalePrice")
test_rmse = sapply(model_list, get_rmse, data = test_data, response = "SalePrice")
model_complexity = sapply(model_list, get_complexity)



plot(model_complexity, train_rmse, type = "b",
     ylim = c(min(c(train_rmse, test_rmse)) - 0.02,
              max(c(train_rmse, test_rmse)) + 0.02),
     col = "dodgerblue",
     xlab = "Model Size",
     ylab = "RMSE")
lines(model_complexity, test_rmse, type = "b", col = "darkorange")




