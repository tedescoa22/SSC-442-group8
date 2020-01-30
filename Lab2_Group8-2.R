library(plyr)

ameslist <- read.table("https://msudataanalytics.github.io/SSC442/Labs/data/ames.csv",
                       header = TRUE,
                       sep = ",")
names(ameslist)

typeof(ameslist)

GarageType <- ameslist$GarageType
unique(ameslist$GarageType)

GarageTemp = model.matrix( ~ GarageType - 1, data=ameslist$GarageType )


ameslist <- merge(ameslist, GarageTemp, all = TRUE)

ameslist$GarageOutside <- ifelse(ameslist$GarageTypeDetchd == 1 | ameslist$GarageTypeCarPort == 1, 1, 0)
unique(ameslist$GarageOutside)

Ames <- ameslist[sapply(ameslist, is.numeric)]

# Excercise 1

# 1
Ames$MSSubClass <- NULL
Ames$MasVnrArea <- NULL
Ames$X3SsnPorch <- NULL


#2
Ames_plot <- subset(Ames, select = c(LotArea,OverallQual,OverallCond,TotalBsmtSF,FullBath,BedroomAbvGr,
                                     KitchenAbvGr,TotRmsAbvGrd,Fireplaces,GarageCars,GarageArea,HalfBath))

# Tkae very long to load
pairs(Ames_plot)


# 3
cor(Ames_plot,Ames$SalePrice)

# Lot area, Overall condition, Total basement, full bath, bedrooms above ground, total rooms above ground, fire places, garage cars, garage area and half bath are all positively correlated
# Overall condition and kitchen above ground are all negatively correlated
# A woud assume that all of these varaibles woud be postiivley correlated to sales price, especially overall condition


# 4
plot(GrLivArea, SalePrice)
# may take a while to load 
abline(a=18569.03,b=107.13)


# End exercise 1

lm.fit = lm(SalePrice ~ GrLivArea)

attach(Ames)
lm.fit = lm(SalePrice ~ GrLivArea, data = Ames)

lm.fit
summary(lm.fit)

coef(lm.fit)

lm.fit = lm(SalePrice ~ GrLivArea + LotArea)

# Excercise 2

# 1
lm(ameslist$SalePrice ~ Ames$GarageTypeBuiltIn)

# 2
z = as.data.frame(cbind(42,Ames))
multi_reg <- lm(ameslist$SalePrice ~ .,data = z)

summary(multi_reg)

# 3
plot(multi_reg)

#4
lm(ameslist$SalePrice ~ ameslist$GarageCars * ameslist$GarageYrBlt, data = z)
lm(ameslist$SalePrice ~ ameslist$LotShape : ameslist$LotArea, data = z)

#5
# transforms for natural log to reduce the size of the possible values of sales price in order to make the graph more legable

lm(log(ameslist$SalePrice) ~ ameslist$LotArea)

lm((ameslist$SalePrice)*0.5 ~ ameslist$LotArea)

lm((ameslist$SalePrice)*2 ~ ameslist$LotArea)


# Taking the log or the square root of sales price woud be benificial as it would reduce the value and make the graph more condensed
# Squaring the sale price woud not be benificial as it would have the opposite effect and woud make the graph more spread out

