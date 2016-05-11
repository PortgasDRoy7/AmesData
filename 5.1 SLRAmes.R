# STEP WISE REGRESSION
download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

attach(ames)
#Create Factors
Age = Yr.Sold - Year.Built
ames$Age = Age
ames$MS.SubClass = factor(MS.SubClass)

new.ames = ames
#Get rid of the outliers and only including residential categories
new.ames = new.ames[which(new.ames$MS.Zoning == "RL"  #Residential Low Density
                          |new.ames$MS.Zoning == "RH" #Residential High
                          |new.ames$MS.Zoning == "RM" #Residential Medium
                          |new.ames$MS.Zoning == "FV" #Floating Village
                          |new.ames$MS.Zoning == "RP"),]#Low Density Park. (No values after second filter)

new.ames = new.ames[which(new.ames$MS.SubClass == "20"  #One Story 1946-
                          |new.ames$MS.SubClass == "30" #One Story -1945
                          |new.ames$MS.SubClass == "50" #1.5 Stories Finished
                          |new.ames$MS.SubClass == "60" #2 Story 1946-
                          |new.ames$MS.SubClass == "70" #2 Story -1945
                          |new.ames$MS.SubClass == "80" #MultiLevel
                          |new.ames$MS.SubClass == "90" #Duplex
                          |new.ames$MS.SubClass == "120" #1 Story PUD
                          |new.ames$MS.SubClass == "160" #2 Story PUD
                          |new.ames$MS.SubClass == "190"),] # 2 family conversion.
new.ames = subset(new.ames, new.ames$Gr.Liv.Area <= 4475) 

# splitting data into train and validation set
set.seed(1)
train.ratio = 0.8
s = sample(1:dim(new.ames)[1],round(train.ratio*dim(new.ames)[1],0))
train = new.ames[s,];
test = new.ames[-s,];

# model with all reasonable variables included. 
slr.model = SalePrice ~ Gr.Liv.Area

library(leaps)

regr_slr = lm(slr.model, data = train, na.action=na.exclude)
k = summary(regr_slr)
AIC(regr_slr)
BIC(regr_slr)

yf = fitted.values(regr_sel)   # fitted values
e = residuals(regr_sel) # residuals
mean(e, na.rm=TRUE)             # a property of the residuals: zero mean 
hist(e, 30, col = 'blue', main = 'histogram of residual')  
plot(train$SalePrice ,yf[1:2227], col ="red", main="Training Data: Predicted vs Observed") # predicted vs observed (any nonlinearity?)
plot(train$SalePrice, e, col ="red", main="Training Data: Residual Plot")  # residuals vs observed
abline(h=0)            # draws a horizontal line at zero 

