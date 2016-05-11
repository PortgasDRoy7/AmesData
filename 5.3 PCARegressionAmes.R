##############################
#                            #
#       PCA REGRESSION       #
#                            #
##############################

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
library(ggplot2)
library(cvTools)
attach(ames)
#Create Factors
Age = Yr.Sold - Year.Built
ames$Age = Age
ames$MS.SubClass = factor(MS.SubClass)
ames$Mo.Sold = factor(Mo.Sold)

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

# model with all reasonable variables included. 
fake.model = Order ~ SalePrice + Gr.Liv.Area + MS.Zoning + MS.SubClass +
  Street + Lot.Shape + Land.Contour + Utilities +
  Lot.Config + Land.Slope + Neighborhood + Condition.1 +
  Condition.2 + Bldg.Type +House.Style + Overall.Qual+
  Overall.Cond + Year.Built  +Year.Remod.Add  + Roof.Style  +
  Roof.Matl	+Exterior.1st	+Exterior.2nd	+ Mas.Vnr.Type +
  Mas.Vnr.Area +	Exter.Qual +Exter.Cond +	Foundation	+  Bsmt.Qual	+ Bsmt.Cond	+ 
  Bsmt.Exposure	+ BsmtFin.Type.1 + BsmtFin.SF.1	+ BsmtFin.Type.2	+
  BsmtFin.SF.2	+ Bsmt.Unf.SF	+  Total.Bsmt.SF	+ Heating	+ Heating.QC +
  Central.Air	+ Electrical + X1st.Flr.SF	+ X2nd.Flr.SF	+ 
  Low.Qual.Fin.SF	+ Bsmt.Full.Bath	+ Bsmt.Half.Bath + 
  Full.Bath +Half.Bath +Bedroom.AbvGr +Kitchen.AbvGr +Kitchen.Qual +
  TotRms.AbvGrd +Functional +Fireplaces +
  Garage.Type +Garage.Cars +
  Garage.Area +Garage.Qual + Garage.Cond +Paved.Drive + 
  Mo.Sold +Sale.Type +Sale.Condition

############################## PCA Regression #############################################

full.model = SalePrice ~ Gr.Liv.Area + MS.Zoning + MS.SubClass +
  Street + Lot.Shape + Land.Contour + Utilities +
  Lot.Config + Land.Slope + Neighborhood + Condition.1 +
  Condition.2  +House.Style +        Overall.Qual+
  Overall.Cond + Year.Built        +Year.Remod.Add  + Roof.Style	+
  Roof.Matl	+Exterior.1st	+ Mas.Vnr.Type +
  Mas.Vnr.Area +	Exter.Qual +Exter.Cond +	Foundation	+  Bsmt.Qual	+ Bsmt.Cond	+ 
  Bsmt.Exposure	+ BsmtFin.Type.1 + BsmtFin.SF.1	+ BsmtFin.Type.2	+
  BsmtFin.SF.2	+ Bsmt.Unf.SF	+  Heating	+ Heating.QC +
  Central.Air	+ Electrical + X1st.Flr.SF	+ X2nd.Flr.SF	+ 
  Bsmt.Full.Bath	+ Bsmt.Half.Bath + 
  Full.Bath +Half.Bath +Bedroom.AbvGr +Kitchen.AbvGr +Kitchen.Qual +
  TotRms.AbvGrd +Functional +Fireplaces +
  Garage.Type +Garage.Cars +
  Garage.Area +Garage.Qual + Garage.Cond +Paved.Drive + 
  Mo.Sold +Sale.Type +Sale.Condition


amesmat = model.frame(fake.model,data=new.ames)

#setting up models and PCA 
y_val = amesmat[,2]
X_val = amesmat[,-(1:2)]
r = princomp(X_val,corr=TRUE)
resid = c()

#Setting up matricies to be filled
pca.ms = matrix(0,10,160)
AIC = matrix(0,10,160)
BIC = matrix(0,10,160)
rsq = matrix(0,10,160)
V = 10
cvsets= cvFolds(dim(X_val)[1],V)
for (i in 71:230){
  k = r$scores[,1:i]
  for(j in 1:V){
    inds=which(cvsets$which==j) #inds = jth test set from CV
    y.train =y_val[-inds]
    x.train = k[-inds,]
    y.test =y_val[inds]
    x.test = k[inds,]
    pca_lm = lm(y.train~x.train)
    predict.pca_lm = coef(pca_lm)%*%t(cbind(1,x.test))
    # mean square error
    pca.ss = (predict.pca_lm - y.test)^2
    pca.ms[j,i-70] = sqrt(mean(pca.ss))
    AIC[j,i-70] = extractAIC(pca_lm)[2]
    BIC[j,i-70] = BIC(pca_lm)
  }}
means.pca = apply(pca.ms,2,mean)


#using optimal parameter 170 - Note that the index starts at i = 1, p = 70, hence i = 101, p = 170
means.pca[101] #CV mean RMSE


#Calculating R-Squared
k = r$scores[,1:170]
rsq = c()
for(j in 1:V){
  inds=which(cvsets$which==j) #inds = jth test set from CV
  y.train =y_val[-inds]
  x.train = k[-inds,]
  y.test =y_val[inds]
  x.test = k[inds,]
  pca_lm = lm(y.train~x.train)
  rsq[j] = cor(pca_lm$fitted,y.train)
}
mean(rsq)
#Setting Up GGPlot Equivilents
x = c()
for (j in 71:230) {
  x = c(x, rep(j,10))  # My matricies are sorted such that the first 10 values come from 
  # the 1st 10 nodes, and so on and so forth. 
}
#Turn the matrixes into vectors. 
y1 = as.vector(pca.ms)
AIC = as.vector(AIC)
BIC = as.vector(BIC)
#Create a Data frame with the values
cv = data.frame(x,y1,AIC,BIC)
#This data frame can be used to plot, however it is extremely confusing due to
#lage upswing in the last error rates increasing so much, therefore, we plotted
#Two seperate but connected graphs to illustrate our point. 
cv2 = cv[401:1510,]
lvls = cv2$structure
m = ggplot(cv2,aes(as.factor(x)))
# Combining dotplots and boxplots.
m +   geom_boxplot(outlier.shape = 5,aes(y=y1),fill="#FF1A1A",size=0.75)+
  geom_point(aes(y=y1,col=y1),size=2,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Regressors",
       y = "RMSE of Cross Validation",
       title = "PCA Regression - Cross Validated Errors")+
  theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 0))+ 
  scale_colour_gradient(low="#33FF33",high="#1C68FF")

m +   geom_boxplot(outlier.shape = 5,aes(y=AIC),fill="#FF1A1A",size=0.75)+
  geom_point(aes(y=AIC,col=AIC),size=2,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Regressors",
       y = "AIC of Cross Validation",
       title = "PCA Regression - AIC")+
  theme(legend.position="none")+ scale_colour_gradient(low="#33FF33",high="#1C68FF")

m +   geom_boxplot(outlier.shape = 5,aes(y=BIC),fill="lightblue",size=0.75)+
  geom_point(aes(y=BIC,col=BIC),size=2,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Regressors",
       y = "BIC of Cross Validation",
       title = "PCA Regression - BIC")+
  theme(legend.position="none")+ scale_colour_gradient(low="#33FF33",high="#1C68FF")

num = 71:230
n = ggplot(data.frame(means.pca,num),aes(x=num))
n + geom_point(aes(y=means.pca,colour=num)) + 
  labs(x="Number of Parameters",
       y = "Mean RMSE",
       title = "Finding the Lowest Mean RMSE")+
  scale_colour_gradient(low="yellow",high="red")

cv3 = cv[1511:1600,]
m = ggplot(cv3,aes(as.factor(x)))
# Combining dotplots and boxplots.
m +   geom_boxplot(outlier.shape = 5,aes(y=y1),fill="#FF1A1A",size=0.75)+
  geom_point(aes(y=y1,col=y1),size=2,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Regressors",
       y = "RMSE of Cross Validation",
       title = "PCA Regression - Cross Validated Errors")+
  theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 0))+ 
  scale_colour_gradient(low="#33FF33",high="#1C68FF")

# The graphs seem to tell us that from the range of 170 parameters to 
# about 205, the mean does not change much, therefore this is range from
# which we should choose our model. The distances in means are rather arbitrary
# within a range of about 300$, somewhat irrelevant to a purchasing decision 
# for a house, therefore we chose a model that had the lowest SD of CV errors in this range.
sd.pca = apply(pca.ms,2,sd)
which.min(sd.pca[100:135]) + 169


