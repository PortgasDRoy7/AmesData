#####################################
#                                   #
#          NEURAL NETWORKS          #
#                                   #
#####################################

##### SET-UP ----------------------------------------------------------------------------------------

download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")
set.seed(1)
attach(ames)
library(cvTools)
library(nnet)
library(ggplot2)

############### Neural Network ##############################
Age = ames$Yr.Sold - ames$Year.Built
ames$Mo.Sold = factor(ames$Mo.Sold)
ames$Age = Age
ames$MS.SubClass = factor(MS.SubClass)
nn.ames = ames
nn.ames = nn.ames[which(nn.ames$MS.Zoning == "RL"  #Residential Low Density
                        |nn.ames$MS.Zoning == "RH" #Residential High
                        |nn.ames$MS.Zoning == "RM" #Residential Medium
                        |nn.ames$MS.Zoning == "FV" #Floating Village
                        |nn.ames$MS.Zoning == "RP"),]#Low Density Park. (No values after second filter)

nn.ames = nn.ames[which(nn.ames$MS.SubClass == "20"  #One Story 1946-
                        |nn.ames$MS.SubClass == "30" #One Story -1945
                        |nn.ames$MS.SubClass == "50" #1.5 Stories Finished
                        |nn.ames$MS.SubClass == "60" #2 Story 1946-
                        |nn.ames$MS.SubClass == "70" #2 Story -1945
                        |nn.ames$MS.SubClass == "80" #MultiLevel
                        |nn.ames$MS.SubClass == "90" #Duplex
                        |nn.ames$MS.SubClass == "120" #1 Story PUD
                        |nn.ames$MS.SubClass == "160" #2 Story PUD
                        |nn.ames$MS.SubClass == "190"),] # 2 family conversion.
nn.ames = subset(nn.ames, nn.ames$Gr.Liv.Area <= 4475)
set.seed(1)
train.ratio = 0.8
s = sample(1:dim(nn.ames)[1],round(train.ratio*dim(nn.ames)[1],0))
train = nn.ames[s,];
test = nn.ames[-s,];
fitnn = nnet(work.model,train[,-c(1,2)],size = 1,skip=TRUE,linout=TRUE)
P = predict(fitnn, test[, c(3:81,83)])
l = 1:dim(test)[1]
pred = data.frame(l,predicted = P, actual = test[, 82])
pred = na.omit(pred)
#Visualising the Neural Network fit, 
dev.off()
m = ggplot(data=pred,aes(l),ylab)
m + geom_line(aes(y=pred[,2],colour="Predicted")) + geom_point(aes(y=pred[,3],colour="Actual"))


resid.nn = (pred[,2]-pred[, 3])^2
nn.se =mean(resid.nn)
nn.mse =sqrt(nn.se)
nn.mse

#Creating CV sets

V=10

nn.ames$Mo.Sold = as.factor(nn.ames$Mo.Sold)
work.model = SalePrice ~ Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add + 
  Exter.Qual + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + X2nd.Flr.SF + 
  Gr.Liv.Area + TotRms.AbvGrd + Central.Air + Lot.Area + 
  Full.Bath + Age + MS.Zoning + MS.SubClass +Bedroom.AbvGr +Fireplaces+
  Neighborhood +Garage.Cars + Functional+Roof.Style  +
  Roof.Matl  +Exterior.1st  +Exterior.2nd	+ Mas.Vnr.Type+
  Exter.Qual +Exter.Cond +Age+Full.Bath +Half.Bath+ Heating  + Heating.QC +
  Central.Air	+ Electrical+Mo.Sold
#Running 10 fold CV
V = 10
k.values= seq(1,100,4)
cvsets= cvFolds(dim(nn.ames)[1],V)
p_nn = matrix(0,10,5)
nn.mse = matrix(0,10,5)
AIC = matrix(0,10,5)
BIC = matrix(0,10,5)
nn.cv.avg.error = c()
nn.cv.avg.AIC = c()
nn.cv.avg.BIC = c()

#Without weight decay
for(i in 1:5){
for(j in 1:V){
    inds=which(cvsets$which==j)
    test.set=nn.ames[inds,]
    train.set=nn.ames[-inds,]
    fitnn = nnet(work.model,data = train.set,size = i,skip=TRUE,linout=TRUE)#Fitting network on training
    P = predict(fitnn, test.set[, c(3:81,83)],na.action=na.pass) # Making predictions on test set
    l = 1:dim(test.set)[1]
    p_nn[j,i] = sum(abs(fitnn$wts)>1)
    pred = data.frame(l,predicted = P, actual = test.set[, 82])#Turning the act/obs into frame
    pred = na.omit(pred)
    resid.nn = (pred[,2]-pred[, 3])^2
    nn.se =mean(resid.nn)
    nn.mse[j,i] =sqrt(nn.se)  
    AIC[j,i] = dim(train.set)[1]*log(mean(fitnn$resid^2))+2*(p_nn[j,i]+1)
    BIC[j,i] = dim(train.set)[1]*log(mean(fitnn$resid^2))+log(length(test.set[,82]))*(p_nn[j,i]+1)
}
nn.cv.avg.error[i] = mean(nn.mse[,i])
nn.cv.avg.AIC[i] = mean(AIC[,i])
nn.cv.avg.BIC[i] = mean(BIC[,i])
}
x = c()
for (j in 1:5) {
  x = c(x, rep(j,10))
}
y = as.vector(nn.mse)
nn_AIC = as.vector(AIC)
nn_BIC = as.vector(BIC)
cv = data.frame(x,y,nn_AIC,nn_BIC)
m = ggplot(cv,aes(as.factor(x)))
m +   geom_boxplot(outlier.shape = 5,aes(y=y),fill="lightgreen",size=0.75)+
  geom_point(aes(y=y,col=y),size=4,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Nodes in Hidden Layer",
       y = "RMSE of Cross Validation",
       title = "No Decay - RMSE")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")

m +   geom_boxplot(outlier.shape = 5,aes(y=nn_AIC),fill="lightgreen",size=0.75)+
  geom_point(aes(y=nn_AIC,col=nn_AIC),size=4,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Nodes in Hidden Layer",
       y = "AIC of Cross Validation",
       title = "No Decay - AIC")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")

m +   geom_boxplot(outlier.shape = 5,aes(y=nn_BIC),fill="lightgreen",size=0.75)+
  geom_point(aes(y=nn_BIC,col=nn_BIC),size=4,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Nodes in Hidden Layer",
       y = "BIC of Cross Validation",
       title = "No Decay - BIC")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")
#With 0.2 weight decay
p_nnd=c()
nn.mse = matrix(0,10,5)
AIC = matrix(0,10,5)
BIC = matrix(0,10,5)
p_nnd = matrix(0,10,5)
nnd.cv.avg.error = c()
nnd.cv.avg.AIC = c()
nnd.cv.avg.BIC = c()
for(i in 1:5){
  for(j in 1:V){
    inds=which(cvsets$which==j)
    test.set=nn.ames[inds,]
    train.set=nn.ames[-inds,]
    fitnn = nnet(work.model,data = train.set,decay = 0.2,size = i,skip=TRUE,linout=TRUE)
    P = predict(fitnn, test.set[, c(3:81,83)],na.action=na.pass)#Making predictions
    l = 1:dim(test.set)[1]
    p_nnd[j,i] = sum(abs(fitnn$wts)>1)
    pred = data.frame(l,predicted = P, actual = test.set[, 82]) #Turning the act/obs into frame
    pred = na.omit(pred)
    resid.nn = (pred[,2]-pred[, 3])^2 #Finding the Squared Errors
    nn.se =mean(resid.nn)
    nn.mse[j,i] =sqrt(nn.se)
    N = length(train.set[,82])
    AIC[j,i] = dim(train.set)[1]*log(mean(fitnn$resid^2))+2*(p_nnd[j,i]+1)
    BIC[j,i] = dim(train.set)[1]*log(mean(fitnn$resid^2))+log(length(test.set[,82]))*(p_nnd[j,i]+1)
  }
  nnd.cv.avg.error[i] = mean(nn.mse[,i])
  nnd.cv.avg.AIC[i] = mean(AIC[,i])
  nnd.cv.avg.BIC[i] = mean(BIC[,i])
}
#Create a vector which tells you what to have on the x axis
x = c()
for (j in 1:5) {
  x = c(x, rep(j,10))  # My matricies are sorted such that the first 10 values come from 
  # the 1st 10 nodes, and so on and so forth. 
}
#Turn the matrixes into vectors. 
y1 = as.vector(nn.mse)
nnd_AIC = as.vector(AIC)
nnd_BIC = as.vector(BIC)
#Create a Data frame with the values
cv = data.frame(x,y1,nnd_AIC,nnd_BIC)
#Set up GGPLOT
m = ggplot(cv,aes(as.factor(x)))
# Combining dotplots and boxplots.
m +   geom_boxplot(outlier.shape = 5,aes(y=y1),fill="lightblue",size=0.75)+
  geom_point(aes(y=y1,col=y1),size=4,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Nodes in Hidden Layer",
       y = "RMSE of Cross Validation",
       title = "Decay of 0.2 - RMSE")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")

m +   geom_boxplot(outlier.shape = 5,aes(y=nnd_AIC),fill="lightblue",size=0.75)+
  geom_point(aes(y=nnd_AIC,col=nnd_AIC),size=4,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Nodes in Hidden Layer",
       y = "BIC of Cross Validation",
       title = "Decay of 0.2 - AIC")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")

m +   geom_boxplot(outlier.shape = 5,aes(y=nnd_BIC),fill="lightblue",size=0.75)+
  geom_point(aes(y=nnd_BIC,col=nnd_BIC),size=4,position = 
               position_jitter(w = 0.1)) + 
  labs(x="Number of Nodes in Hidden Layer",
       y = "BIC of Cross Validation",
       title = "Decay of 0.2 - BIC")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")

comparison = data.frame(nn.cv.avg.error,nnd.cv.avg.error,nn.cv.avg.AIC,
                        nnd.cv.avg.AIC,nn.cv.avg.BIC,nnd.cv.avg.BIC)
comparison
