############################################
#                                          #
#             CROSS-VALIDATED              #
#    CLASSIFICATION AND REGRESSION TREE    #
#                                          #
############################################

##### SET-UP ----------------------------------------------------------------------------------------

download.file("http://www.openintro.org/stat/data/ames.RData", destfile="ames.RData")
load("ames.RData")
set.seed(1)
attach(ames)

# add the packages you need here
need = c("randomForest","rpart","cvTools","lattice","robustbase","ggplot2","rpart.plot");
#install.packages(need);
for(i in 1:length(need)) {
  library(need[i], character.only=TRUE);
}

##### CLEAN DATA + TRAIN/TEST SET -------------------------------------------------------------------
Age = ames$Yr.Sold - ames$Year.Built;
ames$Mo.Sold = factor(ames$Mo.Sold);
ames$Age = Age;
ames$MS.SubClass = factor(MS.SubClass);
cart.ames = ames;
cart.ames = cart.ames[which(cart.ames$MS.Zoning == "RL" #Residential Low Density
                            |cart.ames$MS.Zoning == "RH" #Residential High
                            |cart.ames$MS.Zoning == "RM" #Residential Medium
                            |cart.ames$MS.Zoning == "FV" #Floating Village
                            |cart.ames$MS.Zoning == "RP"),]; #Low Density Park. (No values after second filter)

cart.ames = cart.ames[which(cart.ames$MS.SubClass == "20"  #One Story 1946-
                            |cart.ames$MS.SubClass == "30" #One Story -1945
                            |cart.ames$MS.SubClass == "50" #1.5 Stories Finished
                            |cart.ames$MS.SubClass == "60" #2 Story 1946-
                            |cart.ames$MS.SubClass == "70" #2 Story -1945
                            |cart.ames$MS.SubClass == "80" #MultiLevel
                            |cart.ames$MS.SubClass == "90" #Duplex
                            |cart.ames$MS.SubClass == "120" #1 Story PUD
                            |cart.ames$MS.SubClass == "160" #2 Story PUD
                            |cart.ames$MS.SubClass == "190"),]; # 2 family conversion.
cart.ames = subset(cart.ames, cart.ames$Gr.Liv.Area <= 4475);
set.seed(1);
train.ratio = 0.8;
s = sample(1:dim(cart.ames)[1],round(train.ratio*dim(cart.ames)[1],0));
train = cart.ames[s,];
test = cart.ames[-s,];

cart.ames$Mo.Sold = as.factor(cart.ames$Mo.Sold)
work.model = SalePrice ~ Overall.Qual + Overall.Cond + Year.Built + Year.Remod.Add +
  Exter.Qual + BsmtFin.SF.1 + Total.Bsmt.SF + X1st.Flr.SF + X2nd.Flr.SF + Gr.Liv.Area +
  TotRms.AbvGrd + Central.Air + Lot.Area + Full.Bath + Age + MS.Zoning + MS.SubClass +
  Bedroom.AbvGr + Fireplaces + Neighborhood + Garage.Cars + Functional + Roof.Style +
  Roof.Matl + Exterior.1st + Exterior.2nd + Mas.Vnr.Type + Exter.Qual + Exter.Cond +
  Age + Full.Bath + Half.Bath + Heating + Heating.QC + Central.Air + Electrical + Mo.Sold;

rpart.ms = matrix(0,10,10);
res.rpart = c();
res.rpart = rpart(work.model, data=train, cp=0.005);
class.predict.rpart = predict(res.rpart, test);
plotcp(res.rpart)
# testing error
rpart.ss = (class.predict.rpart - test[,82])^2;
rpart.ms = sqrt(mean(rpart.ss));
rpart.ms;
plot(res.rpart)
text(res.rpart[[j]], use.n=T, all=T, cex=0.5, col = 'red')


# This sets up 10 different training and test sets as per ten-fold cross validation method. 
V = 10
cvsets= cvFolds(dim(cart.ames)[1],V)
rpart.ms=matrix(0,10,50)
# i = parameters goes from cp 0.05... 0.00000...5
for (i in 1:50){
  # j = cross validation sets
  for(j in 1:V){
    inds=which(cvsets$which==j) #inds = jth test set from CV
    test.set=cart.ames[inds,]
    train.set=cart.ames[-inds,]
    # running classification tree
    res.rpart  = rpart(work.model,data=train.set,cp=0.5/(10*i) )
    class.predict.rpart = predict(res.rpart, test.set)
    # mean square error
    rpart.ss = (class.predict.rpart-test.set$SalePrice)^2
    rpart.ms[j,i] = mean(sqrt(rpart.ss))}
}
rpart.ms # columns param
mean_CV_ms = apply(rpart.ms,2,mean)
x = c()
for (j in 1:50) {
  x = c(x, rep(j,10))  # My matricies are sorted such that the first 10 values come from 
  # the 1st 10 nodes, and so on and so forth. 
}
#Turn the matrixes into vectors. 
y1 = as.vector(rpart.ms)
#Create a Data frame with the values
cv = data.frame(x,y1)
#Set up GGPLOT
m = ggplot(cv,aes(as.factor(x)))
# Combining dotplots and boxplots.
m +   geom_boxplot(outlier.shape = 5,aes(y=y1),fill="lightblue",size=0.75)+
  geom_point(aes(y=y1,col=y1),size=3,position = 
               position_jitter(w = 0.1)) + 
  labs(x="10 to the power of -X",
       y = "RMSE(s) of Cross Validation",
       title = "Classification and Regression Tree - RMSE")+
  theme(legend.position="none")+ scale_colour_gradient(low="yellow",high="red")

num = 15:50
n = ggplot(data.frame(mean_CV_ms[15:50],num),aes(x=num))
n + geom_point(aes(y=mean_CV_ms[15:50],colour=num)) + 
  labs(x="10 to the power of -X",
       y = "Mean RMSE",
       title = "Finding optimal Complexity")

#Optimum Model
opt.reg.tree = rpart(work.model,data=train,cp=0.5/(10*39) )
cart_mod = summary(opt.reg.tree)
class.predict.rpart = predict(opt.reg.tree, test)
# mean square error
opt.ss = (class.predict.rpart-test$SalePrice)^2
opt.ms = mean(sqrt(rpart.ss))
plot(opt.reg.tree)
text(opt.reg.tree, use.n=T, all=T, cex=0.3, col = 'red')
options(scipen = 999)

rpart.plot(res.rpart[[j]],type=1,extra=1,branch = 1,digits=1,col="blue",
           main="Fitted tree - Missclassification Loss",cex = 0.5)

