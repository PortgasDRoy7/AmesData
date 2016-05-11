#######################################
#                                     #
#    CROSS-VALIDATED RANDOM FOREST    #
#                                     #
#######################################

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

# CV set-up
V = 10;
cvsets = cvFolds(dim(cart.ames)[1], V);
inds = which(cvsets$which == 1);
test.set = cart.ames[inds,];
train.set = cart.ames[-inds,];

##### RANDOM FOREST (no CV) -------------------------------------------------------------------------

numTrees = c(300,450,600,750,900);
numNodes = c(300,450,600,750,900);

# rows = numTrees, columns = numNodes
treeRMSE = matrix(0,length(numTrees),length(numNodes));

# WARNING: takes long time to run
for (i in 1:length(numTrees)) {
  for (j in 1:length(numNodes)) {
    trainForest = randomForest(work.model, data=train,
                               maxnodes=numNodes[j], ntree=numTrees[i], na.action=na.omit);
    testForest = predict(trainForest, newdata=test)
    rmse = sqrt(mean((test$SalePrice - testForest)^2))
    treeRMSE[i,j] = rmse;
  }
}

# rmse of every tree and node variation
treeRMSE;
matplot(treeRMSE, type="l", xlab="Max Nodes / Max Trees", ylab="RMSE");


##### CV RANDOM FOREST ------------------------------------------------------------------------------
# WARNING: takes long time to run

numTrees = c(300,450,600,750,900);
numNodes = c(300,450,600,750,900);

# rows = numTrees, columns = numNodes
cvForestRMSE = matrix(0,length(numTrees),length(numNodes));

for(i in length(numTrees)) {
  for(j in length(numNodes)) {
    cvRMSE = 0;
    for(k in 1:V) {
      
      inds = which(cvsets$which == k) #inds = k-th test set from CV
      test.set = cart.ames[inds,]
      train.set = cart.ames[-inds,]

      trainForest = randomForest(work.model, train.set, maxnodes=numNodes[j], ntree=numTrees[i], na.action=na.omit);
      testForest = predict(trainForest, newdata=test.set);
      SS = (testForest - test.set$SalePrice)^2;
      rmse = sqrt(mean(as.numeric(SS), na.rm=TRUE));
      
      cvRMSE = cvRMSE + rmse;
    }
    cvRMSE = cvRMSE/V;
    cvForestRMSE[i,j]=cvRMSE
  }
}
# this outputs a matrix of CV-ed RMSEs, with different maxTrees and maxNodes
cvForestRMSE;


##### OPTIMAL RANDOM FOREST -------------------------------------------------------------------------

rforest = randomForest(work.model, data=train.set, maxnodes=750, ntree=750, na.action=na.omit);
rrforest = predict(rforest, data=test.set);
sqrt(mean(((testForest - test.set$SalePrice)^2), na.rm=TRUE));

importance(rforest);

