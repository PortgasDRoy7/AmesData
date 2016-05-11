# STEP WISE REGRESSION
download.file("http://www.openintro.org/stat/data/ames.RData", destfile = "ames.RData")
load("ames.RData")

attach(ames)
#Create Factors
Age = Yr.Sold - Year.Built
ames$Age = Age
ames$MS.SubClass = factor(MS.SubClass)
ames$Mo.Sold = factor(Mo.Sold)

new.ames = ames

######################### DATA CLEANING ##################################
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
full.model = SalePrice ~ Gr.Liv.Area + MS.Zoning + MS.SubClass +
        Street + Lot.Shape + Land.Contour + Utilities +
        Lot.Config + Land.Slope + Neighborhood + Condition.1 +
        Condition.2  +House.Style +        Overall.Qual+
        Overall.Cond + Year.Built	+Year.Remod.Add	+ Roof.Style	+
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

library(leaps)

############### Forward stepwise selection METHOD ######################
############### we entered different nvmax into this to create out different models ####### 
subr = regsubsets(full.model, data = train, method = "forward", nvmax = 10) 

subr.out = summary(subr);# this object will contain the subr output
subr.out
subr.out$cp;                # Mallow's C_p 
min(subr.out$cp);           # minimum value 

subr.sel= which(subr.out$cp==min(subr.out$cp)); # locates minimum

### Minimising by BIC
min(subr.out$bic)
subr.sel= which(subr.out$bic==min(subr.out$bic));

# this is the index of the selected model depending on min BIC or mallows CP
subr.sel   
subr$xnames[subr.out$which[subr.sel,]]

subr.out$outmat; # matrix indicating which variables are included in each mod 
subr.out$outmat[subr.sel,]; # row for selected model regressors to add in
cbind(subr.out$cp, subr.out$adjr2, 
      subr.out$bic, subr.out$rsq); # compare different criteria: Cp, AdjR2, BIC, R2

# each row is a model. The shaded rectangle indicate that
# the variable is included in model. The value of BIC is 
# reported 
#plot(subr, scale="Cp");
#### other useful functions for retrieving info about the model fit
coef(subr,subr.sel);  # OLS coefficients # this doesn't make sense
# it may be useful to list the variables selected
subr$xnames[subr.out$which[subr.sel,]]

################################## MODELS #######################################
#### CHOOSE 1 MODEL TO RUN ######

######### these have the same 'model_sel' variable so it will override each time, so choose ONE, model 
######### each is based on complexity (number of regressors), which is then added to the CV RMSE.)
######### NOTE: according to textbook CV is meant to incorporate the subset selection but we were unsure
######### of how to implement it. thus it is not the 'most correct way' of doing a CV as listed by the
######### statistical elements of learning textbook.

######## MODEL 1 Simple Linear Regression 
#model_sel = SalePrice ~Gr.Liv.Area

######### MODEL 2 and 3 log(SalePrice) foward selection Model selected by Mallows CP 10 variables
#model_sel = Log(SalePrice) ~ Gr.Liv.Area + Overall.Qual + Year.Remod.Add + I(factor(Bsmt.Exposure)=="Gd") + 
model_sel = SalePrice ~ Gr.Liv.Area + Overall.Qual + Year.Remod.Add + I(factor(Bsmt.Exposure)=="Gd") + 
       BsmtFin.SF.1 + I(factor(Bsmt.Qual)=="Ex") + Total.Bsmt.SF + Kitchen.AbvGr + Garage.Area
# there is bound to be quite a lot of collinearity in here, since all of them are AREAs... 

######### MODEL 4 foward selection Model selected by Mallows CP 20 variables CP = 686.655
#model_sel = SalePrice ~ Gr.Liv.Area + I(factor(MS.SubClass)=="60") + Overall.Qual + 
#        I(factor(Condition.1)=="Feedr") + I(factor(Condition.1)=="RRNe") +
#        I(factor(Roof.Matl)=="Membran") + I(factor(Roof.Matl)=="Metal") + I(factor(Roof.Matl)=="WdShake") +
#        I(factor(Bsmt.Cond)=="Ex") + I(factor(Foundation)=="Wood") +  I(factor(BsmtFin.Type.1)=="BLQ") + 
#        I(factor(Heating.QC)=="Fa") + I(factor(Kitchen.Qual)=="Fa") +  I(factor(Kitchen.Qual)=="Gd") + 
#        I(factor(Garage.Cond)=="Fa") + I(factor(Mo.Sold)=="9") + I(factor(Sale.Type)=="CWD") +
#        BsmtFin.SF.1 + I(factor(Roof.Style)=="Gable") + I(factor(Heating)=="Grav")
        
######## MODEL 5 minimsed by bic with nvmax = 80 variables
#model_sel = SalePrice ~ Gr.Liv.Area + I(factor(MS.SubClass)=="60") + I(factor(Land.Contour)=="HLS") +
#        I(factor(Land.Contour)=="Lvl") + I(factor(Neighborhood)=="NoRidge") + I(factor(Neighborhood)=="NPkVill") +
#        I(factor(Neighborhood)=="OldTown") + I(factor(Neighborhood)=="StoneBr") + I(factor(Neighborhood)=="SWISU") + 
#        I(factor(Neighborhood)=="Timber") + I(factor(Condition.1)=="Feedr") + I(factor(Condition.1)=="Norm") +
#        I(factor(Condition.1)=="RRAn") + I(factor(Condition.1)=="RRNe") + 
#        I(factor(Condition.2)=="PosN") + I(factor(Condition.2)=="RRAn") +
#        I(factor(Roof.Matl)=="Metal") + I(factor(Roof.Matl)=="Tar&Grv") + I(factor(Roof.Matl)=="WdShake") +
#        I(factor(Exterior.1st)=="BrkComm") + I(factor(Exterior.1st)=="WdShing") + I(factor(Exterior.2nd)=="BrkFace") + 
#        I(factor(Exter.Qual)=="Fa") + I(factor(Bsmt.Exposure)=="Av") + I(factor(Bsmt.Cond)=="Ex") +
#        I(factor(Bsmt.Cond)=="Po") + I(factor(Bsmt.Exposure)=="Gd") + I(factor(Bsmt.Exposure)=="Mn") +
#        I(factor(BsmtFin.Type.2)=="GLQ") + I(factor(Heating.QC)=="TA") + I(factor(Central.Air)=="Y") +
#        factor(Full.Bath) + I(factor(Kitchen.Qual)=="TA") + I(factor(Functional)=="Maj2") +
#       I(factor(Paved.Drive)=="Y") + I(factor(Mo.Sold)=="2") + I(factor(Mo.Sold)=="3") +
#        I(factor(Mo.Sold)=="4") + I(factor(Mo.Sold)=="6") + I(factor(Sale.Type)=="ConLD") +
#        I(factor(Sale.Type)=="ConLI") + I(factor(Sale.Condition)=="Alloca") + I(factor(Garage.Cond)=="TA")



#### PERFORMING THE REGRESSION.
regr_sel = lm(model_sel, data = train, na.action=na.exclude); 
options(scipen=999)
summary(regr_sel)
# plot(regr_sel)

ypred_sel = predict(regr_sel, newdata = test)
pred_errors_sel = test$SalePrice - ypred_sel;   # selected model predictions
mse_sel = mean(pred_errors_sel^2, na.rm=TRUE)
rmse_sel = sqrt(mse_sel)
rmse_sel

########## CROSS VALIDATION OF ERRORS ##################
#### LOOKING AT VARIATION OF ERRORS#####################
library(cvTools)

V = 10
cvsets= cvFolds(dim(new.ames)[1],V)

#regr_sel = lm(model_sel, data = train.set, na.action=na.exclude);
#summary(regr_sel)
#plot(regr_sel$fitted.values)

rmse_sel = c()
for(j in 1:V){ 
        inds=which(cvsets$which==j)
        test.set=new.ames[inds,]
        train.set=new.ames[-inds,]
        # CHOOSE 1 MODEL
        #MODEL 1
        #regr_sel = lm(SalePrice~Gr.Liv.Area, data = train.set, na.action=na.exclude); #SLR
        
        #MODELS 2, 4, 5
        regr_sel = lm(model_sel, data = train.set, na.action=na.exclude); #MLR  
        
        #MODEL 3
        regr_sel = lm(model_sel, data = train.set, na.action=na.exclude);
        
        
        ypred_sel = predict(regr_sel, newdata = test.set)
        pred_errors_sel = test.set$SalePrice - ypred_sel;   # selected model predictions
        #pred_errors_sel = test.set$SalePrice - exp(ypred_sel); # for model 3 (log transform)
        mse_sel = mean(pred_errors_sel^2, na.rm=TRUE)
        rmse_sel[j] = sqrt(mse_sel)
}
rmse_sel
mean(rmse_sel)

# for each, run the corresponding model, then the CV, and then that will input the CV rmse into 
# each of these each should have 10 numbers. 
#model1rmse = rmse_sel #mean = (1 regressor - SLR)

model2rmse = rmse_sel #mean = 27636.74 (10 regressors)

#model3rmse = rmse_sel #mean = 33866.82 (20 regressors)

#model4rmse = rmse_sel #mean = 40447.86 (40 regressors)

#### OTHER STUFF e.g. plotting #####

##one of train, and one of test. 
#library(ggplot2)
#fullm = lm(model_sel, data = new.ames, na.action=na.exclude); 
#act = new.ames[-fullm$na.action,82]
#dat = data.frame(fullm$resid,fullm$fitted,act)
#m = ggplot(dat,aes(dat[,1]))
#m + geom_histogram(col='blue')
#m = ggplot(dat)
#m + geom_point(aes(x = fullm$fitted,y=fullm$resid,col=new.ames$Overall.Qual[-fullm$na.action]))+
#        scale_colour_gradientn(colours = rainbow(3,start=0,end=4/6,alpha=1)) +theme_bw()
