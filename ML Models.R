###load libraries
library(randomForest)
library(e1071)
library(ggthemes)
library(grid)
library(ggplot2)
library(tidyr)
library(viridis)
library(caret)
library(gbm)
library(raster)
library(Metrics)
library(xgboost)

#####load data set from your file path
DATA_BIOMASS<-read.csv('D:/JP_Backup/AGB_BWS_PALSAR/FINAL_DATA_AGB_PAPER/Training_DATA/AGB_P-2_S-1.csv')

##### reproducibility number. It is the extent to which a tool can produce the same result when used repeatedly under the same circumstances. Reproducibility is used interchangeably with the terms repeatability and reliability.
set.seed(1) 


# randomly pick 75% of the number of observations
index <- sample(1:nrow(DATA_BIOMASS),size = 0.75*nrow(DATA_BIOMASS))

# subset weather to include only the elements in the index
train <- DATA_BIOMASS[index,]

# subset weather to include all but the elements in the index
test <- DATA_BIOMASS [-index,]

#print number of data for train and validation
nrow(train)
nrow(test)


##################
# 1 GAM Model implementation
##################
fitControl <- trainControl(method = "cv",
                           number = 10,
                           savePredictions = TRUE)


#GAM model run
mod_gam <- train(AGB~.,data = DATA_BIOMASS,
                 method = "gam",trControl = fitControl,
                 verbose = FALSE,
                 importance=TRUE)

summary(mod_gam)

###################################

#Tested Gam Model implementation#
###########################################
mod_gam_test <- gam(AGB~s(Variable1)+ s(Variable12) + s(Variable13) + s(Variable14) +
                         s(Variable15), data = DATA_BIOMASS, correlation=corExp(form = ~ Latitude + Longitude),
                       method = 'REML')
summary(model.gamm_act$gam)

summary(mod_gam)$r.sq

varImp(mod_gam, scale=FALSE)



# Apply the model to the testing data (i.e., make predictions) ...
test.pred.gam_ <- (predict(mod_gam,test))-1

# ...and evaluate the accuracy
RMSE.mod_gam <- sqrt(mean((test.pred.gam_-test$AGB)^2))

RMSE.mod_gam

MAE.mod_gam <- mean(abs(test.pred.gam_-test$AGB))
MAE.mod_gam

Rel.RMSEgam <- sqrt(mean((test.pred.gam_-test$AGB)^2)) / diff(range(test$AGB))
Rel.RMSEgam


R2gam <- 1 - sum((test$AGB-test.pred.gam_)^2)/sum((test$AGB-mean(test$AGB))^2)

plot(test[,1],test.pred.gam_,pch = 16,cex.axis = 1.15, cex.lab = 1.15, col = "black",main = "Field vs Model Predicted Biomass\n GAM \n", xlim = c(0,500), ylim = c(0,500), xlab = "Field Biomass (t/ha)", ylab = "Predicted Biomass (t/ha)")

abline(1.123e-15, 1)


##################
# 3 RANDOM FOREST
##################
rfGrid<- expand.grid(mtry = c(1:10))
rf_H_with <- randomForest(AGB ~., data = train,importance = TRUE, proximity = T,tuneGrid = rfGrid)
print(rf_H_with)
varImpPlot(rf_H_with)
# How many trees are needed to reach the minimum error estimate? 
# This is a simple problem; it appears that about 100 trees would be enough. 
which.min(rf_H_with$mse)

# Plot rf to see the estimated error as a function of the number of trees
# (not running it here)
# plot(rf) 

# Using the importance()  function to calculate the importance of each variable
imp_H_with <- as.data.frame(sort(importance(rf_H_with)[,1],decreasing = TRUE),optional = T,col.names=T)
names(imp_H_with) <- "% Inc MSE"
imp_H_with



test2 <- rbind(test, train)

# As usual, predict and evaluate on the test set
test.pred.forest <- predict(rf_H_with,test2)

test.pred.forest <- test.pred.forest[1:nrow(test)]

RMSE.forest_H_with <- sqrt(mean((test.pred.forest-test$AGB)^2))
RMSE.forest_H_with


MAE.forest_H_with <- mean(abs(test.pred.forest-test$AGB))
MAE.forest_H_with


# Rel.RMSE <- sqrt(mean((test.pred.forest-test$BIOMASS)^2)) / sd(test$BIOMASS)
Rel.RMSErf <- sqrt(mean((test.pred.forest-test$AGB)^2)) / diff(range(test$AGB))
Rel.RMSErf
### ref Remote Sens. 2016, 8, 583; doi:10.3390/rs8070583 
R2RF <- 1 - sum((test$AGB-test.pred.forest)^2)/sum((test$AGB-mean(test$AGB))^2)


plot(test[,1],test.pred.forest,pch = 16,cex.axis = 1.15, cex.lab = 1.15, col = "black",main = "Field vs Model Predicted Biomass\n GAM \n", xlim = c(0,500), ylim = c(0,500), xlab = "Field Biomass (t/ha)", ylab = "Predicted Biomass (t/ha)")

abline(1.123e-15, 1)

##################
# 4 SVM
##################


## create rmse function
rmse <- function(error)
{
  sqrt(mean(error^2))
}

##load a model SVM
##################
# 4 SVM
##################


## create rmse function
rmse <- function(error)
{
  sqrt(mean(error^2))
}

##load a model SVM
SVM_H <- svm(AGB ~., data = train)

C
svm.imp <- importance(SVM_H,data = train )


predictedY <- predict(SVM_H, test2)
predictedY <- predictedY[1:nrow(test)]
plot(test$AGB, predictedY, col = "red", pch=4)

error <- train$AGB - predictedY
svrPredictionRMSE <- rmse(error)
svrPredictionRMSE

tuneResult <- tune(svm, AGB ~., data = train, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

print(tuneResult)

tuneResult


svm.imp <- importance(tuneResult, data=train)

# Draw the tuning graph
plot(tuneResult)

##tune the SVM model
tunedModel <- tuneResult$best.model
test.pred.svm <- predict(tunedModel, test2) 
test.pred.svm <- test.pred.svm[1:nrow(test)]
error <- test$AGB - test.pred.svm  


##compute the performance metrics
tunedModelRMSE <- rmse(error)
tunedModelRMSE


RMSE.svm_H_with <- sqrt(mean((test.pred.svm-test$AGB)^2))
RMSE.svm_H_with


MAE.svm_H_with <- mean(abs(test.pred.svm-test$AGB))
MAE.svm_H_with

Rel.RMSEsvm <- sqrt(mean((test.pred.svm-test$AGB)^2)) / diff(range(test$AGB))
Rel.RMSEsvm
# SVM
R2svm <- 1 - sum((test$AGB-test.pred.svm)^2)/sum((test$AGB-mean(test$AGB))^2)

plot(test[,1],test.pred.svm,pch = 16,cex.axis = 1.15, cex.lab = 1.15, col = "black",main = "Field vs Model Predicted Biomass\n GAM \n", xlim = c(0,500), ylim = c(0,500), xlab = "Field Biomass (t/ha)", ylab = "Predicted Biomass (t/ha)")

abline(1.123e-15, 1)

#GBM MODEL RUN

gbmGrid <-  expand.grid(interaction.depth = c(1,3,5),
                        n.trees = (1:100)*60,
                        shrinkage = c(0.1,0.05,0.01,0.005,0.001),
                        n.minobsinnode = 5)

gbmFit1 <- train(AGB ~.,data = train,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid)

plot(gbmFit1)
summary(gbmFit1)

# Apply the model to the testing data (i.e., make predictions) ...
test.pred.gbm_ <- (predict(gbmFit1,test))-1

# ...and evaluate the accuracy
RMSE.mod_gbm <- sqrt(mean((test.pred.gbm_-test$AGB)^2))

RMSE.mod_gbm

MAE.mod_gbm <- mean(abs(test.pred.gbm_ -test$AGB))
MAE.mod_gbm

Rel.RMSEgbm <- sqrt(mean((test.pred.gbm_-test$AGB)^2)) / diff(range(test$AGB))
Rel.RMSEgbm


R2gbm <- 1 - sum((test$AGB-test.pred.gbm_)^2)/sum((test$AGB-mean(test$AGB))^2)


plot(test[,1],test.pred.gbm_,pch = 16,cex.axis = 1.15, cex.lab = 1.15, col = "black",main = "Field vs Model Predicted Biomass\n GAM \n", xlim = c(0,500), ylim = c(0,500), xlab = "Field Biomass (t/ha)", ylab = "Predicted Biomass (t/ha)")

abline(1.123e-15, 1)


# Create a data frame with the error metrics for each method
accuracy_H_with <- data.frame(Method = c("GAM model","Random forest","SVM","GBM"),
                              RMSE   = c(RMSE.mod_gam,RMSE.forest_H_with,RMSE.svm_H_with,RMSE.mod_gbm),
                              MAE    = c(MAE.mod_gam,MAE.forest_H_with,MAE.svm_H_with,MAE.mod_gbm),
                              relRMSE    = c(Rel.RMSEgam,Rel.RMSErf,Rel.RMSEsvm,Rel.RMSEgbm),
                              r2    = c(R2gam,R2RF,R2svm,R2gbm)) 
# Round the values and print the table
accuracy_H_with$RMSE <- round(accuracy_H_with$RMSE,2)
accuracy_H_with$MAE <- round(accuracy_H_with$MAE,2) 
accuracy_H_with$r2 <- round(accuracy_H_with$r2,2) 

#print results
accuracy_H_with



# Create a data frame with the predictions for each method
all.predictions <- data.frame(actual = test$AGB,
                              gam.regression = test.pred.gam_,
                              random.forest = test.pred.forest,
                              support.vector.machine = test.pred.svm,
                              GBM.regression=test.pred.gbm_)

index_0df <- which(all.predictions$actual !=0)
all.predictions <- all.predictions[index_0df,]



# Gather the prediction variables (columns) into a single row (i.e., wide to long)
# Recall the ggplot2 prefers the long data format
all.predictions <- gather(all.predictions,key = model,value = predictions,2:5)
all.predictions[,4] <- rep(test[,3],4)

###plot for publication
theme_Publication <- function(base_size=14, base_family="Times New Roman") {

  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}




##### create a dataframe with the summary of the stats for the four models

all.predictions$model[all.predictions$model== "gam.regression"] = "Generalized Additive Model"
all.predictions$model[all.predictions$model== "random.forest"] = "Random Forest"
all.predictions$model[all.predictions$model== "support.vector.machine"] = "Support Vector Machine"
all.predictions$model[all.predictions$model== "GBM.regression"] = "Gradient boosting model"

all.predictions$RMSE <- rep(accuracy_H_with$RMSE, each=70)
all.predictions$MAE <- rep(accuracy_H_with$MAE, each=70)
all.predictions$relRMSE <- rep(round(accuracy_H_with$relRMSE, digit=3), each=70)
all.predictions$r2 <- rep(round(accuracy_H_with$r2, digit=2), each=70)



####first plot(base theme)

Map_1 <- ggplot(data = all.predictions,aes(x = actual, y = predictions)) + ###,color= V4
  geom_point(colour = "black") + #colour = "black"
  geom_abline(intercept = 0, slope = 1, colour = "black") + #geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2)  +xlim (c(0,max(all.predictions$actual))) +ylim (c(0,max(all.predictions$actual))) +
  ggtitle("Biomass, Predicted vs. Observed, by model")+ coord_fixed(ratio = 1)+theme_Publication()+ 
  xlab("Observed Biomass [t/ha]") +
  ylab("Predicted Biomass [t/ha]")+
  geom_text(aes(x=12, y=400, label= paste("RMSE= ", RMSE ), hjust=0))+
  geom_text(aes(x=12, y=350, label= paste("MAE= ", MAE ), hjust=0))+
  geom_text(aes(x=12, y=300, label= paste("relRMSE= ", relRMSE ), hjust=0)) # data= accuracy_H_with,


Map_1


##save as png
namepng <- paste('Plot1.png', sep = '')
png(namepng, width=2800, height=2800, units='px', res=300)
print(Map_1)
dev.off()



######alternative plot with viridis library (i.e. fancy colors)
Map_2 <- ggplot(data = all.predictions,aes(x = actual, y = predictions)) + ###,color= V4
  geom_abline(intercept = 0, slope = 1, colour = "black",linetype="dashed") + #geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  geom_point(aes(size=actual, fill=actual), colour="black",pch=21,show.legend = TRUE) + #colour = "blue"
  facet_wrap(~ model,ncol = 2)  +xlim (c(0,max(all.predictions$actual))) +ylim (c(0,max(all.predictions$actual))) +
  #ggtitle("Biomass, Predicted vs. Observed, by model")+ 
  coord_fixed(ratio = 1)+theme_Publication()+ 
  xlab("Observed AG Biomass (t/ha)]") +
  ylab("Predicted Biomass [t/ha]")+scale_fill_viridis(direction = -1) 
 # geom_text(aes(x=12, y=400, label= paste("RMSE= ", RMSE ), hjust=0))+
  #geom_text(aes(x=12, y=350, label= paste("MAE= ", MAE ), hjust=0))+
  #geom_text(aes(x=12, y=300, label= paste("relRMSE= ", relRMSE ),
 


Map_2
 ##save as png

namepng <- paste('Plot2.png', sep = '')
png(namepng, width=2800, height=2800, units='px', res=300)
print(Map_2)
dev.off()




#another one
ggplot(data = all.predictions,aes(x = actual, y = predictions)) +
  geom_smooth(method=lm, se=FALSE)+
  geom_point(aes(size=actual, fill=actual), colour="black",pch=21,show.legend = FALSE) + #colour = "blue"
  geom_abline(intercept = 0, slope = 1, colour = "black") + #geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  facet_wrap(~ model,ncol = 2)  +xlim (c(0,max(all.predictions$actual))) +ylim (c(0,max(all.predictions$actual))) +
  ggtitle("Biomass, Predicted vs. Observed, by model")+ coord_fixed(ratio = 1)+theme_Publication()+ 
  xlab("Observed Biomass [t/ha]") +
  ylab("Predicted Biomass [t/ha]")+
  geom_text(aes(x=12, y=400, label= paste("RMSE= ", RMSE ), hjust=0))+
  geom_text(aes(x=12, y=350, label= paste("MAE= ", MAE ), hjust=0))+
  geom_text(aes(x=12, y=300, label= paste("relRMSE= ", relRMSE ), hjust=0))+scale_fill_viridis(direction = -1)

###...and another one



#another one
Map_4 <- ggplot(data = all.predictions,aes(x = actual, y = predictions)) +
  geom_smooth(method=lm)+# data= accuracy_H_with, ###,color= V4
   #colour = "blue"
  geom_abline(intercept = 0, slope = 1, colour = "black",linetype="dashed") + #geom_vline(xintercept = 23, colour = "green", linetype = "dashed") +
  geom_point(aes(size=actual, fill=actual), colour="black",pch=21,show.legend = T) +
  facet_wrap(~ model,ncol = 2)  +xlim (c(0,max(all.predictions$actual))) +ylim (c(0,max(all.predictions$actual))) +
  ggtitle("AboveGround Biomass, Predicted vs. Observed, by model")+ coord_fixed(ratio = 1)+theme_Publication()+ 
  ylab(expression("Predicted AboveGround Biomass ["~t~ha^-1~"]"))+
  xlab(expression("Observed AboveGround Biomass ["~t~ha^-1~"]"))+
  # 
  # xlab("Observed AboveGround Biomass [t ha^-1]") +
  # ylab("Predicted AboveGround Biomass [t ha^-1]")+
  geom_text(aes(x=12, y=400, label= paste("RMSE== ", RMSE, "~t~ha^-1"), hjust=0),parse=TRUE)+
  geom_text(aes(x=12, y=350, label= paste("MAE== ", MAE , "~t~ha^-1"), hjust=0),parse=TRUE)+
  geom_text(aes(x=12, y=300, label= paste("R^2== ", r2 ), hjust=0),parse=TRUE)+
  
  # geom_text(aes(x=12, y=170, label= paste("relRMSE= ", relRMSE ), hjust=0))+
  scale_fill_viridis(direction = -1)


Map_4
 namepng <- paste('Plot4.png', sep = '')
png(namepng, width=2800, height=2800, units='px', res=300)
print(Map_4)
dev.off()

########################################################END################################################################
