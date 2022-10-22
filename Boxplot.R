#Boxplots are a measure of how well data is distributed across a data set. This divides the data set into three quartiles. 
#This graph represents the minimum, maximum, average, first quartile, and the third quartile in the data set. Boxplot is also useful in comparing the distribution of data in a data set by drawing a boxplot for each of them.


mydata<-read.csv('E:/Boxplot_height.csv')

mydata1<-read.csv('E:/mean.csv')

boxplot(mydata,ylab="height(m)",xlab="species",las=1.5,font.lab=3,cex.lab=1,col = c("grey"), ylim = c(0, 25), yaxs = "i",page=2)
points(1:18,mydata1,pch=23,cex=0.75,bg="red")



#install.packages('Boruta')
library(ranger)
library(Boruta)
setwd(choose.dir())

#source location of your data

DATA_BIOMASS<-read.csv('D:/JP_Backup/AGB_BWS_PALSAR/FINAL_DATA_AGB_PAPER/Training_DATA/AGB_P-2_S-1.csv')


# Boruta variable selection
#Boruta is a feature selection algorithm. Precisely, it works as a wrapper algorithm around Random Forest.

Bortua <- Boruta(AGB ~ ., data=DATA_BIOMASS, doTrace=0)
names(Bortua)

Bortua_Sig <- getSelectedAttributes(Bortua, withTentative = TRUE)
print(Bortua_Sig) 

roughFixMod <- TentativeRoughFix(Bortua)
Bortua_Sig <- getSelectedAttributes(roughFixMod)
print(Bortua_Sig)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

# Plot variable importance
plot(Bortua, cex.axis=.7, las=2, xlab="", main="Variable Importance",ylab = "Variable Importance", cex.lab=1.3)  
