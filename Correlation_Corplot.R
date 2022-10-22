
#Correlation plot for feature selection

library(raster)
library(Metrics)
library(corrplot)
library(Hmisc)
library(lattice)


#load variables data from the directory preferably .csv format

DATA_BIOMASS<-read.csv('D:/JP_Backup/AGB_BWS_PALSAR/FINAL_DATA_AGB_PAPER/Training_DATA/AGB_P-2_S-1.csv')

head(DATA_BIOMASS)
cor(DATA_BIOMASS)
M=cor(DATA_BIOMASS)

corrplot(M, method="circle")

corrplot(M, method="number", sig.level = 0.01, number.cex = 0.6, diag=FALSE, insig='blank', col = COL2('BrBG', 5),tl.col = 'black')

corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.5)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(100),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         # hide correlation coefficient on the principal diagonal
         diag=TRUE 
)
library(RColorBrewer)
corrplot(M, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="YlGnBu"))
library(RColorBrewer)
corrplot(M, type="upper", order="hclust", 
         col=brewer.pal(n=8, name="RdBu"))
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=45)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )
         
         