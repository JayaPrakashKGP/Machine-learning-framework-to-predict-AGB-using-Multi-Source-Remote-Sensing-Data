#Boxplots are a measure of how well data is distributed across a data set. This divides the data set into three quartiles. 
#This graph represents the minimum, maximum, average, first quartile, and the third quartile in the data set. Boxplot is also useful in comparing the distribution of data in a data set by drawing a boxplot for each of them.


mydata<-read.csv('E:/Boxplot_height.csv')

mydata1<-read.csv('E:/mean.csv')

boxplot(mydata,ylab="height(m)",xlab="species",las=1.5,font.lab=3,cex.lab=1,col = c("grey"), ylim = c(0, 25), yaxs = "i",page=2)
points(1:18,mydata1,pch=23,cex=0.75,bg="red")
