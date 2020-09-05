setwd("C:/Users/mypa4/Documents/ITB/MSIT H6029 - MSc Research Project/Eurostat")
library(dplyr)
library(tidyverse)
library(MTS)
library(zoo)
library(forecast)
library(reshape)
library(nnet)
library(NeuralNetTools)
library(rpart)
library(rpart.plot)
library(tseries)
library(randomForest)
library(Metrics)
library(randomForestSRC)
library(cluster)

eurostat<- read.csv("Eurostat/une_rt_m_1_Data.csv")
eurostat$TIME<-as.yearmon(gsub("M","-",eurostat$TIME))
eurostat$Value<-as.double(gsub(":",NA,eurostat$Value))

myYesSUBSET<-eurostat%>%
  filter(TIME>=1993+(10/12) & TIME<2019+(7/12) & str_detect(S_ADJ,"^Seasonally")) %>%
  select(TIME,GEO,Value)

myYesPIVOT<-cast(melt(myYesSUBSET,id.vars=c("TIME","GEO")),TIME~GEO) %>% select_if(~sum(is.na(.)) == 0)
diffss<-myYesPIVOT
initialValues<-myYesPIVOT%>%filter(TIME=="Dec 1993")
diffss[diffss$TIME>="Dec 1993",! names(diffss) %in% "TIME"]<- apply(myYesPIVOT[myYesPIVOT>="Nov 1993",! names(diffss) %in% "TIME"],2,diff)
myYesPIVOT<-myYesPIVOT%>%filter(TIME>"Dec 1993")
egX<-as.matrix(diffss[-nrow(diffss),]%>%filter(TIME>="Dec 1993")%>%select(-"TIME"))
diffss<-diffss%>%filter(TIME>"Dec 1993")

egXTS<-ts(egX,start = 1994,frequency=12)
myTS<-ts(myYesPIVOT[,-1],start = 1994,frequency=12)
myTSdiffss<-ts(diffss[,-1],start = 1994,frequency=12)
class(myTS)
plot(myTS[,1:10])

for (i in 1:ncol(myTS)){
  PlotTSMain(myTS[,i],colnames(myTS)[i], "Time (per month)", "unemployment rate", TRUE,"Images/")
  PlotAutoCorr(myTS[,i],"ACF",colnames(myTS)[i],"Images/00")
  PlotAutoCorr(myTS[,i],"PACF",colnames(myTS)[i],"Images/00")
}
myTSdiffss<-window(myTSdiffss,start = c(2015,1))

ArimaModels<-vector(mode = "list", length = ncol(myTS))
ArimaForecast1<-vector(mode = "list", length = ncol(myTS))
ArimaAccuracy<-vector(mode = "list", length = ncol(myTS))
eArimaModels<-vector(mode = "list", length = ncol(myTS))
eArimaForecast1<-vector(mode = "list", length = ncol(myTS))
eArimaAccuracy<-vector(mode = "list", length = ncol(myTS))
annModels<-vector(mode = "list", length = ncol(myTS))
annForecast1<-vector(mode = "list", length = ncol(myTS))
annAccuracy<-vector(mode = "list", length = ncol(myTS))
rfModels<-vector(mode = "list", length = ncol(myTS))
rfAccuracy<-vector(mode = "list", length = ncol(myTS))

for (i in 1:ncol(myTS)){
  ArimaModels[[i]]<-forecast::auto.arima(myTSdiffss[,i])
  ArimaForecast1[[i]]<-forecast::forecast(ArimaModels[[i]],h=1)
  ArimaAccuracy[[i]]<-forecast::accuracy(ArimaForecast1[[i]])
  annModels[[i]]<-nnetar(myTSdiffss[,i])
  annForecast1[[i]]<-forecast(annModels[[i]],h=1)
  annAccuracy[[i]]<-forecast::accuracy(forecast(myTSdiffss[,i],model=annModels[[i]],h=1))
  tempTS<-data.frame(embed(myTSdiffss[,i],12))
  tempTS$month<-rep(1:12,len=nrow(tempTS))
  rfModels[[i]]<-randomForest(X1~.,tempTS)
  rfAccuracy[[i]]<-rmse(tempTS$X1,rfModels[[i]]$predicted)
}
for (i in 1:ncol(myTS)){
  print(names(myYesPIVOT)[i+1])
  print(paste0(ArimaForecast1[[i]]$method,": ", ArimaAccuracy[[i]][2]))
  print(paste0(": ", rmse(ArimaModels[[i]]$x[-c(1:24)],ArimaModels[[i]]$fitted[-c(1:24)])))
  print(paste0(eArimaForecast1[[i]]$method,": ", eArimaAccuracy[[i]][2]))
  print(paste0(annForecast1[[i]]$method,": ", annAccuracy[[i]][2]))
  print(paste0(": ", rmse(annModels[[i]]$x[-c(1:24)],annModels[[i]]$fitted[-c(1:24)])))
  print(paste0("Ranfom Forest: ", rfAccuracy[[i]]))
  print("-------------------------------------------------------------")
}


nnetar(myTSdiffss)


myDTW<-function(X){
  N<-ncol(X)
  toReturn<-matrix(rep(0,N*N),ncol=N)
  for (i in 1:(N-1)){
    for (j in (i+1):N){
      d<-dtw_basic(X[,i],X[,j])
      toReturn[i,j]<-d
      toReturn[j,i]<-d
    }
  }
  return(toReturn)
}

myTS.dtw<-myDTW(myTS)
myTS.pam<-pam(myTS.dtw,5)$clustering
colnames(myTS)[myTS.pam==1]
colnames(myTS)[myTS.pam==2]
colnames(myTS)[myTS.pam==3]
colnames(myTS)[myTS.pam==4]
colnames(myTS)[myTS.pam==5]
myTS.k<-kmeans(t(myTS),5)$cluster
colnames(myTS)[myTS.k==1]
colnames(myTS)[myTS.k==2]
colnames(myTS)[myTS.k==3]
colnames(myTS)[myTS.k==4]
colnames(myTS)[myTS.k==5]
