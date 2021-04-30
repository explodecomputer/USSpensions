#' Calculate inflation rate and increment rates
#' Data taken from ONS 
#' Data from 1988 to 2020 are CPI 
#' Data from 1947 to 1988 are RPI-0.7pp
#' RPI https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czbh/mm23
rpi<-read.csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/czbh/mm23')
rpi<-rpi[8:79,]
colnames(rpi)<-c("year","rpi")
#' Adjustment to allow for RPI being higher than CPI
rpi$rpi<-(as.numeric(rpi$rpi)-0.7)*0.01
#' CPI https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/l55o/mm23
cpi<-read.csv('https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/d7g7/mm23')
cpi<-cpi[8:39,]
colnames(cpi)<-c("year","cpi")
cpi$cpi<-((as.numeric(cpi$cpi)))*0.01

#' Combine to create imputed CPI for each year. 
colnames(rpi)<-c("year","cpi")
cpi_imputed=rbind(rpi[1:40,],cpi)

#' Create capped inflation changes:
#' 2.5% cap
cpi_imputed$cpi_cap2.5<-as.numeric(cpi_imputed$cpi)-0.025
cpi_imputed$cpi_cap2.5[cpi_imputed$cpi_cap2.5<=0]<-0

#' 5.0% cap, +50% between 5% and 15%
cpi_imputed$cpi_cap5<-as.numeric(cpi_imputed$cpi)-0.05
cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5<=0]<-0
cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>0&cpi_imputed$cpi_cap5<0.1]<-cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>0&cpi_imputed$cpi_cap5<0.1]*0.5
cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>=0.1]<-0.1

#This gives us the inflation above each of the caps for each year. 
#Randomize order
set.seed(12345)
cpi_imputed$rand<-runif(72)

cpi_imputed_new<-cpi_imputed[order(cpi_imputed$rand),] 
cpi_imputed_new$year<-as.numeric(cpi_imputed$year)+73
rm(cpi_imputed,cpi,rpi)

#This gives us the inflation above each of the caps for each year from 2022 to 2093

