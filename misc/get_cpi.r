#' Calculate inflation rate and increment rates
#' 
#' Data taken from ONS 
#' Data from 1988 to 2020 are CPI 
#' Data from 1947 to 1988 are RPI-0.7pp
#' RPI https://www.ons.gov.uk/economy/inflationandpriceindices/timeseries/czbh/mm23
inflation_incr <- function()
{
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

	#Get mean and SD of inflation
	# Mean and SD of inflation for whole series are:
	inf_sum<-0
	inf_sum$mean_all<-mean(cpi_imputed$cpi)
	# 0.04527778
	inf_sum$sd_all<-sd(cpi_imputed$cpi)
	# 0.04449283

	# Mean and SD of inflation for post-1997 Bank of England inflation targeting are:
	inf_sum$mean_post96<-mean(cpi_imputed$cpi[cpi_imputed$year>1996])
	# 0.019375
	inf_sum$sd_post96<-sd(cpi_imputed$cpi[cpi_imputed$year>1996])
	# 0.01014595

	#Create random variables for inflation
	cpi_imputed$sim_all<-rnorm(72, mean=inf_sum$mean_all, sd=inf_sum$sd_all)
	cpi_imputed$sim_post96<-rnorm(72, mean=inf_sum$mean_post96, sd=inf_sum$sd_post96)

	#' Create capped inflation changes:
	#' 2.5% cap
	#' Actual data 
	cpi_imputed$cpi_cap2.5<-as.numeric(cpi_imputed$cpi)-0.025
	cpi_imputed$cpi_cap2.5[cpi_imputed$cpi_cap2.5<=0]<-0

	#' Simulated inflation data
	cpi_imputed$sim_all_cap2.5<-as.numeric(cpi_imputed$sim_all)-0.025
	cpi_imputed$sim_all_cap2.5[cpi_imputed$sim_all_cap2.5<=0]<-0

	cpi_imputed$sim_post96_cap2.5<-as.numeric(cpi_imputed$sim_post96)-0.025
	cpi_imputed$sim_post96_cap2.5[cpi_imputed$sim_post96_cap2.5<=0]<-0

	#' 5.0% cap, +50% between 5% and 15%
	#' Actual data
	cpi_imputed$cpi_cap5<-as.numeric(cpi_imputed$cpi)-0.05
	cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5<=0]<-0
	cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>0&cpi_imputed$cpi_cap5<0.1]<-cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>0&cpi_imputed$cpi_cap5<0.1]*0.5
	cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>=0.1]<-cpi_imputed$cpi_cap5[cpi_imputed$cpi_cap5>=0.1]-0.05

	#' Simulated
	cpi_imputed$sim_all_cap5<-as.numeric(cpi_imputed$sim_all)-0.05
	cpi_imputed$sim_all_cap5[cpi_imputed$sim_all_cap5<=0]<-0
	cpi_imputed$sim_all_cap5[cpi_imputed$sim_all_cap5>0&cpi_imputed$sim_all_cap5<0.1]<-cpi_imputed$sim_all_cap5[cpi_imputed$sim_all_cap5>0&cpi_imputed$sim_all_cap5<0.1]*0.5
	cpi_imputed$sim_all_cap5[cpi_imputed$sim_all_cap5>=0.1]<-cpi_imputed$sim_all_cap5[cpi_imputed$sim_all_cap5>=0.1]-0.05

	cpi_imputed$sim_post96_cap5<-as.numeric(cpi_imputed$sim_post96)-0.05
	cpi_imputed$sim_post96_cap5[cpi_imputed$sim_post96_cap5<=0]<-0
	cpi_imputed$sim_post96_cap5[cpi_imputed$sim_post96_cap5>0&cpi_imputed$sim_post96_cap5<0.1]<-cpi_imputed$sim_post96_cap5[cpi_imputed$sim_post96_cap5>0&cpi_imputed$sim_post96_cap5<0.1]*0.5
	cpi_imputed$sim_post96_cap5[cpi_imputed$sim_post96_cap5>=0.1]<-cpi_imputed$sim_post96_cap5[cpi_imputed$sim_post96_cap5>=0.1]-0.05

	#' This gives us the inflation above each of the caps for each year. 
	#' Randomize order
	set.seed(12345)
	cpi_imputed$rand<-runif(72)

	cpi_imputed_new<-cpi_imputed[order(cpi_imputed$rand),] 
	cpi_imputed_new$year<-as.numeric(cpi_imputed$year)+73
	rm(cpi_imputed,cpi,rpi)

	#' This gives us the inflation above each of the caps for each year from 2022 to 2093

	#' Recode inflation so that it's from 2018
	cpi_imputed_new$year=cpi_imputed_new$year-4
	#Limit inflation figures up 2067 to be consistent with the rest of the model.
	cpi_imputed_new<-cpi_imputed_new[1:50,]

	#Next generate the increment for each year. This is one currently, but would be 1-cpi above the cap in a inflation adjusted model:
	cpi_imputed_new$incr_2.5<-1-cpi_imputed_new$cpi_cap2.5
	cpi_imputed_new$incr_5<-1-cpi_imputed_new$cpi_cap5

	cpi_imputed_new$incr_sim_all_2.5<-1-cpi_imputed_new$sim_all_cap2.5
	cpi_imputed_new$incr_sim_all_5<-1-cpi_imputed_new$sim_all_cap5

	cpi_imputed_new$incr_sim_post96_2.5<-1-cpi_imputed_new$sim_post96_cap2.5
	cpi_imputed_new$incr_sim_post96_5<-1-cpi_imputed_new$sim_post96_cap5


	return(subset(cpi_imputed_new,select=c(year, incr_2.5, incr_5, incr_sim_all_2.5, incr_sim_all_5,incr_sim_post96_2.5, incr_sim_post96_5)))
}

a <- inflation_incr()
dir.create("../data")

saveRDS(a, file="../data/inflation.rds")
