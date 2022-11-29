library(tidyverse)
library(fredr)
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(dplyr)
<<<<<<< Updated upstream
library(magrittr)
=======
>>>>>>> Stashed changes

fredr_set_key("a276ace28f00c2ba0f9bfa14ea5f2289")

#Dependent Variable - Output - Production Index
##Production Index Indonesia
create_df <- function(a,b,c,d){
  df <- fredr(series_id = a,
              observation_start = as.Date("2014-01-01"),
              observation_end   = as.Date("2022-09-01")) %>% 
    mutate({{b}} := ((value/lag(value,12))-1)*100)%>%
    arrange(date) %>%
    mutate({{c}} := log(value),
           date = format(date, "%Y-%m")) %>%
    rename({{d}} := value) %>%
    select(1,3,6,7) %>%
    filter(date > '2014-12')
}

prod_index_ind <- create_df("IDNPRMNTO01IXOBM", growth_prod_ind, log_prod_ind, prod_index_ind)
#Reference: https://fred.stlouisfed.org/series/PRMNTO01IDQ661N

<<<<<<< Updated upstream
  
##Production Index United States
=======
prod_index_ind <- prod_index_ind %>% 
  group_by(month=month(date)) %>%
  arrange(date) %>%
  mutate(growth_prod_ind=((value/lag(value,1))-1)*100)%>%
  ungroup() %>% arrange(date)

prod_index_ind$log_prod_ind<-log(prod_index_ind$value)
colnames(prod_index_ind)[3] <- "prod_index_ind"
prod_index_ind$date <- format(as.Date(prod_index_ind$date), "%Y-%m")
prod_index_ind <- prod_index_ind[, c('date', 'prod_index_ind', 'growth_prod_ind', 'log_prod_ind')]
prod_index_ind = prod_index_ind[13:99,]

##Production Index United States
prod_index_us <- fredr(series_id = "INDPRO",
                       observation_start = as.Date("2014-01-01"),
                       observation_end   = as.Date("2022-09-01"))
#Reference: https://fred.stlouisfed.org/series/PRMNTO01USQ661N

prod_index_us = prod_index_us %>% group_by(month=month(date)) %>%
  arrange(date) %>%
  mutate(growth_prod_us=((value/lag(value,1))-1)*100)%>%
  ungroup() %>% arrange(date)
>>>>>>> Stashed changes

prod_index_us <- create_df("INDPRO", growth_prod_us, log_prod_us, prod_index_us)
#Reference: https://fred.stlouisfed.org/series/PRMNTO01USQ661N


# Dependent Variable-CPI data
##CPI Indonesia

<<<<<<< Updated upstream
CPI_Indonesia <- create_df("IDNCPIALLMINMEI", inflation_ind, log_inflation_ind, CPI_ind)
#Reference: https://fred.stlouisfed.org/series/IDNCPIALLMINMEI
=======
CPI_indonesia = CPI_indonesia %>% group_by(month=month(date)) %>%
  arrange(date) %>%
  mutate(inflation_ind=((value/lag(value,1))-1)*100)%>%
  ungroup() %>% arrange(date)

CPI_indonesia$log_inflation_ind<-log(CPI_indonesia$value)
colnames(CPI_indonesia)[3] <- "CPI_ind"
CPI_indonesia$date <- format(as.Date(CPI_indonesia$date), "%Y-%m")
CPI_indonesia = subset(CPI_indonesia, select = c(date, CPI_ind ,inflation_ind,log_inflation_ind))
CPI_indonesia = CPI_indonesia[13:105,]
>>>>>>> Stashed changes


##CPI United States
CPI_unitedstates <- create_df("USACPIALLMINMEI", inflation_us, log_inflation_us, CPI_us)
#Reference: https://fred.stlouisfed.org/series/FPCPITOTLZGUSA


#Dependent Variable - Credit Growth
##Credit Indonesia


<<<<<<< Updated upstream
##Bank Credit Indonesia


#Bank Credit US
credit_us <- create_df("LOANINV", growth_credit_us, log_credit_us, credit_us)
#Reference?????
=======
#Credit US
credit_us<-fredr(series_id = "LOANINV",
                 observation_start = as.Date("2014-01-01"),
                 observation_end   = as.Date("2022-09-01"))

credit_us = credit_us %>% group_by(month=month(date)) %>%
  arrange(date) %>%
  mutate(growth_credit_us=((value/lag(value,1))-1)*100)%>%
  ungroup() %>% arrange(date)

credit_us$log_credit_us<-log(credit_us$value)
colnames(credit_us)[3] <- "credit_us"
credit_us$date <- format(as.Date(credit_us$date), "%Y-%m")
credit_us = subset(credit_us, select = c(date, credit_us ,growth_credit_us,log_credit_us))
credit_us = credit_us[13:105,]

>>>>>>> Stashed changes

#Independent Variable-Policy rate
##Policy rate Indonesia
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www.bis.org/statistics/cbpol/cbpol_2211.xlsx", temp.file, mode = "wb")
policyrate_indonesia <- read_excel(temp.file, sheet =3, skip = 2)
policyrate_indonesia<-policyrate_indonesia[-1,]
colnames(policyrate_indonesia)[1]<-"date"
policyrate_indonesia$date<-as.Date('1899-12-30')+days(policyrate_indonesia$date)
policyrate_indonesia = subset(policyrate_indonesia, select = c(date,Indonesia))
policyrate_indonesia = policyrate_indonesia[829:921,]
colnames(policyrate_indonesia)[2]<-"BI_rate"
policyrate_indonesia$date <- format(as.Date(policyrate_indonesia$date), "%Y-%m")

#Reference: https://community.rstudio.com/t/number-to-date-problem-excel-to-r/40075 

##Fed Fund Rate
policyrate_unitedstates <- create_df("FEDFUNDS", 
                                     growth_rate_us, 
                                     og_rate_us, 
                                     FFR) %>%
  select(1,2)
#Reference: https://fred.stlouisfed.org/series/FEDFUNDS


#Independent Variable-Real effective exchange rate
##REER Indonesia and US
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www.bis.org/statistics/eer/broad.xlsx", temp.file, mode = "wb")
effective_exchange_rate <- read_excel(temp.file, skip = 3)
effective_exchange_rate <- effective_exchange_rate[-1,]
colnames(effective_exchange_rate)[1] <- "date"
effective_exchange_rate <- effective_exchange_rate[, c('date', 'Indonesia', 'United States')]
effective_exchange_rate = effective_exchange_rate[253:345,]
effective_exchange_rate$date <- format(as.Date(effective_exchange_rate$date), "%Y-%m")
colnames(effective_exchange_rate)[2] <- "er_ind"
colnames(effective_exchange_rate)[3] <- "er_us"
effective_exchange_rate$er_ind <- as.numeric(as.character(effective_exchange_rate$er_ind))
effective_exchange_rate$er_us <- as.numeric(as.character(effective_exchange_rate$er_us))
effective_exchange_rate$log_er_ind<-log(effective_exchange_rate$er_ind)
effective_exchange_rate$log_er_us<-log(effective_exchange_rate$er_us)

#Independent Variable - Global Uncertainty Index

global_uncertainty_index <- create_df("GEPUPPP", growth_uncertainty, log_uncertainty, uncertainty_index)
#Reference???

#merge data
library(plyr)
data_ind<-join_all(list(prod_index_ind, CPI_indonesia,global_uncertainty_index,effective_exchange_rate,policyrate_indonesia), by='date', type='left')
data_ind<-data_ind[,-c(12,14)]
data_ind$dummy<-ifelse(data_ind$date>="2020-03", 1, 0)

data_us<-join_all(list(prod_index_us, CPI_unitedstates, credit_us, global_uncertainty_index,effective_exchange_rate,policyrate_unitedstates), by='date', type='left')
data_us<-data_us[,-c(14,16)]
data_us$dummy<-ifelse(data_us$date>="2020-01", 1, 0)

write.csv(data_ind,"C:/HARRIS/FALL 2022/R/Final Project/FinalProjectMacroResearch/data_ind.csv")
write.csv(data_us,"C:/HARRIS/FALL 2022/R/Final Project/FinalProjectMacroResearch/data_us.csv")

#build time series data
library("xts")
data_ind_ts<-as.ts(data_ind)
data_us_ts<-as.ts(data_us)

#run model ARDL each country (STEP1)
##run ARDL model Indonesia - Credit Growth
library(dynlm)
##run ARDL model Indonesia - Inflation
model_1_ind_inflation<-dynlm(d(log_inflation_ind,1)~L(BI_rate,1)+L(log_uncertainty,1)+L(log_er_ind,1),data=data_ind_ts)
summary(model_1_ind_inflation)
model_2_ind_inflation<-dynlm(d(log_inflation_ind,1)~L(BI_rate,1)+L(log_uncertainty,1)+L(log_er_ind,1)+L((BI_rate*log_uncertainty),1),data=data_ind_ts)
summary(model_2_ind_inflation)
model_3_ind_inflation<-dynlm(d(log_inflation_ind,1)~L(BI_rate,1)+L(log_uncertainty,1)+L(log_er_ind,1)+L((BI_rate*log_uncertainty),1)+L((BI_rate*log_uncertainty*dummy),1),data=data_ind_ts)
summary(model_3_ind_inflation)

##run ARDL model Indonesia - Output Growth
model_1_ind_output<-dynlm(d(log_prod_ind,1)~L(BI_rate,1)+L(log_uncertainty,1)+L(log_er_ind,1),data=data_ind_ts)
summary(model_1_ind_output)
model_2_ind_output<-dynlm(d(log_prod_ind,1)~L(BI_rate,1)+L(log_uncertainty,1)+L(log_er_ind,1)+L((BI_rate*log_uncertainty),1),data=data_ind_ts)
summary(model_2_ind_output)
model_3_ind_output<-dynlm(d(log_prod_ind,1)~L(BI_rate,1)+L(log_uncertainty,1)+L(log_er_ind,1)+L((BI_rate*log_uncertainty),1)+L((BI_rate*log_uncertainty*dummy),1),data=data_ind_ts)
summary(model_3_ind_output)


##run ARDL model US - Credit Growth
model_1_us_credit<-dynlm(d(log_credit_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1),data=data_us_ts)
summary(model_1_us_credit)
model_2_us_credit<-dynlm(d(log_credit_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1),data=data_us_ts)
summary(model_2_us_credit)
model_3_us_credit<-dynlm(d(log_credit_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1)+L((FFR*log_uncertainty*dummy),1),data=data_us_ts)
summary(model_3_us_credit)

##run ARDL model US - Inflation
model_1_us_inflation<-dynlm(d(log_inflation_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1),data=data_us_ts)
summary(model_1_us_inflation)
model_2_us_credit<-dynlm(d(log_inflation_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1),data=data_us_ts)
summary(model_2_us_inflation)
model_3_us_inflation<-dynlm(d(log_inflation_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1)+L((FFR*log_uncertainty*dummy),1),data=data_us_ts)
summary(model_3_us_inflation)

##run ARDL model - Output Growth
model_1_us_output<-dynlm(d(log_prod_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1),data=data_us_ts)
summary(model_1_us_output)
model_2_us_output<-dynlm(d(log_prod_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1),data=data_us_ts)
summary(model_2_us_output)
model_3_us_output<-dynlm(d(log_prod_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1)+L((FFR*log_uncertainty*dummy),1),data=data_us_ts)
summary(model_3_us_output)


