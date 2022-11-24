library(tidyverse)
library(fredr)
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(dplyr)
library(magrittr)

fredr_set_key("a276ace28f00c2ba0f9bfa14ea5f2289")

#Dependent Variable - Output - Production Index
##Production Index Indonesia

listID <- c("IDNPRMNTO01IXOBM", 
            "INDPRO", 
            "IDNCPIALLMINMEI", 
            "USACPIALLMINMEI", 
            "LOANINV",
            "FEDFUNDS",
            "GEPUPPP")

colname1 <- c("growth_prod_ind", 
              "growth_prod_us",
              "inflation_ind",
              "inflation_us",
              "growth_credit_us",
              "growth_rate_us",
              "growth_uncertainty")
  
colname2 <- c("log_prod_ind",
              "log_prod_us",
              "log_inflation_ind",
              "log_inflation_us",
              "log_credit_us",
              "log_rate_us",
              "log_uncertainty")

colname3 <- c("prod_index_ind",
              "prod_index_us",
              "CPI_ind",
              "CPI_us",
              "credit_us",
              "FFR",
              "uncertainty_index")

dfname <- c("prod_index_ind",
            "prod_index_us",
            "CPI_Indonesia",
            "CPI_unitedstates",
            "credit_us",
            "policyrate_unitedstates",
            "global_uncertainty_index")

create_df <- function(a,b,c,d){
  df <- fredr(series_id = a,
              observation_start = as.Date("2014-01-01"),
              observation_end   = as.Date("2022-09-01")) %>% 
    mutate({{b}} := ((value/lag(value,12)-1)*100)) %>%
    arrange(date) %>%
    mutate({{c}} := log(value),
           date = format(date, "%Y-%m")) %>%
    rename({{d}} := value) %>%
    select(1,3,6,7) %>%
    filter(date > '2014-12')
}

------
#UNDERCONSTRUCTION
  
all_df <- list()
lapply(dfname, FUN = create_df(listID[i], colname1[i], colname2[i], colname3[i]))
for (i in 1:7){
  test <- create_df(listID[i], colname1[i], colname2[i], colname3[i])
  all_df[[i]] <- test[i]
}

------
prod_index_ind <- create_df("IDNPRMNTO01IXOBM", growth_prod_ind, log_prod_ind, prod_index_ind)
#Reference: https://fred.stlouisfed.org/series/PRMNTO01IDQ661N

  
##Production Index United States

prod_index_us <- create_df("INDPRO", growth_prod_us, log_prod_us, prod_index_us)
#Reference: https://fred.stlouisfed.org/series/PRMNTO01USQ661N


# Dependent Variable-CPI data
##CPI Indonesia

CPI_Indonesia <- create_df("IDNCPIALLMINMEI", inflation_ind, log_inflation_ind, CPI_ind)
#Reference: https://fred.stlouisfed.org/series/IDNCPIALLMINMEI


##CPI United States
CPI_unitedstates <- create_df("USACPIALLMINMEI", inflation_us, log_inflation_us, CPI_us)
#Reference: https://fred.stlouisfed.org/series/FPCPITOTLZGUSA


#Dependent Variable - Credit Growth


##Bank Credit Indonesia


#Bank Credit US
credit_us <- create_df("LOANINV", growth_credit_us, log_credit_us, credit_us)
#Reference?????

#Independent Variable-Policy rate
##Policy rate Indonesia
extract_data <- function (a, b, c, d, e){
  temp.file <- paste(tempfile(),".xlsx",sep = "")
  download.file(a, temp.file, mode = "wb")
  df <- read_excel(temp.file, sheet = b, skip = c)
  df <- df[-1,]
  colnames(df)[1] <- "date"
  df <- df %>% select(c('date', 'Indonesia', 'United States')) %>%
    rename({{d}} := Indonesia,
           {{e}} := 'United States')
}
  
policyrate_indonesia <- extract_data("https://www.bis.org/statistics/cbpol/cbpol_2211.xlsx", 
                                     "Monthly Series", 
                                     2, 
                                     BI_rate, 
                                     US_rate) %>%
  select(1,2) 
policyrate_indonesia$date <- as.Date('1899-12-30') + days(policyrate_indonesia$date)
policyrate_indonesia$date <- format(as.Date(policyrate_indonesia$date), "%Y-%m")
policyrate_indonesia <- policyrate_indonesia %>% filter(date > '2014-12')

#Reference: https://community.rstudio.com/t/number-to-date-problem-excel-to-r/40075 

##Fed Fund Rate
policyrate_unitedstates <- create_df("FEDFUNDS", 
                                     growth_rate_us, 
                                     log_rate_us, 
                                     FFR) %>%
  select(1,2)
#Reference: https://fred.stlouisfed.org/series/FEDFUNDS


#Independent Variable-Real effective exchange rate
##REER Indonesia and US
effective_exchange_rate <- extract_data("https://www.bis.org/statistics/eer/broad.xlsx",
                      "Nominal",
                      3,
                      er_ind,
                      er_us) %>%
  mutate(date = format(date, "%Y-%m"),
         er_ind = as.numeric(as.character(er_ind)),
         er_us = as.numeric(as.character(er_us)),
         log_er_ind = log(er_ind),
         log_er_us = log(er_us)) %>%
  filter(date > '2014-12')

#Independent Variable - Global Uncertainty Index

global_uncertainty_index <- create_df("GEPUPPP", growth_uncertainty, log_uncertainty, uncertainty_index)
#Reference???

#merge data
library(plyr)

-----------
#Bisa berfungsi kalau Credit Indo sudah dapat
merge <- function(a,b,c,d,e,f,g,h){
  join_all(list(a,b,c,d,e), by='date', type='left') %>%
    select(-c(f,g)) %>%
    mutate(dummy = ifelse(date >= 'h', 1, 0))
}

data_ind <- merge(prod_index_ind, 
                  CPI_Indonesia,
                  global_uncertainty_index,
                  effective_exchange_rate,
                  policyrate_indonesia, 
                  12,
                  14,
                  2020-03)

data_us <- merge(prod_index_us, 
                 CPI_unitedstates,
                 credit_us,
                 global_uncertainty_index,
                 effective_exchange_rate,
                 policyrate_unitedstates, 
                 14,
                 16,
                 2020-01)
------------------
data_ind <- join_all(list(prod_index_ind, CPI_Indonesia,global_uncertainty_index,effective_exchange_rate,policyrate_indonesia), by='date', type='left') %>%
  select(-c(12,14)) %>%
  mutate(dummy = ifelse(date >= '2020-03', 1, 0))

data_us<-join_all(list(prod_index_us, CPI_unitedstates, credit_us, global_uncertainty_index,effective_exchange_rate,policyrate_unitedstates), by='date', type='left') %>%
  select(-c(14,16)) %>%
  mutate(dummy = ifelse(date >= '2020-01', 1, 0))

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
------
model1 <- function(var1,var2,var3,var4,data){
  dynlm(paste((var1), "~", L(var2,1), "+", L(var3,1), "+", L(var4,1)), data = data)
}

model1_inflation <- model1("log_inflation_ind", 
                           "BI_rate",
                           "log_uncertainty",
                           "log_er_ind",
                           data_ind_ts)
-------
  
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

##run ARDL model Indonesia - Output Growth
model_1_us_output<-dynlm(d(log_prod_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1),data=data_us_ts)
summary(model_1_us_output)
model_2_us_output<-dynlm(d(log_prod_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1),data=data_us_ts)
summary(model_2_us_output)
model_3_us_output<-dynlm(d(log_prod_us,1)~L(FFR,1)+L(log_uncertainty,1)+L(log_er_us,1)+L((FFR*log_uncertainty),1)+L((FFR*log_uncertainty*dummy),1),data=data_us_ts)
summary(model_3_us_output)

#PLOTTING
#BELOM KELAR
CPI_Ind_forplot <-CPI_Indonesia %>% 
  select(1,2) %>%
  separate("date", c("Year", "Month"), sep = "-") %>%
  group_by(Year, Month) %>%
  summarise(av_CPI = mean(CPI_ind))

ggplot(CPI_Ind_forplot,
       aes(x= date, y = CPI_ind)) 

