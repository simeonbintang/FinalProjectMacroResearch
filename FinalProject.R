library(tidyverse)
library(fredr)
library(readxl)
library(dplyr)
library(lubridate)

fredr_set_key("a276ace28f00c2ba0f9bfa14ea5f2289")

#download GDP data
gdp_indonesia <- fredr(series_id = "NGDPRSAXDCIDQ",
              observation_start = as.Date("2014-01-01"),
              observation_end   = as.Date("2022-07-01"))
#Reference: https://fred.stlouisfed.org/series/NGDPRSAXDCIDQ

gdp_unitedstates <- fredr(series_id = "GDPC1",
                          observation_start = as.Date("2014-01-01"),
                          observation_end   = as.Date("2022-07-01"))
#Reference: https://fred.stlouisfed.org/series/GDPC1


#Convert GDP to growth
gdp_indonesia = gdp_indonesia %>% group_by(month=month(date)) %>%
  arrange(date) %>%
  mutate(growth=value/lag(value,1))%>%
  ungroup() %>% arrange(date)

gdp_indonesia$growth<-(gdp_indonesia$growth-1)*100


#download CPI data
CPI_indonesia <- fredr(series_id = "IDNCPIALLMINMEI",
                       observation_start = as.Date("2014-01-01"),
                       observation_end   = as.Date("2022-01-01"))
#Reference: https://fred.stlouisfed.org/series/IDNCPIALLMINMEI


CPI_unitedstates <- fredr(series_id = "FPCPITOTLZGUSA",
                       observation_start = as.Date("2014-01-01"),
                       observation_end   = as.Date("2022-01-01"))
#Reference: https://fred.stlouisfed.org/series/FPCPITOTLZGUSA

#Convert CPI to monthly


#Policy rate
policyrate_unitedstates <- fredr(series_id = "FEDFUNDS",
                                observation_start = as.Date("2014-01-01"),
                                observation_end   = as.Date("2022-01-01"))
#Reference: https://fred.stlouisfed.org/series/FEDFUNDS

temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www.bis.org/statistics/cbpol/cbpol_2211.xlsx", temp.file, mode = "wb")
policyrate_indonesia <- read_excel(temp.file, sheet =3, skip = 2)
policyrate_indonesia<-policyrate_indonesia[-1,]
colnames(policyrate_indonesia)[1]<-"Date"
policyrate_indonesia$Date<-as.Date('1899-12-30')+days(policyrate_indonesia$Date)
policyrate_indonesia = subset(policyrate_indonesia, select = c(Date,Indonesia))
policyrate_indonesia = policyrate_indonesia[829:922,]

#Reference: https://community.rstudio.com/t/number-to-date-problem-excel-to-r/40075 


#Real effective exchange rate
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www.bis.org/statistics/eer/broad.xlsx", temp.file, mode = "wb")
effective_exchange_rate <- read_excel(temp.file, skip = 3)
effective_exchange_rate <- effective_exchange_rate[-1,]
colnames(effective_exchange_rate)[1] <- "Date"
effective_exchange_rate <- effective_exchange_rate[, c('Date', 'Indonesia', 'United States')]
effective_exchange_rate$Date <- as.Date(effective_exchange_rate$Date,
                                                format = "%y-%m-%d")


