library(tidyverse)
library(fredr)
library(readxl)
#fredr_set_key("a276ace28f00c2ba0f9bfa14ea5f2289")

gdp_indonesia <- fredr(series_id = "NGDPRSAXDCIDQ",
              observation_start = as.Date("2014-01-01"),
              observation_end   = as.Date("2022-07-01"))
#Reference: https://fred.stlouisfed.org/series/NGDPRSAXDCIDQ

gdp_unitedstates <- fredr(series_id = "GDPC1",
                          observation_start = as.Date("2014-01-01"),
                          observation_end   = as.Date("2022-07-01"))
#Reference: https://fred.stlouisfed.org/series/GDPC1

#Convert Indonesia local currency to Billions of USD
gdp_indonesia$value_2 <- gdp_indonesia$value * 1000000 / 15735.60 / 1000000000

CPI_indonesia <- fredr(series_id = "FPCPITOTLZGIDN",
                       observation_start = as.Date("2014-01-01"),
                       observation_end   = as.Date("2022-01-01"))
#Reference: https://fred.stlouisfed.org/series/FPCPITOTLZGIDN

CPI_unitedstates <- fredr(series_id = "FPCPITOTLZGUSA",
                       observation_start = as.Date("2014-01-01"),
                       observation_end   = as.Date("2022-01-01"))
#Reference: https://fred.stlouisfed.org/series/FPCPITOTLZGUSA

interestrate_unitedstates <- fredr(series_id = "FEDFUNDS",
                                observation_start = as.Date("2014-01-01"),
                                observation_end   = as.Date("2022-01-01"))
#Reference: https://fred.stlouisfed.org/series/FEDFUNDS

#Real effective exchange rate
temp.file <- paste(tempfile(),".xlsx",sep = "")
download.file("https://www.bis.org/statistics/eer/broad.xlsx", temp.file, mode = "wb")
effective_exchange_rate <- read_excel(temp.file, skip = 3)
effective_exchange_rate <- effective_exchange_rate[-1,]
colnames(effective_exchange_rate)[1] <- "Date"
effective_exchange_rate <- effective_exchange_rate[, c('Date', 'Indonesia', 'United States')]
effective_exchange_rate$Date <- as.Date(effective_exchange_rate$Date,
                                                format = "%y-%m-%d")


