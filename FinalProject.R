library(tidyverse)
library(fredr)
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(dplyr)
library("xts")
library(rvest)
library(tidytext)
library(urca)
library(forecast)
library(tseries)
library(ggplot2)

##1. DATA WRANGLING/CLEANING

fredr_set_key("a276ace28f00c2ba0f9bfa14ea5f2289")

#1.1 DEPENDENT VARIABLE - OUTPUT - PRODUCTION INDEX

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
              "growth_credit_ind",
              "growth_credit_us",
              "growth_rate_us",
              "growth_uncertainty")

colname2 <- c("log_prod_ind",
              "log_prod_us",
              "log_inflation_ind",
              "log_inflation_us",
              "log_credit_ind",
              "log_credit_us",
              "log_rate_us",
              "log_uncertainty")

colname3 <- c("prod_index_ind",
              "prod_index_us",
              "CPI_ind",
              "CPI_us",
              "credit_ind",
              "credit_us",
              "FFR",
              "uncertainty_index")

dfname <- c("prod_index_ind",
            "prod_index_us",
            "CPI_indonesia",
            "CPI_unitedstates",
            "credit_ind",
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
  
##a. Production Index Indonesia
prod_index_ind <- create_df("IDNPRMNTO01IXOBM", growth_prod_ind, log_prod_ind, prod_index_ina)

prod_index_ind <- fredr(series_id = "IDNPRMNTO01IXOBM",
            observation_start = as.Date("2014-01-01"),
            observation_end   = as.Date("2022-09-01")) %>% 
  mutate(growth_prod_ind = ((value/lag(value,12)-1)*100)) %>%
  arrange(date) %>%
  mutate(log_prod_ind = log(value),
         date = format(date, "%Y-%m")) %>%
  dplyr::rename(prod_index_ind = value) %>%
  dplyr::select(1,3,6,7) %>%
  filter(date > '2014-12')

#Reference: https://fred.stlouisfed.org/series/PRMNTO01IDQ661N

##b. Production Index United States

prod_index_us <- create_df("INDPRO", growth_prod_us, log_prod_us, prod_index_us)
#Reference: https://fred.stlouisfed.org/series/PRMNTO01USQ661N

##c. CPI Indonesia

CPI_indonesia <- create_df("IDNCPIALLMINMEI", inflation_ind, log_inflation_ind, CPI_ind)
#Reference: https://fred.stlouisfed.org/series/IDNCPIALLMINMEI

##d. CPI United States

CPI_unitedstates <- create_df("USACPIALLMINMEI", inflation_us, log_inflation_us, CPI_us)
#Reference: https://fred.stlouisfed.org/series/FPCPITOTLZGUSA

##e. Credit Indonesia
credit_ind <- read_excel("credit_ind.xlsx")
credit_ind$log_credit_ind<-log(credit_ind$credit_ind)
#Reference:https://www.ojk.go.id/id/kanal/perbankan/data-dan-statistik/statistik-perbankan-indonesia/Default.aspx 

##f. Credit US
credit_us <- create_df("LOANINV", growth_credit_us, log_credit_us, credit_us)
#Reference:https://fred.stlouisfed.org/searchresults/?st=LOANINV 

#1.2 INDEPENDENT VARIABLE - POLICY RATE

##a. Policy rate Indonesia

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
policyrate_indonesia$BI_rate <- as.numeric(policyrate_indonesia$BI_rate)
#Reference: https://community.rstudio.com/t/number-to-date-problem-excel-to-r/40075 

##b. Fed Fund Rate

policyrate_unitedstates <- create_df("FEDFUNDS", 
                                     growth_rate_us, 
                                     log_rate_us, 
                                     FFR) %>%
  select(1,2)

policyrate_unitedstates$FFR <- as.numeric(policyrate_unitedstates$FFR)
#Reference: https://fred.stlouisfed.org/series/FEDFUNDS

##c. REER Indonesia and US

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

##d.  Global Uncertainty Index

global_uncertainty_index <- create_df("GEPUPPP", growth_uncertainty, log_uncertainty, uncertainty_index)

##merging data

merge <- function(a,b,c,d){
  test<-a%>%reduce(full_join,by='date')%>%
    select(-c(b,c)) %>%
    mutate(dummy = ifelse(date >= d, 1, 0))
}

df_list_ind<-list(prod_index_ind,
                  credit_ind,
                  CPI_indonesia,
                  global_uncertainty_index,
                  effective_exchange_rate,
                  policyrate_indonesia)

data_ind<-merge(df_list_ind,15,17,"2020-03")

df_list_us<-list(prod_index_us, 
                 CPI_unitedstates,
                 credit_us,
                 global_uncertainty_index,
                 effective_exchange_rate,
                 policyrate_unitedstates)

data_us<-merge(df_list_us,14,16,"2020-01")

write.csv(data_ind)
write.csv(data_us)

##2. CREATING PLOTS FOR DEPENDENT VARIABLES

data_ind$date <- ym(data_ind$date)
data_us$date <- ym(data_us$date)

all_df_plots <- inner_join(data_ind, data_us, by = c('date', 
                                                     'uncertainty_index', 
                                                     'growth_uncertainty', 
                                                     'log_uncertainty', 
                                                     'dummy'))
write.csv("all_df")

vars_plot <- function(x,y){
all_df_plots %>%
  select(date, {{x}}, {{y}}) %>%
  gather(key = "Lines", value = "growth", -date) %>%
  ggplot(aes(x = date, y = growth)) +
           geom_line(aes(color = Lines)) +
           scale_color_manual(values = c("darkblue", "darkred")) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_labels="%b %y",date_breaks  ="3 month") +
    xlab("Date") +
    theme_bw()
}

#plot uncertainty
plot_uncertainty <- ggplot(all_df_plots, aes(x = date, y = growth_uncertainty)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_date(date_labels="%b %y",date_breaks  ="3 month") +
  xlab("Date") +
  ggtitle("Global Uncertainty Index 2015-2022") + 
  ylab("Global Uncertainty")
plot_uncertainty

#plot credit
plot_credit <- vars_plot(growth_credit_ind, growth_credit_us) +
  ggtitle("Comparison of Indonesia and US Credit Growth 2015-2022") + 
  ylab("Growth Credit")
plot_credit

#plot inflation
plot_inflation <- vars_plot(inflation_ind,inflation_us) +
  ggtitle("Comparison of Inflation between Indonesia and US 2015-2022") + 
  ylab("Inflation")
plot_inflation

#plot production (output)
plot_prod <- vars_plot(growth_prod_ind,growth_prod_us) +
  ggtitle("Comparison of Indonesia and US Production Growth 2015-2022") + 
  ylab("Growth Production")
plot_prod

##3. WORKING ON THE REGRESSION MODEL

##a. build time series data
data_ind_ts<-as.ts(data_ind)
data_us_ts<-as.ts(data_us)

##b. before going to the model, we first check the stationary from the pattern of each variable 
ts.plot(data_ind$log_inflation_ind)
ts.plot(data_ind$log_prod_ind)
ts.plot(data_ind$BI_rate)
ts.plot(data_ind$log_er_ind)
ts.plot(data_ind$log_uncertainty)

ts.plot(data_us$log_inflation_us)
ts.plot(data_us$log_prod_us)
ts.plot(data_us$FFR)
ts.plot(data_us$log_er_us)

##c. check stationarity of data log
pp.test(data_ind$CPI_ind) #nonstatitioner
pp.test(data_ind$prod_index_ind) #statitioner
pp.test(data_ind$uncertainty_index) #stationer
pp.test(data_ind$BI_rate)#nonstationer
pp.test(data_ind$er_ind) #nonstationer

pp.test(data_us$CPI_us) #nonstatitioner
pp.test(data_us$prod_index_us) #nonstatitioner
pp.test(data_us$FFR) #nonstationer
pp.test(data_us$er_us) #nonstationer

##d. ARDL model will work on stationary as well as non stationary data

#The ARDL model can be specified for a combination of variables with I(1) and 
#I(0), but not I(2) or higher

##e. check whether data will stationary in first difference
d_CPI<-diff(data_ind$CPI_ind)
pp.test(d_CPI)
d_birate<-diff(data_ind$BI_rate)
str(data_ind)
pp.test(d_birate)
d_er_ind<-diff(data_ind$er_ind)
pp.test(d_er_ind)

d_CPI_US<-diff(data_us$CPI_us)
pp.test(d_CPI_US)
d_prod_index_US<-diff(data_us$prod_index_us)
pp.test(d_prod_index_US)
d_FFR<-diff(data_us$FFR)
pp.test(d_FFR)
d_er_us<-diff(data_us$er_us)
pp.test(d_er_us)

#Based on PP test, all data are stationary in the level and first difference, 
#so we can apply an ARDL model

##f. run model ARDL each country (STEP1)
#run ARDL model Indonesia - Credit Growth
library(dynlm)
library(vars)

#running model (Indonesia)
##model credit

#----------------------
#model1 <- function(var1,var2,var3,var4,data){
#  dynlm(paste(d(var1,1), "~", L(var2,0:1), "+", L(var3,0:1), "+", L(var4,0:1)), data = data)
#}

#model1_inflation <- model1("log_inflation_ind", 
#                           "BI_rate",
#                           "log_uncertainty",
#                           "log_er_ind",
#                           data_ind_ts)
#---------------------
  
model_1_ind_credit<-dynlm(d(log_credit_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1),data=data_ind_ts)
summary(model_1_ind_credit)

model_2_ind_credit<-dynlm(d(log_credit_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1)+L((BI_rate*log_uncertainty),0:1),data=data_ind_ts)
summary(model_2_ind_credit)

model_3_ind_credit<-dynlm(d(log_credit_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1)+L((BI_rate*log_uncertainty),0:1)+L(dummy,0:1)+L((BI_rate*log_uncertainty*dummy),0:1),data=data_ind_ts)
summary(model_3_ind_credit)

##model inflation 
model_1_ind_inflation<-dynlm(d(log_inflation_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1),data=data_ind_ts)
summary(model_1_ind_inflation)

model_2_ind_inflation<-dynlm(d(log_inflation_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1)+L((BI_rate*log_uncertainty),0:1),data=data_ind_ts)
summary(model_2_ind_inflation)

model_3_ind_inflation<-dynlm(d(log_inflation_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1)+L((BI_rate*log_uncertainty),0:1)+L(dummy,0:1)+L((BI_rate*log_uncertainty*dummy),0:1),data=data_ind_ts)
summary(model_3_ind_inflation)

##model output 
model_1_ind_output<-dynlm(d(log_prod_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1),data=data_ind_ts)
summary(model_1_ind_output)

model_2_ind_output<-dynlm(d(log_prod_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1)+L((BI_rate*log_uncertainty),0:1),data=data_ind_ts)
summary(model_2_ind_output)

model_3_ind_output<-dynlm(d(log_prod_ind,1)~L(BI_rate,0:1)+L(log_uncertainty,0:1)+L(log_er_ind,0:1)+L((BI_rate*log_uncertainty),0:1)+L(dummy,0:1)+L((BI_rate*log_uncertainty*dummy),0:1),data=data_ind_ts)
summary(model_3_ind_output)

#running model (United States)
##model credit 
model_1_us_credit<-dynlm(d(log_credit_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1),data=data_us_ts)
summary(model_1_us_credit)

model_2_us_credit<-dynlm(d(log_credit_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1)+L((FFR*log_uncertainty),0:1),data=data_us_ts)
summary(model_2_us_credit)

model_3_us_credit<-dynlm(d(log_credit_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1)+L((FFR*log_uncertainty),0:1)+L(dummy,1:0)+L((FFR*log_uncertainty*dummy),0:1),data=data_us_ts)
summary(model_3_us_credit)

##run ARDL model US - Inflation
model_1_us_inflation<-dynlm(d(log_inflation_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1),data=data_us_ts)
summary(model_1_us_inflation)

model_2_us_inflation<-dynlm(d(log_inflation_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1)+L((FFR*log_uncertainty),0:1),data=data_us_ts)
summary(model_2_us_inflation)

model_3_us_inflation<-dynlm(d(log_inflation_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1)+L((FFR*log_uncertainty),0:1)+L(dummy,0:1)+L((FFR*log_uncertainty*dummy),0:1),data=data_us_ts)
summary(model_3_us_inflation)

##run ARDL model - Output 
model_1_us_output<-dynlm(d(log_prod_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1),data=data_us_ts)
summary(model_1_us_output)

model_2_us_output<-dynlm(d(log_prod_us,1)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1)+L((FFR*log_uncertainty),0:1),data=data_us_ts)
summary(model_2_us_output)

model_3_us_output<-dynlm(d(log_prod_us,)~L(FFR,0:1)+L(log_uncertainty,0:1)+L(log_er_us,0:1)+L((FFR*log_uncertainty),0:1)+L(dummy,0:1)+L((FFR*log_uncertainty*dummy),0:1),data=data_us_ts)
summary(model_3_us_output)


##4.SENTIMENT ANALYSIS

text_tibble <- function(x) {
  tryCatch({
    x %>%
      read_html %>%
      html_nodes('p') %>%
      html_text(trim = T) %>%
      toString()
  }, error = function(e) NA)
}

extract_news <- function(x){
  which_country <- read_xlsx(x) %>% 
  mutate(text = sapply(Link, FUN = text_tibble)) %>% 
  select(text)
}

#4.1 PLOT FOR NEWS IN INDONESIA
IND_news <- extract_news("Indonesia_news.xlsx")

sentiment_nrc <- 
  get_sentiments("nrc") %>%
  rename(nrc = sentiment)

sentiment_afinn <- 
  get_sentiments("afinn") %>%
  rename(afinn = value)

sentiment_bing <- 
  get_sentiments("bing") %>%
  rename(bing = sentiment)

all_articles_Ind <- list()
for (i in 1:lengths(IND_news)){
  list_article <- unnest_tokens(IND_news[i,], 
                                word_tokens, 
                                text, 
                                token = "words") %>%
    anti_join(stop_words, by = c("word_tokens" = "word")) %>%
    group_by(word_tokens) %>%
    count(sort = T) %>%
    left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
    left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
    left_join(sentiment_bing, by = c("word_tokens" = "word"))
  all_articles_Ind[[i]] <- list_article
}

all_df_Ind <- do.call(rbind, all_articles_Ind)

#Reference: 
#https://stackoverflow.com/questions/68064788/scraping-links-in-df-columns-with-rvest
#https://stackoverflow.com/questions/72663669/how-to-scrape-same-type-of-data-from-multiple-link-in-r
#https://uc-r.github.io/scraping_HTML_text

#Creating Plots for the three sentiments of Indonesian News

plot <- function(a,b){
ggplot(data = filter(a, !is.na({{b}}))) +
  geom_histogram(aes({{b}}), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45))
}

NRC_Ind_News <- plot(all_df_Ind, nrc) +
  labs(title = "United States News Sentiment (NRC) to Interest Rate Changes 2015 - 2022") 

BING_Ind_News <- plot(all_df_Ind, bing) +
  labs(title = "United States News Sentiment (BING) to Interest Rate Changes 2015 - 2022") 

Affin_Ind_News <- plot(all_df_Ind, afinn) +
  labs(title = "Indonesia News Sentiment (AFINN) to Interest Rate Changes 2015 - 2022") 

#4.2 PLOT FOR NEWS IN THE US

----------------
##WARNING: Butuh di function kan tapi gw gagal. Mungkin bs diminta bantuan Jeff or Icha. Soalnya ini berulang dr yg di atas

test <- function(a){
all_articles <- list()
for (i in 1:lengths(a)){
  list_article <- unnest_tokens(a[i,], 
                                word_tokens, 
                                text, 
                                token = "words") %>%
    anti_join(stop_words, by = c("word_tokens" = "word")) %>%
    group_by(word_tokens) %>%
    count() %>%
    left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
    left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
    left_join(sentiment_bing, by = c("word_tokens" = "word"))
  all_articles[[i]] <- list_article
}
}

test_again <- test(US_news)
----------------

US_news <- extract_news("US_news.xlsx")

all_articles_US <- list()
for (i in 1:lengths(US_news)){
  list_article <- unnest_tokens(US_news[i,], 
                                word_tokens, 
                                text, 
                                token = "words") %>%
    anti_join(stop_words, by = c("word_tokens" = "word")) %>%
    group_by(word_tokens) %>%
    count() %>%
    left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
    left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
    left_join(sentiment_bing, by = c("word_tokens" = "word"))
  all_articles_US[[i]] <- list_article
}

all_df_US <- do.call(rbind, all_articles_US)

#Creating Plots for the three sentiments of Indonesian News

NRC_US_News <- plot(all_df_US, nrc) +
  labs(title = "United States News Sentiment (NRC) to Interest Rate Changes 2015 - 2022")

BING_US_News <- plot(all_df_US, bing) +
  labs(title = "United States News Sentiment (BING) to Interest Rate Changes 2015 - 2022") 

Affin_US_News <- plot(all_df_US, afinn) +
  labs(title = "United States News Sentiment (AFINN) to Interest Rate Changes 2015 - 2022")

##5. SHINY 

library(tidyverse)
library(sf)
library(spData)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)


all_df_plots$date <- format(as.Date(all_df_plots$date), "%Y-%m")
all_df_plots <- all_df_plots %>% separate(date,
                                          c("year", "month"),
                                          sep = "-")
ui <- fluidPage(
  fluidRow(
    column(width = 12, 
           align = "center", 
           tags$h1("Research Report: Effects of Monetary Policy During Covid-19"),
           tags$hr())
    ),
  fluidRow(
    column(width = 12, 
           align = "center", 
           tags$em(tags$h1("An Overview from Indonesia and the US")))
      ),
  fluidRow(
    column(width = 12,
           align = "center",
           radioButtons(inputId = "dp",
                       label = "Choose Dependend Variable Plot",
                       choices = c("Global Uncertainty Plot", "Inflation Plot", "Production Growth", "Credit"),
                       inline = T),
           plotlyOutput("varplot")
    )),
  fluidRow(
    titlePanel("Explore Our Data Table"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(
          "show_vars",
          "Columns to show:",
          choices = colnames(all_df_plots),
          multiple = TRUE,
          selected = c("year", "month", "prod_index_ind")
        ),
        actionButton("button", "Click to Confirm"),
        uiOutput("date")
      ),
      mainPanel(tableOutput("table"))
    )
  ),
  fluidRow(
    column(width = 12,
           align = "center",
           selectInput(inputId = "sa",
                       label = "Choose Sentiment Type",
                       choices = c("NRC US", "BING US", "Afinn US")),
           plotlyOutput("sent", height = "700px"))
    )
  )

server <- function(input, output, session) {
  
  output$varplot <- renderPlotly({
    if (input$dp == "Global Uncertainty Plot") {plot_uncertainty} 
    else if (input$dp == "Inflation Plot") {plot_inflation}
    else if (input$dp == "Production Growth") {plot_prod}
    else if (input$dp == "Credit") {plot_credit}
    else (return(NULL))
  })

  data.react <- eventReactive(input$button, {
    all_df_plots[, input$show_vars]
  })
  observeEvent(input$button, {
    output$date <- renderUI({
      data.sel <- data.react()
      selectizeInput("wy",
                     "Choose Year",
                     choices = c("All", sort(as.character(
                       unique(data.sel$year)
                     ))),
                     selected = "All")
    })
    
    df_subset <- eventReactive(input$wy, {
      data.sel <- data.react()
      if (input$wy == "All") {
        data.sel
      }
      else{
        data.sel[data.sel$year == input$wy,]
      }
    })
    
    output$table <- renderTable({
      df_subset()
    })
  })

#source: https://stackoverflow.com/questions/47657110/selection-of-columns-for-the-table-in-shiny
  
  output$sent <- renderPlotly({
    if (input$sa == "NRC US") {NRC_US_News} 
    else if (input$sa == "BING US") {BING_US_News}
    else if (input$sa == "Afinn US") {Affin_US_News}
    else (return(NULL))
  })
}
shinyApp(ui = ui, server = server)


