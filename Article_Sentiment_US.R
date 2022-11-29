library(tidyverse)
library(readxl)
#install.packages("rvest")
library(rvest)
library(tidytext)


setwd("~/GitHub/FinalProjectMacroResearch")

US_news <- read_xlsx("US_news.xlsx")

US_news$text <- sapply(US_news$Link, function(x) {
      tryCatch({
        x %>%
          read_html %>%
          html_nodes('p') %>%
          html_text(trim = T) %>%
          toString()
      }, error = function(e) NA)
    })

#Reference: 
#https://stackoverflow.com/questions/68064788/scraping-links-in-df-columns-with-rvest
#https://stackoverflow.com/questions/72663669/how-to-scrape-same-type-of-data-from-multiple-link-in-r
#https://uc-r.github.io/scraping_HTML_text

US_news_text <- tibble(US_news$text)
names(US_news_text)[1] <- "text"

#Combine all rows
US_news_merged <- US_news_text %>%
  dplyr::summarise(text = paste(text, collapse = " "))
#Reference:
#https://stackoverflow.com/questions/49225596/merge-multiple-rows-into-one-using-r

word_tokens <- unnest_tokens(US_news_merged, word_tokens, text, token = "words")

no_sw_US_news <- anti_join(word_tokens, stop_words, by = c("word_tokens" = "word"))

count(no_sw_US_news, word_tokens, sort = TRUE)

sentiment_nrc <- 
  get_sentiments("nrc") %>%
  rename(nrc = sentiment)

sentiment_afinn <- 
  get_sentiments("afinn") %>%
  rename(afinn = value)

sentiment_bing <- 
  get_sentiments("bing") %>%
  rename(bing = sentiment)

no_sw_US_news <- no_sw_US_news %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_afinn, by = c("word_tokens" = "word")) %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"))

ggplot(data = filter(no_sw_US_news, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "United States News Sentiment (NRC) to Interest Rate Changes 2015 - 2022")

ggplot(data = filter(no_sw_US_news, !is.na(bing))) +
  geom_histogram(aes(bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(title = "United States News Sentiment (BING) to Interest Rate Changes 2015 - 2022")

ggplot(data = filter(no_sw_US_news, !is.na(afinn))) +
  geom_histogram(aes(afinn), stat = "count") +
  scale_x_continuous(n.breaks = 7) +
  labs(title = "United States News Sentiment (AFINN) to Interest Rate Changes 2015 - 2022")

#Check number of strings
str_count(US_news_text$text," ")+1
str_count(US_news_merged$text," ")+1
