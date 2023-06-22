library(jsonlite)
library(tidyverse)
library(tidytext)

# stream in 1 million randomly sampled reviews and all business data
review_data_1M <- stream_in(file("/Users/lamhine/Downloads/archive/sample.json"))
businesses <- stream_in(file("/Users/lamhine/Downloads/archive/yelp_academic_dataset_business.json"))

# filter for only businesses containing "Chinese" in categories
chinese <- businesses %>% filter(str_detect(categories, "Chinese"))

# inner_join with review data 
chinese_revs <- inner_join(chinese, review_data_1M, by = "business_id")

# keep only vars of interest
chinese_revs <- chinese_revs %>% 
  select(business_id:review_count, 
         categories,
         review_id,
         stars.y,
         text,
         date)

# unnest tokens and remove stop words
data("stop_words")

tidy_words <- chinese_revs %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# get table of most commonly used words
top_words <- tidy_words %>%
  count(word, sort = TRUE) 

# get sentiments from nrc lexicon
nrc_sent <- get_sentiments("nrc") 

# join with top_words
top_sent <- left_join(top_words, nrc_sent, by = "word")

# plot
top_sent %>% 
  group_by(sentiment) %>% 
  summarize(n = sum(n)) %>% 
  mutate(sentiment = reorder(sentiment, n)) %>% 
  ggplot(aes(n, sentiment, fill = sentiment)) + geom_col(show.legend = F)




# get sentiment score for words using "syuzhet"
sent_score <-get_sentiment(tidy_words$word)
tidy_words$score<-sent_score

# convert date to different date format 
tidy_words$date <- as.Date(tidy_words$date)

# calculate total sentiment score (adding scores of words) for each review received by one restaurant on the day
tidy_words_totalscore<- tidy_words %>%
  group_by(date,business_id,review_id)%>%
  mutate(total_score=sum(score)) %>%
  select(business_id,date,total_score)%>%
  unique()%>%
  mutate(year=substr(date, 1, 4))

# calulate mean score by year
tidy_words_totalscore <- tidy_words_totalscore%>%
  group_by(year)%>%
  mutate(mean_score_year=mean(total_score))

# plot score and year
ggplot(tidy_words_totalscore, aes(x = year, y = mean_score_year)) +
  geom_point() +
  labs(x = "Year", y = "Score") +
  ggtitle("Score by Year")







