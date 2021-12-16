library(readr)
library(rtweet)

sanchez <- c

a <- search_tweets(q = "sanchez", n = 2000, lang = "es", include_rts = FALSE)
b <- search_tweets(q = "asturies", n = 2000, lang = "es", include_rts = FALSE)
c <- search_tweets(q = "sanchez", n = 15000, lang = "es", include_rts = FALSE)
tweets <- search_tweets(q = "sanchez", n = 15000, lang = "es", include_rts = FALSE)
tweets <- rbind(a,b,c, tweets)

#head(tweets$text)

# remove http elements manually
tweets$stripped_text <- gsub("http.*","",  tweets$text)
tweets$stripped_text <- gsub("https.*","", tweets$stripped_text)
tweets$stripped_text <- gsub("sanchez","", tweets$stripped_text)
tweets$stripped_text <- gsub("sánchez","", tweets$stripped_text)
tweets$stripped_text <- gsub("pedro","", tweets$stripped_text)
tweets$stripped_text <- gsub("gobierno","", tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
tweets_clean <- tweets %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

# Remove stop words in spanish --------------------------------------------

stop_words_spanish <- read_csv("stop_words_spanish.txt", col_names = FALSE)
stop_words_spanish$word <- stop_words_spanish$X1
stop_words_spanish$X1 <- NULL

cc <- tweets_clean %>% 
  anti_join(stop_words_spanish) ##remove stop words in Spanish

cc <- cc[ !(cc$word %in% c("sanchez","sánchez","pedro","gobierno","presidente", "juan", 
                           "701.000","15","d","1","2" ,"olga")), ]
cc$word<- gsub("pablocasado_","casado", cc$word)
cc$word<- gsub("pablo","casado", cc$word)
cc$word<- gsub("santi_abascal","abascal", cc$word)
cc$word<- gsub("santi","abascal", cc$word)
cc$word<- gsub("santiago","abascal", cc$word)
cc$word<- gsub("j_sanchez_serna","sanchez serna", cc$word)

# cc <- tweets_clean %>% 
#   anti_join(stop_words) ##remove stop words in English

#head(cc)

# plot the top 15 words -- notice any issues?
top = cc %>%
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n))

#head(top)

top%>%
  ggplot(aes(x = word, y = n, color =word)) +
  geom_col() +
  xlab(NULL) +
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))+
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
