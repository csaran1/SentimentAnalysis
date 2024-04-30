install.packages("tidytext")
library(tidytext)
library(dplyr)
library(tidyr)
library(pdftools)
library(textstem)
library(textdata)
txt <- pdf_text('/Users/admin/Documents/WebSearch-7700/Project1/beauty-the-beast.pdf')

txt<- unlist(txt)

#tokenizing the text into sentences
s0<- data_frame(page=1:length(txt), text=txt) %>%
  unnest_tokens(sentence, text, token= 'sentences')

# keeping the sentence num in dataframe 
s0$sentence_no<- c("")
print(length(s0$page))
for(x in 1:length(s0$page)) {
  s0$sentence_no[x]<-paste('sentence',x)
}

#further tokenizing sentences into words
w<- s0 %>%
  unnest_tokens(word,sentence)

# cleaning the text 
w<- w %>%
  anti_join(stop_words)

#determine the frequency of words
f=  w%>% count(word)%>% arrange(desc(n))

library(ggplot2)

# plotting the histogram  based on top occurences of the words based on frequency
f1<- subset(f,n>10)
ggplot(f1, aes(x=word, y=n))+
  geom_bar(stat='identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#word by sentiments can use lexicons- bing, affin
ws<- w %>%
  inner_join(get_sentiments("afinn"))

#sentiment for each sentence
s1 <- ws %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(sentence_no) %>%
  summarise(sentiment_score = sum(value)) %>%
  ungroup()

s2<- merge(s0,s1,by="sentence_no")

s3<-s2[order(s2$page),]

p0 <- s3 %>%
  group_by(page) %>%
  summarise(page_sentiment_score = sum(sentiment_score))

# Plotting AFINN lexicon
ggplot(p0, aes(x = page, y = page_sentiment_score)) +
  geom_bar(stat = "identity", fill = "green") +
  theme_minimal() +
  labs(title = "AFINN Lexicon Sentiment Scores",
       x = "page", y = "page_Sentiment Score")

mean(p0$page_sentiment_score)


#bing
bing <- get_sentiments("bing") # 6786 words
bing$word <- lemmatize_words(bing$word) 
bing <- bing %>% distinct() # 5641 words
bing$dict <- "bing"

ws_bing<- w %>%
  inner_join(get_sentiments("bing"))

# Convert sentiment to numeric values
ws_bing$sentiment_score <- ifelse(ws_bing$sentiment == "positive", 1, ifelse(ws_bing$sentiment == "negative", -1, 0))

# Group by page and lexicon, then summarize
p1 <- ws_bing %>%
  group_by(page) %>%
  summarise(sentiment_score = sum(sentiment_score))

sum(p1$sentiment_score)

# Plotting Bing lexicon
ggplot(p1, aes(x = page, y = sentiment_score)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Bing Lexicon Sentiment Scores",
       x = "page", y = "avvg_Sentiment")

#nrc
nrc <- get_sentiments("nrc") # 13872 words
nrc <- lemmatize_words(nrc$word)
nrc <- unique(nrc) # 6029 words
nrc <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) # 5624 words
nrc$word <- lemmatize_words(nrc$word)
nrc <- nrc %>% distinct() # 5299 words
nrc$dict <- "nrc" 

ws_nrc<- w %>%
  inner_join(get_sentiments("nrc"))
# Convert sentiment to numeric values
ws_nrc$sentiment_score <- ifelse(ws_nrc$sentiment == "positive", 1, ifelse(ws_nrc$sentiment == "negative", -1, 0))

# Group by page and lexicon, then summarize
p2 <- ws_nrc %>%
  group_by(page) %>%
  summarise(avg_sentiment = sum(sentiment_score))

sum(p2$avg_sentiment)

# Plotting NRC lexicon
ggplot(p2, aes(x = page, y = avg_sentiment)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "NRC Lexicon Sentiment Scores",
       x = "page", y = "avg_Sentiment Score")


# comparitive analysis of lexicons (bing,afinn,nrc)
dicts<- rbind(bing,nrc,afinn)

dicts %>%
  group_by(dict) %>%
  summarise(n_tokens = n(),
            n_types = length(unique(word)),
            n_in_both_pos_and_neg = n_tokens - n_types,
            pos_and_neg_words = paste(word[duplicated(word)], collapse = ", ")) 

dicts %>%
  group_by(dict, sentiment) %>%
  summarise(n_tokens = n()) %>%
  ggplot(aes(fill=sentiment, y=n_tokens, x=sentiment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = n_tokens), vjust = 0.1) +
  facet_wrap(~dict) +
  ggtitle("Number of Sentiment Words per Dictionary")


# finding unique words
dicts <- dicts %>%
  group_by(word) %>%
  mutate(n_dicts = n()) %>%
  ungroup()

# unique words in each lexicon graph
dicts %>%
  filter(n_dicts == 1) %>%
  group_by(dict, sentiment) %>%
  summarise(n_unique = n()) %>%
  ggplot(aes(fill=sentiment, y=n_unique, x=sentiment)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = n_unique), vjust = -0.2) +
  facet_wrap(~dict) +
  scale_fill_manual(values = c("darkblue", "hotpink")) +
  labs(title = "Number of Sentiment Words Unique to each Dictionary", 
       subtitle = "AKA words found ONLY in 1 dictionary")


# Combine sentiment scores from different lexicons into one dataframe
combined_df <- bind_rows(
  mutate(ws_bing, lexicon = "Bing"),
  mutate(ws, lexicon = "AFINN"),
  mutate(ws_nrc, lexicon = "NRC")
)









  