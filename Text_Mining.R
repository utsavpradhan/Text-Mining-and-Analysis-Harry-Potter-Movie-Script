library(dplyr)
library(tm)
library(textstem)
library(ggplot2)
library(tidyr)
library(tidytext)
library(ggthemes)
library(gridExtra)

#reading data
setwd("C:/Users/Lenovo/Downloads")
data <- read.csv("HP_script.csv")
#data<- data[1:1371,] for before case
#data<- data[1371:1536,] for after case

##Basic Text preprocessing
# bring to lower case 
data <- data %>% mutate(Narrative_lower = tolower(data$Dialogue)) # mutate() function is from the dplyr package and it is used to create a new column
# remove numbers
data <- data %>% mutate(Narrative_noNumbers = gsub('[[:digit:]]','',Narrative_lower)) # gsub functoin searches for any digit in the text and removes it; 
# remove stopwords
stopwords('en')
str(stopwords('en'))
stopwords('en')[1:10]
stopwords('en')[!stopwords('en') %in% c('i')]
c(stopwords('en'), "under")
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
data <- data %>% mutate(Narrative_noStopWords = gsub(stopwords_regex,'',Narrative_noNumbers))
data <- data %>% mutate(Narrative_noPunctuation = gsub('[[:punct:]]','',Narrative_noStopWords))
data <-data %>% mutate(Narrative_noTypos = gsub('thankssssssss','thanks',Narrative_noPunctuation))
data <- data %>% mutate(Narrative_noSpaces = gsub('\\s+',' ',Narrative_noTypos))
# stemming 
data <-data %>% mutate(Narrative_Stem = stem_strings(Narrative_noSpaces))
# lemmatization
data <-data %>% mutate(Narrative_Lemma = lemmatize_strings(Narrative_noSpaces))
head(data)
# keep just the text column
my_text <- data %>% select(Narrative_Lemma)

# create DTM 
my_corpus <- my_text
my_corpus <- my_corpus %>% rename(text = Narrative_Lemma)  %>% mutate(doc_id = rownames(my_text))
my_corpus <- Corpus(DataframeSource(my_corpus))  # transform the data frame into a corpus
str(my_corpus)
# check the first conversation
inspect(my_corpus[[1]])
# Transform the text to DTM
my_dtm <- as.matrix(DocumentTermMatrix(my_corpus))
str(my_dtm)
# create TDM
my_tdm <- as.matrix(TermDocumentMatrix(my_corpus))
str(my_tdm)
my_tdm <- sort(rowSums(my_tdm), decreasing = T)
my_tdm <- data.frame(Word = names(my_tdm), Number = my_tdm)

#word cloud
library(wordcloud2)
wc <- my_tdm %>%
  filter(Number > 8) %>%
  select(Word, Number) %>%
  wordcloud2(., color = 'random-dark', backgroundColor = "white", size = 0.9)
wc

data$Dialogue <- as.character(data$Dialogue)

#bigrams
data %>%
  unnest_tokens(output = word, input = Dialogue, token = "ngrams", n = 2) %>%
  filter(is.na(word)==F) %>%
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% c("on", "in", "the", "be", "are", "i", "you", "is", "to", "a", "has", "of", "it", "he", "was", "it's")) %>%
  filter(!word2 %in% c("on", "in", "the", "be", "are", "i", "you", "is", "to", "a", "has", "of", "it", "he", "was", "it's")) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = T) %>% 
  slice(1:10) %>%
  ggplot(., aes(reorder(word, +n), n))+
  geom_bar(stat = "identity", width = 0.65, fill = "#b7e82b", alpha = 0.85)+
  coord_flip()+
  labs(title = "10 most used bigrams",
       x = "Bigram", y = "Frequency")+
  theme_clean()

#trigram
data %>%
  unnest_tokens(output = word, input = Dialogue, token = "ngrams", n = 3) %>%
  filter(is.na(word)==F) %>%
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% c("on", "in", "the", "be", "are", "i", "you", "is", "to", "a", "has", "of", "it", "he", "was", "it's", "we")) %>%
  filter(!word2 %in% c("you", "we", "the")) %>% 
  filter(!word3 %in% c("on", "in", "the", "be", "are", "i", "you", "is", "to", "a", "has", "of", "it", "he", "was", "it's", "we")) %>% 
  unite(word,word1, word2, word3, sep = " ") %>% 
  count(word, sort = T) %>% 
  slice(1:10) %>%
  ggplot(., aes(reorder(word, +n), n))+
  geom_bar(stat = "identity", width = 0.62, fill = "sea green", alpha = 0.85)+
  coord_flip()+
  labs(title = "10 most used trigrams",
       x = "Trigram", y = "Frequency")+
  theme_clean()

#sentimental analysis using nrc
Sentiment <- data %>%
  unnest_tokens(output = word, input = Dialogue) %>%
  left_join(get_sentiments("nrc"), "word") %>%
  filter(is.na(sentiment)==F)

Sentiment %>% 
  filter(Speaker %in% c("Harry Potter","Albus Dumbledore","Cornelius Fudge", "Dolores Umbridge"))  %>%
  group_by(Speaker, sentiment) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(Speaker, count, fill = sentiment))+
  geom_bar(stat = "identity", position = "fill", width = 0.57, alpha = 0.9)+
  scale_x_discrete(limits = c("Harry Potter","Albus Dumbledore","Cornelius Fudge", "Dolores Umbridge"))+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  labs(title = "Sentiments of Key Characters", fill = "Sentiment",
       x = "Character", y = "Share")+
  guides(fill = guide_legend(reverse = T))+
  theme_base()+
  theme(legend.title.align = 0.5, legend.position = "right", legend.direction = "vertical")

#loughran lexicon sentimental analysis
Sentiment <- data %>%
  unnest_tokens(output = word, input = Dialogue) %>%
  left_join(get_sentiments("loughran"), "word") %>%
  filter(is.na(sentiment)==F)

Sentiment %>% 
  filter(Speaker %in% c("Harry Potter","Albus Dumbledore","Cornelius Fudge", "Dolores Umbridge"))  %>%
  group_by(Speaker, sentiment) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(Speaker, count, fill = sentiment))+
  geom_bar(stat = "identity", position = "fill", width = 0.57, alpha = 0.9)+
  scale_x_discrete(limits = c("Harry Potter","Albus Dumbledore","Cornelius Fudge", "Dolores Umbridge"))+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  labs(title = "Sentiments of Key Characters", fill = "Sentiment",
       x = "Character", y = "Share")+
  guides(fill = guide_legend(reverse = T))+
  theme_test()+
  theme(legend.title.align = 0.5, legend.position = "right", legend.direction = "vertical")

#Bing lexicon sentimental analysis
Sentiment <- data %>%
  unnest_tokens(output = word, input = Dialogue) %>%
  left_join(get_sentiments("bing"), "word") %>%
  filter(is.na(sentiment)==F)

Sentiment %>% 
  filter(Speaker %in% c("Harry Potter","Albus Dumbledore","Cornelius Fudge", "Dolores Umbridge"))  %>%
  group_by(Speaker, sentiment) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ggplot(., aes(Speaker, count, fill = sentiment))+
  geom_bar(stat = "identity", position = "fill", width = 0.57, alpha = 0.9)+
  scale_x_discrete(limits = c("Harry Potter","Albus Dumbledore","Cornelius Fudge", "Dolores Umbridge"))+
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  labs(title = "Sentiments of Key Characters", fill = "Sentiment",
       x = "Character", y = "Share")+
  guides(fill = guide_legend(reverse = T))+
  theme_test()+
  theme(legend.title.align = 0.5, legend.position = "right", legend.direction = "vertical")

#NRC for radial plot
Nrc <- data %>% unnest_tokens(output = word, input = Dialogue) %>%
  inner_join(get_sentiments("nrc"), by="word")

n1 <- Nrc %>% filter(Speaker=="Cornelius Fudge") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment)) +
  labs(x="", y="", title="Cornelius Fudge")
n1

n2 <- Nrc %>% filter(Speaker=="Albus Dumbledore") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment)) +
  labs(x="", y="", title="Albus Dumbledore")
n2

n3 <- Nrc %>% filter(Speaker=="Harry Potter") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment)) +
  labs(x="", y="", title="Harry Potter")
n3

n4 <- Nrc %>% filter(Speaker=="Dolores Umbridge") %>% count(sentiment) %>%
  ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
  geom_bar(stat="identity") + coord_polar() +
  theme(legend.position = "none", axis.text.x = element_blank()) +
  geom_text(aes(label=sentiment)) +
  labs(x="", y="", title="Harry Potter")
n4

grid.arrange(n1, n2,n3,n4, nrow=2,ncol=2)