---
  title: "Glassdoor Sentiment Analysis & Topic Model"
author: 'Doris Kuo'
date: "02/21/2020"
output:
  radix::radix_article:
  toc: true
toc_depth: 2
---
  




#install.packages(c("dplyr","tidyverse","ggplot2","gridExtra","textdata","sentimentr","lexicon","magrittr","quantmod","ggpubr"))
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(textdata)
library(sentimentr)
library(lexicon)
library(magrittr)
library(quantmod)
library(ggpubr)
library(tm)
library(textstem)
library(stm)


#read data

load('glassDoor.Rdata')


# sentiment analyses


## get and clean columns
### pros,rating,workLifeRating, cultureValueRating

### 1. change data type


glassDoor_my <- glassDoor%>%
  select(pros,cons, advice, rating,workLifeRating, cultureValueRating)%>%
  mutate(rating = as.numeric(rating),)%>%
  mutate(workLifeRating = as.numeric(workLifeRating))%>%
  mutate(cultureValueRating = as.numeric(cultureValueRating))


### 2. deal with words which run together


glassDoor_my_all<-glassDoor_my%>%
  mutate(pros = gsub("([a-z])([A-Z])", "\\1 \\2", pros),
         pros = tolower(pros))%>%
  mutate(cons = gsub("([a-z])([A-Z])", "\\1 \\2", cons),
         cons = tolower(cons))%>%
  mutate(advice = gsub("([a-z])([A-Z])", "\\1 \\2", advice),
         advice = tolower(advice))

glassDoor_my <- glassDoor_my_all%>%
  select(-cons,-advice)
head(glassDoor_my)



## sentiment score
### calculate the sentiment "joy" levels in pros and see whether the joy score can influence the rating, workLifeRating, cultureValueRating


### 1. get joy words


nrcWord <- textdata::lexicon_nrc()
nrcWord_joy <- nrcWord%>%
  filter(sentiment=="joy")
head(nrcWord_joy,10)


### 2. get nrcValues


nrcValues <- lexicon::hash_sentiment_nrc


### 3. get the nrcValues for words in joy

joy_score <- nrcValues[nrcValues$x %in% nrcWord_joy$word,]
head(joy_score)


### 4. get pros' joy score


joy_score_Sentiment <- sentiment(get_sentences(glassDoor_my$pros), 
          polarity_dt = joy_score) %>% 
  group_by(element_id) %>% 
  summarize(nrc_meanjoyscore = mean(sentiment))
head(joy_score_Sentiment)


### 5. add score the original df

### clean up, change table type, and merge with the original df


# reset index
rownames(joy_score_Sentiment) <- joy_score_Sentiment$element_id

# delete element_id
joy_score_Sentiment<-joy_score_Sentiment%>%
  select(-element_id)

#convert to df
joy_score_Sentiment<-as.data.frame(joy_score_Sentiment)

#concat df
df_combined <- cbind(glassDoor_my, joy_score_Sentiment)


### 6. Result of sentiment joy score
concat the two df : original df with sentiment df

head(df_combined,20)


## Prediction / Influence

### nrc joy score X rating, nrc joy score X workLifeRating, nrc joy score X cultureValueRating


a<-df_combined %>%
  ggplot(aes(nrc_meanjoyscore, rating)) + 
  geom_point(size = 4, color = "#00AFBB", alpha=0.3)+
  geom_smooth(method = "lm", color="#108F98")+
  theme_minimal()+
  ggtitle('nrc joy score X rating')

b<-df_combined %>%
  ggplot(aes(nrc_meanjoyscore, workLifeRating)) + 
  geom_point(size = 4, color = "#F6C200", alpha=0.3)+
  geom_smooth(method = "lm", color="#D99900")+
  theme_minimal()+
  ggtitle('nrc joy score X workLifeRating')

c<-df_combined %>%
  ggplot(aes(nrc_meanjoyscore, cultureValueRating)) + 
  geom_point(size = 4, color = "#D04D31", alpha=0.3)+
  geom_smooth(method = "lm", color="#902711")+
  theme_minimal()+
  ggtitle('nrc joy score X cultureValueRating')

source("http://peterhaschke.com/Code/multiplot.R")
multiplot(a,b,c,cols=3)


> It is interesting and surprising that higher joy score will cause lower total rating. Probably it because employees emphasize othe criteria such as development or something than joy level while evaluating the total rating. Same as the cultureValueRating. High joy level leads to lower culture value rating.

> It make sense for WorkLifeRating. Higher level of joy level in pros comment will lead to higher workLifeRating.


# Topic Analysis

## Clean up data

cleanglassDoor_topic = glassDoor_my_all%>%
  select(pros,rating) %>% 
  mutate(text = as.character(pros), 
         #text = str_replace_all(text, "\n", " "),   
         #text = str_replace_all(text, "(\\[.*?\\])", ""),
         text = str_squish(text), 
         #text = gsub("([a-z])([A-Z])", "\\1 \\2", text), 
         #text = tolower(text), 
         text = removeWords(text, c("'", stopwords(kind = "en"))), 
         text = removePunctuation(text), 
         text = removeNumbers(text),
         text = lemmatize_strings(text), 
         doc_id = c(1:1831)) %>% 
  select(doc_id, text, rating)
head(cleanglassDoor_topic)



set.seed(1001)

holdoutRows = sample(1:nrow(cleanglassDoor_topic), 100, replace = FALSE)

glassDoorText = textProcessor(documents = cleanglassDoor_topic$text[-c(holdoutRows)], 
                          metadata = cleanglassDoor_topic[-c(holdoutRows), ], 
                          stem = FALSE)

glassDoorPrep = prepDocuments(documents = glassDoorText$documents, 
                               vocab = glassDoorText$vocab,
                               meta = glassDoorText$meta)

kTest = searchK(documents = glassDoorPrep$documents, 
             vocab = glassDoorPrep$vocab, 
             K = c(3, 4, 5, 10, 20), verbose = FALSE)

plot(kTest)


> find lowed residual and highed semantic coherence: therefore, I choose Number of Topics(k) = 10


topics10 = stm(documents = glassDoorPrep$documents, 
             vocab = glassDoorPrep$vocab, seed = 1001,
             K = 10, verbose = FALSE)

plot(topics10)


> topic7, 10 is the top2 most possible/close topic


labelTopics(topics10)


> When people talking about the pros of a company, they care more about good coworkers and flexible working environment. Then, the care more about whether people are smart in the working environment and whether their are some interesting project and clients.



findThoughts(topics10, texts = glassDoorPrep$meta$text, n = 1)
















