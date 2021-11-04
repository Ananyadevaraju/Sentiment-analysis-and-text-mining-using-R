#### MA331 -Programming and Text Analytics in R ####
## Student Registration no. - 2010487
## Coursework 
## Sentiment Analysis

#################################### Load data ####
#load dataset from child list
child_data<-read.csv(file="C:\\Users\\ANANYA D\\Desktop\\MSc modules\\Programming and text analytics in R\\Assignment and coursework\\Assignment 2\\child list\\11_Alice's Adventures in Wonderland.csv", header=TRUE)
#load dataset from adult list
adult_data<-read.csv(file="C:\\Users\\ANANYA D\\Desktop\\MSc modules\\Programming and text analytics in R\\Assignment and coursework\\Assignment 2\\Adult list\\15_Moby Dick.csv", header=TRUE)


################################## Load packages ####
library(dplyr)
#package for string processing
library(stringr)
#package for extracting tokens and lexicons
library(tidytext) 
#package for lexicons
library(textdata)
#package for visualization
library(ggplot2)


############################## Investigating the data ####
#child_data
#view the complete data  
View(child_data)
#getting the structure of the data
str(child_data)
#getting the summary statistics for the data
summary(child_data)
#checking the class of the data
class(child_data)
#checking the class of each column/variable in data
class(child_data$gutenberg_id)
class(child_data$text)

#adult_data
#view the complete data 
View(adult_data)
#getting the structure of the data
str(adult_data)
#getting the summary statistics for the data
summary(adult_data)
#checking the class of the data
class(child_data)
#checking the class of each column/variable in data
class(child_data$gutenberg_id)
class(child_data$text)

################################## Data cleaning ####

##drop columns which are not important
#we can drop the first column in both the datasets, as it contains only the id of the book
#and does not add any significant information to our analysis
#child_data<-child_data[,1]
#take a subset of the dataframe and unselect first column
child_data<-subset(child_data, select = -1)
adult_data<-subset(adult_data, select = -1)


#child_data cleaning
##drop the initial rows from the title to the list of the chapters, 
#till the beginning of the first chapter and the last row with "THE END"
#we will retain only the story for analysis 
#slice only the rows required for analysis
child_data<-child_data%>%
    slice(30:3378)
#detect the word "CHAPTER" in the text and filter out the entire row from the text for analysis
#also filter the empty strings in the text
child_data<-child_data%>%
   filter(!str_detect(text, pattern='^CHAPTER'),
          text != "") 

#adult_data cleaning
##drop initial rows with title, chapter names and extracts, till the beginning of first chapter
#we will only retain the story for the analysis
#slice only the rows required for analysis
adult_data<-adult_data%>%
  slice(664:22243)
#detect the word "CHAPTER" in the text and filter out the entire row from the text for analysis
#also filter the empty strings in the text
adult_data<-adult_data%>%
  filter(!str_detect(text, pattern="^CHAPTER"),
         text != "") 

############################### Extracting tokens ####
#from text in both the datasets
#extract vector of words from the texts and convert into lower case
#child_data
child_token_df<-child_data %>%
  unnest_tokens(word, text, token = "words", to_lower = TRUE)

#adult_data
adult_token_df<-adult_data %>% 
  unnest_tokens(word, text, token = "words", to_lower = TRUE)


############################# Cleaning tokens ####
#remove numbers and trim any white spaces
#detecting and filtering any rows (words) with numbers
#replace the character "_" in the words with nothing
#mutate is used to add a new column to the dataframe, but here we are overriding the existing column,
#with new changes

#child data
child_token_df <- child_token_df %>%
  mutate(word = str_trim(word))%>%
  filter(!str_detect(word, pattern="\\d"))%>%
  mutate(word = str_replace_all(word, pattern="_", replacement = ""))

#adult data
adult_token_df <- adult_token_df %>%
  mutate(word = str_trim(word))%>%
  filter(!str_detect(word, pattern="\\d"))%>%
  mutate(word = str_replace_all(word, pattern="_", replacement = ""))


############################# Word count ####
#get the count of all the words in both the datasets and arrange in descending order
#child data
child_token_df<-child_token_df %>% 
  count(word) %>%
  arrange(desc(n))

#dimension of the dataframe with all the unique words and their counts
dim(child_token_df)

#adult data
adult_token_df<-adult_token_df %>% 
  count(word) %>%
  arrange(desc(n))

dim(adult_token_df)


############################### Stop words ####
#remove/filter stop words (commonly used English language words) among the tokens in both the datasets
#child data
child_token_df <- child_token_df %>%
  filter(!word %in% stop_words$word)

#get the dimension of data.frame with tokens after removing the stop words
dim(child_token_df)
#summary of the dataframe
summary(child_token_df)

#adult data
adult_token_df <- adult_token_df %>%
  filter(!word %in% stop_words$word)

dim(adult_token_df)
summary(adult_token_df)


############################## Visualizing most frequently occurring words ####
#top 10 words among the most occurring words in the 2 books

#child data - 10 most frequently occurring words
#get the head of the dataframe with words and counts, as it is already arranged in descending order
top_10_child <- head(child_token_df, n=10)
top_10_child
#adult data - 10 most frequently occurring words
top_10_adult <- head(adult_token_df, n=10)

#plot to show the 10 most frequent words in child data
ggplot(top_10_child, aes(x=reorder(word, n), y=n, fill = "orangered3"))+     #reorder the columns/bars in the plot in descending order of n
  geom_col(show.legend = FALSE)+     #type of plot
  coord_flip()+   #flip the coordinates
  labs(caption ="Fig.1. 10 most frequent words in child data", x = "Word", y = "Frequency")+       #labelling the axes and plot
  theme_minimal(base_size=12)+        #setting the background theme for the plot and the size
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))  #adjusting the position of the caption
  
  
  
#plot to show the 10 most frequent words in adult data
ggplot(top_10_adult, aes(x=reorder(word, n), y=n, fill = "seagreen4"))+
  geom_col(show.legend = FALSE)+
  coord_flip()+
  labs(caption ="Fig.2. 10 most frequent words in adult data", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))


#10 words with the least frequency in the 2 books
#get the tail of the dataframe, as the words and their occurrances are already arranged in descending order
#child data
lowest_10_child <- tail(child_token_df, n=10)
#adult data
lowest_10_adult <- tail(adult_token_df, n=10)


############################# Sentiment analysis ####

#initially, download the lexicons required 
#downloading "bing"   
#classify words into positive and negative
bing <- get_sentiments("bing")

#downloading "afinn" lexicon 
#give positive and negative scores for words
afinn <- get_sentiments("afinn") %>%
  select(word, value)

#downloading "nrc" lexicon 
#extract more sentiments in the words
nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)



############################### Visualizing sentiments ####
#get the positive and negative sentiments in texts (for the words)
#"bing" lexicon object is a dataframe and has a column of words and the their associated sentiments
#merge the 2 objects using inner join, our child dataset and lexicon object(bing) by words common in both and obtain the sentiment
#child data
child_bing<-child_token_df %>% 
  inner_join(bing, by = "word") 

#show the head of the dataframe
head(child_bing)

#adult data
#using inner join to merge the 2 objects, our adult dataset and lexicon object(bing) by words common in both and obtain the sentiment
adult_bing<-adult_token_df %>% 
  inner_join(bing, by = "word") 

##show the head of the dataframe
head(adult_bing)


#############
## plotting the top 10 negative and positive words(highest occurrence) in child data
#get the 10 most frequently occurring words with positive sentiment 
child_positive<-child_bing %>% filter(sentiment=="positive") %>% head(n=10)
#get the 10 most frequently occurring words with negative sentiment
child_negative<-child_bing %>% filter(sentiment=="negative") %>% head(n=10)
#bind both the dataframes together into one
child_pos_neg<-bind_rows(child_positive,child_negative)
#plot
child_pos_neg %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) +    #reorder the columns/bars in the plot in descending order of n and fill different colours based on different sentiments 
  geom_col(show.legend = FALSE) +          #type of plot and do not show legend
  facet_wrap(~sentiment, scales = "free_y") +    #plot 2 plots in a row
  labs(x = "Word",y = "Frequency")+      #label x and y axis
  coord_flip()+          #flip the coordinates
  labs(caption ="Fig.3. Top 10 positive and negative sentiments in child data", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=1, size=rel(1)))


####plotting the top 10 negative and positive words in adult data
#get the 10 most frequently occurring words with positive sentiment
adult_positive<-adult_bing %>% filter(sentiment=="positive") %>% head(n=10)
#get the 10 most frequently occurring words with negative sentiment
adult_negative<-adult_bing %>% filter(sentiment=="negative") %>% head(n=10)
#bind both the dataframes together into one
adult_pos_neg<-bind_rows(adult_positive,adult_negative)
#plot
adult_pos_neg %>%
  ggplot(aes(x = reorder(word, n),y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Word",y = "Contribution to sentiment")+
  coord_flip()+
  labs(caption ="Fig.4. Top 10 positive and negative senitments in adult data", x = "Word", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=1, size=rel(1)))


#################
#count/proportion of negative and positive words in child data
#count of positive and negative words
table(child_bing$sentiment)
#number of positive and negative words 
count_child_negative<-sum(child_bing$sentiment=="negative")
count_child_positive<-sum(child_bing$sentiment=="positive")
#length of total words
count_child_words<-length(child_bing$sentiment)
#proportion of positive and negative words in the text
negative_prop_child<-count_child_negative/count_child_words
positive_prop_child<-count_child_positive/count_child_words


#count/proportion of negative and positive words in adult data
table(adult_bing$sentiment)
#proportion of positive and negative words 
count_adult_negative<-sum(adult_bing$sentiment=="negative")
count_adult_positive<-sum(adult_bing$sentiment=="positive")
count_adult_words<-length(adult_bing$sentiment)
#proportion of positive and negative words in the text
negative_prop_adult<-count_adult_negative/count_adult_words
positive_prop_adult<-count_adult_positive/count_adult_words


##################### positive and negative scores 
#get the positive and negative sentiment scores for the words in texts
child_afinn<-child_token_df %>% 
  inner_join(afinn, by = "word") 
#show the head of the dataframe
head(child_afinn)
#get the summary statistics for the  sentiment scores
summary(child_afinn)
#plot boxplot to visualize the summary statistics
boxplot(child_afinn$value, main="Fig.5. Sentiment scores for child data")
#number of words in each sentiment score
table(child_afinn$value)

#similarly, for adult data
adult_afinn<-adult_token_df %>% 
  inner_join(afinn, by = "word") 
head(adult_afinn)
summary(adult_afinn)
boxplot(adult_afinn$value, main="Fig.6. Sentiment scores for adult data")
#number of words in each sentiment score
table(adult_afinn$value)   


############## other sentiments (nrc lexicon)
#child data
#get the sentiments for the words common in child text and the lexicon objects, by merging using inner join
child_nrc<-child_token_df %>% 
  inner_join(nrc, by = "word") 
#print the head of the dataframe
head(child_nrc)
#count of each sentiment in the dataframe
table(child_nrc$sentiment)

#drop or filter negative and positive sentiments from this dataframe, 
#as we have already analysed it
#we will not analyse it again using nrc lexicon
child_nrc<-child_nrc%>%
  filter(!sentiment == "negative",
         !sentiment == "positive")

#plot the frequency of each sentiment in the child data
ggplot(child_nrc, aes(x=sentiment, fill = sentiment))+
  geom_bar()+       #type of plot
  coord_flip()+    #flip coordinates
  labs(caption ="Fig.7. Frequency of sentiments in child data", x = "Sentiment", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))

#count of other sentiments recognized by the nrc lexicon in child data
table(child_nrc$sentiment)
#count of other sentiments recognized by the nrc lexicon in child data
#filter and get a sum of each sentiment in the data
count_child_anger<-sum(child_nrc$sentiment=="anger")
count_child_anticipation<-sum(child_nrc$sentiment=="anticipation")
count_child_disgust<-sum(child_nrc$sentiment=="disgust")
count_child_fear<-sum(child_nrc$sentiment=="fear")
count_child_joy<-sum(child_nrc$sentiment=="joy")
count_child_sadness<-sum(child_nrc$sentiment=="sadness")
count_child_surprise<-sum(child_nrc$sentiment=="surprise")
count_child_trust<-sum(child_nrc$sentiment=="trust")
#total count of words (sentiments) recognized by nrc lexicon in child data
count_child_nrc<-length(child_nrc$sentiment)

#proportion of other sentiments recognized by the nrc lexicon in child data
#count of each sentiment(number of words associated to each sentiment) divided by total number of words recognized by nrc lexicon
anger_prop_child<-count_child_anger/count_child_nrc
anticipation_prop_child<-count_child_anticipation/count_child_nrc
disgust_prop_child<-count_child_disgust/count_child_nrc
fear_prop_child<-count_child_fear/count_child_nrc
joy_prop_child<-count_child_joy/count_child_nrc
sadness_prop_child<-count_child_sadness/count_child_nrc
surprise_prop_child<-count_child_surprise/count_child_nrc
trust_prop_child<-count_child_trust/count_child_nrc


#adult data
#get the sentiments for the words common in adult text and the lexicon objects, by merging usinf inner join
adult_nrc<-adult_token_df %>% 
  inner_join(nrc, by = "word") 
#print the head of the dataframe
head(adult_nrc)
#count of each sentiment in the dataframe
table(adult_nrc$sentiment)

#drop or filter negative and positive sentiments from this dataframe, 
#as we have already analysed it 
#we will not analyse it again using nrc lexicon
adult_nrc<-adult_nrc%>%
  filter(!sentiment == "negative",
         !sentiment == "positive")


#plot the frequency of each sentiment in the child data
ggplot(adult_nrc, aes(x=sentiment, fill= sentiment))+
  geom_bar()+     #type of plot
  coord_flip()+     #flip coordinates, x and y
  labs(caption ="Fig.8. Frequency of sentiments in adult data", x = "Sentiment", y = "Frequency")+
  theme_minimal(base_size=12)+
  theme(plot.caption = element_text(hjust=0.5, size=rel(1)))


#count of other sentiments recognized by the nrc lexicon in adult data
table(adult_nrc$sentiment)
#count of other sentiments recognized by the nrc lexicon in adult data
count_adult_anger<-sum(adult_nrc$sentiment=="anger")
count_adult_anticipation<-sum(adult_nrc$sentiment=="anticipation")
count_adult_disgust<-sum(adult_nrc$sentiment=="disgust")
count_adult_fear<-sum(adult_nrc$sentiment=="fear")
count_adult_joy<-sum(adult_nrc$sentiment=="joy")
count_adult_sadness<-sum(adult_nrc$sentiment=="sadness")
count_adult_surprise<-sum(adult_nrc$sentiment=="surprise")
count_adult_trust<-sum(adult_nrc$sentiment=="trust")
#total count of words (sentiments) recognized by nrc lexicon in adult data
count_adult_nrc<-length(adult_nrc$sentiment)

#proportion of other sentiments recognized by the nrc lexicon in adult data
(anger_prop_adult<-count_adult_anger/count_adult_nrc)
(anticipation_prop_adult<-count_adult_anticipation/count_adult_nrc)
(disgust_prop_adult<-count_adult_disgust/count_adult_nrc)
(fear_prop_adult<-count_adult_fear/count_adult_nrc)
(joy_prop_adult<-count_adult_joy/count_adult_nrc)
(sadness_prop_adult<-count_adult_sadness/count_adult_nrc)
(surprise_prop_adult<-count_adult_surprise/count_adult_nrc)
(trust_prop_adult<-count_adult_trust/count_adult_nrc)

