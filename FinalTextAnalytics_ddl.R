#install.packages("readxl")

#install.packages(c("dplyr", "wordcloud", "tidytext", "tidyverse", "tidyr", "ggplot2", "tidytext","readxl"))
#install.packages("devtools")
#devtools::install_github("mjockers/syuzhet")
#install.packages('syuzhet', dependencies = TRUE, repos='http://cran.rstudio.com/')
# 
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/ggpubr")

library(tm)
library(tidytext)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(rworldmap)
library(ggraph)
library(igraph)
library(dplyr)
library(gapminder)
library(widyr)
library(gridExtra)
library(grid)
library(ggstatsplot)
library(plotly)
library(readxl)
library(syuzhet)

library("ggpubr")


#TextInformation <- file.choose()
TextInformation<-read_excel("E:\\Uthi-DU\\datadriven\\Assignment 10\\Group-Personal ReflectiveTextAnalytics.xlsx")
names(TextInformation) <- TextInformation[1,]
TextInformation <- TextInformation[-1,]




text_tb <- tibble(
                  Member = TextInformation$Member,
                  Topic = TextInformation$Topic,
                  Q11 = TextInformation$Q11,
                  Q12 = TextInformation$Q12,
                  Q13 = TextInformation$Q13,
                  Q14 = TextInformation$Q14,
                  Q15 = TextInformation$Q15,
                  Q16 = TextInformation$Q16
                  
)


bing <- get_sentiments("bing")

###Q11- untoken text
All_counts_Q11 <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Member,Topic) %>%
  unnest_tokens(word, Q11)%>%
  anti_join(stop_words, by = "word") %>%
  #unnest_tokens(word, Q12)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()


###Q12- untoken text
All_counts_Q12 <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Member,Topic) %>%
  unnest_tokens(word, Q12)%>%
  anti_join(stop_words, by = "word") %>%
  #unnest_tokens(word, Q12)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

###Q13- untoken text
All_counts_Q13 <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Member,Topic) %>%
  unnest_tokens(word, Q13)%>%
  anti_join(stop_words, by = "word") %>%
  #unnest_tokens(word, Q12)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

###Q14- untoken text
All_counts_Q14 <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Member,Topic) %>%
  unnest_tokens(word, Q14)%>%
  anti_join(stop_words, by = "word") %>%
  #unnest_tokens(word, Q12)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

###Q15- untoken text
All_counts_Q15 <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Member,Topic) %>%
  unnest_tokens(word, Q15)%>%
  anti_join(stop_words, by = "word") %>%
  #unnest_tokens(word, Q12)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()

###Q16- untoken text
All_counts_Q16 <- na.omit(text_tb) %>%
  select(everything()) %>%
  group_by(Member,Topic) %>%
  unnest_tokens(word, Q16)%>%
  anti_join(stop_words, by = "word") %>%
  #unnest_tokens(word, Q12)%>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)%>%
  ungroup()


### merge tokened data Q11 - Q16
total <- rbind(All_counts_Q12, All_counts_Q11,All_counts_Q13,All_counts_Q14,All_counts_Q15,All_counts_Q16)



#with circle connection - Debiasing 
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('Debiasing', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    theme_void()
)




#with circle connection - Secretary Problem  
jpeg("secretary.jpeg", quality = 75)
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('Secretary Problem', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    ggtitle("Secretary")+
    theme_void()
)
dev.off()


#with circle connection - Institution   
jpeg("Institution.jpeg", quality = 75)
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('Instituition', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    ggtitle("Institution")+
    theme_void()
)
dev.off()

#with circle connection - Exponential Backoff      
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('Exponential Backoff', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    theme_void()
)

#with circle connection - MABE     
jpeg("MABE.jpeg", quality = 75)
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('MABE', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    ggtitle("MABE")+
    theme_void()
)

dev.off()

#with circle connection - Weber        
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('Weber', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    theme_void()
)

#with circle connection - Team Analysis  
jpeg("TeamAnalysis.jpeg", quality = 75)
set.seed(123)
(word_pairs <- na.omit(total) %>%
    group_by(Member,Topic) %>%
    filter(grepl('Team Analysis', Topic))%>%
    filter(!grepl('[0-9]', word)) %>%
    group_by(word) %>%
    mutate(freq=n()) %>%  
    pairwise_cor(word, freq, sort = TRUE)%>% 
    #pairwise_cor(word, Topic, sort = TRUE)%>%
    #filter(!is.na(correlation),correlation > .65)%>%
    graph_from_data_frame()%>%
    ggraph( layout = "linear", circular = TRUE) +
    geom_edge_arc(aes(colour = correlation))+
    geom_node_point(color = "lightblue",stroke=1) +
    geom_node_text(aes(label = name), repel = FALSE) +
    ggtitle("Team Analysis")+
    theme_void()
)
dev.off()

# #with noncircle connection
# set.seed(123)
# (word_pairs <- na.omit(total) %>%
#     group_by(Member,Topic) %>%
#     filter(grepl('MABE', Topic))%>%
#     filter(!grepl('[0-9]', word)) %>%
#     group_by(word) %>%
#     mutate(freq=n()) %>%  
#     pairwise_cor(word, freq, sort = TRUE)%>% 
#     filter(!is.na(correlation),correlation > .65)%>%
#     graph_from_data_frame()%>%
#     ggraph(layout = "fr") +
#     geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
#     geom_node_point(color = "lightblue", size = 5) +
#     geom_node_text(aes(label = name), repel = TRUE) +
#     theme_void()
# )


##########Emotions on Q11
d_Q11<-get_nrc_sentiment(as.character(All_counts_Q11))#left_join(get_sentiments("nrc"), by = "word")
result_Q11<-data.frame(t(d_Q11))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result_Q11 <- data.frame(rowSums(result_Q11))
#name rows and columns of the dataframe
names(new_result_Q11)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result_Q11), new_result_Q11)
rownames(new_result_Q11) <- NULL


#plot the first 8 rows,the distinct emotions
#qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("GROUP H : Summary - Q11")


 jpeg("Q11.jpeg", quality = 75)
# 
q11<-  ggplot(data=new_result[1:8,], aes (x="", y = count, fill = sentiment)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(count / sum(count) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Q11") + 
  coord_polar("y")
 dev.off()

##########Emotions on Q12
d_Q12<-get_nrc_sentiment(as.character(All_counts_Q12))
result_Q12<-data.frame(t(d_Q12))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result_Q12 <- data.frame(rowSums(result_Q12))
#name rows and columns of the dataframe
names(new_result_Q12)[1] <- "count"
new_result_Q12 <- cbind("sentiment" = rownames(new_result_Q12), new_result_Q12)
rownames(new_result_Q12) <- NULL


#plot the first 8 columns,the distinct emotions
#qplot(sentiment, data=new_result_Q12[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("GROUP H : Summary - Q12")

q12<-ggplot(data=new_result_Q12[1:8,], aes (x="", y = count, fill = sentiment)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(count / sum(count) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Q12") + 
  coord_polar("y")


##########Emotions on Q13
d_Q13<-get_nrc_sentiment(as.character(All_counts_Q13))#left_join(get_sentiments("nrc"), by = "word")
result_Q13<-data.frame(t(d_Q13))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result_Q13 <- data.frame(rowSums(result_Q13))
#name rows and columns of the dataframe
names(new_result_Q13)[1] <- "count"
new_result_Q13 <- cbind("sentiment" = rownames(new_result_Q13), new_result_Q13)
rownames(new_result_Q13) <- NULL
#plot the first 8 rows,the distinct emotions

#qplot(sentiment, data=new_result_Q13[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("GROUP H : Summary - Q13")

jpeg("Q13.jpeg", quality = 75)

q13<-ggplot(data=new_result_Q13[1:8,], aes (x="", y = count, fill = sentiment)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(count / sum(count) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Q13") + 
  coord_polar("y")

dev.off()

##########Emotions on Q14
d_Q14<-get_nrc_sentiment(as.character(All_counts_Q14))#left_join(get_sentiments("nrc"), by = "word")
result_Q14<-data.frame(t(d_Q14))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result_Q14 <- data.frame(rowSums(result_Q14))
#name rows and columns of the dataframe
names(new_result_Q14)[1] <- "count"
new_result_Q14 <- cbind("sentiment" = rownames(new_result_Q14), new_result_Q14)
rownames(new_result_Q14) <- NULL


#plot the first 8 rows,the distinct emotions
#qplot(sentiment, data=new_result_Q14[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("GROUP H : Summary - Q14")

jpeg("Q14.jpeg", quality = 75)
q14<- ggplot(data=new_result_Q14[1:8,], aes (x="", y = count, fill = sentiment)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(count / sum(count) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.6)) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = 'top', 
        legend.direction = "horizontal", 
        legend.box = "horizontal") + 
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Q14") + 
  coord_polar("y")

dev.off()

##########Emotions on Q15
d_Q15<-get_nrc_sentiment(as.character(All_counts_Q15))#left_join(get_sentiments("nrc"), by = "word")
result_Q15<-data.frame(t(d_Q15))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result_Q15 <- data.frame(rowSums(result_Q15))
#name rows and columns of the dataframe
names(new_result_Q15)[1] <- "count"
new_result_Q15 <- cbind("sentiment" = rownames(new_result_Q15), new_result_Q15)
rownames(new_result_Q15) <- NULL

#plot the first 8 rows,the distinct emotions
#qplot(sentiment, data=new_result_Q15[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("GROUP H : Summary - Q15")

q15<-ggplot(data=new_result_Q15[1:8,], aes (x="", y = count, fill = sentiment)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(count / sum(count) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Q15") + 
  coord_polar("y")

##########Emotions on Q16
d_Q16<-get_nrc_sentiment(as.character(All_counts_Q16))#left_join(get_sentiments("nrc"), by = "word")
result_Q16<-data.frame(t(d_Q16))
#rowSums computes column sums across rows for each level of a #grouping variable.
new_result_Q16 <- data.frame(rowSums(result_Q16))
#name rows and columns of the dataframe
names(new_result_Q16)[1] <- "count"
new_result_Q16 <- cbind("sentiment" = rownames(new_result_Q16), new_result_Q16)
rownames(new_result_Q16) <- NULL

#plot the first 8 rows,the distinct emotions
#qplot(sentiment, data=new_result_Q16[1:8,], weight=count, geom="bar",fill=sentiment)+ggtitle("GROUP H : Summary - Q16")

q16<-ggplot(data=new_result_Q16[1:8,], aes (x="", y = count, fill = sentiment)) + 
  geom_col(position = 'stack', width = 1) +
  geom_text(aes(label = paste(round(count / sum(count) * 100, 1), "%"), x = 1.3),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Q16") + 
  coord_polar("y")

#Grouping plot
jpeg("Questionplot.jpeg", quality = 75)
figure <- ggarrange(q11,q13,q14, 
                    common.legend = TRUE, legend = "bottom",
                    ncol = 3, nrow = 2)
figure
dev.off()