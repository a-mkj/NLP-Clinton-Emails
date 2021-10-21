#Loading required libraries
library(stm)
library(tm)
library(SnowballC)
library(Matrix)
library(stm)
library(gofastr)
#library(stylo)
library(qdapDictionaries)
library(ggplot2)

#Clearing workspace and moving to correct folder
rm(list=ls())
setwd( "/Desktop/" )

#Reading in data
dat <- read.csv( 'Clinton.csv' )

#Getting a sense for what the most frequent words are across documents
dat_unigrams <- dat[ , 10:3009 ]
dat_unigram_freq <- colSums( dat_unigrams )
df_unigrams <- data.frame( names( dat_unigrams ), dat_unigram_freq )
names( df_unigrams ) <- c( 'Word', 'Count' )
df_unigrams <- df_unigrams[ order( df_unigrams$Count ), ]

#Getting a sense for what the most frequent bigrams are across documents
dat_bigrams <- dat[ , 3010:4009 ]
dat_bigram_freq <- colSums( dat_bigrams )
df_bigrams <- data.frame( names( dat_bigrams ), dat_bigram_freq )
names( df_bigrams ) <- c( 'Bigram', 'Count' )
df_bigrams <- df_bigrams[ order( df_bigrams$Count ), ]

#Getting a sense for what the most frequent trigrams are across documents
dat_trigrams <- dat[ , 4010:4509 ]
dat_trigram_freq <- colSums( dat_trigrams )
df_trigrams <- data.frame( names( dat_trigrams ), dat_trigram_freq )
names( df_trigrams ) <- c( 'Trigram', 'Count' )
df_trigrams <- df_trigrams[ order( df_trigrams$Count ), ]

#Running LDA model using unigrams
dat_unigrams <- dat[ , c( 10:3009 ) ]
#Removing stopwords and meaningless words below
x_words <- grep('X', colnames( dat ), value=TRUE)
stopwords = c(stopwords(), x_words, "s", "d", "u", "e", "f", "g", "t", "r", "m", "w", "j", "c", "p", "h", "l", "o", "r", "de", "el", "al", "en", "b", 'in.', 'will', 'if.', 're', 'for.', 'http', 'com', 'www', 'hdr22' )
dat_unigrams <- dat_unigrams[ , !names(dat_unigrams) %in% stopwords ]
rownames( dat_unigrams ) <- dat$DocNumber
mat_unigrams <- as.matrix( dat_unigrams )
mat_unigrams <- Matrix( mat_unigrams)

lda_fit <- stm( documents = mat_unigrams, K = 10, seed = 8675309 )
labelTopics( lda_fit )

#Analysis of Benghazi/Libya
docs <- data.frame( make.dt( lda_fit )[ , c( 1, 9 ) ] )
colnames( docs ) <- c( 'doc', 'loading' )
docs$date <- dat$DateSent
docs$year <- substr(docs$date, 0, 4)
docs$month <- substr(docs$date, 6, 7)
docs <- docs[ docs$loading > 0.05, ]
pre_benghazi_docs_index <- docs[ ( docs$year < 2012 ), 'doc'  ]
post_benghazi_docs_index <- docs[ ( docs$year >= 2012 ), 'doc' ]

#Looking at before vs during Benghazi
pre_benghazi_docs <- dat_unigrams[ dat$ID %in%  pre_benghazi_docs_index, ]
post_benghazi_docs <- dat_unigrams[ dat$ID %in%  post_benghazi_docs_index, ]

#Positive/Negative Word Analysis
#Examining the number of positive vs negative words
pos_words <- scan("positivewords.txt", what="", sep="\n")
neg_words <- scan("negativewords.txt", what="", sep="\n")
pre_beng_pos_count <- c()
pre_beng_neg_count <- c()
post_beng_pos_count <- c()
post_beng_neg_count <- c()

#Consider the pre-Benghazi words
pre_col_names <- colnames( pre_benghazi_docs )
for ( i in 1:nrow( pre_benghazi_docs ) ){
  row_temp <- pre_benghazi_docs[ i, ]
  words_temp <- pre_col_names[ which( row_temp > 0 ) ]
  pre_beng_pos_count <- c( pre_beng_pos_count, length( intersect( words_temp, pos_words ) ) )
  pre_beng_neg_count <- c( pre_beng_neg_count, length( intersect( words_temp, neg_words ) ) )
}

#Consider the post-Benghazi words
post_col_names <- colnames( post_benghazi_docs )
for ( i in 1:nrow( post_benghazi_docs ) ){
  row_temp <- post_benghazi_docs[ i, ]
  words_temp <- post_col_names[ which( row_temp > 0 ) ]
  post_beng_pos_count <- c( post_beng_pos_count, length( intersect( words_temp, pos_words ) ) )
  post_beng_neg_count <- c( post_beng_neg_count, length( intersect( words_temp, neg_words ) ) )
}

#Final results
pre_beng_neg_ratio <- pre_beng_neg_count / (pre_beng_neg_count + pre_beng_pos_count)
post_beng_neg_ratio <- post_beng_neg_count/ ( post_beng_neg_count + post_beng_pos_count )


#Hilary Sent vs Received for different topics:

#Running LDA
lda_fit_temp <- stm( documents = mat_unigrams, K = 20, seed = 8675309 )
labelTopics( lda_fit_temp )

#Topics <- 6, 10, 11, 13, 15
dat_new <- dat
dat_new$actual_sender <- dat_new$Sender
dat_new[ dat_new$Sender == 'hrod17 clintonemail com', 'actual_sender' ] <- 'H'
dat_new[ dat_new$Sender == 'Clinton  Hillary Rodham', 'actual_sender' ] <- 'H'
dat_new[ dat_new$Sender == 'Hillary Rodham Clinton', 'actual_sender' ] <- 'H'
dat_new[ dat_new$Sender == 'Clinton Hillary R', 'actual_sender' ] <- 'H'
dat_new$actual_receiver <- dat_new$Recipient
dat_new[ dat_new$Recipient == "Clinton  Hillary", 'actual_receiver' ] <- 'H'
dat_new[ dat_new$Recipient == "Hillary", 'actual_receiver' ] <- 'H'
dat_new[ dat_new$Recipient == "Clinton  Hillary Rodham", 'actual_receiver' ] <- 'H'
dat_new[ dat_new$Recipient == "Clinton", 'actual_receiver' ] <- 'H'

#Getting Loadings
df_temp <- data.frame( make.dt( lda_fit_temp ) )

#Illustrative histograms for many of the topics
hist( df_temp$Topic17, col = 'pink', xlab = 'Loading on Topic', ylab = 'Frequency', main = 'Loadings on Topic 17' )

#Saving the emails that correspond to each topic (loading threshold of 0.05)
emails6 <- df_temp$docnum[ df_temp$Topic6 >= 0.05 ]
emails10 <- df_temp$docnum[ df_temp$Topic10 >= 0.05 ]
emails11 <- df_temp$docnum[ df_temp$Topic11 >= 0.05 ]
emails13 <- df_temp$docnum[ df_temp$Topic13 >= 0.05 ]
emails15 <- df_temp$docnum[ df_temp$Topic15 >= 0.05 ]

library( syuzhet )

#Analysis of Topic 6
relevant_mails <- dat_new[ dat_new$ID %in% emails6,  ]
h_sent <- relevant_mails[ relevant_mails$actual_sender == 'H', 'ID' ]
h_rec <- relevant_mails[ relevant_mails$actual_receiver == 'H', 'ID' ]
mat_sent <- mat_unigrams[ h_sent, ]
mat_rec <- mat_unigrams[ h_rec, ]

sent6_sentiment = get_nrc_sentiment( colnames( mat_sent ) )
rec6_sentiment = get_nrc_sentiment( colnames( mat_rec ) )

for (i in 1:nrow(sent6_sentiment)) {
  sent6_sentiment[i,] = sent6_sentiment[i,]*sum(mat_sent[,i])
  rec6_sentiment[i,] = rec6_sentiment[i,]*sum(mat_rec[,i])
}

total_sent <- data.frame(prop = colSums(sent6_sentiment)/sum(colSums(sent6_sentiment)))
total_rec <- data.frame(prop = colSums(rec6_sentiment)/sum(colSums(rec6_sentiment)))
sent_sent <- data.frame(sentiment = colnames(sent6_sentiment), prop = total_sent)
sent_rec <- data.frame(sentiment = colnames(rec6_sentiment), prop = total_rec)

diff6 <- data.frame( sentiment = colnames(rec6_sentiment), diff = sent_sent$prop - sent_rec$prop )

ggplot( diff6[1:8,], aes(x= sentiment, weight = diff, fill=sentiment)) + geom_bar() +ggtitle("Sentiment Diff. for Topic 6 (Israel/Palestine)") + theme(plot.title = element_text(hjust = 0.5)) + xlab( 'Sentiment' ) + ylab( 'Proportion (Sent - Rec)' )


#Analysis of Topic 10
relevant_mails <- dat_new[ dat_new$ID %in% emails10,  ]
h_sent <- relevant_mails[ relevant_mails$actual_sender == 'H', 'ID' ]
h_rec <- relevant_mails[ relevant_mails$actual_receiver == 'H', 'ID' ]
mat_sent <- mat_unigrams[ h_sent, ]
mat_rec <- mat_unigrams[ h_rec, ]

sent6_sentiment = get_nrc_sentiment( colnames( mat_sent ) )
rec6_sentiment = get_nrc_sentiment( colnames( mat_rec ) )

for (i in 1:nrow(sent6_sentiment)) {
  sent6_sentiment[i,] = sent6_sentiment[i,]*sum(mat_sent[,i])
  rec6_sentiment[i,] = rec6_sentiment[i,]*sum(mat_rec[,i])
}

total_sent <- data.frame(prop = colSums(sent6_sentiment)/sum(colSums(sent6_sentiment)))
total_rec <- data.frame(prop = colSums(rec6_sentiment)/sum(colSums(rec6_sentiment)))
sent_sent <- data.frame(sentiment = colnames(sent6_sentiment), prop = total_sent)
sent_rec <- data.frame(sentiment = colnames(rec6_sentiment), prop = total_rec)

diff10 <- data.frame( sentiment = colnames(rec6_sentiment), diff = sent_sent$prop - sent_rec$prop )

ggplot( diff10[1:8,], aes(x= sentiment, weight = diff, fill=sentiment)) + geom_bar() +ggtitle("Sentiment Diff. for Topic 10 (Benghazi)") + theme(plot.title = element_text(hjust = 0.5)) + xlab( 'Sentiment' ) + ylab( 'Proportion (Sent - Rec)' )


#Analysis of Topic 11
relevant_mails <- dat_new[ dat_new$ID %in% emails11,  ]
h_sent <- relevant_mails[ relevant_mails$actual_sender == 'H', 'ID' ]
h_rec <- relevant_mails[ relevant_mails$actual_receiver == 'H', 'ID' ]
mat_sent <- mat_unigrams[ h_sent, ]
mat_rec <- mat_unigrams[ h_rec, ]

sent6_sentiment = get_nrc_sentiment( colnames( mat_sent ) )
rec6_sentiment = get_nrc_sentiment( colnames( mat_rec ) )

for (i in 1:nrow(sent6_sentiment)) {
  sent6_sentiment[i,] = sent6_sentiment[i,]*sum(mat_sent[,i])
  rec6_sentiment[i,] = rec6_sentiment[i,]*sum(mat_rec[,i])
}

total_sent <- data.frame(prop = colSums(sent6_sentiment)/sum(colSums(sent6_sentiment)))
total_rec <- data.frame(prop = colSums(rec6_sentiment)/sum(colSums(rec6_sentiment)))
sent_sent <- data.frame(sentiment = colnames(sent6_sentiment), prop = total_sent)
sent_rec <- data.frame(sentiment = colnames(rec6_sentiment), prop = total_rec)

diff11 <- data.frame( sentiment = colnames(rec6_sentiment), diff = sent_sent$prop - sent_rec$prop )

ggplot( diff11[1:8,], aes(x= sentiment, weight = diff, fill=sentiment)) + geom_bar() +ggtitle("Sentiment Diff. for Topic 11 (Domestic Politics)") + theme(plot.title = element_text(hjust = 0.5)) + xlab( 'Sentiment' ) + ylab( 'Proportion (Sent - Rec)' )


#Analysis of Topic 13
relevant_mails <- dat_new[ dat_new$ID %in% emails13,  ]
h_sent <- relevant_mails[ relevant_mails$actual_sender == 'H', 'ID' ]
h_rec <- relevant_mails[ relevant_mails$actual_receiver == 'H', 'ID' ]
mat_sent <- mat_unigrams[ h_sent, ]
mat_rec <- mat_unigrams[ h_rec, ]

sent6_sentiment = get_nrc_sentiment( colnames( mat_sent ) )
rec6_sentiment = get_nrc_sentiment( colnames( mat_rec ) )

for (i in 1:nrow(sent6_sentiment)) {
  sent6_sentiment[i,] = sent6_sentiment[i,]*sum(mat_sent[,i])
  rec6_sentiment[i,] = rec6_sentiment[i,]*sum(mat_rec[,i])
}

total_sent <- data.frame(prop = colSums(sent6_sentiment)/sum(colSums(sent6_sentiment)))
total_rec <- data.frame(prop = colSums(rec6_sentiment)/sum(colSums(rec6_sentiment)))
sent_sent <- data.frame(sentiment = colnames(sent6_sentiment), prop = total_sent)
sent_rec <- data.frame(sentiment = colnames(rec6_sentiment), prop = total_rec)

diff13 <- data.frame( sentiment = colnames(rec6_sentiment), diff = sent_sent$prop - sent_rec$prop )

ggplot( diff13[1:8,], aes(x= sentiment, weight = diff, fill=sentiment)) + geom_bar() +ggtitle("Sentiment Diff. for Topic 13 (Development/Aid)") + theme(plot.title = element_text(hjust = 0.5)) + xlab( 'Sentiment' ) + ylab( 'Proportion (Sent - Rec)' )


#Analysis of Topic 15
relevant_mails <- dat_new[ dat_new$ID %in% emails15,  ]
h_sent <- relevant_mails[ relevant_mails$actual_sender == 'H', 'ID' ]
h_rec <- relevant_mails[ relevant_mails$actual_receiver == 'H', 'ID' ]
mat_sent <- mat_unigrams[ h_sent, ]
mat_rec <- mat_unigrams[ h_rec, ]

sent6_sentiment = get_nrc_sentiment( colnames( mat_sent ) )
rec6_sentiment = get_nrc_sentiment( colnames( mat_rec ) )

for (i in 1:nrow(sent6_sentiment)) {
  sent6_sentiment[i,] = sent6_sentiment[i,]*sum(mat_sent[,i])
  rec6_sentiment[i,] = rec6_sentiment[i,]*sum(mat_rec[,i])
}

total_sent <- data.frame(prop = colSums(sent6_sentiment)/sum(colSums(sent6_sentiment)))
total_rec <- data.frame(prop = colSums(rec6_sentiment)/sum(colSums(rec6_sentiment)))
sent_sent <- data.frame(sentiment = colnames(sent6_sentiment), prop = total_sent)
sent_rec <- data.frame(sentiment = colnames(rec6_sentiment), prop = total_rec)

diff15 <- data.frame( sentiment = colnames(rec6_sentiment), diff = sent_sent$prop - sent_rec$prop )

ggplot( diff15[1:8,], aes(x= sentiment, weight = diff, fill=sentiment)) + geom_bar() +ggtitle("Sentiment Diff. for Topic 15 (UK)") + theme(plot.title = element_text(hjust = 0.5)) + xlab( 'Sentiment' ) + ylab( 'Proportion (Sent - Rec)' )







