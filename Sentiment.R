
# Sentiment Analysis package
#install.packages("devtools")
require(devtools)

#install_url("http://www.omegahat.org/Rstem/Rstem_0.4-1.tar.gz")
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz") 
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

#install.packages("plyr")
library(plyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("wordcloud")
library(wordcloud)

#install.packages("RColorBrewer")
library(RColorBrewer)

#install.packages("tm")
library(tm)

#install.packages("SnowballC")
library(SnowballC)

data <- readLines("https://www.r-bloggers.com/wp-content/uploads/2016/01/vent.txt")

?readLines
View(data)

df <- data.frame(data)
View(df)

textdata <- df[df$data, ]

textdata = gsub("[[:punct:]]", "", textdata)

??gsub

textdata = gsub("[[:digit:]]", "", textdata)
textdata = gsub("http\\w+", "", textdata)
textdata = gsub("[ \t]{2,}", "", textdata)
textdata = gsub("^\\s+|\\s+$", "", textdata)
textdata


try.error = function(x)
{
y = NA
try_error = tryCatch(tolower(x), error=function(e) e)
if (!inherits(try_error, "error"))
  y = tolower(x)
return(y)
}


textdata = sapply(textdata, try.error)
textdata = textdata[!is.na(textdata)]

names(textdata) = NULL

#install.packages("sentimentr")
library(sentiment)

class_emo = classify_emotion(textdata, algorithm="bayes", prior=1.0)

?classify_emotion()

#######################Example ###############################

# LOAD LIBRARY
library(sentiment)

# DEFINE DOCUMENTS
documents <- c("I am very happy, excited, and optimistic.",
               "I am very scared, annoyed, and irritated.",
               "Iraq's political crisis entered its second week one step closer to the potential 
				dissolution of the government, with a call for elections by a vital coalition partner 
				and a suicide attack that extended the spate of violence that has followed the withdrawal 
				of U.S. troops.")

# CLASSIFY EMOTIONS
classify_emotion(documents,algorithm="bayes",verbose=TRUE, prior = 1.0)

emotion = class_emo[,7]

emotion[is.na(emotion)] = "unknown"

class_pol = classify_polarity(textdata, algorithm="bayes")

polarity = class_pol[,4]

sent_df = data.frame(text=textdata, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

View(sent_df)

sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

?within()


View(sent_df_1)

str(sent_df)
str(sent_df_1)


# Now that we have processed the comments, we can graph the emotions and polarities.

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="")



ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="")



# We now prepare the data for creating a word cloud.  
# This includes removing common English stop words.


emos = levels(factor(sent_df$emotion))
nemo = length(emos)

emo.docs = rep("", nemo)

for (i in 1:nemo)
{
  tmp = textdata[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

emo.docs = removeWords(emo.docs, stopwords("english"))

corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE,
                 title.size = 1.5)



















