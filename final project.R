tweets = subset(Tweets, Tweets$Language=='en')
install.packages("twitteR")
library(twitteR)
library(plyr)
library(stringr)

# function score.sentiment
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   # remove punctuation
                   sentence = gsub("[[:punct:]]", "", sentence)
                   # remove control characters
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   # remove digits?
                   sentence = gsub('\\d+', '', sentence)
                   
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   sentence = sapply(sentence, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress=.progress )
  
  # data frame with scores for each sentence
  scores.df = data.frame(text=sentences, score=scores)
  return(scores.df)
}

# import positive and negative words
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")
tweets$score= score.sentiment(tweets$Text, pos, neg, .progress='text')$score

tweets$very.pos = as.numeric(tweets$score >= 2)
tweets$very.neg = as.numeric(tweets$score <= -2)

# how many very positives and very negatives
numpos = sum(tweets$very.pos)
numneg = sum(tweets$very.neg)

# global score
global_score = round( 100 * numpos / (numpos + numneg) )

tweets$Time.Stamp = as.Date(gsub("T"," ", tweets$Universal.Time.Stamp))


ggplot(tweets, aes(x=Time.Stamp, y=Text, fill=score)) +
  geom_bar(stat='identity')


vals = (aggregate(Text ~ score +Time.Stamp,data = tweets, FUN=length))
vals$Text[vals$score<0] = -1*vals$Text[vals$score<0]
vals$pos[vals$Text>0 & vals$score >0] =1
vals$pos[vals$Text>0 &vals$score==0]=0
vals$pos[vals$Text<0]=-1

install.packages("scales")
ggplot(data = vals, aes(x = Time.Stamp, y = Text, fill=factor(pos))) + 
  geom_bar(stat = "identity", position="identity")+ scale_x_date(labels = date_format("%b-%d"),breaks = date_breaks("3 days"))

library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(slam)
corp <-Corpus(VectorSource(enc2utf8(as.character(paste(tweets$Text,collapse=" ")))))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp,FUN= function(x) removeWords(x, stopwords("english")))
corp = tm_map(corp, function(x) removeWords(x, c("isp","isps","amppure","amptitle","will","gonna","thing","just","like","wants","dear","want","doesnt","can","youre","let","dont","tell","net", "neutrality","netneutrality","RT", "amp")))

tdm <- TermDocumentMatrix(corp)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
pal=brewer.pal(6,"Dark2")
png("wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq,scale=c(4, 1.2),min.freq=1500,max.words=100, random.order=T,color=pal, rot.per=.15, vfont=c("sans serif","plain"))
dev.off()

