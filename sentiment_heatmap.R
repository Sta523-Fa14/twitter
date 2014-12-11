library(plyr)
library(stringr)
library(sqldf)

# Reading collections of positive and negative words stored in the form .csv files 
pos = readLines("positive_words.txt")
neg = readLines("negative_words.txt")

# Reading a database of US cities, states and corresponding geo coordinates
#US_cities <- read.csv("US_cities.csv")
US_cities$state_code = tolower(US_cities$state_code)
US_cities$state = tolower(US_cities$state)
US_cities$city = gsub("[^[:alnum:]]","", US_cities$city)
US_cities$city = tolower(US_cities$city)
US_cities$city_state <- paste(US_cities$city, US_cities$state, sep = "")
US_cities$city_state_code <- paste(US_cities$city, US_cities$state_code, sep = "")

# Creating aggregated subsets of US city & state database to help match with various types of locations provided by Twitter users
US_states <- sqldf("select state,state_code,state_code st_cd_dup,avg(lat) lat,avg(lng) lng from US_cities group by state,state_code")
US_city_state <- sqldf("select city_state,state_code, avg(lat) lat,avg(lng) lng from US_cities group by city_state")
US_city_state_code <- sqldf("select city_state_code,state_code, avg(lat) lat,avg(lng) lng from US_cities group by city_state_code")

# Reading tweets.csv file 
tweets <- read.csv("tweets.csv")
# Subset tweets to conisder only English tweets
tweets = subset(tweets, tweets$Language=='en')

# Filtering data based on 4 US timezones
tweets_us_zones <- tweets[tweets$Time.Zone %in% c("Eastern Time (US & Canada)", "Central Time (US & Canada)", "Mountain Time (US & Canada)", "Pacific Time (US & Canada)"), ]

# Purging the location data 
tweets_us_zones$loc_for_geo = gsub("[^[:alnum:]]","", tweets_us_zones$Location)
tweets_us_zones$loc_for_geo = as.character(tweets_us_zones$loc_for_geo)
tweets_us_zones$loc_for_geo = tolower(tweets_us_zones$loc_for_geo)

# The locations provided by Twitter users are being compared with four different combinations of US cities and states
m1 <- merge(tweets_us_zones, US_city_state, by.x = "loc_for_geo", by.y = "city_state", all=FALSE)
m2 <- merge(tweets_us_zones, US_city_state_code, by.x = "loc_for_geo", by.y = "city_state_code", all=FALSE)
m3 <- merge(tweets_us_zones, US_states, by.x = "loc_for_geo", by.y = "state", all=FALSE)
m4 <- merge(tweets_us_zones, US_states, by.x = "loc_for_geo", by.y = "st_cd_dup", all=FALSE)

# Subsetting the required columns
m1 <- m1[,c('ID', 'Text', 'Location', 'state_code', 'Time.Zone', 'lat', 'lng')]
m2 <- m2[,c('ID', 'Text', 'Location', 'state_code', 'Time.Zone', 'lat', 'lng')]
m3 <- m3[,c('ID', 'Text', 'Location', 'state_code', 'Time.Zone', 'lat', 'lng')]
m4 <- m4[,c('ID', 'Text', 'Location', 'state_code', 'Time.Zone', 'lat', 'lng')]

# Union operation on all four merge sets m1 to m4
data <- rbind(m1,m2,m3,m4)
data$state_code <- toupper(data$state_code)
colnames(data) <- c('Id', 'Text', 'Location', 'State_code', 'Time_zone', 'Lat', 'Lng')

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

# Calling function score.sentiment
data$Sentiment_Score = score.sentiment(data$Text, pos, neg, .progress='text')$score

# Collecting average positive and negative scores per state to present in the form of a heatmap
positive_data_for_heatmap <- sqldf("select round(avg(Sentiment_Score),100) Sentiment_Score, avg(Lat) Lat, avg(Lng) Lng, State_code from 
                                   (select Sentiment_Score, Lat, Lng, State_code from data where Sentiment_Score >2) group by State_code ")
negative_data_for_heatmap <- sqldf("select round(avg(Sentiment_Score),100) Sentiment_Score, avg(Lat) Lat, avg(Lng) Lng, State_code from 
                                   (select Sentiment_Score, Lat, Lng, State_code from data where Sentiment_Score <-2) group by State_code ")


library(maps)
library(maptools)
library(sp)

mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))


positive_data_for_heatmap$abrv <- tolower(state.name[match(positive_data_for_heatmap$State_code,  state.abb)])
positive_data_for_heatmap= positive_data_for_heatmap

idx <- match(unique(nms), positive_data_for_heatmap$abrv)
dat2 <- data.frame(value = positive_data_for_heatmap$Sentiment_Score[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'])


negative_data_for_heatmap$abrv <- tolower(state.name[match(negative_data_for_heatmap$State_code,  state.abb)])
negative_data_for_heatmap= negative_data_for_heatmap

idx <- match(unique(nms),negative_data_for_heatmap$abrv)
dat2 <- data.frame(value = negative_data_for_heatmap$Sentiment_Score[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'])

