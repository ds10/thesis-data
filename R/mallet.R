subreds<-c("truegaming","gaming")

documents<- data.frame(
text=character(), 
id=character(), 
subredit=character(), 
stringsAsFactors=FALSE)

time="week"

for(loop in 1:length(subreds)){

print(subreds[loop])

if (subreds[loop] == 'allTop') {
url <- paste('http://www.reddit.com/top/?sort=top&t=', time, sep = "")
} else { 
url <- paste("http://www.reddit.com/r/", subreds[loop], "/top/?sort=top&t=", time, sep = "")
}

doc <- htmlParse(url)

#######################################################
# 2. Get the links that go to comment sections of the posts

links <- xpathSApply(doc, "//a/@href")
comments <- grep("comments", links)
comLinks <- links[comments]
comments <- grep('reddit.com', comLinks)
comLinks <- comLinks[comments]

#######################################################
# 3. Scrape the pages
# This will scrape a page and put it in to 
# an R list object 

textList <- as.list(rep(as.character(""), length(comLinks))) 
docs <- getURL(comLinks)
for (i in 1:length(docs)) {
textList[[i]] <- htmlParse(docs[i], asText = TRUE)
textList[[i]] <- xpathSApply(textList[[i]], "//p", xmlValue)
}

#######################################################
# 4. Clean up the text.

# Remove the submitted lines and lines at the end of each page
for (i in 1:length(textList)) {
submitLine <- grep("submitted [0-9]", textList[[i]]) 
textList[[i]] <- textList[[i]][{(submitLine[1] + 1):(length(textList[[i]])-10)}]
}

# Removing lines capturing user and points, etc.
# Yes, there could be fewer grep calls, but this made it 
# easier to keep track of what was going on.
for (i in 1:length(textList)) { 
grep('points 1 minute ago', textList[[i]]) -> nameLines1
grep('points [0-9] minutes ago', textList[[i]]) -> nameLines2
grep('points [0-9][0-9] minutes ago', textList[[i]]) -> nameLines3
grep("points 1 hour ago", textList[[i]]) -> nameLines4
grep("points [0-9] hours ago", textList[[i]]) -> nameLines5
grep("points [0-9][0-9] hours ago", textList[[i]]) -> nameLines6
grep('points 1 day ago', textList[[i]]) -> nameLines7
grep('points [0-9] days ago', textList[[i]]) -> nameLines8
grep('points [0-9][0-9] days ago', textList[[i]]) -> nameLines9
grep('points 1 month ago', textList[[i]]) -> nameLines10
grep('points [0-9] months ago', textList[[i]]) -> nameLines11
grep('points [0-9][0-9] months ago', textList[[i]]) -> nameLines12
allLines <- c(nameLines1, nameLines2, nameLines3, nameLines4, 
nameLines5, nameLines6, nameLines7, nameLines8, nameLines9, 
nameLines10, nameLines11, nameLines12)
textList[[i]] <- textList[[i]][-allLines]
textList[[i]] <- textList[[i]][textList[[i]]!=""]
textList[[i]] <- tolower(textList[[i]])
}

# Let's simplify our list. Could have been done earlier, but so it goes. 
allText <- unlist(textList)

# Remove the punctuation, links, etc.
allText <- gsub("https?://[[:alnum:][:punct:]]+", "", allText)
allText <- gsub("[,.!?"]", "", allText)
#allText <- strsplit(allText, "\W+", perl=TRUE)
rm(alldocuments)
alldocuments<- data.frame(
text= allText, 
stringsAsFactors=FALSE)

alldocuments$text <- allText
alldocuments$id <- make.unique(subreds[loop]) 
alldocuments$subred <- subreds[loop]

documents <- rbind( alldocuments, documents)
}

require(mallet)
mallet.instances <- mallet.import( documents$text , documents$text , "en.txt", token.regexp = "\p{L}[\p{L}\p{P}]+\p{L}")

## Create a topic trainer object.
n.topics <- 30
topic.model <- MalletLDA(n.topics)

#loading the documents
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
## These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations, 
## after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
## We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(500)

## NEW: run through a few iterations where we pick the best topic for each token, 
## rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
## so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)

## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=3)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels

# create data.frame with columns as authors and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

require(reshape2)
require(ggplot2)

topic_docs_t <- data.frame(t(topic_docs))
topic_docs_t$thread <- documents$subred
df3 <- aggregate(topic_docs_t, by=list(topic_docs_t$thread), FUN=mean)
df3 <- data.frame(t(df3[-32,-length(df3)]), stringsAsFactors = FALSE)
names(df3) <- c("truegaming","gaming")
df3 <- df3[-1,]
df3 <- data.frame(apply(df3, 2, as.numeric, as.character))
df3$topic <- 1:n.topics

# which topics differ the most?
df3$diff <- df3[,1] - df3[,2] 
df3[with(df3, order(-abs(diff))), ]

# plot
df3m <- melt(df3[,-4], id = 3)
ggplot(df3m, aes(fill = as.factor(topic), topic, value)) +
geom_bar(stat="identity") +
coord_flip() +
facet_wrap(~ variable)

## cluster based on shared words
plot(hclust(dist(topic.words)), labels=topics.labels)