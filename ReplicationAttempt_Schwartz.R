#----------------------------------------------------------------------------
# Kolleg Stylometry (Sept17)
# Replication of "Author Attribution of Micro Messages
#
# DISCLAIMER: a different corpus of tweets was used, since the original
#             corpus of tweets seemed to be no longer available
# NOTE that this is also a work in progress
#
# 1) Pre-processing of the corpus
# 2) feature extraction
# 3) preparing data for SVM
# 4) SVM calculation
# 5) TO DO ...
#
# Claudia Glemser, last edit: 17/Sept/17
#----------------------------------------------------------------------------


#####===== 1) pre-processing =====#####

# create base data.frame
# set working directory
# setwd("D:/GoogleDrive/Kolleg_Stylometry/01Meeting_Prep/Corpus/")

# read both datasets and extract author & tweet
dat.trai <- read.csv("training.1600000.processed.noemoticon.csv",
										 header = FALSE)
dat.trai <- dat.trai[,5:6]
colnames(dat.trai) <- c("author", "tweet")
dat.trai$tweet <- as.character(dat.trai$tweet)

dat.test <- read.csv("testdata.manual.2009.06.14.csv", header = FALSE)
dat.test <- dat.test[,5:6]
colnames(dat.test) <- c("author", "tweet")
dat.test$tweet <- as.character(dat.test$tweet)

# combine those and save them in a format that can be load more quickly
dat.all <- rbind(dat.test, dat.trai)
save(dat.all, file = "combineddata.RData")

# look at our data
tweetsPA <- aggregate(tweet ~ author, dat.all, length)
# we have many authors: 660120

# exclude those with less than 50 tweets (least no of tweets used in paper)
omit <- tweetsPA[tweetsPA$tweet %in% c(1:49), "author"]
dat <- droplevels(dat.all[!(dat.all$author %in% omit),])
str(dat)
# 927 authors w/ at least 50 tweets
# 74589 tweets overall
## (considerably smaller set than the original one) ##

### pre-processing of tweet strings: omits and metatags
# include START and END tag
dat$tweet <- gsub("^", "START ", dat$tweet)
dat$tweet <- gsub("$", " END", dat$tweet)

## replace all @<user> with "REF"
dat$tweet <- gsub('@+\\S+', "REF", dat$tweet)
# replace @+all non-space characters with REF

## replace all web adresses with "URL"
# replace http:// or www.+all non-space characters with 'URL'
dat$tweet <- gsub('http://+\\S+', 'URL', dat$tweet)
dat$tweet <- gsub('www\\.\\w{1}\\S+', 'URL', dat$tweet)

## replace time of the day with "TIME"
dat$tweet <- gsub("\\d{1,2}:\\d{2}:?\\d{0,2}", "TIME", dat$tweet)

## replace year with "DATE"
dat$tweet <- gsub("2009", "DATE", dat$tweet)
# in paper: replace dates with DATE, but what does this include?
# -> specified differently by everyone, so I restricted it to changing only
# the year 2009

## replaces all numbers with "NUM"
# -> again not closely specified: I have decided on all numbers >= 2 digits,
#    surrounded by blank spaces
dat$tweet <- gsub("\\s\\d\\d*\\s", " NUM ", dat$tweet)

## exclude all tweets with too few words
omit <- vapply(strsplit(dat$tweet, "\\S+"), length, integer(1)) < 5
dat <- droplevels(dat[!omit,])
## 71198 tweets overall

# for character n-grams we want only a space at the beginning and end
dat$tweet.c <- gsub("^START", " ", dat$tweet)
dat$tweet.c <- gsub("END$", " ", dat$tweet.c)



#####===== 2) feature extraction =====#####

library(ngram)  # include citation
# pretty quick (works via C)
# downside: also seems to split when there are two spaces


## NOTE TO SELF ##0
# extract the n-grams from all tweets by one author for all authors
# every char n-gram that can be found at least t_cng times is a feature
# every word n-gram that can be found at least t_wng times is a feature


## PROBLEMS ##
# -> it's unclear from the paper whether START END etc. also count as a
#    word
# -> it's also unclear how to deal with the other tags in char-ngrams etc.


### word n-grams
## HOW IT IS DONE IN THE PAPER ##
# subset_tweets t_cng t_wng
# 50            2     2 
# 100           2     2
# 200           4     2
# 500           10    3
# 1000          20    5
# we'll use two for both here, cause we have a small subset for most authors

# exclude the author w/ > 500 tweets
tweetsPA <- aggregate(tweet ~ author, dat, length)
omit <- tweetsPA[tweetsPA$tweet > 500, "author"]
dat <- droplevels(dat[dat$author != omit,])
## 926 authors, 72869 tweets
save(dat, file = "processeddata.RData")

authors <- unique(dat$author)

author.ngrams <- list(author = authors,
											word.ngrams = vector("list", length(authors)),
											char.ngrams = vector("list", length(authors)))
t_wng <- 2

## finds all tweets per author and looks for word n-grams (n = 2-5)
## all that are found at least twice are saved in the author.ngrams list
system.time(
for(i in 1:length(authors)){
	includePA <- c()
  aut <- authors[i]
  tweets <- dat[dat$author == aut, "tweet"]
		
  for(no in 2:5){
		word.ngrams <- get.ngrams(ngram(tweets, n = no, sep = "~ "))
		include <- table(word.ngrams) >= t_wng
	  includePA <- c(names(table(word.ngrams)[include]), includePA)
	}
	if(!identical(includePA, character(0))){
	  author.ngrams$word.ngrams[[i]] <- includePA
	}
}
)

save(author.ngrams, "wordngrams.RData")
## loads of authors with no word n-grams ... (not enough tweets per person?)


### character n-grams
system.time(
for(i in 1:length(authors)){
	aut <- authors[i]
  t_cng <- ifelse(tweetsPA[tweetsPA$author == aut, 2] > 200, 4, 2)
  tweets <- dat[dat$author == aut, "tweet.c"]
	# split the tweets into single character so that each character
	# constitutes a word (spaces become an underscore)
	char.tab <- data.frame(ngrams = "TEST", freq = -1)
	for(j in 1:length(tweets)){
	  tweet.sp <- splitter(tweets[j], split.char = TRUE, split.space = TRUE)
	  # lists the frequency of each ngram: only include those appearing > 2
	  char.tab.temp <- get.phrasetable(ngram(tweet.sp, n = 4, sep = " "))[,1:2]
    char.tab <- rbind(char.tab.temp, char.tab)
	}
	char.tab <- aggregate(freq ~ ngrams, char.tab, sum)
	include <- char.tab[2] >= t_cng
	
	# save them in the overall list
	if(!identical(include, character(0))){
	  author.ngrams$char.ngrams[[i]] <- char.tab[include, 1]
	}
}
)

# transform ngrams back into their original form
system.time(
for(i in 1:length(authors)){
	# remove the space between the single characters
  charngramsPA <- as.vector(sapply(author.ngrams$char.ngrams[[i]],
																	 FUN = concatenate, rm.space = TRUE))
  # replace the underscores with a space
  author.ngrams$char.ngrams[[i]] <- gsub("_", " ", charngramsPA)
}
)

## they all have loads of char n-grams
save(author.ngrams, file = "charandwordngrams.RData")



#####===== 3) preparing data for SVM =====#####

# add word n-grams as columns
svm.dat <- data.frame(author = authors)
for(i in 1:length(authors)){
  svm.dat[author.ngrams$word.ngrams[[i]]] <- ifelse(svm.dat$author ==
																									 authors[i], 1, 0)
}

# add char n-grams as columns
for(i in 1:length(authors)){
	aut <- authors[i]
  chargramsPA <- author.ngrams$char.ngrams[[i]]
  # introduce new char n-grams as new columns
	newgrams <- chargramsPA[!chargramsPA %in% colnames(svm.dat)]
  svm.dat[newgrams] <- ifelse(svm.dat$author == aut, 1, 0)
	
	# add ones for the already existent grams
	usedgrams <- chargramsPA[chargramsPA %in% colnames(svm.dat)]
	for(j in 1:length(usedgrams)){
	  svm.dat[svm.dat$author == aut, colnames(svm.dat) == usedgrams[j]] <- 1
	}
}

dim(svm.dat)
# 926 authors
# 43874 features



#####===== 4) SVM calculation =====#####

## how did they split the data into testing and training data?
## -> sampling e.g. 2/3 of tweets within a single author or across all
##    authors?

library(LiblineaR)
cost.par <- heuristicC(as.matrix(svm.dat[,-1]))  # 0.0362
# in paper: 10-fold cross-validation on training set
# used heuristic here to save computational power

svm.mod <- LiblineaR(svm.dat[,-1], svm.dat[,1], cost = cost.par)
# cross-validation could be entered here via "cross = 10"

preds <- predict(svm.mod, svm.dat[,-1])$predictions
sum(preds == svm.dat[,1])/length(svm.dat[,1])
# svm.dat should here be another testing data set (will fix that asap)

# -> I forgot to split the whole dataset into a training and test
#    data set before the whole preprocessing, feature extraction ...
# -> since the whole process takes quite some time up to here, I did not yet
#    have time to fix it, but I'll change it asap and it doesn't change much
#    in the actual code



#####===== 5) SVM calculation =====#####

## proper calculation of k-signatures ##

# k-signatures: features that appear in at least k% of author a's training
# sample, while not appearing in the training set of any other author
colsums <- apply(svm.dat[,-1], 2, sum)
sum(colsums == 1)  # 21624 features seem to only be present in one author's
                   # set of tweets


## calculating flexible patterns ##
