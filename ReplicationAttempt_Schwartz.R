#----------------------------------------------------------------------------
# Stylometry 
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
# 5) Validation
# 6) k-signatures & flexible patterns 
#
# Claudia Glemser, last edit: 14/Jan/18
#----------------------------------------------------------------------------


#####===== 1) pre-processing =====#####

# create base data.frame
# set working directory
setwd("D:/GoogleDrive/Kolleg_Stylometry/01Meeting_Prep/Corpus/")

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

load("combineddata.RData")

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


## filter ~70% of tweets per author for training set
## => remaining 30% are validation/test data
# extract all tweets per author -> sample 70% of them
tweetsPA <- aggregate(tweet ~ author, dat, length)
tweetsPA$train <- round(tweetsPA[,2]*0.7)
tweetsPA$test  <- with(tweetsPA, tweet - train)
tweetsPA <- tweetsPA[order(as.character(tweetsPA$author)),]

auts <- tweetsPA$author

dat.train <- data.frame(author = rep(auts, tweetsPA$train), tweet = "",
												stringsAsFactors = FALSE)

dat.test <- data.frame(author = rep(auts, tweetsPA$test), tweet = "",
												stringsAsFactors = FALSE)

for(i in 1:length(auts)){
  tweets <- dat[dat$author == auts[i], "tweet"]
  ind <- sample(1:length(tweets), size = tweetsPA$train[i], replace = FALSE)
	train_tweets <- tweets[ind]
	test_tweets  <- tweets[-ind]
  dat.train[dat.train$author == auts[i], "tweet"] <- train_tweets
  dat.test[dat.test$author == auts[i], "tweet"] <- test_tweets
}

save(dat.train, file = "processedTrainData.RData")
save(dat.test, file = "processedTestData.RData")

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
# tweetsPA <- aggregate(tweet ~ author, dat, length)
# omit <- tweetsPA[tweetsPA$tweet > 500, "author"]
# dat <- droplevels(dat[dat$author != omit,])
## 926 authors, 72869 tweets

## following done with training data
dat <- dat.train

author.ngrams <- list(author = auts,
											word.ngrams = vector("list", length(auts)),
											char.ngrams = vector("list", length(auts)))
t_wng <- 2

## finds all tweets per author and looks for word n-grams (n = 2-5)
## all that are found at least twice are saved in the author.ngrams list
system.time(
for(i in 1:length(auts)){
	includePA <- c()
  aut <- auts[i]
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
# 44.01s

save(author.ngrams, "wordngrams_Train.RData")
## loads of authors with no word n-grams ... (not enough tweets per person?)


### character n-grams
system.time(
for(i in 1:length(auts)){
	aut <- auts[i]
  t_cng <- ifelse(tweetsPA[tweetsPA$author == aut, "train"] > 200, 4, 2)
  tweets <- dat[dat$author == aut, "tweet"]
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
# 123.93s

# transform ngrams back into their original form
system.time(
for(i in 1:length(auts)){
	# remove the space between the single characters
  charngramsPA <- as.vector(sapply(author.ngrams$char.ngrams[[i]],
																	 FUN = concatenate, rm.space = TRUE))
  # replace the underscores with a space
  author.ngrams$char.ngrams[[i]] <- gsub("_", " ", charngramsPA)
}
)
# 23.23s

## they all have loads of char n-grams
save(author.ngrams, file = "charandwordngrams_Train.RData")


#####===== 3) preparing data for SVM =====#####

# add word n-grams as columns
svm.dat <- data.frame(author = auts)
for(i in 1:length(auts)){
  svm.dat[author.ngrams$word.ngrams[[i]]] <- ifelse(svm.dat$author ==
																									 auts[i], 1, 0)
}

# add char n-grams as columns: !!!THIS TAKES A LONG TIME!!!
for(i in 1:length(auts)){
	aut <- auts[i]
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

length(unique(unlist(author.ngrams$word.ngrams)))  # 41
length(unique(unlist(author.ngrams$char.ngrams)))  # 35269

dim(svm.dat)
# 927 authors
# 35310 features (first col: author/target variable)

save(svm.dat, file = "SVM_TrainingData.RData")

#####===== 4) SVM calculation =====#####

## how did they split the data into testing and training data?
## -> sampling e.g. 2/3 of tweets within a single author or across all
##    authors?

library(LiblineaR)
cost.par <- heuristicC(as.matrix(svm.dat[,-1]))  # 0.0422
# in paper: 10-fold cross-validation on training set
# used heuristic here to save computational power
system.time(
svm.mod <- LiblineaR(svm.dat[,-1], svm.dat[,1], cost = cost.par)
)  # 63.78
# cross-validation could be entered here via "cross = 10"


#####===== 5) Validate with remaining data set =====#####
dat <- dat.test

author.ngrams <- list(author = auts,
											word.ngrams = vector("list", length(auts)),
											char.ngrams = vector("list", length(auts)))
t_wng <- 2

## finds all tweets per author and looks for word n-grams (n = 2-5)
## all that are found at least twice are saved in the author.ngrams list
system.time(
for(i in 1:length(auts)){
	includePA <- c()
  aut <- auts[i]
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
# 18.89s

### character n-grams
system.time(
for(i in 1:length(auts)){
	aut <- auts[i]
  t_cng <- ifelse(tweetsPA[tweetsPA$author == aut, "train"] > 200, 4, 2)
  tweets <- dat[dat$author == aut, "tweet"]
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
# 60.27s

# transform ngrams back into their original form
system.time(
for(i in 1:length(auts)){
	# remove the space between the single characters
  charngramsPA <- as.vector(sapply(author.ngrams$char.ngrams[[i]],
																	 FUN = concatenate, rm.space = TRUE))
  # replace the underscores with a space
  author.ngrams$char.ngrams[[i]] <- gsub("_", " ", charngramsPA)
}
)
# 8.89s

## they all have loads of char n-grams
save(author.ngrams, file = "charandwordngrams_Test.RData")

svm.test <- data.frame(author = auts)
svm.test[colnames(svm.dat)[-1]] <- 0

colnames(svm.dat)[2:42]  # all word n-grams
for(i in 1:length(auts)){
  wordgrams <- author.ngrams$word.ngrams[[i]]
	if(!is.null(wordgrams)){
	  gramsInclude <- wordgrams[wordgrams %in% colnames(svm.dat)[2:42]]
	  svm.test[gramsInclude] <- ifelse(svm.test$author == auts[i], 1, 0)
	}
}

for(i in 1:length(auts)){
  chargrams <- author.ngrams$char.ngrams[[i]]
  gramsInclude <- chargrams[chargrams %in% colnames(svm.dat)[-(2:42)]]
	svm.test[gramsInclude] <- ifelse(svm.test$author == auts[i], 1, 0)
}

### test predictions ###
system.time(
preds <- predict(svm.mod, svm.test[,-1])$predictions
)

sum(preds == svm.test[,1])/length(svm.test[,1])
# 33.12% of the authors are predicted correctly
# not too bad 


#####===== 6) k-signatures & flexible patterns =====#####

## calculation of k-signatures ##

# k-signatures: features that appear in at least k% of author a's training
# sample, while not appearing in the training set of any other author
colsums <- apply(svm.dat[,-1], 2, sum)
sum(colsums == 1)  # 17024 features seem to only be present in one author's
                   # set of tweets

## calculating flexible patterns ##
# --- #
