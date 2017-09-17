# Reproduction_TweetAuthorship_Schwartzetal
Attempt to reproduce authorship attribution as implemented in "Authorship Attribution of Micro Messages"
by Schwartz, Tsur, Rappoport and Koppel (2013) using R.

Schwartz, Tsur, Rappoport and Koppel (2013) classified authors of tweets, using support vector classification.
They use word and character n-grams as well as flexible patterns as features. They also show that many authors
have a unique writing style, which can be identified by so-called k-signatures (features that appear in at least
k% of an author's training set, but in no set of any other author).

This implementation is so far focused on reproducing their feature extraction and SVM calculation, using R
instead of MATLAB, using the R packages "ngram" for feature extraction and "LiblineaR" for SVM estimation. 

Problems I have encountered so far were numerous:
1) I could not find the original tweet corpus (probably because of a change in Twitter's privacy regulations),
   so I used another corpus I found here: http://neuro.compute.dtu.dk/wiki/Sentiment140
2) While that corpus was better than no corpus, it included many authors with very few tweets, so I had to
   exclude a huge part of the data
3) This however also turned out to be a blessing, cause my implementation is very slow (maybe using
   another language could help and you should probably parallelize part of the process, but I do not yet know
   how I would go about doing that)
3) The paper uses meta tags, but it was unclear to me whether they should then also be considered part of the
   input string during n-gram extraction (in particular the START and END tag)
   
This is still nowhere near finished, I'm sorry! :( I am new to machine learning and to working with that
large an amount of data and every single step of the way took me a lot longer than I had anticipated.

I have also forgotten to split the whole dataset into a training and a testing set at the very beginning,
so I have to run through everything again with just the training subset of the data (I haven't had  the
time to do that so far, but will work on it asap).
