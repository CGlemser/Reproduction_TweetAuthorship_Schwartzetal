# Reproduction_TweetAuthorship_Schwartzetal
Attempt to reproduce authorship attribution as implemented in "Authorship Attribution of Micro Messages"
by Schwartz, Tsur, Rappoport and Koppel (2013) using R. This is unfinished / a work in progress.

Schwartz, Tsur, Rappoport and Koppel (2013) classified authors of tweets, using support vector classification.
They use word and character n-grams as well as flexible patterns as features. They also show that many authors
have a unique writing style, which can be identified by so-called k-signatures (features that appear in at least
k% of an author's training set, but in no set of any other author).

This implementation is so far focused on reproducing their feature extraction and SVM calculation, using R
instead of MATLAB, using the R packages "ngram" for feature extraction and "LiblineaR" for SVM estimation. 

Problems I have encountered so far:
1) I could not find the original tweet corpus (probably because of a change in Twitter's privacy regulations),
   so I used another corpus I found here: http://neuro.compute.dtu.dk/wiki/Sentiment140
2) That corpus included many authors with very few tweets, so I had to exclude a huge part of the data
3) The implementation is pretty slow so far (I think)
3) The paper uses meta tags, but it was unclear from the paper whether they should then also be considered part of
   the input string during n-gram extraction (in particular the START and END tags)
