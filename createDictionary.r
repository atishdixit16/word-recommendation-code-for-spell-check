library(tm)
library(BBmisc)
words <- read.table('words')
wordsSmall <- removePunctuation( apply(words,1,tolower) )
wordList <- as.list(wordsSmall)


convertWord <- function(word) {
	strsplit(word,NULL)[[1]]
}

covertedDictionary <- function(wordList) {
	len <- length(wordList)
	for (i in 1:len)
		wordList[[i]] <- convertWord ( wordList[[i]] )
	wordList
}

similarityCloseness <- function(word1 , word2) {
	closeness <- 0
	minLength <- min(length(word1) , length(word2))
	for (i in 1:minLength)
		if (word1[i] == word2[i])
			closeness = closeness + 1
	closeness
}


createVector <- function(word){
        k = length(letters)
        indices = NULL
        for(i in 1:length(word))
                indices = c(indices,which(letters==word[i]))
        vect = rep(0,k)
        vect[unique(indices)]=1
        vect
}

createFreqVector <- function(word){
        k = length(letters)
        indices = NULL
        for(i in 1:length(word))
                indices = c(indices,which(letters==word[i]))
        vect = rep(0,k)
	for (i in indices)
		vect[i] = vect[i] + 1 
        vect
}

vectorCloseness <- function(word1 , word2) {
	closeness <- 0
	v1 <- createVector(word1)
	v2 <- createVector(word2)
	k <- length(v1)
	for (i in 1:k) 
		if (v1[i] == v2[i])
                        closeness = closeness + 1
	closeness
}

vectorFreqCloseness <- function(word1 , word2, type) {
	v1 <- c ( createFreqVector(word1) , length(word1))
	v2 <- c ( createFreqVector(word2) , length(word2))
	k <- length(v1)
	if (type=='hamming') {
	closeness <- 0
		for (i in 1:k) 
			if (v1[i] == v2[i])
                        	closeness = closeness + 1
	}
	if (type=='eucl') {
		closeness <- 1 / sqrt ( sum((v1-v2)^2) )
	}
	if (type=='manhattan') {
		closeness <- 1 / sum(abs(v1-v2))
	}
	closeness
}

patternCloseness <- function(word1, word2) {
	n <- min(length(word1), length(word2))
	m <- max(length(word1), length(word2))
	if (length(word1) == n) {
		wordSmall <- word1
		wordBig <- word2
	} else {
		wordSmall <- word2
		wordBig <- word1
	}
	closeness=0
	i=1
	check <- FALSE
	patternMatched <- FALSE
	while(i<n) {
		for (k in 1:(n-(i-1))) {
			for(l in 1:(m-k)) {
				if (collapse( wordSmall[i:(i+k)] , sep="") == collapse (wordBig[l:(l+k)] , sep="")) {
					check <- TRUE
					patternMatched <- TRUE
					break
				} else if (l==(m-k)) {
					check <- FALSE
				}
			}
			if (check==FALSE) {
				i = i + k
				if (patternMatched==TRUE) {
					closeness <- closeness + k
					patternMatched <- FALSE
				}
				check <- FALSE
				break
			}
		}
	}
	closeness / length(word1)
}

findNearest <- function( word, dictionary=wordsSmall, closenessFunc=patternCloseness, suggestions=5) {
	word <- convertWord(word)
	if (length(word) > 2) {
		checks <- sample((length(word) - 1 ), 2)
		dictionary <- dictionary [  which ( grepl( collapse( word[c(checks[1], checks[1]+1)], sep=""),dictionary) | grepl(collapse( word[c(checks[2],checks[2]+1)], sep=""),dictionary) ) ]
	} else {
		dictionary <- dictionary [which( length(dictionary)>1 & length(dictionary)>=4 )] 
	}
	dictionary <- as.list(dictionary)
	dictionary <- covertedDictionary (dictionary)
	totalWords <- length(dictionary)
	closenessVector <- NULL
	for (i in 1:totalWords)
		closenessVector <- c(closenessVector, closenessFunc( word, dictionary[[i]]))
	similarIndices <- order(closenessVector, decreasing=TRUE)#which(closenessVector==max(closenessVector))
	similarWords <- list()
	top20 <- min(10,length(similarIndices))
	a = 1
	for (i in similarIndices[1:top20]) {
		similarWords[[a]] <- dictionary[[i]]
		a= a+1
	}
	simCloseness <- NULL
	for (i in 1:top20) 
		simCloseness <- c( simCloseness, similarityCloseness( word, similarWords[[i]] ) )
	ranks <- order(simCloseness, decreasing=TRUE)
	output <- list()
	a <- 1
	for (i in ranks[1:suggestions]) {
		output[[a]] <- similarWords[[i]]
		a = a + 1
	}
	for (i in 1:suggestions) 
		output[[i]] <- collapse ( output[[i]] , sep="" )
	output
}
