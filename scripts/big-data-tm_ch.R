# http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
# https://stackoverflow.com/questions/51942767/r-tm-error-of-transformation-drops-documents
# https://rstudio-pubs-static.s3.amazonaws.com/132792_864e3813b0ec47cb95c7e1e2e2ad83e7.html
# data input 

abstracts <- read.csv(file.choose(), encoding="UTF-8")
abs <- abstracts$Abstract
class(abs)
dim(abs)
length(abs)
head(abs)
abs[1]

# text mining 

library('tm')

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

abscorp <- VCorpus(VectorSource(abs))

#abscorpch <- tm_map(abscorp, toSpace, "/")
#abscorpch <- tm_map(abscorp, toSpace, "\")
#abscorpch <- tm_map(abscorp, toSpace, "(")
#abscorpch <- tm_map(abscorp, toSpace, ")")
#abscorpch <- tm_map(abscorp, toSpace, "the")
# "and", "are", "for", "from", "with", "to", "be", "being", "this", "that", 
# "have", "has", "had", "use", "using", "can", "could", "will", "would", 
# "should", "shall"

abscorpch <- tm_map(abscorp, toSpace, "and")
abscorpch <- tm_map(abscorpch, toSpace, "are")
abscorpch <- tm_map(abscorpch, toSpace, "for")
abscorpch <- tm_map(abscorpch, toSpace, "from")
abscorpch <- tm_map(abscorpch, toSpace, "with")
abscorpch <- tm_map(abscorpch, toSpace, "to")
abscorpch <- tm_map(abscorpch, toSpace, "be")
abscorpch <- tm_map(abscorpch, toSpace, "be")
abscorpch <- tm_map(abscorpch, toSpace, "being")
abscorpch <- tm_map(abscorpch, toSpace, "this")
abscorpch <- tm_map(abscorpch, toSpace, "that")
abscorpch <- tm_map(abscorpch, toSpace, "have")
abscorpch <- tm_map(abscorpch, toSpace, "has")
abscorpch <- tm_map(abscorpch, toSpace, "had")
abscorpch <- tm_map(abscorpch, toSpace, "use")
abscorpch <- tm_map(abscorpch, toSpace, "using")
abscorpch <- tm_map(abscorpch, toSpace, "can")
abscorpch <- tm_map(abscorpch, toSpace, "could")
abscorpch <- tm_map(abscorpch, toSpace, "will")
abscorpch <- tm_map(abscorpch, toSpace, "would")
abscorpch <- tm_map(abscorpch, toSpace, "should")
abscorpch <- tm_map(abscorpch, toSpace, "shall")

abscorpch  <- tm_map(abscorpch , removePunctuation)
abscorpch  <- tm_map(abscorpch , removeNumbers)
abscorpch  <- tm_map(abscorpch , tolower)
abscorpch  <- tm_map(abscorpch , removeWords, stopwords("english"))
#acorp <- tm_map(abscorpch, removeWords, c("the", "and", "are", "for", "from", "with", "to", "be", "being", "this", "that", "have", "has", "had", "use", "using", "can", "could", "will", "would", "should", "shall"))
abscorpch  <- tm_map(abscorpch , stripWhitespace)

tm::inspect(abscorpch)
dim(tm::inspect(abscorpch))

adtm <- DocumentTermMatrix(abscorpch)
# adtm <- DocumentTermMatrix(abscorp, control = list(weighting = weightTfIdf))
tm::inspect(adtm[10:16, ] )
dim(adtm)
adtm$dimnames

adtmsparse = removeSparseTerms(adtm, 0.99)
dim(adtmsparse)

adtmsparse$dimnames
mean(adtmsparse)

findFreqTerms(adtmsparse, 30)
length(findFreqTerms(adtmsparse, 30))
dataf <- findFreqTerms(adtmsparse, 30)

# correspondence analysis 