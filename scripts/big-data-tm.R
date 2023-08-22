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

acorp <- tm_map(abscorp, removePunctuation)
acorp <- tm_map(acorp, removeNumbers)
acorp <- tm_map(acorp, tolower)
acorp <- tm_map(acorp, removeWords, stopwords("en"))
#acorp <- tm_map(acorp, removeWords, c("the", "and", "are", "for", "from", "with", "to", "be", "being", "this", "that", "have", "has", "had", "use", "using", "can", "could", "will", "would", "should", "shall"))
acorp <- tm_map(acorp, stripWhitespace)

# abscorpch <- tm_map(abscorp, toSpace, "/")
# abscorpch <- tm_map(abscorp, toSpace, "\")
# abscorpch <- tm_map(abscorp, toSpace, "(")
# abscorpch <- tm_map(abscorp, toSpace, ")")
# abscorpch <- tm_map(abscorp, toSpace, "for")

tm::inspect(acorp)
dim(tm::inspect(acorp))

adtm <- DocumentTermMatrix(abscorp)
# adtm <- DocumentTermMatrix(abscorp, control = list(weighting = weightTfIdf))
tm::inspect(adtm[10:16, ] )
dim(adtm)
adtm$dimnames
length(adtm$dimnames)

bigdataf <- data.frame(as.matrix(adtm), stringsAsFactors=False)
dim(bigdataf)
length(names(bigdataf))

# adtmsparse = removeSparseTerms(adtm, 0.30)
# dim(adtmsparse)
# 
# adtmsparse$dimnames
# mean(adtmsparse)
# 
# findFreqTerms(adtmsparse, 30)
# length(findFreqTerms(adtmsparse, 30))
# dataf <- findFreqTerms(adtmsparse, 30)

bigdataf[, (names(bigdataf) %in% c('from'))]

bigdataframe <- subset(bigdataf, select = -c(about, the, and, are, from, with, being, this, that, have, has, use, using, can, could, will, would, should))
length(names(bigdataframe)) # 2970
bigdataframe <- bigdataframe[, 123:2970]
length(names(bigdataframe)) # 2848

# feature selection 
max(apply(bigdataframe, 2, mean)) # 4.780488
min(apply(bigdataframe, 2, mean)) # 0.02439024
mean(apply(bigdataframe, 2, mean)) # 0.06214031
(4.780488 + 0.02439024)/2 # average of max-mean and min-mean - 2.402439

bdf <- subset(bigdataframe, select = which(apply(bigdataframe, 2, mean) > 0.24))
length(names(bdf)) #106

# install.packages(c("FactoMinerR", "factoextra"))

# CORRESPONDENCE ANALYSIS

# https://www.clres.com/ca/pdepca01a.html
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/113-ca-correspondence-analysis-in-r-essentials/

library(FactoMineR)
library(factoextra)

cafit <- FactoMineR::CA(bdf, ncp=2)
get_eigenvalue(cafit)

fviz_screeplot(cafit, addlabels = TRUE, ylim = c(0, 50))
fviz_ca_biplot(cafit, repel = TRUE)

fviz_ca_row(cafit, col.row = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
fviz_ca_col(cafit, col.col = "contrib",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
# MANOVA

fit <- lm(healthcare ~ big + data + analytics, data = bigdataframe)
summary(fit)
anova(fit)

sum(names(bigdataframe) %in% c("data"))
sum(names(bigdataframe) %in% c("analytics"))

bigdataframe[, 'technology']
bigdataframe[, 'technologies']
bigdataframe[, 'techniques']
bigdataframe[, 'clinical']

bda <- subset(bigdataframe, select = c(big, data, analytics))
bdt <- subset(bigdataframe, select = c(big, data, technology, technologies, techniques))

head(bda)
head(bdt)

bdav <- rowSums(bda)
bdat <- rowSums(bdt)

bigdataframe$bda <- bdav
bigdataframe$bdt <- bdat

fit <- manova(cbind(healthcare, outcomes) ~ big + data + analytics, data = bigdataframe)
summary(fit)
anova(fit) # data significant 


fit <- manova(cbind(clinical, medical, disease, diseases, treatment, healthcare, outcomes) ~ big + data + analytics, data = bigdataframe)
summary(fit)
anova(fit) # data significant 

fit <- manova(cbind(clinical, medical, disease, diseases, treatment) ~ big + data + technology + techniques, data = bigdataframe)
summary(fit)
anova(fit) # data significant 


fit <- manova(cbind(clinical, medical, disease, diseases, treatment, healthcare, outcomes) ~ bda, data = bigdataframe)
summary(fit)
anova(fit)

fit <- manova(cbind(clinical, medical, disease, diseases, treatment, healthcare, outcomes) ~ bdt, data = bigdataframe)
summary(fit)
anova(fit)

library(psych)

fit <- fa(bigdataframe, 2)
loadings(fit)
# getwd()
write.csv(loadings(fit), 'bda-loadings.csv')
write.csv(fit$r.scores, 'bda-rscores.csv')

structure.diagram(fit)

sum(names(bigdataframe) %in% c("outcomes"))
bigdataframe [, c("decisions")]

bdadf <- bigdataframe[, c('big', 'data', 'analytics', 'technology', 'clinical', 'medical', 'social', 'business', 'management', 'security', 'risk', 'fraud', 'decisions', 'model', 'approach', 'patients', 'disease', 'healthcare')]
names(bdadf)
head(bdadf)

scree(bdadf) # 2/7 factor solutions (FA & PC)

fit <- fa(bdadf, 2, fm='wls')
loadings(fit)
# getwd()
write.csv(loadings(fit), 'bda-2fs-loadings.csv')
write.csv(fit$r.scores, 'bda-2fs-rscores.csv')

structure.diagram(fit)


fit <- fa(bdadf, 4, fm='wls')
loadings(fit)
# getwd()
write.csv(loadings(fit), 'bda-4fs-loadings.csv')
write.csv(fit$r.scores, 'bda-4fs-rscores.csv')

structure.diagram(fit)


fit <- fa(bdadf, 7, fm='wls')
loadings(fit)
# getwd()
write.csv(loadings(fit), 'bda-7fs-loadings.csv')
write.csv(fit$r.scores, 'bda-7fs-rscores.csv')

structure.diagram(fit)

