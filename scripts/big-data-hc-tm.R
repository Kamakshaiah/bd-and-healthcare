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
acorp <- tm_map(acorp, content_transformer(tolower))
acorp <- tm_map(acorp, removeWords, stopwords("en"))
acorp <- tm_map(acorp, stripWhitespace)

# abscorpch <- tm_map(abscorp, toSpace, "/")

tm::inspect(acorp)
dim(tm::inspect(acorp))

adtm <- DocumentTermMatrix(abscorp)
# adtm <- DocumentTermMatrix(abscorp, control = list(weighting = weightTfIdf))
tm::inspect(adtm[10:16, ] )
dim(adtm)
adtm$dimnames[[1]]
length(adtm$dimnames[[2]])

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

# sum(names(bigdataframe) %in% c("data"))
# sum(names(bigdataframe) %in% c("analytics"))
# 
# bigdataframe[, 'technology']
# bigdataframe[, 'technologies']
# bigdataframe[, 'techniques']
# bigdataframe[, 'clinical']
# 
# bda <- subset(bigdataframe, select = c(big, data, analytics))
# bdt <- subset(bigdataframe, select = c(big, data, technology, technologies, techniques))
# 
# head(bda)
# head(bdt)
# 
# bdav <- rowSums(bda)
# bdat <- rowSums(bdt)
# 
# bigdataframe$bda <- bdav
# bigdataframe$bdt <- bdat

bdf[, 'business']

# q0 - healthcare 

library(heplots)
library(psych)

fit <- lm(cbind(clinical, medical, disease, diseases, treatment, healthcare, outcomes) ~ big + data + analytics + technology, data = bdf)
summary(fit)

path <- file.path("D:/Research/papers/big data/healthcare/plots/regression")
setwd(path)
getwd()
dir.create('q0'); list.dirs()
setwd(file.path(path, 'q0')); getwd()
path <- getwd()
sink(file.path(path, 'q0-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q0-lm-anova.csv')
# update(fit, clinical ~ .)
# lmDiagram(update(fit, clinical ~ .))

fit <- manova(cbind(clinical, medical, disease, diseases, treatment, healthcare, outcomes) ~ big + data + analytics + technology, data = bdf)
summary(fit) 

sink(file.path(path, 'q0-manova-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q0-manova-anova.csv')
pairs(fit)

# q-1: design, approach, model, 

fit <- lm(cbind(design, approach, model) ~ big + data + analytics + technology, data = bdf) # intercept is significant 
summary(fit)

path <- file.path("D:/Research/papers/big data/healthcare/plots/regression")
getwd()
pathch <- file.path(path, 'q1')
setwd(pathch); getwd()
sink(file.path(path, 'q1-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q1-lm-anova-table.csv')

fit <- manova(cbind(design, approach, model) ~ big + data + analytics, data = bdf)
summary(fit) 
sink(file.path(pathch, 'q1-manova-anova.csv'))
print(summary(fit))
sink()

#q2 - electronic business

bdf[, 'technology']

fit <- lm(cbind(electronic, business) ~ big + data + analytics + technology, data = bdf)
summary(fit)

dir.create(file.path(path, 'q2')); list.dirs(path)
pathch <- file.path(path, 'q2')

setwd(pathch); getwd()
sink(file.path(path, 'q2-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q2-lm-anova-table.csv')

fit <- manova(cbind(electronic, business) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

sink(file.path(pathch, 'q2-manova-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q2-manova-anova-table.csv')

heplot(fit)

#q3 - cloud technology

bdf[, 'technology']

fit <- lm(cloud ~ big + data + analytics + technology, data = bdf)
summary(fit)
lmDiagram(fit)

pathch <- file.path(path, 'q3')

setwd(pathch); getwd()
sink(file.path(pathch, 'q3-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q3-lm-anova-table.csv')

fit <- manova(cbind(cloud, electronic, business) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

sink(file.path(pathch, 'q3-manova-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q3-manova-anova-table.csv')
pairs(fit)

#q4 - cloud & iot in business 

# bdf[, '5G']
# no patterns found in the data

# q5 - sql, database engineering 

# bdf[, 'base']
# no patterns found 

# q6 - knowledge engineering

bdf[, 'engineering']

fit <- lm(knowledge  ~ big + data + analytics + technology, data = bdf)
summary(fit)
lmDiagram(fit)

pathch <- file.path(path, 'q6')
setwd(pathch); getwd()
sink(file.path(pathch, 'q6-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q6-lm-anova-table.csv')

# q7- data mining & data extraction 

# bdf[, 'data']
# not found 

# q8 - ml & ebusiness 

bdf[, 'machine']; 
bdf[, 'learning']

fit <- lm(cbind(machine, learning, electronic, business) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

pathch <- file.path(path, 'q8')
setwd(pathch); getwd()

sink(file.path(pathch, 'q8-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q8-lm-anova-table.csv')

fit <- manova(cbind(machine, learning, electronic, business) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

pairs(fit)

# q9 - Security, privacy, and trust in E-business

bdf[, 'fraud']

fit <- lm(cbind(security, risk, electronic, business) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

pathch <- file.path(path, 'q9')
setwd(pathch); getwd()

sink(file.path(pathch, 'q9-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q9-lm-anova-table.csv')

pairs(fit)

# q10 - AI-based models for decision-making in business

bdf[, 'model']

fit <- lm(cbind(decision, model, business) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

pathch <- file.path(path, 'q10')
setwd(pathch); getwd()

sink(file.path(pathch, 'q10-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q10-lm-anova-table.csv')

pairs(fit)

# q-11 - Reliability and trustworthiness of the AI-based decision-making models

# no queries

# q12 - ai, business applications

bdf[, 'applications']

fit <- lm(cbind(business, applications) ~ big + data + analytics + technology, data = bdf)
summary(fit) # data significant 

pathch <- file.path(path, 'q12')
setwd(pathch); getwd()

sink(file.path(pathch, 'q12-lm-summary.csv'))
print(summary(fit))
sink()
write.csv(anova(fit), 'q12-lm-anova-table.csv')

par(mfrow = c(1, 2))
lmDiagram(update(fit, business ~ .))
lmDiagram(update(fit, applications ~ .))



# factor analysis 

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

