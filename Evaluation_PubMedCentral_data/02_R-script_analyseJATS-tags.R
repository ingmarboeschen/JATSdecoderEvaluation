###################################################################################
## JATSdecoder: Insights to PubMed Central’s Open Access Database (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#################################################################################

########################################################################
## This script reproduces tables and graphics presented in the article.
#####################################################################

# clean work space
rm(list=ls())

# set working directory to folder containing the extracted tags only
setwd("./PMC/tags") # adjust to your system
setwd("/home/ingmar/JATSdecoderEvaluation/JATSdecoder/JATSdecoder - Language Resources and Evaluation/tags")

##################
## Start analysis 
#################

#################################################
### 3.1 Summary and NISO-JATS tag use over time
###############################################
# load all tags extracted data by script '01_extractJATSfromPMC.R' tag-wise
tags <- list.files(patt = "rda$", full.names = T)
for(i in 1:length(tags)){
  load(tags[i])
  # progress
  print(paste(tags[i]))
}
# objects in work space
ls()

## total number of articles
length(pubyear)
## total number of journals 
length(unique(unlist(journal)))

## Table 1: Relative frequency of extracted tag use over time
# extract use per tag
abstractUse    <- unlist(lapply(abstract, function(x) sum(nchar(x), na.rm = T) >= 30  &!  is.na(x[1])))
affiliationUse <- unlist(lapply(affiliation, function(x) sum(nchar(x), na.rm=T) >= 5 &! is.na(x[1])))
authorUse      <- unlist(lapply(author, function(x) sum(nchar(x), na.rm=T) >= 2 &! is.na(x[1])))
countryUse     <- unlist(lapply(country, function(x) sum(nchar(x), na.rm=T) >= 2 &! is.na(x[1])))
doiUse         <- unlist(lapply(doi, function(x) sum(nchar(x), na.rm=T) >= 15 &! is.na(x[1])))
editorUse      <- unlist(lapply(editor, function(x) sum(nchar(x), na.rm=T) >= 5 &! is.na(x[1])))
historyUse     <- unlist(lapply(history, function(x) sum(nchar(x), na.rm=T) >= 4 &! is.na(x[1])))
journalUse     <- unlist(lapply(journal, function(x) sum(nchar(x), na.rm=T) >= 2 &! is.na(x[1])))
keywordsUse    <- unlist(lapply(keywords, function(x) sum(nchar(x), na.rm=T) >= 3 &! is.na(x[1])))
pubyearUse     <- unlist(lapply(pubyear, function(x) sum(nchar(x), na.rm=T)==4 &! is.na(x[1])))
referencesUse  <- unlist(lapply(references, function(x) sum(nchar(x), na.rm=T) >= 15 &! is.na(x[1])))
sectionsUse    <- unlist(lapply(sections, function(x) sum(nchar(x), na.rm=T) >= 5 &! is.na(x[1])))
subjectUse     <- unlist(lapply(subject, function(x) sum(nchar(x), na.rm=T) >= 5 &! is.na(x[1])))
textUse        <- unlist(lapply(text, function(x)  sum(nchar(x), na.rm=T) >= 15 &! is.na(x[1])))
titleUse       <- unlist(lapply(title, function(x) sum(nchar(x), na.rm=T) >= 5 &! is.na(x[1])))
typeUse        <- unlist(lapply(type, function(x) sum(nchar(x), na.rm=T) >= 2 &! is.na(x[1])))
volumeUse      <- unlist(lapply(volume, function(x) sum(nchar(x), na.rm=T) >= 5 &! is.na(x[1])))

# vector with objects containing the tag uses
Use <- grep("Use$", ls(), value = T)
# time intervalls
from <- c(0, 0, 2000, 2005, 2010, 2015)
to <- c(Inf, 2000, 2005, 2010, 2015, 2020)
# text of time intervals
subsets <- c("total", "[1781; 2000]", "(2000; 2005]", "(2005; 2010]", "(2010; 2015]", "(2015; 2020]")
# pubyear to vector
pubyear <- as.numeric(unlist(pubyear))
# create table of use per time period
totalUse <- NULL
for(j in 1:length(from)){
  periodUse <- NULL
  for(i in 1:length(Use)){
    periodUse <- rbind(periodUse, prop.table(table(factor(get(Use[i])[pubyear > from[j] & pubyear <= to[j]], c(FALSE, TRUE)))))
    print(i)
  }
  periodUse <- rbind(periodUse, rep(sum(pubyear > from[j] & pubyear <= to[j], na.rm = T), 2))
  totalUse <- cbind(totalUse, periodUse)
}

# set row names
tags <- gsub("Use$", "", Use)
rownames(totalUse) <- c(tags, "total n")
# remove unnecessary columns
totalUse <- totalUse[, seq(-1, -ncol(totalUse), -2)]
# convert to percent
totalUse[-nrow(totalUse), ] <- paste0(round(totalUse[-nrow(totalUse), ], 3)*100, "%")
totalUse[nrow(totalUse), ] <- format(as.numeric(totalUse[nrow(totalUse), ]), big.m = ",")
totalUse <- gsub("\\.0|  ", "", totalUse)
colnames(totalUse) <- subsets

# resulting table 
totalUse

## maximum of published papers per year
pubyear<-unlist(pubyear)
max(table(pubyear))
## in year
names(table(pubyear)[table(pubyear) == max(table(pubyear))])


######################
### 3.2 Document type
####################
# load files
load("type.rda")
load("title.rda")
load("subject.rda")
load("keywords.rda")

# type as vector
type <- unlist(type)
# absolute frequencies
sort(table(unlist(type)), dec = T)
# convert NA to 'no type'
type[is.na(type)] <- "no <type>-tag"
type[nchar(type) == 0] <- "no <type>-tag"

## n types
types <- sort(table(type))
length(types)

## proportion and N of reasearch-articles
paste0(100*round(types[names(types) == "research-article"]/length(type), 2), "%,  ", 
       "N=", format(as.numeric(types[names(types) == "research-article"]), big.mark = ","))
## proportion and N of reviews
paste0(100*round(types[names(types) == "review-article"]/length(type), 2), "%,  ", 
       "N=", format(as.numeric(types[names(types) == "review-article"]), big.mark = ","))
## proportion and N of case-reports
paste0(100*round(types[names(types) == "case-report"]/length(type), 2), "%,  ", 
       "N=", format(as.numeric(types[names(types) == "case-report"]), big.mark = ","))

## number of retractions
sum(as.numeric(types[grep("retract", names(types))]))
## number of corrections
sum(as.numeric(types[grep("correct", names(types))]))

## Fig. 1: Absolute frequency distribution of article types
layout(matrix(c(1, 1, 1, 2, 2), nrow = 1))
par(mar = c(4, 10, 2, 2))
# first column
i <- 31:60
barplot(log(types[i]), horiz = T, las = 2, xlim = c(0, 16), axes = F, col = "white", names = "", border = "white", ylim = c(1.25, length(types[i])*1.2-1.5), xlab = "number of documents")
axis(1, log(c(1, 10, 100, 1000, 10000, 10^5, 10^6)), c(1, 10, 100, "1, 000", "10^4", "10^5", "10^6"), cex = .8)
abline(v = log(c(1, 10, 100, 1000, 10000, 100000, 1000000)), lty = 2, col = "grey")
barplot(log(types[i]), horiz = T, las = 2, xlim = c(0, 15), axes = F, add = T, col = c("darkgrey", "lightgrey"), ylim = c(1.5, length(types[i])*1.2-1.5))
par(xpd = T)
text(log(types[i]), (1:length(types[i]))-.65+(1:length(types[i]))*.2, gsub(" ", "", format(types[i], big.mark = ",")), pos = 4)
par(xpd = F)
# second column
par(mar = c(4, 8, 2, 2))#, mfrow = c(1, 2))
i <- 1:30
barplot(log(types[i]), horiz = T, las = 2, xlim = c(0, 7), axes = F, col = "white", names = "", border = "white", ylim = c(1.25, length(types[i])*1.2-1.5), xlab = "number of documents")
axis(1, log(c(1, 10, 100, 1000)), c(1, 10, 100, "1,000"))
abline(v = log(c(1, 10, 100, 1000, 10000)), lty = 2, col = "grey")
barplot(log(types[i]), horiz = T, las = 2, xlim = c(0, 15), axes = F, add = T, col = c("darkgrey", "lightgrey"), ylim = c(1.5, length(types[i])*1.2-1.5))
par(xpd = T)
text(log(types[i]), (1:length(types[i]))-.65+(1:length(types[i]))*.2, gsub(" ", "", format(types[i], big.mark = ",")), pos = 4)
par(xpd = F)

## imprecise ("wrong") type tags
# title as vector
title <- unlist(title)
# n articles that have Correction: in begining of title but aren't type tagged as correction
i <- grep("^correction:", tolower(title))[(!is.element(grep("^correction:", tolower(title)), grep("correct", tolower(type))))]
length(i)
# titles
title[i]

# n untagged retractions
i <- grep("^retraction:", tolower(title))[(!is.element(grep("^retraction:", tolower(title)), grep("retract", tolower(type))))]
length(i)
# titles
title[i]

# n systematic and literature reviews type tagged as reseach article
i1 <- grep("research-article", tolower(type))
i2 <- grep("systematic[- ]review|literature[- ]review", tolower(title))
indexTitle <- i1[is.element(i1, i2)]
length(indexTitle)
title[indexTitle]

## subject tag as identifier for reviews
# subject as vector with collapsed subjects
subject <- unlist(lapply(subject, paste, collapse = ",  "))
i3 <- grep("^review|[- ]review", tolower(subject))
indexSubj <- i3[is.element(i3, indexTitle)]
length(indexSubj)

## keyword tag as identifier for reviews
# keywords as vector with collapsed keywords
keywords <- unlist(lapply(keywords, paste, collapse = ", "))
i4 <- grep("^review|[- ]review", tolower(keywords))
indexKey <- i4[is.element(i4, indexTitle)]
length(indexKey)

# both, kwd and subject tag containing `review`
sum(is.element(indexKey,indexSubj))

#########################
## 3.3 Involved journals
#######################
# load files
load("journal.rda")

## Table 2: Absolute and relative frequency of journals supplying n articles
journal <- unlist(journal)
breaks <- c(0, 1, 10, 100, 1000, 10000, 100000, max(table(journal)))
labels <- c("1", "2-10", "11-100", "101-1,000", "1,001-10,000", "10,001-100,000", ">100,000")
jou <- cut(table(journal), breaks, labels)
jou

# proportion of journals with less equal 10 articles
sum(prop.table(table(jou))[1:2])
# n journals with more than 10,000 articles
jou2 <- table(journal)[table(journal) > 10000]
jou2
length(jou2)
# n articles from journals with >10,000 articles
sum(jou2)
# these journals represent a propartion:
sum(jou2)/length(journal) # of all articles

## Fig. 2: Total number of published articles for journals with >10,000 articles
# absolute number of files of 25 most publishing journals
tops <- sort(table(journal)[table(journal) > 10000], dec = F)
tops
# shorten journal name
journal <- gsub("International Journal of Environmental", "Int. Journal of Environmental", journal)

# 1 column view
par(mar = c(4, 27, 1, 5), xpd = T)
barplot(tops, horiz = T, col = c("darkgrey", "lightgrey"),  las = 2, axes = F, xlab = "number of articles", xlim = c(0, 250000))
axis(1)
text(tops, (1:length(tops))-.65+(1:length(tops))*.2, format(tops, big.mark = ","), pos = 4)

# 2 column view
layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 2, 2), nrow = 1))
par(mar = c(4, 23.5, .5, 8), xpd = T)
i <- ceiling(length(tops)/2):length(tops)
barplot(tops[i], horiz = T, col = c("darkgrey", "lightgrey"),  las = 2, axes = F, xlab = "number of articles", xlim = c(0, 250000))
marks <- c(0, 50000, 100000, 150000, 200000, 250000)
axis(1, marks, format(marks, big.m = ",", trim = T))
text(tops[i], (1:length(tops[i]))-.65+(1:length(tops[i]))*.2, format(tops[i], big.mark = ",", trim = T), pos = 4)

i <- 1:floor(length(tops)/2)
par(mar = c(4, 10, .5, 2))
barplot(tops[i], horiz = T, col = c("darkgrey", "lightgrey"),  las = 2, axes = F, xlab = "number of articles", xlim = c(0, 25000))
marks <- c(0, 5000, 10000, 15000, 20000, 25000)
axis(1, marks, format(marks, big.m = ",", trim = T))
text(tops[i], (1:length(tops[i]))-.65+(1:length(tops[i]))*.2, format(tops[i], big.mark = ",", trim = T), pos = 4)


###########################
## 3.4 Publication history
#########################
# load files
load("history.rda")
load("journal.rda")

## Table 3: Frequencies of use and time spans of extracted history date stamps
# time stamps
stamps <- gsub("^history\\.", "", unique(names(unlist(history))))
stamps <- sort(stamps)[c(1:11, 14:17, 12:13)]
# extract use of each stamp
stampsUsed <- matrix(NA, ncol = 3, nrow = length(stamps))
for(i in 1:length(stamps)){
  temp <- unlist(lapply(history, "[", stamps[i]))
  stampsUsed[i, 1] <- round(sum(!is.na(temp))/length(history), 3)*100
  stampsUsed[i, 2:3] <- range(temp, na.rm = T)
  print(i)
}
# edit table
rownames(stampsUsed) <- stamps
stampsUsed[, 1] <- paste0(stampsUsed[, 1], "%")
colnames(stampsUsed) <- c("relative use", "earliest date", "latest date")
stampsUsed
# remove corrected
stampsUsed <- stampsUsed[-3, ]
stampsUsed

## Errors in history tags
# received after accepted
journal <- unlist(journal)
received <- unlist(lapply(history, "[", "received"))
accepted <- unlist(lapply(history, "[", "accepted"))
i <- (as.Date(received)) > (as.Date(accepted))
i[is.na(i)] <- FALSE

# n files with bad accept date
sum(i, na.rm = T)
# in n journals
length(table(journal[i]))

## show accept/receive errors
error1 <- history[i]
tail(error1)

## published before accepted
published <- unlist(lapply(history, "[", "pubDate"))
i <- (as.Date(published))<(as.Date(accepted))
i[is.na(i)] <- FALSE
# n articles
sum(i, na.rm = T)
# show errors
error1 <- history[i]
tail(error1)

## aricles with time stamp from future
pubyear <- as.numeric(unlist(lapply(history, "[", "pubyear")))
sum(accepted >= 2021, na.rm = T)
sum(published >= 2021, na.rm = T)
sum(received >= 2021, na.rm = T)

## n articles from journals that were released in 1781
table(as.character(journal[!is.na(pubyear) & pubyear == "1781"]))
# years missing
(min(pubyear, na.rm = T):max(pubyear, na.rm = T))[!is.element(min(pubyear, na.rm = T):max(pubyear, na.rm = T), names(table(pubyear)))]

# proportion of articles since 2000
prop.table(table(pubyear>=2000))[2]

##################
### 3.5.1 Authors
#################
# load files
load("author.rda")
load("type.rda")
load("title.rda")
load("pubyear.rda")
pubyear<-as.numeric(unlist(pubyear))

## author distribution
# author name frequency
tab <- sort(table(unlist(author)), dec = T)
# n articles with any author ID 
i <- grep("id-type|[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-", tolower(names(tab)))
ids <- table(gsub(", .*", "", gsub(".*type: ", "", tolower(names(tab[i])))))
length(ids)

# n orcid IDs
o <- grep("[0-9]{4}", names(tab), v = T)
o <- grep("[0-9]{4}-[0-9a-zA-Z]{4}-", o, v = T)
o <- grep("zoobank|twitter|sciprof", o, inv = T, v = T)
# clean up
o <- gsub(".*org/([0-9])", "\\1", o)
o <- gsub("[[:punct:]]*$", "", o)
o <- sub(".*([0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}).*", "\\1", o)
o <- grep("[a-zA-Z]{3}", o, inv = T, v = T)
norcid <- length(table(o))
norcid

# n sciprofile IDs
length(grep("sciprofiles.com", names(tab)))
# n zoobank IDs
unname(ids[which(names(ids) == "zoobank")])
# n twitter IDs
unname(ids[which(names(ids) == "twitter")])

# create has at least one author-id
hasID <- unlist(grepl("id-type|[0-9]{4}-[0-9a-zA-Z]{4}-", author))
head(author[hasID])
table(hasID)
prop.table(table(hasID))
prop.table(table(hasID,pubyear),m=2)

# proportion of documents with at least one author ID
prop.table(table(hasID))[2]
# proportion of documents with at least one author ID in 2020
prop.table(table(hasID[pubyear == 2020]))[2]

# full table and barplot ogf proportion of articles with ID tagged author over time
prop.table(table(hasID[pubyear >= 2003], pubyear[pubyear >= 2003]), m = 2)
barplot(prop.table(table(hasID[pubyear >= 2003], pubyear[pubyear >= 2003]), m = 2)[2, ], col = c("grey30", "grey70"), xlab = "year", ylab = "f(x)")
legend("topleft", c("has author ID  ", "no author ID"), col = c("grey70", "grey30"), pch = 15)

# select only hasID
subset <- author[hasID]
# n authors with ID per paper
nID <- unlist(lapply(subset, function(x){length(grep("id-type|[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-", x))}))
table(nID)
# n authors per paper
naut <- unlist(lapply(subset, length))
# proportion of articles with all authors ID tagged
prop.table(table(naut-nID))[1]
# relative amount of files with 50% of authors ID tagged
prop.table(table(cut(nID/naut, seq(0, 1, len = 3), inc = T)))
# maximum relative use of ID per year
tab <- round(prop.table(table(hasID[pubyear > 2002], pubyear[pubyear > 2002]), m = 2), 3)[2,]
# maximum amount
m <- max(tab);m
# in year
names(tab)[tab==m]

## top 250 authors
aut <- unlist(author)
tabaut <- table(aut)
res <- sort(tabaut, dec = T)[1:250]
res 

## Inspect articles with >500 authors
# n authors per paper
naut <- unlist(lapply(author, length))
sum(naut>500)
# by type
t <- (type[!is.na(naut) & naut > 500])
prop.table(table(unlist(t)))
# by title
tit <- (title[!is.na(naut) & naut > 500])
# n articles with >500 authors
length(tit)
# first 100 titles
head(tit, 100)
# number/proportion of 'atlas' alike articles in this seletion
length(grep("atlas|proton|collision|jet|detector|particle", tolower(tit), inv = F, v = T))
length(grep("atlas|proton|collision|jet|detector|particle", tolower(tit), inv = F, v = T))/length(tit) 
# number/proportion of 'global  burden of disease study' articles in this seletion
length(grep("global burden of disease study", tolower(tit), inv = F, v = T))
length(grep("global burden of disease study", tolower(tit), inv = F, v = T))/length(tit)

#################
## 3.5.2 Editors
###############
# load files
load("editor.rda")
# n documents with ID tagged editor
i <- which(unlist(grepl("id-type|[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-", editor)))
length(i)
# n editors with Orcid
orcids <- lapply(editor[i], function(x) gsub("https", "http", x))
length(unique(grep("http", unlist(orcids), value = TRUE)))
# sum/proportion of articles with editor
n <- sum(!is.na(lapply(editor, "[", 1)));n
p <- round(n/length(editor)*100, 2);p
# sum of unique editors
e <- (unique(unlist(editor)))
e <- na.omit(e)
length(e)

# n editors per document
l <- unlist(lapply(editor[!is.na(editor)], length))
table(l)
# which article has max editors
max(l) # 121 editors
i <- which(unlist(lapply(editor, length)) == max(l))
editor[i]
title[i]
# -> German Book Infektionserkrankungen, Therapie in der Kinder- und Jugendmedizin, Strategien für Klinik und Praxis (2007), Pages 439-533 


## Fig. 3: Wordclouds of 250 most present author and editor names
# function to convert ORCID to name with orcid.org api (needs valid ORCID token for OAuth authentication)
library(rorcid)
orcid2name<-function(x){
  x<-unlist(x)
  # get index
  index<-grep("[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}-[0-9a-zA-Z]{4}",x)
  if(length(index)>0){
    x<-gsub(".*orcid\\.org/","",x)
    x<-gsub("([0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{4}).*","\\1",x)
    for(i in 1:length(index)){
      tryCatch({
        o<-orcid_id(orcid=x[index[i]])
        n<-o[[1]]["name"]
        given<-unname(unlist(n[[1]]["given-names"]))
        family<-unname(unlist(n[[1]]["family-name"]))
        out<-(paste0(family,", ",given))
        x[index[i]]<-out
      })
    }}
  return(x)
}

# load files
load("author.rda")
load("editor.rda")
# all authors as vector
aut <- unlist(author)
# absolute frequency table
tabaut <- table(aut)
# top 251 authors
res <- sort(tabaut, dec = T)[1:251]
# convert ORCIDs to names
names(res)<-orcid2name(names(res))
# remove "NA." if has "NA." 
res <- res[which(names(res) != "NA.")][1:250]
length(res)

# all editors as vector
eds <- unlist(editor)
# top 250 editors
res2 <- sort(table(eds), dec = T)[1:251]
# remove "NA." if has "NA."
res2 <- res2[which(names(res2) != "NA.")][1:250]
length(res2)

# draw wordclouds
library(wordcloud)
layout(matrix(c(1, 2), ncol = 2, byr = T))
par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
wordcloud(names(res[1:250]), res[1:250], c(1.55, .3), rot.per = .15)
mtext("250 most present author names", font = 2, padj = -1, cex = 1.2)
wordcloud(names(res2[1:250]), res2[1:250], c(1.3, .4), rot.per = .15)#mtext("250 most present editor names", font = 2, padj = -1, cex = 1.2)
# adjust settings and retry if cannot display all names

######################
### 3.5.3 Affiliation
#####################

# load file
load("affiliation.rda")
# extract little cleaned up universitiy names
uni <- lapply(affiliation, function(x) unique(gsub("^[^[:alpha:]]*([A-Z])", "\\1", grep("Universit", unlist(strsplit(x, "[[:punct:]] | and | the |The ")), v = T))))
# absolute frequencies of involved universities
tab <- sort(table(unlist(uni)), dec = T)
# 100 most present university names
tab[1:100]

# diversity in naming of one and the same institution
tab[grep("Oxford", names(tab))][1:20]
tab[grep("Hamburg", names(tab))][1:20]


##############################
### 3.6 Subjects and keywords
############################
# load files
load("subject.rda")
load("keywords.rda")
load("pubyear.rda")
load("title.rda")
load("type.rda")

## <subject> tag
# subjects as vector
temp <- unlist(subject)
# remove type alike terms
#temp <- grep("article|research|original|review|editor|comment|erratum|abstract|presentation|report|correction|communication|correspondence|letter|perspective|essay|symposium|retraction", tolower(temp), inv = T, v = T)
# absolute frequencies of subjects
t <- sort(table(temp), dec = T)
# n distinct subject labels
length(t)
# top 250 subject labels for wordcloud
t <- t[1:250]
t

## N subjects per article
nSub <- unlist(lapply(subject, length))
# .995 quantile of used subjects
quantile(nSub,.995)
# maximum of used subjects
max(nSub)
subject[which(nSub==max(nSub))]
type[which(nSub==max(nSub))]
title[which(nSub==max(nSub))]


## <kwd> tag
# set empty keywords to NA
index <- which(unlist(lapply(keywords, function(x) sum(nchar(x), na.rm = T)<2|is.na(x[1]))))
# n keywords per article
Nkeywords <- unlist(lapply(keywords, length))
# set no keywords to 0
Nkeywords[index] <- 0
# .995 quntile of used keywords
quantile(Nkeywords,.995)
# maximum of used keywords
max(Nkeywords, na.rm = T)
keywords[Nkeywords == max(Nkeywords)]
# in 
title[Nkeywords == max(Nkeywords)]
pubyear[Nkeywords == max(Nkeywords)]
keywords[which(Nkeywords == max(Nkeywords, na.rm = T))]

# pubyear as factor (adjust year max if enough data for 2021 or any other year is recorded)
pubyear <- as.numeric(unlist(pubyear))
pubyear <- factor(pubyear, 1983:2020)

# relative amount with keywords per year
addmargins(round(prop.table(table(Nkeywords > 0, pubyear),m=2),3),m=1)
# maximum amount of articles with keywords
hasKey <- prop.table(table(Nkeywords > 0, pubyear), m = 2)[2, ]
round(max(hasKey)*100, 1)
# in year
names(which.max(hasKey))

# n articles with covid-19 in lowerizes keywords
i<-which(unlist(grepl("covid.19|sars.cov.2",lapply(keywords,tolower))))
length(i)

# remove obsolete keywords and lowerize
keys <- grep("in any medium|permits use|properly cited", tolower(unlist(keywords)), inv = T, v = T)
# frequency table of top 250 keywords
t2 <- sort(table(keys), dec = T)[1:250]
t2
# select non emty keywords only
keys <- keywords[unlist(keys != "")]


## Fig. 4: Wordclouds of 250 most frequent <subject> and <kwd> tags
layout(matrix(1:2, ncol = 2))
par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
wordcloud(names(t), t, c(1.3, .45), rot.per = .15)
mtext("250 most present subjects", font = 2, padj = -2.8, cex = 1.2)
wordcloud(names(t2), t2, c(1.3, .4), rot.per = .12)
mtext("250 most present keywords", font = 2, padj = -2.8, cex = 1.2)

#########################
## 3.7 Country of origin
#######################
load("pubyear.rda")
load("country.rda")
# vector with all countries
countryvec <- unlist(country)
# absolute frequencies of unique country detections per article
t <- table(countryvec)
t
# n countries detected
length(t)

# n articles with detected country entries
n<-sum(!is.na(lapply(country, "[", 1)))
n
# proportion of documents with country tag
n/length(country)
# n articles with more than one country detected
Ncountries<-unlist(lapply(country,length))
sum(Ncountries>1)
# porportion of international collaborations within articles with country
sum(Ncountries>1)/length(country[Ncountries>0])

## Fig. 5: Change in top 25 country involvement over time
# pubyear as numeric vector
pubyear<-as.numeric(unlist(pubyear))

# helper vectors with all countries in specific time intervalls
sub2000 <- country[pubyear < 2000]
min2000 <- min(pubyear, na.rm = T)
sub2010 <- country[pubyear >= 2000 & pubyear < 2010]
sub2020 <- country[pubyear >= 2010 & pubyear < 2020]
in2020 <- country[pubyear == 2020]

# set main label
main <- c(paste(min2000, "- 1999"), "2000 - 2009", "2010 - 2019", "2020")
# draw graph
par(mar = c(4.6, 8, 2, 1.5), mfrow = c(2, 2))
for(i in 1:4){
  countryvec <- unlist(get(c("sub2000", "sub2010", "sub2020", "in2020")[i]))
  t <- table(countryvec)
  t <- sort(t)[(length(t)-25):length(t)]
  barplot(t, horiz = T, las = 1, xlab = "n articles", axes = F, xlim = c(0, c(5000, 60000, 600000, 120000)[i]))
  # axis labels
  if(i == 1) mark <- c(0:5*1000)
  if(i == 2) mark <- c(0:6*10000)
  if(i == 3) mark <- c(0:6*100000)
  if(i == 4) mark <- c(0:6*20000)
  axis(1, mark, format(mark, big.m = ",", trim = T))
  mtext(main[i], font = 2)
}

