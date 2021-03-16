################################################################################################################################
## JATSdecoder: Evaluation of an automated text extraction tool for statistical results in scientific reports (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#############################################################################################################################

####################################################################################################################################
## This script reproduces the extraction and analyses on the open access article collection presented in the article (or any other).
## all results to create the published table with script: "03_stats_createResultTables.R"
###################################################################################################################################

###############
## Preparation
#############
rm(list = ls())
library(future.apply)
library(JATSdecoder)
library(statcheck)
plan(multisession, workers = 60, gc = TRUE) # adjust to your system

## identify PLoS One file names for analysis
folder <- "./PMC/comm_use.O-Z.xml/PLoS_One" # adjust to your system
# PLoS One XML files
PLoS <- list.files(patt = "xml$", rec = T, full.names = T)
# total number of PLoS One articles
nPLoS <- length(PLoS)
nPLoS 
# create subset with type == "research" & psychology relatedness in subjects or keywords
meta <- future_lapply(PLoS[], JATSdecoder, output = c("type", "subject", "keywords"))
keywords <- mapply(c, lapply(meta, "[", "keywords"))
subject <- mapply(c, lapply(meta, "[", "subject"))
type <- mapply(c, lapply(meta, "[", "type"))
# get index
index1 <- unlist(grepl("research", type))
index2 <- unlist(grepl("[Pp]sych", subject))
index3 <- unlist(grepl("[Pp]sych", keywords))
indexPLoS <- which(index1&index2|index1&index3)
# select identifies file names
PLoS <- PLoS[indexPLoS]
# n PLoS One files
nPLoS <- length(PLoS)
nPLoS

## all file names by Frontiers in Psychology 
folder <- "./PMC/comm_use.C-H.xml/Front_Psychol" # adjust to your system
front <- list.files(folder, patt = "xml$", rec = T, full.names = T)
# n files
nfront <- length(front)

## merge file names
files <- c(front, PLoS)
# total XML files for analysis
length(files)

## create journal vector
journal <- rep(c("Frontiers in Psychology", "PLoS One"), times = c(nfront, nPLoS))
table(journal)

###############################################################################
## Extraction of statistical results from PLoS One and Frontiers in Psychology
#############################################################################

## extraction of sticked results with study.character()
s <- Sys.time()
stats <- mapply(c, future_lapply(files, study.character, output = "stats", text = 1))
statsTime <- Sys.time()-s
statsTime
# remove lines with "^Result in "
stats <- future_lapply(stats, function(x) grep("^\u2022 Results in ", x, inv = T, v = T))

## extraction of all standardStats with get.stats()
s <- Sys.time()
allstandardStats <- future_lapply(stats, standardStats, stat = "all", T2t = T, R2r = T)
allstandardStatsTime <- Sys.time()-s
allstandardStatsTime

## extraction of computable results with get.stats()
# get all pcomputable stats
s <- Sys.time()
computable <- future_lapply(stats, standardStats, stat = "computable", T2t = T, R2r = T)
computableTime <- Sys.time()-s
computableTime

## extraction of computable results with get.stats(x,etimateZ=T)
s <- Sys.time()
estZ <- future_lapply(stats, standardStats, stat = "computable", T2t = T, estimate = T, R2r = T)
estZTime <- Sys.time()-s
estZTime

## extraction of checkable results with get.stats()
s <- Sys.time()
checkable <- future_lapply(stats, standardStats, stat = "checkable", T2t = T, R2r = T)
checkableTime <- Sys.time()-s
checkableTime

## extraction of checkable results with get.stats(x,estimateZ=T)
s <- Sys.time()
checkableBoost <- future_lapply(stats, standardStats, stat = "checkable", T2t = T, estimate = T, R2r = T)
checkableBoostTime <- Sys.time()-s
checkableBoostTime

# statcheck() on sticked results by study.character()
s <- Sys.time()
check <- future_lapply(stats, function(x) tryCatch(statcheck(x), error = function(e) NULL, warning = function(w) NULL))
checkTime <- Sys.time()-s
checkTime
# selection of results with numeric p value
check <- lapply(check, function(x) if(is.element("Reported.P.Value", names(x))) x[!is.na(x[, "Reported.P.Value"]), ])

# statcheck on native XML-files
s <- Sys.time()
HTMLcheck <- future_lapply(files, function(x) tryCatch(checkHTML(x), error = function(e) NULL, warning = function(w) NULL))
HTMLtime <- Sys.time()-s
HTMLtime
# selection of results with numeric p value
HTMLcheck <- lapply(HTMLcheck, function(x) if(is.element("Reported.P.Value", names(x))) x[!is.na(x[, "Reported.P.Value"]), ])


############
## Analysis
##########

## extraction times
timesXML <- c(statsTime, allstandardStatsTime, computableTime, estZTime, checkableTime, checkableBoostTime, checkTime, HTMLtime)
timesXML
 
##############################
## prepare n stats extracted
###########################
# n stats
n0 <- lapply(stats, length)
n1 <- lapply(allstandardStats, dim)
n2 <- lapply(computable, dim)
n3 <- lapply(estZ, dim)
n4 <- lapply(checkable, dim)
n5 <- lapply(checkableBoost, dim)
n6 <- lapply(check, dim)
n7 <- lapply(HTMLcheck, dim)
# set no captures to 0
l0 <- unlist(lapply(n0, length)); n0[l0 == 0] <- 0
l1 <- unlist(lapply(n1, length)); n1[l1 == 0] <- 0
l2 <- unlist(lapply(n2, length)); n2[l2 == 0] <- 0
l3 <- unlist(lapply(n3, length)); n3[l3 == 0] <- 0
l4 <- unlist(lapply(n4, length)); n4[l4 == 0] <- 0
l5 <- unlist(lapply(n5, length)); n5[l5 == 0] <- 0
l6 <- unlist(lapply(n6, length)); n6[l6 == 0] <- 0
l7 <- unlist(lapply(n7, length)); n7[l7 == 0] <- 0
# n results as vector
nstat0 <- unlist(lapply(n0, "[", 1))
nstat1 <- unlist(lapply(n1, "[", 1))
nstat2 <- unlist(lapply(n2, "[", 1))
nstat3 <- unlist(lapply(n3, "[", 1))
nstat4 <- unlist(lapply(n4, "[", 1))
nstat5 <- unlist(lapply(n5, "[", 1))
nstat6 <- unlist(lapply(n6, "[", 1))
nstat7 <- unlist(lapply(n7, "[", 1))

# Merge to data frame
d <- data.frame(stats = nstat0, standardStats = nstat1, computable = nstat2, computableBoost = nstat3, checkable = nstat4, checkableBoost = nstat5, statcheck = nstat6, checkHTML = nstat7)
dim(d)

# documents with stats
colSums(d>0)
# total stats
colSums(d)

# descriptives
means <- apply(d, 2, function(x) mean(x[x>0]))
means
sds <- apply(d, 2, function(x) sd(x[x>0]))
sds
medians <- apply(d, 2, function(x) median(x[x>0]))
medians
IQR <- apply(d, 2, function(x) quantile(x[x>0], c(.25, .75)))
IQR <- apply(IQR, 2, function(x) paste0("[", x[1], "; ", x[2], "]"))
IQR
q99 <- apply(d, 2, function(x) quantile(x[x>0], c(.99)))
q99
max <- apply(d, 2, function(x) max(x[x>0]))
max

#######################################################################################
## hit statistic per journal and method
# n documents per journal
Nstudies <- table(journal)
Nstudies
if(length(table(journal))>1) Nstudies <- c(Nstudies, sum(Nstudies))
table(journal)

# n documents with stats per journal and allgorithm
Ndocs <- apply(d, 2, function(x) tapply(x, journal, function(x) sum(x>0)))
# total stats per journal and allgorithm
Nstats <- apply(d, 2, function(x) tapply(x, journal, function(x) sum(x)))
statsum <- colSums(d)
# relative amount of studies with stats
Fdocs <- apply(d, 2, function(x) tapply(x, journal, function(x) sum(x>0)/length(x)))

if(is.null(dim(Ndocs))) sums <- Ndocs else sums <- colSums(Ndocs)
Fsums <- sums/length(journal)

if(!is.null(dim(Ndocs))) Ndocs <- rbind(Ndocs, sums) 
if(!is.null(dim(Ndocs))) Fdocs <- rbind(Fdocs, Fsums)

# format as scientific with ","
Ndocs <- format(Ndocs, big.m = ",")
Nstudies <- format(Nstudies, big.m = ",")
Nstats <- format(Nstats, big.m = ",")
statsum <- format(statsum, big.m = ",")

# paste % with N
if(is.null(dim(Ndocs))) tab <- t(as.data.frame(Ndocs)) else tab <- Ndocs
if(is.null(dim(Fdocs))) Fdocs <- t(as.data.frame(Fdocs));tab
if(!is.null(dim(tab))) for(i in 1:dim(tab)[1]) tab[i, ] <- paste0(tab[i, ], " (", round(Fdocs[i, ], 3)*100, "%)");tab
if(!is.null(dim(tab))) tab <- cbind(Nstudies, tab);tab
if(!is.null(dim(tab))) tab <- cbind(tab, "");tab

# total stats with descriptives
descriptives <- rbind(Nstats, sum = statsum, mean = round(means, 1), sd = round(sds, 1), median = medians, IQR = IQR, quantile99 = round(q99, 1), max = max, timeinminutes = round(timesXML, 1), secsperpaper = round(timesXML*60*60/as.numeric(gsub(",", "", length(stats))), 2))
n <- rownames(descriptives)
descriptives <- cbind(c(Nstudies, rep("", 8)), descriptives)
rownames(descriptives) <- n

## result table
descriptives

