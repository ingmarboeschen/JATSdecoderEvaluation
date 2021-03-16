################################################################################################################################
## JATSdecoder: Evaluation of an automated text extraction tool for statistical results in scientific reports (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#############################################################################################################################

############################################################################################################################
## This script reproduces the extraction and analyses on the PDF article collection (or any other) presented in the article.
###########################################################################################################################

# Note: all PDF files were converted to CERMXML with CERMIN (https://github.com/CeON/CERMINE)

###############
## Preparation
#############
rm(list = ls())
library(JATSdecoder)
library(statcheck)
library(future.apply)
plan(multisession, workers = 60, gc = TRUE) # adjust to your system

# folder with PDFs and CERMINE converted PDFs (.cermxml) as working directory
folder <- "./PDFarticles2020-2021" # adjust to your system
# get file locations of PDF and CERMXML files
PDFfiles <- list.files(folder, patt = "pdf$", rec = T, full.names = T)
files <- list.files(folder, patt = "xml$", rec = T, full.names = T)

# create journal vector
journal <- list.files(folder, patt = "xml$", rec = T)
journal <- unlist(lapply(strsplit(journal, "/"), "[", 1))
journal <- gsub("Journal", "J.", journal)
journal <- gsub("Bulletin", "Bul.", journal)
# labels
lab <- unique(journal)
# factorize
journal <- factor(journal, lab)
# absolute frequencies
table(journal)

## extraction of results with get.stats()
s <- Sys.time()
stats <- mapply(c, future_lapply(files[], get.stats, output = "stats"))
statsTime <- Sys.time()-s
statsTime
# remove lines with "^Result in "
stats <- future_lapply(stats, function(x) grep("^\u2022 Results in ", x, inv = T, v = T))

# get all standardStats
s <- Sys.time()
allstandardStats <- future_lapply(stats, standardStats, stat = "all", T2t = T, R2r = T)
allstandardStatsTime <- Sys.time()-s
allstandardStatsTime

# get all stats with computable p value
s <- Sys.time()
computable <- future_lapply(stats, standardStats, stat = "computable", T2t = T, R2r = T)
computableTime <- Sys.time()-s
computableTime

# get all computable stats with estimateZ = TRUE
s <- Sys.time()
computableBoost <- future_lapply(stats, standardStats, stat = "computable", T2t = T, estimate = T, R2r = T)
computableBoostTime <- Sys.time()-s
computableBoostTime

# get all p checkable stats with get.stats(x,T2t=T,R2r=T)
s <- Sys.time()
checkable <- future_lapply(stats, standardStats, stat = "checkable", T2t = T, estimate = F, R2r = T)
checkableTime <- Sys.time()-s
checkableTime

# get all p checkable stats with get.stats(x,T2t=T,R2r=T,estimateZ=T)
s <- Sys.time()
checkableBoost <- future_lapply(stats, standardStats, stat = "checkable", T2t = T, estimate = T, R2r = T)
checkableBoostTime <- Sys.time()-s
checkableBoostTime

## statcheck on sticked results by study.character(x,out="stats")
# convert "<=>" to "="
stats <- lapply(stats, function(x) gsub("<=>", "=", x))
s <- Sys.time()
check <- future_lapply(stats, function(x) tryCatch(statcheck(x), error = function(e) NULL, warning = function(w) NULL))
checkTime <- Sys.time()-s
checkTime
# remove non captured p value results (ns)
check <- lapply(check, function(x) if(is.element("Reported.P.Value", names(x))) x[!is.na(x[, "Reported.P.Value"]), ])

## statcheck() on raw HTML-files
s <- Sys.time()
HTMLcheck <- future_lapply(files, function(x) tryCatch(checkHTML(x), error = function(e) NULL, warning = function(w) NULL))
HTMLtime <- Sys.time()-s
HTMLtime
# remove non captured p value results (ns)
HTMLcheck <- lapply(HTMLcheck, function(x) if(is.element("Reported.P.Value", names(x))) x[!is.na(x[, "Reported.P.Value"]), ])

## statcheck on raw PDF-files
s <- Sys.time()
PDFcheck <- future_lapply(PDFfiles, function(x) tryCatch(checkPDF(x), error = function(e) NULL, warning = function(w) NULL))
PDFtime <- Sys.time()-s
PDFtime
# remove non captured p value results (ns)
PDFcheck <- lapply(PDFcheck, function(x) if(is.element("Reported.P.Value", names(x))) x[!is.na(x[, "Reported.P.Value"]), ])

## extraction times per method
times <- c(statsTime, allstandardStatsTime, computableTime, computableBoostTime, checkableTime, checkableBoostTime, checkTime, HTMLtime, PDFtime)
times

############
## Analysis
##########

## create n results extracted
n0 <- lapply(stats, length)
n1 <- lapply(allstandardStats, dim)
n2 <- lapply(computable, dim)
n3 <- lapply(computableBoost, dim)
n4 <- lapply(checkable, dim)
n5 <- lapply(checkableBoost, dim)
n6 <- lapply(check, dim)
n7 <- lapply(HTMLcheck, dim)
n8 <- lapply(PDFcheck, dim)
# set no captures to 0
l0 <- unlist(lapply(n0, length)); n0[l0 == 0] <- 0
l1 <- unlist(lapply(n1, length)); n1[l1 == 0] <- 0
l2 <- unlist(lapply(n2, length)); n2[l2 == 0] <- 0
l3 <- unlist(lapply(n3, length)); n3[l3 == 0] <- 0
l4 <- unlist(lapply(n4, length)); n4[l4 == 0] <- 0
l5 <- unlist(lapply(n5, length)); n5[l5 == 0] <- 0
l6 <- unlist(lapply(n6, length)); n6[l6 == 0] <- 0
l7 <- unlist(lapply(n7, length)); n7[l7 == 0] <- 0
l8 <- unlist(lapply(n8, length)); n8[l8 == 0] <- 0
# n results as vector
nstat0 <- unlist(lapply(n0, "[", 1))
nstat1 <- unlist(lapply(n1, "[", 1))
nstat2 <- unlist(lapply(n2, "[", 1))
nstat3 <- unlist(lapply(n3, "[", 1))
nstat4 <- unlist(lapply(n4, "[", 1))
nstat5 <- unlist(lapply(n5, "[", 1))
nstat6 <- unlist(lapply(n6, "[", 1))
nstat7 <- unlist(lapply(n7, "[", 1))
nstat8 <- unlist(lapply(n8, "[", 1))

## Merge to data frame
d <- data.frame(stats = nstat0, standardStats = nstat1, computable = nstat2, computableBoost = nstat3, checkable = nstat4, checkableBoost = nstat5, statcheck = nstat6, checkHTML = nstat7, checkPDF = nstat8)

## descriptives
journal <- factor(journal, sort(levels(journal)))
# n documents with stats
colSums(d>0)
# total stats
colSums(d)

# descriptives
means <- apply(d, 2, function(x) mean(x[x>0], na.rm = T));means
sds <- apply(d, 2, function(x) sd(x[x>0], na.rm = T));sds
medians <- apply(d, 2, function(x) median(x[x>0], na.rm = T));medians
IQR <- apply(d, 2, function(x) quantile(x[x>0], c(.25, .75), na.rm = T));IQR
IQR <- apply(IQR, 2, function(x) paste0("[", x[1], "; ", x[2], "]"))
q99 <- apply(d, 2, function(x) quantile(x[x>0], c(.99), na.rm = T));q99
max <- apply(d, 2, function(x) max(x[x>0], na.rm = T));max

## hit statistic per journal and method
# n documents per journal
Nstudies <- table(journal);Nstudies
Nstudies <- c(Nstudies, sum(Nstudies))
# n documents with stats per journal and allgorithm
Ndocs <- apply(d, 2, function(x) tapply(x, journal, function(x) sum(x>0, na.rm = T)))
# total stats per journal and allgorithm
Nstats <- apply(d, 2, function(x) tapply(x, journal, function(x) sum(x, na.rm = T)))
statsum <- colSums(d, na.rm = T)
# relative amount of studies with stats
Fdocs <- apply(d, 2, function(x) tapply(x, journal, function(x) sum(x>0, na.rm = T)/length(x)))

sums <- colSums(Ndocs, na.rm = T)
Fsums <- sums/length(journal)

Ndocs <- rbind(Ndocs, sums)
Fdocs <- rbind(Fdocs, Fsums)
# format as scientific with ","
Ndocs <- format(Ndocs, big.m = ",")
Nstudies <- format(Nstudies, big.m = ",")
Nstats <- format(Nstats, big.m = ",")
statsum <- format(statsum, big.m = ",")

# paste % with N
tab <- Ndocs
for(i in 1:dim(tab)[1]) tab[i, ] <- paste0(tab[i, ], " (", round(Fdocs[i, ], 3)*100, "%)");tab
tab <- cbind(Nstudies, tab)
tab

# total stats with descriptives
descriptives <- rbind(Nstats, sum = statsum, mean = round(means, 1), sd = round(sds, 1), median = medians, IQR = IQR, quantile99 = round(q99, 1), max = max, timeinsecs = round(times*60), secsperpaper = round(times*60/as.numeric(gsub(",", "", dim(d)[1])), 2))
n <- rownames(descriptives)
descriptives <- cbind(c(Nstudies, rep("", 8)), descriptives)
rownames(descriptives) <- n

# result table
descriptives

