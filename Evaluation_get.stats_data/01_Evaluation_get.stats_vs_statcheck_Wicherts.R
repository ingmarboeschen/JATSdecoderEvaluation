################################################################################################################################
## JATSdecoder: Evaluation of an automated text extraction tool for statistical results in scientific reports (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#############################################################################################################################

##################################################################################
## This script reproduces the extraction and comparison to Wicherts et al. (2012)
################################################################################

###############
## Preparation
#############
rm(list = ls())
library(JATSdecoder)
library(statcheck)
library(future.apply)
plan(multisession, workers = 60, gc = TRUE) # adjust to your system

# set working directory to folder containing native PDF and CERMINE compiled PDF files in CERMXML format
setwd("./49papersWicherts")
#setwd("/home/tower/Verschriftlichung/02_Psychology in PMC/Wicherts/49 papers")
setwd("/home/ingmar/JATSdecoderEvaluation/get.stats/Wicherts/49papersWicherts")

# create file name vectors for different input formats
# native PDF
PDFfiles <- list.files(pattern = "pdf$", rec = T, full.names = T)
# CERMINE compiled PDF as CERMXML
files <- list.files(pattern = "cermxml$", rec = T, full.names = T)
# browser generated HTML
HTMLfiles <- list.files(pattern = "html$", rec = T, full.names = T)
# manually extracted number of sign. t-, F-, chi^2 results by Wicherts et al. 
# copied from: https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0026828
nStatsWicherts <- c(7,13,33,25,83,30,19,22,9,21,16,10,21,8,8,32,37,11,39,23,46,20,35,30,37,33,15,21,24,27,6,16,23,24,5,29,9,15,20,8,48,28,27,8,7,9,36,45,30)
sum(nStatsWicherts)

## define function to remove indices from t-, F-, Chi2-results for statcheck()
rm.index <- function(x){
  # remove number of numbered F-values
  noIndex <- gsub("([ \\[\\(])F[0-9]*?\\(([1-9])|^F[0-9]*?\\(([1-9])", "\\1F(\\2\\3", unlist(x))
  noIndex <- gsub(",F", ", F", noIndex)
  # remove letter of labeled F-values in lines with (df1,df2)
  if(length(grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)", noIndex))>0){ 
    noIndex[grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)", noIndex)] <- gsub("([ \\[\\(])F[a-zA-Z ]*?\\(([1-9])|^F[a-zA-Z ]*?\\(([1-9])", "\\1F(\\2\\3", noIndex[grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)", noIndex)])
    }
  # remove number of numbered t-values
  noIndex <- gsub("( )t[0-9]*?\\(([1-9])|^t[0-9]*?\\(([1-9])", "\\1t(\\2\\3", noIndex)
  noIndex <- gsub("( )t[a-zA-Z]\\(([1-9])|^t[a-zA-Z]\\(([1-9])", "\\1t(\\2\\3", noIndex)
  # remove text from chi_text
  if(length(grep("^chi[A-Za-z]| chi[A-Za-z]", noIndex))>0){
    noIndex[grep("^chi[A-Za-z]| chi[A-Za-z]", noIndex)] <- gsub("chi[A-Za-z]*?([\\(=])", "chi\\1", noIndex[grep("^chi[A-Za-z]| chi[A-Za-z]", noIndex)])
    }
  return(noIndex)
}

###################################################################################
## Extract and count significant t-, F-, Chi2-results in method and result section
#################################################################################
## extraction of significant t- F-, chi^2 results with study.character() from method and result section
getStatsComp <- mapply(c, future_lapply(files[], function(x) study.character(x, text.mode = 2, out = "standardStats", stats = "computable", select = c("t", "F", "Chi2"))))
# reduce to significant results only
getStatsCompSig <- NA
ps <- unlist(lapply(lapply(getStatsComp, names), function(x) is.element("p", x)))
recP <- unlist(lapply(lapply(getStatsComp, names), function(x) is.element("recalculatedP", x)))
# set cases with no p-values to 0
getStatsCompSig[which((ps+recP) == 0)] <- 0 
# with reported and recomputable p value
getStatsCompSig[which((ps+recP) == 2)] <- lapply(getStatsComp[which((ps+recP) == 2)], function(x) x[which(as.numeric(x[, "p"]) <= .05|as.numeric(x[, "recalculatedP"]) <= .05), ])
# with reported p value only
getStatsCompSig[which(ps == 1&recP == 0)] <- lapply(getStatsComp[which(ps == 1&recP == 0)], function(x) x[which(as.numeric(x[, "p"]) <= .05), ])
# with recomputable p value only
getStatsCompSig[which(ps == 0&recP == 1)] <- lapply(getStatsComp[which(ps == 0&recP == 1)], function(x) x[which(as.numeric(x[, "recalculatedP"]) <= .05), ])
# count significant results per paper
getStatsCompSig <- unlist(lapply(lapply(getStatsCompSig, nrow), function(x) ifelse(is.null(x), 0, x)))
# total sign. t-, F-, chi^2-results
sum(getStatsCompSig)
# total difference to Wicherts et al.
sum(getStatsCompSig-nStatsWicherts)

## extraction of sticked results from method and result sections with study.character() for statcheck()
MethResStats <- future_lapply(files, study.character, text.mode = 2, output = "stats")
# convert "<=>" -> "0"
MethResStats <- lapply(MethResStats, function(x) gsub("<=>", "=", unlist(x)))

## extraction of significant t-, F-, and chi2 results with statcheck()
statcheckMethRes <- future_lapply(MethResStats, statcheck, stat = c("t", "F", "chisq"))
# select and count significant results
statcheckSig <- lapply(statcheckMethRes, function(a) a[a$Reported.P.Value <= .05|a$Computed <= .05, ])
statcheckSig <- unlist(lapply(lapply(statcheckSig, nrow), function(x) ifelse(is.null(x), 0, x)))
# total sign. t-, F-, chi^2-results
sum(statcheckSig)

# apply function to remove indices for statcheck()
MethResStatsIndex <- lapply(MethResStats, rm.index)

## extraction of results from index removed results in method and result sections with statcheck()
statcheckIndex <- lapply(MethResStatsIndex, statcheck, stat = c("t", "F", "chisq"))
# select and count significant results
statcheckIndexSig <- lapply(statcheckIndex, function(a) a[a$Reported.P.Value <= .05|a$Computed <= .05, ])
statcheckIndexSig <- unlist(lapply(lapply(statcheckIndexSig, nrow), function(x) ifelse(is.null(x), 0, x)))
# count
sum(statcheckIndexSig)

## extraction of significant t-, F-, and chi2 results with checkHTML()
HTMLstatcheck <- future_lapply(HTMLfiles, checkHTML, stat = c("t", "F", "chisq"))
# select and count significant results
HTMLstatcheckSig <- lapply(HTMLstatcheck, function(a) a[a$Reported.P.Value <= .05|a$Computed <= .05, ])
HTMLstatcheckSig <- unlist(lapply(lapply(HTMLstatcheckSig, nrow), function(x) ifelse(is.null(x), 0, x)))
# count
sum(HTMLstatcheckSig)

## extraction of significant t-, F-, and chi2 results with checkPDF()
PDFstatcheck <- future_lapply(PDFfiles, checkPDF, stat = c("t", "F", "chisq"))
# select and count significant results
PDFstatcheckSig <- lapply(PDFstatcheck, function(a) a[a$Reported.P.Value <= .05|a$Computed <= .05, ])
PDFstatcheckSig <- unlist(lapply(lapply(PDFstatcheckSig, nrow), function(x) ifelse(is.null(x), 0, x)))
# count
sum(PDFstatcheckSig)


#########################################################
## Extract and count all standard results from full text
########################################################

## extract "all" standardStats with get.stats()
getStatsAll <- lapply(files, get.stats, output = "standardStats", stats = "all")
# count
getStatsAll <- unlist(lapply(lapply(getStatsAll, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(getStatsAll)

## extract "computable" standardStats with get.stats()
computableStats <- future_lapply(files, get.stats, output = "standardStats", stats = "computable")
# count
computableStats <- unlist(lapply(lapply(computableStats, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(computableStats)

## extract "checkable" standardStats with get.stats()
checkableStats <- future_lapply(files, get.stats, output = "standardStats", stats = "checkable")
# count
checkableStats <- unlist(lapply(lapply(checkableStats, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(checkableStats)

## extract all sticked results from full text and abstract with study.character() for statcheck()
fullTextStats <- future_lapply(files, study.character, text.mode = 1, output = "stats")
# convert "<=>" -> "0"
fullTextStats <- lapply(fullTextStats, function(x) gsub("<=>", "=", unlist(x)))

## extract results with statcheck() on sticked results
statcheckAll <- lapply(fullTextStats, statcheck)
# count
statcheckAll <- unlist(lapply(lapply(statcheckAll, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(statcheckAll)

## apply function to remove indices from sticked results
fullTextIndex <- lapply(lapply(fullTextStats, rm.index), unlist)

## extract results from index removed sticked results with statcheck()
statcheckFullTextIndex <- lapply(fullTextIndex, statcheck)
# count
statcheckFullTextIndex <- unlist(lapply(lapply(statcheckFullTextIndex, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(statcheckFullTextIndex)

## extract results from browser generated HTML files with checkHTML
HTMLstatcheckAll <- future_lapply(HTMLfiles, checkHTML)
# count
HTMLstatcheckAll <- unlist(lapply(lapply(HTMLstatcheckAll, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(HTMLstatcheckAll)

## extract results from native PDF files with checkPDF
PDFstatcheckAll <- future_lapply(PDFfiles, checkPDF)
# count
PDFstatcheckAll <- unlist(lapply(lapply(PDFstatcheckAll, nrow), function(x) ifelse(is.null(x), 0, x)))
sum(PDFstatcheckAll)

#############################
### preperation for graphics
###########################

## sums of significant results as vector
sums <- c(sum(nStatsWicherts), 
          sum(getStatsCompSig), 
          sum(statcheckIndexSig), 
          sum(statcheckSig), 
          sum(HTMLstatcheckSig), 
          sum(PDFstatcheckSig))
# labels of extraction methods
labels <- paste0(c("Wicherts et al.", 
                   "study.character(x,text=2,stats='computable')", 
                   "statcheck() on index removed stats", 
                   "statcheck() on stats by study.character()", 
                   "statcheck::checkHTML()", 
                   "statcheck::checkPDF()"),
                 ", N=", format(sums, big.m = ",", trim = T))
method <- rep(labels, each = length(nStatsWicherts))
# factorize with reordered levels
method <- factor(method, labels[6:1])
# sums of extracted stats as vectors
nstats <- c(nStatsWicherts, 
            getStatsCompSig, 
            statcheckIndexSig, 
            statcheckSig, 
            HTMLstatcheckSig, 
            PDFstatcheckSig)

## all extracted standard results as vector
# sum of results per method
fullsums <- c(sum(getStatsAll), 
              sum(computableStats), 
              sum(checkableStats), 
              sum(statcheckFullTextIndex), 
              sum(statcheckAll), 
              sum(HTMLstatcheckAll), 
              sum(PDFstatcheckAll))
# method labels
fulllabels <- paste0(c("get.stats(x,stats='all')", 
                       "get.stats(x,stats='computable')", 
                       "get.stats(x,stats='checkable')",  
                       "statcheck() on index removed stats", 
                       "statcheck() on stats by get.stats()", 
                       "statcheck::checkHTML()", 
                       "statcheck::checkPDF()"), 
                       ", N=", format(fullsums, big.m = ",", trim = T))
# method and extracted stats as vectors
fullmethod <- rep(fulllabels, each = length(nStatsWicherts))
fullmethod <- factor(fullmethod, fulllabels[c(7:1)])
fullnstats <- c(getStatsAll, computableStats, checkableStats, statcheckFullTextIndex, statcheckAll, HTMLstatcheckAll, PDFstatcheckAll)

######################################################################################################
## Fig. 1 Total sums of extracted significant t-, F- and χ 2 -test results per method and 
## distributions of number of extracted significant t-, F-, χ 2 -test results per article and method
par(mar = c(4.5, 21.1, 1, 1), xpd = TRUE)
boxplot(nstats~method, horizontal = T, col = "lightblue", las = 1, boxwex = 0.5, ylim = c(0, 87), ylab = "", xlab = "")
text(97, -1.5, "number of extracted significant t, F, \u03c7\u00B2 results per article", pos = 2)

###################################################################################################
## Fig. 2 Total sums of extracted test results per method and distributions of number of extracted 
## results per article and method
par(mar = c(4.5, 16, 1, 1), xpd = TRUE)
boxplot(fullnstats~fullmethod, horizontal = T, col = "lightblue", las = 1, boxwex = 0.5, ylim = c(0, 120), cex.axis = .9, xlab = "", ylab = "")
text(130, -1.5, "number of extracted standard results per article", pos = 2)

