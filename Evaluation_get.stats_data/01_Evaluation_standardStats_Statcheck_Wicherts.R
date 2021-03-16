########################################################################################################################
## Comparison of number of extracted significant t-, F-, Chi2 results in method and result section/s 
## in 49 papers by Wicherts et al. (2011) by study.character() and statchecks functions statcheck(), checkHTML() and checkPDF()
rm(list=ls())
library(JATSdecoder);library(statcheck)
library(xtable);library(future.apply)
plan(multisession,workers=4,gc=TRUE)  
## 49 articles analysed by "Wicherts et al."
setwd("/home/ingmar/JATSdecoderEvaluation/get.stats/Wicherts/49 papers")
files<-paste0(getwd(),"/",list.files(,".cerm",rec=T))
HTMLfiles<-paste0(getwd(),"/",list.files(,".html",rec=T))
PDFfiles<-paste0(getwd(),"/",list.files(,".pdf",rec=T))
# number of extracted significant results by paper by Wicherts
nStatsWicherts<-read.table("/home/ingmar/JATSdecoderEvaluation/get.stats/Wicherts/02_nStatsWicherts.csv",head=T)[,1]

## function to remove indices from t-, F-, Chi2-results
rm.index<-function(x){
  # remove number of numbered F-values
  noIndex<-gsub("([ \\[\\(])F[0-9]*?\\(([1-9])|^F[0-9]*?\\(([1-9])","\\1F(\\2\\3",unlist(x))
  noIndex<-gsub(",F", ", F",noIndex)
  # remove letter of labeled F-values in lines with (df1,df2)
  if(length(grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)",noIndex))>0){ 
    noIndex[grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)",noIndex)]<-gsub("([ \\[\\(])F[a-zA-Z ]*?\\(([1-9])|^F[a-zA-Z ]*?\\(([1-9])","\\1F(\\2\\3",noIndex[grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)",noIndex)])}
  # remove number of numbered t-values
  noIndex<-gsub("( )t[0-9]*?\\(([1-9])|^t[0-9]*?\\(([1-9])","\\1t(\\2\\3",noIndex)
  noIndex<-gsub("( )t[a-zA-Z]\\(([1-9])|^t[a-zA-Z]\\(([1-9])","\\1t(\\2\\3",noIndex)
  # remove text from chi_text(12)=
  if(length(grep("^chi[A-Za-z]| chi[A-Za-z]",noIndex))>0) noIndex[grep("^chi[A-Za-z]| chi[A-Za-z]",noIndex)]<-gsub("chi[A-Za-z]*?([\\(=])","chi\\1",noIndex[grep("^chi[A-Za-z]| chi[A-Za-z]",noIndex)])
  return(noIndex)
}

#################################################################################################
## extract significant t- F-, Ch2-results in method and result section/s with study.character()
getStatsComp<-mapply(c,future_lapply(files[],function(x) study.character(x,text.mode=2,out="standardStats",stats="computable",select=c("t","F","Chi2"))))
# extract p and recalculatedP to reduce to significant results only
ps<-unlist(lapply(lapply(getStatsComp,names),function(x) is.element("p",x)))
recP<-unlist(lapply(lapply(getStatsComp,names),function(x) is.element("recalculatedP",x)))
# create binary output variable
getStatsCompSig<-NA
# set cases with no p-values to 0
getStatsCompSig[which((ps+recP)==0)]<-0 
# with reported and recomputable p value
getStatsCompSig[which((ps+recP)==2)]<-lapply(getStatsComp[which((ps+recP)==2)],function(x) x[which(as.numeric(x[,"p"])<=.05|as.numeric(x[,"recalculatedP"])<=.05),])
# with reported p value only
getStatsCompSig[which(ps==1&recP==0)]<-lapply(getStatsComp[which(ps==1&recP==0)],function(x) x[which(as.numeric(x[,"p"])<=.05),])
# with recomputable p value only
getStatsCompSig[which(ps==0&recP==1)]<-lapply(getStatsComp[which(ps==0&recP==1)],function(x) x[which(as.numeric(x[,"recalculatedP"])<=.05),])
# count significant results
getStatsCompSig<-unlist(lapply(lapply(getStatsCompSig,nrow),function(x) ifelse(is.null(x),0,x)))
sum(getStatsCompSig)

## extract raw sticked results from method and result section/s with study.character() for analysis with statcheck()
MethResStats<-future_lapply(files,study.character,text.mode=2,output="stats")
# convert "<=>" -> "0"
MethResStats<-lapply(MethResStats,function(x) gsub("<=>","=",unlist(x)))
# extract number of extracted results with statcheck()
statcheckMethRes<-future_lapply(MethResStats,statcheck,stat = c("t", "F", "chisq"))
statcheckSig<-lapply(statcheckMethRes,function(a) a[which(a$Reported.P.Value<=.05|a$Computed<=.05),])
# count significant results
statcheckSig<-unlist(lapply(lapply(statcheckSig,nrow),function(x) ifelse(is.null(x),0,x)))
sum(statcheckSig)

# apply function to remove indices
MethResStatsIndex<-lapply(MethResStats,rm.index)
# extract stats from index removed results in method and result sections
statcheckMethResIndex<-lapply(MethResStatsIndex,statcheck,stat=c("t","F","chisq"))
# recuce to significant results
statcheckIndexSig<-lapply(statcheckMethResIndex,function(a) a[which(a$Reported.P.Value<=.05|a$Computed<=.05),])
statcheckIndexSig<-unlist(lapply(lapply(statcheckIndexSig,nrow),function(x) ifelse(is.null(x),0,x)))
sum(statcheckIndexSig)

## extract all significant stats from browser generated HTML files with checkHTML()
checkHTMLstats<-future_lapply(HTMLfiles,checkHTML,stat = c("t","F","chisq"))
# recuce to significant results
checkHTMLsig<-lapply(checkHTMLstats,function(a) a[which(a$Reported.P.Value<=.05|a$Computed<=.05),])
# count significant results
checkHTMLsig<-unlist(lapply(lapply(checkHTMLsig,nrow),function(x) ifelse(is.null(x),0,x)))
sum(checkHTMLsig)

## extract all significant stats with checkPDF() from pdf files
checkPDFsig<-future_lapply(PDFfiles,checkPDF)
# recuce to significant results
checkPDFsig<-lapply(checkPDFsig,function(a) a[which(a$Reported.P.Value<=.05|a$Computed<=.05),])
# count significant results
checkPDFsig<-unlist(lapply(lapply(checkPDFsig,nrow),function(x) ifelse(is.null(x),0,x)))
sum(checkPDFsig)

#########################
## create result vectors
NstatsSig<-c(nStatsWicherts,getStatsCompSig,statcheckIndexSig,statcheckSig,checkHTMLsig,checkPDFsig)
settings<-c("Wicherts et al.",
"study.character(x,text=2,stats='computable')",
"statcheck() on index removed stats",
"statcheck() on stats by study.character()",
"statcheck::checkHTML()","statcheck::checkPDF()")
methodSig<-factor(rep(settings,each=49),settings)
# sum of detected results per setting
tapply(NstatsSig,methodSig,sum)
# add sum per setting
settings<-paste0(settings,", N=",format(tapply(NstatsSig,methodSig,sum),big.m=",",trim=T))
# update levels
methodSig<-factor(methodSig,levels(methodSig)[6:1],settings[6:1])

################################################################
# Visualisation of only significant t-, F-, Chi2-statistics in method in result section/s
par(mar=c(4,19,.5,.5),xpd=TRUE)
boxplot(NstatsSig~methodSig,horizontal=T,col="lightblue",las=1,boxwex=0.5,
        ylim=c(0,110),xlab="",ylab="",cex.axis=.9)#,xlab="number of extracted significant t, F, \u03c7\u00B2 results per article")
text(120,-1.5,"number of extracted significant t, F, \u03c7\u00B2 results per article",pos=2,cex=1)

#################################################################################################
## Extract standard results from full text with get.stats() and statcheck functions
#################################################################################################
## extract "all" standardStats results stats with get.stats()
getStatsAll<-lapply(files,get.stats,output="standardStats",stats="all")
# count results
getStatsAll<-unlist(lapply(lapply(getStatsAll,nrow),function(x) ifelse(is.null(x),0,x)))
sum(getStatsAll)

## extract "computable" standardStats results stats with get.stats()
computableStats<-future_lapply(files,get.stats,output="standardStats",stats="computable")
# count results
computableStats<-unlist(lapply(lapply(computableStats,nrow),function(x) ifelse(is.null(x),0,x)))
sum(computableStats)

## extract "checkable" standardStats results stats with get.stats()
checkableStats<-future_lapply(files,get.stats,output="standardStats",stats="checkable")
# count results
checkableStats<-unlist(lapply(lapply(checkableStats,nrow),function(x) ifelse(is.null(x),0,x)))
sum(checkableStats)

## extract number of results from sticked results with statcheck()
# sticked results with study.character()
fullTextStats<-future_lapply(files,study.character,text.mode=1,output="stats")
# convert "<=>" -> "0"
fullTextStats<-lapply(fullTextStats,function(x) gsub("<=>","=",unlist(x)))
# apply statcheck()
statcheckAll<-lapply(fullTextStats,statcheck)
# count results
statcheckAll<-unlist(lapply(lapply(statcheckAll,nrow),function(x) ifelse(is.null(x),0,x)))
sum(statcheckAll)

## extract stats from index removed results in method and result sections
# apply function to remove indices
fullTextIndex<-lapply(lapply(fullTextStats,rm.index),unlist)
# apply statcheck()
statcheckFullTextIndex<-lapply(fullTextIndex,statcheck)
# count results
statcheckFullTextIndex<-unlist(lapply(lapply(statcheckFullTextIndex,nrow),function(x) ifelse(is.null(x),0,x)))
sum(statcheckFullTextIndex)

## extract results with checkHTML()
HTMLstatcheckAll<-future_lapply(HTMLfiles,checkHTML)
# count results
HTMLstatcheckAll<-unlist(lapply(lapply(HTMLstatcheckAll,nrow),function(x) ifelse(is.null(x),0,x)))
sum(HTMLstatcheckAll)

## extract results with checkPDF()
PDFstatcheckAll<-future_lapply(PDFfiles,checkPDF)
# count results
PDFstatcheckAll<-unlist(lapply(lapply(PDFstatcheckAll,nrow),function(x) ifelse(is.null(x),0,x)))
sum(PDFstatcheckAll)

#########################
## create result vectors
Nstats<-c(getStatsAll,computableStats,checkableStats,statcheckFullTextIndex,statcheckAll,HTMLstatcheckAll,PDFstatcheckAll)
settings<-c(
            "get.stats(x,stats='all')",
            "get.stats(x,stats='computable')",
            "get.stats(x,stats='checkable')",
            "statcheck() on index removed stats",
            "statcheck() on stats by get.stats()",
            "statcheck::checkHTML()",
            "statcheck::checkPDF()")
method<-factor(rep(settings,each=49),settings)
# sum of detected results per setting
tapply(Nstats,method,sum)
# add sum per setting
settings<-paste0(settings,", N = ",format(tapply(Nstats,method,sum),big.m=",",trim=T))
# update levels
method<-factor(method,levels(method)[7:1],settings[7:1])

# Visualisation of all extracted standard results
par(mar=c(4.,17.5,.5,.5),xpd=TRUE)
boxplot(Nstats~method,horizontal=T,col="lightblue",las=1,boxwex=0.5,ylim=c(0,110),
        xlab="",ylab="",cex.axis=.95)#,xlab="number of extracted significant t, F, \u03c7\u00B2 results per article")#,main="49 articles of study by Wicherts et al.")
text(120,-1.75,"number of extracted results per article",pos=2)

#######################################################################################
## Table of causes for differences in significant t-,F-,Chi2-results to Wicherts et al.
cause<-read.csv("/home/ingmar/JATSdecoderEvaluation/get.stats/Wicherts/01_Ursachen.Differenzen.Wicherts.csv")
rownames(cause)<-NULL
dif<-getStatsCompSig-nStatsWicherts
sum(dif)
which(dif<0)
which(dif>0)
# rearrange table
cause[["capture"]]<-as.character(cause[["capture"]])
cause$Wicherts<-c(nStatsWicherts[dif>0],nStatsWicherts[dif<0])
cause$study.character<-c(getStatsCompSig[dif>0],getStatsCompSig[dif<0])
cause$dif<-c(dif[dif>0],dif[dif<0])
cause<-(cause)[,c(1,12:14,3:8,10,11,9)]
# add colSums
cause<-rbind(cause,c("","","",colSums(cause[,-1:-3],na.rm=T)))
# output
cause

