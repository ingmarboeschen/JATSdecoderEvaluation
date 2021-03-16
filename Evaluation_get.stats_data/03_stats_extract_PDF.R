##############################################################################################################################
## JATSdecoder: Evaluation of an automated text extraction tool for statistical results in scientific reports (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#############################################################################################################################

##################################################################################################
## This script reproduces the extraction and comparison to statcheck on big PDF article collection 
#################################################################################################

rm(list=ls())
library(future.apply)
library(JATSdecoder)
library(statcheck)
plan(multisession,workers=60,gc=TRUE)
setwd("/home/tower/Ingmar/Data PDFPsychoLiteratur")#/comm_use.I-N.xml")
files<-paste0(getwd(),"/",list.files(patt="xml$",rec=T))
PDFfiles<-paste0(getwd(),"/",list.files(patt="pdf$",rec=T))

# create journal
 journal<-list.files(patt="xml$",rec=T)
 journal<-unlist(lapply(strsplit(journal,"/"),"[",2))
 journal<-gsub("Journal","J.",journal)
 journal<-gsub("Bulletin","Bul.",journal)
 lab<-unique(journal)
 journal<-factor(journal,lab)
 save(journal,file="/home/tower/Ingmar/results/journal.rda")

# extracts stats with study.character()
s<-Sys.time()
stats<-mapply(c,future_lapply(files[],study.character,output="stats",text=1))
statsTime<-Sys.time()-s;statsTime
# remove lines with "^Result in "
stats<-future_lapply(stats,function(x) grep("^\u2022 Results in ",x,inv=T,v=T))
save(stats,file="/home/tower/Ingmar/results/stats.rda")

# get all standardStats
load(file="/home/tower/Ingmar/results/stats.rda")
s<-Sys.time()
allstandardStats<-future_lapply(stats,standardStats,stat="all",T2t=T,R2r=T)
allstandardStatsTime<-Sys.time()-s;allstandardStatsTime
save(allstandardStats,file="/home/tower/Ingmar/results/allstandardStats.rda")

# get all pcomputable stats
s<-Sys.time()
computable<-future_lapply(stats,standardStats,stat="computable",T2t=T,R2r=T)
computableTime<-Sys.time()-s;computableTime
save(computable,file="/home/tower/Ingmar/results/computable.rda")
# get all pcomputable stats with estZ
s<-Sys.time()
estZ<-future_lapply(stats,standardStats,stat="computable",T2t=T,estimate=T,R2r=T)
estZTime<-Sys.time()-s;computableTime
save(estZ,file="/home/tower/Ingmar/results/estZ.rda")

# get all p checkable stats
s<-Sys.time()
checkable<-future_lapply(stats,standardStats,stat="checkable",T2t=T,estimate=F,R2r=T)
checkableTime<-Sys.time()-s;computableTime
save(checkable,file="/home/tower/Ingmar/results/checkable.rda")

s<-Sys.time()
checkableBoost<-future_lapply(stats,standardStats,stat="checkable",T2t=T,estimate=T,R2r=T)
checkableBoostTime<-Sys.time()-s;checkableBoostTime
save(checkableBoost,file="/home/tower/Ingmar/results/checkableBoost.rda")

# statcheck on "<=>" -> "=" converted stats by study.character(,out="stats")
load("/home/tower/Ingmar/results/stats.rda")
# convert "<=>" to "="
stats<-lapply(stats,function(x) gsub("<=>","=",x))
s<-Sys.time()
check<-future_lapply(stats,function(x) tryCatch(statcheck(x),error=function(e) NULL, warning=function(w) NULL))
checkTime<-Sys.time()-s;checkTime
# remove non captured p value results (ns)
check<-lapply(check,function(x) if(is.element("Reported.P.Value",names(x))) x[!is.na(x[,"Reported.P.Value"]),])
save(check,file="/home/tower/Ingmar/results/check.rda")


# statcheck on raw HTML-files
s<-Sys.time()
HTMLcheck<-future_lapply(files,function(x) tryCatch(checkHTML(x),error=function(e) NULL, warning=function(w) NULL))
HTMLtime<-Sys.time()-s
# remove non captured p value results (ns)
HTMLcheck<-lapply(HTMLcheck,function(x) if(is.element("Reported.P.Value",names(x))) x[!is.na(x[,"Reported.P.Value"]),])
save(HTMLcheck,file="/home/tower/Ingmar/results/HTMLcheck.rda")

# statcheck on raw PDF-files
s<-Sys.time()
PDFcheck<-future_lapply(PDFfiles,function(x) tryCatch(checkPDF(x),error=function(e) NULL, warning=function(w) NULL))
PDFtime<-Sys.time()-s
PDFcheck<-lapply(PDFcheck,function(x) if(is.element("Reported.P.Value",names(x))) x[!is.na(x[,"Reported.P.Value"]),])
save(PDFcheck,file="/home/tower/Ingmar/results/PDFcheck.rda")
# remove non captured p value results (ns)


# Extraction times
times<-c(statsTime,allstandardStatsTime,computableTime,estZTime,checkableTime,checkableBoostTime,checkTime,HTMLtime,PDFtime)
times
save(times,file="times.rda")


############################################################################
 setwd("/home/tower/Ingmar/results/")
 load("journal.rda")
 load("times.rda")
 load("stats.rda")
 load("allstandardStats.rda")
 load("computable.rda")
 load("estZ.rda")
 load("checkable.rda")
 load("checkableBoost.rda")
 load("HTMLcheck.rda")
 load("PDFcheck.rda")
 load("check.rda")
# time statistics 
times # total time per allgorithm
times/length(stats) # per paper
times/length(stats)*60 # per paper in second
sum(times)

## prepare n stats extracted
# n stats
n0<-lapply(stats,length)
n1<-lapply(allstandardStats,dim)
n2<-lapply(computable,dim)
n3<-lapply(estZ,dim)
n4<-lapply(checkable,dim)
n5<-lapply(checkableBoost,dim)
n6<-lapply(check,dim)
n7<-lapply(HTMLcheck,dim)
n8<-lapply(PDFcheck,dim)

# set empty to 0
l0<-unlist(lapply(n0,length)); n0[l0==0]<-0
l1<-unlist(lapply(n1,length)); n1[l1==0]<-0
l2<-unlist(lapply(n2,length)); n2[l2==0]<-0
l3<-unlist(lapply(n3,length)); n3[l3==0]<-0
l4<-unlist(lapply(n4,length)); n4[l4==0]<-0
l5<-unlist(lapply(n5,length)); n5[l5==0]<-0
l6<-unlist(lapply(n6,length)); n6[l6==0]<-0
l7<-unlist(lapply(n7,length)); n7[l7==0]<-0
l8<-unlist(lapply(n8,length)); n8[l8==0]<-0
# extract n stats as vector
nstat0<-unlist(lapply(n0,"[",1))
nstat1<-unlist(lapply(n1,"[",1))
nstat2<-unlist(lapply(n2,"[",1))
nstat3<-unlist(lapply(n3,"[",1))
nstat4<-unlist(lapply(n4,"[",1))
nstat5<-unlist(lapply(n5,"[",1))
nstat6<-unlist(lapply(n6,"[",1))
nstat7<-unlist(lapply(n7,"[",1))
nstat8<-unlist(lapply(n8,"[",1))

## Merge to data frame
d<-data.frame(stats=nstat0,standardStats=nstat1,computable=nstat2,computableBoost=nstat3,checkable=nstat4,checkableBoost=nstat5,statcheck=nstat6,checkHTML=nstat7,checkPDF=nstat8)
write.table(d,"data.txt")
d<-read.table("data.txt")

## descriptives
setwd("/home/tower/Ingmar/results/")
 load("journal.rda")
journal<-factor(journal,sort(levels(journal)))
 # documents with stats
colSums(d>0)
# stats   standardStats      computable computableBoost       checkable    checkableBoost       statcheck       checkHTML        checkPDF 
# 11900           10650            7500            7667            7133              7316            6474            2965            3072 
# total stats
colSums(d)
#  stats   standardStats      computable computableBoost       checkable          checkableBoost       statcheck       checkHTML        checkPDF 
# 488979          213680          116865          122764          106392                  111481           94612           44047           46393 
 
head(d)
# descriptives
means<-apply(d,2,function(x) mean(x[x>0],na.rm=T));means
sds<-apply(d,2,function(x) sd(x[x>0],na.rm=T));sds
medians<-apply(d,2,function(x) median(x[x>0],na.rm=T));medians
IQR<-apply(d,2,function(x) quantile(x[x>0],c(.25,.75),na.rm=T));IQR
IQR<-apply(IQR,2,function(x) paste0("[",x[1],"; ",x[2],"]"))
q99<-apply(d,2,function(x) quantile(x[x>0],c(.99),na.rm=T));q99
max<-apply(d,2,function(x) max(x[x>0],na.rm=T));max
######################################################


#######################################################################################
## hits statistic per journal and method
# n documents per journal
Nstudies<-table(journal);Nstudies
Nstudies<-c(Nstudies,sum(Nstudies))
# n documents with stats per journal and allgorithm
Ndocs<-apply(d,2,function(x) tapply(x,journal,function(x) sum(x>0,na.rm=T)))
# total stats per journal and allgorithm
Nstats<-apply(d,2,function(x) tapply(x,journal,function(x) sum(x,na.rm=T)))
statsum<-colSums(d,na.rm=T)
# relative amount of studies with stats
Fdocs<-apply(d,2,function(x) tapply(x,journal,function(x) sum(x>0,na.rm=T)/length(x)))

sums<-colSums(Ndocs,na.rm=T)
Fsums<-sums/length(journal)

Ndocs<-rbind(Ndocs,sums)
Fdocs<-rbind(Fdocs,Fsums)
# format as scientific with ","
Ndocs<-format(Ndocs,big.m=",")
Nstudies<-format(Nstudies,big.m=",")
Nstats<-format(Nstats,big.m=",")
statsum<-format(statsum,big.m=",")

# paste % with N
tab<-Ndocs
for(i in 1:dim(tab)[1]) tab[i,]<-paste0(tab[i,]," (",round(Fdocs[i,],3)*100,"%)");tab
tab<-cbind(Nstudies,tab)
dim(tab)

# save
library(xtable)
setwd("/home/tower/Ingmar/results/")
print(xtable(tab),file="PDFhits.txt")
#print(xtable(tab[,c(1:7)]),file="PDFhits1.txt")
#print(xtable(tab[,c(1,8:10)]),file="PDFhits2.txt")

# total stats with descriptives
descriptives<-rbind(Nstats,sum=statsum,mean=round(means,1),sd=round(sds,1),median=medians,IQR=IQR,quantile99=round(q99,1),max=max,timeinsecs=round(times*60),secsperpaper=round(times*60/as.numeric(gsub(",","",dim(d)[1])),2))
n<-rownames(descriptives)
descriptives<-cbind(c(Nstudies,rep("",8)),descriptives)
rownames(descriptives)<-n
descriptives[,6:10]
print(xtable(descriptives),file="PDFNstats.txt")

#######################################################################################
# checkableBoost-checkPDF
dif1<-nstat4-nstat8
# checkableBoost-checkHTML
dif2<-nstat4-nstat7
# checkableBoost-statcheck(get.stats())
dif3<-nstat4-nstat6

# n articles with less capture in checkableBoost vs checkPDF
sum(dif1<0) # 895 -> 753 -> 676 -> 613 -> 558 -> 533 -> 395 -> 352 -> 333 -> 358
sum(dif1==0) # 6563 -> 6547 -> 6601 -> 6410 -> 6095 -> 6106 -> 6112 -> 6096 -> 6373
sum(dif1>0) # 4905 -> 4988 -> 5144 -> 5145 -> 5361 -> 5814 ->  5840 -> 5875 -> 5564
table(dif1)
table(dif1)[1:20]
i<-which(dif1==-92)
standardStats[i]
stats[i]

# less capture in checkableBoost vs checkHTML
sum(dif2<0) # 510 -> 428 -> 300 -> 283 ->278 -> 131 -> 95 -> 80 -> 59 -> 89
sum(dif2==0) # 6817 -> 6864 -> 6657 -> 6338 -> 6354 -> 6357 -> 6346 -> 6668
sum(dif2>0) # 5143 ->5357 -> 5369 -> 5835 -> 5855 ->  5867 -> 5899 -> 5537
table(dif2)[1:10]
i<-which(dif2==-8)[1]
get.stats(files[i],stats="checkable")
checkHTML(files[i])

# less capture in checkableBoost vs statcheck(get.stats())
sum(dif3<0) # 19
sum(dif3==0) # 9111
sum(dif3>0) # 3167
table(dif3)
i<-which(dif3==-7)[1]
dif3[i]
study.character(files[i],stats="checkable")
check[i]

# fullstat/checkPDF´
sum(nstat5)/sum(nstat8) # 2.47
# fullstat/checkHTML
sum(nstat5)/sum(nstat7) # 2.6
# fullstat/statcheck(get.stats())
sum(nstat5)/sum(nstat6) # 1.2

# articles where checkPDF and checkHTM but not standardStats found stats to check
j<-which(dif1<0)
a<-read.csv("/home/tower/Ingmar/Data PDFPsychoLiteratur/indexAbweichungen.csv")
ind<-j[!is.element(j,a[,1])]
length(ind)
cbind(ind,dif3[ind])
library(JATSdecoder)
i<-6924
dif3[i]
study.character(files[i],text.mode=1,stat="checkable",out=c("stats","standardStats"),T2t=T)
statcheck(gsub("<=>","=",study.character(files[i],text.mode=1,out=c("stats"))))[8:13]
(checkPDF(PDFfiles[i]))[,10]
checkHTML(files[i])
standardStats("F=1.17, U=1.46; t(33)=.829, p=.413, BF=2.353" )
text2sentences(JATSdecoder(x<-files[i])$text[4])
grep("121",get.text(files[i]))
# reason for dif1<0
checked<-c(
i<-3700, # 11, 
4069, # 11, 
i<-5783, # 13, 
5960, # 11, 
7546, # 11, 
i<-2423
) ## -12, 14 t values with index "p4-c2" not captured by study.character but captured as chi2 statistic by statcheck, same only twice in study.character

