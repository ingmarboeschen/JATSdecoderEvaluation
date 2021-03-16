################################################################################################################################
## JATSdecoder: Evaluation of an automated text extraction tool for statistical results in scientific reports (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#############################################################################################################################

#######################################################################################################
## This script reproduces the extraction and comparison to statcheck on all articles of 2 PMC journals 
#####################################################################################################

# prepare
rm(list=ls())
library(future.apply)
library(JATSdecoder)
require(statcheck)
plan(multisession,workers=60,gc=TRUE)

# get file names from folder with PLoS One articles 
folder<-"./PMC/comm_use.O-Z.xml/PLoS_One" # adjust to your system
PLoS<-paste0(getwd(),"/",list.files(folder=folder,patt="xml$",rec=T))
nPLoS<-length(PLoS)
# create subset with type=="research" & psychology relatedness
meta<-future_lapply(PLoS[],JATSdecoder,output=c("type","subject","keywords"))
keywords<-mapply(c,lapply(meta,"[","keywords"))
subject<-mapply(c,lapply(meta,"[","subject"))
type<-mapply(c,lapply(meta,"[","type"))
# create index for selection
index1<-unlist(grepl("research",type))
index2<-unlist(grepl("[Pp]sych",subject))
index3<-unlist(grepl("[Pp]sych",keywords))
indexPLoS<-which(index1&index2|index1&index3)

#############################################################
## Start
nPLoS<-length(indexPLoS);nPLoS
PLoS<-PLoS[indexPLoS]
length(PLoS)
# Frontiers in Psychology file names
folder="./PMC/comm_use.C-H.xml/Front_Psychol" # adjust to your system
front<-paste0(getwd(),"/",list.files(folder=folder,patt="xml$",rec=T))
nfront<-length(front)

# merge file names
files<-c(front,PLoS)

# create journal vector
journal<-rep(c("Frontiers in Psychology","PLoS One"),times=c(nfront,nPLoS))
table(journal)

################################################
## extraction of statistical results from files
##############################################

# with study.character()
s<-Sys.time()
stats<-mapply(c,future_lapply(files,study.character,output="stats",text=1))
statsTime<-Sys.time()-s;statsTime
# remove lines with "^Result in "
stats<-future_lapply(stats,function(x) grep("^\u2022 Results in ",x,inv=T,v=T))
save(stats,file="/home/tower/Ingmar/results/statsXML.rda")
save(statsTime,file="/home/tower/Ingmar/results/statsTimeXML.rda")

# get all standardStats
load(file="/home/tower/Ingmar/results/statsXML.rda")
s<-Sys.time()
allstandardStats<-future_lapply(stats,standardStats,stat="all",T2t=T,R2r=T)
allstandardStatsTime<-Sys.time()-s;allstandardStatsTime
save(allstandardStats,file="/home/tower/Ingmar/results/allstandardStatsXML.rda")
save(allstandardStatsTime,file="/home/tower/Ingmar/results/allstandardStatsTimeXML.rda")

# get all pcomputable stats
s<-Sys.time()
computable<-future_lapply(stats,standardStats,stat="computable",T2t=T,R2r=T)
computableTime<-Sys.time()-s;computableTime
save(computable,file="/home/tower/Ingmar/results/computableXML.rda")
save(computableTime,file="/home/tower/Ingmar/results/computableTimeXML.rda")

# get all pcomputable stats
s<-Sys.time()
estZ<-future_lapply(stats,standardStats,stat="computable",T2t=T,estimate=T,R2r=T)
estZTime<-Sys.time()-s;estZTime
save(estZ,file="/home/tower/Ingmar/results/estZXML.rda")
save(estZTime,file="/home/tower/Ingmar/results/estZTimeXML.rda")

# get all p checkable stats
s<-Sys.time()
checkable<-future_lapply(stats,standardStats,stat="checkable",T2t=T,R2r=T)
checkableTime<-Sys.time()-s;checkableTime
save(checkable,file="/home/tower/Ingmar/results/checkableXML.rda")
save(checkableTime,file="/home/tower/Ingmar/results/checkableTimeXML.rda")

# with est=T and T2t=T
s<-Sys.time()
checkableBoost<-future_lapply(stats,standardStats,stat="checkable",T2t=T,estimate=T,R2r=T)
checkableBoostTime<-Sys.time()-s;checkableBoostTime
save(checkableBoost,file="/home/tower/Ingmar/results/checkableBoostXML.rda")
save(checkableBoostTime,file="/home/tower/Ingmar/results/checkableBoostXMLTime.rda")

# statcheck stats by study.character(,out="stats") with added space to front
load("/home/tower/Ingmar/results/statsXML.rda")
s<-Sys.time()
check<-future_lapply(stats,function(x) tryCatch(statcheck(x),error=function(e) NULL, warning=function(w) NULL))
checkTime<-Sys.time()-s;checkTime
# remove non captured p value results (ns)
check<-lapply(check,function(x) if(is.element("Reported.P.Value",names(x))) x[!is.na(x[,"Reported.P.Value"]),])
save(check,file="/home/tower/Ingmar/results/checkXML.rda")
save(checkTime,file="/home/tower/Ingmar/results/checkTimeXML.rda")


# statcheck on raw HTML-files
s<-Sys.time()
HTMLcheck<-future_lapply(files,function(x) tryCatch(checkHTML(x),error=function(e) NULL, warning=function(w) NULL))
HTMLtime<-Sys.time()-s
# remove non captured p value results (ns)
HTMLcheck<-lapply(HTMLcheck,function(x) if(is.element("Reported.P.Value",names(x))) x[!is.na(x[,"Reported.P.Value"]),])
save(HTMLcheck,file="/home/tower/Ingmar/results/HTMLcheckXML.rda")
save(HTMLtime,file="/home/tower/Ingmar/results/HTMLtimeXML.rda")

############################################################################
setwd("/home/tower/Ingmar/results/")
 load("journalXML.rda")            ;load("filesXML.rda")
load("statsXML.rda")               ; load("statsTimeXML.rda")
 load("allstandardStatsXML.rda")   ; load("allstandardStatsTimeXML.rda")
 load("computableXML.rda")         ; load("computableTimeXML.rda")
 load("estZXML.rda")               ; load("estZTimeXML.rda")
 load("checkableXML.rda")          ; load("checkableTimeXML.rda")
 load("checkableBoostXML.rda")     ; load("checkableBoostXMLTime.rda")
 load("HTMLcheckXML.rda")          ; load("HTMLtimeXML.rda")
 load("checkXML.rda")              ; load("checkTimeXML.rda")
 load("timesXML.rda")              
# Extraction times
timesXML<-c(statsTime,allstandardStatsTime,computableTime,estZTime,checkableTime,checkableBoostTime,checkTime,HTMLtime)
save(timesXML,file="/home/tower/Ingmar/results/timesXML.rda")
timesXML
 
# time statistics 
timesXML # total time per allgorithm
timesXML/length(stats) # per paper in mins
timesXML/length(stats)*60 # per paper in secs

############################
### count n stats extracted
##########################
# n stats
n0<-lapply(stats,length)
n1<-lapply(allstandardStats,dim)
n2<-lapply(computable,dim)
n3<-lapply(estZ,dim)
n4<-lapply(checkable,dim)
n5<-lapply(checkableBoost,dim)
n6<-lapply(check,dim)
n7<-lapply(HTMLcheck,dim)
# set empty to 0
l0<-unlist(lapply(n0,length)); n0[l0==0]<-0
l1<-unlist(lapply(n1,length)); n1[l1==0]<-0
l2<-unlist(lapply(n2,length)); n2[l2==0]<-0
l3<-unlist(lapply(n3,length)); n3[l3==0]<-0
l4<-unlist(lapply(n4,length)); n4[l4==0]<-0
l5<-unlist(lapply(n5,length)); n5[l5==0]<-0
l6<-unlist(lapply(n6,length)); n6[l6==0]<-0
l7<-unlist(lapply(n7,length)); n7[l7==0]<-0
# extract n stats as vector
nstat0<-unlist(lapply(n0,"[",1))
nstat1<-unlist(lapply(n1,"[",1))
nstat2<-unlist(lapply(n2,"[",1))
nstat3<-unlist(lapply(n3,"[",1))
nstat4<-unlist(lapply(n4,"[",1))
nstat5<-unlist(lapply(n5,"[",1))
nstat6<-unlist(lapply(n6,"[",1))
nstat7<-unlist(lapply(n7,"[",1))

length(nstat7)
## Merge to data frame
d<-data.frame(stats=nstat0,standardStats=nstat1,computable=nstat2,computableBoost=nstat3,checkable=nstat4,checkableBoost=nstat5,statcheck=nstat6,checkHTML=nstat7)
dim(d)
write.table(d,"dataXML.txt")

## descriptives
setwd("/home/tower/Ingmar/results/")
 load("journalXML.rda")
 load("timesXML.rda")
 load("filesXML.rda")
d<-read.table("dataXML.txt")
attach(d)
# documents with stats
colSums(d>0)
#          stats   standardStats      computable computableBoost       checkable  checkableBoost       statcheck       checkHTML 
#          34021           30312           16320           16583           15749           16020           13388            6604 
# total stats
colSums(d)
#          stats   standardStats      computable computableBoost       checkable checkableBoost       statcheck       checkHTML 
#         910297          580060          226888          234210          214558          220930          172281           59181 
 
# descriptives
means<-apply(d,2,function(x) mean(x[x>0]));means
sds<-apply(d,2,function(x) sd(x[x>0]));sds
medians<-apply(d,2,function(x) median(x[x>0]));medians
IQR<-apply(d,2,function(x) quantile(x[x>0],c(.25,.75)));IQR
IQR<-apply(IQR,2,function(x) paste0("[",x[1],"; ",x[2],"]"))
q99<-apply(d,2,function(x) quantile(x[x>0],c(.99)));q99
max<-apply(d,2,function(x) max(x[x>0]));max

#######################################################################################
## hits statistic per journal and method
# n documents per journal
Nstudies<-table(journal);Nstudies
if(length(table(journal))>1) Nstudies<-c(Nstudies,sum(Nstudies))
table(journal);dim(d)
# n documents with stats per journal and allgorithm
Ndocs<-apply(d,2,function(x) tapply(x,journal,function(x) sum(x>0)))
# total stats per journal and allgorithm
Nstats<-apply(d,2,function(x) tapply(x,journal,function(x) sum(x)))
statsum<-colSums(d)
# relative amount of studies with stats
Fdocs<-apply(d,2,function(x) tapply(x,journal,function(x) sum(x>0)/length(x)))

if(is.null(dim(Ndocs))) sums<-Ndocs else sums<-colSums(Ndocs)
Fsums<-sums/length(journal)

if(!is.null(dim(Ndocs))) Ndocs<-rbind(Ndocs,sums) 
if(!is.null(dim(Ndocs))) Fdocs<-rbind(Fdocs,Fsums)

# format as scientific with ","
Ndocs<-format(Ndocs,big.m=",")
Nstudies<-format(Nstudies,big.m=",")
Nstats<-format(Nstats,big.m=",")
statsum<-format(statsum,big.m=",")

# paste % with N
if(is.null(dim(Ndocs))) tab<-t(as.data.frame(Ndocs)) else tab<-Ndocs
if(is.null(dim(Fdocs))) Fdocs<-t(as.data.frame(Fdocs))
tab
if(!is.null(dim(tab))) for(i in 1:dim(tab)[1]) tab[i,]<-paste0(tab[i,]," (",round(Fdocs[i,],3)*100,"%)");tab
if(!is.null(dim(tab))) tab<-cbind(Nstudies,tab);tab
if(!is.null(dim(tab))) tab<-cbind(tab,"");tab

# save
library(xtable)
setwd("/home/tower/Ingmar/results/")
print(xtable(tab),file="XMLhits.txt")
#print(xtable(tab)[,c(1:7)],file="XMLhits1.txt")
#print(xtable(tab)[,c(1,8:9)],file="XMLhits2.txt")


# total stats with descriptives
descriptives<-rbind(Nstats,sum=statsum,mean=round(means,1),sd=round(sds,1),median=medians,IQR=IQR,quantile99=round(q99,1),max=max,timeinminutes=round(timesXML,1),secsperpaper=round(timesXML*60*60/as.numeric(gsub(",","",length(stats))),2))
n<-rownames(descriptives)
descriptives<-cbind(c(Nstudies,rep("",8)),descriptives)
rownames(descriptives)<-n
descriptives
print(xtable(descriptives),file="XMLNstats.txt")


# factor computable vs checkable statcheck
sum(d$computable)/sum(d$statcheck) # 1.335
# factor checkable vs statcheck(get.stats())
sum(d$checkableBoost)/sum(d$statcheck) # 1.292
# factor checkable vs checkHTML
sum(d$checkableBoost)/sum(d$checkHTML) # 3.83
# factor checkable statcheck(get.stats()) vs checkHTML
sum(d$checkableBoost)/sum(d$statcheck) # 1.29

