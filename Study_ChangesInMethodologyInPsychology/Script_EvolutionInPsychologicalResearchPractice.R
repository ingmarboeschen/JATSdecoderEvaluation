##########################################################################################################
## Skript to reproduce results presented in:
## Changes in methodological study characteristics in psychology between 2010-2021
##########################################################################################################

## Install 'graphing' library for function catplot, prettybarplot and stacked.area.chart
# - Install the devtools package
#install.packages("devtools")
# - Install graphing from github
#devtools::install_github("ingmarboeschen/graphing")

# remove objects from work space
rm(list=ls())

# set working directory to folder with data files
setwd("/home/data")
# load prepared lists (object names: d1-d4)
for(i in 1:4) load(paste0("data",i,".rda"))
d<-c(d1,d2,d3,d4)
length(d)


# variable names in d
names(d[[1]])
# empty result table for pre post 2015 analyses
res <- NULL

# n articles in initial selection
Ninitial <- length(d);Ninitial
format(Ninitial,big.m=",")

## N articles less equal/post 2015
# year before equal/after 2015
year <- unlist(lapply(d,"[","year"))
yearGE2016 <- year>=2016
yearGE2016 <- factor(yearGE2016,c(FALSE,TRUE),c("<=2015",">2015"))
tab <- c(table(yearGE2016),total=sum(table(yearGE2016)))
res <- rbind(res,"N articles in initial selection"=tab)
res

# remove articles with "review", "meta-analysis", "letter to the editor", "commentary", "corrigendum" in title
t <- unlist(lapply(d,"[","title"))
i <- (grep("[^a-zA-Z][Rr]eview|[Mm]eta.[Aa]nalys[ei]s|[Mm]eta[Aa]nalysis|[Ll]etter to the [Ee]ditor|[Cc]orrigendum|[Cc]ommentary",t))
# n articles removed by search terms in title
length(i)
# remove
d <- d[-i]

# select articles with statistical results
stats <- mapply(c,lapply(d,"[","stats"))
hasStats <- unlist(grepl("[<=>]",stats))

## reduce to articles with statistical results 
d <- d[hasStats]

# N articles included
N <- length(d);N
# N removed articles
Ninitial-N

## year before equal/after 2015
year <- unlist(lapply(d,"[","year"))
yearGE2016 <- year>=2016
yearGE2016 <- factor(yearGE2016,c(FALSE,TRUE),c("<=2015",">2015"))
## n articles by journal and year
journal <- factor(unlist(lapply(d,"[","journal")))
# readjust labels
lev <- levels(journal)
levNew <- gsub("Journal of","J. o.",lev)
levNew <- gsub("Child Psychology","Child Psych.",levNew)
levNew <- gsub("Personality and Social Psychology Bulletin","Pers. and Social Psych. Bull.",levNew)
journal <- factor(journal,lev,levNew)


# N articles in analysis less equal/post 2015
tab <- c(table(yearGE2016),total=sum(table(yearGE2016)))
res <- rbind(res,"N empirical research articles"=tab)
res

# proportion of PLoS One and Frontiers in Psychology
pOA <- (round(prop.table(table(is.element(journal,c("Frontiers in Psychology","PLoS ONE")))),2)*100)[2];pOA

# number auf articles by journal
tab <- table(journal,year)
xtable::xtable(addmargins(tab),digi=0)

# year with minimum of included articles by journal
min <- NULL
for(i in 1:nrow(tab)) min[i] <- names(which.min(tab[i,]))
cbind(levels(journal),min)

## frequeny of n articles by journal and year in catplot
journalF <- journal
NperJournal <- table(journal)
levels(journalF) <- paste0(levels(journal)," (N = ",format(as.numeric(NperJournal),big.mark=",",trim=TRUE),")")

# figure 1:
# recode journal levels with n per jornal
par(mar=c(4.5,4,1,1),xpd=FALSE)
graphing::catplot(year,journalF,marginleft=18,cexm=3,xlab="year of publication")
par(xpd=TRUE)
text(-1.6,.75,paste("Total number of articles:",format(length(d),big.mark=",",trim=TRUE)),pos=2)

#######################################################################
## extract n p-values, n computable p-values and n checkable p-values
stats <- lapply(lapply(d,"[","statsOnStats"),unlist)
#head(stats)

# nPvalues
nPvalues <- lapply(stats,function(x) unname(x["statsOnStats.nPvalues"]))
l <- unlist(lapply(nPvalues,length))==0
nPvalues[l] <- 0
nPvalues <- unlist(nPvalues)
nPvalues[is.na(nPvalues)] <- 0

nPcomputable <- lapply(stats,function(x) x["statsOnStats.nPcomputable"])
l <- unlist(lapply(nPcomputable,length))==0
#table(unlist(lapply(nPcomputable,length)))
nPcomputable[l] <- 0
nPcomputable <- unname(unlist(nPcomputable))
nPcomputable[is.na(nPcomputable)] <- 0
length(nPcomputable)

sum(nPcomputable>0)

nPcheckable <- lapply(stats,function(x) x["statsOnStats.nPcheckable"])
l <- unlist(lapply(nPcheckable,length))==0

nPcheckable[l] <- 0
nPcheckable <- unlist(nPcheckable)
nPcheckable[is.na(nPcheckable)] <- 0

# N articles and prop of articles with p-value
a <- paste0(format(sum(nPvalues>0),big.mark=",")," (",paste0(100*round(sum(nPvalues>0)/length(nPvalues),2),"\\%)"));a

# has p value pre/post 2016
tab <- round(prop.table(table(nPvalues>0,yearGE2016),2)[2,],2)
propP <- round(prop.table(table(nPvalues>0)),2)[2]
tab <- c(tab,propP)
res <- rbind(res,"articles with p-value/s"=tab)
res

# median n p-values pre/post2016 in articles with p-values
tab <- tapply(nPvalues[nPvalues>0],year[nPvalues>0]>=2016,median)
tab <- c(tab,median(nPvalues[nPvalues>0]))
res <- rbind(res,"-> median n p-values"=tab)
res

#  has computable p-value pre/post 2016
tab <- round(prop.table(table(nPcomputable[nPvalues>0]>0,yearGE2016[nPvalues>0]),2)[2,],2)
names(tab) <- c("<=2015",">2015")
propPcomp <- round(prop.table(table(nPcomputable[nPvalues>0]>0)),2)[2]
tab <- c(tab,propPcomp)
res <- rbind(res,"-> has recomputable p-value"=tab)
res

#  has checkable p-value pre/post 2016
tab <- round(prop.table(table(nPcheckable[nPvalues>0]>0,yearGE2016[nPvalues>0]),2)[2,],2)
names(tab) <- c("<=2015",">2015")
propPcheck <- round(prop.table(table(nPcheckable[nPvalues>0]>0)),2)[2]
tab <- c(tab,propPcheck)
res <- rbind(res,"-> has checkable p-value"=tab)
res

# figure 2:
# distrubition of n p-values per journal
par(mar=c(4.5,18,1,1))
journalF <- journal
NwithPval <- as.numeric(table(journal[nPvalues>0&!is.na(nPvalues)]))
levels(journalF) <- paste0(levels(journal),
                         " (N = ",format(NwithPval,big.mark=",",trim=TRUE),
                         #           "/",format(NperJournal,big.mark=",",trim=TRUE),
                         ")")

# revert levels for boxplot 
journalF <- factor(journalF,rev(levels(journalF)))

# boxplot n p-values per paper
boxplot(nPvalues~journalF,horizontal=T,las=1,ylab="",xlim=c(0,12.25),xlab="number of extracted p-values")
# absolute and relative number of articles with p-values
totalN <- length(d[nPvalues>0&!is.na(nPvalues)])
relN <- length(d[nPvalues>0&!is.na(nPvalues)])/length(d)
par(xpd=TRUE)
text(-20.0,-.20,paste0("N research articles with p-values: ",
                       format(totalN,big.mark=",",trim=TRUE),
                       " (",round(relN,2)*100,"%)"),pos=2)


# n articles and % articles with p-value per journal
tabJ <- table(journal)
tabJ <- round(cbind(tabJ,table(journal[nPvalues>0&!is.na(nPvalues)])/table(journal)),2)

nPvalues[nPvalues==0] <- NA
nPcomputable[nPcomputable==0] <- NA
nPcheckable[nPcheckable==0] <- NA
# add median number of p-, computable p-, checkable p-values by journal
tab <- cbind(tabJ,tapply(nPvalues,journal,median,na.rm=T))
tab <- cbind(tab,tapply(nPcomputable,journal,median,na.rm=T))
tab <- cbind(tab,tapply(nPcheckable,journal,median,na.rm=T))

colnames(tab) <- c("n articles","proportion with p-value","med n p-values","med n comp. p-values","med n checkable p-values")
tab

# add total number of p-, computable p-, checkable p-values by journal
tab <- cbind(tab,tapply(nPvalues,journal,sum,na.rm=T))
tab <- cbind(tab,tapply(nPcomputable,journal,sum,na.rm=T))
tab <- cbind(tab,tapply(nPcheckable,journal,sum,na.rm=T))
colnames(tab)[(ncol(tab)-2):ncol(tab)] <- c("n p-values","n comp. p-values","n checkable p-values")
tab

# proportion of significant computable results
stats <- mapply(c,mapply(c,lapply(d,"[","standardStats")))
p <- lapply(stats,"[[","p")
# convert all elements in stats with p>1 to NA (mostly because bad decimal use)
p <- lapply(p,function(x) if(sum(x>1,na.rm=T)) rep(NA,length(x)) else x)

p_op <- lapply(stats,"[[","p_op")
psig <- lapply(p,function(x) x<=.05)
p_opsig <- lapply(p_op,function(x) x=="<"|x=="<="|x=="=")

propSig <- rep(NA,length(psig))
l <- unlist(lapply(psig,length))
for(i in (1:length(psig))[l>0]){
  temp <- unlist(psig[i])==TRUE&unlist(p_opsig[i])==TRUE
  temp <- prop.table(table(temp))[2]
  propSig[i] <- temp
    }

res <- rbind(res,"-> median proportion of reported p's <.05"=round(c(tapply(propSig,yearGE2016,median,na.rm=T),total=median(propSig,na.rm=T)),2))
res

# global proportion of significant computable results
recalculatedP <- lapply(stats,"[[","recalculatedP")
l <- unlist(lapply(recalculatedP,length))
n <- unlist(lapply(recalculatedP,function(x) sum(!is.na(x),na.rm=T)))
nsig <- unlist(lapply(recalculatedP,function(x) sum(x<.05,na.rm=T)))
ninsig <- unlist(lapply(recalculatedP,function(x) sum(x>=.05,na.rm=T)))
nsig2 <- unlist(lapply(recalculatedP,function(x) sum(x<.01,na.rm=T)))
ninsig2 <- unlist(lapply(recalculatedP,function(x) sum(x>=.01,na.rm=T)))
nsig3 <- unlist(lapply(recalculatedP,function(x) sum(x<.001,na.rm=T)))
ninsig3 <- unlist(lapply(recalculatedP,function(x) sum(x>=.001,na.rm=T)))

propNsig <- unlist(lapply(recalculatedP,function(x) sum(x<.05,na.rm=T)/sum(!is.na(x))))
res <- rbind(res,"-> median proportion of recalculated p's <.05"=round(c(tapply(propNsig,yearGE2016,median,na.rm=T),total=median(propNsig,na.rm=T)),2))

# n significant results per journal
# proportion of significant (p<.05, p<.01, p<.001) results
tab <- round(cbind(tab,tapply(nsig,journal,sum)/(tapply(nsig,journal,sum)+tapply(ninsig,journal,sum))),2)
tab <- round(cbind(tab,tapply(nsig2,journal,sum)/(tapply(nsig2,journal,sum)+tapply(ninsig2,journal,sum))),2)
tab <- round(cbind(tab,tapply(nsig3,journal,sum)/(tapply(nsig3,journal,sum)+tapply(ninsig3,journal,sum))),2)
#colnames(tab)[(ncol(tab)-3):ncol(tab)] <- c("n p comp.<.05","prop. p comp.<.05","prop. p comp.<.01","prop. p comp.<.001")
colnames(tab)[(ncol(tab)-2):ncol(tab)] <- c("prop. comp. p<.05","prop. comp. p<.01","prop. comp. p<.001")

# add sums and global medians
tab <- addmargins(tab,1)
tab[nrow(tab),1] <- length(d)
tab[nrow(tab),2] <- round(relN,2)
tab[nrow(tab),3:5] <- c(median(nPvalues,na.rm=T),median(nPcomputable,na.rm=T),median(nPcheckable,na.rm=T))
tab[nrow(tab),6:8] <- c(sum(nPvalues,na.rm=T),sum(nPcomputable,na.rm=T),sum(nPcheckable,na.rm=T))
tab[nrow(tab),9:11] <- round(c(sum(nsig)/(sum(nsig)+sum(ninsig)),sum(nsig2)/(sum(nsig2)+sum(ninsig2)),sum(nsig3)/(sum(nsig3)+sum(ninsig3))),2)
tab <- format(tab,big.mark=",",trim=TRUE)
tab

a <- print(xtable::xtable(tab))
a <- gsub("\\.00([^0-9])","\\1",a)
a <- gsub("([^0-9])0\\.([0-9][0-9])","\\1\\2\\\\%",a)
a <- gsub("Sum","Sum, global median and proportion",a)
a <- gsub("  *"," ",a)
cat(a)

############ alpha-error in articles with p-value #####################################
alpha <- unlist(lapply(mapply(c,lapply(d,"[","alpha_error")),"[","alpha_max"))
hasNoAlpha <- unlist(lapply(alpha,is.na))

## extract n p-values, n computable p-values and n checkable p-values
statsOnStats <- lapply(d,"[","statsOnStats")
nPvalues <- unlist(lapply(statsOnStats,function(x) unlist(x)[1]))
nPvalues[is.na(nPvalues)] <- 0

# table of alpha values below 1 and above 0 that appear more than 30 times
t <- table(unlist(alpha[alpha<1&alpha>0&nPvalues>0]))
t <- t[t>30]
t <- as.table(c(t,"NA"=unname(table(unname(hasNoAlpha)[nPvalues>0])[2])))

# proportion of articles without alpha error
paste0(round(prop.table(table(unname(hasNoAlpha)[nPvalues>0]))[2],2)*100,"\\%")

par(mar=c(4.1,4,1,2),xpd=F)
graphing::prettybarplot(t,xlab="maximum alpha level",ylab="",names=names(t),cex.names=.9)

## has alpha<.05  
hits <- alpha[nPvalues>0]<.05&!is.na(alpha[nPvalues>0])&alpha[nPvalues>0]>0
alphaProp <- round(prop.table(table(hits>0)),2)
alphaProp
# before and after 2016
round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)
tab <- round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)[2,]
tab <- c(tab,alphaProp[2])
res <- rbind(res,"-> has alpha error < .05"=tab)
res

# by journal and year
tabs <- table(journal[nPvalues>0],year[nPvalues>0],hits)[,,2]
tab <- table(journal[nPvalues>0],year[nPvalues>0])
round(addmargins(tabs)/addmargins(tab),2)
a <- print(xtable::xtable(round(addmargins(tabs)/addmargins(tab),2)))
a <- gsub("0\\.",".",a)
a <- gsub("  *"," ",a) 
a <- gsub("Sum","Total",a)
cat(a)

## has alpha error <.01  
hits <- alpha[nPvalues>0]<.01&!is.na(alpha[nPvalues>0])
alphaProp <- round(prop.table(table(hits>0)),2)
alphaProp
# before and after 2016
round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)
tab <- round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)[2,]
tab <- c(tab,alphaProp[2])
res <- rbind(res,"-> has alpha error < .01"=tab)
res

# by journal and year
tabs <- table(journal[nPvalues>0],year[nPvalues>0],hits)[,,2]
tab <- table(journal[nPvalues>0],year[nPvalues>0])
round(addmargins(tabs)/addmargins(tab),2)
xtable::xtable(round(addmargins(tabs)/addmargins(tab),2))

# in table by journal
probs <- round((tabs)/(tab),2)
probs


############# confidence interval use ###################
## proportion of confidence interval use per year and journal
# in methods
hits1 <- (unlist(grepl("confidence interval",lapply(d,"[","methods"))))
table(hits1)
# in results
hits2 <- (unlist(grepl("[^a-zA-Z][[Cc][Ii][^a-zA-Z]",mapply(c,lapply(d,"[","stats")))))
table(hits2)
# total
hits <- hits1>0|hits2>0
table(hits)

## overall use of CIs
CIprop <- round(prop.table(table(hits)),2)
CIprop
# before and after 2016
tab <- round(addmargins(prop.table(table(hits,yearGE2016),2),1),2)[2,]
tab <- c(tab,CIprop[2])
tab
res <- rbind(res,"has confidence interval"=tab)
res

# use of CI by journal and year
tabs <- table(journal,year,hits)[,,2]
tab <- table(journal,year)
CI <- round(addmargins(tabs)/addmargins(tab),2)
CI
a <- print(xtable::xtable(CI))
a <- gsub("([^0-9])0\\.",".",a)
a <- gsub("  *"," ",a)
a <- gsub("Sum","Total",a)
cat(a)

################ power ##############################################
## proportion of articles with power values or analysis per year and journal
p <- mapply(c,lapply(d,"[[","power"))
m <- mapply(c,lapply(d,"[[","methods"))

hasPowerVal <- (unlist(lapply(p, function(x) length(x)>0)))
hasPA <- unlist(grepl("power ana",m))

hits <- (unlist(lapply(mapply(c,lapply(d,"[[","power")), function(x) length(x)>0)))|unlist(grepl("power ana",m))
sum(hits)
# overall
powerProp <- round(prop.table(table(hits)),2)
powerProp

# before and after 2016
round(addmargins(prop.table(table(hits,yearGE2016),2),1),2)
tab <- round(addmargins(prop.table(table(hits,yearGE2016),2),1),2)[2,]
tab <- c(tab,powerProp[2])
res <- rbind(res,"has power analysis/value"=tab)
res

# by journal and year
tabs <- table(journal,year,hits)[,,2]
tab <- table(journal,year)
power <- round(addmargins(tabs)/addmargins(tab),2)
power

# distribution of categorized first power value
power1 <- NULL
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]<.8)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]==.8)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]>.8&unlist(x)[1]<.85)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]==.85)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]>.85&unlist(x)[1]<.9)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]==.9)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]>.9&unlist(x)[1]<.95)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]==.95)),na.rm=T))
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]>.95)),na.rm=T))
# n articles with power value
power1 <- c(power1,sum(unlist(lapply(lapply(d,"[","power"),function(x) unlist(x)[1]>=0&unlist(x)[1]<=1)),na.rm=T))

power1 <- as.table(power1)
names(power1) <- c("[0; .8)",.8,"(.8; .85)",.85,"(.85; .9)",.9,"(.9; .95)",.95,"(.95; 1]","total")
power1

power1 <- rbind(power1,round(power1/sum(power1[10]),2))
rownames(power1) <- c("h(x)","f(x)")
power1


##### analysis of mentions of bayesian statistics ################
## proportion of pattern detection "bayes" per journal and year
m <- mapply(c,lapply(d,"[[","methods"))
l <- unlist(lapply(m,length))
m[l==0] <- "no detection"

# remove "information criterion" and ininformative elements from list
m <- lapply(m,function(x) grep("sample size adj|information crit|^a*bic |^bayesian$|^bayes$|schwarz bayes|[a-z]bayes",x,inv=TRUE,value=TRUE))
# table of extracted bayesian methods
bay <- sort(table(gsub("^ | $| ( )|(factor)s|highest (bayes)","\\1\\2\\3",gsub("bayes","bayes",grep("bayes",unlist(m),v=T)))),dec=T)

# extract hits
hits <- (unlist(grepl("bayes",m)))
sum(hits)

## overall mentions of bayesian statistics in articles
bayes <- round(prop.table(table(hits)),2)

# before and after 2016
tab <- round(addmargins(prop.table(table(hits,yearGE2016),2),1),2)[2,]
tab <- c(tab,bayes[2])
res <- rbind(res,"with Bayesian analysis"=tab)
res

# by journal and year
tabs <- table(journal,year,hits>0)[,,2]
tab <- table(journal,year)
bayes <- round(addmargins(tabs)/addmargins(tab),2)
bayes

# which has max
bayes[bayes==max(bayes)]
bayes==max(bayes) #  Psychology & Aging in 2019

############### correction for multiple testing ################################
## has correction for multiple testing per year and journal
hits <- (unlist(lapply(mapply(c,lapply(d,"[","multi_comparison_correction")), function(x) length(unlist(x)))))

l <- unlist(lapply(nPvalues,length))==0
nPvalues[l] <- 0
nPvalues <- unlist(nPvalues)
nPvalues[is.na(nPvalues)] <- 0

# n mentioned methods
table(hits)
d[which(hits==6)]
# overall use of any correction method
corrProp <- round(prop.table(table(hits>0)),2)
corrProp

# before and after 2016
round(addmargins(prop.table(table(hits>0,yearGE2016),2),1),2)
tab <- round(addmargins(prop.table(table(hits>0,yearGE2016),2),1),2)[2,]
tab <- c(tab,corrProp[2])
res <- round(rbind(res,"has correction for multiple testing"=tab),2)
res

# by journal and year
tabs <- table(journal,year,hits>0)[,,2]
tab <- table(journal,year)
correction <- round(addmargins(tabs)/addmargins(tab),2)
correction

# correlation of use of correction methods and number of reported Pvalues
bin <- as.numeric(hits>0)
nPvaluesC <- cut(nPvalues,c(1,2,5,10,20,40,80,200,1000))
# readjust labels
levels(nPvaluesC)[1] <- "2"
levels(nPvaluesC)[8] <- ">200"

result <- as.table(prop.table(table(bin,nPvaluesC),m=2)[2,])

par(mar=c(4,4,3,1)+.1,xpd=F)
graphing::prettybarplot(result,
        xlab="number of extracted p-values within text",
        ylab="proportion of multiple test correction use",
        ylim=c(0,1))#,main="use of multiple test correction procedures")

par(xpd=T)
i <- 1:8
text(i-.5+.2*i,rep(1.0,8),
     paste0(format(table(nPvaluesC[bin>0]),trim=T,big.m=",")," out
of ",format(table(nPvaluesC),trim=T,big.m=",")),pos=3)

# use of correction dependent on nPvalues by journal
tab <- NULL
for(i in 1:length(levels(journal))){
  tab <- rbind(tab,prop.table(table(nPvaluesC[journal==levels(journal)[i]],bin[journal==levels(journal)[i]]),m=1)[,2])
}
rownames(tab) <- levels(journal)
tab[tab=="NaN"] <- 0

cols <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075', '#808080', '#ffffff', '#000000')[1:12]

par(mar=c(4.1,4.1,2.5,3),xpd=T)
barplot(tab,beside=T,legend=F,ylim=c(0,1.05),col=cols,xlab="categorized number of p-values within text",ylab="proportion of multiple test correction use")
legend(-2,1.025,levels(journal),cex=.8,bty="n",fill=cols)
axis(4)
i <- 1:8
text(i-6.0+12*i,rep(1.04,8),
     paste0(format(table(nPvaluesC[bin>0]),trim=T,big.m=",")," out
of ",format(table(nPvaluesC),trim=T,big.m=",")),pos=3,cex=.8)
text(-4.50,1.04,"Total
articles",pos=3,cex=.8)
segments(seq(.5,26.5,by=13),0,seq(.5,26.5,by=13),.625,lty=2,col="grey")
segments(seq(.5,26.5,by=13),1.04,seq(.5,26.5,by=13),1.172,lty=2,col="grey")
segments(seq(39.5,104.5,by=13),0,seq(39.5,104.5,by=13),1.172,lty=2,col="grey")


################ test direction ##############################################
## proportion of articles with undirected or/and directed test settings
# new:
dir <- mapply(c,lapply(d,"[","test_direction"))
nc <- unlist(lapply(dir,nchar))
dir[nc==2|nc==12] <- "no detection"
direction <- unlist(dir)
table(direction)

# frequencies before and after 2015
hits <- direction=="one and two sided"|direction=="one sided"
tab <- prop.table(table(hits,yearGE2016),m=2)[2,]
tab <- round(c(tab,total=prop.table(table(hits))[2]),2)
res <- rbind(res,"has one sided test"=tab)
res

# precise frequencies before and after 2015
tab <- prop.table(table(direction,yearGE2016),m=2)
# add total frequencies
tab <- round(cbind(tab,total=prop.table(table(direction))),3)
colnames(tab) <- c("<= 2015","> 2015","Total")
tab

## frequencies of test direction by journal
tab <- prop.table(table(journal,direction),m=1)
tab <- round(rbind(tab,"Total"=prop.table(table(direction))),2)
tab

################# SAMPLE SIZE ##################
size <- unlist(lapply(mapply(c,lapply(d,"[","estimated_sample_size")),"[","estimatedSampleSize"))
sizeAbstract <- unlist(lapply(mapply(c,lapply(d,"[","estimated_sample_size")),"[","SSabstract"))
sizeStats <- unlist(lapply(mapply(c,lapply(d,"[","estimated_sample_size")),"[","SSstandardStats"))
sizeStatsN <- unlist(lapply(mapply(c,lapply(d,"[","estimated_sample_size")),"[","SSstats"))

par(mfrow=c(3,1),mar=c(4,4.5,2,2))

boxplot(sizeAbstract~factor(yearGE2016,levels(yearGE2016)[2:1]),horizontal=TRUE,las=1,ylab="",ylim=c(0,1000))
boxplot(sizeStats~factor(yearGE2016,levels(yearGE2016)[2:1]),horizontal=TRUE,las=1,ylab="",ylim=c(0,1000))
boxplot(sizeStatsN~factor(yearGE2016,levels(yearGE2016)[2:1]),horizontal=TRUE,las=1,ylab="",ylim=c(0,1000))

sizeCut <- cut(size,c(1,20,50,100,200,500,1000,10000,100000,Inf))
levels(sizeCut) <- gsub("e\\+05","00,000",gsub("e\\+04","0,000",gsub("e\\+03",",000",gsub(",","; ",levels(sizeCut)))))
levels(sizeCut)[9] <- ">100,000"
levels(sizeCut)[1] <- "\u2264 20"
a <- table(sizeCut)
#a <- rbind(a,"median proportion of recalculated p's < .05"=round(tapply(propNsig,sizeCut,median,na.rm=T),2))
a <- rbind(a,"median prop. of recalculated p's < .05 (<=2015)"=round(tapply(propNsig[yearGE2016=="<=2015"],sizeCut[yearGE2016=="<=2015"],median,na.rm=T),2))
a <- rbind(a,"median prop. of recalculated p's < .05 (>2015)"=round(tapply(propNsig[yearGE2016==">2015"],sizeCut[yearGE2016==">2015"],median,na.rm=T),2))
rownames(a)[1] <- "total detections"
a


# reversed levels for boxplot
sizeCut <- factor(sizeCut,levels(sizeCut)[length(levels(sizeCut)):1])

par(mfrow=c(1,1),mar=c(4.1,8.1,1,1))
boxplot(propNsig~sizeCut,
        xlab="proportion of recalculated p-values < .05",
        ylab="",#categorized estimated sample size",
        col="lightblue",cex.axis=1,horizontal=T,las=1)
par(xpd=T)
text(-.055,9.65,"sample size",pos=2,font=2)


temp <- journal:yearGE2016
temp <- factor(temp,levels(temp)[length(levels(temp)):1])
levels(temp)
par(mfrow=c(1,1),mar=c(4,15.5,2,2))
boxplot(size~temp,horizontal=T,las=1,ylim=c(1,10000),col=)

# median sample sizes by journal
temp <- journal:yearGE2016
tapply(size,temp,median,na.rm=T)
m1 <- matrix(tapply(size,temp,median,na.rm=T),ncol=2,byrow=T)
rownames(m1) <- levels(journal)
colnames(m1) <- levels(yearGE2016)
m1 <- cbind(m1,"factor"=round(m1[,2]/m1[,1],2))
m1

# .75 quantile sample sizes by journal
m2 <- matrix(tapply(size,temp,quantile,.75,na.rm=T),ncol=2,byrow=T)
rownames(m2) <- levels(journal)
colnames(m2) <- levels(yearGE2016)
m2 <- cbind(m2,"factor"=round(m2[,2]/m2[,1],2))
m2

m <- cbind(m1,m2)
m

m <- print(xtable::xtable(m))

################
# convert NA to 0 in extracted sample size
size[is.na(size)] <- 0
# proprtion of articles with extractable sample size
round(prop.table(table(journal,size>0),m=1)[,2],2)
prop.table(table(size>0))

# median sample size pre/post 2016
#res <- rbind(res,"n articles with extractable sample size"=c(tapply(size[size>0&!is.na(size)],yearGE2016[size>0&!is.na(size)],length),sum(size>0,na.rm=T)))
res <- rbind(res,"prop.of articles with extr. sample size"=round(c(tapply(size[size>0&!is.na(size)],yearGE2016[size>0&!is.na(size)],length),sum(size>0,na.rm=T))/c(table(yearGE2016),length(yearGE2016)),2))
res <- rbind(res,"-> median of extracted sample sizes"=c(tapply(size[size>0&!is.na(size)],yearGE2016[size>0&!is.na(size)],median,na.rm=T),median(size[size>0&!is.na(size)],na.rm=T)))
res


## median estimated sample size per journal and year
med <- NULL
for(i in 1:length(levels(journal))){
  med <- rbind(med,tapply(size[size>0&journal==levels(journal)[i]],year[size>0&journal==levels(journal)[i]],median,na.rm=T))
}
rownames(med) <- levels(journal)

# add overall median by journal and year
med <- cbind(med,"Global median"=tapply(size[size>0],journal[size>0],median,na.rm=T))
med <- rbind(med,"Global median"=c(tapply(size[size>0],year[size>0],median,na.rm=T),median(size[size>0],na.rm=T)))
round(med)

# N p-values by sample size
par(mar=c(4.1,4.1,1,2),xpd=F)
boxplot(nPvalues[which(nPvalues>0)]~cut(size,c(1,20,40,80,200,500,1000,Inf))[nPvalues>0],xlab="categorized sample size",ylab="number of p-values")

# table of categorized sample sizes
sampleTab <- rbind("h(x)="=table(cut(size,c(1,20,40,80,200,500,1000,5000,Inf))),
      "f(x)="=round(prop.table(table(cut(size,c(1,20,40,80,200,500,1000,5000,Inf)))),2))

sampleTab <- addmargins(sampleTab,2)
sampleTab

# sample size by journal
par(mar=c(4.1,13,1,3),xpd=F)
boxplot(log(size[size<10^7&size>0],10)~journal[size<10^7&size>0],horizontal=T,las=1,ylab="",axes=FALSE,xlab="estimated sample size",ylim=c(0,7))
box();axis(1,0:7,format(c(10^(0:7)),scient=F,big.m=",",trim=T));axis(2,1:12,levels(journal),las=1,cex=.85)


############### country ###################
c <- lapply(d,"[","country")
c <- lapply(c,function(x) grep("[a-z]",unlist(x),v=T))
l <- unlist(lapply(c,length))
# extraction rate of country by journal
round(prop.table(table(journal,l>0),1),2)
j <- rep(journal,times=l)
y <- rep(year,times=l)
c <- unlist(c)

# change in top 20 countries before and after 2016
pre2016 <- y<=2015
tab <- prop.table(table(pre2016,c),mar=1)
tab <- tab[,order(tab[1,],decreasing=TRUE)][,20:1]

par(mfrow=c(1,1),mar=c(4.2,8,1,2))
barplot(tab,beside=T,horiz=TRUE,las=1,xlim=c(0,.3),col=c("grey30","grey80"),xlab="relative frequency")
legend("bottomright",c("\U2264 2015","> 2015"),col=c("grey80","grey30"),pch=15,bty="n",title="published",cex=1)


## convert country of origin to continent of origin
library(countrycode)
c <- lapply(d,"[","country")
c <- lapply(c,function(x) grep("[a-z]",unlist(x),v=T))

temp <- function(x){
cc <- character(0)
if(length(x)>0){
  # convert country names to convertable format
  x <- gsub("Fr. Polynesia","French Polynesia",x)
  x <- gsub("Kosovo","Serbia",x) 
  cc <- countrycode(sourcevar=x,origin="country.name",destination="continent")
  # recode specific countries
  cc[x=="United States"|x=="Canada"] <- "USA/Canada"
  cc[x=="Israel"] <- "Israel"
  cc[x=="Australia"] <- "Australia"
  cc[x=="New Zealand"] <- "New Zealand"
  cc[x=="China"] <- "China"
  cc[x=="Greenland"] <- "Greenland"
  cc[x=="Americas"] <- "Latin America"
}
return(cc)
  }

continent <- lapply(c,temp)
continent <- lapply(continent,unique)

l <- unlist(lapply(continent,length))          
# relative frequencies of continents
tab <- round(prop.table(table(unlist(continent),rep(yearGE2016,l)),2),4)*100
tab <- tab[order(tab[,1],decreasing=T),]
tab
round(addmargins(tab,1),1)

# stacked area chart by year
par(mar=c(4,4,2,8),mfrow=c(1,1))
tab <- prop.table(table(rep(year,l),unlist(continent)),1)
graphing::stacked.area.chart(tab,reorder=T,ylim=c(0,1))

# WEIRD vs non-WEIRD origin
con2WEIRD <- function(x){
 out <- NULL
 if(length(x)>0){
 out[is.element(x,c("United States","Canada","USA/Canada","Israel","Europe","Australia","New Zealand"))] <- "WEIRD"
 out[!is.element(out,c("United States","Canada","USA/Canada","Israel","Europe","Australia","New Zealand","WEIRD"))] <- "non-WEIRD"
 out <- paste(sort(unique(out)),collapse=" & ")
 }
 return(out)
}

WEIRD <- lapply(continent,con2WEIRD)
l <- unlist(lapply(WEIRD,length))          

tab <- addmargins(round(prop.table(table(unlist(WEIRD),rep(yearGE2016,l)),2),2),1)
tab <- tab[c(3,1,2,4),]
tab

# WEIRD vs non-WEIRD in stacked area chart by year
tab <- prop.table(table(rep(year,times=l),unlist(WEIRD)),1)

par(mar=c(4.2,4.1,1,11),mfrow=c(1,1))
graphing::stacked.area.chart(tab,reorder=F,ylim=c(0,1),col=c("grey80","grey55","grey35"),xlab="year of publication",ylab="relative frequency")
axis(2,at=round(sum(tab[1,1:2]),2),las=1)



