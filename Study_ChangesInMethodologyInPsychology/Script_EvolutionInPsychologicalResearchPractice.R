##########################################################################################################
## Script to reproduce results presented in:
## Changes in methodological study characteristics in psychology between 2010-2021
##########################################################################################################

## Install 'devtools' to install 'graphing' library for function catplot, prettybarplot and stacked.area.chart
# - install the 'devtools' package
#install.packages("devtools")
# - install 'graphing' from github
#devtools::install_github("ingmarboeschen/graphing")

# remove objects from work space
rm(list=ls())

# set working directory to folder with data files (adjust to your system)
setwd("/home/data")
# load prepared lists (object names: d1-d4)
for(i in 1:4) load(paste0("data",i,".rda"))
d <- c(d1,d2,d3,d4)

# names of variables
names(d[[1]])

#########################################
### preparation and data selection #####
#######################################
# empty result object to store results of pre post 2015 analyses (Table 1)
res <- NULL

# N articles in initial selection
Ninitial <- length(d); format(Ninitial,big.m=",")

## N articles less equal/post 2015
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
length(d)

# store index for selection of abstracts with preregistration
iPrereg <- -i

# select articles with statistical results
stats <- mapply(c,lapply(d,"[","stats"))
hasStats <- unlist(grepl("[<=>]",stats))

## reduce to articles with statistical results 
d <- d[hasStats]

# N articles included
N <- length(d); format(N,big.m=",")

# N removed articles
Ninitial-N

## year before equal/after 2015
year <- unlist(lapply(d,"[","year"))
yearGE2016 <- year>=2016
yearGE2016 <- factor(yearGE2016,c(FALSE,TRUE),c("<=2015",">2015"))

## N articles by journal and year
journal <- factor(unlist(lapply(d,"[","journal")))
# finetune labels
lev <- levels(journal)
levNew <- gsub("Journal of","J. o.",lev)
levNew <- gsub("Child Psychology","Child Psych.",levNew)
levNew <- gsub("Personality and Social Psychology Bulletin","Pers. and Social Psych. Bull.",levNew)
journal <- factor(journal,lev,levNew)

################################
###### Start of analysis ######
##############################
# N articles in analysis less equal/post 2015
tab <- c(table(yearGE2016),total=sum(table(yearGE2016)))
res <- rbind(res,"N empirical research articles"=tab)
res

# proportion of PLoS One and Frontiers in Psychology
pOA <- (round(prop.table(table(is.element(journal,c("Frontiers in Psychology","PLoS ONE")))),2)*100)[2];pOA

# number auf articles by journal
tab <- table(journal,year)
addmargins(tab)
addmargins(tab)

# year of publication with the fewest articles by journal
minN <- NULL
for(i in 1:nrow(tab)) minN[i] <- names(which.min(tab[i,]))
data.frame(levels(journal),minN)

## frequeny of n articles by journal and year in catplot
journalF <- journal
NperJournal <- table(journal)
levels(journalF) <- paste0(levels(journal)," (N = ",format(as.numeric(NperJournal),big.mark=",",trim=TRUE),")")

# Figure 1: Number of yearly released research articles by journal and year
# recode journal levels with n per jornal
par(mar=c(4.5,4,1,1),xpd=FALSE)
graphing::catplot(year,journalF,marginleft=18,cexm=3,xlab="year of publication")
par(xpd=TRUE)
text(-1.6,.75,paste("Total number of articles:",format(length(d),big.mark=",",trim=TRUE)),pos=2)

##################################
##### the use of p-values #######
################################
## extract n p-values, n computable p-values and n checkable p-values
stats <- lapply(lapply(d,"[","statsOnStats"),unlist)
# extract and adjust number of p-values per article
nPvalues <- lapply(stats,function(x) unname(x["statsOnStats.nPvalues"]))
l <- unlist(lapply(nPvalues,length))==0
nPvalues[l] <- 0
nPvalues <- unlist(nPvalues)
nPvalues[is.na(nPvalues)] <- 0

# extract and adjust number of computable p-values per article
nPcomputable <- lapply(stats,function(x) x["statsOnStats.nPcomputable"])
l <- unlist(lapply(nPcomputable,length))==0
nPcomputable[l] <- 0
nPcomputable <- unname(unlist(nPcomputable))
nPcomputable[is.na(nPcomputable)] <- 0

# extract and adjust number of checkable p-values per article
nPcheckable <- lapply(stats,function(x) x["statsOnStats.nPcheckable"])
l <- unlist(lapply(nPcheckable,length))==0
nPcheckable[l] <- 0
nPcheckable <- unlist(nPcheckable)
nPcheckable[is.na(nPcheckable)] <- 0

# N articles and proportion of articles with p-value
a <- paste0(format(sum(nPvalues>0),big.mark=",")," (",paste0(100*round(sum(nPvalues>0)/length(nPvalues),2),"\\%)"));a

# proportion of articles with p-value pre/post 2016
tab <- round(prop.table(table(nPvalues>0,yearGE2016),2)[2,],2)
propP <- round(prop.table(table(nPvalues>0)),2)[2]
tab <- c(tab,propP)
res <- rbind(res,"proportion of articles with with p-value/s"=tab)
res

# median N p-values pre/post2016 per study in articles with p-values
Nstudies <- unlist(lapply(d,"[","Nstudies"))


## proportion of articles with more than one study
multiStudy <- Nstudies>1
all <- prop.table(table(multiStudy[nPvalues]))[2]; all
# by journal
round(prop.table(table(journal,multiStudy),m=1)[,2],2)
# by time (>2015)
tab <- prop.table(table(multiStudy=multiStudy[nPvalues>0],year=year[nPvalues>0]>=2016),m=2)
tab <- round(c(tab[2,],all),2)
res <- rbind(res,"-> proportion of articles with multiple studies"=tab)
res

tab <- tapply(nPvalues[nPvalues>0]/Nstudies[nPvalues>0],year[nPvalues>0]>=2016,median)
tab <- c(tab,median(nPvalues[nPvalues>0]/Nstudies[nPvalues>0],na.rm=T))

res <- rbind(res,"-> median number of p-values per study"=tab)
res

#  has computable p-value pre/post 2016
tab <- round(prop.table(table(nPcomputable[nPvalues>0]>0,yearGE2016[nPvalues>0]),2)[2,],2)
names(tab) <- c("<=2015",">2015")
propPcomp <- round(prop.table(table(nPcomputable[nPvalues>0]>0)),2)[2]
tab <- c(tab,propPcomp)
res <- rbind(res,"-> proportion of articles with recomputable p-value"=tab)
res

# proportion of studies with checkable p-values pre/post 2016
tab <- round(prop.table(table(nPcheckable[nPvalues>0]>0,yearGE2016[nPvalues>0]),2)[2,],2)
names(tab) <- c("<=2015",">2015")
propPcheck <- round(prop.table(table(nPcheckable[nPvalues>0]>0)),2)[2]
tab <- c(tab,propPcheck)
res <- rbind(res,"-> proportion of articles with checkable p-value"=tab)
res

# Figure 2: Number of articles with p-values and distribution of number of extracted p-values per study by journal
par(mar=c(4.5,19,1,1))
journalF <- journal
NwithPval <- as.numeric(table(journal[nPvalues>0&!is.na(nPvalues)]))
levels(journalF) <- paste0(levels(journal),
                         " (N = ",format(NwithPval,big.mark=",",trim=TRUE),
                         #           "/",format(NperJournal,big.mark=",",trim=TRUE),
                         ")")

# revert levels for boxplot 
journalF <- factor(journalF,rev(levels(journalF)))

# boxplot n p-values per paper
boxplot(nPvalues/Nstudies~journalF,horizontal=T,las=1,ylab="",xlim=c(0,12.25),xlab="number of extracted p-values per study")
# absolute and relative number of articles with p-values
totalN <- length(d[nPvalues>0&!is.na(nPvalues)])
relN <- length(d[nPvalues>0&!is.na(nPvalues)])/length(d)
par(xpd=TRUE)
text(-20.0,-.20,paste0("N research articles with p-values: ",
                       format(totalN,big.mark=",",trim=TRUE),
                       " (",round(relN,2)*100,"%)"),pos=2)


# number and proportion of articles with p-value per journal
tabJ <- table(journal)
tabJ <- round(cbind(tabJ,table(journal[nPvalues>0&!is.na(nPvalues)])/table(journal)),2)

nPvalues[nPvalues==0] <- NA
nPcomputable[nPcomputable==0] <- NA
nPcheckable[nPcheckable==0] <- NA
# median number of p-, computable p-, checkable p-values per study by journal
tab <- cbind(tabJ,tapply(nPvalues/Nstudies,journal,median,na.rm=T))
tab <- cbind(tab,tapply(nPcomputable/Nstudies,journal,median,na.rm=T))
tab <- cbind(tab,tapply(nPcheckable/Nstudies,journal,median,na.rm=T))

colnames(tab) <- c("n articles","proportion with p-value","med n p-values","med n comp. p-values","med n checkable p-values")
tab

# add total number of p-, computable p-, checkable p-values by journal
tab <- cbind(tab,tapply(nPvalues,journal,sum,na.rm=T))
tab <- cbind(tab,tapply(nPcomputable,journal,sum,na.rm=T))
tab <- cbind(tab,tapply(nPcheckable,journal,sum,na.rm=T))
colnames(tab)[(ncol(tab)-2):ncol(tab)] <- c("n p-values","n comp. p-values","n checkable p-values")
tab

## proportion of results reported as significant at alpha < .05
stats <- mapply(c,mapply(c,lapply(d,"[","standardStats")))
p <- lapply(stats,"[[","p")
# convert all elements in stats with p>1 to NA (mostly because bad decimal use)
p <- lapply(p,function(x) if(sum(x>1,na.rm=T)) rep(NA,length(x)) else x)
# extract operator
p_op <- lapply(stats,"[[","p_op")
# is operator indicating significance
p_opsig <- lapply(p_op,function(x) x=="<"|x=="<="|x=="=")
# is p <= .05
psig <- lapply(p,function(x) x<=.05)

propSig <- rep(NA,length(psig))
l <- unlist(lapply(psig,length))
for(i in (1:length(psig))[l>0]){
  temp <- unlist(psig[i])==TRUE&unlist(p_opsig[i])==TRUE
  temp <- prop.table(table(temp))[2]
  propSig[i] <- temp
    }

res <- rbind(res,"-> median proportion of reported p < .05"=round(c(tapply(propSig,yearGE2016,median,na.rm=T),total=median(propSig,na.rm=T)),2))
res

## proportion of computable p < .05
recalculatedP <- lapply(stats,"[[","recalculatedP")
l <- unlist(lapply(recalculatedP,length))
n <- unlist(lapply(recalculatedP,function(x) sum(!is.na(x),na.rm=T)))
# p < .05
nsig <- unlist(lapply(recalculatedP,function(x) sum(x<.05,na.rm=T)))
ninsig <- unlist(lapply(recalculatedP,function(x) sum(x>=.05,na.rm=T)))
# p < .01
nsig2 <- unlist(lapply(recalculatedP,function(x) sum(x<.01,na.rm=T)))
ninsig2 <- unlist(lapply(recalculatedP,function(x) sum(x>=.01,na.rm=T)))
# p < .001
nsig3 <- unlist(lapply(recalculatedP,function(x) sum(x<.001,na.rm=T)))
ninsig3 <- unlist(lapply(recalculatedP,function(x) sum(x>=.001,na.rm=T)))

propNsig <- unlist(lapply(recalculatedP,function(x) sum(x<.05,na.rm=T)/sum(!is.na(x))))
res <- rbind(res,"-> median proportion of computable p < .05"=round(c(tapply(propNsig,yearGE2016,median,na.rm=T),total=median(propNsig,na.rm=T)),2))
res

# N significant results per journal
# proportion of significant (p<.05, p<.01, p<.001) results
tab <- round(cbind(tab,tapply(nsig,journal,sum)/(tapply(nsig,journal,sum)+tapply(ninsig,journal,sum))),2)
tab <- round(cbind(tab,tapply(nsig2,journal,sum)/(tapply(nsig2,journal,sum)+tapply(ninsig2,journal,sum))),2)
tab <- round(cbind(tab,tapply(nsig3,journal,sum)/(tapply(nsig3,journal,sum)+tapply(ninsig3,journal,sum))),2)
#colnames(tab)[(ncol(tab)-3):ncol(tab)] <- c("n p comp.<.05","prop. p comp.<.05","prop. p comp.<.01","prop. p comp.<.001")
colnames(tab)[(ncol(tab)-2):ncol(tab)] <- c("prop. comp. p<.05","prop. comp. p<.01","prop. comp. p<.001")

# add sums and global medians
relN <- length(d[nPvalues>0&!is.na(nPvalues)])/length(d)
tab <- addmargins(tab,1)
tab[nrow(tab),1] <- length(d)
tab[nrow(tab),2] <- round(relN,2)
tab[nrow(tab),3:5] <- c(median(nPvalues/Nstudies,na.rm=T),median(nPcomputable/Nstudies,na.rm=T),median(nPcheckable/Nstudies,na.rm=T))
tab[nrow(tab),6:8] <- c(sum(nPvalues,na.rm=T),sum(nPcomputable,na.rm=T),sum(nPcheckable,na.rm=T))
tab[nrow(tab),9:11] <- round(c(sum(nsig)/(sum(nsig)+sum(ninsig)),sum(nsig2)/(sum(nsig2)+sum(ninsig2)),sum(nsig3)/(sum(nsig3)+sum(ninsig3))),2)
tab[,3]<-round(as.numeric(tab[,3]),1)
tab[,4]<-round(as.numeric(tab[,4]),1)
tab[,5]<-round(as.numeric(tab[,5]),1)
tab <- format(tab,big.mark=",",trim=TRUE)

a <- print(xtable::xtable(tab))
a <- gsub("\\.00([^0-9])","\\1",a)
a <- gsub("([^0-9])0\\.([0-9][0-9])","\\1\\2\\\\%",a)
a <- gsub("(\\.[0-9])0([^0-9])","\\1\\2",a)
a <- gsub("Sum","Sum, global median and proportion",a)
a <- gsub("  *"," ",a)

# Table 2: Journal specific properties of extracted raw, computable and checkable p-values within articles that contain any statistical result
cat(a)

##############################################
## change in reporting of effect sizes #####
############################################
stats <- mapply(c,mapply(c,lapply(d,"[","standardStats")))
m <- mapply(c,lapply(d,"[[","methods"))

# check if has effect meassure
Cohensd <- unlist(lapply(stats,function(x) sum(is.element(colnames(x),"d"))))
eta2 <- unlist(lapply(stats,function(x) sum(is.element(colnames(x),"eta2"))))
beta <- unlist(lapply(stats,function(x) sum(is.element(colnames(x),"beta"))))
OR <- unlist(lapply(stats,function(x) sum(is.element(colnames(x),"OR"))))

# check if has appropiate method
Ttest <- unlist(lapply(m,function(x) length(grep("^t test| t test",x))>0))
anova <- unlist(lapply(m,function(x) length(grep("^anova| anova",x))>0))
regression <- unlist(lapply(m,function(x) length(grep("^regression| regression",x))>0))

# proportion of effect size per method pre/post 2015
tab <- rbind(CohensdinTtest=prop.table(table(Cohensd[Ttest==TRUE],yearGE2016[Ttest==TRUE]),m=2)[2,])
tab <- rbind(tab,eta2inANOVA=prop.table(table(eta2[anova==TRUE],yearGE2016[anova==TRUE]),m=2)[2,])
tab <- rbind(tab,betaORinRegression=prop.table(table((beta[regression==TRUE]+OR[regression==TRUE])>0,yearGE2016[regression==TRUE]),m=2)[2,])
round(tab,2)
total <- as.vector(t(tab))

## prepare Table 3: Proportion of articles that report standard effect sizes in studies with t-test, ANOVA and regression analysis
# Cohen's d
table(journal[Ttest==TRUE],Cohensd[Ttest==TRUE],yearGE2016[Ttest==TRUE])
a <- (table(journal[Ttest==TRUE],Cohensd[Ttest==TRUE],yearGE2016[Ttest==TRUE]))[,,1]
tab <- cbind(d_before2016=a[,2]/rowSums(a))
a <- (table(journal[Ttest==TRUE],Cohensd[Ttest==TRUE],yearGE2016[Ttest==TRUE]))[,,2]
tab <- cbind(tab,d_after2015=a[,2]/rowSums(a))
tab <- cbind(tab,factor=tab[,2]/tab[,1])
round(tab,2)
# eta^2
a <- (table(journal[anova==TRUE],eta2[anova==TRUE],yearGE2016[anova==TRUE]))[,,1]
tab <- cbind(tab,eta2_before2016=a[,2]/rowSums(a))
a <- (table(journal[anova==TRUE],eta2[anova==TRUE],yearGE2016[anova==TRUE]))[,,2]
tab <- cbind(tab,eta2_after2015=a[,2]/rowSums(a))
tab <- cbind(tab,factor=tab[,5]/tab[,4])
tab
# beta
a <- (table(journal[regression==TRUE],beta[regression==TRUE],yearGE2016[regression==TRUE]))[,,1]
tab <- cbind(tab,betaOR_before2016=a[,2]/rowSums(a))
a <- (table(journal[regression==TRUE],beta[regression==TRUE],yearGE2016[regression==TRUE]))[,,2]
tab <- cbind(tab,betaOR_after2015=a[,2]/rowSums(a))
tab <- cbind(tab,factor=tab[,8]/tab[,7])
# add total with factor
facD <- total[2]/total[1]
facEta <- total[4]/total[3]
facBeta <- total[6]/total[5]

tab <- rbind(tab,Total=c(total[1:2],facD,
                         total[3:4],facEta,
                         total[5:6],facBeta))

round(tab,2)


# has effect measure by journal in articles with anova|regression|Ttest
hasProc<-anova|regression|Ttest
hasEffect<-Cohensd|eta2|beta|OR
table(hasProc,hasEffect)
prop.table(table(hasProc[year>2015],hasEffect[year>2015]),m=1)[2,]
table(journal[hasProc],hasEffect[hasProc])
table(year[hasProc],hasEffect[hasProc])
byJournal<-round(prop.table(table(journal[hasProc],hasEffect[hasProc]),m=1)[,2],2);byJournal
byYear<-round(prop.table(table(year[hasProc],hasEffect[hasProc]),m=1)[,2],2);byYear

effect<-table(journal[hasProc],year[hasProc],hasEffect[hasProc])[,,2]/
  table(journal[hasProc],year[hasProc])

effect<-cbind(effect,byJournal)

effect<-rbind(effect,c(byYear,round(prop.table(table(hasEffect&hasProc,hasProc),m=2)[2,2],2)))

## Table 3: Proportion of articles that report standard effect sizes in studies with t-test, ANOVA and regression analysis
round(effect,2)

######################################################
##### alpha-error in articles with p-value ##########
####################################################

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

# Figure 3: Distribution of maximum α levels extracted over 30 times
par(mar=c(4.1,4,1,2),xpd=F)
graphing::prettybarplot(t,xlab="maximum alpha level",ylab="",names=names(t),cex.names=.9)

## has alpha level < .05  
hits <- alpha[nPvalues>0]<.05&!is.na(alpha[nPvalues>0])&alpha[nPvalues>0]>0
alphaProp <- round(prop.table(table(hits>0)),2)
alphaProp
# before and after 2016
round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)
tab <- round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)[2,]
tab <- c(tab,alphaProp[2])
res <- rbind(res,"-> proportion with alpha level < .05"=tab)
res

# by journal and year
tabs <- table(journal[nPvalues>0],year[nPvalues>0],hits)[,,2]
tab <- table(journal[nPvalues>0],year[nPvalues>0])
round(addmargins(tabs)/addmargins(tab),2)
a <- print(xtable::xtable(round(addmargins(tabs)/addmargins(tab),2)))
a <- gsub("0\\.",".",a)
a <- gsub("  *"," ",a) 
a <- gsub("Sum","Total",a)

# Table 4: Relative frequency of extracted maximum α levels < .05 in articles with p-values
cat(a)

## proportion with alpha level < .01  
hits <- alpha[nPvalues>0]<.01&!is.na(alpha[nPvalues>0])
alphaProp <- round(prop.table(table(hits>0)),2)
alphaProp
# before and after 2016
round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)
tab <- round(addmargins(prop.table(table(hits>0,yearGE2016[nPvalues>0]),2),1),2)[2,]
tab <- c(tab,alphaProp[2])
res <- rbind(res,"-> proportion with alpha level < .01"=tab)
res

# by journal and year
tabs <- table(journal[nPvalues>0],year[nPvalues>0],hits)[,,2]
tab <- table(journal[nPvalues>0],year[nPvalues>0])
round(addmargins(tabs)/addmargins(tab),2)

# in table by journal
probs <- round((tabs)/(tab),2)
probs

#############################################
######### confidence interval use ##########
###########################################
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
res <- rbind(res,"proportion of articles with confidence interval"=tab)
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

# Table 5: Relative frequency of confidence interval use in articles with p-values
cat(a)

###############################################
###### application of power analysis #########
#############################################
## proportion of articles with power values or analysis per year and journal
p <- mapply(c,lapply(d,"[[","power"))
m <- mapply(c,lapply(d,"[[","methods"))

hasPowerVal <- (unlist(lapply(p, function(x) length(x)>0)))
hasPA <- unlist(grepl("power ana",m))

hits <- (unlist(lapply(mapply(c,lapply(d,"[[","power")), function(x) length(x)>0)))|unlist(grepl("power ana",m))

# overall stats of power usage
sum(hits)
powerProp <- round(prop.table(table(hits)),2); powerProp

# before and after 2016
round(addmargins(prop.table(table(hits,yearGE2016),2),1),2)
tab <- round(addmargins(prop.table(table(hits,yearGE2016),2),1),2)[2,]
tab <- c(tab,powerProp[2])
res <- rbind(res,"proportion of articles with power analysis/value"=tab)
res

# by journal and year
tabs <- table(journal,year,hits)[,,2]
tab <- table(journal,year)
power <- round(addmargins(tabs)/addmargins(tab),2)

# Table 6: Relative frequency of power values or mentions of power analysis by journal and year in articles with p-values
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

# Table 7: Absolute (h(x)) and relative (f(x)) frequency distribution of the first detected and categorized power value per article
power1

###########################################################
##### analysis of mentions of bayesian statistics ########
#########################################################
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

# Table 8: Relative frequency of application of Bayesian inferential statistics by journal and year
bayes

# which has max
bayes[bayes==max(bayes)]
bayes==max(bayes) #  Psychology & Aging in 2019

#############################################
########## use of preregistration ##########
###########################################
# Due to file size and copyright concerns the data of abstracts is not made publicy available. 
# Please contact the author to provide the data. 
load("/home/ingmar/JATSdecoderEvaluation/04_Psychology/data/fullDataAbstract.rda") 
# reduce data
fullData <- fullData[iPrereg]
fullData <- fullData[hasStats]
# extract data
abstractPrereg <- lapply(fullData,"[","abstract")
titlePrereg <- lapply(fullData,"[","title")
yearPrereg <- unlist(lapply(fullData,"[","year"))
yearPrereg <- factor(yearPrereg,as.character(2010:2021))
journalPrereg <- factor(unlist(lapply(fullData,"[","journal")))
# finetune journal labels
lev <- levels(journalPrereg)
levNew <- gsub("Journal of","J. o.",lev)
levNew <- gsub("Child Psychology","Child Psych.",levNew)
levNew <- gsub("Personality and Social Psychology Bulletin","Pers. and Social Psych. Bull.",levNew)
journalPrereg <- factor(journalPrereg,lev,levNew)

# has registered report/replication/study in title
i1<-grepl("registered report|registered replication|registered stud[yi]",titlePrereg)
sum(i1)
# has preregistered report/replication/study in title
i2<-grepl("pre[- ]*registered report|pre[- ]*registered replication|pre[- ]*registered stud",titlePrereg)
sum(i2)

# has registered report/replication/study in abstract
j1<-grepl("registered report|registered replication|registered stud[yi]",abstractPrereg)
sum(j1)

# has preregistered report/study in abstract
j2<-grepl("pre[- ]*registered report|pre[- ]*registered replication|pre[- ]*registered stud",abstractPrereg)
sum(j2)


# frequency of registered reports
i<-(i1&!i2)|(j1&!j2)
sum(i)

# frequency of preregistered reports
j<-i2|j2
sum(j)

# frequency of registered or preregistered reports by journal and year
sum(i|j)

# relative frequency of preregistrated reports
sum(i|j)/length(d)


table(journalPrereg[i|j])

# Table 9: Absolute frequency of preregistered or registered reports by journal and year
addmargins(table(journalPrereg[i|j],yearPrereg[i|j]))

# the first preregistered articles in 2013
titlePrereg[i|j][journalPrereg[i|j]=="Frontiers in Psychology"&yearPrereg[i|j]==2013]

# frequency of 'replication' in title or abstract
sum(grepl(" replicat",abstractPrereg[i|j])|grepl(" replicat",titlePrereg[i|j]))
# relative frequency of 'replication' in title or abstract
sum(grepl(" replicat",abstractPrereg[i|j])|grepl(" replicat",titlePrereg[i|j]))/sum(i|j)
25/82

# absolute frequency of replications
sum(grepl(" replicat",abstractPrereg[])|grepl(" replicat",titlePrereg[]))
# relative frequency of replications
1992/length(abstractPrereg)
# relative frequency of preregistered replications of replications
25/1992

# increase factor of preregistration in ordinary and replication studies
(25/1992)/((sum(i|j)-25)/(length(d)-1992))

#########################################################
######## use of corrections for multiple testing #######
#######################################################
## has correction for multiple testing per year and journal
hits <- (unlist(lapply(mapply(c,lapply(d,"[","multi_comparison_correction")), function(x) length(unlist(x)))))

# n mentioned methods
table(hits)
proc <- lapply(d,"[","multi_comparison_correction")
table(unlist(proc))
# by year >2015 in articles with correction
y <- rep(yearGE2016[hits>0], times=hits[hits>0])

round(addmargins(prop.table(table(unlist(proc),y),m=2),1),2)

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

# Table 10: Relative frequency of at least one detected multiple test correction procedure by journal and year in articles with p-values
correction



# correlation of use of correction methods and number of reported Pvalues
stats <- mapply(c,lapply(d,"[","statsOnStats"))
nPvalues <- lapply(stats,function(x) unname(x["nPvalues"]))
l <- unlist(lapply(nPvalues,length))==0
nPvalues[l] <- 0
nPvalues <- unlist(nPvalues)
nPvalues[is.na(nPvalues)] <- 0
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

# Figure 4: Absolute frequencies of articles with correction procedures by categorized number of p-values (numbers on top) and journal-wise relation
# of number of extracted p-values from text and use of correction procedures for multiple testing (bars)
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

############################################
####### use of one-sided testing ##########
#########################################
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

# frequencies before and after 2015
tab <- prop.table(table(direction,yearGE2016),m=2)
# add total frequencies
tab <- round(cbind(tab,total=prop.table(table(direction))),3)
colnames(tab) <- c("<= 2015","> 2015","Total")
tab

## frequencies of test direction by journal
tab <- prop.table(table(journal,direction),m=1)
tab <- round(rbind(tab,"Total"=prop.table(table(direction))),2)

# Table 11: Relative frequencies of detected test direction by journal
tab

##############################################
##### change in estimated sample size #######
############################################
size <- unlist(lapply(mapply(c,lapply(d,"[","estimated_sample_size")),"[","estimatedSampleSize"))

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

# Table 12: Median and .75-quantile of estimated sample size before and in 2015 and after 2015 by journal
m

## median sample size pre/post 2016
size[is.na(size)] <- 0
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

# Table 13: Median estimated sample size by journal and year
round(med)

# Table 1: Change in study characteristics in research articles with statistical results before and after 2015
res

###################################################
########## change in country of origin ###########
#################################################
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

# Figure 5: Change in relative country involvement before and after 2015
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

# Figure 6: Change in WEIRD and non-WEIRD country of origin over time
par(mar=c(4.2,4.1,1,11),mfrow=c(1,1))
graphing::stacked.area.chart(tab,reorder=F,ylim=c(0,1),col=c("grey80","grey55","grey35"),xlab="year of publication",ylab="relative frequency")
axis(2,at=round(sum(tab[1,1:2]),2),las=1)


