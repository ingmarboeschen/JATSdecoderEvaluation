####################################################################################
## Script to reproduce the analysis presented in:
## Evaluation of the extraction of methodical study characteristics with JATSdecoder. 
## Böschen, I. (2022). 
####################################################################################
## Comparison of study.character()'s output with manually coded data and: 
## Blanca, M. J., Alarcón, R. & Bono, R. Current Practices in Data Analysis 
## Procedures in Psychology: What Has Changed? Frontiers in Psychology 9 (2018).
## URL https://www.frontiersin.org/article/10.3389/fpsyg.2018.02558.
#################################################################################

## install JATSdecoder with devtools
# devtools::install_git("https://github.com/ingmarboeschen/JATSdecoder")
# if devtools is not installed, run: install.packages("devtools"); devtools::install_git("https://github.com/ingmarboeschen/JATSdecoder")

## load packages
library(JATSdecoder)
library(xtable) # or: install.packages("xtable");library(xtable)
library(wordcloud) # or: install.packages("wordcloud");library(wordcloud)

## download data files from repository and unpack on your system:
## https://github.com/ingmarboeschen/JATSdecoderEvaluation/blob/main/Evaluation_study.character_data/data.zip

# set working directory
setwd("/home/Evaluation_study.character_data/")

## load data 
# manually coded study characteristics
data <- read.csv("manualStudyCharacteristics.csv")
# study.character()'s extractions
load(file="study.character.rda")
# alpha level extraction with 'p2alpha=TRUE'
load(file="p2alphaTRUE.rda") 
# frequency table of methods by Blanca et al.
DAP <- read.table("blancaTableDAP.txt",sep="\t",header=T)


## function to compare list elements and compute CP/FP/CN/FN rates
compare <- function(x,y,sums=FALSE){
  if(length(x)!=length(y)){
    return(warning("lists must be of same length"))}else{
      # convert empty cells and "" to NA
      x[unlist(lapply(x,length))==0] <- NA
      y[unlist(lapply(y,length))==0] <- NA
      x[unlist(grepl("^$|^NA$",x))] <- NA
      y[unlist(grepl("^$|^NA$",y))] <- NA
      # extract false/correct hits
      CP <- unlist(lapply(1:length(x),function(i) sum(!is.na(x[i])&!is.na(y[i])&is.element(x[[i]],y[[i]])))      )
      CN <- unlist(lapply(1:length(x),function(i) sum(is.na(x[i])&is.na(y[i]))))
      FP <- unlist(lapply(1:length(x),function(i) sum(!is.na(x[i])&!is.element(x[[i]],y[[i]]))))
      FN <- unlist(lapply(1:length(x),function(i) sum(!is.na(y[[i]])&!is.element(y[[i]],x[[i]]))))
      d <- data.frame(CP,CN,FP,FN)
      if(sums==TRUE) return(c(CP=unname(colSums(d)[1]),CN=unname(colSums(d)[2]),FP=unname(colSums(d)[3]),FN=unname(colSums(d)[4])))
      if(sums!=TRUE) return(d)
    }
}

## function to detect false detection rates by level of output
falseDetections<-function(a,b,names,type="FP"){
  if(is.null(names)&type=="FP") names<-unique(unlist(a))
  if(is.null(names)&type=="FN") names<-unique(unlist(b))
  names<-gsub("\\(|\\[|\\)|\\]","",names)
  res<-NULL
  for(i in 1:length(names)){
    # is name[i] present in a
    aTrue<-unlist(lapply(a, function(x) length(grep(paste0("^",names[i],"$"),gsub("\\(|\\[|\\)|\\]","",x)))>0))
    # is name[i] present in a
    bTrue<-unlist(lapply(b, function(x) length(grep(paste0("^",names[i],"$"),gsub("\\(|\\[|\\)|\\]","",x)))>0))
    if(type=="FP") res[i]<-sum(aTrue&!bTrue)
    if(type=="FN") res[i]<-sum(!aTrue&bTrue)
  }
  names(res)<-names
  return(res)
}

##########################################
## identify and remove duplicated article
########################################
i <- which(duplicated(unlist(lapply(character,"[","title"))));i
data <- data[-i,]
character <- character[-i]
p2alpha <- p2alpha[-i]

#############################################################################
## compare statistical methods/tests extracted with Table 4 in Blanca et al.
############################################################################
# extracted methods by get. method()
method <- mapply(c,lapply(character,"[","methods"))

## Table 2. Number of articles with mentions of specific statistical methods extracted with study.character 
##          and frequency of main analytical methods reported in Blanca et al.’s TABLE 4

# 1. Block: general methods
# define search patterns
names_1 <- c("descriptive statistics|descriptive analysis|descriptives",
             "distributional analysis|distribution analysis|distribution fitting",
             "inter rater reliability|kappa|intraclass correlation|intra class correlation",
             "pearson correlation|pearson product|product moment|zero order correlation|^correlation$",
             "correlation comparison|coefficient comparison",
             "spearman corr|spearman brown|spearman coef|spearman rank|spearman rho",
             "gamma|cramer v|somer d|contingency table analysis|contingency coef",
             "^chi square|[^d] chi square",
             "wald chi square",
             "fisher exact|fisher z|mcnemar|cochran q|z statistic",
             "mann whitney|mannwhitney|u test",
             "wilcoxon|signed rank",
             "kruskal |wallis ",
             "friedman test",
             "one sample t test|single sample t test",
             "^t test$|[^n][^e] t test|independent t test|two sample t test", 
             "paired samples t test|paired sample t test|paired t test",
             " anova|^anova",
             " ancova|^ancova",
             "manova|mancova")

# extract hits
test_1 <- lapply(method,which.term,names_1)
test_1 <- t(mapply(cbind,test_1))
colnames(test_1) <- names_1

cbind(study.character=colSums(test_1))

## 2. Block: regression
# reduce to elements with regressions|model|estimat
reg <- lapply(method,get.sentence.with.pattern,"regression|model|estimat")
# define search terms
names_reg <- c("regression",
               "multilevel.*?regression|hierarchic.*?regression|mixed.*?regression",
               "multivariate.*?regres|multiple*?regres",
               "poisson regression",
               "log.*?regression",
               "multilevel logistic|logistic multilevel",
               "multinom.*?regres|ordin.*?regres",
               "gee|generalized estimation equation")

# extract hits
test_reg <- lapply(reg,which.term,names_reg)
test_reg <- t(mapply(cbind,test_reg))
colnames(test_reg) <- names_reg

cbind(study.character=colSums(test_reg))

## 3. Block: SEM
# define search terms
names_sem <- c("path analysis|path model|path coefficient|path estimate|structural equation",
               "multilevel structural equation",
               "growth curve|growth model",
               "multilevel growth|multigroup.*?growth",
               "confirmatory factor",
               "exploratory factor")

# extract hits
test_sem <- lapply(method,which.term,names_sem)
test_sem <- t(mapply(cbind,test_sem))
colnames(test_sem) <- names_sem

cbind(study.character=colSums(test_sem))

# 4. Block: other
# define search terms
names_other <- c("cronbach alpha|reliability coeff|cronbach coeff",
                 "mcdonald|mc donald|omega coef|omega estimate",
                 "test retest reliability|test retest corr",
                 "convergent validity|discriminant validity|[^a-z]ave |^ave |msv",
                 "sensitivity|specificity|specifity",
                 "item analysis|items analysis",
                 "item response analysis|dif analysis",
                 "cluster analysis",
                 "roc curve|receiver operating",
                 "markov|marcov")

# extract hits
test_other <- lapply(method,which.term,names_other)
test_other <- t(mapply(cbind,test_other))
colnames(test_other) <- names_other

cbind(study.character=colSums(test_other))

# merge to final table
# get.method()'s hits
test_total <- cbind(test_1,test_reg,test_sem,test_other)
cbind(study.character=colSums(test_total))

# add comparison with Table 4 by Blanca et al.
res <- colSums(test_total)
dat <- data.frame(searchterm=paste0("'",names(res),"'"),Blanca=DAP[,2],study.character=colSums(test_total),difference=colSums(test_total)-DAP[,2],check.names=F)
rownames(dat) <- DAP[,1]
dat
print(xtable(dat,digit=0,rownames=F))

## methods that are the same, less and more often extracted by get.method()
# same often
sum((dat[,"difference"])==0) 
rownames(dat)[(dat[,"difference"])==0]
# less often with get.method()
sum((dat[,"difference"])<0) 
rownames(dat)[(dat[,"difference"])<0]
# more often with get.method()
sum((dat[,"difference"])>0) 
rownames(dat)[(dat[,"difference"])>0]

## Figure 1. Word cloud of the extracted statistical methods by study.character
meth<-table(unlist(method))
par(mar=c(0,0,0,0))
wordcloud(names(meth),meth)


#################################################################################ä##
## Sensitivity, specifity and accuracy analysis of extracted study characteristics
#################################################################################

# empty object for Table 3. Sensitivity, specificity and accuracy of study.character’s extractions
res <- data.frame()

####################################
## alphas from confidence intervals
##################################
# extract from study.character() result
alphaJATSci <- mapply(c,lapply(mapply(c,lapply(character,"[","alpha_error")),"[","alpha_from_CI"))
alphaJATSci <- lapply(alphaJATSci,sort)
# extract from manual analysis
alphaCI_hand <- lapply(strsplit(data$alpha_from_CI,"; "),function(x) as.numeric(unlist(strsplit(gsub(",",".",x),"; "))))
alphaCI_hand <- lapply(alphaCI_hand,sort)

# hit statistic
compare(alphaJATSci,alphaCI_hand,sums=T)
# add to result table
res <- rbind(res,c("alpha error from CI",compare(alphaJATSci,alphaCI_hand,sums=T)))
colnames(res) <- c("feature","CP","CN","FP","FN")

## Table 4. Absolute frequencies of detected α-level from 1-α confidence intervals
tab <- cbind(table(unlist(alphaJATSci)),table(unlist(alphaCI_hand)))
colnames(tab) <- c("study.character()","manual coding")
tab
tab<-cbind(tab,"false positive"=falseDetections(alphaJATSci,alphaCI_hand,names=rownames(tab),type="FP"))
tab<-cbind(tab,"false negative"=falseDetections(alphaJATSci,alphaCI_hand,names=rownames(tab),type="FN"))
tab
xtable(addmargins(tab,m=1),dig=0)

# analyse false negatives
a<-compare(alphaJATSci,alphaCI_hand,sums=F)
i<-which(a$FN>0);i
data$file[i]
# 3: 95% CI in table column 
# 26: 95% CI in table column 
# 43: "confidence intervals set to .95"
# 75: 90% CI RMSEA in table column
# 120: 95% in Discussion
# 151: 95% CI in table column 
# 162: 95% CI in table column 
# 165: 95% CI in figure caption
# 166: 95% CI in table column 
# 167: 95% CI in table column 
# 187: "95% confidence area" 
# 192: 95% CI in figure caption
# 196: 95% CI in figure caption
# 202: 95% CI in figure caption and y-axis label 
# 207: 95% CI in table column 
# 210: 95% CI in figure caption
# 217: 95% CI in table column 
# 235: 95% CI in reported within table 
# 249: 95% CI in table column and table note
# 268: 95% CI in figure caption
# 286: 95% CI in figure caption


# distribution of 1-alpha CIs per article
table(unlist(lapply(alphaJATSci,length)))
table(unlist(lapply(alphaCI_hand,length)))

# total CI values to detect: 126
sum(compare(alphaJATSci,alphaCI_hand)$CP+compare(alphaJATSci,alphaCI_hand)$FN)
# in n artcicles with CI: 118
sum(compare(alphaJATSci,alphaCI_hand,sums=F)$CP>0|compare(alphaJATSci,alphaCI_hand,sums=F)$FN>0)
# detected ci values: 105
sum(compare(alphaJATSci,alphaCI_hand)$CP)
# in n articles: 97
sum(compare(alphaJATSci,alphaCI_hand,sums=F)$CP>0)

#########################
## alpha level from text
# first without p2alpha than with p2alpha conversion
for(pASalpha in c(FALSE,TRUE)){
  # alpha from study.character()
  if(pASalpha==FALSE){
    alphaJATS <- mapply(c,lapply(mapply(c,lapply(character,"[","alpha_error")),"[","alpha_error"))
    alphaJATScorr <- mapply(c,lapply(mapply(c,lapply(character,"[","alpha_error")),"[","corrected_alpha"))
    alphaJATSci <- mapply(c,lapply(mapply(c,lapply(character,"[","alpha_error")),"[","alpha_from_CI"))
    alphaJATSmax <- unlist(mapply(c,lapply(mapply(c,lapply(character,"[","alpha_error")),"[","alpha_max")))
  }
  # with p2alpha=TRUE
  if(pASalpha==TRUE){
    alphaJATS <- mapply(c,lapply(mapply(c,lapply(p2alpha,"[","alpha_error")),"[","alpha_error"))
    alphaJATScorr <- mapply(c,lapply(mapply(c,lapply(p2alpha,"[","alpha_error")),"[","corrected_alpha"))
    alphaJATSci <- mapply(c,lapply(mapply(c,lapply(p2alpha,"[","alpha_error")),"[","alpha_from_CI"))
    alphaJATSmax <- unlist(mapply(c,lapply(mapply(c,lapply(p2alpha,"[","alpha_error")),"[","alpha_max")))
  }
  
  # alpha from manual analysis
  alpha_hand <- lapply(strsplit(data$alpha_error,"; "),function(x) as.numeric(unlist(strsplit(gsub(",",".",x),"; "))))
  alpha_hand[unlist(lapply(alpha_hand,length))==0] <- NA
  corrected_alpha_hand <- lapply(strsplit(data$corrected_alpha,"; "),function(x) as.numeric(unlist(strsplit(gsub(",",".",x),"; "))))
  corrected_alpha_hand[unlist(lapply(corrected_alpha_hand,length))==0] <- NA
  alphasHand <- lapply(lapply(1:length(alphaJATS), function(x) c(unlist(alpha_hand[x]),unlist(corrected_alpha_hand[x]))),as.numeric)
  alphasHand <- lapply(alphasHand,function(x) x[!is.na(x)])
  alpha_hand_max <- unlist(lapply(lapply(alphasHand,max,na.rm=T),function(x) ifelse(x==-Inf,NA,x)))
  
  table(unlist(alphaJATS))
  table(unlist(alpha_hand))
  
  table(unlist(alphaJATSmax))
  table(unlist(alpha_hand_max))
  
  table(unlist(alphaJATScorr))
  table(unlist(corrected_alpha_hand))
  
  #############################################################################
  # analyse collapsed alpha and alpha corrected (not included to result table)
  alphas <- lapply(lapply(1:length(alphaJATS), function(x) c(unlist(alphaJATS[x]),unlist(alphaJATScorr[x]))),as.numeric)
  l <- unique(c(names(table(unlist(alphas)))),names(table(unlist(alphasHand))))
  tab <- cbind(table(factor(unlist(alphas),l)),table(factor(unlist(alphasHand),l)))
  colnames(tab) <- c("study.character()","manual coding")
  tab <- addmargins(tab,m=1)
  print(xtable::xtable(tab,dig=0))
}

# n detections corrected alpha
sum(unlist(lapply(corrected_alpha_hand, is.na))==0)
sum(unlist(lapply(alphaJATScorr, is.na))==0)

####################################
## compare highest corrected alphas
l <- unique(c(names(table(unlist(alphaJATScorr))),names(table(unlist(corrected_alpha_hand)))))
l <- sort(as.numeric(l))
tab <- cbind(table(factor(unlist(alphaJATScorr),l)),table(factor(unlist(corrected_alpha_hand),l)))
colnames(tab) <- c("study.character()","manual coding")
tab
d <- cbind(as.numeric(unname(unlist(lapply(alphaJATScorr,max,na.rm=T)))),as.numeric(unname(unlist(lapply(corrected_alpha_hand,max,na.rm=T)))))
d[d==-Inf] <- NA

# n total exact hits: 1
sum((d[,1]-d[,2])==0,na.rm=T)
# out of n total correct hits with corrected alpha
sum(!is.na(d[,2]))

##############################################################
for(pASalpha in c(FALSE,TRUE)){
  if(pASalpha==FALSE) alphaJATSmax <- unlist(mapply(c,lapply(mapply(c,lapply(character,"[","alpha_error")),"[","alpha_max")))
  if(pASalpha==TRUE) alphaJATSmax <- unlist(mapply(c,lapply(mapply(c,lapply(p2alpha,"[","alpha_error")),"[","alpha_max")))
  # max/min of alpha and alphafrom CI and corrected alpha
  alphaallmax <- alphaJATSmax
  alphaallmax[alphaallmax==-Inf] <- NA
  alphaallmin <- unlist(lapply(1:287,function(x) min(as.numeric(c(unlist(alphaJATS[x]),unlist(alphaJATScorr[x]),unlist(alphaJATSci[x]))),na.rm=T)))
  alphaallmin[alphaallmin==Inf] <- NA
  # min/max from manual analysis
  alphaallman <- mapply(c,lapply(1:287,function(x) unique(c(data$alpha_error[x],data$corrected_alpha[x],data$alpha_from_CI[x]))))
  alphaallmaxman <- unlist(lapply(alphaallman, function(x) max(as.numeric(gsub(",",".",unlist(strsplit(x,"; ")))),na.rm=T)))
  alphaallmaxman[alphaallmaxman==-Inf] <- NA
  alphaallminman <- unlist(lapply(alphaallman, function(x) max(as.numeric(gsub(",",".",unlist(strsplit(x,"; ")))),na.rm=T)))
  alphaallminman[alphaallminman==-Inf] <- NA
  
  ## add hit statistic to result table
  if(pASalpha==FALSE) res <- res <- rbind(res,c("max of $alpha$-level and CI (p2alpha=FALSE)",compare(alphaallmax,alphaallmaxman,sums=T)))
  if(pASalpha==TRUE) res <- res <- rbind(res,c("max of $alpha$-level and CI (p2alpha=TRUE)*",compare(alphaallmax,alphaallmaxman,sums=T)))

## Table 5. Distribution of extracted maximum α-level with option ‘p2alpha’ deactivated and in default mode (numbers in brackets)
  lev <- sort(as.numeric(names(table(c(alphaallmaxman,alphaallmax)))))
  tab_hand <- table(factor(alphaallmaxman,lev))
  tab_JATS <- table(factor(alphaallmax,lev))
  
  m <- cbind(tab_JATS,tab_hand)
  colnames(m) <- c("study.character()","manual analysis")
  m<-cbind(m,"false positive"=falseDetections(alphaallmax,alphaallmaxman,names=rownames(m),type="FP"))
  m<-cbind(m,"false negative"=falseDetections(alphaallmax,alphaallmaxman,names=rownames(m),type="FN"))
  
  print(paste("p2alpha =",pASalpha))
  m <- addmargins(m,m=1)
  print(m)
  if(pASalpha==FALSE) m1 <- m
  
}

# merge frequencies of m1 and m
m1[,1] <- paste0(m1[,1]," (",m[,1],")")
m1[,3] <- paste0(m1[,3]," (",m[,3],")")
m1[,4] <- paste0(m1[,4]," (",m[,4],")")

# result table
m1
xtable(m1)


#########
## Power
#######
# from study.character()
power <- mapply(c,mapply(c,lapply(character,"[","power")))
power_obs <- mapply(c,lapply(mapply(c,lapply(character,"[","power")),"[","observed_power"))
# from manual analysis
power_hand <- strsplit(gsub(",",".",data$power),"; |;")
power_hand[power_hand==""] <- NA
power_obs_hand <- strsplit(gsub(",",".",data$observed_power),"; |;")

# compare detected power and observed power values per article
powerall <- lapply(1:287,function(x) unique(sort(c(unlist(power[x]),unlist(power_obs[x])))))
powerallhand <- lapply(1:287,function(x) as.numeric(unique(sort(c(unlist(power_hand[x]),unlist(power_obs_hand[x]))))))

# hit statistic
a <- compare(powerall,powerallhand)
colSums(a)
# add to result table
res <- rbind(res,c("power",compare(powerall,powerallhand,sums=T)))

# in n articles
sum(a$CP>0) # 42
# n power values are detected
sum(a$CP) # 61
# out of n articles with power
sum((a$CP+a$FN)>0) # 45
# and n total power values
sum(a$CP+a$FN) # 73

# all present power values exactly extracted in n articles
sum(a$CP[(a$CP+a$FN)>0]==(a$CP+a$FN)[(a$CP+a$FN)>0]) # 37
# with n false positives
sum(a$FP) # 2

# extractions with false negatives
i <- which(compare(powerall,powerallhand)$FN>0);i
powerall[compare(powerall,powerallhand)$FN>0]
# from manual coding
powerallhand[compare(powerall,powerallhand)$FN>0]

# n articles with non detections
length(i)
# have n power values
length(unlist(powerallhand[i]))
# n undetected power values
length(unlist(powerallhand[i]))-length(unlist(powerall[i]))
# n detected
length(unlist(powerall[i]))
# in n articles
sum(unlist(lapply(powerall[i],length))>0)

# false negatives
# 87 the power to detect an effect size in the present study (partial ␩ 2 ⫽ .14) at ␣ ⫽ .05 level with a sample size of 32 was .93.
# 87 the power to detect an effect size in the present study (partial ␩ 2 ⫽ .14) at ␣ ⫽ .05 level with a sample size of 56 was .99. 
# 115 The statistical power was very high (0.99),
# 115 in figure caption: , R = .39; effect size (f ), .64; power, 1 (without competitive victimhood, R = .38; effect size (f ), 0.62; power, 1).
# 175 power = 80
# 187 acceptable test power to detect large effect sizes (Cohen’s f ⫽ 0.40, with medium correlations among measurements, r ⫽ .30) for both main effects (age group: 1-␤ ⫽AGE, POSTURAL SWAY, AND VIMS 0.83, driving condition: 1-␤ ⫽ 0.97) and their interaction (1-␤ ⫽ 0.97).
# 192 but because of the power in the experiment (the achieved power for the interaction was only 0.46).
# 197 showed that the final sample size ensured sufficient power (i.e., 0.99)
# 213 between-subjects design with a power ranging between .80 and .90, 50 to 70 participants per condition are required
# -> only 80%
# 284 With the sample size of 56, the estimated power increased to 0.925, and with the 3 9 2 9 2 design that we actually used in Experiment 1, it went up to 0.979.
# -> erkannt: which resulted in the power of 0.340.

# false positives
i <- which(compare(powerall,powerallhand)$FP>0);i
powerall[compare(powerall,powerallhand)$FP>0]
# from manual coding
powerallhand[compare(powerall,powerallhand)$FP>0]

# 111: Cohen (1988) recommends conducting studies that have an 80% probability of detecting the effect when the effect is present (i.e., power = 0.80).
# 228: bad compiling of table: 1-beta .37


## Table 6. Absolute frequencies of extracted categorized test power
l <- unique(c(names(table(unlist(powerall)))),names(table(unlist(powerallhand))))
tab <- cbind(table(factor(unlist(powerall),l)),table(factor(unlist(powerallhand),l)))
colnames(tab) <- c("study.character()","manual coding")
addmargins(tab,m=1)
print(tab)

l <- cut(c(unlist(powerall),unlist(powerallhand)),c(0,.2,.5,.79,.8,.9,1,100))
tab <- cbind(table(cut(unlist(powerall),c(0,.2,.5,.79,.8,.9,1,100))),table(cut(unlist(powerallhand),c(0,.2,.5,.79,.8,.9,1,100))))
colnames(tab) <- c("study.character()","manual coding")
rownames(tab)[nrow(tab)] <- ">1"
# convert empirical values to interval
powerall2<-lapply(powerall, cut,c(0,.2,.5,.79,.8,.9,1,100))
powerallhand2<-lapply(powerallhand, cut,c(0,.2,.5,.79,.8,.9,1,100))

tab<-cbind(tab,"false positive"=falseDetections(powerall2,powerallhand2,names=gsub("\\(","",gsub("\\[","",rownames(tab))),type="FP"))
tab<-cbind(tab,"false negative"=falseDetections(powerall2,powerallhand2,names=rownames(tab),type="FN"))
tab
tab<-addmargins(tab,m=1)
tab
xtable(tab,digit=0)

############################################################
## multi.comparison.correction
############################################################
# from study.character()
correction <- mapply(c,lapply(character,"[","multi_comparison_correction"))

# from manual analysis
correction_hand <- strsplit(data$multi.comparison.correction,", ")
correction_hand <- lapply(correction_hand,function(x) gsub("Scheffe","Scheffé",x))
correction_hand <- lapply(correction_hand,function(x) gsub("Fisher$","Fisher LSD",x))

# hit statistic
compare(correction,correction_hand,sums=T)

# add to result table
res <- rbind(res,c("correction for multiple testing",compare(correction,correction_hand,sums=T)))

# false negatives
correction_hand[compare(correction,correction_hand)$FN>0]
correction[compare(correction,correction_hand)$FN>0]

## Table 7. Absolute frequencies of authors of multiple test correction procedures
hand <- unlist(correction_hand)
JATS <- unlist(correction)
lev <- names(sort(table(c(JATS,hand)),dec=T))
hand <- factor(hand,lev)
JATS <- factor(JATS,lev)

tab <- cbind(table(JATS),table(hand))
colnames(tab) <- c("study.character()","manual coding")
tab
xtable(addmargins(tab,m=1),dig=0)

##########################################
## outlier removal in standard deviations
########################################
# from study.character()
outlier <- lapply(mapply(c,lapply(character,"[","OutlierRemovalInSD")),unlist)
outlier[which(unlist(lapply(outlier,length))==0)] <- NA
# from manual analysis
outlier_hand <- as.list(as.numeric(gsub(",",".",data$OutlierRemovalInSD)))

# hit statistic
compare(outlier,outlier_hand,sums=T)
# add to result table
res <- rbind(res,c("outlier removal",compare(outlier,outlier_hand,sums=T)))

## Table 9. Absolute frequencies of extracted outlier definition expressed in standard deviations (SD)
l <- unique(c(unlist(outlier),unlist(outlier_hand)))
l <- l[length(l):1]
tab_hand <- table(factor(unlist(outlier_hand),l))
tab_JATS <- table(factor(unlist(outlier),l))
tab <- cbind(tab_JATS,tab_hand)
colnames(tab) <- c("study.character()","manual coding")
tab
tab<-cbind(tab,"false positive"=falseDetections(outlier,outlier_hand,names=rownames(tab),type="FP"))
tab<-cbind(tab,"false negative"=falseDetections(outlier,outlier_hand,names=rownames(tab),type="FN"))
tab

xtable(addmargins(tab,m=1),dig=0)

# false negative
which(compare(outlier,outlier_hand)$FN>0)
# 100: uncompiled +- in listing brackets: (+- 2 SD) 
# x<-"Twenty-nine additional infants were excluded from final analyses for the following reasons: failure to meet the habituation criteria, described below, extreme looking times (2 SD) during the test trials,..."
# get.outlier.def(x)
# x<-"Twenty-nine additional infants were excluded from final analyses for the following reasons: failure to meet the habituation criteria, described below, extreme looking times (> 2 SD) during the test trials,..."
# get.outlier.def(x)

##################
## test direction
################
# from study.character()
direction <- lapply(mapply(c,lapply(character,"[","test_direction")),unlist)
direction[unlist(lapply(direction,length))==0] <- NA
direction <- unlist(direction)
# from manual analysis
direction_hand <- data$test.direction
direction_hand[direction_hand==""] <- NA

# hit statistic
compare(direction,direction_hand,sums=T)

# add to result table
res <- rbind(res,c("test direction",compare(direction,direction_hand,sums=T)))
res

## Table 10. Absolute detections of test direction/s
tab <- cbind(table(direction),table(direction_hand))
colnames(tab) <- c("study.character()","manual coding")
tab
tab<-cbind(tab,"false positive"=falseDetections(direction,direction_hand,names=rownames(tab),type="FP"))
tab<-cbind(tab,"false negative"=falseDetections(direction,direction_hand,names=rownames(tab),type="FN"))
tab

xtable(addmargins(tab,m=1),dig=0)

# false positive: "one sided aggression"
which(compare(direction,direction_hand)$FP>0)
# false negative: "two tails"
which(compare(direction,direction_hand)$FN>0)


########################
## interaction analysis
######################
# from study.character()
interaction <- mapply(c,lapply(character,"[","InteractionModeratorMediatorEffect"))
interaction[unlist(grepl("^$",interaction))] <- NA
# from manual analysis
interaction_hand <- tolower(data$InteractionModeratorMediatorEffect)
interaction_hand[interaction_hand==""] <- NA
interaction_hand <- strsplit(interaction_hand,", ")

# hit statistic
a <- compare(interaction,interaction_hand)
# add to result table
res <- rbind(res,c("interaction/mediator/moderator",compare(interaction,interaction_hand,sums=T)))

## Table 8. Absolute frequencies of specific interaction effects
l <- sort(unique(c(unlist(interaction),unlist(interaction_hand))))
tab <- cbind(table(factor(unlist(interaction),l)),
           table(factor(unlist(interaction_hand),l)))

colnames(tab) <- c("study.character","manual coding")
tab
tab<-cbind(tab,"false positive"=falseDetections(interaction,interaction_hand,names=rownames(tab),type="FP"))
tab<-cbind(tab,"false negative"=falseDetections(interaction,interaction_hand,names=rownames(tab),type="FN"))
tab

xtable(addmargins(tab,m=1),dig=0)

## Interaction as binary output
interaction_bin <- unname(unlist(lapply(interaction,length))>0)
interaction_hand_bin <- unname(!unlist(lapply(interaction_hand,function(x) is.na(x[1]))))
interaction_bin[interaction_bin==FALSE] <- NA
interaction_hand_bin[interaction_hand_bin==FALSE] <- NA

# hit statistic
a <- compare(interaction_bin,interaction_hand_bin)
# add to result table
res <- rbind(res,c("interaction (binary)",compare(interaction_bin,interaction_hand_bin,sums=T)))


###########################
## statistical assumptions
#########################
# from study.character()
assumptions <- lapply(mapply(c,lapply(character,"[","assumptions")),unlist)
# from manual analysis
assumptions_hand <- strsplit(data$assumption,", ")

# hit statistic
compare(assumptions,assumptions_hand,sums=T)
# add to result table
res <- rbind(res,c("assumptions",compare(assumptions,assumptions_hand,sums=T)))

# n assumptions detected
table(unlist(lapply(assumptions,length)))
table(unlist(lapply(assumptions_hand,length)))

## Table 11. Absolute frequencies of detected assumptions
t <- sort(table(unlist(assumptions)));t
t_hand <- sort(table(unlist(assumptions_hand)));t_hand

l <- unique(c(names(t),names(t_hand)))
tab <- cbind((table(factor(unlist(assumptions),l))),
           (table(factor(unlist(assumptions_hand),l))))
colnames(tab) <- c("study.character()","manual coding")
tab <- tab[order(tab[,1],decreasing=T),]
tab
tab<-cbind(tab,"false positive"=falseDetections(assumptions,assumptions_hand,names=rownames(tab),type="FP"))
tab<-cbind(tab,"false negative"=falseDetections(assumptions,assumptions_hand,names=rownames(tab),type="FN"))

tab
xtable(addmargins(tab,1),dig=0)

## assumptions as binary
assumptions_bin <- unname(unlist(lapply(assumptions,length))>0)
assumptions_hand_bin <- unname(!unlist(lapply(assumptions_hand,function(x) is.na(x[1]))))
assumptions_bin[assumptions_bin==FALSE] <- NA
assumptions_hand_bin[assumptions_hand_bin==FALSE] <- NA

# hit statistic
compare(assumptions_bin,assumptions_hand_bin,sums=T)
# add to result table
res <- rbind(res,c("assumptions (binary)",compare(assumptions_bin,assumptions_hand_bin,sums=T)))

#######################
## analytical software
#####################
# from study.character()
software <- mapply(c,lapply(character,"[","software"))
# from manual analysis by us
software_hand <- strsplit(gsub("-"," ",data$softwareTRUTH),", ")
# from manual analysis by Blanca
software_blanca <- strsplit(gsub("-"," ",data$softwareBlanca),", ")

## hit statistic for recoded software
a <- compare(software,software_hand)
colSums(a)
# add to result table
res <- rbind(res,c("software",compare(software,software_hand,sums=T)))

# which are false negative
table(unlist(software_hand[which(a$FN>0)]))[!is.element(names(table(unlist(software_hand[which(a$FN>0)]))),names(table(unlist(software[which(a$FN>0)]))))]

## Table 12. Absolute frequencies of detected software solutions by study.character, the manually coded data, and 
##           the explicitly stated software solution used for the main analyis reported in Blanca et al.’s TABLE 8.
hand <- unlist(software_hand)
JATS <- unlist(software)
blanca <- unlist(software_blanca)
lev <- rev(unique(rev(c(names(sort(table(hand))),names(sort(table(JATS))),names(sort(table(blanca)))))))
hand <- factor(hand,lev)
JATS <- factor(JATS,lev)
blanca <- factor(blanca,lev)

tab <- cbind(table(JATS),table(hand),table(blanca))
tab<-tab[nrow(tab):1,]

colnames(tab) <- c("study.character()","manual coding","Blanca et al.")
tab

# sort by frequency
tab<-tab[order(tab[,1],decreasing=T),]
tab
xtable(tab[1:16,])
xtable(tab[17:31,])

## hit statistic for Blanca's software extraction
compare(software_blanca,software,sums=T)


#############
## n studies
###########
# from study.character()
nstudies <- unlist(mapply(c,lapply(character,"[","Nstudies")))
# from manual analysis
nstudies_hand <- data$nstudies
nstudies_Blanca <- data$nstudiesBlanca

# hit statistics
compare(nstudies,nstudies_hand,sums=T)

# add to results
res <- rbind(res,c("n studies",compare(nstudies,nstudies_hand,sums=T)))

# analyse errors
cbind(1:287,nstudies,nstudies_hand,data$file)[compare(nstudies,nstudies_hand)$FP>0,]

## Table 13. Absolute frequencies of extracted number of studies per paper by study.character, 
##           the manually coded data and Blanca et al.’s TABLE 2
l <- unique(nstudies)
tab <- rbind(table(factor(nstudies,l)),table(factor(nstudies_hand,l)),table(factor(nstudies_Blanca,l)))
rownames(tab) <- c("study.character()","manual coding","Blanca et al.")
tab
xtable(tab)


###################################################
## calculate sum, accuracy, sensitivity, specifity
#################################################
for(i in 2:5) res[,i] <- as.numeric(res[,i])
digits <- 2

res$sum <- res$CP+res$CN+res$FP+res$FN
res$sensitivity <- round(res$CP/(res$CP+res$FN),digits)
res$specifity <- round(res$CN/(res$CN+res$FP),digits)
res$accuracy <- round((res$CP+res$CN)/(res$CP+res$CN+res$FP+res$FN),digits)
res
a<-print(xtable(res),include.row=F)
cat(gsub("\\.00 "," ",a))


