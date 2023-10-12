##################################################################################
## Script to reproduce the analysis reported in:
## statcheck is flawed by design and no valid spell checker (Böschen, I, 2023)
## The test was run on statcheck 1.4 (Nuijten, M. B. & Epskamp, S., 2023)
############################################################################

## install statcheck and/or load the statcheck package
# install.packages("statcheck")
library(statcheck)
# Note: the Z-statistic requires a space in front to be detected!
statcheck("Z=2.3, p<.04")
statcheck(" Z=2.3, p<.04")
# wheras no space is needed as initial character in other result types 
statcheck("t(12)=2.3, p<.04")
statcheck("F(12, 34)=2.3, p<.04")

#################################################################
## Some examples of results that statcheck can check
##################################
x<-c(
  " t(12)=2.3, p<.05",
  " F(1,23)=4.5, p=.23",
  " r(12)=.34, p=.56",
  " Z=1.2, p<.34",
  " \u03A7^2(12)=3.4, p<.05",
  " \u03A72(12)=3.4, p<.05",
  " Chi^2(12)=3.4, p<.05",
  " chi2(12)=3.4, p<.05",
  " Q(12)=3.4, p<.01")
x

## Table 1: Example text representations of statistical test results that are checkable with statcheck in rearranged output table
a<-data.frame(rawInput=paste0('"',x,'"'),statcheck(x)[,c(10,2:9,11:12)])
a
## render output with xtable
#print(xtable::xtable(gsub("  *"," ",gsub(".00([^0-9])","\\1",format(as.matrix(a),)))),include.rownames=T)
# manually replace: "$chi$" with "$\chi$"

#####################################################################
### false positive chi-square detections in non-standard statistics

## Table 2: Example text representations of statistical test results with
## all upper- and lowercase letters in front of brackets
x<-paste0(" ",c(LETTERS,letters),"(12)=.3, p<.05")
x
a<-statcheck(x)

## render output with xtable
#xtable::xtable(gsub("  *"," ",gsub(".00([^0-9])","\\1",format(as.matrix(a[,c(10,1:9,11:14)])))))

## Table 3: Example text representations of statistical test results with
## all upper- and lowercase letters followed and the number 2 in front of brackets
x<-paste0(" ",c(LETTERS,letters),"2(12)=.3, p<.05")
x
a<-statcheck(x)
a

## render output with xtable
#xtable::xtable(gsub("  *"," ",gsub(".00([^0-9])","\\1",format(as.matrix(a[,c(10,1:9,11:14)]),))))

## Table 4: Example text representations of statistical test results with
## all upper- and lowercase letters followed the exponent sign and the number 2 in front of brackets
x<-paste0(" ",c(LETTERS,letters),"^2(12)=.3, p<.05")
x
a<-statcheck(x)
a

## render output with xtable
xtable::xtable(gsub("  *"," ",gsub(".00([^0-9])","\\1",format(as.matrix(a[,c(10,1:9,11:14)])))))

###################################################
## Table 5: General result patterns that statcheck does not recognize nor spell check and a correspoding simple example
pattern<-c(
"does not allow a calculation of the p-value",
"contains a report of any other value (e.g. Cohen's d) between the test statistic and the p-value",
"is supplied with a semicolon as seperator",
"is supplied without a seperator",
"contains the report of the degrees of freedom outside brackets",
"contains an exponent/fraction",
"has badly used seperators/decimals/numbers",
"has indexed results",
"has p-, r- or $R^2$-values outside their valid range",
"has estimatable p-values by beta and standard error",
"has a value reported with percent sign",
"a report of multiple results",
"is reported with a less equal or greater equal sign or alias",
"has degrees of freedom in squared brackets",
"has high degrees of freedom with coma as punctuation",
"has a double space behind the coma",
"has badly or non compiled operators due to prior PDF to text conversion ")

x<-c(
  " p=.12 or r=.12, p=.34",
  " t(12)=1.2, d=3.4, p=.56",
  " t(12)=1.2; p=.34",
  " t(12)=1.2 p=.34",
  " t=1.2, df=34, p=.56",
  " t(12)=1.2^3, p=4/5",
  " t(12)=1..2, p=.n3",
  " t index(12)=1.2, p=.34",
  " r(12)=1.2, p=3.45, R^2=6.7",
  " beta=1.2, SE=.34, p<.05",
  " t(12)=1.2, p=5%",
  " all t's(12)>1.2, p's>.05",
  " t(12)≤1.2, p<=.05",
  " t[12]=1.2, p<.05",
  " t(1,234)=5.6, p<.05",
  " t(12)=1.2,  p<.05",
  " t 1.2, p 5 .34"
  )

cbind(substr(pattern,1,61),x)

# Not a single result is detected by statcheck:
statcheck::statcheck(x)
x
