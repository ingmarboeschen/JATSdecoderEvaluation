## JATSdecoder: Insights to PubMed Central’s Open Access Database (submitted 2021).
This folder contains follwing materials:
- R script 1: extracts and saves all NISO-JATS tags analysed in the paper
- R script 2: reproduces all analyses reported in the paper
- The *tags*-folder contains the R data files (.rda) that do not exceed file size limitation.

**_Note:_** Each file contains the content of a specific NISO-JATS tag extracted from the full PubMed Central collection of open access files downloaded from:  `ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/` on 1st January 2021. The file names are stored in "files.rda". The exported affiliations, author lists, publishing history, keywords and text elements are not supplied here due to their high memory consumption but can be requested via email from the author.   

### Import and use the data
``` r
# Download each .rda file and load its content with R for analysis. 
load("./tags/type.rda") # adjust to your system
head(type)
sort(table(unlist(type))) # type distribution
``` 

### Extract not supplied data

``` r
# set working directory with XML files
setwd("./PMC") # adjust to your system
# get XML file names in directory
files<-list.files(pattern="XML$|xml$",recursive=TRUE)
# load JATSdecoder future package
library(JATSdecoder)
# load future.apply package for multithreading
library(future.apply)
# define number of cores for multithreading
plan(multisession, workers = 60, gc = TRUE)
# extract desired content (takes several hours)
author<-future_lapply(files,JATSdecoder,output="author")
keywords<-future_lapply(files,output="keywords")
text<-future_lapply(files,output="text")
# save for further analysis
save(author,file="author.rda")
save(keywords,file="keywords.rda")
text(text,file="text.rda")
``` 
