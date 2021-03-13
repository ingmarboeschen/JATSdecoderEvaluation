## JATSdecoder: Insights to PubMed Central’s Open Access Database (2021).
This folder contains follwing materials:
- Skript 1: extracts and saves all tags analysed in the paper
- Skript 2: reproduces all analyses reported in the paper
- Data files: R data files (.rda) with extracted NISO-JATS tags

**_Note:_** Each file contains the content of a NISO-JATS tag extracted from the full PubMed Central collection of open access files downloaded from:  `ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/` on 1st January 2021. The file names are stored in "files.rda". The exported affiliations, author lists, publishing history, keywords and text elements are not supplied here due to their high memory consumption but can be requested via email from the author.   

### Import and use the data
``` r
# Download each .rda file and load its content with R for analysis. 
load("type.rda")
head(type)
sort(table(unlist(type))) # type distribution
``` 

### Extract not supplied data

``` r
# set working directory with XML files
setwd("./home/PMC") # adjust to your system
# get XML file names in directory
files<-list.files(pattern="XML$|xml$",recursive=TRUE)
# load JATSdecoder future package
library(JATSdecoder)
# load future.apply package for multithreading
library(future.apply)
# define number of cores for multithreading
plan(multisession, workers = 60, gc = TRUE)
# extract desired content
author<-future_lapply(files,get.author)
keywords<-future_lapply(files,get.keywords)
text<-future_lapply(files,get.text)
# save for before further analysis
save(author,file="author.rda")
save(keywords,file="keywords.rda")
text(author,file="text.rda")
``` 
