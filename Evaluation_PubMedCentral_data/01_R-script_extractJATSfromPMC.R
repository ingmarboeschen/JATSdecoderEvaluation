###################################################################################
## JATSdecoder: Insights to PubMed Central’s Open Access Database (submitted 2021).
## Ingmar Böschen, Universität Hamburg, ingmar.boeschen@uni-hamburg.de
#################################################################################

#####################################################################################
## This script extracts and stores the NISO-JATS tags from all PubMed Central files.
## The full repository of PMC linked open access articles is downloadable via 
## PMCs bulk download server: ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk 
###########################################################################

# folder with NXML files
folder <- "./PMC/" # adjust to your system
# XML file names
files <- list.files(folder, patt = "xml$|XML$", rec = T, full.names = T)
# n files
length(files)

## total file size
file.size <- file.size(files)
# in GB
totalsize <- round(sum(file.size)/1000000000, 2)

############################################
## Extraction of JATS tags with JATSdecoder
##########################################
# load libraries
library(JATSdecoder)
library(future.apply)
# define number of cores for multithreading
plan(multisession, workers = 60, gc = TRUE) # adjust to your system

## for VERY powerfull computers with a lot of RAM (approx. >= 256 Gb)
JATS<-future_lapply(files,JATSdecoder)
# save full results
save(JATS,file="JATS.rda")

## for still powerfull computers with a lot of RAM (approx. >= 128 Gb)
## with article batch size = BatchSize
# define size of each batch (choose small batches if only small RAM memory space available)
BatchSize <- 100000
# start/stop indices for batches
i <- seq(1, length(files), BatchSize)
o <- c(seq(BatchSize, length(files), BatchSize), length(files))

# extract batch wise
for(j in 1:length(i)){
# with first 50 characters in text only
JATS <- future_lapply(files[i[j]:o[j]], function(x){J <- tryCatch(JATSdecoder(x), error = function(e) JATSdecoder(x, letter = F));J$text <- substr(J$text, 1, 50);return(J)})
# with full text (remove '#' to activate but deactivate former line)
#JATS <- future_lapply(files[i[j]:o[j]], JATSdecoder)
save(JATS, file = paste0("JATS_BATCH_", 100+j, ".rda"))
rm(JATS)
print(j);print(Sys.time())
}

## merge batches of JATSdecoder results to one file 
# files with batches of results
d <- grep("JATS_BATCH", list.files(pattern = "rda$"), v = T)
# empty object
fullJATS <- NULL
# load and merge batches
for(i in 1:length(d)){ 
# load
  load(file = d[i])
# merge
  fullJATS <- c(fullJATS, JATS)
  print(length(fullJATS))
}
JATS <- fullJATS
# n files in merged list
length(JATS)
# save 
save(JATS, file = "JATS.rda")

## get missing files (if there are any) and add JATSdecoder result to list
# has missing file?
length(JATS)!=length(files)
# add JATSdecoder result
if(length(JATS)!=length(files)){
file <- mapply(c, lapply(JATS, "[", "file"))
missings <- files[which(!is.element(files, file))]
new <- 
lapply(missings, function(x){J <- tryCatch(JATSdecoder(x), error = function(e) JATSdecoder(x, letter = F));J$text <- substr(J$text, 1, 50);return(J)})
# or with full text (remove '#' to activate)
# new <- lapply(missings, JATSdecoder)
JATS <- c(JATS, new)
}

# new n files
length(JATS)
# save result for further analysis
save(JATS, file = "JATS.rda")

############################################################
## Extract and save each extracted element from object JATS
load("JATS.rda")
# create folder to store new files (remove '#' to activate)
#dir.create("tags")
# change working directory
setwd("./tags")
# extract content and save as .rda files for further analysis
abstract <- lapply(JATS, "[", "abstract")
save(abstract, file = "abstract.rda")
affiliation <- mapply(c, lapply(JATS, "[", "affiliation"))
save(affiliation, file = "affiliation.rda")
author <- mapply(c, lapply(JATS, "[", "author"))
save(author, file = "author.rda")
country <- mapply(c, lapply(JATS, "[", "country"))
save(country, file = "country.rda")
editor <- mapply(c, lapply(JATS, "[", "editor"))
save(editor, file = "editor.rda")
file <- mapply(c, lapply(JATS, "[", "file"))
save(file, file = "file.rda")
keywords <- mapply(c, lapply(JATS, "[", "keywords"))
save(keywords, file = "keywords.rda")
history <- mapply(c, lapply(JATS, "[", "history"))
save(history, file = "history.rda")
pubyear <- lapply(history, "[", "pubyear")
save(pubyear, file = "pubyear.rda")
journal <- lapply(JATS, "[", "journal")
save(journal, file = "journal.rda")
references <- mapply(c, lapply(JATS, "[", "references"))
save(references, file = "references.rda")
sections <- mapply(c, lapply(JATS, "[", "sections"))
save(sections, file = "sections.rda")
subject <- mapply(c, lapply(JATS, "[", "subject"))
save(subject, file = "subject.rda")
text <- mapply(c, lapply(JATS, "[", "text"))
save(text, file = "text.rda")
title <- mapply(c, lapply(JATS, "[", "title"))
save(title, file = "title.rda")
type <- mapply(c, lapply(JATS, "[", "type"))
save(type, file = "type.rda")
doi <- mapply(c, lapply(JATS, "[", "doi"))
save(doi, file = "doi.rda")
volume <- mapply(c, lapply(JATS, "[", "volume"))
save(volume, file = "volume.rda")

## alternatively extract tag wise from files (takes longer)
# abstract <- future_lapply(files,JATSdecoder,output="abstract")
# save(abstract, file = "abstract.rda")
# affiliation <- future_lapply(files,JATSdecoder,output="affiliation")
# save(affiliation, file = "affiliation.rda")
# ...
