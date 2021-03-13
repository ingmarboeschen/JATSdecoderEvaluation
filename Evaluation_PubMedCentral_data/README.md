## Extracted NISO-JATS tags with JATSdecoder as individual files
Each file contains the content of a NISO-JATS tag extracted from the full PubMed Central collection of open access files downloaded from: ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/ on 1.1.2021. <br>
Note: The file names are stored in "files.rda". The exported affiliations, author lists, publishing history, keywordss and text elements are not supplied due to high memory consumption but can be requested via email.   
``` r
# Download each file and load its content with R for analysis. 
load("type.rda")
head(type)
sort(table(unlist(type))) # type distribution
``` 
