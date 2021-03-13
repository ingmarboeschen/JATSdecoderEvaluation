## Extracted NISO-JATS tags with JATSdecoder as individual files
Each file contains the content of a NISO-JATS tag extracted from the full PubMed Central collection of open access files downloaded on 1.1.2021. <br>
Note: The file names are stored in "files.rda".
``` r
# Download each file and load its content with R for analysis. 
load("type.rda")
head(type)
sort(table(unlist(type))) # type distribution
``` 
