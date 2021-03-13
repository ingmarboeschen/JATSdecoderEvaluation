## JATSdecoder: Insights to PubMed Central’s Open Access Database (2021).
This folder contains follwing materials:
- Skript 1: extracts and saves all tags analysed in the paper
- Skript 2: reproduces all analyses reported in the paper
- Data files: R data files (.rda) with extracted NISO-JATS tags<br>
*Note:* Each file contains the content of a NISO-JATS tag extracted from the full PubMed Central collection of open access files downloaded from: <a href="ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/">ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/</a> on 1st January 2021. The file names are stored in "files.rda". The exported affiliations, author lists, publishing history, keywords and text elements are not supplied here due to their high memory consumption but can be requested via email from the author.   

### Import and use the data
``` r
# Download each file and load its content with R for analysis. 
load("type.rda")
head(type)
sort(table(unlist(type))) # type distribution
``` 

