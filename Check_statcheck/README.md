The *statcheck* package is flawed by design and is not a valid spell checker for statistical results in scientific reports.
==================================
Böschen, Ingmar (submitted 2013).

<ins>*Abstract*</ins>

The R package *statcheck* aims to extract statistical test results from text and check the reported p-values for consistency. Recently, it has also been featured as a spell checker for statistical results. 

Here, I perform a check on *statcheck* with a non-exhaustive list of simple text strings with arbitrary results. I show that *statcheck*'s extraction heuristic is too simple and narrow. 
Because *statcheck*'s detection heuristic is tied to a specific set of statistical test results that strictly adhere to the American Psychological Association reporting guidelines, it is unable to detect and check any reported results that even slightly deviate from this narrow style. 
It is unlikely to detect many or most statistical results reported in the literature, and produces false extractions of chi-square test results in many potential reports of non-standard statistics. 

The example test set consists of correctly handable results, potential reports of non-targeted test statistics, plausible individual reporting styles, irregularities and errors that a statistical results spell checker should detect. 
I conclude, that the capabilities and usefulness of the software are very limited and that it should not be used to detect irregularities in results or as a spell checker for results.
