*statcheck* is flawed by design and no valid spell checker.
==================================
Böschen, Ingmar (2024).

<ins>*Abstract*</ins>
The R package *statcheck* is designed to extract statistical test results from text and check the consistency of the reported test statistics and corresponding p-values. 
Recently, it has also been featured as a spell checker for statistical results, aimed at improving reporting accuracy in scientific publications. 

In this study, I perform a check on *statcheck* using a non-exhaustive list of 187 simple text strings with arbitrary statistical test results. These strings represent a wide range of textual representations of results including correctly manageable results, non-targeted test statistics, variable reporting styles, and common typos.

Since *statcheck*'s detection heuristic is tied to a specific set of statistical test results that strictly adhere to the American Psychological Association (APA) reporting guidelines, it is unable to detect and check any reported result that even slightly deviates from this narrow style. 
In practice, *statcheck* is unlikely to detect many statistical test results reported in the literature. 

I conclude that the capabilities and usefulness of the \textit{statcheck} software are very limited and that it should not be used to detect irregularities in results nor as a spell checker for statistical results. 

Future developments should aim to incorporate more flexible algorithms capable of handling a broader variety of reporting styles, such as those provided by *JATSdecoder* and Large Language Models, which show promise in overcoming these limitations but they cannot replace the critical eye of a knowledgeable reader. 

R Script to perform the statcheck check: [Script_statcheckIsFlawedByDesign.R](https://github.com/ingmarboeschen/JATSdecoderEvaluation/blob/main/Check_statcheck/Script_statcheckIsFlawedByDesign.R)

File with example results to check statcheck on [statcheck.io](https://michelenuijten.shinyapps.io/statcheck-web/): [examples4checking_statcheck.docx](https://github.com/ingmarboeschen/JATSdecoderEvaluation/blob/main/Check_statcheck/examples4checking_statcheck.docx)
