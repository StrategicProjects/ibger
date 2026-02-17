## R CMD check results

0 errors | 0 warnings | 0 note

* This is a ressubmission.

## Dear Konstanze,

Thank you for the review. All three issues have been addressed:

1. Software names in single quotes: 'IBGE' is now quoted in both Title and Description.

2. Title length: shortened to 47 characters ("Access the 'IBGE' Aggregate Data 'API' from R").

3. Missing \value tag: added to ibge_clear_cache.Rd ("No return value, called for side effects.").

The corrected version has been resubmitted.

Best regards,
Andre Leite


## Uwe Ligges
We see: Found the following (possibly) invalid URLs:
     URL: https://sidra.ibge.gov.br/pesquisa/
       From: inst/doc/api-concepts.html
       Status: 404
       Message: Not Found

- This URL has been corrected to the proper and currently valid SIDRA address. 

Thanks!


