# IBGE subject codes lookup

Returns a tibble with IBGE subject (theme) codes and descriptions. These
codes can be used to understand the thematic organization of IBGE
surveys and aggregates.

## Usage

``` r
ibge_subjects(pattern = NULL, ignore_case = TRUE)
```

## Arguments

- pattern:

  Optional character string. A regular expression used to filter subject
  descriptions. If `NULL` (default), returns the full table. Matching is
  case-insensitive by default.

- ignore_case:

  Logical. If `TRUE` (default), pattern matching ignores case.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with
columns:

- id:

  Integer subject code.

- name:

  Character subject description.

## Details

The table is built-in (no API call is made) and contains all subject
codes currently defined by IBGE.

## Examples

``` r
# All subjects
ibge_subjects()
#> ✔ 254 subjects loaded.
#> # A tibble: 254 × 2
#>       id name                                        
#>    <int> <chr>                                       
#>  1   148 Abastecimento de água                       
#>  2    70 Abate de animais                            
#>  3   110 Acesso a esgotamento sanitário              
#>  4   147 Acesso à internet                           
#>  5   107 Acesso a serviço de coleta de lixo doméstico
#>  6   146 Acesso a serviços de telefonia              
#>  7   109 Acesso a sistema de abastecimento de água   
#>  8   316 Acidentes                                   
#>  9   221 Acidentes, violência e segurança            
#> 10   128 Adequação de moradia                        
#> # ℹ 244 more rows

# Find sanitation-related themes
ibge_subjects("saneamento|esgoto|lixo")
#> ✔ 8 subjects matched pattern "saneamento|esgoto|lixo".
#> # A tibble: 8 × 2
#>      id name                                                   
#>   <int> <chr>                                                  
#> 1   107 Acesso a serviço de coleta de lixo doméstico           
#> 2   296 Água potável e saneamento                              
#> 3   141 Coleta seletiva de lixo                                
#> 4   108 Destinação final do lixo                               
#> 5   124 Doenças relacionadas ao saneamento ambiental inadequado
#> 6   153 Gestão municipal de saneamento básico                  
#> 7   151 Limpeza urbana e Coleta de lixo                        
#> 8   111 Tratamento de esgoto                                   

# Find themes mentioning "internet"
ibge_subjects("internet")
#> ✔ 2 subjects matched pattern "internet".
#> # A tibble: 2 × 2
#>      id name             
#>   <int> <chr>            
#> 1   147 Acesso à internet
#> 2   201 Internet         
```
