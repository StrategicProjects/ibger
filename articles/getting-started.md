# Getting started with ibger

## Overview

**ibger** provides a tidyverse-friendly interface to the [IBGE Aggregate
Data API](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3)
(version 3). This is the same API that powers
[SIDRA](https://sidra.ibge.gov.br/) — the automatic data retrieval
system for all surveys and censuses conducted by the Brazilian Institute
of Geography and Statistics (IBGE).

Each SIDRA table corresponds to an **aggregate** in the API. With ibger
you can browse aggregates, inspect their metadata, and retrieve tidy
data — all from R.

All the output shown in this vignette was produced by running the code
against the live API when the vignette was last built (see the date at
the end); numbers such as the catalog size or the latest period will
change over time.

## Installation

``` r

# CRAN release
install.packages("ibger")

# Development version
# install.packages("remotes")
remotes::install_github("StrategicProjects/ibger")
```

``` r

library(ibger)
```

## A typical workflow

### Step 1 — Find an aggregate

Use
[`ibge_aggregates()`](https://strategicprojects.github.io/ibger/reference/ibge_aggregates.md)
to list every aggregate grouped by survey. Optional filters let you
narrow the search. Subject codes come from
[`ibge_subjects()`](https://strategicprojects.github.io/ibger/reference/ibge_subjects.md);
`70` is “Abate de animais” (animal slaughter):

``` r

# All aggregates (the catalog is large: only the first rows are shown)
ibge_aggregates()
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [755ms]
#> 
#> ✔ 9336 aggregates found.
#> # A tibble: 9,336 × 4
#>    survey_id survey_name                             aggregate_id aggregate_name
#>    <chr>     <chr>                                   <chr>        <chr>         
#>  1 D5        "Áreas Urbanizadas do Brasil"           10763        Áreas urbaniz…
#>  2 D5        "Áreas Urbanizadas do Brasil"           8418         Áreas urbaniz…
#>  3 EO        "Avaliação dos dados sobre a Biodivers… 10510        Índice de con…
#>  4 EO        "Avaliação dos dados sobre a Biodivers… 10511        Quantidade de…
#>  5 EO        "Avaliação dos dados sobre a Biodivers… 10512        Categorias de…
#>  6 CL        "Cadastro Central de Empresas"          1685         Unidades loca…
#>  7 CL        "Cadastro Central de Empresas"          1732         Dados gerais …
#>  8 CL        "Cadastro Central de Empresas"          1733         Dados gerais …
#>  9 CL        "Cadastro Central de Empresas"          1734         Dados gerais …
#> 10 CL        "Cadastro Central de Empresas"          1735         Dados gerais …
#> # ℹ 9,326 more rows

# One subject: animal slaughter
ibge_aggregates(subject = 70)
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [155ms]
#> 
#> ✔ 16 aggregates found.
#> # A tibble: 16 × 4
#>    survey_id survey_name                             aggregate_id aggregate_name
#>    <chr>     <chr>                                   <chr>        <chr>         
#>  1 AB        Pesquisa Mensal de Abate de Animais     19           Quantidade de…
#>  2 AB        Pesquisa Mensal de Abate de Animais     20           Peso das carc…
#>  3 AB        Pesquisa Mensal de Abate de Animais     41           Quantidade de…
#>  4 AB        Pesquisa Mensal de Abate de Animais     42           Quantidade de…
#>  5 AB        Pesquisa Mensal de Abate de Animais     43           Quantidade de…
#>  6 AB        Pesquisa Mensal de Abate de Animais     44           Quantidade de…
#>  7 AB        Pesquisa Mensal de Abate de Animais     45           Peso das carc…
#>  8 AB        Pesquisa Mensal de Abate de Animais     46           Peso das carc…
#>  9 AB        Pesquisa Mensal de Abate de Animais     47           Peso das carc…
#> 10 AB        Pesquisa Mensal de Abate de Animais     48           Peso das carc…
#> 11 AX        Pesquisa Trimestral do Abate de Animais 1092         Número de inf…
#> 12 AX        Pesquisa Trimestral do Abate de Animais 1093         Número de inf…
#> 13 AX        Pesquisa Trimestral do Abate de Animais 1094         Número de inf…
#> 14 AX        Pesquisa Trimestral do Abate de Animais 6669         Quantidade e …
#> 15 AX        Pesquisa Trimestral do Abate de Animais 6829         Quantidade e …
#> 16 AX        Pesquisa Trimestral do Abate de Animais 9395         Número de inf…

# Monthly aggregates only
ibge_aggregates(periodicity = "P5")
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [75ms]
#> 
#> ✔ 288 aggregates found.
#> # A tibble: 288 × 4
#>    survey_id survey_name                            aggregate_id aggregate_name 
#>    <chr>     <chr>                                  <chr>        <chr>          
#>  1 IL        Índice de Preços ao Consumidor em Real 86           IPCR - Percent…
#>  2 IL        Índice de Preços ao Consumidor em Real 89           IPCR - Peso no…
#>  3 IR        Índice de Preços ao Produtor           3104         Índice de Preç…
#>  4 IR        Índice de Preços ao Produtor           5796         Índice de Preç…
#>  5 IR        Índice de Preços ao Produtor           5800         Índice de Preç…
#>  6 IR        Índice de Preços ao Produtor           6723         Índice de Preç…
#>  7 IR        Índice de Preços ao Produtor           6903         Índice de Preç…
#>  8 IR        Índice de Preços ao Produtor           6904         Índice de Preç…
#>  9 IJ        Índice de Reajuste do Salário Mínimo   90           IRSM - Percent…
#> 10 IJ        Índice de Reajuste do Salário Mínimo   91           IRSM - Peso no…
#> # ℹ 278 more rows

# Aggregates with municipality-level data
ibge_aggregates(level = "N6")
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [1.2s]
#> 
#> ✔ 4448 aggregates found.
#> # A tibble: 4,448 × 4
#>    survey_id survey_name                             aggregate_id aggregate_name
#>    <chr>     <chr>                                   <chr>        <chr>         
#>  1 D5        "Áreas Urbanizadas do Brasil"           10763        Áreas urbaniz…
#>  2 D5        "Áreas Urbanizadas do Brasil"           8418         Áreas urbaniz…
#>  3 EO        "Avaliação dos dados sobre a Biodivers… 10510        Índice de con…
#>  4 EO        "Avaliação dos dados sobre a Biodivers… 10511        Quantidade de…
#>  5 EO        "Avaliação dos dados sobre a Biodivers… 10512        Categorias de…
#>  6 CL        "Cadastro Central de Empresas"          1685         Unidades loca…
#>  7 CL        "Cadastro Central de Empresas"          1734         Dados gerais …
#>  8 CL        "Cadastro Central de Empresas"          1735         Dados gerais …
#>  9 CL        "Cadastro Central de Empresas"          2933         Empresas e ou…
#> 10 CL        "Cadastro Central de Empresas"          3421         Unidades loca…
#> # ℹ 4,438 more rows
```

The filters are checked before the request: the API silently ignores
values it cannot parse, so a typo would otherwise return the whole
catalog.

``` r

ibge_aggregates(periodicity = "monthly")
#> Error:
#> ! Invalid `periodicity` filter: "monthly".
#> ℹ Expected a single value like "P5".
#> ℹ See `?ibge_aggregates` for the accepted formats.
```

### Step 2 — Inspect the metadata

Once you have an aggregate ID,
[`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md)
tells you everything about its structure. Aggregate **1092** is the
quarterly animal slaughter table (number of establishments, animals
slaughtered and carcass weight):

``` r

meta <- ibge_metadata(1092)
#> ℹ Fetching metadata for aggregate 1092 from IBGE API...
#> ✔ Fetching metadata for aggregate 1092 from IBGE API... [161ms]
#> 
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories.
meta
#> 
#> ── Número de informantes, Quantidade e Peso total das carcaças dos bovinos abati
#> ID: 1092
#> Survey: Pesquisa Trimestral do Abate de Animais
#> Subject: Abate de animais
#> Periodicity: trimestral (199701 to 202602)
#> Territorial levels: N1, N3
#> 
#> ── Variables (6) ──
#> 
#>   151: Número de informantes (Unidades)
#>   1000151: Número de informantes - percentual do total geral (%)
#>   284: Animais abatidos (Cabeças)
#>   1000284: Animais abatidos - percentual do total geral (%)
#>   285: Peso total das carcaças (Quilogramas)
#>   1000285: Peso total das carcaças - percentual do total geral (%)
#> 
#> ── Classifications (3) ──
#> 
#>   12716: Referência temporal (4 categories)
#> 115236: Total do trimestre [level 0]
#> 115233: No 1º mês [level 1]
#> 115234: No 2º mês [level 1]
#> 115235: No 3º mês [level 1]
#>   18: Tipo de rebanho bovino (6 categories)
#> 992: Total [level 0]
#> 55: Bois [level 1]
#> 56: Vacas [level 1]
#> 111734: Novilhos [level 1]
#> 111735: Novilhas [level 1]
#> ... and 1 more categories
#>   12529: Tipo de inspeção (4 categories)
#> 118225: Total [level 0]
#> 111737: Federal [level 1]
#> 111738: Estadual [level 1]
#> 111739: Municipal [level 1]
#> 
#> Use `meta$variables` and `meta$classifications` to access the data.
#> Use `tidyr::unnest(meta$classifications, categories)` to unnest.
```

Each component is accessible directly:

``` r

meta$variables
#> # A tibble: 6 × 3
#>   id      name                                                unit       
#>   <chr>   <chr>                                               <chr>      
#> 1 151     Número de informantes                               Unidades   
#> 2 1000151 Número de informantes - percentual do total geral   %          
#> 3 284     Animais abatidos                                    Cabeças    
#> 4 1000284 Animais abatidos - percentual do total geral        %          
#> 5 285     Peso total das carcaças                             Quilogramas
#> 6 1000285 Peso total das carcaças - percentual do total geral %

meta$classifications
#> # A tibble: 3 × 3
#>   id    name                   categories      
#>   <chr> <chr>                  <list>          
#> 1 12716 Referência temporal    <tibble [4 × 4]>
#> 2 18    Tipo de rebanho bovino <tibble [6 × 4]>
#> 3 12529 Tipo de inspeção       <tibble [4 × 4]>

# Unnest to see every category
tidyr::unnest(meta$classifications, categories)
#> # A tibble: 14 × 6
#>    id    name             category_id category_name category_unit category_level
#>    <chr> <chr>            <chr>       <chr>         <chr>         <chr>         
#>  1 12716 Referência temp… 115236      Total do tri… <NA>          0             
#>  2 12716 Referência temp… 115233      No 1º mês     <NA>          1             
#>  3 12716 Referência temp… 115234      No 2º mês     <NA>          1             
#>  4 12716 Referência temp… 115235      No 3º mês     <NA>          1             
#>  5 18    Tipo de rebanho… 992         Total         <NA>          0             
#>  6 18    Tipo de rebanho… 55          Bois          <NA>          1             
#>  7 18    Tipo de rebanho… 56          Vacas         <NA>          1             
#>  8 18    Tipo de rebanho… 111734      Novilhos      <NA>          1             
#>  9 18    Tipo de rebanho… 111735      Novilhas      <NA>          1             
#> 10 18    Tipo de rebanho… 57          Vitelos e vi… <NA>          1             
#> 11 12529 Tipo de inspeção 118225      Total         <NA>          0             
#> 12 12529 Tipo de inspeção 111737      Federal       <NA>          1             
#> 13 12529 Tipo de inspeção 111738      Estadual      <NA>          1             
#> 14 12529 Tipo de inspeção 111739      Municipal     <NA>          1

# Geographic levels
meta$territorial_level
#> $administrative
#> [1] "N1" "N3"
#> 
#> $special
#> character(0)
#> 
#> $ibge
#> character(0)

# Time range
meta$periodicity
#> $frequency
#> [1] "trimestral"
#> 
#> $start
#> [1] 199701
#> 
#> $end
#> [1] 202602
```

### Step 3 — Retrieve data

[`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
is the main workhorse. It sends the request and returns a tidy tibble
(one row per variable × locality × period × category):

``` r

ibge_variables(1092, localities = "BR")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [73ms]
#> 
#> ✔ 18 records retrieved.
#> # A tibble: 18 × 11
#>    variable_id variable_name           variable_unit classification_18
#>    <chr>       <chr>                   <chr>         <chr>            
#>  1 151         Número de informantes   Unidades      Total            
#>  2 151         Número de informantes   Unidades      Total            
#>  3 151         Número de informantes   Unidades      Total            
#>  4 151         Número de informantes   Unidades      Total            
#>  5 151         Número de informantes   Unidades      Total            
#>  6 151         Número de informantes   Unidades      Total            
#>  7 284         Animais abatidos        Cabeças       Total            
#>  8 284         Animais abatidos        Cabeças       Total            
#>  9 284         Animais abatidos        Cabeças       Total            
#> 10 284         Animais abatidos        Cabeças       Total            
#> 11 284         Animais abatidos        Cabeças       Total            
#> 12 284         Animais abatidos        Cabeças       Total            
#> 13 285         Peso total das carcaças Quilogramas   Total            
#> 14 285         Peso total das carcaças Quilogramas   Total            
#> 15 285         Peso total das carcaças Quilogramas   Total            
#> 16 285         Peso total das carcaças Quilogramas   Total            
#> 17 285         Peso total das carcaças Quilogramas   Total            
#> 18 285         Peso total das carcaças Quilogramas   Total            
#> # ℹ 7 more variables: classification_12529 <chr>, classification_12716 <chr>,
#> #   locality_id <chr>, locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>
```

## Specifying localities

The `localities` parameter accepts several convenient formats:

``` r

# Country total
ibge_variables(1092, variable = 284, periods = -1, localities = "BR")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [74ms]
#> 
#> ✔ 1 record retrieved.
#> # A tibble: 1 × 11
#>   variable_id variable_name variable_unit classification_18 classification_12529
#>   <chr>       <chr>         <chr>         <chr>             <chr>               
#> 1 284         Animais abat… Cabeças       Total             Total               
#> # ℹ 6 more variables: classification_12716 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# All states
ibge_variables(1092, variable = 284, periods = -1, localities = "N3")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching N3 localities for aggregate 1092 from IBGE API...
#> ✔ Fetching N3 localities for aggregate 1092 from IBGE API... [142ms]
#> 
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [69ms]
#> 
#> ✔ 27 records retrieved.
#> # A tibble: 27 × 11
#>    variable_id variable_name    variable_unit classification_18
#>    <chr>       <chr>            <chr>         <chr>            
#>  1 284         Animais abatidos Cabeças       Total            
#>  2 284         Animais abatidos Cabeças       Total            
#>  3 284         Animais abatidos Cabeças       Total            
#>  4 284         Animais abatidos Cabeças       Total            
#>  5 284         Animais abatidos Cabeças       Total            
#>  6 284         Animais abatidos Cabeças       Total            
#>  7 284         Animais abatidos Cabeças       Total            
#>  8 284         Animais abatidos Cabeças       Total            
#>  9 284         Animais abatidos Cabeças       Total            
#> 10 284         Animais abatidos Cabeças       Total            
#> # ℹ 17 more rows
#> # ℹ 7 more variables: classification_12529 <chr>, classification_12716 <chr>,
#> #   locality_id <chr>, locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>

# Specific states (RJ = 33, SP = 35)
ibge_variables(1092, variable = 284, periods = -1,
               localities = list(N3 = c(33, 35)))
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [69ms]
#> 
#> ✔ 2 records retrieved.
#> # A tibble: 2 × 11
#>   variable_id variable_name variable_unit classification_18 classification_12529
#>   <chr>       <chr>         <chr>         <chr>             <chr>               
#> 1 284         Animais abat… Cabeças       Total             Total               
#> 2 284         Animais abat… Cabeças       Total             Total               
#> # ℹ 6 more variables: classification_12716 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# Mix levels: two metropolitan areas + one municipality
# (IPCA, aggregate 7060, is published for metropolitan areas and capitals)
ibge_variables(7060, variable = 63, periods = -1,
               localities = list(N7 = c(3501, 3301), N6 = 5208707))
#> ℹ Fetching metadata for aggregate 7060 from IBGE API...
#> ✔ Fetching metadata for aggregate 7060 from IBGE API... [74ms]
#> 
#> ✔ Aggregate 7060: 4 variables,
#> 1 classification, 457 categories.
#> ℹ Fetching variables for aggregate 7060 from IBGE API...
#> ✔ Fetching variables for aggregate 7060 from IBGE API... [123ms]
#> 
#> ✔ 3 records retrieved.
#> # A tibble: 3 × 9
#>   variable_id variable_name         variable_unit classification_315 locality_id
#>   <chr>       <chr>                 <chr>         <chr>              <chr>      
#> 1 63          IPCA - Variação mens… %             Índice geral       3501       
#> 2 63          IPCA - Variação mens… %             Índice geral       3301       
#> 3 63          IPCA - Variação mens… %             Índice geral       5208707    
#> # ℹ 4 more variables: locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>
```

The geographic level codes follow the IBGE convention:

| Code | Level             | Example                             |
|------|-------------------|-------------------------------------|
| `N1` | Brazil            | `"BR"` or `list(N1 = 1)`            |
| `N2` | Major region      | `list(N2 = 1)` — North              |
| `N3` | State (UF)        | `list(N3 = 33)` — Rio de Janeiro    |
| `N6` | Municipality      | `list(N6 = 3550308)` — São Paulo/SP |
| `N7` | Metropolitan area | `list(N7 = 3501)` — RM São Paulo    |

> **Tip**: Not every aggregate is available at every level. Aggregate
> 1092 has data for N1 and N3 but not N6. Use
> [`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md)
> to check, or rely on the validation described below.

## Specifying periods

Periods follow the API convention — negative values mean “last N”:

``` r

# Last 6 periods (the default)
ibge_variables(1092, variable = 284, periods = -6, localities = "BR")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [70ms]
#> 
#> ✔ 6 records retrieved.
#> # A tibble: 6 × 11
#>   variable_id variable_name variable_unit classification_18 classification_12529
#>   <chr>       <chr>         <chr>         <chr>             <chr>               
#> 1 284         Animais abat… Cabeças       Total             Total               
#> 2 284         Animais abat… Cabeças       Total             Total               
#> 3 284         Animais abat… Cabeças       Total             Total               
#> 4 284         Animais abat… Cabeças       Total             Total               
#> 5 284         Animais abat… Cabeças       Total             Total               
#> 6 284         Animais abat… Cabeças       Total             Total               
#> # ℹ 6 more variables: classification_12716 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# Last 12 periods
ibge_variables(1092, variable = 284, periods = -12, localities = "BR")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [80ms]
#> 
#> ✔ 12 records retrieved.
#> # A tibble: 12 × 11
#>    variable_id variable_name    variable_unit classification_18
#>    <chr>       <chr>            <chr>         <chr>            
#>  1 284         Animais abatidos Cabeças       Total            
#>  2 284         Animais abatidos Cabeças       Total            
#>  3 284         Animais abatidos Cabeças       Total            
#>  4 284         Animais abatidos Cabeças       Total            
#>  5 284         Animais abatidos Cabeças       Total            
#>  6 284         Animais abatidos Cabeças       Total            
#>  7 284         Animais abatidos Cabeças       Total            
#>  8 284         Animais abatidos Cabeças       Total            
#>  9 284         Animais abatidos Cabeças       Total            
#> 10 284         Animais abatidos Cabeças       Total            
#> 11 284         Animais abatidos Cabeças       Total            
#> 12 284         Animais abatidos Cabeças       Total            
#> # ℹ 7 more variables: classification_12529 <chr>, classification_12716 <chr>,
#> #   locality_id <chr>, locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>

# Specific period codes (IPCA is monthly: 202301 = January 2023)
ibge_variables(7060, variable = 63, periods = c(202301, 202302, 202303),
               localities = "BR")
#> ✔ Aggregate 7060: 4 variables,
#> 1 classification, 457 categories (cached).
#> ℹ Fetching variables for aggregate 7060 from IBGE API...
#> ✔ Fetching variables for aggregate 7060 from IBGE API... [72ms]
#> 
#> ✔ 3 records retrieved.
#> # A tibble: 3 × 9
#>   variable_id variable_name         variable_unit classification_315 locality_id
#>   <chr>       <chr>                 <chr>         <chr>              <chr>      
#> 1 63          IPCA - Variação mens… %             Índice geral       1          
#> 2 63          IPCA - Variação mens… %             Índice geral       1          
#> 3 63          IPCA - Variação mens… %             Índice geral       1          
#> # ℹ 4 more variables: locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>

# Range (inclusive)
ibge_variables(7060, variable = 63, periods = "202101-202104",
               localities = "BR")
#> ✔ Aggregate 7060: 4 variables,
#> 1 classification, 457 categories (cached).
#> ℹ Fetching variables for aggregate 7060 from IBGE API...
#> ✔ Fetching variables for aggregate 7060 from IBGE API... [159ms]
#> 
#> ✔ 4 records retrieved.
#> # A tibble: 4 × 9
#>   variable_id variable_name         variable_unit classification_315 locality_id
#>   <chr>       <chr>                 <chr>         <chr>              <chr>      
#> 1 63          IPCA - Variação mens… %             Índice geral       1          
#> 2 63          IPCA - Variação mens… %             Índice geral       1          
#> 3 63          IPCA - Variação mens… %             Índice geral       1          
#> 4 63          IPCA - Variação mens… %             Índice geral       1          
#> # ℹ 4 more variables: locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>

# Range + extra period
ibge_variables(7060, variable = 63, periods = "202101-202103|202301",
               localities = "BR")
#> ✔ Aggregate 7060: 4 variables,
#> 1 classification, 457 categories (cached).
#> ℹ Fetching variables for aggregate 7060 from IBGE API...
#> ✔ Fetching variables for aggregate 7060 from IBGE API... [76ms]
#> 
#> ✔ 4 records retrieved.
#> # A tibble: 4 × 9
#>   variable_id variable_name         variable_unit classification_315 locality_id
#>   <chr>       <chr>                 <chr>         <chr>              <chr>      
#> 1 63          IPCA - Variação mens… %             Índice geral       1          
#> 2 63          IPCA - Variação mens… %             Índice geral       1          
#> 3 63          IPCA - Variação mens… %             Índice geral       1          
#> 4 63          IPCA - Variação mens… %             Índice geral       1          
#> # ℹ 4 more variables: locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <chr>
```

> **Note**: Negative values cannot be mixed with specific periods.
> Period codes encode both the date and the periodicity — `202001` means
> January 2020 in a monthly aggregate but the first quarter of 2020 in a
> quarterly one.
> [`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md)
> reports the periodicity, and
> [`ibge_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_periods.md)
> lists the valid codes.

## Filtering with classifications

Many aggregates break their data further by classifications
(dimensions). For instance, aggregate 1712 (temporary crops in the 2006
Agricultural Census) has a classification for the product (226) and
another for the producer’s condition (218).

``` r

# Single category: pineapple (4844) from product classification (226)
ibge_variables(
  aggregate      = 1712,
  variable       = 214,
  localities     = "BR",
  classification = list("226" = 4844)
)
#> ℹ Fetching metadata for aggregate 1712 from IBGE API...
#> ✔ Fetching metadata for aggregate 1712 from IBGE API... [71ms]
#> 
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories.
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [73ms]
#> 
#> ✔ 1 record retrieved.
#> # A tibble: 1 × 14
#>   variable_id variable_name  variable_unit classification_226 classification_218
#>   <chr>       <chr>          <chr>         <chr>              <chr>             
#> 1 214         Quantidade pr… Mil frutos    Abacaxi            Total             
#> # ℹ 9 more variables: classification_220 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# Multiple categories
ibge_variables(
  aggregate      = 1712,
  variable       = 214,
  localities     = "BR",
  classification = list("226" = c(4844, 96608, 96609))
)
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [76ms]
#> 
#> ✔ 3 records retrieved.
#> # A tibble: 3 × 14
#>   variable_id variable_name  variable_unit classification_226 classification_218
#>   <chr>       <chr>          <chr>         <chr>              <chr>             
#> 1 214         Quantidade pr… Mil frutos    Abacaxi            Total             
#> 2 214         Quantidade pr… Mil frutos    Amendoim em casca  Total             
#> 3 214         Quantidade pr… Mil frutos    Batata-inglesa     Total             
#> # ℹ 9 more variables: classification_220 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# Multiple classifications
ibge_variables(
  aggregate      = 1712,
  variable       = 214,
  localities     = "BR",
  classification = list("226" = c(4844, 96608), "218" = 4780)
)
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [108ms]
#> 
#> ✔ 2 records retrieved.
#> # A tibble: 2 × 14
#>   variable_id variable_name  variable_unit classification_226 classification_218
#>   <chr>       <chr>          <chr>         <chr>              <chr>             
#> 1 214         Quantidade pr… Mil frutos    Abacaxi            Proprietário      
#> 2 214         Quantidade pr… Mil frutos    Amendoim em casca  Proprietário      
#> # ℹ 9 more variables: classification_220 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# All categories of a classification
ibge_variables(
  aggregate      = 1712,
  variable       = 214,
  localities     = "BR",
  classification = list("226" = "all")
)
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [77ms]
#> 
#> ✔ 53 records retrieved.
#> # A tibble: 53 × 14
#>    variable_id variable_name variable_unit classification_226 classification_218
#>    <chr>       <chr>         <chr>         <chr>              <chr>             
#>  1 214         Quantidade p… Unidades      Total              Total             
#>  2 214         Quantidade p… Unidades      Abacaxi            Total             
#>  3 214         Quantidade p… Unidades      Abóbora, moranga,… Total             
#>  4 214         Quantidade p… Unidades      Algodão herbáceo   Total             
#>  5 214         Quantidade p… Unidades      Alho               Total             
#>  6 214         Quantidade p… Unidades      Amendoim em casca  Total             
#>  7 214         Quantidade p… Unidades      Arroz em casca     Total             
#>  8 214         Quantidade p… Unidades      Aveia branca em g… Total             
#>  9 214         Quantidade p… Unidades      Batata-inglesa     Total             
#> 10 214         Quantidade p… Unidades      Cana-de-açúcar     Total             
#> # ℹ 43 more rows
#> # ℹ 9 more variables: classification_220 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>
```

When no classification is specified, the API returns the **Total**
category (ID = 0) — an aggregate across all categories.

## Automatic validation

Before sending any request,
[`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
and
[`ibge_localities()`](https://strategicprojects.github.io/ibger/reference/ibge_localities.md)
validate your parameters against the aggregate’s metadata. If something
doesn’t match, you get a clear error with the allowed values:

``` r

# N6 (municipalities) is not available for aggregate 1092
ibge_variables(1092, localities = "N6")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> Error:
#> ! Geographic level(s) "N6" not available for aggregate 1092.
#> ℹ Available levels: "N1" and "N3".

# Period out of range
ibge_variables(1092, periods = 199001, localities = "BR")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> Error:
#> ! Period(s) 199001 out of range for aggregate 1092.
#> ℹ Valid range: 199701 to 202602 (trimestral).

# Non-existent variable
ibge_variables(1092, variable = 999, localities = "BR")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> Error:
#> ! Variable(s) "999" not found in aggregate 1092.
#> ℹ Available variables:
#> 151 - Número de informantes (Unidades)
#> 1000151 - Número de informantes - percentual do total geral (%)
#> 284 - Animais abatidos (Cabeças)
#> 1000284 - Animais abatidos - percentual do total geral (%)
#> 285 - Peso total das carcaças (Quilogramas)
#> 1000285 - Peso total das carcaças - percentual do total geral (%)

# Non-existent category
ibge_variables(1092, localities = "BR", classification = list("18" = 1))
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> Error:
#> ! Category(ies) "1" not found in classification "18" (Tipo de rebanho
#>   bovino).
#> ℹ First categories available (6 total):
#> 992 - Total
#> 55 - Bois
#> 56 - Vacas
#> 111734 - Novilhos
#> 111735 - Novilhas
#> 57 - Vitelos e vitelas
```

Metadata is fetched once per session and cached. To force a refresh:

``` r

ibge_clear_cache()
#> ✔ Metadata cache cleared.
```

Skip validation entirely with `validate = FALSE`:

``` r

ibge_variables(1092, variable = 284, periods = -1, localities = "BR",
               validate = FALSE)
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [75ms]
#> 
#> ✔ 1 record retrieved.
#> # A tibble: 1 × 11
#>   variable_id variable_name variable_unit classification_18 classification_12529
#>   <chr>       <chr>         <chr>         <chr>             <chr>               
#> 1 284         Animais abat… Cabeças       Total             Total               
#> # ℹ 6 more variables: classification_12716 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>
```

## Large queries

The API rejects requests whose result is too large (see
[`?ibge_variables`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
and the API-concepts vignette). You do not need to split such queries by
hand:
[`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
estimates the result size from the metadata and, when it exceeds the
limit, splits the request into several smaller ones and combines the
results. The query below asks for the estimated population of every
municipality (aggregate 6579, about 5,570 localities) for the last 12
years — some 67,000 values — and is transparently sent as two requests:

``` r

pop <- ibge_variables(6579, localities = "N6", periods = -12)
#> ℹ Fetching metadata for aggregate 6579 from IBGE API...
#> ✔ Fetching metadata for aggregate 6579 from IBGE API... [70ms]
#> 
#> ✔ Aggregate 6579: 1 variable,
#> 0 classifications, 0 categories.
#> ℹ Fetching N6 localities for aggregate 6579 from IBGE API...
#> ✔ Fetching N6 localities for aggregate 6579 from IBGE API... [166ms]
#> 
#> ℹ Fetching period list for aggregate 6579 from IBGE API...
#> ✔ Fetching period list for aggregate 6579 from IBGE API... [71ms]
#> 
#> ℹ Estimated result exceeds the API limit (50000 values);
#> splitting into 2 requests.
#> ℹ Fetching chunk 1/2 for aggregate 6579 from IBGE API...
#> ✔ Fetching chunk 1/2 for aggregate 6579 from IBGE API... [186ms]
#> 
#> ℹ Fetching chunk 2/2 for aggregate 6579 from IBGE API...
#> ✔ Fetching chunk 2/2 for aggregate 6579 from IBGE API... [375ms]
#> 
#> ✔ 66852 records retrieved.
pop
#> # A tibble: 66,852 × 8
#>    variable_id variable_name             variable_unit locality_id locality_name
#>    <chr>       <chr>                     <chr>         <chr>       <chr>        
#>  1 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  2 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  3 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  4 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  5 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  6 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  7 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  8 9324        População residente esti… Pessoas       1100015     Alta Florest…
#>  9 9324        População residente esti… Pessoas       1100023     Ariquemes - …
#> 10 9324        População residente esti… Pessoas       1100023     Ariquemes - …
#> # ℹ 66,842 more rows
#> # ℹ 3 more variables: locality_level <chr>, period <chr>, value <chr>
```

Municipality-level requests are slow on the server side (one to several
minutes each), so expect this to take a while. Use `chunk = FALSE` to
force a single request, or pass a number to change the per-request
limit.

## Browsing the survey catalog

Beyond aggregate-level data, ibger also provides access to the [IBGE
Metadata
API](https://servicodados.ibge.gov.br/api/docs/metadados?versao=2) (v2),
which catalogs IBGE’s surveys with institutional and methodological
information such as status, category, collection frequency, and thematic
classifications.

This is useful when you want to understand **what surveys exist** and
**how they are structured** before diving into specific aggregates.

``` r

# List all IBGE surveys
ibge_surveys()
#> ℹ Fetching survey catalog from IBGE Metadata API...
#> ✔ Fetching survey catalog from IBGE Metadata API... [83ms]
#> 
#> ✔ 100 surveys found.
#> # A tibble: 100 × 8
#>    id    name                       name_en status category collection_frequency
#>    <chr> <chr>                      <chr>   <chr>  <chr>    <chr>               
#>  1 AC    "Pesquisa Anual da Indúst… <NA>    Ativa  Estrutu… Anual               
#>  2 AA    "Pesquisa Nacional de Saú… <NA>    Ativa  Especial Eventual            
#>  3 AM    "Pesquisa de Assistência … <NA>    Desat… Especial Eventual            
#>  4 AX    "Pesquisa Trimestral do A… Quarte… Ativa  Conjunt… Trimestral          
#>  5 CD    "Censo Demográfico"        Popula… Ativa  Estrutu… Decenal             
#>  6 CL    "Cadastro Central de Empr… Busine… Ativa  Estrutu… Anual               
#>  7 EI    "Economia Informal Urbana" Urban … Concl… Outro    Eventual            
#>  8 ER    "Pesquisa das Característ… Ethno-… Desat… Especial Eventual            
#>  9 FP    "Fundações Privadas e Ass… <NA>    Ativa  Outro    Eventual            
#> 10 IA    "Índice Nacional de Preço… <NA>    Ativa  Conjunt… Mensal              
#> # ℹ 90 more rows
#> # ℹ 2 more variables: publication_frequency <chr>,
#> #   thematic_classifications <list>

# Filter active short-term ("conjuntural") surveys
library(dplyr)
#> 
#> Anexando pacote: 'dplyr'
#> Os seguintes objetos são mascarados por 'package:stats':
#> 
#>     filter, lag
#> Os seguintes objetos são mascarados por 'package:base':
#> 
#>     intersect, setdiff, setequal, union
ibge_surveys(thematic_classifications = FALSE) |>
  filter(status == "Ativa", category == "Conjuntural")
#> ℹ Fetching survey catalog from IBGE Metadata API...
#> ✔ Fetching survey catalog from IBGE Metadata API... [154ms]
#> 
#> ✔ 100 surveys found.
#> # A tibble: 17 × 7
#>    id    name                       name_en status category collection_frequency
#>    <chr> <chr>                      <chr>   <chr>  <chr>    <chr>               
#>  1 AX    Pesquisa Trimestral do Ab… Quarte… Ativa  Conjunt… Trimestral          
#>  2 IA    Índice Nacional de Preços… <NA>    Ativa  Conjunt… Mensal              
#>  3 ST    Contas Nacionais Trimestr… <NA>    Ativa  Conjunt… Trimestral          
#>  4 IR    Índice de Preços ao Produ… <NA>    Ativa  Conjunt… Mensal              
#>  5 LT    Pesquisa Trimestral do Le… Quarte… Ativa  Conjunt… Trimestral          
#>  6 MC    Pesquisa Mensal de Comérc… Monthl… Ativa  Conjunt… Mensal              
#>  7 PC    Índice Nacional de Preços… <NA>    Ativa  Conjunt… Mensal              
#>  8 PO    Produção de Ovos de Galin… Quarte… Ativa  Conjunt… Trimestral          
#>  9 SC    Pesquisa Mensal de Serviç… <NA>    Ativa  Conjunt… Mensal              
#> 10 DD    Pesquisa Nacional por Amo… <NA>    Ativa  Conjunt… Trimestral          
#> 11 ES    Pesquisa de Estoques       Survey… Ativa  Conjunt… Semestral           
#> 12 BB    Pesquisa Nacional por Amo… <NA>    Ativa  Conjunt… Mensal (Trimestre M…
#> 13 SI    Sistema Nacional de Pesqu… <NA>    Ativa  Conjunt… Mensal              
#> 14 CQ    Pesquisa Trimestral do Co… Quarte… Ativa  Conjunt… Trimestral          
#> 15 IQ    Índice Nacional de Preços… <NA>    Ativa  Conjunt… Mensal              
#> 16 PZ    Pesquisa Industrial Mensa… <NA>    Ativa  Conjunt… Mensal              
#> 17 LA    Levantamento Sistemático … System… Ativa  Conjunt… Mensal              
#> # ℹ 1 more variable: publication_frequency <chr>

# Check which periods have metadata for the Censo Demográfico
ibge_survey_periods("CD")
#> ℹ Fetching survey catalog from IBGE Metadata API...
#> ✔ Fetching survey catalog from IBGE Metadata API... [79ms]
#> 
#> ℹ Fetching periods for survey CD from IBGE Metadata API...
#> ✔ Fetching periods for survey CD from IBGE Metadata API... [69ms]
#> 
#> ✔ 2 periods found for survey "CD".
#> # A tibble: 2 × 3
#>    year month order
#>   <int> <int> <int>
#> 1  2010    NA    NA
#> 2  2022    NA    NA

# Get full institutional metadata for a specific period
meta_cd <- ibge_survey_metadata("CD", year = 2022)
#> ℹ Fetching metadata for CD (2022) from IBGE Metadata API...
#> ✔ Fetching metadata for CD (2022) from IBGE Metadata API... [89ms]
#> 
#> ✔ Survey "CD" (2022): 1 metadata occurrence.
meta_cd
#> 
#> ── CD ──────────────────────────────────────────────────────────────────────────
#> Status: Ativa
#> Category: Estrutural
#> Type: Censo demográfico
#> Area: Estatísticos
#> Started: 01/01/1872
#> 
#> ── Thematic classifications (19) ──
#> 
#> ── Metadata occurrences (1) ──
#> 
#> Use `meta$occurrences` to explore the full metadata.
#> Fields: "sigla", "data_inicio_coleta", "data_fim_coleta",
#> "data_previsao_divulgacao", "historico", "historico_ingles", "objetivo", and
#> "objetivo_ingles" ... and 69 more

# Explore methodology fields
names(meta_cd$occurrences[[1]])
#>  [1] "sigla"                                                        
#>  [2] "data_inicio_coleta"                                           
#>  [3] "data_fim_coleta"                                              
#>  [4] "data_previsao_divulgacao"                                     
#>  [5] "historico"                                                    
#>  [6] "historico_ingles"                                             
#>  [7] "objetivo"                                                     
#>  [8] "objetivo_ingles"                                              
#>  [9] "populacao_alvo"                                               
#> [10] "populacao_alvo_ingles"                                        
#> [11] "metodologia"                                                  
#> [12] "metodologia_ingles"                                           
#> [13] "notas_ocorrencia"                                             
#> [14] "notas_ocorrencia_ingles"                                      
#> [15] "ind_divulgacao"                                               
#> [16] "principais_variaveis"                                         
#> [17] "principais_variaveis_ingles"                                  
#> [18] "niveis_divulgacao"                                            
#> [19] "niveis_divulgacao_ingles"                                     
#> [20] "nivel_geografico_abrangencia"                                 
#> [21] "nivel_geografico_abrangencia_ingles"                          
#> [22] "nivel_geografico_desagregacao"                                
#> [23] "nivel_geografico_desagregacao_ingles"                         
#> [24] "instituicoes_responsaveis"                                    
#> [25] "periodos_referencia"                                          
#> [26] "tipos_dados"                                                  
#> [27] "unidades_informante"                                          
#> [28] "unidades_investigacao"                                        
#> [29] "unidades_analise"                                             
#> [30] "tecnicas_coleta"                                              
#> [31] "formas_disseminacao"                                          
#> [32] "nivel_desagregacao"                                           
#> [33] "nivel_desagregacao_ingles"                                    
#> [34] "instrumentos_coleta"                                          
#> [35] "palavras_chave"                                               
#> [36] "palavras_chave_ingles"                                        
#> [37] "definicao_fatores_expansao"                                   
#> [38] "definicao_fatores_expansao_ingles"                            
#> [39] "notas_coleta_dados"                                           
#> [40] "notas_coleta_dados_ingles"                                    
#> [41] "supervisao"                                                   
#> [42] "supervisao_ingles"                                            
#> [43] "critica_imputacao"                                            
#> [44] "critica_imputacao_ingles"                                     
#> [45] "outras_info_processamento"                                    
#> [46] "outras_info_processamento_ingles"                             
#> [47] "calculo_erro_amostral"                                        
#> [48] "calculo_erro_amostral_ingles"                                 
#> [49] "outras_info_qualidade"                                        
#> [50] "outras_info_qualidade_ingles"                                 
#> [51] "autoridade_acesso"                                            
#> [52] "autoridade_acesso_ingles"                                     
#> [53] "confidencialidade"                                            
#> [54] "confidencialidade_ingles"                                     
#> [55] "condicoes_acesso"                                             
#> [56] "condicoes_acesso_ingles"                                      
#> [57] "referencia_citacao_fonte"                                     
#> [58] "referencia_citacao_fonte_ingles"                              
#> [59] "base_legal"                                                   
#> [60] "base_legal_ingles"                                            
#> [61] "direitos_autorais"                                            
#> [62] "direitos_autorais_ingles"                                     
#> [63] "contato"                                                      
#> [64] "contato_ingles"                                               
#> [65] "procedimento_amostragem"                                      
#> [66] "procedimento_amostragem_ingles"                               
#> [67] "ocorrencias_variavel"                                         
#> [68] "qualidade"                                                    
#> [69] "observacoes_sidra"                                            
#> [70] "observacoes_sidra_ingles"                                     
#> [71] "recomendacoes_internacionais_referencias_metodologicas"       
#> [72] "recomendacoes_internacionais_referencias_metodologicas_ingles"
#> [73] "ano"                                                          
#> [74] "mes"                                                          
#> [75] "ordem_periodo"                                                
#> [76] "nome_ocorrencia"                                              
#> [77] "nome_ocorrencia_ingles"
```

Survey codes are validated before each request. If you use a wrong code,
the error suggests similar alternatives:

``` r

ibge_survey_periods("PMS")
#> Error:
#> ! Survey code "PMS" not found in the IBGE catalog.
#> ℹ Did you mean one of these?
#> •  PM - Pesquisa de Informações Básicas Municipais
#> •  AM - Pesquisa de Assistência Médico-Sanitária
#> •  PT - Pesquisa de Serviços de Tecnologia da Informação
#> •  TS - Conta Satélite da Saúde
#> •  VS - Produção da Extração Vegetal e da Silvicultura
#> → Use `ibge_surveys()` to see all 100 valid codes.
```

## Special values

The `value` column may contain special codes instead of numbers (`-`,
`..`, `...`, `X`), so it comes through as a character column. Use
[`parse_ibge_value()`](https://strategicprojects.github.io/ibger/reference/parse_ibge_value.md)
to convert it to numeric in one step — its documentation
([`?parse_ibge_value`](https://strategicprojects.github.io/ibger/reference/parse_ibge_value.md))
has the full table of codes and how each one is handled:

``` r

ibge_variables(7060, variable = 63, localities = "BR") |>
  mutate(value = parse_ibge_value(value))
#> ℹ Fetching metadata for aggregate 7060 from IBGE API...
#> ✔ Fetching metadata for aggregate 7060 from IBGE API... [114ms]
#> 
#> ✔ Aggregate 7060: 4 variables,
#> 1 classification, 457 categories.
#> ℹ Fetching variables for aggregate 7060 from IBGE API...
#> ✔ Fetching variables for aggregate 7060 from IBGE API... [104ms]
#> 
#> ✔ 6 records retrieved.
#> # A tibble: 6 × 9
#>   variable_id variable_name         variable_unit classification_315 locality_id
#>   <chr>       <chr>                 <chr>         <chr>              <chr>      
#> 1 63          IPCA - Variação mens… %             Índice geral       1          
#> 2 63          IPCA - Variação mens… %             Índice geral       1          
#> 3 63          IPCA - Variação mens… %             Índice geral       1          
#> 4 63          IPCA - Variação mens… %             Índice geral       1          
#> 5 63          IPCA - Variação mens… %             Índice geral       1          
#> 6 63          IPCA - Variação mens… %             Índice geral       1          
#> # ℹ 4 more variables: locality_name <chr>, locality_level <chr>, period <chr>,
#> #   value <dbl>
```

------------------------------------------------------------------------

This vignette was last built on 2026-09-22 against the live IBGE API.
