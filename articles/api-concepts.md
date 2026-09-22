# Understanding the IBGE Aggregate Data API

## Introduction

The IBGE Aggregate Data API (version 3) is the programmatic interface
behind [SIDRA](https://sidra.ibge.gov.br/), IBGE’s automatic data
retrieval system. It covers every survey and census produced by the
Brazilian Institute of Geography and Statistics.

This vignette explains the API’s data model so you can make the most of
ibger. If you’re familiar with OLAP terminology: variables = measures,
classifications = dimensions, and categories = members.

All output below comes from running the code against the live API when
the vignette was last built (date at the end), so counts and latest
periods will drift over time.

## Core concepts

### Aggregates

An **aggregate** is a specific table of results from an IBGE survey.
Each aggregate has a numeric ID that is stable over time. For example:

- **1092** — Número de informantes, Quantidade e Peso total das carcaças
  dos animais abatidos (quarterly animal slaughter)
- **1712** — Produção, venda, valor da produção e área colhida da
  lavoura temporária (temporary crops, 2006 Agricultural Census)
- **7060** — IPCA — Variação mensal, acumulada no ano, acumulada em 12
  meses e peso mensal (consumer price index)

``` r

library(ibger)

# Search for aggregates
ibge_aggregates()
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [805ms]
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
```

You can filter by periodicity, geographic level, subject, or
classification:

``` r

# Only quarterly aggregates
ibge_aggregates(periodicity = "P9")
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [136ms]
#> 
#> ✔ 94 aggregates found.
#> # A tibble: 94 × 4
#>    survey_id survey_name                  aggregate_id aggregate_name           
#>    <chr>     <chr>                        <chr>        <chr>                    
#>  1 ST        Contas Nacionais Trimestrais 1620         Série encadeada do índic…
#>  2 ST        Contas Nacionais Trimestrais 1621         Série encadeada do índic…
#>  3 ST        Contas Nacionais Trimestrais 1846         Valores a preços corrent…
#>  4 ST        Contas Nacionais Trimestrais 2072         Contas econômicas trimes…
#>  5 ST        Contas Nacionais Trimestrais 2205         Conta financeira trimest…
#>  6 ST        Contas Nacionais Trimestrais 5932         Taxa de variação do índi…
#>  7 ST        Contas Nacionais Trimestrais 6612         Valores encadeados a pre…
#>  8 ST        Contas Nacionais Trimestrais 6613         Valores encadeados a pre…
#>  9 ST        Contas Nacionais Trimestrais 6726         Taxa de poupança         
#> 10 ST        Contas Nacionais Trimestrais 6727         Taxa de investimento     
#> # ℹ 84 more rows

# Aggregates that have state-level data
ibge_aggregates(level = "N3")
#> ℹ Fetching aggregates from IBGE API...
#> ✔ Fetching aggregates from IBGE API... [166ms]
#> 
#> ✔ 6269 aggregates found.
#> # A tibble: 6,269 × 4
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
#> # ℹ 6,259 more rows
```

Filters are checked for the format the API expects before the request is
sent (the API ignores what it cannot parse and returns the whole
catalog):

``` r

ibge_aggregates(periodicity = "quarterly")
#> Error:
#> ! Invalid `periodicity` filter: "quarterly".
#> ℹ Expected a single value like "P5".
#> ℹ See `?ibge_aggregates` for the accepted formats.
```

### Variables

Each aggregate exposes one or more **variables** — the measures being
reported. For aggregate 1712 (crop production):

``` r

meta <- ibge_metadata(1712)
#> ℹ Fetching metadata for aggregate 1712 from IBGE API...
#> ✔ Fetching metadata for aggregate 1712 from IBGE API... [71ms]
#> 
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories.
meta$variables
#> # A tibble: 7 × 3
#>   id      name                                          unit                    
#>   <chr>   <chr>                                         <chr>                   
#> 1 183     Número de estabelecimentos agropecuários      Unidades                
#> 2 214     Quantidade produzida                          Vide categorias da clas…
#> 3 1982    Quantidade vendida                            Vide categorias da clas…
#> 4 215     Valor da produção                             Mil Reais               
#> 5 1000215 Valor da produção - percentual do total geral %                       
#> 6 216     Área colhida                                  Hectares                
#> 7 1000216 Área colhida - percentual do total geral      %
```

When calling
[`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md),
you can request specific variables by ID:

``` r

# Two specific variables
ibge_variables(1712, variable = c(214, 1982), localities = "BR")
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [120ms]
#> 
#> ✔ 2 records retrieved.
#> # A tibble: 2 × 14
#>   variable_id variable_name  variable_unit classification_218 classification_220
#>   <chr>       <chr>          <chr>         <chr>              <chr>             
#> 1 214         Quantidade pr… Unidades      Total              Total             
#> 2 1982        Quantidade ve… Unidades      Total              Total             
#> # ℹ 9 more variables: classification_226 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>
```

Use `variable = NULL` (default) for all standard variables, or
`variable = "all"` to include API-generated percentage variables (the
`1000215`-style ids above) when available.

### Classifications and categories

Besides being linked to a locality and a period, each observation can be
further broken down by **classifications** (dimensions). Each
classification contains **categories** (members).

For aggregate 1712, the classifications are “product” (226), “producer
condition” (218), “economic activity group”, and so on. Classification
226 has categories like “pineapple” (4844), “garlic” (96608) and
“potato” (96609):

``` r

meta$classifications
#> # A tibble: 6 × 3
#>   id    name                                      categories       
#>   <chr> <chr>                                     <list>           
#> 1 226   Produtos da lavoura temporária            <tibble [53 × 4]>
#> 2 218   Condição do produtor em relação às terras <tibble [7 × 4]> 
#> 3 12517 Grupos de atividade econômica             <tibble [10 × 4]>
#> 4 220   Grupos de área total                      <tibble [19 × 4]>
#> 5 12523 Grupos de área colhida                    <tibble [12 × 4]>
#> 6 12617 Pronafiano                                <tibble [7 × 4]>

# Unnest to see all categories
tidyr::unnest(meta$classifications, categories)
#> # A tibble: 108 × 6
#>    id    name             category_id category_name category_unit category_level
#>    <chr> <chr>            <chr>       <chr>         <chr>         <chr>         
#>  1 226   Produtos da lav… 113869      Total         <NA>          0             
#>  2 226   Produtos da lav… 4844        Abacaxi       Mil frutos    1             
#>  3 226   Produtos da lav… 111671      Abóbora, mor… Toneladas     1             
#>  4 226   Produtos da lav… 111672      Algodão herb… Toneladas     1             
#>  5 226   Produtos da lav… 4847        Alho          Toneladas     1             
#>  6 226   Produtos da lav… 96608       Amendoim em … Toneladas     1             
#>  7 226   Produtos da lav… 4851        Arroz em cas… Toneladas     1             
#>  8 226   Produtos da lav… 111673      Aveia branca… Toneladas     1             
#>  9 226   Produtos da lav… 96609       Batata-ingle… Toneladas     1             
#> 10 226   Produtos da lav… 4857        Cana-de-açúc… Toneladas     1             
#> # ℹ 98 more rows
```

When you don’t specify a classification, the API returns results for the
**Total** category (ID = 0). This is a special aggregate across all
categories.

``` r

# Default: Total category (aggregated across all products)
ibge_variables(1712, variable = 214, localities = "BR")
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [69ms]
#> 
#> ✔ 1 record retrieved.
#> # A tibble: 1 × 14
#>   variable_id variable_name  variable_unit classification_218 classification_220
#>   <chr>       <chr>          <chr>         <chr>              <chr>             
#> 1 214         Quantidade pr… Unidades      Total              Total             
#> # ℹ 9 more variables: classification_226 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# Specific products
ibge_variables(
  1712,
  variable       = 214,
  localities     = "BR",
  classification = list("226" = c(4844, 96608))
)
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [72ms]
#> 
#> ✔ 2 records retrieved.
#> # A tibble: 2 × 14
#>   variable_id variable_name  variable_unit classification_226 classification_218
#>   <chr>       <chr>          <chr>         <chr>              <chr>             
#> 1 214         Quantidade pr… Mil frutos    Abacaxi            Total             
#> 2 214         Quantidade pr… Mil frutos    Amendoim em casca  Total             
#> # ℹ 9 more variables: classification_220 <chr>, classification_12517 <chr>,
#> #   classification_12523 <chr>, classification_12617 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>

# All products
ibge_variables(
  1712,
  variable       = 214,
  localities     = "BR",
  classification = list("226" = "all")
)
#> ✔ Aggregate 1712: 7 variables,
#> 6 classifications, 108 categories (cached).
#> ℹ Fetching variables for aggregate 1712 from IBGE API...
#> ✔ Fetching variables for aggregate 1712 from IBGE API... [72ms]
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

### Geographic levels and localities

IBGE organizes Brazil into a hierarchy of geographic levels. Each
aggregate supports a specific subset of these levels:

| Code   | Level                  | Count  | Example                    |
|--------|------------------------|--------|----------------------------|
| `N1`   | Brazil                 | 1      | BR                         |
| `N2`   | Major region           | 5      | 1 (North), 3 (Southeast)   |
| `N3`   | State (UF)             | 27     | 33 (RJ), 35 (SP)           |
| `N6`   | Municipality           | 5,570  | 3550308 (São Paulo city)   |
| `N7`   | Metropolitan area      | varies | 3501 (RM São Paulo)        |
| `N8`   | Mesoregion             | 137    | 3515 (Metropolitana de SP) |
| `N9`   | Microregion            | 558    | …                          |
| `N10`  | District               | 9,700+ | …                          |
| `N102` | Neighbourhood (bairro) | varies | census aggregates only     |

> **Important**: municipality IDs (N6) and metropolitan area IDs (N7)
> use different numbering. São Paulo city is 3550308 (N6), while the São
> Paulo metropolitan area is 3501 (N7). Don’t confuse them.

The available levels for each aggregate are in the metadata, and
[`ibge_localities()`](https://strategicprojects.github.io/ibger/reference/ibge_localities.md)
lists the localities at a level:

``` r

meta <- ibge_metadata(1092)
#> ℹ Fetching metadata for aggregate 1092 from IBGE API...
#> ✔ Fetching metadata for aggregate 1092 from IBGE API... [154ms]
#> 
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories.
meta$territorial_level
#> $administrative
#> [1] "N1" "N3"
#> 
#> $special
#> character(0)
#> 
#> $ibge
#> character(0)

ibge_localities(1092, level = "N3")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching N3 localities for aggregate 1092 from IBGE API...
#> ✔ Fetching N3 localities for aggregate 1092 from IBGE API... [73ms]
#> 
#> ✔ 27 localities found.
#> # A tibble: 27 × 4
#>    id    name      level_id level_name          
#>    <chr> <chr>     <chr>    <chr>               
#>  1 11    Rondônia  N3       Unidade da Federação
#>  2 12    Acre      N3       Unidade da Federação
#>  3 13    Amazonas  N3       Unidade da Federação
#>  4 14    Roraima   N3       Unidade da Federação
#>  5 15    Pará      N3       Unidade da Federação
#>  6 16    Amapá     N3       Unidade da Federação
#>  7 17    Tocantins N3       Unidade da Federação
#>  8 21    Maranhão  N3       Unidade da Federação
#>  9 22    Piauí     N3       Unidade da Federação
#> 10 23    Ceará     N3       Unidade da Federação
#> # ℹ 17 more rows
```

You can request all localities at a level, or pick specific ones:

``` r

# All states
ibge_variables(1092, variable = 284, periods = -1, localities = "N3")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching N3 localities for aggregate 1092 from IBGE API...
#> ✔ Fetching N3 localities for aggregate 1092 from IBGE API... [75ms]
#> 
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [71ms]
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

# Specific states
ibge_variables(1092, variable = 284, periods = -1,
               localities = list(N3 = c(33, 35)))
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [151ms]
#> 
#> ✔ 2 records retrieved.
#> # A tibble: 2 × 11
#>   variable_id variable_name variable_unit classification_18 classification_12529
#>   <chr>       <chr>         <chr>         <chr>             <chr>               
#> 1 284         Animais abat… Cabeças       Total             Total               
#> 2 284         Animais abat… Cabeças       Total             Total               
#> # ℹ 6 more variables: classification_12716 <chr>, locality_id <chr>,
#> #   locality_name <chr>, locality_level <chr>, period <chr>, value <chr>
```

The API also supports **contextual queries** — filtering municipalities
by their parent state or region. For example, `N6[N3[33,35],N2[1]]`
means “all municipalities in RJ, SP, or the North region”. ibger passes
this through directly (aggregate 512 is the harvested area of permanent
crops in the 1995 Agricultural Census):

``` r

ibge_variables(
  512,
  variable   = 216,
  localities = "N6[N3[33,35],N2[1]]"
)
#> ℹ Fetching metadata for aggregate 512 from IBGE API...
#> ✔ Fetching metadata for aggregate 512 from IBGE API... [76ms]
#> 
#> ✔ Aggregate 512: 2 variables,
#> 2 classifications, 84 categories.
#> ℹ Fetching variables for aggregate 512 from IBGE API...
#> ✔ Fetching variables for aggregate 512 from IBGE API... [78ms]
#> 
#> ✔ 1069 records retrieved.
#> # A tibble: 1,069 × 10
#>    variable_id variable_name variable_unit classification_218 classification_227
#>    <chr>       <chr>         <chr>         <chr>              <chr>             
#>  1 216         Área colhida  Hectares      Total              Total             
#>  2 216         Área colhida  Hectares      Total              Total             
#>  3 216         Área colhida  Hectares      Total              Total             
#>  4 216         Área colhida  Hectares      Total              Total             
#>  5 216         Área colhida  Hectares      Total              Total             
#>  6 216         Área colhida  Hectares      Total              Total             
#>  7 216         Área colhida  Hectares      Total              Total             
#>  8 216         Área colhida  Hectares      Total              Total             
#>  9 216         Área colhida  Hectares      Total              Total             
#> 10 216         Área colhida  Hectares      Total              Total             
#> # ℹ 1,059 more rows
#> # ℹ 5 more variables: locality_id <chr>, locality_name <chr>,
#> #   locality_level <chr>, period <chr>, value <chr>
```

### Periods and periodicities

Each aggregate has a fixed periodicity. The codes used by the API’s
`periodicity` filter (as observed in the catalog) are:

| Code  | Periodicity                     |
|-------|---------------------------------|
| `P1`  | Annual                          |
| `P5`  | Monthly                         |
| `P7`  | Every three years               |
| `P8`  | Semi-annual                     |
| `P9`  | Quarterly                       |
| `P11` | Every two years                 |
| `P13` | Rolling quarter (PNAD Contínua) |
| `P16` | Every six years                 |

Period codes encode both the date and periodicity. The code `202001`
means different things depending on the aggregate’s periodicity:

- Monthly (`P5`): January 2020
- Quarterly (`P9`): Q1 2020
- Semi-annual (`P8`): first half of 2020

The metadata tells you the frequency and the valid range:

``` r

meta <- ibge_metadata(7060)
#> ℹ Fetching metadata for aggregate 7060 from IBGE API...
#> ✔ Fetching metadata for aggregate 7060 from IBGE API... [79ms]
#> 
#> ✔ Aggregate 7060: 4 variables,
#> 1 classification, 457 categories.
meta$periodicity
#> $frequency
#> [1] "mensal"
#> 
#> $start
#> [1] 202001
#> 
#> $end
#> [1] 202608
```

ibger’s
[`ibge_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_periods.md)
lists every individual period:

``` r

ibge_periods(7060)
#> ℹ Fetching periods for aggregate 7060 from IBGE API...
#> ✔ Fetching periods for aggregate 7060 from IBGE API... [70ms]
#> 
#> ✔ 80 periods found.
#> # A tibble: 80 × 3
#>    id     literal                            modification
#>    <chr>  <chr>                              <chr>       
#>  1 202001 janeiro 2020 / janeiro de 2020     07/02/2020  
#>  2 202002 fevereiro 2020 / fevereiro de 2020 11/03/2020  
#>  3 202003 março 2020 / março de 2020         09/04/2020  
#>  4 202004 abril 2020 / abril de 2020         08/05/2020  
#>  5 202005 maio 2020 / maio de 2020           10/06/2020  
#>  6 202006 junho 2020 / junho de 2020         10/07/2020  
#>  7 202007 julho 2020 / julho de 2020         07/08/2020  
#>  8 202008 agosto 2020 / agosto de 2020       09/09/2020  
#>  9 202009 setembro 2020 / setembro de 2020   09/10/2020  
#> 10 202010 outubro 2020 / outubro de 2020     06/11/2020  
#> # ℹ 70 more rows
```

## Request limits

The API rejects requests whose result is too large, answering HTTP 500.
The documented limit is **100,000 values** per request, computed as:

> **variables × categories × periods × localities ≤ 100,000**

In practice requests start failing above roughly 50,000 values, which is
the limit ibger uses. For example, a request for aggregate 2654 (deaths
by month, nature, sex and age) with:

- Classification 244: 1 category
- Classification 1836: 2 categories
- Classification 2: 2 categories
- Classification 260: 1 category
- 6 periods (default)
- 4 municipalities

produces 1 × 2 × 2 × 1 × 6 × 4 = **96 values** — well within the limit.

You do not need to split large requests yourself.
[`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
estimates the size of the result from the metadata (fetching the period
and locality lists when needed) and, when the estimate exceeds the
limit, splits the query — first by periods, then by localities — and
binds the pieces back together. The `chunk` argument controls this:
`TRUE` (the default) uses the built-in limit, `FALSE` forces a single
request, and a number sets a custom per-request limit. Lowering the
limit is a quick way to see the mechanism at work on a small query (27
states × 12 years = 324 values):

``` r

ibge_variables(6579, localities = "N3", periods = -12, chunk = 100)
#> ℹ Fetching metadata for aggregate 6579 from IBGE API...
#> ✔ Fetching metadata for aggregate 6579 from IBGE API... [69ms]
#> 
#> ✔ Aggregate 6579: 1 variable,
#> 0 classifications, 0 categories.
#> ℹ Fetching N3 localities for aggregate 6579 from IBGE API...
#> ✔ Fetching N3 localities for aggregate 6579 from IBGE API... [101ms]
#> 
#> ℹ Fetching period list for aggregate 6579 from IBGE API...
#> ✔ Fetching period list for aggregate 6579 from IBGE API... [80ms]
#> 
#> ℹ Estimated result exceeds the API limit (100 values);
#> splitting into 4 requests.
#> ℹ Fetching chunk 1/4 for aggregate 6579 from IBGE API...
#> ✔ Fetching chunk 1/4 for aggregate 6579 from IBGE API... [198ms]
#> 
#> ℹ Fetching chunk 2/4 for aggregate 6579 from IBGE API...
#> ✔ Fetching chunk 2/4 for aggregate 6579 from IBGE API... [208ms]
#> 
#> ℹ Fetching chunk 3/4 for aggregate 6579 from IBGE API...
#> ✔ Fetching chunk 3/4 for aggregate 6579 from IBGE API... [126ms]
#> 
#> ℹ Fetching chunk 4/4 for aggregate 6579 from IBGE API...
#> ✔ Fetching chunk 4/4 for aggregate 6579 from IBGE API... [122ms]
#> 
#> ✔ 324 records retrieved.
#> # A tibble: 324 × 8
#>    variable_id variable_name             variable_unit locality_id locality_name
#>    <chr>       <chr>                     <chr>         <chr>       <chr>        
#>  1 9324        População residente esti… Pessoas       11          Rondônia     
#>  2 9324        População residente esti… Pessoas       11          Rondônia     
#>  3 9324        População residente esti… Pessoas       11          Rondônia     
#>  4 9324        População residente esti… Pessoas       12          Acre         
#>  5 9324        População residente esti… Pessoas       12          Acre         
#>  6 9324        População residente esti… Pessoas       12          Acre         
#>  7 9324        População residente esti… Pessoas       13          Amazonas     
#>  8 9324        População residente esti… Pessoas       13          Amazonas     
#>  9 9324        População residente esti… Pessoas       13          Amazonas     
#> 10 9324        População residente esti… Pessoas       14          Roraima      
#> # ℹ 314 more rows
#> # ℹ 3 more variables: locality_level <chr>, period <chr>, value <chr>
```

The getting-started vignette shows a real case (all municipalities, 12
years) that is split automatically.

## View modes

The API supports three view modes for the response format. ibger uses
the default JSON mode, but you can also pass `view = "OLAP"` or
`view = "flat"`:

``` r

# OLAP notation
ibge_variables(1092, variable = 284, periods = -1, localities = "BR",
               view = "OLAP")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [201ms]
#> 
#> ✔ 1 record retrieved.
#> # A tibble: 1 × 8
#>   variable_id variable_name variable_unit locality_id locality_name
#>   <chr>       <chr>         <chr>         <chr>       <chr>        
#> 1 284         <NA>          Cabeças       1           Brasil       
#> # ℹ 3 more variables: locality_level <chr>, period <chr>, value <chr>

# Flat mode (first element is metadata, data starts at second)
ibge_variables(1092, variable = 284, periods = -1, localities = "BR",
               view = "flat")
#> ✔ Aggregate 1092: 6 variables,
#> 3 classifications, 14 categories (cached).
#> ℹ Fetching variables for aggregate 1092 from IBGE API...
#> ✔ Fetching variables for aggregate 1092 from IBGE API... [101ms]
#> 
#> ✔ 1 record retrieved.
#> # A tibble: 1 × 17
#>   `Nível Territorial (Código)` `Nível Territorial` `Unidade de Medida (Código)`
#>   <chr>                        <chr>               <chr>                       
#> 1 1                            Brasil              24                          
#> # ℹ 14 more variables: `Unidade de Medida` <chr>, Valor <chr>,
#> #   `Brasil (Código)` <chr>, Brasil <chr>, `Trimestre (Código)` <chr>,
#> #   Trimestre <chr>, `Variável (Código)` <chr>, Variável <chr>,
#> #   `Tipo de rebanho bovino (Código)` <chr>, `Tipo de rebanho bovino` <chr>,
#> #   `Tipo de inspeção (Código)` <chr>, `Tipo de inspeção` <chr>,
#> #   `Referência temporal (Código)` <chr>, `Referência temporal` <chr>
```

In most cases, the default mode with ibger’s tidy output is the most
convenient.

## How ibger maps to the API

Here is a quick reference showing how ibger functions correspond to API
endpoints:

| ibger function | API endpoint |
|----|----|
| [`ibge_aggregates()`](https://strategicprojects.github.io/ibger/reference/ibge_aggregates.md) | `GET /agregados` |
| [`ibge_metadata()`](https://strategicprojects.github.io/ibger/reference/ibge_metadata.md) | `GET /agregados/{id}/metadados` |
| [`ibge_periods()`](https://strategicprojects.github.io/ibger/reference/ibge_periods.md) | `GET /agregados/{id}/periodos` |
| [`ibge_localities()`](https://strategicprojects.github.io/ibger/reference/ibge_localities.md) | `GET /agregados/{id}/localidades/{nivel}` |
| [`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md) | `GET /agregados/{id}/periodos/{p}/variaveis/{v}` |

The ibger parameters map to URL path segments and query parameters:

| ibger parameter | API parameter | Format |
|----|----|----|
| `aggregate` | `{agregado}` (path) | Numeric ID |
| `variable` | `{variavel}` (path) | `214\|1982` or `all` or `allxp` |
| `periods` | `{periodos}` (path) | `-6` or `201701-201706` or `201701\|201702` |
| `localities` | `localidades` (query) | `BR` or `N3` or `N6[3550308,3304557]` |
| `classification` | `classificacao` (query) | `226[4844,96608]\|218[4780]` |
| `view` | `view` (query) | `OLAP` or `flat` |

If you already have a SIDRA API URL (from the SIDRA query builder or the
sidrar package),
[`parse_sidra_url()`](https://strategicprojects.github.io/ibger/reference/parse_sidra_url.md)
translates it into the equivalent
[`ibge_variables()`](https://strategicprojects.github.io/ibger/reference/ibge_variables.md)
call and
[`fetch_sidra_url()`](https://strategicprojects.github.io/ibger/reference/fetch_sidra_url.md)
runs it.

## Further reading

- [IBGE API
  documentation](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3)
- [SIDRA portal](https://sidra.ibge.gov.br/)
- [IBGE Query
  Builder](https://servicodados.ibge.gov.br/api/docs/agregados?versao=3#api-bq)
  — useful for exploring tables before writing R code

------------------------------------------------------------------------

This vignette was last built on 2026-09-22 against the live IBGE API.
