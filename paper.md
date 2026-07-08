---
title: 'ibger: A tidyverse-friendly R client for the Brazilian Institute of Geography and Statistics (IBGE) Aggregates API'
tags:
  - R
  - official statistics
  - Brazil
  - IBGE
  - SIDRA
  - open data
  - API client
authors:
  - name: André Leite
    orcid: 0000-0002-4718-9766
    corresponding: true
    affiliation: 1
  - name: Marcos Wasiliew
    affiliation: 1
  - name: Hugo Vasconcelos
    affiliation: 1
  - name: Carlos Amorim
    affiliation: 1
  - name: Diogo Bezerra
    affiliation: 1
affiliations:
  - name: Universidade Federal de Pernambuco, Recife, Brazil
    index: 1
date: 8 July 2026
bibliography: paper.bib
---

# Summary

The Brazilian Institute of Geography and Statistics (IBGE) is the agency
responsible for Brazil's official statistics, conducting the national
census and dozens of recurring demographic, economic, social and
agricultural surveys. Their results are disseminated as *aggregate
tables* through SIDRA (*Sistema IBGE de Recuperação Automática*) and
exposed programmatically by the IBGE Aggregates API [@ibgeapi].

`ibger` is an R [@rcore] package that provides a complete,
tidyverse-friendly [@tidyverse] interface to that API. Users can
discover tables (`ibge_aggregates()`), inspect their variables,
classifications and categories (`ibge_metadata()`), list available
periods and localities, and retrieve data (`ibge_variables()`) for any
combination of variables, periods, geographic levels — from the whole
country down to its 5,570 municipalities — and classification
categories. Every function returns a tidy tibble in long format, ready
for analysis with standard tidyverse workflows. The package also covers
the IBGE Metadata API (survey catalog and methodological documentation)
and ships an interactive Shiny explorer for browsing the table catalog.

Three design features distinguish `ibger`. First, queries are
**validated before any request is sent**: parameters are checked against
cached table metadata, and invalid variables, periods, localities or
categories produce immediate, informative errors listing the allowed
values. Second, results are **transparently chunked**: the Aggregates
API rejects large requests (empirically, above roughly 50,000 values),
so `ibger` estimates the result size in advance and automatically splits
oversized queries into multiple requests — by periods, then by
localities — combining them into a single tibble. Third,
`parse_sidra_url()` and `fetch_sidra_url()` **translate legacy SIDRA API
URLs** (from the SIDRA Query Builder or existing `sidrar` scripts) into
equivalent `ibger` calls, easing migration and cross-checking.

A Python sibling package, `ibgepy` [@ibgepy], mirrors the same public
API, so workflows translate directly between the two languages.

# Statement of need

Brazilian official statistics underpin research in economics,
demography, public health, agriculture and public policy, and IBGE data
are the standard source for indicators such as consumer price inflation
(IPCA), municipal GDP and census population counts. Reproducible
research requires scripted, validated access to these series rather
than manual downloads from the SIDRA website.

The main existing R client, `sidrar` [@sidrar], accesses the legacy
SIDRA API and has served the community well, but it inherits that API's
constraints: a 20,000-value limit per request with no built-in
work-around, metadata discovery via HTML scraping, no pre-flight
validation, and positional string-based parameters. Packages such as
`PNADcIBGE` [@pnadcibge] target survey *microdata*, a different data
product. `ibger` fills the remaining gap: a modern client for the
structured Aggregates API v3 with JSON metadata endpoints, tidy
outputs, argument validation, in-memory caching, robust HTTP handling
via `httr2` [@httr2], and automatic chunking that removes the practical
size ceiling for large extractions (e.g., time series for all
municipalities in a single call).

The package is aimed at researchers, statistical agencies, journalists
and analysts who need reliable, reproducible pipelines over Brazilian
official statistics. It is available on CRAN, documented with four
vignettes and a pkgdown site, and tested against recorded API fixtures
so its behaviour is verified offline.

# Acknowledgements

We thank Daniel Vartanian for early feedback that motivated the
automatic chunking feature, and the IBGE for maintaining the public
APIs on which this package relies.

# References
