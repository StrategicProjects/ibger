# Design History and Architecture

This note documents how `ibger` came to have its current structure and the
design decisions behind it. It is written for rOpenSci review and future
maintenance.

## Design History

`ibger` grew out of applied data work at the Secretaria de Projetos
Estratégicos (Strategic Projects Office) of the Pernambuco state government,
in collaboration with the Universidade Federal de Pernambuco (UFPE). Our
teams routinely build indicators from Brazilian official statistics —
census tables, price indices, agricultural and labor surveys — all of which
IBGE disseminates as *aggregate tables* through SIDRA and exposes
programmatically via the IBGE Aggregates API v3. The existing R client,
`sidrar`, targets the legacy SIDRA API: a different endpoint family with
positional string parameters, no programmatic access to the v3 metadata
endpoints, and a hard value cap that it leaves to the user to work around.
Repeated in-house scripting against the v3 API — building URLs by hand,
decoding the nested JSON, rediscovering the same undocumented failure
modes — is what motivated a proper package.

Development proceeded in three stages:

**Initial version (early 2026, `ibger` 0.1.0 on CRAN).** The first public
version already contained the core of the current design: the discovery
functions (`ibge_aggregates()`, `ibge_metadata()`, `ibge_periods()`,
`ibge_localities()`), the main data-retrieval function (`ibge_variables()`)
returning tidy long-format tibbles, pre-flight validation of query
parameters against cached metadata, the session-level metadata cache, the
`parse_ibge_value()` helper for IBGE's special value codes, and the SIDRA
URL translator (`parse_sidra_url()` / `fetch_sidra_url()`) for migrating
existing SIDRA/`sidrar` workflows. This version was developed in-house —
the ideas had been iterated in internal analysis scripts before the package
existed — and was pushed to GitHub essentially complete when it was being
prepared for CRAN submission. That is why the public git history begins
with a single large "cran ready" commit rather than a long series of
incremental ones: the incremental history happened in scripts and local
iterations that predate the repository. Shortly after the initial release
we added the Metadata API v2 client (survey catalog and methodological
metadata), the Shiny catalog explorer, and a README comparison with
`sidrar` prompted by early user questions.

**Automatic chunking (July 2026, `ibger` 0.2.0).** The first external
feature request (repo issue #1, @danielvartan) asked how to retrieve
tables whose result size exceeds the API's per-request value limit. We had
been splitting such queries by hand internally; version 0.2.0 moved that
logic into the package. `ibge_variables()` now estimates the result size
of a query before sending it and, when the estimate exceeds the limit,
transparently splits the query — by periods first, then by localities —
and binds the pieces into a single tibble. Determining the real limit
required empirical testing: the API documents 100,000 values per request,
but requests reliably fail with HTTP 500 well below that (a 44,560-value
request succeeds; 50,130 fails), so the default limit is 50,000 and the
observations are recorded in `R/chunking.R`.

**rOpenSci preparation (July 2026).** In preparation for this review the
test suite was expanded from chunking-only unit tests to ~87% coverage of
all exported functions, using `httptest2` recorded fixtures so the suite
runs offline; CI (R CMD check on three platforms, coverage upload to
Codecov) and community files (`CONTRIBUTING.md`, `CODE_OF_CONDUCT.md`,
`codemeta.json`) were added; and the JOSS paper was drafted. None of this
changed the public API.

As disclosed in the submission, recent development (tests, documentation,
paper drafting) was assisted by an LLM coding tool (Claude Code), with all
generated content reviewed, executed and validated by the authors. The
architecture and API design described below are human decisions that
predate that assistance.

## Architecture Overview

`ibger` talks to two IBGE web services and understands the URL scheme of a
third:

| Service | Base URL | Used by |
| --- | --- | --- |
| Aggregates API v3 | `servicodados.ibge.gov.br/api/v3/agregados` | All data/metadata functions (`ibge_request()` in `R/utils.R`) |
| Metadata API v2 | `servicodados.ibge.gov.br/api/v2/metadados` | Survey catalog only (`ibge_metadados_request()` in `R/surveys.R`) |
| SIDRA API | `apisidra.ibge.gov.br/values/...` | Never called — `parse_sidra_url()` translates these URLs into equivalent Aggregates-API queries |

The exported API is small and task-oriented: 14 functions covering
discovery (which tables exist, what is in them), retrieval (get the data as
a tidy tibble), migration (translate a SIDRA URL) and interactive browsing
(the Shiny explorer). Most internal code exists to make `ibge_variables()`
reliable against an API with an undocumented failure envelope, and to turn
deeply nested JSON into flat tibbles.

## Component Map

| Component | File | Responsibility |
| --- | --- | --- |
| HTTP core + formatters | `R/utils.R` | `ibge_request()` (httr2 request, retries, error hints), query-string formatters for periods/localities/classifications, `parse_ibge_value()` |
| Table catalog | `R/agregados.R` | `ibge_aggregates()` — list tables, optionally by survey/subject/periodicity/level |
| Metadata | `R/metadados.R` | `ibge_metadata()` — variables, classifications, categories of one table; pretty `print()` method |
| Periods / localities | `R/periodos.R`, `R/localidades.R` | Available periods and localities for a table |
| Data retrieval | `R/variaveis.R` | `ibge_variables()` — the main function; parses both default and flat API views into long tibbles |
| Query chunking | `R/chunking.R` | Result-size estimation and automatic splitting of oversized queries |
| Validation + cache | `R/validacao.R` | `validate_query()` and helpers; the session cache; `ibge_clear_cache()` |
| Survey catalog | `R/surveys.R` | Metadata API v2 client: `ibge_surveys()`, `ibge_survey_periods()`, `ibge_survey_metadata()` |
| SIDRA migration | `R/sidra_url.R` | `parse_sidra_url()`, `fetch_sidra_url()`, `print.sidra_query()` |
| Subjects lookup | `R/subjects.R` | `ibge_subjects()` — static subject (assunto) table |
| Shiny explorer | `R/shiny.R`, `inst/shiny/explorer/` | `ibge_explorer()` catalog browser |

## Retrieval Flow

A call to `ibge_variables()` proceeds in five steps:

1. **Normalize arguments.** Periods, localities and classifications each
   accept several convenient forms (a string like `"BR"`, a level code like
   `"N3"`, a named list like `list(N6 = c(...))`, a negative integer for
   the last *n* periods). Formatters in `R/utils.R` normalize all of them
   into the API's query syntax.
2. **Validate (optional, default on).** The table's metadata is fetched
   once per session and cached; requested variables, levels, localities,
   periods and classification categories are checked against it. Invalid
   input aborts *before* any data request, with a `cli` error listing the
   allowed values.
3. **Estimate and chunk.** `R/chunking.R` estimates the result size
   (variables x periods x localities x classification categories). If it
   exceeds the per-request limit, the query is split into a plan of
   smaller requests — by periods first, then by localities, with a
   separate cap on locality ids per request to keep URLs at a safe length.
4. **Request.** Each request goes through `ibge_request()`: `httr2` with
   up to 3 retries and a custom error handler that, on HTTP 500, hints at
   the API's value limit (the API's own 500 responses carry no useful
   message).
5. **Parse and bind.** The nested response (or the flat view, when
   requested) is flattened into a long tibble — one row per
   variable x classification-category x locality x period — and chunked
   results are row-bound in order.

## Main Design Decisions

### English public API over a Portuguese web API

All exported functions, arguments and outputs are in English, while
internal helpers and HTTP query parameters keep the API's Portuguese
vocabulary (`periodo`, `localidades`, `classificacao`, `nivelTerritorial`).
This makes the package usable by non-Portuguese speakers without hiding
the correspondence with IBGE's own documentation — internal names match
the API docs exactly, which matters when debugging against them.

### Tidy long output; values stay character

Every fetch function returns a tibble, and `ibge_variables()` returns long
format with stable columns (`variable_id/name/unit`, one column per
classification, `locality_id/name/level`, `period`, `value`). The `value`
column is deliberately *character*: IBGE encodes missingness semantics in
special codes (`-`, `..`, `...`, `X` — zero, not applicable, not available,
suppressed) that a silent numeric coercion would destroy. The exported
`parse_ibge_value()` performs the coercion explicitly, letting users decide
how to treat each code.

### Fail fast: pre-flight validation against cached metadata

The API responds to invalid parameters with generic errors (or silently
empty results). Instead of relaying those, `ibger` validates every query
against the table's metadata before sending it, and error messages list
the valid options (e.g. "level N3 not available; available levels: N1, N6,
N7"). Validation is on by default and can be disabled (`validate = FALSE`)
for users who know their query and want to skip the metadata round-trip.

### Session-only in-memory caching

Metadata, period lists, locality lists and the survey catalog are cached
in package-level environments for the duration of the R session, keyed by
request. This makes validation and chunking essentially free after the
first call on a table, without creating hidden state on disk; caches are
explicit (`ibge_clear_cache()`) and die with the session. Users who need
durable data should save the returned tibbles.

### Automatic chunking with an empirically determined limit

The API's per-request value cap is its main retrieval constraint, and its
documented value (100,000) does not match observed behavior (failures
above ~50,000). Rather than exposing the cap as a user problem — the
approach of the legacy tooling — `ibge_variables()` estimates result sizes
and splits oversized queries transparently. Splitting is by periods first
(cheap, preserves locality sets per request), then by localities. The
empirical measurements justifying the 50,000 default are documented in
code comments in `R/chunking.R` so they can be revisited if the API
changes. `chunk = FALSE` disables the behavior; a number sets a custom
limit.

### A migration path from SIDRA URLs

SIDRA's web Query Builder and the `sidrar` package both produce
`apisidra.ibge.gov.br/values/...` URLs, and much existing Brazilian
analysis code is written in those terms. `parse_sidra_url()` parses such a
URL into a structured query object (with a readable `print()` method
showing the equivalent `ibge_variables()` call) and `fetch_sidra_url()`
executes it — so existing workflows can move to `ibger` without being
rewritten from scratch.

### Offline, fixture-based testing

All API interactions are tested against `httptest2` recorded fixtures
(`tests/testthat/api/`), so the suite needs no network and is insensitive
to IBGE server availability. `httptest2` lives in `Suggests` and the API
test file skips when it is unavailable, following the recommended pattern.
Pure logic (formatters, parsers, chunk planning, validation) is tested
directly. Fixtures are re-recorded with `httptest2::capture_requests()`
when the API's responses change.

## Why the Internal Function Graph Is Larger than the Exported API

Static checks see 14 exported functions but ~45 internal ones and a call
network in the upper percentiles for CRAN. That shape is deliberate; the
internal surface supports four needs:

- **Argument flexibility** requires one small formatter per input form
  (period specs, locality specs, classification specs, variables).
- **Validation** is one focused helper per parameter family, each able to
  produce a specific, listing error message.
- **Chunking** needs its own estimators (per dimension), a plan builder,
  splitters and cached id lookups.
- **Parsing** the API's two response layouts (nested default and flat
  view) plus the Metadata API's document tree is done with narrow, named
  helpers rather than one large function, so each quirk can be tested and
  fixed in isolation.

The same preference explains the handful of functions with high
cyclomatic complexity flagged by `pkgcheck`: they are concentrated in the
SIDRA URL parser and the `print()` methods, which by nature enumerate many
optional fields, not in the retrieval path.

## Relationship to the Python Sibling (`ibgepy`)

A Python port, [`ibgepy`](https://github.com/StrategicProjects/ibgepy)
(on PyPI), mirrors this package's public API name-for-name
(`ibge_variables`, `ibge_metadata`, ...). This constrains API evolution
deliberately: changes to the R public surface are weighed against keeping
the two clients in sync, so that documentation, examples and user
knowledge transfer across languages.

## Maintenance Notes and Known Tradeoffs

- **The value limit is empirical.** If IBGE fixes or changes the server
  cap, only `ibge_value_limit` in `R/chunking.R` needs updating.
- **Result-size estimation is an upper bound**, computed from metadata
  before the request; actual results can be smaller (sparse tables). The
  cost of overestimating is extra (smaller) requests, never a failure.
- **The Shiny explorer is optional.** All its dependencies (`shiny`, `DT`,
  `bslib`, `bsicons`) are in `Suggests`; the package's data functionality
  never touches them.
- **Two caches, one clearer.** The aggregates/metadata cache and the
  survey cache are separate environments (they serve different APIs), but
  `ibge_clear_cache()` clears both — users should not need to know the
  internal split.
