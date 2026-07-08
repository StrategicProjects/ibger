# Helpers ---------------------------------------------------------------

fake_meta <- function(n_vars = 8, cats_226 = 34) {
  list(
    variables = tibble::tibble(
      id   = as.character(seq_len(n_vars)),
      name = paste("var", seq_len(n_vars)),
      unit = rep("un", n_vars)
    ),
    classifications = tibble::tibble(
      id   = "226",
      name = "Produtos",
      categories = list(tibble::tibble(
        category_id   = as.character(seq_len(cats_226)),
        category_name = paste("cat", seq_len(cats_226)),
        category_unit = NA_character_,
        category_level = NA_character_
      ))
    )
  )
}

seed_cache <- function(key, value) {
  assign(key, value, envir = .ibger_cache)
}

clear_cache_keys <- function(...) {
  keys <- c(...)
  keys <- keys[vapply(keys, exists, logical(1), envir = .ibger_cache)]
  if (length(keys) > 0) rm(list = keys, envir = .ibger_cache)
}

# resolve_chunk_limit ----------------------------------------------------

test_that("resolve_chunk_limit handles TRUE, FALSE and numbers", {
  expect_equal(resolve_chunk_limit(TRUE), 50000)
  expect_null(resolve_chunk_limit(FALSE))
  expect_equal(resolve_chunk_limit(5000), 5000)
  expect_error(resolve_chunk_limit("yes"))
  expect_error(resolve_chunk_limit(-1))
  expect_error(resolve_chunk_limit(NA))
})

# split_in_groups --------------------------------------------------------

test_that("split_in_groups splits into consecutive groups", {
  groups <- split_in_groups(1:10, 3)
  expect_length(groups, 4)
  expect_equal(groups[[1]], 1:3)
  expect_equal(groups[[4]], 10L)
  expect_equal(unlist(groups), 1:10)

  expect_length(split_in_groups(1:3, 10), 1)
})

# units_to_localities_str ------------------------------------------------

test_that("units_to_localities_str rebuilds the API syntax", {
  units <- tibble::tibble(
    level = c("N3", "N3", "N6"),
    id    = c("33", "35", "3550308")
  )
  expect_equal(units_to_localities_str(units), "N3[33,35]|N6[3550308]")
})

# estimate_n_periods -----------------------------------------------------

test_that("estimate_n_periods handles all accepted forms", {
  expect_equal(estimate_n_periods(NULL), 6)
  expect_equal(estimate_n_periods(-4), 4)
  expect_equal(estimate_n_periods("-4"), 4)
  expect_equal(estimate_n_periods(c(2020, 2021)), 2)
  expect_equal(estimate_n_periods("201701-201712"), 12)
})

# estimate_n_variables ---------------------------------------------------

test_that("estimate_n_variables uses metadata only when needed", {
  get_meta <- function() fake_meta(n_vars = 8)

  expect_equal(estimate_n_variables(get_meta, NULL), 8)
  expect_equal(estimate_n_variables(get_meta, "all"), 16)
  expect_equal(estimate_n_variables(get_meta, c(214, 215, 216)), 3)

  # Explicit variables must not touch metadata
  no_meta <- function() stop("metadata should not be fetched")
  expect_equal(estimate_n_variables(no_meta, c(214, 215)), 2)
})

# estimate_n_categories --------------------------------------------------

test_that("estimate_n_categories multiplies category counts", {
  get_meta <- function() fake_meta(cats_226 = 34)

  expect_equal(estimate_n_categories(get_meta, NULL), 1)
  expect_equal(estimate_n_categories(get_meta, list("226" = c(1, 2, 3))), 3)
  expect_equal(estimate_n_categories(get_meta, list("226" = "all")), 34)
  expect_equal(
    estimate_n_categories(get_meta, list("226" = c(1, 2), "218" = c(1, 2, 3))),
    6
  )
})

# resolve_localities_for_chunking ----------------------------------------

test_that("resolve_localities_for_chunking counts without the API when possible", {
  res <- resolve_localities_for_chunking(9999, "BR")
  expect_equal(res$n, 1)
  expect_null(res$units)

  res <- resolve_localities_for_chunking(9999, list(N3 = c(33, 35), N6 = 3550308))
  expect_equal(res$n, 3)
  expect_equal(res$units$level, c("N3", "N3", "N6"))
  expect_equal(res$units$id, c("33", "35", "3550308"))
})

test_that("resolve_localities_for_chunking uses the cached level ids", {
  seed_cache("locality_ids_9999_N3", as.character(1:27))
  on.exit(clear_cache_keys("locality_ids_9999_N3"))

  res <- resolve_localities_for_chunking(9999, "N3")
  expect_equal(res$n, 27)
  expect_equal(res$units$level, rep("N3", 27))
})

# build_chunk_plan -------------------------------------------------------

test_that("build_chunk_plan returns NULL for small queries", {
  plan <- build_chunk_plan(
    aggregate      = 9999,
    meta           = NULL,
    variable       = 214,
    periods        = c(2020, 2021),
    localities     = list(N3 = "33"),
    classification = NULL,
    limit          = 100000
  )
  expect_null(plan)
})

test_that("build_chunk_plan splits by periods first", {
  seed_cache("period_ids_9999", as.character(2000:2019))
  on.exit(clear_cache_keys("period_ids_9999"))

  # 2 variables x 25 localities = 50 values per period; limit 300 -> 6
  # periods per chunk -> 4 chunks for 20 periods
  plan <- build_chunk_plan(
    aggregate      = 9999,
    meta           = NULL,
    variable       = c(1, 2),
    periods        = -20,
    localities     = list(N3 = as.character(1:25)),
    classification = NULL,
    limit          = 300
  )

  expect_length(plan, 4)
  expect_equal(plan[[1]]$periods_str, paste(2000:2005, collapse = "|"))
  expect_equal(plan[[4]]$periods_str, "2018|2019")
  expect_equal(plan[[1]]$localities_str, paste0("N3[", paste(1:25, collapse = ","), "]"))
})

test_that("build_chunk_plan splits localities when one period is too large", {
  seed_cache("period_ids_9999", as.character(2000:2019))
  on.exit(clear_cache_keys("period_ids_9999"))

  # 2 variables x 25 localities = 50 values per period; limit 40 -> split
  # localities in groups of 20 -> 2 groups x 20 periods = 40 chunks
  plan <- build_chunk_plan(
    aggregate      = 9999,
    meta           = NULL,
    variable       = c(1, 2),
    periods        = -20,
    localities     = list(N3 = as.character(1:25)),
    classification = NULL,
    limit          = 40
  )

  expect_length(plan, 40)
  expect_equal(plan[[1]]$periods_str, "2000")
  expect_equal(plan[[1]]$localities_str, paste0("N3[", paste(1:20, collapse = ","), "]"))
  expect_equal(plan[[2]]$periods_str, "2000")
  expect_equal(plan[[2]]$localities_str, paste0("N3[", paste(21:25, collapse = ","), "]"))
})

test_that("resolve_period_ids resolves last-N and explicit periods", {
  seed_cache("period_ids_9999", as.character(2000:2019))
  on.exit(clear_cache_keys("period_ids_9999"))

  expect_equal(resolve_period_ids(9999, -3), as.character(2017:2019))
  expect_equal(resolve_period_ids(9999, c(2005, 2007)), c("2005", "2007"))
  expect_equal(resolve_period_ids(9999, "2005-2007"), as.character(2005:2007))
})
