test_that("ibge_localities keeps its columns when a level is empty", {
  cleanup <- seed_fake_meta(1437, make_fake_meta(id = 1437,
                                                 levels = c("N1", "N6", "N7")))
  withr::defer(cleanup())

  local_mocked_bindings(ibge_request = function(...) list())
  expect_message(ibge_localities(1437, level = c("N6", "N7")),
                 "No localities found")
  res <- suppressMessages(ibge_localities(1437, level = c("N6", "N7")))
  expect_named(res, c("id", "name", "level_id", "level_name"))
  expect_identical(nrow(res), 0L)
})

test_that("ibge_localities queries each level separately and binds rows", {
  cleanup <- seed_fake_meta(1437, make_fake_meta(id = 1437,
                                                 levels = c("N1", "N6", "N7")))
  withr::defer(cleanup())

  seen <- new.env(parent = emptyenv())
  seen$calls <- character()
  local_mocked_bindings(ibge_request = function(..., .label = "") {
    parts <- c(...)
    lvl <- parts[length(parts)]
    seen$calls <- c(seen$calls, lvl)
    if (lvl == "N7") return(list())
    list(list(id = "1", nome = "Brasil", nivel = list(id = lvl, nome = "x")))
  })

  res <- ibge_localities(1437, level = c("N6", "N7"))
  expect_identical(seen$calls, c("N6", "N7"))
  expect_identical(nrow(res), 1L)
  expect_identical(res$level_id, "N6")
})
