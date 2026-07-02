DOI <- "10.1257/aer.91.5.1369"
STUDY_REPO <- "replicate-anything/rep-10.1257-aer.91.5.1369"

local_index <- utils::read.csv(
  file.path(
    normalizePath(file.path("..", "..", "..", "registry"), mustWork = FALSE),
    "index.csv"
  ),
  stringsAsFactors = FALSE
)

options(
  replicateEverything.registry_root = normalizePath(
    file.path("..", "..", "..", "registry"),
    mustWork = FALSE
  ),
  replicateEverything.index = local_index,
  replicateEverything.use_sibling_packages = TRUE,
  replicateEverything.study_folders_root = normalizePath(
    file.path("..", "..", ".."),
    mustWork = FALSE
  )
)

test_that("tab_2 R replication runs", {
  result <- replicateEverything::render_replication(DOI, "tab_2")
  testthat::expect_s3_class(result, "replication_result")
  obj <- replicateEverything::replication_object(result)
  testthat::expect_type(obj, "list")
})

test_that("replication.yml lists dual engines per table", {
  meta <- yaml::read_yaml(file.path("..", "..", "replication.yml"))
  ids <- vapply(meta$replications, function(x) x$id, character(1))
  testthat::expect_true(all(paste0("tab_", 1:8) %in% ids))
  testthat::expect_true(all(paste0("tab_", 1:8, "_stata") %in% ids))
})
