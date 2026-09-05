test_that("published notebooks are self-contained tidyverse workflows", {
  notebook_paths <- c(
    here::here("index.qmd"),
    here::here("es", "index.qmd")
  )
  forbidden_calls <- paste(
    c(
      "source\\(", "R/analysis\\.R", "function\\s*\\(", "\\\\\\(",
      "read_bike_snapshot\\(", "fit_bike_models\\(",
      "bike_data_profile\\(", "prepare_modeling_data\\(",
      "temporal_partition\\(", "plot_[a-z_]+\\("
    ),
    collapse = "|"
  )

  for (path in notebook_paths) {
    notebook <- readr::read_file(path)
    expect_match(notebook, "library\\(tidyverse\\)")
    expect_match(notebook, "library\\(tidymodels\\)")
    expect_false(grepl(forbidden_calls, notebook))
    expect_match(notebook, "#\\| results: hide")
  }
})
