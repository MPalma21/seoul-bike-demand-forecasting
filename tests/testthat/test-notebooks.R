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

    chunks <- stringr::str_extract_all(
      notebook,
      stringr::regex("```\\{r\\}.*?```", dotall = TRUE)
    )[[1]]
    expect_lte(max(stringr::str_count(chunks, "\\n")), 26)
    expect_match(notebook, "label: .*linear")
    expect_match(notebook, "label: .*elastic")
    expect_match(notebook, "label: .*(forest|bosque)")
  }
})
