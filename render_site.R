dir(pattern = "index.+Rmd$") |> 
  c(dir(pattern = "etl.+Rmd$")) |> 
  c(dir(pattern = "ques.+1.+Rmd$")) |> 
  c(dir(pattern = "ques.+2.+Rmd$")) |> 
  # magrittr::extract(1) |> 
  purrr::walk(\(x){ 
    rmarkdown::render(
      input = x
      , output_format = "html_notebook"
      , output_dir = "docs"
      , knit_root_dir = "."
      , run_pandoc = TRUE
      )
  })

dir("docs", pattern = "nb.html", full.names = TRUE) %>%
  file.rename(to = stringi::stri_replace_first_fixed(., "nb.", ""))
