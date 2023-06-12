# Cumbersome way to force execution order without renaming files just to order them
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

# Copy 'nb.html' files to the 'docs' directory which allows the site
#   to be served via github.io
dir("docs", pattern = "nb.html", full.names = TRUE) %>%
  file.rename(to = stringi::stri_replace_first_fixed(., "nb.", ""))

dir(pattern = "css|setup|viz.+html$") |> file.copy(to = "docs", overwrite = TRUE)
