# Render Document ----

# Cumbersome way to force execution order without renaming files just to order them
if (interactive()){ 
  dir(pattern = "index.+qmd$") |> 
    c(dir(pattern = "etl.+qmd$")) |> 
    c(dir(pattern = "ques.+1.+qmd$")) |> 
    c(dir(pattern = "ques.+2.+qRmd$")) |> 
    magrittr::extract(c(1,3)) |>
    quarto::quarto_render(
      execute_dir = getwd()
      , as_job = TRUE
      );
}
