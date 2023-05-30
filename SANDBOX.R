.tmp_obj <- get.smart("drugs")$use(identifier, metrics, retain = c(route, sched), subset = days_to_market >= 0) |> 
  setattr("class", c("data.table", "data.frame")) |>
  setkey(route, alt_ndc, days_to_market, on_market_age) |>
  modify_at(c("days_to_market", "on_market_age"), as.numeric) |>
  modify_at("dea_schedule", \(x) modify_if(x, is.na, \(i) "~") |> factor()) 

library(future.callr)
plan(callr)

inspect <- replicate(30, dplyr::slice_sample(.tmp_obj, prop = 0.1), simplify = FALSE) |>
  future_map(\(x){ 
    .tmp_obj; 
    stats::manova(formula = cbind(days_to_market, on_market_age) ~ route*dea_schedule, data = x) 
    }, .options = furrr_options(scheduling = Inf, seed = TRUE, packages = c("data.table"))) 

inspect |>
  map(broom::tidy) |>
  rbindlist(idcol = "sample_id") |>
  define(.SD[!grepl("Resid", term)] |> setorder(term, p.value)) |>
  print()


# Route of Administration URL ----
roa_content <- xml2::read_html("https://www.fda.gov/drugs/data-standards-manual-monographs/route-administration") |> 
                xml2::xml_find_all(xpath = "//table") |> 
                as.character() |> 
                HTML()
