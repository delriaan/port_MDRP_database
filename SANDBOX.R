# NDC Augmentation ----
openFDA_ndc.combined <- data.table::rbindlist(fill = TRUE, modify_at(openFDA_ndc$results, 1:length(openFDA_ndc$results), as.data.table))

plan(list(tweak(multisession, workers = 5), callr))
# future_walk(1:5, \(x) book.of.workflow::load_unloaded(magrittr, furrr, future, data.table))

# NDC format: lengths (excluding hyphens)
ndc_lengths %<-% { list(api_NDC = api_data$NDC, openFDA_NDC = openFDA_ndc.combined$product_ndc) |>
  future_map(\(x){ 
    replicate(
      n = 30
      , expr = sample(x, 100) |>
                stringi::stri_replace_all_regex("-", "", vectorise_all = FALSE) |> 
                stringi::stri_length() |> 
                outer(rlang::set_names(7:11), `==`) |> 
                colSums() %>% 
                magrittr::divide_by(sum(.))
      , simplify = FALSE
      ) |>
    purrr::reduce(rbind) |>
    colMeans() |>
    as.list()
  }, .options = furrr_options(scheduling = Inf, seed = TRUE)) |>
  rbindlist(idcol = "source") 
} %lazy% TRUE %seed% TRUE %packages% c("magrittr", "furrr", "future", "data.table")

# NDC format: first character/digit
ndc_first_digit %<-% { list(api_NDC = api_data$NDC, openFDA_NDC = openFDA_ndc.combined$product_ndc) |>
    future_map(\(x){ 
      replicate(
        n = 30
        , expr = sample(x, 100) |>
                  stringi::stri_sub(length = 1) |> 
                  outer(rlang::set_names(0:9), `==`) |> 
                  colSums() %>% 
                  divide_by(sum(.))
        , simplify = FALSE
        ) |>
      purrr::reduce(rbind) |>
      colMeans() |>
      as.list()
    }, .options = furrr_options(scheduling = Inf, seed = TRUE)) |>
    rbindlist(idcol = "source")
} %lazy% TRUE %seed% TRUE %packages% c("magrittr", "furrr", "future", "data.table")

# NDC format: last character/digit
ndc_last_digit %<-% { list(api_NDC = api_data$NDC, openFDA_NDC = openFDA_ndc.combined$product_ndc) |>
    future_map(\(x){ 
      replicate(
        n = 30
        , expr = sample(x, 100) %>%
                  stringi::stri_sub(from = stringi::stri_length(.), length = 1) |> 
                  outer(rlang::set_names(0:9), `==`) |> 
                  colSums() %>% 
                  divide_by(sum(.))
        , simplify = FALSE
        ) |>
      purrr::reduce(rbind) |>
      colMeans() |>
      as.list()
    }, .options = furrr_options(scheduling = Inf, seed = TRUE)) |>
    rbindlist(idcol = "source")
} %lazy% TRUE %seed% TRUE %packages% c("magrittr", "furrr", "future", "data.table")

# NDC format: consecutive zeros
ndc_zero_seq %<-% { list(api_NDC = api_data$NDC, openFDA_NDC = openFDA_ndc.combined$product_ndc) |>
    future_map(\(x){ 
      replicate(
        n = 30
        , expr = sample(x, 100) |>
                  stringi::stri_extract_first_regex("0{1,11}") |> 
                  stringi::stri_length() |>
                  purrr::modify_if(\(x) is.na(x)||rlang::is_empty(x), \(x) 0) |>
                  outer(rlang::set_names(0:5), `==`) |> 
                  colSums() %>% 
                  divide_by(sum(.))
        , simplify = FALSE
        ) |>
      purrr::reduce(rbind) |>
      colMeans() |>
      as.list()
    }, .options = furrr_options(scheduling = Inf, seed = TRUE)) |>
    rbindlist(idcol = "source")
} %lazy% TRUE %seed% TRUE %packages% c("magrittr", "furrr", "future", "data.table")

invisible(value(.GlobalEnv))

plan(sequential)

mget(ls(pattern = "^ndc.+(f|l|z)"))

# Drug Events ----
ndc_events[!is.na(reactivation_date)] |> View()
