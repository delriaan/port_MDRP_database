tmp_taxonomy <- pmap(drug_taxonomy, \(...) list(...)[-c(3)])
new_taxonomy <- drug_taxonomy$term |> rlang::set_names() |> as.list();

new_taxonomy <- map(tmp_taxonomy, \(i) as.taxonomy(i[-3]))
old_taxonomy <- drug_taxonomy

drug_taxonomy <- new_taxonomy;
drug_taxonomy <- rlang::set_names(drug_taxonomy, map_chr(drug_taxonomy, \(i) i@term))

drug_taxonomy %<>% list2env(envir = new.env())
cache_prep(drug_taxonomy) |> cache_save(.cache);

drug_obs_smart_data$smart.rules$for_usage <- drug_taxonomy;
drug_obs_smart_data$
  enforce.rules(for_usage)$
  cache_mgr(action = upd) |>
  invisible();

cache_prep(drug_obs_smart_data) |> cache_save(.cache)
