mdrp_crosstab <- replicate(n = 30, { 
  replications(
    formula = ~ cod_status*drug_category + cod_status*dea_schedule + drug_category:dea_schedule
    , data = get.smart("drugs")$
              use(identifier
                  , retain = c(dea, drug_cat, cod_stat)
                  , subset = runif(n = length(alt_ndc)) <= 0.25
                  ) |> modify_at("cod_status", as.factor)
    )
  })

setattr(
  mdrp_crosstab
  , "interactions"
  , apply(mdrp_crosstab
      , MARGIN = 1
      , FUN = \(k) reduce(k, rbind) |> apply(2, \(x) Rmisc::CI(x))
      ) |> print()
  )
