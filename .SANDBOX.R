
get.smart("drugs")$
  use(identifier, metrics, retain = c(route), subset = days_to_market >= 0) |> 
  # View() 
  setkey(route, alt_ndc, days_to_market, on_market_age) |>
  plot_metric_cor(metric = on_market_age)


subplot(
  drug_metric_oma_viz
  , drug_metric_dma_viz
  , nrows = 2
  , shareX = FALSE
  , shareY = FALSE
  , titleX = TRUE
  , titleY = TRUE
  ) 

