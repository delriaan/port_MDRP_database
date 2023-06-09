.cache$set("nb_env", nb_env)
book.of.workflow::save_image(!!!ls(), save.dir = "rdata", file.name = "workspace", use.prefix = FALSE, use.timestamp = FALSE)
book.of.workflow::save_image(!!!ls(nb_env), save.dir = "rdata", file.name = "nb_env", use.prefix = FALSE, use.timestamp = FALSE, envir = nb_env)

plot_ly(
  x = degree(graph = gph, mode = "in") |> make.quantiles(as.factor = !FALSE, 0:5/5)
  , y = page_rank(graph = gph)$vector |> ratio(type = "pareto", decimals = 4)
  , type = "box"
  ) |>
  plotly::layout(
    xaxis = list(title = "In-Degree Quantile")
    , yaxis = list(title = "Cumulative Proportional Pagerank")
    )
