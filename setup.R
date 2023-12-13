options(future.globals.maxSize = 2 * 1024 * 10E6);

params <- list(
  data_dir = "data"
  , cran_libs =  c('purrr', 'jsonlite', 'httr', 'summarytools', 'munsell', 'cachem', 'SmartEDA', 'htmltools', 'slider', 'stringi', 'magrittr', 'plotly', 'DT', 'data.table', 'pdftools', 'lubridate', 'future', 'furrr', 'future.callr', "visNetwork", "igraph")
  , git_libs = paste0('book.of.', c('utilities', 'features', 'workflow')) |> c('architect', 'smart.data', 'event.vectors')
  , refresh = FALSE
  );

c("book.of.utilities", "book.of.features", "book.of.workflow", "architect", "smart.data") |>
  purrr::walk(\(i){ 
    if (!require(i, character.only = TRUE)){
      spsUtil::quiet(remotes::install_github(glue::glue("delriaan/{i}"), subdir = "pkg"))
    }
  });

library(book.of.workflow)
load_unloaded(!!!params$cran_libs, autoinstall = TRUE)
load_unloaded(!!!params$git_libs)

smart.start();

if (!".cache" %in% ls()){ .cache <- cachem::cache_disk(dir = "r_session_cache") }

#
spsUtil::quiet(if (!"nb_env" %in% search()){ 
  attach(rlang::env(), name = "nb_env")
  makeActiveBinding("nb_env", \() as.environment("nb_env"), env = as.environment("nb_env"))
})

#
if (!"urls" %in% ls("nb_env")){ 
  assign("urls", { list(
    data = list(
      `Data Dictionary` = c("https://www.medicaid.gov/medicaid-chip-program-information/by-topics/prescription-drugs/downloads/recordspecficationanddefinitions.pdf", " (Medicaid.gov)")
      , `MDRP Data` = c("https://download.medicaid.gov/data/drugproducts1q_2023.csv", "")
      , `openFDA Drug Data` = c("https://download.open.fda.gov/drug/ndc/drug-ndc-0001-of-0001.json.zip"
                                , " (used to augment MDRP data)")
      , `Route of Administration` = c("https://www.fda.gov/drugs/data-standards-manual-monographs/route-administration", "FDA.gov")
      ) |> 
      purrr::imap(\(x, y) htmltools::tags$li(htmltools::tags$a(href = x[1], y), x[2])) |> 
      htmltools::tags$ol()
    , git_libs = purrr::map(params$git_libs, \(x){
          htmltools::tags$span(
            htmltools::tags$a(
              href = paste0("https://github.com/delriaan/", x)
              , title = paste0("delriaan/", x)
              , target = "_blank"
              , x)
            , htmltools::tags$br()
            )
          })
    )}, envir = as.environment("nb_env"));
}

# :: Functions & Active-Bindings
#
check_ndc_format <- \(lc, pc){
  #' Check and Conform NDC sequences
  #' 
  #' \code{check_ndc_format} checks labeler and product codes against a predefined, expected format and conforms the arguments accordingly.
  #' 
  #' @param lc,pc Labeler code and product code respectively (package size omitted)
  #' 
  #' @return A string in the format \code{labeler}-\code{product}
  #' 
  pc <- purrr::modify_if(
    pc, \(i) stringi::stri_length(i) < 3
    , \(i) stringi::stri_pad_left(i, width = 3, pad = "0")
  );
  
  lc <- purrr::modify_if(
    lc
    , \(i) stringi::stri_length(i) < 4
    , \(i) stringi::stri_pad_left(i, width = ifelse(stringi::stri_length(pc) == 3, 5, 4), pad = "0")
  );
  
  paste(lc, pc, sep = "-");
}

#
if (!rlang::is_empty(find("read.dictionary"))){ 
  rm(list = "read.dictionary", envir = as.environment("nb_env")) 
}

makeActiveBinding("read.dictionary", \(){
  (get(".cache", envir = as.environment(find(".cache"))))$
    get("api_dictionary") |> 
    stri_replace_all_regex(
        c("\t", "(((\n\n)?[A-UW-Z]([a-z]+)?[ ]?)+[:])", "\n")
        , c("&nbsp;&nbsp;&nbsp;", "<br><span style='font-weight:bold; color:blue'>$0</span>", "<br>")
        , vectorize_all = FALSE
        ) |> 
    paste(collapse = "") |> 
    HTML() |> 
    tags$p() |> 
    html_print(viewer = NULL) |>
    browseURL()
}, env = as.environment("nb_env"))

#
Sys.getenv("GIT_REPOS") |> dir(pattern = "(cache|split).+R$", full.names = TRUE, recursive = TRUE) |>
  walk(source);

split_f <- f_split;

plot_metric_cor <- function(data, metric, primary_metric = "days_to_market", f = "route"){
  f <- rlang::ensym(f)
  metric <- rlang::enexpr(metric) |> as.character();
  primary_metric <- rlang::enexpr(primary_metric) |> as.character();
  ndc_cts <- data[, .(ndc_cts = uniqueN(alt_ndc)), by = c(as.character(f))][, ndc_cts];
  
  # browser()
  rlang::expr(f_split(data, ~!!f)) |>
    eval() |>
    purrr::map_dbl(\(x) spsUtil::quiet(x %$% {
        cor(as.numeric(get(primary_metric)), as.numeric(get(metric)))
      })) |>
    modify_if(is.na, \(x) 0) %>%
    (\(cor_coeffs){
      # Correlation Coefficients and NDC Counts
      cor_coeffs <- cor_coeffs[order(cor_coeffs)];
      ndc_cts <<- ndc_cts[order(cor_coeffs)];
      
      # Correlation Coefficient Vector Names
      nm <- names(cor_coeffs);
      
      # Z-score of Correlation Coefficients
      zscore <- book.of.utilities::calc.zero_mean(cor_coeffs, as.zscore = TRUE, use.population = TRUE);
      
      # Cumulative Proportion of Correlation Coefficients
      cprop <- book.of.utilities::ratio(cor_coeffs + abs(min(cor_coeffs)), type="pareto", decimals = 6);
      
      .wh_scale <- 800 * c(1.25, 0.65)
      
      # Visualize the Correlation Coefficients
      plotly::plot_ly(
        data = data.table(nm, cor_coeffs, ndc_cts, zscore, cprop, key = "cor_coeffs")
        , y = ~nm
        , x = ~cor_coeffs
        , width = .wh_scale[1]
        , height = .wh_scale[2]
        , hoverinfo = "text"
        , hovertext = ~sprintf(
            "<b>%s <sup>NDC Ct. = %s</sup></b><br><b>Cor</b> (%s, %s):<br><span style='font-size:1.2em;'>&nbsp;&nbsp; %.2f</span>"
            , nm, ndc_cts, primary_metric, metric, cor_coeffs
            )
        , name = metric
        , type = "bar"
        ) |>
        plotly::hide_legend() |>
        plotly::config(mathjax = "cdn", displayModeBar = FALSE) |>
        plotly::layout(
          yaxis = list(
            title = list(
              text = "Route"
              , font = list(size = 16, family = "Georgia")
              )
            , gridcolor = "#FFFFFF"
            )
          , xaxis = list(
              title = list(
                text = glue::glue("Correlation Coefficient <br><sup>{primary_metric} vs. {metric}</sup>")
                , font = list(size = 16, family = "Georgia")
                )
              , gridcolor = "#FFFFFF"
              , side = "top"
              )
          , plot_bgcolor = rgb(.8,.8,.8)
          )
    })();
}
