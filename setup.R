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
split_f <- function(x, f, ...){
#' Formula Split
#' 
#' \code{split_f} is a wrapper base::split() and data.table:::split.data.table() that supports a formulaic definition of the splitting factors.
#' 
#' @param x See \code{?split}
#' @param f See \code{?split}: may also be a right-hand sided formula (e.g., \code{~var1 + var2 + ...})
#' 
#' @return See \code{?split}
#' 
  if (is_formula(f)){ f <- terms(f, data = x) |> attr("term.labels") }
  if (is.data.table(x)){ split(x, by = f, ...) } else { split(x, f = f, ...)}
}

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
  pc <- modify_if(
          pc, \(i) stri_length(i) < 3
          , \(i) stri_pad_left(i, width = 3, pad = "0")
          )
  lc <- modify_if(
          lc
          , \(i) stri_length(i) < 4
          , \(i) stri_pad_left(i, width = ifelse(stri_length(pc) == 3, 5, 4), pad = "0")
          )
  paste(lc, pc, sep = "-")
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
Sys.getenv("GIT_REPOS") |> dir(pattern = "cache.+R$", full.names = TRUE, recursive = TRUE) |>
  walk(source)
