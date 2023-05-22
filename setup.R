options(future.globals.maxSize = 2 * 1024 * 10E6);

if (!"book.of.workflow" %in% dir(sprintf("%s/library", R.home()))){
  if (!"remotes" %in% dir(sprintf("%s/library", R.home()))){ 
    htmltools::tags$span(style = "text-deoration:italic; color:#333333; ", "... installing missing package 'remotes'")
    install.packages("remotes", repos = "https://cloud.r-project.org") 
  }
  
  htmltools::tags$span(
    style = "text-deoration:italic; color:#333333; "
    , "... installing missing packages 'book.of.utilities', 'book.of.features', 'book.of.workflow', 'architect'"
    );
  
  remotes::install_github("delriaan/book.of.utilities", subdir = "pkg")
  remotes::install_github("delriaan/book.of.features", subdir = "pkg")
  remotes::install_github("delriaan/book.of.workflow", subdir = "pkg")
  remotes::install_github("delriaan/architect", subdir = "pkg")
}

library(book.of.workflow)
load_unloaded(!!!params$cran_libs, autoinstall = TRUE)
load_unloaded(!!!params$git_libs)

if (!"nb_env" %in% search()){ 
  attach(rlang::env(), name = "nb_env")
  makeActiveBinding("nb_env", \() as.environment("nb_env"), env = as.environment("nb_env"))
}

assign("urls", { list(
    data = list(
      `Data Dictionary` = c("https://www.medicaid.gov/medicaid-chip-program-information/by-topics/prescription-drugs/downloads/recordspecficationanddefinitions.pdf", " (Medicaid.gov)")
      , `MDRP Data` = c("https://download.medicaid.gov/data/drugproducts1q_2023.csv", "")
      , `openFDA Drug Data` = c("https://download.open.fda.gov/drug/ndc/drug-ndc-0001-of-0001.json.zip", " (used to augment MDRP data)")
      ) |> 
      purrr::imap(\(x, y) htmltools::tags$li(htmltools::tags$a(href = x[1], y), x[2])) |> htmltools::tags$ol()
    , git_libs = purrr::map(globalenv()$params$git_libs, \(x) htmltools::tags$li(htmltools::tags$a(title = paste0("delriaan/", x), x))) |> list() |> htmltools::tags$ul()
    )
}, envir = as.environment("nb_env"))

.cache <- cachem::cache_disk(dir = "r_session_cache")