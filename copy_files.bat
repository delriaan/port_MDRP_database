rscript -e "output_dir <- '../delriaan.github.io/content/data_projects/mcd_mdrp_database'; dir(pattern = 'css|setup|(viz|mdd).+html$') |> file.copy(to = output_dir, overwrite = TRUE)"
