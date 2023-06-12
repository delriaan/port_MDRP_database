.cache$set("nb_env", nb_env)
book.of.workflow::save_image(!!!ls(), save.dir = "rdata", file.name = "workspace", use.prefix = FALSE, use.timestamp = FALSE)
book.of.workflow::save_image(!!!ls(nb_env), save.dir = "rdata", file.name = "nb_env", use.prefix = FALSE, use.timestamp = FALSE, envir = nb_env)

logivec <- sample(c(T,F), 4*30, TRUE) |> matrix(ncol = 4)
apply(logivec, 1, prod) |> mean()

 