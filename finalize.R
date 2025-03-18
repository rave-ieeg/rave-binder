libpath="/srv/conda/envs/notebook/lib/R/library"

repos <- getOption("repos")
repos[["rave-ieeg"]] <- "https://rave-ieeg.r-universe.dev"

install.packages("ravemanager", repos = repos, lib = libpath)
install.packages("threeBrain", repos = repos, lib = libpath)
install.packages("readNSx", repos = repos, lib = libpath)
install.packages("rpymat", repos = repos, lib = libpath)
install.packages("rpyANTs", repos = repos, lib = libpath)
install.packages("ieegio", repos = repos, lib = libpath)
install.packages("ravepipeline", repos = repos, lib = libpath)
install.packages("raveio", repos = repos, lib = libpath)
install.packages("ravedash", repos = repos, lib = libpath)
install.packages("rave", repos = repos, lib = libpath)
install.packages("rutabaga", repos = repos, lib = libpath)
install.packages("ravebuiltins", repos = repos, lib = libpath)

threeBrain::threebrain_finalize_installation(upgrade = "config-only", async = FALSE)
ravepipeline::ravepipeline_finalize_installation(upgrade = "config-only", async = FALSE)
