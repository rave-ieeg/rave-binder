options(timeout = 3600)

libpath="/srv/conda/envs/notebook/lib/R/library"

repos <- c(
  "CRAN" = "https://cloud.r-project.org/",
  "rave-ieeg" = "https://rave-ieeg.r-universe.dev"
)

# incompatible issue with Matrix
# install.packages("lme4", lib = libpath)
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

try({
  startup_file <- "~/startup.R"
  if(file.exists(startup_file)) {
    source(startup_file)
  }
})

# Part of startup.R
root_path <- "/home/jovyan"
rave_root <- file.path(root_path, "RAVE")
conda_path <- Sys.which("conda")
whoami <- "jovyan"

Sys.setenv("R_RPYMAT_CONDA_EXE" = conda_path)
Sys.setenv("R_RPYMAT_CONDA_PREFIX" = "/srv/conda/envs/notebook")

config <- list(
  paths = list(
    root = rave_root,
    runtime = file.path(rave_root, "Runtime"),
    data = file.path(rave_root, "Data", "Derivative"),
    raw = file.path(rave_root, "Data", "Raw"),
    bids = file.path(rave_root, "Data", "BIDS"),
    conda = conda_path
  ),
  conda = list(
    # python = "3.11",
    # default is file.path(rappdirs::user_data_dir(), "r-rpymat", "miniconda", "envs", ...)
    env_prefix = ""
  ),
  threeBrain = list(
    ensure_templates = c(
      "cvs_avg35_inMNI152"
      # "N27"
    )
  ),
  rpyANTs = list(
    ensure_templates = c(
      # "mni_icbm152_nlin_asym_09a",
      # "mni_icbm152_nlin_asym_09b"
      # "mni_icbm152_nlin_asym_09c"
    )
  )
)


init_paths <- function(config) {
  rave_root <- config$paths$root
  if( length(rave_root) != 1 || is.na(rave_root) ) {
    stop("Fatal: unable to obtain RAVE root path")
  }
  dir.create(rave_root, showWarnings = FALSE, recursive = TRUE, mode = "0777")
  rave_root <- normalizePath(rave_root, mustWork = TRUE)

  r_data_path <- normalizePath(file.path(rave_root, "UserPreferences", "shared", "data"), mustWork = FALSE)

  dir.create(r_data_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")

  # set R_user_dir & conda
  Sys.setenv(
    # data like pipeline templates can be shared
    R_USER_DATA_DIR = r_data_path,

    # config and cache are user-specific
    R_USER_CONFIG_DIR = file.path(rave_root, "UserPreferences", whoami, "config"),
    R_USER_CACHE_DIR = file.path(rave_root, "UserPreferences", whoami, "cache")
  )

  config
}

setup_rave <- function(config) {
  # rave_root <- "/Users/Shared/RAVE"
  rave_root <- normalizePath(config$paths$root, mustWork = TRUE)
  rave_runtime <- raveio::dir_create2(normalizePath(config$paths$runtime, mustWork = FALSE))
  rave_datadir <- raveio::dir_create2(normalizePath(config$paths$data, mustWork = FALSE))
  rave_rawdir <- raveio::dir_create2(normalizePath(config$paths$raw, mustWork = FALSE))
  rave_bidsdir <- raveio::dir_create2(normalizePath(config$paths$bids, mustWork = FALSE))

  # set cache & session dir
  ravepipeline <- asNamespace("ravepipeline")
  # binder only has 1 core by default
  ravepipeline$raveio_setopt(key = "max_worker", value = 1L)
  ravepipeline$raveio_setopt(key = "tensor_temp_path", value = file.path(rave_runtime, "shared"))
  ravepipeline$raveio_setopt(key = "ravedash_session_root", value = file.path(rave_runtime, "sessions", whoami))

  # set data & raw dir
  ravepipeline$raveio_setopt(key = "data_dir", value = rave_datadir)
  ravepipeline$raveio_setopt(key = "raw_data_dir", value = rave_rawdir)
  ravepipeline$raveio_setopt(key = "bids_data_dir", value = rave_bidsdir)

  # set 3D viewer
  rave_viewerdir <- tools::R_user_dir("threeBrain", "data")
  if(!dir.exists(rave_viewerdir)) { dir.create(rave_viewerdir, recursive = TRUE) }
  options('threeBrain.template_dir' = normalizePath(rave_viewerdir, winslash = "/"))

  for(subj in config$threeBrain$ensure_templates) {
    template_subjpath <- file.path(rave_viewerdir, subj)
    if(!dir.exists(template_subjpath)) {
      asNamespace('threeBrain')$download_template_subject(subject_code = subj, template_dir = rave_viewerdir)
    }
  }

  # YAEL
  for(subj in config$rpyANTs$ensure_templates) {
    template_path <- file.path(tools::R_user_dir(package = "rpyANTs",
                                                 which = "data"), "templates", subj)
    if(!dir.exists(template_path)) {
      asNamespace("rpyANTs")$ensure_template(subj)
    }
  }
}


initialize_impl <- function() {
  initialized <- Sys.getenv("RAVE_INITIALIZED", unset = "")
  if( identical(initialized, "TRUE") ) { return() }

  # source("renv/activate.R")
  Sys.setenv(RAVE_INITIALIZED = "TRUE")

  # setup_path <- file.path(normalizePath(".", winslash = "/"), "renv", "rave-setup.rds")
  # config <- readRDS(setup_path)

  init_paths(config)

  # RAVE might be missing
  setup_rave(config)
}
initialize_impl()

