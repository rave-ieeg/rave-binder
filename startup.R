# module load miniconda
# Sys.setenv("R_RPYMAT_CONDA_EXE" = Sys.which("conda"))

# Sys.setenv("R_RPYMAT_CONDA_PREFIX" = conda_env_path)
# ravemanager::configure_python()



local({

  options(timeout = 3600)
  conda_path <- Sys.which("conda")
  root_path <- "/home/jovyan"
  rave_root <- file.path(root_path, "RAVE")

  whoami <- Sys.info()[["user"]]
  if(!length(whoami) || !nzchar(whoami)) {
    whoami <- "shared"
  }

  Sys.setenv("R_RPYMAT_CONDA_EXE" = conda_path)
  Sys.setenv("R_RPYMAT_CONDA_PREFIX" = "/srv/conda/envs/notebook")

  config <- list(
    paths = list(
      root = rave_root,
      runtime = file.path(rave_root, "Runtime"),
      data = file.path(rave_root, "Data", "Derivative"),
      raw = file.path(rave_root, "Data", "Raw"),
      bids = file.path(rave_root, "Data", "BIDS")
    )
  )

  init_paths <- function(config) {
    rave_root <- config$paths$root
    if( length(rave_root) != 1 || is.na(rave_root) ) {
      stop("Fatal: unable to obtain RAVE root path")
    } else if (!dir.exists(rave_root)) {
      stop("Cannot initialize RAVE root path at: ", rave_root, ". Please create this folder and restart this program.")
    }
    rave_root <- normalizePath(rave_root, mustWork = TRUE)

    r_data_path <- normalizePath(file.path(rave_root, "UserPreferences", "shared", "data"), mustWork = FALSE)

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
    rave_runtime <- normalizePath(config$paths$runtime, mustWork = FALSE)
    rave_datadir <- normalizePath(config$paths$data, mustWork = FALSE)
    rave_rawdir <- normalizePath(config$paths$raw, mustWork = FALSE)
    rave_bidsdir <- normalizePath(config$paths$bids, mustWork = FALSE)

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

    rave_viewerdir <- tools::R_user_dir("threeBrain", "data")
    if(!dir.exists(rave_viewerdir)) { dir.create(rave_viewerdir, recursive = TRUE) }
    options('threeBrain.template_dir' = normalizePath(rave_viewerdir, winslash = "/"))
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

    tryCatch(
      {

        setup_rave(config)

        cli <- asNamespace("cli")
        d <- cli$cli_div(theme = list(rule = list(
          color = "cyan",
          "line-type" = "double")))
        cli$cli_h1("Functions to check {.pkg RAVE} status")
        cli$cli_li("{.run ravemanager::version_info()}          - Check RAVE version")
        cli$cli_li("{.run ravemanager::validate_python()}       - Check Python libraries")
        cli$cli_li("{.run ravemanager::configure_python()}      - Configure Python for RAVE")
        cli$cli_li("{.run ravemanager::finalize_installation()} - Update templates & snippet code")
        cli$cli_h1("Quick start")
        cli$cli_li("{.run rave::start_rave2(as_job=FALSE)} - Start GUI (current console)")
        cli$cli_end(d)
      },
      error = function(e) {
        # ignore : D
      }
    )
  }
  initialize_impl()

  invisible()
})

