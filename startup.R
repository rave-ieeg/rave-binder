# module load miniconda
# Sys.setenv("R_RPYMAT_CONDA_EXE" = Sys.which("conda"))

# Sys.setenv("R_RPYMAT_CONDA_PREFIX" = conda_env_path)
# ravemanager::configure_python()

local({
  
  # check if this is a dev env
  if(file.exists("~/.DS_Store")) {
    isdev <- TRUE
    root_path <- normalizePath(".")
    conda_path <- normalizePath("~/miniconda3/bin/conda")
    # binder user... Who is this?
    whoami <- "jovyan"
  } else {
    isdev <- FALSE
    root_path <- path.expand("~")
    conda_path <- Sys.which("conda")
    whoami <- Sys.info()[["user"]]
    if(!length(whoami) || !nzchar(whoami)) {
      whoami <- "shared"
    }
  }
  
  rave_root <- file.path(root_path, "RAVE")
  
  config <- list(
    paths = list(
      root = rave_root,
      runtime = file.path(rave_root, "Runtime"),
      data = file.path(rave_root, "rave_data", "data_dir"),
      raw = file.path(rave_root, "rave_data", "raw_dir"),
      conda = conda_path
    ),
    conda = list(
      python = "3.11",
      # default is file.path(rappdirs::user_data_dir(), "r-rpymat", "miniconda", "envs", ...)
      env_prefix = ""
    ),
    threeBrain = list(
      ensure_templates = c(
        # "cvs_avg35_inMNI152"
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
  
  options(timeout = 3600)
  
  init_paths <- function(config) {
    rave_root <- config$paths$root
    if( length(rave_root) != 1 || is.na(rave_root) ) {
      stop("Fatal: unable to obtain RAVE root path")
    } else if (!dir.exists(rave_root)) {
      stop("Cannot initialize RAVE root path at: ", rave_root, ". Please create this folder and restart this program.")
    }
    rave_root <- normalizePath(rave_root, mustWork = TRUE)
    
    r_data_path <- normalizePath(file.path(rave_root, "UserPreferences", "shared", "data"), mustWork = FALSE)
    
    conda_path <- config$paths$conda
    if(length(conda_path) != 1 || is.na(conda_path) || !nzchar(conda_path)) {
      conda_path <- Sys.which("conda")
      if(!nzchar(conda_path)) {
        conda_path <- "~/miniconda3/bin/conda"
        if(!file.exists(conda_path)) {
          conda_path <- file.path(tools::R_user_dir("rpymat", which = "data"),
                                  "miniconda",
                                  "condabin",
                                  "conda")
        }
      }
    }
    
    conda_env_path <- file.path(r_data_path, "r-rpymat", "miniconda", "envs")
    conda_env_path <- normalizePath(conda_env_path, winslash = "/", mustWork = FALSE)
    conda_env_path <- gsub("[/]{0,}$", "/", conda_env_path)
    
    # set R_user_dir & conda
    Sys.setenv(
      # data like pipeline templates can be shared
      R_USER_DATA_DIR = r_data_path,
      
      # config and cache are user-specific
      R_USER_CONFIG_DIR = file.path(rave_root, "UserPreferences", whoami, "config"),
      R_USER_CACHE_DIR = file.path(rave_root, "UserPreferences", whoami, "cache"),
      
      R_RPYMAT_CONDA_EXE = conda_path,
      R_RPYMAT_CONDA_PREFIX = conda_env_path
    )
    
    config
  }
  
  setup_rave <- function(config) {
    # rave_root <- "/Users/Shared/RAVE"
    rave_root <- normalizePath(config$paths$root, mustWork = TRUE)
    rave_runtime <- normalizePath(config$paths$runtime, mustWork = FALSE)
    rave_datadir <- normalizePath(config$paths$data, mustWork = FALSE)
    rave_rawdir <- normalizePath(config$paths$raw, mustWork = FALSE)
    
    # set cache & session dir
    ravepipeline <- asNamespace("ravepipeline")
    ravepipeline$raveio_setopt(key = "tensor_temp_path", value = file.path(rave_runtime, "shared"))
    ravepipeline$raveio_setopt(key = "ravedash_session_root", value = file.path(rave_runtime, "sessions", whoami))
    
    # set data & raw dir
    ravepipeline$raveio_setopt(key = "data_dir", value = rave_datadir)
    ravepipeline$raveio_setopt(key = "raw_data_dir", value = rave_rawdir)
    
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
    
    tryCatch(
      {
        
        setup_rave(config)
        
        # gather information
        # print(R.version)
        py <- asNamespace("rpymat")$ensure_rpymat(verbose = FALSE)
        
        ip <- asNamespace('dipsaus')$get_ip()
        ip <- ip$available[ip$available != "0.0.0.0"]
        ip <- ip[[length(ip)]]
        
        cli <- asNamespace("cli")
        d <- cli$cli_div(theme = list(rule = list(
          color = "cyan",
          "line-type" = "double")))
        cli$cli_h1("Functions to check {.pkg RAVE} status")
        cli$cli_li("{.run ravemanager::version_info()}          - Check RAVE version")
        cli$cli_li("{.run ravemanager::validate_python()}       - Check Python libraries")
        cli$cli_li("{.run ravemanager::update_rave()}           - Update RAVE")
        cli$cli_li("{.run ravemanager::configure_python()}      - Configure Python for RAVE")
        cli$cli_li("{.run ravemanager::finalize_installation()} - Update templates & snippet code")
        cli$cli_h1("Quick start")
        cli$cli_li(sprintf("{.run rave::start_rave2(host='%s',as_job=TRUE,launch.browser=FALSE)} - Start GUI (background)", ip))
        cli$cli_li("{.run rave::start_rave2(as_job=FALSE)} - Start GUI (current console)")
        cli$cli_li(sprintf("{.run ravedash::debug_modules(host='%s',launch_browser=FALSE)} - Debug Pipeline (background)", ip))
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

