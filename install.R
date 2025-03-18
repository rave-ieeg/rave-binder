install.packages(c(
  "BH", "checkmate", "circular", "devtools", "dipsaus",
  "filearray", "fst", "gifti", "gsignal", "hdf5r", "ieegio",
  "lme4", "lmerTest", "lmtest", "oro.nifti", "pak", "r3js", "ravepipeline",
  "ravetools", "readNSx", "reshape2", "RNifti", "RNiftyReg",
  "rpyANTs", "rpymat", "rstudioapi", "threeBrain", "visNetwork",
  "xml2", "xmlparsedata"))

install.packages(
  c(
    "ravemanager",
    # "dipsaus",
    # "filearray",
    "ieegio",
    "ravedash",
    "raveio",
    "ravepipeline",
    # "ravetools",
    "readNSx",
    "rpyANTs",
    "rpymat",
    "shidashi",
    "threeBrain"
  ),
  repos = "https://rave-ieeg.r-universe.dev")

source("./startup.R")

ravemanager:::finalize_installation(async = FALSE, upgrade = "config-only")
