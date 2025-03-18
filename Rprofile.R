try({
  local({
    file <- "~/startup.R"
    if(file.exists(file)) {
      source(file)
    }
  })
}, silent = TRUE)
