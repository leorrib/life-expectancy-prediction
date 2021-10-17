tools.load_sources <- function () {
  invisible({capture.output({
    source_files <- list.files('./src', '*.R$')
    sapply(source_files, function (x) {
      path <- paste0('src/', x)
      if(x != 'tools.R') {
        source(path)
      }
    })
  })})
}