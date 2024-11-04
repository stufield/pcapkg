# Inspired by `expect_snapshot_file()` documentation
save_png <- function(code, ..., gg = TRUE) {
  path <- tempfile(fileext = ".png")
  png(path, height = 480, width = 480, ...)
  on.exit(grDevices::dev.off())
  if ( gg ) {
    print(force(code))
  } else {
    force(code)
  }
  path
}

expect_snapshot_plot <- function(code, name, ...) {
  name <- paste0(name, ".png")
  withr::defer(unlink(name, force = TRUE))
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file
  announce_snapshot_file(name = name)
  # Officially test in Linux, this is necessary due to
  # minor changes in plotting defaults across OSs that cause
  # snapshot testing to fail
  skip_on_os(c("mac", "windows"))
  path <- save_png(code, ...)
  expect_snapshot_file(path, name)
}
