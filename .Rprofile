# The commented-out conditional below exists because I had tried this:
# https://github.com/rstudio/renv/issues/345#issuecomment-959114458
# because I was having trouble with renv and devtools::build()
# I commented out because it seemed unnecessary but I am leaving it in just
# in case the issue resurfaces
# if (Sys.getenv("R_BUILD_CHECK") != "TRUE") {
source("renv/activate.R")
# }
if (interactive()) {
  suppressMessages(require(devtools))
  suppressMessages(require(testthat))
}
