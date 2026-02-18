tofu_installed <- function() {
  result <- processx::run("which", c("tofu"), echo = FALSE)
  return(result$status == 0)
}

tofu_help <- function() {
  result <- processx::run("tofu", c("--help"), echo = FALSE)
  cli::cli_text(result$stdout)
  invisible(result$stdout)
}

tofu_init <- function() {
  result <- processx::run("tofu", c("init"), echo = FALSE)
  cli::cli_text(result$stdout)
  invisible(result$stdout)
}
