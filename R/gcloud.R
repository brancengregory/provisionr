library(processx)
library(cli)
library(glue)
library(yyjsonr)
library(stringr)
library(purrr)
library(dplyr)

gcloud_run <- function(args, format = NULL) {
  if (!gcloud_installed()) {
    stop("gcloud is not installed. Please install it first.", call. = FALSE)
  }
  if (!is.null(format)) {
    args <- c(args, glue("--format={format}"))
  }
  result <- processx::run("gcloud", args = args, echo = FALSE,
                          stderr_to_stdout = TRUE) # Combine stdout and stderr
  if (result$status != 0) {
    stop(glue("gcloud command failed:\n{result$stdout}"), call. = FALSE)
  }

  return(result$stdout)
}

gcloud_installed <- function() {
  result <- processx::run("which", c("gcloud"), echo = FALSE)
  return(result$status == 0)
}

gcloud_create_project <- function(project_id, project_name = NULL, billing_account = NULL) {
  if (missing(project_id)) {
    stop("Project ID is required.", call. = FALSE)
  }

  args <- c("projects", "create", project_id)

  if (!is.null(project_name)) {
    args <- c(args, "--name", project_name)
  }

  if (!is.null(billing_account)) {
    args <- c(args, "--set-as-default", "--billing-account", billing_account)
  }

  output <- gcloud_run(args)
  cli_alert_success(output)
  return(invisible(TRUE))
}

gcloud_list_configurations <- function() {
  # Use json format for more reliable parsing
  output <- gcloud_run(c("config", "configurations", "list"), format = "json")

  # Parse the JSON output
  parsed_json <- yyjsonr::read_json_str(output)

  # Extract the relevant information from each configuration
  config_data <- map(parsed_json, function(config) {
    list(
      name = config$name,
      is_active = config$is_active,
      account = config$properties$core$account,
      project = config$properties$core$project
    )
  })

  # Convert the list of lists to a data frame
  df <- as.data.frame(do.call(rbind, config_data), stringsAsFactors = FALSE)
  return(df)
}


gcloud_set_configuration <- function(configuration_name) {
  output <- gcloud_run(c("config", "configurations", "activate", configuration_name))
  cli_alert_success(output)
  return(invisible(TRUE))
}

gcloud_get_active_configuration <- function() {
  output <- gcloud_run(c("config", "configurations", "list", "--filter='is_active=True'", "--format='get(name)'"))
  return(trimws(output))
}

gcloud_set_project <- function(project_id) {
  output <- gcloud_run(c("config", "set", "project", project_id))
  cli_alert_success(output)
  return(invisible(TRUE))
}

gcloud_get_project_id <- function() {
  output <- gcloud_run(c("config", "get-value", "project"))
  return(trimws(output))
}

gcloud_get_billing_account <- function() {
  output <- gcloud_run(c("beta", "billing", "accounts", "list", "--format='value(name)'"))
  return(output)
}

gcloud_describe_project_json <- function(project_id) {
  output <- gcloud_run(c("projects", "describe", project_id), format = "json")
  parsed_json <- yyjsonr::read_json_str(output)
  return(parsed_json)
}
