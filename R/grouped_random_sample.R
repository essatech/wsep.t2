#' @title Grouped Random Sample
#'
#' @description
#' Take a simple random sample by a group
#'
#' @details
#' Note that there is no repeat sampling or replacement. If the sample size `n` exceeds the number of stream crossings in a stratum, then only the limited number of available crossings will be returned.
#' Stream order should be labelled as STREAM_ORDER.
#'
#' @param data A data object of class `sf`.
#' @param group_name String. Column name for the strata to sample across. It is assumed that `group_name` is `strata` and the values are `stratum_1` for stream segments less than 3rd order and `stratum_2` for stream segments 3rd order and greater.
#' @param n Sample size n for each stratum
#' @param stream_order Column name for stream order. Defaults to STREAM_ORDER.
#'
#' @return
#' A object of class `sf` of sample features.
#'
#' @export
grouped_random_sample <- function(data = NA, group_name = "strata", n = 20, stream_order = "STREAM_ORDER") {

  # Fix stream order column name
  if(!(stream_order %in% colnames(data))) {
    stop(paste0("Could not find a column named ", stream_order, ". Check stream_order argument"))
  }

  vals <- data[, stream_order]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]
  vals <- as.numeric(vals)
  data$tmp_stream_order <- vals


  tmp_group_name <- NULL

  vals <- data[, group_name]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]

  data$tmp_group_name <- vals
  data$tmp_id <- 1:nrow(data)


  # Split by each stratum and then sample without replacement
  stdf <- data %>% dplyr::group_by(tmp_group_name) %>%
    dplyr::slice_sample(n = n, replace = TRUE)

  site_types <- data[which(data$tmp_id %in% unique(stdf$tmp_id)), ]
  site_types <- site_types[which(!(duplicated(site_types$tmp_id))), ]

  site_types$tmp_id <- NULL
  site_types <- sf::st_zm(site_types)

  # Sort by strata
  site_types <- site_types[order(site_types$tmp_stream_order), ]

  # Make unique name
  site_types$site_id <- paste0("A_", 1:nrow(site_types),
                               "_", site_types$tmp_group_name)

  site_types$tmp_group_name <- NULL


  # Add stream order
  so <- site_types$tmp_stream_order

  site_types <- site_types[, c("site_id", group_name)]

  site_types$type <- "type_a"

  site_types$stream_order <- so

  # Re-order dataframe
  vals <- site_types[, group_name]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]
  site_types <- site_types[order(vals), ]

  return(site_types)

}
