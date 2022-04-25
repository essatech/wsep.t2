#' @title Grouped Random Sample
#'
#' @description
#' Take a simple random sample by a group
#'
#' @details
#' Note that there is no repeat sampling or replacement. If the sample size `n` exceeds the number of stream crossings in a stratum, then only the limited number of available crossings will be returned.
#'
#' @param data A data object of class `sf`.
#' @param group_name String. Column name for the strata to sample across. It is assumed that `group_name` is `strata` and the values are `stratum_1` for stream segments less than 3rd order and `stratum_2` for stream segments 3rd order and greater.
#' @param n Sample size n for each stratum
#'
#' @return
#' A object of class `sf` of sample features.
#'
#' @export
grouped_random_sample <- function(data = NA, group_name = "strata", n = 20) {

  tmp_group_name <- NULL

  vals <- data[, group_name]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]

  data$tmp_group_name <- vals
  data$tmp_id <- 1:nrow(data)

  stdf <- data %>% dplyr::group_by(tmp_group_name) %>%
    dplyr::slice_sample(n = n, replace = TRUE)

  site_types <- data[which(data$tmp_id %in% unique(stdf$tmp_id)), ]
  site_types <- site_types[which(!(duplicated(site_types$tmp_id))), ]

  site_types$tmp_id <- NULL

  site_types <- sf::st_zm(site_types)

  # Make unique name
  site_types$site_id <- paste0("A_", 1:nrow(site_types),
                               "_", site_types$tmp_group_name)

  site_types$tmp_group_name <- NULL

  site_types <- site_types[, c("site_id", group_name)]

  site_types$type <- "type_a"

  return(site_types)

}
