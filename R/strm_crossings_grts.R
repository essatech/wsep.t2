#' @title Stream Crossings GRTS Sample
#'
#' @description
#' Generate a GRTS sample from the list of stream crossings.
#'
#' @details
#' Generate a GRTS sample from the list of stream crossings.
#' Part of Component 3 – Riparian. Site Type C.
#' GRTS Generalized Random Tesselation Stratified sampling is achieved using the `grts` function in the `spsurvey` package.
#' \itemize{
#'  \item{Generate a GRTS sample from the list of stream crossings for each of the two strata (<3rd order vs. ≥ 3rd order.}
#'  \item{Create a field checklist with at least the key fields}
#'  }
#'
#' @param n Sample size n for each stratum. Defaults to 20.
#' @param strm A streamline data set of class `sf` from the BCFWA. Run `utm_projection()` first to ensure projection is in meters.
#' @param roads A road network data set of class `sf`. Run `utm_projection()` first to ensure projection is in meters.
#' @param stream_order String. Column name for stream order. Defaults to `STREAM_ORDER`. Rename as needed.
#'
#' @return
#' A object of class `sf` of sample features.
#'
#' @export
strm_crossings_grts <- function(
  n = 20,
  strm = NA,
  roads = NA,
  stream_order = 'STREAM_ORDER') {


  # Add/fix stream order column
  # Fix stream order column name
  if(!(stream_order %in% colnames(strm))) {
    stop(paste0("Could not find a column named ", stream_order, ". Check stream_order argument"))
  }

  vals <- strm[, stream_order]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]
  vals <- as.numeric(vals)
  strm$tmp_stream_order <- vals


  # Recreate stream road crossings

  # Adjustment from Darcy to use line
  # segments instead of crossings
  # sc2 <- suppressWarnings({ sf::st_intersection(roads, strm) })
  # sc2$tmp_id <- 1:nrow(sc2)
  # sc2 <- suppressWarnings({ sf::st_cast(sc2, 'POINT') })
  # sc2 <- sc2[!(duplicated(sc2$tmp_id)), ]

  sc2 <- sf::st_zm(strm)

  # Cant sample more than available
  if(nrow(sc2) < n) {
    n <- nrow(sc2)
  }

  set.seed(123)
  # Generate GRTS sample
  grts <- spsurvey::grts(
    sframe = sc2,
    n_base = n
  )

  my_sample <- grts$sites_base


  # Add on column
  my_sample$strata <- ifelse(my_sample$tmp_stream_order >= 3,
                             "stratum_2",
                             "stratum_1")

  my_sample$stream_order <- my_sample$tmp_stream_order

  my_sample$site_id <- paste0("C_", 1:nrow(my_sample), "_", my_sample$strata)
  my_sample$type <- "type_c"
  my_sample$length_m <- NA

  my_sample <- my_sample[, c("site_id", "strata", "type", "length_m", "stream_order")]


  return(my_sample)


}
