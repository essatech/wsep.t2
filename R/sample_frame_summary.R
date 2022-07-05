#' @title Sample Frame Summary Table
#'
#' @description
#' Produce a summary table of the sampling frame.
#'
#' @details
#' It can be convenient to produce a summary table of the entire sampling frame by strata. This function summarizes the total length of streams by strata and the total count of stream crossings by strata.
#'
#' @param strm A streamline data set of class `sf` from the BCFWA. Run `utm_projection()` first to ensure projection is in meters.
#' @param constrained_strm A the constrained streamline data set of class `sf`. Generally this layer is produced as a derivative of the `strm` layer after constraining NCD, excluding lentic areas and removing alpine areas.
#' @param roads A road network data set of class `sf`. Run `utm_projection()` first to ensure projection is in meters.
#' @param stream_order String. Column name for stream order. Defaults to `STREAM_ORDER`. Rename as needed. Stream order is used to define the sampling strata.
#' @param summary_type String. Which summary table should be produced? Options include `stream_lengths` for a summary of stream lengths by strata or `stream_crossings` for summary of stream crossings by strata. Rename as needed. Stream order is used to define the sampling strata.
#' @importFrom magrittr %>%
#'
#' @return
#' A summary table according to the `summary_type`.
#'
#'
#' @export
sample_frame_summary <- function(strm = NA,
                                 constrained_strm = NA,
                                 roads = NA,
                                 stream_order = "STREAM_ORDER",
                                 summary_type = "stream_lengths") {

  # Fix stream order column name
  if (!(stream_order %in% colnames(constrained_strm))) {
    stop(
      paste0(
        "Could not find a column named ",
        stream_order,
        ". Check stream_order argument"
      )
    )
  }
  vals <- constrained_strm[, stream_order]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]
  vals <- as.numeric(vals)
  constrained_strm$tmp_stream_order <- vals

  # Fix stream order column name
  if (!(stream_order %in% colnames(strm))) {
    stop(
      paste0(
        "Could not find a column named ",
        stream_order,
        ". Check stream_order argument"
      )
    )
  }
  vals <- strm[, stream_order]
  sf::st_geometry(vals) <- NULL
  vals <- vals[, 1]
  vals <- as.numeric(vals)
  strm$tmp_stream_order <- vals


  # Recreate stream road crossings
  sc1 <- suppressWarnings({
    sf::st_intersection(roads, strm)
  })
  sc2 <-
    suppressWarnings({
      sf::st_intersection(roads, constrained_strm)
    })



  # Generate Summary Table For Stream Lengths
  if (summary_type == "stream_lengths") {
    strm$strata <- NA
    strm$strata <-
      ifelse(strm$STREAM_ORDER < 3, "stratum_1", strm$strata)
    strm$strata <-
      ifelse(strm$STREAM_ORDER >= 3, "stratum_2", strm$strata)

    constrained_strm$strata <- NA
    constrained_strm$strata <-
      ifelse(constrained_strm$STREAM_ORDER < 3,
             "stratum_1",
             constrained_strm$strata)
    constrained_strm$strata <-
      ifelse(constrained_strm$STREAM_ORDER >= 3,
             "stratum_2",
             constrained_strm$strata)

    strm$length_m <- as.numeric(sf::st_length(strm))
    constrained_strm$length_m <-
      as.numeric(sf::st_length(constrained_strm))

    c1 <-
      sum(constrained_strm$length_m[which(constrained_strm$strata == "stratum_1")])
    c2 <-
      sum(constrained_strm$length_m[which(constrained_strm$strata == "stratum_2")])
    c3 <- sum(constrained_strm$length_m)

    u1 <- sum(strm$length_m[which(strm$strata == "stratum_1")])
    u2 <- sum(strm$length_m[which(strm$strata == "stratum_2")])
    u3 <- sum(strm$length_m)

    df <- data.frame(
      strata = c("stratum_1", "stratum_2", "total"),
      unconstrained = c(u1, u2, u3),
      constrained = c(c1, c2, c3)
    )

    df$constrained <- round(df$constrained)
    df$unconstrained <- round(df$unconstrained)
    return(df)
  }



  # Generate Summary Table For Stream Lengths
  if (summary_type == "stream_crossings") {
    sc1$strata <- NA
    sc1$strata <-
      ifelse(sc1$STREAM_ORDER < 3, "stratum_1", sc1$strata)
    sc1$strata <-
      ifelse(sc1$STREAM_ORDER >= 3, "stratum_2", sc1$strata)

    sc2$strata <- NA
    sc2$strata <-
      ifelse(sc2$STREAM_ORDER < 3, "stratum_1", sc2$strata)
    sc2$strata <-
      ifelse(sc2$STREAM_ORDER >= 3, "stratum_2", sc2$strata)


    c1 <- nrow(sc1[which(sc1$strata == "stratum_1"),])
    c2 <- nrow(sc1[which(sc1$strata == "stratum_2"),])
    c3 <- nrow(sc1)

    u1 <- nrow(sc2[which(sc2$strata == "stratum_1"),])
    u2 <- nrow(sc2[which(sc2$strata == "stratum_2"),])
    u3 <- nrow(sc2)


    df <- data.frame(
      strata = c("stratum_1", "stratum_2", "total"),
      unconstrained = c(c1, c2, c3),
      constrained = c(u1, u2, u3)
    )

    df$constrained <- round(df$constrained)
    df$unconstrained <- round(df$unconstrained)
    return(df)
  }



}
