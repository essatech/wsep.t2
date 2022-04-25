#' @title Road Proximity Sample
#'
#' @description
#' Road Proximity Sample for Site Type B
#'
#' @details
#' WSEP Tier II Road Proximity Sample for Site Type B
#' \itemize{
#'  \item{Apply a buffer (20m for < 3rd order and 40 m for ≥ 3rd order) streams.}
#'  \item{Taking the intersection of roads and this buffer}
#'  \item{Removing any stream crossings (Site Type A), by excluding any cases within 100 m of a crossing to avoid double counting}
#'  \item{Removing any segments < 50m in length unless they are near a switch-back (determined by manual review of the map). Switch-back determination is an optional manual step not automated in R code}
#'  \item{Providing the start point of the segment as well as the segment length and strata id (< 3rd order vs. ≥ 3rd order) and mapping the entire segment on the field maps to facilitate sampling}
#'  \item{Generate a random sample from the Site Type B list the strata with < 3rd order streams}
#'  \item{ Append the complete list of Site Type B from the ≥ 3rd order strata }
#'  \item{ Create a field checklist with at least the following fields: unique identifier (e.g., WatershedName_SD_B_001), coordinates of start point and end point, segment length, and Strata }
#' }
#' Note that there is no repeat sampling or replacement. If the sample size `n` exceeds the number of segments available for a stratum, then only the limited number of available locations will be returned.
#'
#' @param n Sample size n for each stratum. Defaults to 20.
#' @param strm A streamline data set of class `sf` from the BCFWA. Run `utm_projection()` first to ensure projection is in meters.
#' @param roads A road network data set of class `sf`. Run `utm_projection()` first to ensure projection is in meters.
#' @param buffer_s1_m Numeric. Buffer size (in meters) for Strata 1 streams < 3rd order. Defaults to 20 m.
#' @param buffer_s2_m Numeric. Buffer size (in meters) for Strata 2 streams >= 3rd order. Defaults to 40 m.
#' @param buffer_crossings_m Numeric. Exclusion buffer size (in meters) to stream-road crossings to exclude sample sites from Site Type A. Defaults to 100 m.
#' @param small_strm_segment_m Numeric. Remove small stream-road buffered segments unless they are near a switch-back (determined manually and added at end). Default small segment size is set to 50 m.
#' @param stream_order String. Column name for stream order. Defaults to `STREAM_ORDER`. Rename as needed.
#' @importFrom magrittr %>%
#'
#' @return
#' A object of class `sf` of sample features representing sample sites.
#'
#'
#' @export
road_proximity_sample <- function(
  n = 20,
  strm = NA,
  roads = NA,
  buffer_s1_m = 20,
  buffer_s2_m = 40,
  buffer_crossings_m = 100,
  small_strm_segment_m = 50,
  stream_order = "STREAM_ORDER"
  ) {

  # Bind for non-standard
  tmp_group_name <- NULL
  L1 <- NULL


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
  sc2 <- suppressWarnings({ sf::st_intersection(roads, strm) })


  # Apply buffer - group s1
  ssub <- strm[which(strm$tmp_stream_order < 3), ]
  # make placeholder dataframe
  skeep_sb_s1 <- data.frame()
  buff <- sf::st_buffer(roads, buffer_s1_m) # 20 m
  buff <- buff %>% dplyr::group_by() %>% dplyr::summarise()


  # Taking the intersection of roads and this buffer,
  print("Stratum 1 streams and road buffer...")
  rint <- suppressWarnings({ sf::st_intersection(ssub, buff) })
  rint <- suppressWarnings({ sf::st_cast(rint, "LINESTRING") })
  rint$tmp <- "samp"
  rint <- rint[, c("tmp")]



  # Removing any stream crossings (site Type A)
  # by excluding any cases within 100m of a
  # crossing to avoid double counting,
  sc2_100 <- sf::st_buffer(sc2, buffer_crossings_m)
  sc2_100 <- sc2_100 %>% dplyr::group_by() %>% dplyr::summarise()
  sdiff <- suppressWarnings({ sf::st_difference(rint, sf::st_union(sc2_100)) })


  # Removing any segments <50m in length
  # unless they are near a switch-back
  # (determined by manual review of the map),
  sdiff$length_m <- sf::st_length(sdiff)
  sdiff$length_m <- as.numeric(sdiff$length_m)
  skeep <- sdiff[which(sdiff$length_m > small_strm_segment_m), ]
  skeep_sb_s1 <- skeep

  print(paste0(nrow(skeep_sb_s1), " stream segments for Site Type B - Stratum 1"))

  if(nrow(skeep_sb_s1) > 0) {
    skeep_sb_s1$site_id <- paste0("B_", 1:nrow(skeep_sb_s1), "_stratum_1")
    skeep_sb_s1$strata <- "stratum_1"
    skeep_sb_s1$type <- "type_b"
    skeep_sb_s1$tmp <- NULL
    skeep_sb_s1 <- skeep_sb_s1[, c("site_id", "strata", "type", "length_m")]
  }


  # Apply buffer - group s1
  ssub <- strm[which(strm$tmp_stream_order >= 3), ]
  skeep_sb_s2 <- data.frame()


  # Only run if stream segments available
  if(nrow(ssub) > 0) {

    buff <- sf::st_buffer(roads, buffer_s1_m) # 20 m
    buff <- buff %>% dplyr::group_by() %>% dplyr::summarise()

    # taking the intersection of roads and this buffer,
    print("Stratum 2 streams and road buffer...")
    rint <- suppressWarnings({ sf::st_intersection(ssub, buff) })
    rint <- suppressWarnings({ sf::st_cast(rint, "LINESTRING") })
    if(nrow(rint) > 0 ) {
      rint$tmp <- "samp"
      rint <- rint[, c("tmp")]

      # (c) removing any stream crossings (site Type A)
      # by excluding any cases within 100m of a
      # crossing to avoid double counting
      sc2_100 <- sf::st_buffer(sc2, buffer_crossings_m)
      sc2_100 <- sc2_100 %>% dplyr::group_by() %>% dplyr::summarise()
      sdiff <- suppressWarnings({ sf::st_difference(rint, sf::st_union(sc2_100)) })

      # removing any segments <50m in length unless
      # they are near a switch-back
      # (determined by manual review of the map),
      sdiff$length_m <- sf::st_length(sdiff)
      sdiff$length_m <- as.numeric(sdiff$length_m)
      skeep <- sdiff[which(sdiff$length_m > small_strm_segment_m), ]
      skeep_sb_s2 <- skeep

      print(paste0(nrow(skeep_sb_s2), " stream segments for Site Type B - Stratum 2"))

      if(nrow(skeep_sb_s2) > 0) {
        skeep_sb_s2$site_id <- paste0("B_", 1:nrow(skeep_sb_s2), "_stratum_2")
        skeep_sb_s2$strata <- "stratum_2"
        skeep_sb_s2$type <- "type_b"
        skeep_sb_s2$tmp <- NULL
        skeep_sb_s2 <- skeep_sb_s2[, c("site_id", "strata", "type", "length_m")]
      }
    }

  }


  # Merge together
  if(nrow(skeep_sb_s1) == 0 & nrow(skeep_sb_s2) == 0) {
    stop("No stream segments remaining - adjust buffer sizes")
  }

  if(nrow(skeep_sb_s1) == 0 & nrow(skeep_sb_s2) > 0) {
    skeep_all <- skeep_sb_s2
  }

  if(nrow(skeep_sb_s1) > 0 & nrow(skeep_sb_s2) == 0) {
    skeep_all <- skeep_sb_s1
  }

  if(nrow(skeep_sb_s1) > 0 & nrow(skeep_sb_s2) > 0) {
    skeep_all <- rbind(skeep_sb_s1, skeep_sb_s2)
  }


  skeep_all$length_m <- round(skeep_all$length_m, 1)


  # Sample by strata
  s1 <- skeep_all$site_id[which(skeep_all$strata == "stratum_1")]
  if(length(s1) > n) {
    s1_samp <- sample(s1, size = n, replace = FALSE)
  } else {
    s1_samp <- s1
  }

  s2 <- skeep_all$site_id[which(skeep_all$strata == "stratum_2")]
  if(length(s2) > n) {
    s2_samp <- sample(s2, size = n, replace = FALSE)
  } else {
    s2_samp <- s2
  }

  samp_ids <- c(s1_samp, s2_samp)

  # Point sample
  ssamp <- skeep_all[which(skeep_all$site_id %in% samp_ids), ]

  my_geom <- suppressWarnings({ sf::st_cast(ssamp, "LINESTRING") })
  cm <- as.matrix(sf::st_coordinates(my_geom))
  cm <- as.data.frame(cm)

  first_pt <- cm %>% dplyr::group_by(L1) %>%
    dplyr::slice(1)
  first_pt <- as.data.frame(first_pt)

  first_pt_sf <- sf::st_as_sf(first_pt, coords = c("X", "Y"))

  sf::st_crs(first_pt_sf) <- sf::st_crs(strm)$epsg

  if(nrow(first_pt_sf) != nrow(ssamp)) {
    stop("bad coordinate breakdown...")
  }

  # Add column names
  first_pt_sf$site_id  <- ssamp$site_id
  first_pt_sf$strata   <- ssamp$strata
  first_pt_sf$type     <- ssamp$type
  first_pt_sf$length_m <- ssamp$length_m
  first_pt_sf$L1 <- NULL
  first_pt_sf <- first_pt_sf[, c("site_id", "strata", "type", "length_m")]


  # Return sample points and sample lines
  ret_obj <- list()
  ret_obj$points <- first_pt_sf
  ret_obj$line_segments <- skeep_all

  print(paste0("Total sample sites: ", nrow(skeep_all)))

  return(ret_obj)



}
