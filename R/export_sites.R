#' @title Export Sites
#'
#' @description
#' Export sampling sites for field programs
#'
#' @details
#' Define an output directory to export field sampling sites. Choose from available output formats. Note that when exporting to KML and SHP file format the data will be re-projected to latitude/longitude (EPSG: 4326).
#'
#' @param output_dir String. Local file path the an output directory on your computer.
#' @param site_type_a Sample Sites Type A. sf data frame returned from `grouped_random_sample()`
#' @param type_b Sample Sites of Type B and line segments. Object returned from `road_proximity_sample()`.
#' @param site_type_c Sample Sites Type C. sf data frame returned from `strm_crossings_grts()`
#' @param export_csv Boolean. Should a csv file be exported to the output directory.
#' @param export_shp Boolean. Should a shp (shape file) file be exported to the output directory.
#' @param export_kml Boolean. Should a kml file be exported to the output directory.
#'
#' @return
#' Populates the export directory with output files
#'
#'
#'
#' @export
export_sites <- function(output_dir = NA,
                         site_type_a = NA,
                         type_b = NA,
                         site_type_c = NA,
                         export_csv = TRUE,
                         export_shp = TRUE,
                         export_kml = TRUE) {



  if(!(dir.exists(output_dir))) {
    stop("Could not find output directory")
  }

  substrRight <- function(x, n) {
    substr(x, nchar(x)-n+1, nchar(x))
  }

  last_char <- substrRight(output_dir, 1)

  if(last_char != "/") {
    output_dir <- paste0(output_dir, "/")
  }

  site_type_a$length_m <- NA
  sta <- site_type_a[, c("site_id", "strata", "type", "length_m")]
  stb <- type_b$points
  stb <- stb[, c("site_id", "strata", "type", "length_m")]

  stc <- site_type_c
  stc <- stc[, c("site_id", "strata", "type", "length_m")]


  rename_geometry <- function(g, name) {
    current = attr(g, "sf_column")
    names(g)[names(g)==current] = name
    sf::st_geometry(g)=name
    g
  }

  sta <- rename_geometry(sta, "geomf")
  stb <- rename_geometry(stb, "geomf")
  stc <- rename_geometry(stc, "geomf")

  if(nrow(stb) > 0) {
    allsites <- rbind(sta, stb)
  } else {
    allsites <- sta
  }

  if(nrow(stc) > 0) {
    allsites <- rbind(allsites, stc)
  }


  allsites <- suppressWarnings({ sf::st_cast(allsites, "POINT") })

  allsites <- allsites[!(duplicated(allsites$site_id)), ]

  # Get utm and lat long coordinates
  cutm <- sf::st_coordinates(allsites)
  cutm <- as.matrix(cutm)
  cutm <- as.data.frame(cutm)

  allsites_ll <- sf::st_transform(allsites, 4326)
  cll <- sf::st_coordinates(allsites_ll)
  cll <- as.matrix(cll)
  cll <- as.data.frame(cll)

  asd <- allsites
  sf::st_geometry(asd) <- NULL

  epsg <- sf::st_crs(allsites)$epsg
  utm_zone <- substrRight(epsg, 2)
  asd$utm_zone <- utm_zone
  asd$easting <- cutm$X
  asd$northing <- cutm$Y
  asd$latitude <- cll$Y
  asd$longitude <- cll$X


  if(export_csv) {
    utils::write.csv(asd, file = paste0(output_dir, "sites.csv"), na = "NA", row.names = FALSE)
  }


  sites <- allsites_ll
  segments <- type_b$line_segments
  segments <- suppressWarnings({ sf::st_cast(segments, "LINESTRING") })
  segments <- sf::st_transform(segments, 4326)


  if(export_shp) {
    suppressWarnings({ sf::st_write(sites, dsn = paste0(output_dir, "sites.shp"), delete_dsn = TRUE) })
    suppressWarnings({ sf::st_write(segments, dsn = paste0(output_dir, "segments.shp"), delete_dsn = TRUE) })
  }

  if(export_kml) {
    suppressWarnings({ sf::st_write(sites, dsn = paste0(output_dir, "sites.kml"), delete_dsn = TRUE) })
    suppressWarnings({ sf::st_write(segments, dsn = paste0(output_dir, "segments.kml"), delete_dsn = TRUE) })
  }





}
