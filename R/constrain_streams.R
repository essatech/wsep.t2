#' @title Constrain Streams
#'
#' @description
#' Constrains the streamline layer
#'
#' @details
#' There is some lack of correspondence between the tangible,
#' physical population of stream reaches and the BC Freshwater Atlas.
#' Two potential sources of non-correspondence are incomplete coverage
#' (i.e., there are streams in the landscape that do not have
#' corresponding mapped depictions in the 1:20K sample frame) and
#' over-coverage (i.e., there may be stream traces indicated that do
#' not correspond to flowing streams in the field, particularly at the
#' upper end of first order streams) (Stevens 2002). In order to minimize
#' the frequency of encountering a non-classified drainage (NCD) in the field,
#' the sample
#' frame for is constrained by:
#' \itemize{
#'  \item{removing 1st order streams that are less than 500 m in length and}
#'  \item{removing the upper 200 m  of remaining 1st order streams depicted by
#'  the 1:20K GIS layer}
#' }
#' This restriction will not eliminate NCDs,
#' particularly for interior watersheds where NCDs are not restricted to
#' the upper end of the first order streams.
#'
#' @param strm A streamlines dataset of class `sf` from the BCFWA.
#' @param length_remove Numeric. Length in meters of first
#' order tributaries that should be removed. Defaults to 500m.
#' @param length_trim Numeric. Length in meters of upstream portion of
#' first order tributaries that should be removed. Defaults to 200m.
#' @importFrom magrittr %>%
#'
#' @return
#' An constrained streamlines dataset of class `sf`.
#'
#' @examples
#'\dontrun{
#' constrain_streams()
#'}
#'
#'
#' @export
constrain_streams <- function(strm = NA, length_remove = 500,
                              length_trim = 200) {

  # Bind for non-standard
  . <- NULL

  # Ensure trim distance is never larger
  # than remove distance
  if(length_trim >= length_remove) {
    stop("length_trim must be smaller than length_remove")
  }

  # Check class and types
  if(!(all(class(strm) == c("sf", "data.frame")))) {
    stop("strm must be of class sf and data.frame")
  }
  type <- unique(sf::st_geometry_type(strm))
  type <- as.character(type)
  if(!(type %in% c("MULTILINESTRING", "LINESTRING"))) {
    stop("strm must be of sf geometry type linestring or multiline string")
  }

  # Check common projections
  set_epsg <- sf::st_crs(strm)
  if(set_epsg$epsg == 3005 | set_epsg$epsg == 4326) {
    stop("strm must be in a UTM projection.
         Run utm_projection()")
  }
  set_epsg_code <- set_epsg$epsg

  set_epsg <- as.character(set_epsg)
  set_epsg <- paste(set_epsg, collapse = " ")
  if(!(grepl("metre", set_epsg) |
       grepl("meter", set_epsg) |
       grepl("UTM", set_epsg))) {
    stop("1. strm must be in a UTM projection.
         Run utm_projection()")
  }


  #----------------------------------------
  # Begin processing
  #----------------------------------------

  # Cast to LINESTRING to ensure no issues
  # with MULTILINESTRING
  strm <- suppressWarnings({ sf::st_cast(strm, "LINESTRING") })


  # Drop all isolated streams not on the network
  int <- sf::st_intersects(strm, strm)
  strm$int_all <- unlist(lapply(int, length))
  # Must touch more than self
  strm <- strm[which(strm$int_all > 1), ]


  # Add global ID for streamline reaches
  strm$strm_id <- 1:nrow(strm)

  #-----------------------------------------
  # Make source confluence and pseudo nodes
  #-----------------------------------------

  # Get all the tips
  strm_ls <- sf::st_zm(strm)

  # break it to points
  sfpts <- sf::st_geometry(strm_ls) %>%
    lapply(., function(x) {
      sf::st_sfc(x) %>%
        sf::st_cast(., 'POINT')})

  sfls_ends <- sapply(sfpts, function(p) {
    p[c(length(p))]
  }) %>%
    sf::st_sfc() %>%
    sf::st_sf('geom' = .)

  sfls_ends <- sf::st_sf(sfls_ends)
  sf::st_crs(sfls_ends) <- sf::st_crs(strm)$epsg
  sfls_ends$strm_id <- strm_ls$strm_id

  int <- sf::st_intersects(sfls_ends, strm_ls)
  ints <- unlist(lapply(int, length))

  sfls_ends$ints <- ints

  # ints 1 is source
  # ints 2 is pseudo
  # ints 3 is confluence

  #---------------------------------------------
  # Work through each tip and trim the streams
  #---------------------------------------------
  # Prep igraph object

  # Prepare nodes for igraph
  nodes <- sfls_ends
  sf::st_geometry(nodes) <- NULL
  nodes$type <- NA

  nodes$type <- ifelse(nodes$ints == 1, "source", nodes$type)
  nodes$type <- ifelse(nodes$ints == 2, "pseudo", nodes$type)
  nodes$type <- ifelse(nodes$ints == 3, "confluence", nodes$type)
  nodes$id <- nodes$strm_id
  nodes <- nodes[!(duplicated(nodes$id)), ]

  # Prepare edges for igraph
  # Make an adjacency list
  touching_list <- sf::st_touches(strm)
  strm$strm_lengths_m <- as.numeric(sf::st_length(strm))

  # Now use igraph to create a graph objects
  # and get the connectivity
  g <- igraph::graph.adjlist(touching_list)
  # Then get the from to relationships
  edges <- as.data.frame(igraph::get.edgelist(g))
  edges$from <- nodes$strm_id[edges$V1]
  edges$to <- nodes$strm_id[edges$V2]
  edges <- edges[, c("from", "to")]
  nodes <- nodes[, c("id", "type")]

  # Add edge weight as line length
  edges$weight <- strm$strm_lengths_m[match(edges$from, strm$strm_id)]
  edges$weight <- round(edges$weight, 1)

  # Make the graph object
  net <- igraph::graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

  # Identify the source and confluence nodes
  weights <- igraph::E(net)$weight
  # Set node attribute
  igraph::set_vertex_attr(net, "type", value = nodes$type)
  a <- igraph::get.vertex.attribute(net, "type")
  source_nodes <- igraph::V(net)[a == "source"]
  conf_nodes <- igraph::V(net)[a == "confluence"]


  # ---------------------------------------------
  # Loop through each source and find the
  # shortest path to the confluence
  # ---------------------------------------------

  for(i in seq_along(source_nodes)) {

    this_source <- source_nodes[i]

    spath <- suppressWarnings({ igraph::shortest_paths(net,
                                    from = this_source,
                                    to = conf_nodes,
                                    output = "vpath") })

    pl <- unlist(lapply(spath$vpath, length))
    # Shortest path
    this_path <- spath$vpath[which(pl == min(pl))][[1]]
    strm_ids <- as.numeric(names(this_path))

    # Not the tail piece below confluence
    strm_ids <- strm_ids[1:(length(strm_ids) - 1)]

    # -------------------------------------------
    # Determine if entire segment should be cut
    # -------------------------------------------
    target_streams <- strm[which(strm$strm_id %in% strm_ids), ]

    if(sum(target_streams$strm_lengths_m) < length_remove) {
      # Remove entire segment
      strm <- strm[which(!(strm$strm_id %in% strm_ids)), ]
      next
    }

    # -------------------------------------------
    # Trim the tip
    # -------------------------------------------
    if(is.na(length_trim)) {
      next
    }

    if(length_trim == 0) {
      next
    }

    # Be sure to order by node path
    target_streams <- target_streams[match(strm_ids, target_streams$strm_id), ]

    trim_remain <- length_trim
    line_index <- 1

    # while loop to clip tips

    while(trim_remain >= 0) {

      # Only cut where you have to
      this_length <- target_streams$strm_lengths_m[line_index]

      if(this_length < trim_remain) {
        # Drop from parent object
        drop_strm_id <- target_streams$strm_id[line_index]
        strm <- strm[which(strm$strm_id != drop_strm_id), ]
        line_index <- line_index + 1
        trim_remain <- trim_remain - this_length
        next
      }

      this_line <- target_streams[line_index, ]
      this_line_conv <- sf::st_zm(this_line)
      this_line_sp <- sf::as_Spatial(this_line_conv)

      # Define point to cut the line
      cut_pt <-  suppressWarnings({
        c_length <- this_line$strm_lengths_m - trim_remain
        rgeos::gInterpolate(this_line_sp, c_length)
      })


      cut_pt_sf <- sf::st_as_sf(cut_pt)
      sf::st_crs(cut_pt_sf) <- sf::st_crs(strm)$epsg

      # Move line endpoint
      lcoords <- sf::st_coordinates(this_line)

      xdiff <- lcoords[, 1] - sf::st_coordinates(cut_pt_sf)[, 1]
      xdiff <- abs(xdiff)

      ydiff <- lcoords[, 2] - sf::st_coordinates(cut_pt_sf)[, 2]
      ydiff <- abs(ydiff)

      dist <- sqrt(xdiff^2 + ydiff^2)
      ind <- which.min(abs(dist))

      lcoords <- as.data.frame(lcoords)
      lcoords_cut <- lcoords[1:ind, ]

      # set the tail to the cutpoint
      lcoords_cut[nrow(lcoords_cut), 'X'] <-
        sf::st_coordinates(cut_pt_sf)[, 1]

      lcoords_cut[nrow(lcoords_cut), 'Y'] <-
        sf::st_coordinates(cut_pt_sf)[, 2]

      lcoords_cut$L1 <- NULL
      lcoords_cut <- as.matrix(lcoords_cut)
      ls <- sf::st_linestring(lcoords_cut, dim = "XYZ")
      lsc <- sf::st_sfc(ls)
      lsc_sf <- sf::st_sf(lsc)
      sf::st_crs(lsc_sf) <- sf::st_crs(this_line)$epsg

      # Then update the geometry
      sf::st_geometry(this_line) <- lsc

      if(nrow(lcoords_cut) == 1) {
        # cannot have single point
        break
      }

      # Finally update the function object
      sf::st_geometry(strm)[strm$strm_id == this_line$strm_id] <- lsc
      break
    }

    print(paste0("Processing segments: ", i,
                 " of ", length(source_nodes)))

  }
  # end of first order stream loop
  # -------------------------------------------

  # Clean up
  # remove temp columns
  strm$strm_lengths_m <- NULL
  strm$strm_id <- NULL
  strm$int_all <- NULL

  return(strm)

}
