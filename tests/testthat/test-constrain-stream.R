test_that("stream-constraint-works", {

  #---------------------------------
  # TSOLUM RIVER WATERSHED TESTS
  #---------------------------------

  library(wsep.t2)
  data(TsolumStreams)
  strm <- utm_projection(data = TsolumStreams)
  sf::st_crs(strm)$epsg

  # Get length of first order tribs before
  strm$length_m <- sf::st_length(strm)
  sum1 <- as.numeric(sum(strm$length_m))

  # Then run constrain
  cs <- constrain_streams(strm = strm,
                          length_remove = 500,
                          length_trim = 200)

  # cs <- st_zm(cs)
  # strm <- st_zm(strm)
  # mapview::mapview(list(cs, strm))

  # Check format is ok
  testthat::expect_true(class(cs)[1] == "sf")
  testthat::expect_true(class(cs)[2] == "data.frame")
  tsum <- table(sf::st_geometry_type(cs))
  tsum <- tsum[which(tsum > 0)]
  names(tsum)
  testthat::expect_true(names(tsum) == "LINESTRING")

  cs$length_m <- sf::st_length(cs)
  sum2 <- as.numeric(sum(cs$length_m))

  # After constrain we should have shorter segments
  testthat::expect_true(sum1 > sum2)


  # Then run constrain again - should be even smaller
  cs3 <- constrain_streams(strm = strm,
                          length_remove = 3000,
                          length_trim = 1500)
  cs3$length_m <- sf::st_length(cs3)
  sum3 <- as.numeric(sum(cs3$length_m))
  testthat::expect_true(sum2 > sum3)


  # Try parsing down original dataset randomly
  strm_sub <- strm[sample(nrow(strm), 200), ]
  # then see if constrain still works
  cs4 <- constrain_streams(strm = strm_sub)
  # plot(st_geometry(cs4))
  cs4$length_m <- sf::st_length(cs4)
  sum4 <- as.numeric(sum(cs4$length_m))
  testthat::expect_true(sum3 > sum4)



  #---------------------------------
  # SPIUS RIVER WATERSHED TESTS
  #---------------------------------

  data(SpiusStreams)
  strm <- utm_projection(data = SpiusStreams)
  sf::st_crs(strm)$epsg

  # Get length of first order tribs before
  strm$length_m <- sf::st_length(strm)
  sum1 <- as.numeric(sum(strm$length_m))

  # Then run constrain
  # cs <- constrain_streams(strm = strm,
  #                        length_remove = 1500,
  #                        length_trim = 200)

  # cs <- st_zm(cs)
  # strm <- st_zm(strm)
  # mapview::mapview(list(cs, strm))




})

