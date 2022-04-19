test_that("utm projection is valid", {

  library(wsep.t2)
  data(TsolumStreams)
  strm <- utm_projection(data = TsolumStreams)
  epsg <- sf::st_crs(strm)$epsg
  testthat::expect_true(epsg == 26910)

  TsolumStreams_4326 <- sf::st_transform(TsolumStreams, 4326)
  strm <- utm_projection(data = TsolumStreams_4326)
  epsg <- sf::st_crs(strm)$epsg
  testthat::expect_true(epsg == 26910)

  data(SpiusStreams)
  strm <- utm_projection(data = SpiusStreams)
  epsg <- sf::st_crs(strm)$epsg
  testthat::expect_true(epsg == 26910)


})
