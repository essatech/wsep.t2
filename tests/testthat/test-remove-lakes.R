test_that("remove lakes", {

  library(wsep.t2)
  data(TsolumStreams)
  s1 <- sum(sf::st_length(TsolumStreams))
  sr <- remove_lentic_bcfwa(strm = TsolumStreams, EDGE_TYPE = "EDGE_TYPE")
  s2 <- sum(sf::st_length(sr))

  # expect fewer with no lakes
  testthat::expect_true(s1 > s2)



  data(SpiusStreams)
  s1 <- sum(sf::st_length(SpiusStreams))
  SpiusStreams$et <- SpiusStreams$EDGE_TYPE
  sr <- remove_lentic_bcfwa(strm = SpiusStreams, EDGE_TYPE = "et")
  s2 <- sum(sf::st_length(sr))

  # expect fewer with no lakes
  testthat::expect_true(s1 > s2)





})
