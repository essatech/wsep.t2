test_that("summary tables make sense", {

  library(wsep.t2)
  data(TsolumStreams)
  data(TsolumRoads)
  strm <- utm_projection(data = TsolumStreams)
  roads <- utm_projection(data = TsolumRoads)


  df1 <- sample_frame_summary(
    strm = strm,
    constrained_strm = strm,
    roads = roads,
    stream_order = "STREAM_ORDER",
    summary_type = "stream_lengths")

  # Should be the same values if we do not input
  check <- sum(df1$unconstrained - df1$constrained)
  testthat::expect_true(check == 0)


  # and stream crossings
  df2 <- sample_frame_summary(
    strm = strm,
    constrained_strm = strm,
    roads = roads,
    stream_order = "STREAM_ORDER",
    summary_type = "stream_crossings")


  # Should be the same values if we do not input
  check <- sum(df2$unconstrained - df2$constrained)
  testthat::expect_true(check == 0)



})
