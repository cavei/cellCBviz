context("create JSON")

df <- data.frame(
  src_id = c("a_fap", "a_ec", "c_ec"),
  src_label = c("a", "a", "c"),
  src_parent = c("fap", "ec", "ec"),
  dest_id = c("ra_fap", "rb_fap", "rc_ec"),
  dest_label = c("ra", "rb", "rc"),
  dest_parent = c("fap", "fap", "ec"),
  stringsAsFactors = F
)

json <- dataframe2json_cy(df)

testthat::test_that("convert_data_frame_to_json_cy",{
  testthat::expect_is(json, "json")
})
