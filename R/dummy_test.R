#' Save a json to file
#'
#' @param file a file name where the json son will be saved.
#'
#' @return NULL
#'
#' @export
generate_dummy_json <- function(file) {
  # library(RColorBrewer)
  # library(jsonlite)

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
  write(json, file=file)
}

