
#' Load the data files in a directory.
#'
#' @param data_dir The directory of the data files, passed to `file.path`.
#' @param regex_key Pattern passed to `list.files`. Matches are read.
#' @param header_file A file containing the data column names. Assumed
#'        to be in data_dir, unless it exists.
#' @return dplyr::data_frame
#' @export
compile <- function(data_dir, regex_key, header_file) {
  data_files <- list.files(data_dir, regex_key, full.names = TRUE)

  if(missing(header_file)) {
    return plyr::ldply(data_files, readr::read_csv)
  } else {
    header_file <- ifelse(file.exists(header_file), header_file,
                          file.path(data_dir, header_file))
    header <- colnames(readr::read_tsv(header_file))
  
    # hack!!!
    if(sum(header == "response") == 2) {
      header[header == "response"] <- c("response", "response.1")
    }
  
    data_files <- list.files(data_dir, regex_key, full.names = TRUE)
    return plyr::ldply(data_files, readr::read_tsv, col_names = header)
  }
}
