#' Search the British National Corpus (BNC)
#'
#' @description Retrieve keyword-in-context results from the BNC.
#'
#' @param search_terms The search term or terms, as a vector of strings.
#'
#' @param section The section of the BNC to search in, from among \code{"all"} for all sections, \code{"spok"} for the spoken section (the default), \code{"fict"} for fiction, \code{"mag"} for magazine, \code{"news"} for newspaper, \code{"non-acad"} for non-academic, \code{"acad"} for academic, and \code{"misc"} for miscellaneous.
#'
#' @param max_type An integer specifying the maximum number of unique word types to return for each search string (results shown in the upper right portion of the web interface). For example, searching for nouns with the search string "[n*]" could potentially return tens of thousands of unique types, but the user may only be interested in the 100 most frequent ones.
#'
#' @param max_per_term An integer specifying the maximum number of keyword-in-context (KWIC) results to return for each search string.
#'
#' @param max_total_result An integer specifying the maximum number of total results to return. If only one search term is given in \code{search_terms}, this argument should be equal to or greater than the integer specified in \code{max_per_term}.
#'
#' @return A data frame.
#'
#' @examples
#' search_bnc("cheers")
#' search_bnc("cheers", section = "spok")
#' search_bnc(c("cheers", "bye"), section = "spok", max_per_term = 500)
#'
#' @export
search_bnc <- function(search_terms, section = "spok", max_type = 10, max_per_term = 100, max_total_result = 1000) {

  # creates curl handle and gets initial cookies
  ch <- create_byu_curl_handle("bnc")

  # defines the sections of the corpus
  section <- tolower(section)

  codes_df <- data.frame(genre = c("all", "spok", "fict", "mag", "news", "non-acad", "acad", "misc"), code = seq(0, 7), stringsAsFactors = F)

  if (any(!section %in% codes_df$genre)) {
    stop(paste("You must specify 'section' as one or more of:", paste(codes_df$genre, collapse = " ")))

  }

  row_loc <- sapply(section, function(x) grep(x, codes_df$genre))
  cur_code <- codes_df[row_loc, 'code']
  cur_code <- sort(cur_code, decreasing = T)
  cur_code <- paste0("sec1=", cur_code, "&", collapse = "")

  all_results <- retrieve_search_terms(search_terms, cur_code, max_type, max_per_term, max_total_result, ch, "bnc")

  cat("Done!")
  return(all_results)

}  # end function
