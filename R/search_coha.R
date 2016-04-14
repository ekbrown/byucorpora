#' Search the Corpus of Historical American English (COHA)
#'
#' @description Retrieve keyword-in-context results from the COHA.
#'
#' @param search_terms The search term or terms, as a vector of strings.
#'
#' @param section The section or sections of the COHA to search in, from among \code{"all"} for all sections, \code{"fict"} for the fiction section (the default), \code{"mag"} for magazine, \code{"news"} for newspaper, \code{"nf"} for NF Books. Also, a specific decade can be specified by the first year in the decade, for example, \code{1920} or \code{1880}. Any combination of genres and/or decades can be specified in a vector, for example, \code{section = c("1850", "1950")} or \code{section = c("mag", "news")}.
#'
#' @param max_type An integer specifying the maximum number of unique word types to return for each search string (results shown in the upper right portion of the COHA). For example, searching for nouns with the search string "[n*]" could potentially return tens of thousands of unique types, but the user may only be interested in the 100 most frequent ones.
#'
#' @param max_per_term An integer specifying the maximum number of keyword-in-context (KWIC) results to return for each search string.
#'
#' @param max_total_result An integer specifying the maximum number of total results to return. If only one search term is given in \code{search_terms}, this argument should be equal to or greater than the integer specified in \code{max_per_term}.
#'
#' @return A data frame.
#'
#' @examples
#' search_coha("erstwhile")
#' search_coha("erstwhile", section = "mag")
#' search_coha(c("erstwhile", "ere"), section = c("mag", "news"), max_per_term = 500)
#'
#' @export
search_coha <- function(search_terms, section = "fict", max_type = 10, max_per_term = 100, max_total_result = 1000) {

  # creates curl handle and gets initial cookies from COHA
  ch <- create_byu_curl_handle("coha")

  # gets sections of COHA
  section_df <- data.frame(
    input = c("all", seq(1810, 2000, 10), "fict", "mag", "news", "nf_books"),
    code = seq(0, 24),
    stringsAsFactors = F
  )

  section <- stringr::str_to_lower(section)
  if (any(!section %in% section_df$input)) {
    stop(paste("You must specify 'section' as one or more of:", paste(section_df$input, collapse = " ")))
  }

  row_loc <- sapply(section, function(x) grep(x, section_df$input))
  cur_code <- section_df[row_loc, 'code']
  cur_code <- sort(cur_code, decreasing = T)
  cur_code <- stringr::str_c("sec1=", cur_code, "&", collapse = "")

  all_results <- retrieve_search_terms(search_terms, cur_code, max_type, max_per_term, max_total_result, ch, "coha")

  cat("Done!")
  return(all_results)

}  # end function
