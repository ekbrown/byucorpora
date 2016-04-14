#' Search the Corpus del Español (CDE)
#'
#' @description Retrieve keyword-in-context results from the Corpus del Español.
#'
#' @param search_terms The search term or terms, as a vector of strings. Documentation about part-of-speech and wildcard tags is available on the CDE.
#'
#' @param section The section or sections of the CDE to search in, specified by a one-hundred-year period or periods, for example, \code{section = "1900s"} or \code{section = c("1900s", "1800s")}. Specific genres of the 1900s can be specified with \code{"oral"} for the oral section, \code{"fict"} for fiction, \code{"news"} for news, and \code{"acad"} for academic. Additionally, a combination of genres and/or one hundred year periods can be specified, for example, \code{section = c("1200s", "1500s", "1800s")} or \code{section = c("news", "acad")}. To search in all centuries, use \code{section = "all"}.
#'
#' @param max_type An integer specifying the maximum number of unique word types to return for each search string (results shown in the upper right portion of the CDE). For example, searching for nouns with the search string "[n*]" could potentially return tens of thousands of unique types, but the user may only be interested in the 100 most frequent ones.
#'
#' @param max_per_term An integer specifying the maximum number of keyword-in-context (KWIC) results to return for each search string.
#'
#' @param max_total_result An integer specifying the maximum number of total results to return. If only one search term is given in \code{search_terms}, this argument should be equal to or greater than the integer specified in \code{max_per_term}.
#'
#' @return A data frame.
#'
#' @examples
#' search_coca("mujer")
#' search_coca(c("[mujer]", "[hombre]"), section = c("oral", "acad"))
#'
#' @export
search_cde <- function(search_terms, section = "oral", max_type = 10, max_per_term = 100, max_total_result = 1000) {

  # creates curl handle and gets initial cookies from CDE
  ch <- create_byu_curl_handle("cde")

  # gets the genres that the user wants to search in
  genre_df <- data.frame(
    input = c("all", "oral", "fict", "news", "acad", "1900s", "1800s", "1700s", "1600s", "1500s", "1400s", "1300s", "1200s"),
    code = c(0, seq(23, 12)),
    stringsAsFactors = F
  )

  section <- tolower(section)
  if (any(!section %in% genre_df$input)) {
    stop(paste("You must specify 'section' as one or more of:", paste(genre_df$input, collapse = " ")))
  }

  row_loc <- sapply(section, function(x) grep(x, genre_df$input))
  cur_code <- genre_df[row_loc, 'code']
  cur_code <- sort(cur_code, decreasing = T)
  cur_code <- stringr::str_c("sec1=", cur_code, "&", collapse = "")

  # does the heavy lifting of getting the KWIC results for each search term
  all_results <- retrieve_search_terms(search_terms, cur_code, max_type, max_per_term, max_total_result, ch, "cde")

  cat("Done!")
  return(all_results)

}  # end function
