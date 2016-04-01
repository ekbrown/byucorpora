#' Search the Corpus del Español (CDE)
#'
#' @description Search the Corpus del Español.
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

  email <- getOption("byu_email")
  password <- getOption("byu_password")

  if (is.null(email)) stop("It doesn't look like you've set your corpus.byu.edu credentials with 'set_credentials()'")

  ch <- curl::new_handle()
  curl::handle_setopt(ch,
    autoreferer = T,
    followlocation = T,
    header = F
  )

  # gets initial cookie
  splash <- curl::curl_fetch_memory("http://www.corpusdelespanol.org/", handle = ch)

  url <- stringr::str_c("http://www.corpusdelespanol.org/login.asp?email=", shortURLencode(email), "&password=", shortURLencode(password), "&e=")
  login_page <- curl::curl_fetch_memory(url, ch)

  # gets the genres that the user wants to search in
  genre_df <- data.frame(
    input = c("all", "oral", "fict", "news", "acad", "1900s", "1800s", "1700s", "1600s", "1500s", "1400s", "1300s", "1200s"),
    code = c(0, seq(23, 12)),
    stringsAsFactors = F
  )

  # section <- c("oral")
  section <- stringr::str_to_lower(section)
  if (any(!section %in% genre_df$input)) {
    stop(paste("You must specify 'section' as one or more of:", paste(genre_df$input, collapse = " ")))
  }

  row_loc <- sapply(section, function(x) grep(x, genre_df$input))
  cur_code <- genre_df[row_loc, 'code']
  cur_code <- sort(cur_code, decreasing = T)
  cur_code <- stringr::str_c("sec1=", cur_code, "&", collapse = "")

  # counter for running number of results
  all_counter <- 0

  # collector for all results
  all_results <- data.frame()

  # loops over search terms
  for (i in 1:length(search_terms)) {
    #i=1
    cur_search_term <- search_terms[i]
    cat("\tWorking on search term ", i, " of ", length(search_terms), ": ", cur_search_term, "\n", sep = "")

    url <- stringr::str_c("http://www.corpusdelespanol.org/x2.asp?chooser=seq&p=", urlEncodeCdE(cur_search_term), "&w2=&wl=4&wr=4&r1=&r2=&ipos1=-select-&B7=SEARCH&", cur_code, "sec2=0&sortBy=freq&sortByDo2=freq&minfreq1=freq&freq1=4&freq2=4&numhits=", max_type, "&kh=100&groupBy=words&whatshow=raw&saveList=no&changed=&corpus=cde&word=&sbs=&sbs1=&sbsreg1=&sbsr=&sbsgroup=&redidID=&ownsearch=y&compared=&holder=&whatdo=seq&waited=y&rand1=y&whatdo1=1&didRandom=n&minFreq=freq&s1=0&s2=0&s3=0&perc=mi")

    freq_page <- curl::curl_fetch_memory(url, ch)

    no_results <- stringr::str_detect(rawToChar(freq_page$content), stringr::regex("(no matching records|no matches)", ignore_case = T))
    if (no_results) {
      stop(stringr::str_c("There appears to be no results for '", search_term, "'"))
    }

    # links <- stringr::str_match_all(rawToChar(freq_page$content), "href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>")
    links <- stringr::str_match_all(iconv(rawToChar(freq_page$content), "WINDOWS-1252", "UTF-8"), "href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>")


    links_themselves <- links[[1]][,2]
    words <- stringr::str_trim(links[[1]][,3])
    if (length(links_themselves) != length(words)) {
      stop("The number of links and the words that those links are connected are different, and they shouldn't be")
    }

    # counter for number of results for current search string
    cur_counter <- 0

    # loops over the hits for the current search term
    for (j in 1:length(words)) {
      #j=1
      cat("\t\tMatch ", j, " of ", length(words), " for search term ", cur_search_term, ": ", stringr::str_to_lower(words[j]), "\n", sep = "")

      kwic_page <- curl::curl_fetch_memory(stringr::str_c("http://www.corpusdelespanol.org/", urlEncodeCdE(links_themselves[j])), ch)



      # figures out if there is more than one page of results
      more_than_one_page <- stringr::str_detect(rawToChar(kwic_page$content), stringr::regex("<span ID=\"w_page\">PAGE</span>:</font></td>"))
      if (more_than_one_page) {
        # true, there's more than one page of results

        # gets the number of pages of results and runs a loop that many times, loading each successive page and pulling the results

        num_pages <- as.integer(stringr::str_match(rawToChar(kwic_page$content), "&nbsp;&nbsp;1 / (\\d+)&nbsp;\r\n\r")[,2])

        for (k in 1:num_pages) {
          #k=1
          # progress report
          cat("\t\t\tPage ", k, " of ", num_pages, " of ", stringr::str_to_lower(words[j]), "\n", sep="")

          if (k == 1) {
            current_results <- create_kwic_table_cde(rawToChar(kwic_page$content))
            current_results <- cbind(cur_search_term, current_results)
            all_counter <- all_counter + nrow(current_results)
            cur_counter <- cur_counter + nrow(current_results)

            if (all_counter > max_total_result) {
              current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
              all_results <- rbind(all_results, current_results)
              break
            }

            if (cur_counter > max_per_term) {
              current_results <- current_results[1:(nrow(current_results) - (cur_counter - max_per_term)), ]
              all_results <- rbind(all_results, current_results)
              break
            }
            all_results <- rbind(all_results, current_results)
          } else {
            cur_link <- links_themselves[j]
            cur_link <- sub("xx=\\d+", paste0("p=", k), cur_link)
            fifth_url <- stringr::str_c("http://www.corpusdelespanol.org/", cur_link)

            next_kwic_page <- curl::curl_fetch_memory(fifth_url, ch)
            current_results <- create_kwic_table_cde(rawToChar(next_kwic_page$content))

            current_results <- cbind(cur_search_term, current_results)

            all_counter <- all_counter + nrow(current_results)
            cur_counter <- cur_counter + nrow(current_results)
            if (all_counter > max_total_result) {
              current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
              all_results <- rbind(all_results, current_results)
              # cat(current.results, file = output.file, sep="\n", append = T)
              break
            }
            if (cur_counter > max_per_term) {
              current_results <- current_results[1:(nrow(current_results) - (cur_counter - max_per_term)), ]
              all_results <- rbind(all_results, current_results)
              # cat(current.results, file = output.file, sep="\n", append = T)
              break
            }
            all_results <- rbind(all_results, current_results)
            # cat(current.results, file = output.file, sep="\n", append = T)
          }
        } # next page, end of k loop
      } else {
        # only one page of results, pulls the results from that one page
        current_results <- create_kwic_table_cde(rawToChar(kwic_page$content))

        current_results <- cbind(cur_search_term, current_results)
        all_counter <- all_counter + nrow(current_results)
        cur_counter <- cur_counter + nrow(current_results)


        if (all_counter > max_total_result) {
          current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
          all_results <- rbind(all_results, current_results)
          break
        }
        if (cur_counter > max_per_term) {
          current_results <- current_results[1:(length(current_results) - (cur_counter - max_per_term)), ]
          all_results <- rbind(all_results, current_results)
          break
        }
        all_results <- rbind(all_results, current_results)
      }
      if (all_counter >= max_total_result | cur_counter >= max_per_term) {
        break
      }
    } # next link
    if (all_counter >= max_total_result) {
      break
    }
    if (cur_counter >= max_per_term) {
      next
    }
  } # next search string

  cat("Done!")
  return(all_results)

}  # end function
