#' Search the Corpus of Contemporary American English (COCA)
#'
#' @description Search the COCA.
#' @param search_terms The search term or terms, as a vector of strings.
#' @param section The section of the COCA to search in, from among \code{"all"} for all sections (the default), \code{"spok"} for the spoken section, \code{"fic"} for fiction, \code{"mag"} for magazine, \code{"news"} for newspaper, \code{"acad"} for academic.
#' @return A data frame.
#' @examples
#' search_coca("hello")
#' search_coca("dude", section = "spok")
#'
#' @export
search_coca <- function(search_terms, section = "all", max_type = 10, max_per_term = 100, max_total_result = 1000) {

  # search_terms <- c("[dude]", "[hello]")
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
  splash <- curl::curl_fetch_memory("http://corpus.byu.edu/coca", handle = ch)

  url <- stringr::str_c("http://corpus.byu.edu/coca/login.asp?email=", shortURLencode(email), "&password=", shortURLencode(password), "&e=")
  login_page <- curl::curl_fetch_memory(url, ch)


  section <- stringr::str_to_lower(section)
  if (!section %in% c("all", "spok", "fic", "mag", "news", "acad")) {
    stop(cat("You must specify 'section' as one of: 'all', 'spok', 'fic', 'mag', 'news', 'acad'"))
  }
  codes_df <- data.frame(genre = c("all", "spok", "fic", "mag", "news", "acad"), code = c(0, 1, 2, 3, 4, 5))
  cur_code <- codes_df[grep(section, codes_df$genre), 'code']

  # counter for running number of results
  all_counter <- 0

  # collector for all results
  all_results <- data.frame()

  # loops over search terms
  for (i in 1:length(search_terms)) {
    #i=1
    cur_search_term <- search_terms[i]
    cat("\tWorking on search term ", i, " of ", length(search_terms), ": ", cur_search_term, "\n", sep = "")

    url <- stringr::str_c("http://corpus.byu.edu/coca/x2.asp?chooser=seq&p=", urlEncodeCdE(cur_search_term), "&w2=&wl=4&wr=4&r1=&r2=&ipos1=-select-&B7=SEARCH&sec1=", cur_code, "&sec2=0&sortBy=freq&sortByDo2=freq&minfreq1=freq&freq1=10&freq2=10&numhits=", max_type, "&kh=100&groupBy=words&whatshow=raw&saveList=no&changed=&corpus=coca&word=&sbs=&sbs1=&sbsreg1=&sbsr=&sbsgroup=&redidID=&ownsearch=y&compared=&holder=&whatdo=seq&waited=y&rand1=y&whatdo1=1&didRandom=n&minFreq=freq&s1=0&s2=0&s3=0&perc=mi")
    freq_page <- curl::curl_fetch_memory(url, ch)

    no_results <- stringr::str_detect(rawToChar(freq_page$content), stringr::regex("(no matching records|no matches)", ignore_case = T))
    if (no_results) {
      stop(stringr::str_c("There appears to be no results for '", search_term, "'"))
    }

    links <- stringr::str_match_all(rawToChar(freq_page$content), "href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>")

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
      kwic_page <- curl::curl_fetch_memory(stringr::str_c("http://corpus.byu.edu/coca/", urlEncodeCdE(links_themselves[j])), ch)

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
            current_results <- create_kwic_table(rawToChar(kwic_page$content))
            current_results <- cbind(cur_search_term, current_results)
            all_counter <- all_counter + nrow(current_results)
            cur_counter <- cur_counter + nrow(current_results)

            if (all_counter > max_total_result) {
              current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
              all_results <- rbind(all_results, current_results)
              # cat(current_results, file = output.file, sep="\n", append = T)
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
          } else {
            cur_link <- links_themselves[j]
            cur_link <- sub("xx=\\d+", paste0("p=", k), cur_link)
            fifth_url <- paste0("http://corpus.byu.edu/coca/", cur_link)
            next_kwic_page <- curl::curl_fetch_memory(fifth_url, ch)
            # next_kwic_page <- getURL(url = fifth.url, curl = ch)
            current_results <- create_kwic_table(rawToChar(next_kwic_page$content))

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
        current_results <- create_kwic_table(rawToChar(kwic_page$content))

        current_results <- cbind(cur_search_term, current_results)
        all_counter <- all_counter + nrow(current_results)
        cur_counter <- cur_counter + nrow(current_results)


        if (all_counter > max_total_result) {
          current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
          all_results <- rbind(all_results, current_results)
          # cat(current_results, file = output.file, sep="\n", append = T)
          break
        }
        if (cur_counter > max_per_term) {
          current_results <- current_results[1:(length(current_results) - (cur_counter - max_per_term)), ]
          all_results <- rbind(all_results, current_results)
          # cat(current_results, file = output.file, sep="\n", append = T)
          break
        }
        all_results <- rbind(all_results, current_results)
        # cat(current_results, file = output.file, sep="\n", append = T)
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
