#' @export
create_kwic_table <- function(kwic_page, corpus = "not_bnc_cde") {

  kwic_page <- iconv(kwic_page, "WINDOWS-1252", "UTF-8")


	kwic_page <- stringr::str_replace(kwic_page, stringr::regex("(charset=)windows-1252", ignore_case = T), "\\1UTF-8")

	kwic_page <- stringr::str_replace_all(kwic_page, stringr::regex('value=\\".*?\\">'), ">")

	#kwic_page <- stringr::str_replace_all(kwic_page, "<b>", "</td><td>")		# update change in style tag to cell tag
	#kwic_page <- stringr::str_replace_all(kwic_page, "</b>", "</td><td>")		# update change in style tag to cell tag
	kwic_page <- stringr::str_replace_all(kwic_page, "<b><u>", "</td><td>")
	kwic_page <- stringr::str_replace_all(kwic_page, "</u></b> ", "</td><td>")
	kwic_page <- stringr::str_replace_all(kwic_page, "</u></b>", "</td><td>")
	kwic_page <- stringr::str_replace_all(kwic_page, "</td><td></td><td>", " ")

	kwic_page <- stringr::str_replace_all(kwic_page, "</td><td></td><td>", " ")


	kwic_xml <- xml2::read_html(kwic_page)
	res_table <- rvest::html_table(rvest::html_nodes(kwic_xml, "table")[[2]], fill = T)
	print(res_table)
	View(res_table)
	res_table <- dplyr::slice(res_table, 3:n())
# print(corpus)
	if (corpus == "bnc") {
	  res_table <- dplyr::select(res_table, -(X4:X6))
	  res_table <- dplyr::rename(res_table, case = X1, source = X2, genre = X3, pre_context = X7, match = X8, post_context = X9)
	} else if (corpus == "cde") {
    res_table <- dplyr::select(res_table, -(X4:X6))
    res_table <- dplyr::rename(res_table, case = X1, year = X2, source = X3, pre_context = X7, match = X8, post_context = X9)
  } else {
    # res_table <- dplyr::select(res_table, -(X5:X7))
    # res_table <- dplyr::rename(res_table, case = X1, year_section = X2, context = X3)

    # print(res_table)
    # View(res_table)

  }

  return(res_table)

} # end function

#' @export
urlEncodeCdE <- function(url) {
  url <- gsub(" {1,}", "+", url)
  url <- gsub("á", "%E1", url)
  url <- gsub("é", "%E9", url)
  url <- gsub("í", "%ED", url)
  url <- gsub("ó", "%F3", url)
  url <- gsub("ú", "%FA", url)
  url <- gsub("ü", "%FC", url)
  url <- gsub("ñ", "%F1", url)
  url <- gsub("Á", "%C1", url)
  url <- gsub("É", "%C9", url)
  url <- gsub("Í", "%CD", url)
  url <- gsub("Ó", "%D3", url)
  url <- gsub("Ú", "%DA", url)
  url <- gsub("Ü", "%DC", url)
  url <- gsub("Ñ", "%D1", url)
  url <- gsub("\\[", "%5B", url)
  url <- gsub("\\]", "%5D", url)
  url <- gsub("'", "%27", url)
  url <- gsub(",", "%2C", url)
  return(url)
} # end  function

#' @export
shortURLencode = function(url, reserved = F) {
  OK <- paste0("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz0123456789$_.+!*'(),", if (!reserved) ";/?:@=&", "]")
  x <- strsplit(url, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) paste0("%", as.hexmode(utf8ToInt(x)), collapse = ""))
    x[z] <- y
  }
  paste0(x, collapse = "")
} # end function

#' @export
create_byu_curl_handle <- function(corpus) {
  email <- getOption("byu_email")
  password <- getOption("byu_password")

  if (is.null(email)) stop("It doesn't look like you've set your corpus.byu.edu credentials with 'set_credentials()'")

  ch <- curl::new_handle(
    autoreferer = T,
    followlocation = T,
    header = F,
    useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0"
  )

  url <- switch(corpus,
    cde = "http://www.corpusdelespanol.org/",
    coca = "http://corpus.byu.edu/coca/",
    coha = "http://corpus.byu.edu/coha/old/",
    bnc = "http://corpus.byu.edu/bnc/old/"
  )

  # gets initial cookie
  splash <- curl::curl_fetch_memory(url, handle = ch)

  url <- paste0(paste0(url, "login.asp?email="), shortURLencode(email), "&password=", shortURLencode(password), "&e=")

  login_page <- curl::curl_fetch_memory(url, ch)

  return(ch)

}  # end function

#' @export
retrieve_search_terms <- function(search_terms, cur_code, max_type, max_per_term, max_total_result, ch, corpus) {

  # counter for running number of results
  all_counter <- 0

  # collector for all results
  all_results <- data.frame()

  # loops over search terms
  for (i in 1:length(search_terms)) {
    #i=1
    cur_search_term <- search_terms[i]
    cat("\tWorking on search term ", i, " of ", length(search_terms), ": ", cur_search_term, "\n", sep = "")

    base_url <- switch(corpus,
      cde = "http://www.corpusdelespanol.org/",
      coca = "http://corpus.byu.edu/coca/",
      coha = "http://corpus.byu.edu/coha/old/",
      bnc = "http://corpus.byu.edu/bnc/old/"
    )

    url <- paste0(base_url, "x2.asp?chooser=seq&p=", urlEncodeCdE(cur_search_term), "&w2=&wl=4&wr=4&r1=&r2=&ipos1=-select-&B7=SEARCH&", cur_code, "sec2=0&sortBy=freq&sortByDo2=freq&minfreq1=freq&freq1=4&freq2=4&numhits=", max_type, "&kh=100&groupBy=words&whatshow=raw&saveList=no&changed=&corpus=", corpus, "&word=&sbs=&sbs1=&sbsreg1=&sbsr=&sbsgroup=&redidID=&ownsearch=y&compared=&holder=&whatdo=seq&waited=y&rand1=y&whatdo1=1&didRandom=n&minFreq=freq&s1=0&s2=0&s3=0&perc=mi")

    freq_page <- curl::curl_fetch_memory(url, ch)

    no_results <- grepl("(no matching records|no matches)", iconv(rawToChar(freq_page$content), "WINDOWS-1252", "UTF-8"), ignore.case = T)
    # no_results <- stringr::str_detect(rawToChar(freq_page$content), stringr::regex("(no matching records|no matches)", ignore_case = T))
    if (no_results) {
      stop(paste0("There appears to be no results for '", search_term, "'"))
    }


    temp <- gregexpr("href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>", iconv(rawToChar(freq_page$content), "WINDOWS-1252", "UTF-8"))
    temp <- regmatches(iconv(rawToChar(freq_page$content), "WINDOWS-1252", "UTF-8"), temp)[[1]]
    temp <- gsub("href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>", "\\1\t\\2", temp)
    temp <- strsplit(temp, "\t")

    links_themselves <- sapply(temp, function(x) x[1])
    words <- sapply(temp, function(x) x[2])
    words <- sub("\\s*$", "", words, perl = T)

    # links <- stringr::str_match_all(iconv(rawToChar(freq_page$content), "WINDOWS-1252", "UTF-8"), "href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>")
    # links_themselves <- links[[1]][,2]
    # words <- stringr::str_trim(links[[1]][,3])

    if (length(links_themselves) != length(words)) {
      stop("The number of links and the words that those links are connected are different, and they shouldn't be")
    }

    # counter for number of results for current search string
    cur_counter <- 0

    # loops over the hits for the current search term
    for (j in 1:length(words)) {
      #j=1
      cat("\t\tMatch ", j, " of ", length(words), " for search term ", cur_search_term, ": ", tolower(words[j]), "\n", sep = "")

      kwic_page <- curl::curl_fetch_memory(paste0(base_url, urlEncodeCdE(links_themselves[j])), ch)

      # cat("just got the kwic page\n")

      # figures out if there is more than one page of results
      more_than_one_page <- grepl("<span ID=\"w_page\">PAGE</span>:</font></td>", iconv(rawToChar(kwic_page$content), "WINDOWS-1252", "UTF-8"))
      # more_than_one_page <- stringr::str_detect(rawToChar(kwic_page$content), stringr::regex("<span ID=\"w_page\">PAGE</span>:</font></td>"))
      if (more_than_one_page) {
        # true, there's more than one page of results

        # gets the number of pages of results and runs a loop that many times, loading each successive page and pulling the results
        num_pages <- as.integer(stringr::str_match(rawToChar(kwic_page$content), "&nbsp;&nbsp;1 / (\\d+)&nbsp;\r\n\r")[,2])


        for (k in 1:num_pages) {
          #k=1
          # progress report
          cat("\t\t\tPage ", k, " of ", num_pages, " of ", tolower(words[j]), "\n", sep="")

          if (k == 1) {

            # cat("k equals 1\n")

            current_results <- create_kwic_table(rawToChar(kwic_page$content), corpus)
            current_results <- cbind(cur_search_term, current_results)
            all_counter <- all_counter + nrow(current_results)
            cur_counter <- cur_counter + nrow(current_results)

            if (all_counter > max_total_result) {

              # cat("1: all_counter is greater than max_total_result")

              current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
              all_results <- rbind(all_results, current_results)
              # cat("\t\t\t\t*** (1) The max_total_result of", max_total_result, "has been reached ***\n")
              break
            }

            if (cur_counter > max_per_term) {
              # cat("current counter is at", cur_counter, "\n")
              # cat("The max_per_term of", max_per_term, "has been reached ***\n")
              # cat("cur_counter is greater than max_per_term\n")
              # cat("the number of rows of current_results is", nrow(current_results), "\n")

              current_results <- current_results[1:(nrow(current_results) - (cur_counter - max_per_term)), ]


              # cat("after adding to current_results\n")

              all_results <- rbind(all_results, current_results)

              cur_counter <- 0

              break
            }
            all_results <- rbind(all_results, current_results)
          } else {
            cur_link <- links_themselves[j]
            cur_link <- sub("xx=\\d+", paste0("p=", k), cur_link)
            fifth_url <- paste0(base_url, cur_link)

            next_kwic_page <- curl::curl_fetch_memory(fifth_url, ch)
            current_results <- create_kwic_table(rawToChar(next_kwic_page$content), corpus)

            current_results <- cbind(cur_search_term, current_results)

            all_counter <- all_counter + nrow(current_results)
            cur_counter <- cur_counter + nrow(current_results)
            if (all_counter > max_total_result) {
              current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
              all_results <- rbind(all_results, current_results)
              # cat("\t\t\t\t*** (3) The max_total_result of", max_total_result, "has been reached ***\n")
              break
            }
            if (cur_counter > max_per_term) {
              current_results <- current_results[1:(nrow(current_results) - (cur_counter - max_per_term)), ]
              all_results <- rbind(all_results, current_results)
              # cat("\t\t\t\t*** (4) The max_per_term of", max_per_term, "has been reached ***\n")
              break
            }
            all_results <- rbind(all_results, current_results)
          }
        } # next page, end of k loop
      } else {
        # only one page of results, pulls the results from that one page

        # cat("only one page of results\n")

        current_results <- create_kwic_table(rawToChar(kwic_page$content), corpus)

        # cat("just got the kwic table\n")

        current_results <- cbind(cur_search_term, current_results)
        all_counter <- all_counter + nrow(current_results)
        cur_counter <- cur_counter + nrow(current_results)

        # cat("added search term on the left\n")

        if (all_counter > max_total_result) {
          current_results <- current_results[1:(nrow(current_results) - (all_counter - max_total_result)), ]
          all_results <- rbind(all_results, current_results)
          # cat("\t\t\t\t*** (5) The max_total_result of", max_total_result, "has been reached ***\n")
          break
        }

        if (cur_counter > max_per_term) {
          current_results <- current_results[1:(length(current_results) - (cur_counter - max_per_term)), ]
          all_results <- rbind(all_results, current_results)
          # cat("\t\t\t\t*** (6) The max_per_term of", max_per_term, "has been reached ***\n")
          break
        }

        all_results <- rbind(all_results, current_results)
      }

      if ((all_counter >= max_total_result) | (cur_counter >= max_per_term)) {
        # cat("\t\t\t\t*** (7) The max_total_result of", max_total_result, "or the max_per_term of", max_per_term, "has been reached ***\n")
        break
        # next
      }

    } # next link


    if (all_counter >= max_total_result) {
      # cat("\t\t\t\t*** (8) The max_total_result of", max_total_result, "has been reached ***\n")
      break
    }

    if (cur_counter >= max_per_term) {
      # cat("\t\t\t\t*** (9) The max_per_term of", max_per_term, "has been reached ***\n")
      next
    }

  } # next search string

  return(all_results)

}  # end function
