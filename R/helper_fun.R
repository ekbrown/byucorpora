#' @export
create_kwic_table <- function(kwic_page) {

  # kwic_page_RCurl <- kwic.page
  # kwic_page_curl <- kwic_page
  # kwic_page <- rawToChar(kwic_page_curl$content)
  # kwic_page <- rawToChar(kwic_page$content)
  # str_detect(kwic_page_RCurl, "panda")
  # str_detect(rawToChar(kwic_page_curl$content), "panda")

  # locations <- str_locate_all(kwic_page_RCurl, "panda")
  # str_sub(kwic_page_RCurl, locations[[1]][, 1] - 500, locations[[1]][, 2] + 100)


  # kwic_page <- kwic_page_original
  # kwic_page <- rawToChar(kwic_page$content)

  kwic_page <- iconv(kwic_page, "WINDOWS-1252", "UTF-8")


	kwic_page <- stringr::str_replace(kwic_page, stringr::regex("(charset=)windows-1252", ignore_case = T), "\\1UTF-8")

	kwic_page <- stringr::str_replace_all(kwic_page, stringr::regex('value=\\".*?\\">'), ">")

	kwic_page <- stringr::str_replace_all(kwic_page, "<b><u>", "</td><td>")
	kwic_page <- stringr::str_replace_all(kwic_page, "</u></b> ", "</td><td>")
	kwic_page <- stringr::str_replace_all(kwic_page, "</u></b>", "</td><td>")
	kwic_page <- stringr::str_replace_all(kwic_page, "</td><td></td><td>", " ")


	kwic_xml <- xml2::read_html(kwic_page)
	res_table <- rvest::html_table(rvest::html_nodes(kwic_xml, "table")[[2]], fill = T)
	res_table <- dplyr::slice(res_table, 2:n())
  res_table <- dplyr::select(res_table, -(X5:X7))
  res_table <- dplyr::rename(res_table, case = X1, year = X2, section = X3, source = X4, pre_context = X8, match = X9, post_context = X10)

  return(res_table)

} # end function

#' @export
create_kwic_table_cde <- function(kwic_page) {

  # kwic_page <- rawToChar(kwic_page$content)

  kwic_page <- iconv(kwic_page, "WINDOWS-1252", "UTF-8")

  kwic_page <- stringr::str_replace(kwic_page, stringr::regex("(charset=)windows-1252", ignore_case = T), "\\1UTF-8")

  kwic_page <- stringr::str_replace_all(kwic_page, stringr::regex('value=\\".*?\\">'), ">")

  kwic_page <- stringr::str_replace_all(kwic_page, "<b><u>", "</td><td>")
  kwic_page <- stringr::str_replace_all(kwic_page, "</u></b> ", "</td><td>")
  kwic_page <- stringr::str_replace_all(kwic_page, "</u></b>", "</td><td>")
  kwic_page <- stringr::str_replace_all(kwic_page, "</td><td></td><td>", " ")


  kwic_xml <- xml2::read_html(kwic_page)
  res_table <- rvest::html_table(rvest::html_nodes(kwic_xml, "table")[[2]], fill = T)
  res_table <- dplyr::slice(res_table, 2:n())
  res_table <- dplyr::select(res_table, -(X4:X6))
  res_table <- dplyr::rename(res_table, case = X1, year = X2, source = X3, pre_context = X7, match = X8, post_context = X9)

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
