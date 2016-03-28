#' Set credentials
#'
#' @description Sets the email and password registered with the BYU corpora that should be be used during the current R session for queries on the corpora.
#'
#' @param email The email as a string.
#' @param password The password as a string.
#' @return Sets email and password in the global options of the current R session with \code{options()} and returns a message to the user.
#' @examples
#' set_credentials("my_username@institution.edu", "my_password")
#' @details If users are not sure which email and password are registered, they should visit \href{http://corpus.byu.edu/login.asp}{corpus.byu.edu} in a web browser to figure it out.
#' @export
set_credentials <- function(email, password) {
  options(byu_email = email, byu_password = password)
  get_credentials()
}

#' Get credentials
#'
#' @description Gets the currently set email and password registered with the BYU corpora that should be used during the current R session for queries on the corpora.
#' @return The currently set email, or \code{NULL} if no email is currently set.
#' @examples
#' get_credentials()
#' @export
get_credentials <- function() {
  cur_email <- getOption("byu_email")
  if (is.null(cur_email)) {
    return(NULL)
  } else {
    cur_password <- getOption("byu_password")
    return(cat(paste0("The currently set email is ", cur_email, ", with password: ", gsub(".", "*", cur_password))))
  }
}
