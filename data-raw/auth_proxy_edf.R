#' Fonction qui passe le proxy edf
#'
#' @param proxy character, nom du proxy ('noe' ou 'pcy')
#' @param username character, username sesame
#' @param password character, mot de passe sesame
#'
#' @return NULL
#' @export
#' @import baseenc, httr
#' @examples
auth_proxy_edf <- function(proxy = c("noe", "pcy"),
                           username = get_uid(), 
                           password = get_pwd()) {
  # controls
  if (! proxy %in% c("noe", "pcy")) {
    stop("proxy must be either noe or pcy")
  }
  # reset existing http environment variables
  Sys.setenv(http_proxy = "", https_proxy = "",
             HTTP_PROXY = "", HTTPS_PROXY = "",
             ftp_proxy = "", FTP_PROXY = "")
  httr::reset_config()
  # build request
  # authenticate as NNI:passwd to random website
  # do not follow redirection & disable ssl verification of certificates
  url <- base64enc::base64encode(what = charToRaw("http://www.legorafi.fr/"))
  request <- sprintf("https://auth%s.edf.fr/?cfru=%s", proxy, url)
  z <- try(httr::GET(url = request,
                     httr::authenticate(username, password, "basic"),
                     httr::config(followlocation = 0L,
                                  ssl_verifypeer = 0L)))
  # code 302 = redirection
  # if not, something went wrong
  if (!(z$status_code %in% c(302L, 307L))){
    stop("Unable to authenticate.")
  }
  # set envir variables again; useful for curl::curl for ex, and for httr
  httr::set_config(httr::use_proxy(
    url = sprintf("auth%s.edf.fr", proxy),
    port = 3128
  ))
  proxy <- sprintf("http://auth%s.edf.fr:3128", proxy)
  Sys.setenv(http_proxy = proxy, https_proxy = proxy,
             HTTP_PROXY = proxy, HTTPS_PROXY = proxy,
             ftp_proxy = proxy, FTP_PROXY = proxy)
  # if interactive session, print message
  if (interactive()) message("Authentification : OK")
  # return NULL
  return(invisible(NULL))
}