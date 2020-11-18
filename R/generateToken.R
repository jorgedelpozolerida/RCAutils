#' generateToken
#'
#' This function generates the token needed for the autentication to in2data.
#'
# By Lucía & José on 26/02/2020


generateToken <- function(endpoint =
                            'https://in2data.qiagen.com/in2data/backend/api/',
                          user = "api.delpozo",
                          pass = "Api.2019.a20d487a-e05a-4499-b98e-705bf0144124"
){
  # Connects to the in2Data
  lastCallTimestamp <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  url <- paste0(endpoint , 'token/generate')
  body <- paste0('{"login": "',
                 user,
                 '", "password": "',
                 pass,
                 '", "apps": [{"id": "i2data"}]}')

  tokenResp <- httr::POST(url,
                    httr::add_headers("Content-Type"="application/json"),
                    body = body)

  if (tokenResp$status_code == 401) {
    stop(content(tokenResp)$description)
  }

  else if (tokenResp$status_code == 200) {
    auth <- content(tokenResp)$token
    Sys.sleep(1)
  }
  current <- as.numeric(as.numeric(Sys.time())*1000, digits=15)
  if (current - lastCallTimestamp < 1000) {
    message("Many requests, Waiting...")
    Sys.sleep(1)
    message("..OK, doing request")
  }
  auth
}
