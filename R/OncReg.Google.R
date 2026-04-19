# R CMD check kludge due to memoization
#' @importFrom curl curl_fetch_memory
#' @importFrom jsonlite fromJSON
#' @importFrom memoise memoise
NULL

.OR.Google.address <- memoise::memoise(function(address, key) {
  url <- sprintf("https://maps.googleapis.com/maps/api/geocode/json?address=%s&key=%s",
                 utils::URLencode(address, reserved = TRUE), key)
  output <- jsonlite::fromJSON(url)
  if (output$status != "OK") return(NA)
  return(output$results$formatted_address[1])
})

#' Resolve address using Google Geocoding API
#'
#' Returns the formatted address for a given input string. To reduce API calls,
#' addresses are case-insensitive and whitespace-trimmed, empty addresses after
#' processing return `NA` early, and results are memoized.
#' @param address String input address.
#' @param key String Google API key.
#' @return String formatted address or `NA` if no match is found.
#' @family GoogleMapsPlatform
#' @export
OR.Google.address <- function(address, key) {
  if (!is.character(address)) stop("[OR.Google.address] non-string address")
  address <- tolower(trimws(address))
  if (address == "") return(NA)
  return(.OR.Google.address(address, key))
}

.OR.Google.distance <- memoise::memoise(function(a, b, key, raw = FALSE) {
  url <- "https://routes.googleapis.com/directions/v2:computeRoutes"
  body <- jsonlite::toJSON(list(
    origin = list(address = a),
    destination = list(address = b),
    travelMode = "DRIVE",
    routingPreference = "TRAFFIC_UNAWARE",
    requestedReferenceRoutes = list("SHORTER_DISTANCE")
  ), auto_unbox = TRUE)
  handle <- curl::new_handle()
  curl::handle_setheaders(handle, "Content-Type" = "application/json",
                          "X-Goog-Api-Key" = key,
                          "X-Goog-FieldMask" = "routes.distanceMeters") # "*"
  curl::handle_setopt(handle, post = TRUE, postfields = body)
  response <- curl::curl_fetch_memory(url, handle = handle)
  if (raw) return(response)
  output <- jsonlite::fromJSON(rawToChar(response$content))
  if (is.null(output$routes)) return(NA)
  return(OR.min(output$routes$distanceMeters)/1000)
})

#' Road distance between two addresses using Google Routes API
#'
#' Returns the road distance in km between two addresses. To reduce API calls,
#' addresses are case-insensitive and whitespace-trimmed, empty addresses after
#' processing return `NA` early, processed addresses are internally sorted, and
#' results are memoized.
#'
#' If no route is found, input addresses are compared via Google Geocoding API;
#' identical resolved addresses return `0`, otherwise return `NA`.
#' @param a String address A.
#' @param b String address B.
#' @param key String Google API key.
#' @return Numeric distance in kilometres or `NA` if no route is found.
#' @family GoogleMapsPlatform
#' @export
OR.Google.distance <- function(a, b, key) {
  if (!is.character(a)) stop("[OR.Google.distance] non-string address")
  if (!is.character(b)) stop("[OR.Google.distance] non-string address")
  a <- tolower(trimws(a))
  if (a == "") return(NA)
  b <- tolower(trimws(b))
  if (b == "") return(NA)
  sorted <- sort(c(a, b))
  output <- .OR.Google.distance(sorted[1], sorted[2], key)
  if (is.na(output)) {
    if (OR.NA.to.F(OR.Google.address(a, key) == OR.Google.address(b, key))) return(0)
    else return(NA)
  }
  return(output)
}

#' Travelling salesman distance via Google Routes API
#'
#' Computes the shortest round-trip distance in km starting and ending at the
#' origin, visiting each destination exactly once, via brute-force enumeration
#' of all permutations. Distances are computed via `OR.Google.distance()`.
#' @param origin String starting and ending address.
#' @param destinations String vector of addresses to visit.
#' @param key String Google API key.
#' @param echo Boolean indicating whether to print progress and summary to
#' console. Default = `FALSE`.
#' @return Numeric scalar shortest round-trip distance in km.
#' @family GoogleMapsPlatform
#' @export
OR.Google.TSP <- function(origin, destinations, key, echo = FALSE) {
  n <- length(destinations)
  permutation.matrix <- OR.permutations(n, n)
  addresses <- c(origin, destinations)
  permutation.matrix <- cbind(1, permutation.matrix + 1, 1)
  distance.matrix <- matrix(NA, nrow = nrow(permutation.matrix),
                            ncol = ncol(permutation.matrix) - 1)
  colnames(permutation.matrix) <- paste0("a", 0:(ncol(permutation.matrix) - 1))
  colnames(distance.matrix) <- paste0("d", 1:ncol(distance.matrix))
  if (echo) cat("permutations =", nrow(distance.matrix), "\n")
  for (i in 1:nrow(distance.matrix)) {
    if (echo) cat(i, "")
    for (j in 1:ncol(distance.matrix)) {
      address1 <- addresses[permutation.matrix[i, j]]
      address2 <- addresses[permutation.matrix[i, j + 1]]
      distance.matrix[i, j] <- OR.Google.distance(address1, address2, key)
    }
  }
  dsum <- rowSums(distance.matrix)
  results <- cbind(dsum, permutation.matrix - 1, distance.matrix)
  shortest.index <- which.min(dsum)
  shortest <- results[shortest.index,]
  if (echo) {
    cat("\nmin = ", min(dsum), " km, max = ", max(dsum), " km\n", sep = "")
    for (j in 1:ncol(distance.matrix)) {
      address <- addresses[permutation.matrix[shortest.index, j + 1]]
      distance <- distance.matrix[shortest.index, j]
      cat("Step ", j, ": ", address, "\n= ", distance, " km\n", sep = "")
    }
  }
  return(as.numeric(shortest[1]))
}
