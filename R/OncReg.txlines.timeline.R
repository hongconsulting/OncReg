.OR.txlines.timeline.single <- function(regimens, startdates, stopdates, stopreasons = NULL, eventdates = NULL, echo = TRUE) {
  if (is.null(stopreasons)) stopreasons <- rep(NA, length(startdates))
  if (is.null(eventdates)) eventdates <- rep(NA, length(startdates))
  if (echo) {
    cat("[OR.txlines.timeline] start dates =", startdates, "\n")
    cat("[OR.txlines.timeline] stop dates =", stopdates, "\n")
    cat("[OR.txlines.timeline] regimens =", regimens, "\n")
    cat("[OR.txlines.timeline] stop reasons =", stopreasons, "\n")
    cat("[OR.txlines.timeline] event dates =", eventdates, "\n")
  }
  m <- length(eventdates)
  n <- length(startdates)
  udates <- sort(unique(c(startdates, stopdates, eventdates)))
  if (length(udates) == 0) {
    return(data.frame("date" = NA, "regimen" = NA, "stopreasons" = NA, "event" = FALSE))
  }
  output <- data.frame("date" = udates)
  output$regimen <- ""
  output$stopreasons <- ""
  output$event <- FALSE
  for (i in 1:nrow(output)) { # loop through all unique dates
    currentdate <- output$date[i]
    if (echo) cat("[OR.txlines.timeline] current date =", currentdate, "\n")
    if (!is.null(eventdates)) {
      for (j in 1:m) { # loop through all event dates
        if (OR.NA.to.F(currentdate == eventdates[j])) {
          output$event[i] <- TRUE
        }
      }
    }
    for (j in 1:n) { # loop through all lines of treatment
      if (!is.null(stopreasons)) {
        if (OR.NA.to.F(currentdate == stopdates[j])) {
          output$stopreasons[i] <- OR.delim.merge(output$stopreasons[i], stopreasons[j])
        }
      }
      if (OR.NA.to.empty(regimens[j]) == "") next
      if (is.na(startdates[j])) next
      if (currentdate >= startdates[j]) {
        if (echo) cat("[OR.txlines.timeline] i = ", i, ", j = ", j, ": ", sep = "")
        if (!is.na(stopdates[j])) {
          if (currentdate < stopdates[j]) {
            output$regimen[i] <- OR.delim.merge(output$regimen[i], regimens[j])
            if (echo) cat(regimens[j], " ", sep = "")
          }
        } else { # regimen never stopped
          output$regimen[i] <- OR.delim.merge(output$regimen[i], regimens[j])
          if (echo) cat(regimens[j], " ", sep = "")
        }
        if (echo) cat("\n")
      }
    }
  }
  return(output)
}

#' Construct treatment timelines (long format)
#'
#' For each `id`, takes treatment lines stored in wide format (one column per
#' line of treatment for each of treatment regimens, start dates, stop dates,
#' stop reasons, and event dates) and constructs a time-ordered timeline in long
#' format. Each output row corresponds to a unique date at which any treatment
#' starts or stops, or an event (for example, disease progression) occurs. At
#' each unique date, active treatment regimens and stop reasons are merged.
#' @param id Vector of identifiers.
#' @param regimens Comma-delimited string matrix or data frame of treatment
#' regimens (one row per `id`, one column per line of treatment).
#' @param startdates Numeric matrix or data frame of start dates (one row per
#' `id`, one column per line of treatment).
#' @param stopdates Numeric matrix or data frame of stop dates (one row per `id`,
#' one column per line of treatment).
#' @param stopreasons Optional comma-delimited string matrix or data frame of
#' stop reasons (one row per `id`, one column per line of treatment).
#' @param eventdates Optional numeric matrix or data frame of event dates (one
#' row per `id`, one column per line of treatment).
#' @param echo Logical. If `TRUE`, prints additional information to console.
#' Default = `FALSE`.
#' @return A data frame in long format with columns:
#' \describe{
#'   \item{id}{Identifier.}
#'   \item{date}{Date when a treatment starts or stops, or an event occurs.}
#'   \item{regimen}{Active regimens at the date.}
#'   \item{stopreasons}{Stop reasons occurring at the date.}
#'   \item{event}{Logical indicating whether an event occurs at the date.}
#' }
#' @family txlines
#' @export
OR.txlines.timeline <- function(id, regimens, startdates, stopdates,
                                stopreasons = NULL, eventdates = NULL,
                                echo = FALSE) {
  output <- data.frame()
  if (length(id) != nrow(regimens)) stop("[OR.txlines.timeline] inconsistent regimens")
  if (length(id) != nrow(startdates)) stop("[OR.txlines.timeline] inconsistent startdates")
  if (length(id) != nrow(stopdates)) stop("[OR.txlines.timeline] inconsistent stopdates")
  if (length(id) != nrow(stopreasons)) stop("[OR.txlines.timeline] inconsistent stopreasons")
  if (length(id) != nrow(eventdates)) stop("[OR.txlines.timeline] inconsistent eventdates")
  regimens <- as.matrix(regimens)
  startdates <- as.matrix(startdates)
  stopdates <- as.matrix(stopdates)
  stopreasons <- as.matrix(stopreasons)
  eventdates <- as.matrix(eventdates)
  for (i in 1:length(id)) {
    if (echo) cat("[OR.txlines.timeline] id:", id[i],"\n")
    temp <- .OR.txlines.timeline.single(regimens = regimens[i,],
                                        startdates = startdates[i,],
                                        stopdates = stopdates[i,],
                                        stopreasons = stopreasons[i,],
                                        eventdates = eventdates[i,],
                                        echo = echo)
    if (echo) print(temp)
    output <- rbind(output, data.frame("id" = id[i],
                                       "date" = temp$date,
                                       "regimen" = temp$regimen,
                                       "stopreasons" = temp$stopreasons,
                                       "event" = temp$event))
  }
  output$stopreasons <- OR.NA.to.empty(output$stopreasons)
  return(output)
}

#' Convert treatment timelines to wide format
#'
#' Takes treatment timelines in long format (as produced by
#' `OR.txlines.timeline()`) and converts them to wide format with one row per
#' `id` and one set of start date, regimen, and event date columns per treatment
#' line.
#' @param id Vector of identifiers.
#' @param date Numeric vector of dates (sorted within each `id`).
#' @param regimen Comma-delimited string vector of active regimens at each date.
#' @param event Logical vector indicating whether an event occurs at each date.
#' @param line Numeric vector indicating treatment line at each date.
#' @return A data frame in wide format with columns, where `k` indexes treatment
#' lines:
#' \describe{
#'   \item{id}{Identifier.}
#'   \item{date.startL(k)}{Start date of line `k`.}
#'   \item{regimenL(k)}{Regimens in line `k`.}
#'   \item{date.eventL(k)}{Earliest event date (regardless of line label) after
#'   the start date of line `k`.}
#' }
#' @family txlines
#' @export
OR.txlines.wide <- function(id, date, regimen, event, line) {
  line <- OR.as.numeric(line)
  ulines <- OR.NA.rm(sort(unique(line)))
  input <- data.frame("id" = id, "date" = date, "regimen" = regimen,
                      "event" = event, "line" = line)
  input$eventdate <- NA
  input$eventdate[input$event] <- input$date[input$event]
  output <- data.frame("id" = unique(id))
  # setup whole dataset
  for (uline in ulines) {
       output[[paste0("date.startL", uline)]] <- NA
       output[[paste0("regimenL", uline)]] <- ""
       output[[paste0("date.eventL", uline)]] <- NA
  }
  # loop through individual IDs
  for (uid in unique(id)) {
    subset <- input[input$id == uid,]
    subset$line <- OR.LOCF(subset$line)
    for (uline in unique(subset$line)) {
      subsubset <- subset[subset$line == uline,]
      subsubset.startdate <- OR.min(subsubset$date)
      output[[paste0("date.startL", uline)]][output$id == uid] <- subsubset.startdate
      # event dates need not be labelled under the current line
      subsubset.eventdate <- subset$eventdate[subset$eventdate > subsubset.startdate]
      output[[paste0("date.eventL", uline)]][output$id == uid] <- OR.min(subsubset.eventdate)
      for (i in 1:nrow(subsubset)) {
        output[[paste0("date.regimenL", uline)]][output$id == uid] <-
          OR.delim.merge(output[[paste0("date.regimenL", uline)]][output$id == uid], subsubset$regimen[i])
      }

    }
    # for (i in 1:nrow(subset)) {
    #   if (!is.na(subset$line[i])) {
    #     output[[paste0("date.startL", subset$line[i])]][output$id == uid] <- subset$date[i]
    #     # calculate the event date
    #
    #     # if (subset$line[i] > 1) {
    #     #   output[[paste0("stopdate", subset$line[i] - 1)]][output$id == id] <- subset$date[i]
    #     # }
    #     output[[paste0("regimenL", subset$line[i])]][output$id == uid] <-
    #       OR.delim.merge(output[[paste0("regimenL", subset$line[i])]][output$id == uid],
    #                      subset$regimen[i])
    #     # if (subset$event[i]) { # event date needs to be the earliest event after date start
    #     #   output[[paste0("date.eventL", subset$line[i])]][output$id == uid] <-
    #     #     OR.min(c(output[[paste0("date.eventL", subset$line[i])]][output$id == uid],
    #     #              subset$date[i]))
    #     # }
    #   }
    # }
  }
  return(output)
}
