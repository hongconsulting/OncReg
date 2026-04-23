.OR.delim.contains <- function(x, match, delimiter, partial) {
  parts <- OR.delim.split(x, delimiter)
  match_ <- tolower(trimws(match))
  if (partial) {
    for (p in parts) {
      for (m in match_) {
        if (grepl(m, p, fixed = TRUE)) return(TRUE)
      }
    }
    return(FALSE)
  } else {
    return(any(parts %in% match_))
  }
}

#' Detect delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' For each string in `x`, applies `OR.delim.split()` and checks whether any
#' delimited element matches one or more specified strings.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param match String vector of elements to match.
#' @param delimiter String delimiter. Default = `","`.
#' @param partial Logical. If `TRUE`, each delimited element is tested for
#' substring matches against the provided `match` strings. If `FALSE` (default),
#' matches must be exact.
#' @return A logical vector or matrix indicating whether each string contains a
#' delimited element matching any of the specified strings.
#' @family delim
#' @export
OR.delim.contains <- function(x, match, delimiter = ",", partial = FALSE) {
  if (is.data.frame(x)) x <- as.matrix(x)
  output <- sapply(x, function(x) .OR.delim.contains(x, match, delimiter,
                                                     partial))
  if (is.matrix(x)) {
    dim(output) <- dim(x)
  } else {
    output <- unname(output)
  }
  return(output)
}

.OR.delim.intersect <- function(a, b, delimiter) {
  vector_a <- OR.delim.split(a, delimiter)
  vector_b <- OR.delim.split(b, delimiter)
  return(any(vector_a %in% vector_b))
}

#' Check if two delimited strings share any element (case-insensitive and
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `a` and `b`, applies `OR.delim.split()`
#' and checks whether any element is shared between `a` and `b`.
#' @param a String vector, matrix, or data frame.
#' @param b String vector, matrix, or data frame of the same shape as `a`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A logical vector or matrix indicating whether `a` and `b` share any element.
#' @family delim
#' @export
OR.delim.intersect <- function(a, b, delimiter = ",") {
  if (is.data.frame(a)) a <- as.matrix(a)
  if (is.data.frame(b)) b <- as.matrix(b)
  if (is.null(dim(a)) && is.null(dim(b))) {
    if (length(a) != length(b)) stop("[OR.delim.intersect] a and b must have the same length")
  } else {
    if (!identical(dim(a), dim(b))) stop("[OR.delim.intersect] a and b must have the same shape")
  }
  output <- mapply(.OR.delim.intersect, a, b, delimiter, USE.NAMES = FALSE)
  if (is.matrix(a)) dim(output) <- dim(a) else output <- unname(output)
  return(output)
}

.OR.delim.merge <- function(a, b, delimiter) {
  p1 <- OR.delim.split(a, delimiter)
  p2 <- OR.delim.split(b, delimiter)
  parts <- c(p1, p2)
  parts <- parts[parts != ""]
  parts <- sort(unique(parts))
  return(paste(parts, collapse = paste0(delimiter, " ")))
}

#' Merge delimited string elements pairwise (case-insensitive and
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `a` and `b`, applies `OR.delim.split()`,
#' takes the union of the delimited elements, removes duplicate elements, sorts
#' elements alphabetically, and rejoins the elements using the same delimiter.
#' @param a String vector, matrix, or data frame.
#' @param b String vector, matrix, or data frame of the same shape as `a`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector or matrix with merged delimited elements.
#' @family delim
#' @export
OR.delim.merge <- function (a, b, delimiter = ",") {
  if (is.data.frame(a)) a <- as.matrix(a)
  if (is.data.frame(b)) b <- as.matrix(b)
  if (is.null(dim(a)) && is.null(dim(b))) {
    if (length(a) != length(b))
      stop("[OR.delim.merge] a and b must have the same length")
  } else {
    if (!identical(dim(a), dim(b)))
      stop("[OR.delim.merge] a and b must have the same shape")
  }
  output <- mapply(.OR.delim.merge, a, b, delimiter, USE.NAMES = FALSE)
  if (is.matrix(a)) {
    dim(output) <- dim(a)
  } else {
    output <- unname(output)
  }
  return(output)
}

.OR.delim.replace <- function(x, match, replacement, delimiter) {
  parts <- OR.delim.split(x, delimiter)
  if (any(parts %in% tolower(trimws(match)))) {
    parts[parts %in% tolower(trimws(match))] <- tolower(trimws(replacement))
  }
  parts <- sort(unique(parts))
  parts <- parts[parts != ""]
  return(paste(parts, collapse = paste0(delimiter, " ")))
}

#' Replace delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' For each string in `x`, applies `OR.delim.split()` and checks whether any
#' delimited element matches one or more specified strings. Replaces any
#' matching elements with a new string, removes duplicate elements, sorts
#' elements alphabetically, and rejoins the elements using the same delimiter.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param match String vector of elements to match.
#' @param replacement String delimited element replacement.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector or matrix with replaced delimited elements.
#' @family delim
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' treatment <- OR.delim.replace(treatment, "anastrozole", "ai")
#' treatment <- OR.delim.replace(treatment, "letrozole", "ai")
#' treatment <- OR.delim.replace(treatment, "palbociclib", "cdk46i")
#' treatment <- OR.delim.replace(treatment, "ribociclib", "cdk46i")
#' print(treatment)
#' @family delim
#' @export
OR.delim.replace <- function(x, match, replacement, delimiter = ",") {
  if (is.data.frame(x)) x <- as.matrix(x)
  output <- sapply(x, function(t) .OR.delim.replace(t, match, replacement, delimiter))
  if (is.matrix(x)) {
    dim(output) <- dim(x)
  } else {
    output <- unname(output)
  }
  return(output)
}

#' Split delimited string elements (case-insensitive and whitespace-trimmed)
#'
#' Splits a string by a specified delimiter, replaces `NA` with `""`, trims
#' whitespace, converts to lowercase, and returns delimited elements.
#' @param x String vector.
#' @param delimiter String delimiter. Default = `","`.
#' @return A character vector of delimited elements.
#' @family delim
#' @export
OR.delim.split <- function(x, delimiter = ",") {
  x <- OR.NA.to.empty(x)
  return(tolower(trimws(unlist(strsplit(x, delimiter, fixed = TRUE)))))
}

.OR.delim.subset <- function(a, b, delimiter = ",") {
  vector_a <- OR.delim.split(a, delimiter)
  vector_b <- OR.delim.split(b, delimiter)
  return(all(vector_b %in% vector_a))
}

#' Check if one delimited string is a subset of another (case-insensitive and
#' whitespace-trimmed)
#'
#' For each pair of corresponding strings in `a` and `b`, applies `OR.delim.split()`
#' and checks whether all elements in `b` are
#' contained in `a`.
#' @param a String vector, matrix, or data frame.
#' @param b String vector, matrix, or data frame of the same shape as `a`.
#' @param delimiter String delimiter. Default = `","`.
#' @return A logical vector or matrix indicating whether `b` is a subset of `a`.
#' @family delim
#' @export
OR.delim.subset <- function(a, b, delimiter = ",") {
  if (is.data.frame(a)) a <- as.matrix(a)
  if (is.data.frame(b)) b <- as.matrix(b)
  if (is.null(dim(a)) && is.null(dim(b))) {
    if (length(a) != length(b)) stop("[OR.delim.subset] a and b must have the same length")
  } else {
    if (!identical(dim(a), dim(b))) stop("[OR.delim.subset] a and b must have the same shape")
  }
  output <- mapply(.OR.delim.subset, a, b, delimiter, USE.NAMES = FALSE)
  if (is.matrix(a)) dim(output) <- dim(a) else output <- unname(output)
  return(output)
}

#' Frequency table of delimited string elements (case-insensitive and
#' whitespace-trimmed)
#'
#' Splits each string in a string vector, matrix, or data frame by a specified
#' delimiter, trims whitespace, converts to lowercase, and tabulates the
#' frequency of all unique delimited elements.
#' @param x String vector, matrix, or data frame containing delimited strings.
#' @param delimiter String delimiter. Default = `","`.
#' @return A table of delimited element frequencies.
#' @examples
#' treatment <- c("capecitabine",
#'                "LETROZOLE",
#'                "letrozole, palbociclib",
#'                "Letrozole,Ribociclib",
#'                "anastrozole, ribociclib")
#' print(OR.delim.table(treatment))
#' @family delim
#' @export
OR.delim.table <- function(x, delimiter = ",") {
  if (is.data.frame(x)) x <- as.matrix(x)
  if (is.matrix(x)) x <- as.vector(x)
  return(table(tolower(trimws(unlist(strsplit(x, delimiter, fixed = TRUE)))), useNA = "ifany"))
}

#' Classify common medical oncology treatments (work in progress)
#'
#' For each string in `x`, applies `OR.delim.split()` and replaces common cancer
#' treatments with predefined treatment class ("endocrine", "chemotherapy",
#' "immunotherapy", or "targeted") and selected subclass (such as "her2 and
#' "pd1") labels. Multiple labels, together with any unclassified elements, are
#' returned as a delimited string.
#' @param x String vector of delimited treatments.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector of delimited treatment class labels.
#' @family delim
#' @export
OR.delim.txclass <- function(x, delimiter = ",") {
  x <- OR.delim.txname(x, delimiter)
  classes <- list()
  classes[["endocrine"]] <- c("tamoxifen")
  classes[["endocrine, androgen receptor"]] <- c("abiraterone", "apalutamide", "darolutamide", "enzalutamide")
  classes[["endocrine, aromatase"]] <- c("anastrozole", "exemestane", "letrozole")
  classes[["endocrine, gnrh"]] <- c("goserelin", "leuprorelin")
  classes[["endocrine, serd"]] <- c("fulvestrant", "vepdegestrant")
  classes[["chemotherapy"]] <- c("5fu", "gemcitabine", "methotrexate", "pemetrexed", "raltitrexed", "trifluridine") # antimetabolite
  classes[["chemotherapy"]] <- c("bleomycin", "mitomycin", "temozolomide")
  classes[["chemotherapy"]] <- c("cyclophosphamide", "lomustine") # alkylating
  classes[["chemotherapy"]] <- c("eribulin")
  classes[["chemotherapy"]] <- c("etoposide", "irinotecan") # topoisomerase
  classes[["chemotherapy"]] <- c("vinorelbine") # vinca
  classes[["chemotherapy, anthracycline"]] <- c("doxorubicin", "epirubicin", "liposomal doxorubicin")
  classes[["chemotherapy, platinum"]] <- c("carboplatin", "cisplatin", "oxaliplatin")
  classes[["chemotherapy, taxane"]] <- c("cabazitaxel", "docetaxel", "nab-paclitaxel", "paclitaxel")
  classes[["chemotherapy, targeted, her2"]] <- c("trastuzumab emtansine")
  classes[["immunotherapy, ctla4"]] <- c("ipilimumab")
  classes[["immunotherapy, lag3"]] <- c("relatlimab")
  classes[["immunotherapy, pd1"]] <- c("cemiplimab", "nivolumab", "pembrolizumab")
  classes[["immunotherapy, pdl1"]] <- c("atezolizumab", "durvalumab")
  classes[["targeted"]] <- c("axitinib", "cabozantinib", "lenvatinib", "pazopanib", "regorafenib", "sorafenib", "sunitinib")
  classes[["targeted"]] <- c("bevacizumab")
  classes[["targeted"]] <- c("capivasertib", "ipatasertib")
  classes[["targeted, braf"]] <- c("dabrafenib", "encorafenib", "vemurafenib")
  classes[["targeted, cdk46"]] <- c("abemaciclib", "palbociclib", "ribociclib")
  classes[["targeted, egfr"]] <- c("cetuximab", "panitumumab")
  classes[["targeted, egfr"]] <- c("erlotinib", "gefitinib", "osimertinib")
  classes[["targeted, her2"]] <- c("pertuzumab", "trastuzumab")
  classes[["targeted, mtor"]] <- c("everolimus")
  classes[["targeted, parp"]] <- c("olaparib")
  classes[["targeted, pi3k"]] <- c("alpelisib")
  names <- names(classes)
  for (i in 1:length(names)) {
    matches <- classes[[i]]
    for (match in matches) {
      x <- OR.delim.replace(x, match, names[i], delimiter)
    }
  }
  return(x)
}

#' Standardize common medical oncology treatment names (work in progress)
#'
#' For each string in `x`, standardizes common cancer treatment names:
#' 1. Removes administration routes (such as "iv" and "oral"), scheduling terms
#' (such as "q2w" and "weekly"), and other modifiers (such as "modified" and
#' "single agent").
#' 2. Converts common separators ("&", "+", ",", "/", " and ", " with ") to a
#' consistent delimiter.
#' 3. Removes supportive agents (such as "folinic acid").
#' 4. Replaces common trade names,  abbreviations (such as "FOLFOX"), and
#' synonyms (such as "T-DM1") with standardized names.
#' @param x String vector of delimited treatments.
#' @param delimiter String delimiter. Default = `","`.
#' @return A string vector of delimited standarized treatment names.
#' @family delim
#' @export
OR.delim.txname <- function(x, delimiter = ",") {
  x <- gsub(" alone", "", x, fixed = TRUE)
  x <- gsub("bolus ", "", x, fixed = TRUE)
  x <- gsub(" bolus", "", x, fixed = TRUE)
  x <- gsub("bolus/infusion ", "", x, fixed = TRUE)
  x <- gsub(" bolus/infusion", "", x, fixed = TRUE)
  x <- gsub("infusion ", "", x, fixed = TRUE)
  x <- gsub(" infusion", "", x, fixed = TRUE)
  x <- gsub("iv ", "", x, fixed = TRUE)
  x <- gsub(" iv", "", x, fixed = TRUE)
  x <- gsub("modified ", "", x, fixed = TRUE)
  x <- gsub("(modified)", "", x, fixed = TRUE)
  x <- gsub(" modified", "", x, fixed = TRUE)
  x <- gsub(" only", "", x, fixed = TRUE)
  x <- gsub("oral ", "", x, fixed = TRUE)
  x <- gsub(" oral", "", x, fixed = TRUE)
  x <- gsub("q2w ", "", x, fixed = TRUE)
  x <- gsub(" q2w", "", x, fixed = TRUE)
  x <- gsub("q3w ", "", x, fixed = TRUE)
  x <- gsub(" q3w", "", x, fixed = TRUE)
  x <- gsub("q4w ", "", x, fixed = TRUE)
  x <- gsub(" q4w", "", x, fixed = TRUE)
  x <- gsub("q6w ", "", x, fixed = TRUE)
  x <- gsub(" q6w", "", x, fixed = TRUE)
  x <- gsub("single agent ", "", x, fixed = TRUE)
  x <- gsub(" single agent", "", x, fixed = TRUE)
  x <- gsub("weekly ", "", x, fixed = TRUE)
  x <- gsub(" weekly", "", x, fixed = TRUE)

  x <- OR.delim.replace(x, "lonsurf (trifluridine/tipiracil)", "trifluridine", delimiter)
  x <- OR.delim.replace(x, "trifluridine/tipiracil (lonsurf)", "trifluridine", delimiter)

  x <- gsub("&", delimiter, x, fixed = TRUE)
  x <- gsub("+", delimiter, x, fixed = TRUE)
  x <- gsub(",", delimiter, x, fixed = TRUE)
  x <- gsub("/", delimiter, x, fixed = TRUE)
  x <- gsub(" and ", delimiter, x, fixed = TRUE)
  x <- gsub(" with ", delimiter, x, fixed = TRUE)

  x <- OR.delim.replace(x, "folinic acid", "", delimiter)
  x <- OR.delim.replace(x, "leucovorin", "", delimiter)
  x <- OR.delim.replace(x, "tipiracil", "", delimiter)

  x <- OR.delim.replace(x, "abraxane", "nab-paclitaxel", delimiter)
  x <- OR.delim.replace(x, "ac", "doxorubicin, cyclophosphamide", delimiter)
  x <- OR.delim.replace(x, "anastrazole", "anastrozole", delimiter)
  x <- OR.delim.replace(x, "arv-471", "vepdegestrant", delimiter)
  x <- OR.delim.replace(x, "capecitabine", "5fu", delimiter)
  x <- OR.delim.replace(x, "capiri", "5fu, irinotecan", delimiter)
  x <- OR.delim.replace(x, "capox", "5fu, oxaliplatin", delimiter)
  x <- OR.delim.replace(x, "degramont", "5fu", delimiter)
  x <- OR.delim.replace(x, "de gramont", "5fu", delimiter)
  x <- OR.delim.replace(x, "fec", "5fu, epirubicin, cyclophosphamide", delimiter)
  x <- OR.delim.replace(x, "folfiri", "5fu, irinotecan", delimiter)
  x <- OR.delim.replace(x, "folfirinox", "5fu, irinotecan, oxaliplatin", delimiter)
  x <- OR.delim.replace(x, "folfox", "5fu, oxaliplatin", delimiter)
  x <- OR.delim.replace(x, "folfoxiri", "5fu, irinotecan, oxaliplatin", delimiter)
  x <- OR.delim.replace(x, "fufa", "5fu", delimiter)
  x <- OR.delim.replace(x, "ifl", "5fu, irinotecan", delimiter)
  x <- OR.delim.replace(x, "ionsurf", "trifluridine", delimiter)
  x <- OR.delim.replace(x, "lonsurf", "trifluridine", delimiter)
  x <- OR.delim.replace(x, "lonsurf (fluoropyrimidine)", "trifluridine", delimiter)
  x <- OR.delim.replace(x, "lv5fu2", "5fu", delimiter)
  x <- OR.delim.replace(x, "lvfu2", "5fu", delimiter)
  x <- OR.delim.replace(x, "mitomycin c", "mitomycin", delimiter)
  x <- OR.delim.replace(x, "mmc", "mitomycin", delimiter)
  x <- OR.delim.replace(x, "navelbine", "vinorelbine", delimiter)
  x <- OR.delim.replace(x, "tas102", "trifluridine", delimiter)
  x <- OR.delim.replace(x, "tas-102", "trifluridine", delimiter)
  x <- OR.delim.replace(x, "tdm1", "trastuzumab emtansine", delimiter)
  x <- OR.delim.replace(x, "t-dm1", "trastuzumab emtansine", delimiter)
  x <- OR.delim.replace(x, "tc", "docetaxel, cyclophosphamide", delimiter)
  x <- OR.delim.replace(x, "xeliri", "5fu, irinotecan", delimiter)
  x <- OR.delim.replace(x, "xeloda", "5fu", delimiter)
  x <- OR.delim.replace(x, "xelox", "5fu, oxaliplatin", delimiter)
  return(x)
}
