#' Clean parsed profile scrapes
#'
#' Given a parsed profile scrapes, containing columns `marketplace`, `date`,
#' `id`, `profile`, remove rows with missing information, or duplicated rows,
#' match to user database and remove non-matches, and extract any available PGP
#' keys.
#'
#' @param parsedUsers name of dataframe that contains parsed profile scrapes, as
#'   described above.
#' @param users name of dataframe with unique user accounts, with columns
#'   `hash_str`, `marketplace`, `id`, `diversity`
#' @return 1. `parsedUsers` dataframe with `profile` column replaced by
#'   `profileClean`, which has any PGP keys extracted. 2. `retrievedPGPs`
#'   dataframe with columns `date`, `vendor_hash` and `PGPclean` (extracted PGP
#'   keys)
#' @export

cleanParsedUsers <- function(parsedUsers, users) {

    parsedUsers <- parsedUsers[parsedUsers$profile != "" & parsedUsers$id != "", ]
    parsedUsers <- parsedUsers[duplicated(parsedUsers) == FALSE, ]

    users$marketplaceID <- paste0(users$marketplace, users$id)
    parsedUsers$marketplaceID <- paste0(parsedUsers$marketplace, parsedUsers$id)
    parsedUsers <- parsedUsers[parsedUsers$marketplaceID %in% users$marketplaceID, ] # 163706 rows
    parsedUsers$vendor_hash <- users$hash_str[match(parsedUsers$marketplaceID, users$marketplaceID)]

    parsedUsers$profileClean <- NA
    retrievedPGPs <- data.frame(date = numeric(), vendor_hash = character(), PGPclean = character(), stringsAsFactors = FALSE)

    # extract PGP and then check for duplicates again
    for (i in 1:nrow(parsedUsers)) {
        out <- extractPGPs(parsedUsers[i, "profile"])
        parsedUsers$profileClean[i] <- out$outputString
        tmpLength <- length(out$retrievedPGPs)
        if (tmpLength > 0) { # out$retrievedPGPs is a vector
            tmp <- data.frame(date = rep(parsedUsers[i, "date"], tmpLength), vendor_hash = rep(parsedUsers[i, "vendor_hash"], tmpLength), PGPclean = out$retrievedPGPs, stringsAsFactors = FALSE)
            retrievedPGPs <- rbind(retrievedPGPs, tmp)
        }
    }

    parsedUsers <- parsedUsers[duplicated(parsedUsers[, c("date", "vendor_hash", "profileClean")]) == FALSE, c("date", "vendor_hash", "profileClean")]
    noInfoVec <- sapply(parsedUsers$profileClean, FUN = noInfo)
    parsedUsers <- parsedUsers[noInfoVec == FALSE, ]

    retrievedPGPs <- retrievedPGPs[duplicated(retrievedPGPs) == FALSE, ] # -- this would be when they have different profile information on the same day but PGP key is the same on both

    ret <- list(retrievedPGPs = retrievedPGPs, parsedUsers = parsedUsers)
    return(ret)
}


#' Extract any PGP keys from an input string
#'
#' Given an input string, extract any PGP keys (any string starting with
#' -----BEGIN PGP PUBLIC KEY BLOCK----- and ending with -----END PGP PUBLIC KEY
#' BLOCK). Note that trailing dashes are not currently removed.
#'
#' @param inputString string to search for PGP keys
#' @return 1. `outputString` input string removing PGP keys 2. `retrievedPGPs`
#'   vector of extracted PGP keys (header, footer, whitespace etc. removed).
#'   Will have more than one element if there is more than 1 unique PGP key in
#'   the input string
#' @export

extractPGPs <- function(inputString) {

    retrievedPGPs <- c()
    pat <- "-*?BEGIN PGP PUBLIC KEY BLOCK.*?END PGP PUBLIC KEY BLOCK-*?"
    PGP <- regmatches(inputString, regexpr(pat, inputString)) # if no match, character(0) # gets the first match.
    outputString <- inputString
    while (length(PGP) != 0) {
        firstPosition <- regexpr("mQ|xs|mI|xk|mE|xm|xo", PGP)
        outputString <- gsub(PGP, "", outputString, fixed = TRUE) # remove PGP -- if PGP is duplicated exactly it will automatically be removed
        if (firstPosition == -1) { # invalid PGP
            warning(paste0("Invalid PGP ", PGP, "\n"))
        } else {
            PGP <- substr(PGP, firstPosition, nchar(PGP))
            PGP <- gsub("\n", " ", PGP, fixed = TRUE)
            PGP <- gsub("-----END PGP PUBLIC KEY BLOCK", "", PGP)
            PGP <- gsub(" ", "", PGP)
            PGP <- gsub("\\s+", replacement = "", PGP) # remove whitespace
            retrievedPGPs <- c(retrievedPGPs, PGP)
        }
        PGP <- regmatches(outputString, regexpr(pat, outputString))
    }
    ret <- list(outputString = outputString, retrievedPGPs = retrievedPGPs)
    return(ret)
}

#' Check if an input string has useful information
#'
#' If an input string has only whitespace, \\n or "-", return TRUE and
#' otherwise, return FALSE.
#'
#' @param inputString string to check
#' @return logical, TRUE if no useful information.
#' @export

noInfo <- function(inputString) {
    inputString <- gsub("\\s+", "", inputString)
    inputString <- gsub("\n", "", inputString, fixed = TRUE)
    inputString <- gsub("-", "", inputString, fixed = TRUE)
    # should check punctuation too? this is removed later
    if (nchar(inputString) == 0) {
        return(TRUE)
    } else return(FALSE)
}


#' Clean parsed item scrapes
#'
#' Given a parsed item scrapes, containing columns `marketplace`, `date`,
#' `seller_id`, `title`, `listing_description`, remove rows with missing
#' information, or duplicated rows, match to items database and remove
#' non-matches, and extract any available PGP keys. This is analogous to
#' `cleanParsedUsers()`.
#'
#' @param parsedItems name of dataframe that contains parsed item listing
#'   scrapes, as described above.
#' @param items name of dataframe with unique item listings, with at least
#'   columns `hash_str`, `marketplace`, `title`, `vendor`
#' @return 1. `parsedUsers` dataframe with `listing_description` column replaced
#'   by `descriptionClean`, which has any PGP keys extracted. 2. `retrievedPGPs`
#'   dataframe with columns `date`, `vendor_hash` and `PGPclean` (extracted PGP
#'   keys)
#' @export

cleanParsedItems <- function(parsedItems, items) {

    parsedItems <- parsedItems[parsedItems$listing_description != "" & parsedItems$seller_id != "" & parsedItems$title != "", ]
    parsedItems <- parsedItems[duplicated(parsedItems) == FALSE, ]

    items$marketplaceIDtitle <- paste0(items$marketplace, items$vendor, items$title)

    parsedItems$marketplaceIDtitle <- paste0(parsedItems$marketplace, parsedItems$seller_id, parsedItems$title)

    tmp <- match(parsedItems$marketplaceIDtitle, items$marketplaceIDtitle)
    parsedItems$item_hash <- items$hash_str[tmp]
    parsedItems$vendor_hash <- items$vendor_hash[tmp]

    parsedItems <- parsedItems[!is.na(parsedItems$item_hash), c("marketplace", "date", "listing_description", "item_hash", "vendor_hash")]

    parsedItems$descriptionClean <- NA
    retrievedPGPs <- data.frame(date = numeric(), vendor_hash = character(), PGPclean = character(), stringsAsFactors = FALSE)

    # extract PGP and then check for duplicates again
    for (i in 1:nrow(parsedItems)) {
        out <- extractPGPs(parsedItems[i, "listing_description"])
        parsedItems$descriptionClean[i] <- out$outputString
        tmpLength <- length(out$retrievedPGPs)
        if (tmpLength > 0) { # out$retrievedPGPs is a vector
            tmp <- data.frame(date = rep(parsedItems[i, "date"], tmpLength), vendor_hash = rep(parsedItems[i, "vendor_hash"], tmpLength), PGPclean = out$retrievedPGPs, stringsAsFactors = FALSE)
            retrievedPGPs <- rbind(retrievedPGPs, tmp)
        }
    }

    parsedItems <- parsedItems[duplicated(parsedItems[, c("date", "item_hash", "vendor_hash", "descriptionClean")]) == FALSE, c("date", "item_hash", "vendor_hash", "descriptionClean")]
    noInfoVec <- sapply(parsedItems$descriptionClean, FUN = noInfo)
    parsedItems <- parsedItems[noInfoVec == FALSE, ]

    retrievedPGPs <- retrievedPGPs[duplicated(retrievedPGPs) == FALSE, ] # -- this would be when they have different profile information on the same day but PGP key is the same on both

    ret <- list(retrievedPGPs = retrievedPGPs, parsedItems = parsedItems)
    return(ret)
}


