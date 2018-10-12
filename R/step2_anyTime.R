#' Get account-level information from feedback data for some timeframe of
#' interest
#'
#' Feedback data refers to unique feedback left on the marketplaces. Feedback
#' needs to contain the following variables:
#' \itemize{
#'     \item{`hash_str`: this is a unique identifier for each feedback}
#'     \item{`date`: date that the feedback was left}
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`item_hash`: unique identifier for item that feedback was left for}
#'     \item{`marketplace`: marketplace that the feedback was left on}
#'     \item{`order_amount_usd`: cost of item that feedback was left for}
#' }
#'
#' Account-level variables returned are:
#' \itemize{
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`marketplace`: marketplace that the feedback was left on}
#'     \item{`totalFeedback`: total number of feedback that the account received}
#'     \item{`numListingsWithFeedback`: number of item listings where account received feedback}
#'     \item{`firstFeedback`: date of first feedback (character)}
#'     \item{`lastFeedback`: date of last feedback (character)}
#'     \item{`daysActive`: number of days between first and last feedback (inclusive)}
#'     \item{date columns: binary variables indicating whether feedback was received on these days}
#'     \item{`dailyFrac`: (totalFeedback/daysActive for vendor) / (marketplaceTotalFeedback/marketplaceDaysActive)}
#'     \item{`meanPriceSold`}
#'     \item{`medianPriceSold`}
#'     \item{`minPriceSold`}
#'     \item{`maxPriceSold`}
#'     \item{`priceRange`}
#'
#' }

#' @param feedback dataframe as described above
#' @param startDate string for start of time frame of interest, e.g. "2014-01-10"
#' @param endDate string for end of time frame of interest (exclusive), e.g.
#'   "2014-01-15" means up to 1/14
#' @return list containing 1. dataframe with account-level variables as listed
#'   above, 2. unique vendor_hashes of interest (receiving feedback in period of
#'   interest), 3. unique item_hashes of interest
#'
#' @examples
#' \dontrun{
#'     out <- infoFromFeedback(feedback, "2014-08-01", "2014-08-31")
#' }
#' @export
#' @importFrom magrittr %>%
#'

infoFromFeedback <- function(feedback, startDate, endDate) {

    feedbackSubset <- feedback[feedback$date >= startDate & feedback$date < endDate, ]

    out <- feedbackSubset %>%
        dplyr::group_by(vendor_hash) %>%
            dplyr::summarize(totalFeedback = length(hash_str),

                      numListingsWithFeedback = length(unique(item_hash)),

                      firstFeedback = min(date),
                      lastFeedback = max(date),
                      daysActive = as.numeric(as.Date(max(date)) - as.Date(min(date))) + 1,

                      meanPriceSold = mean(order_amount_usd),
                      medianPriceSold = median(order_amount_usd),
                      minPriceSold = min(order_amount_usd),
                      maxPriceSold = max(order_amount_usd),
                      priceRange = max(order_amount_usd)- min(order_amount_usd),

                      marketplace = dplyr::first(marketplace) # all entries should have the same marketplace
                      )

    ###### dailyFrac
    # (note that there is a slight discrepancy: for vendors daysActive counted all days between first and last)

    marketplaceOut <- feedbackSubset %>%
        dplyr::group_by(marketplace) %>%
            dplyr::summarize(marketplaceTotalFeedback = length(hash_str),
                      marketplaceDaysActive = length(unique(date))
            )
    marketplaceOut$scale <- marketplaceOut$marketplaceTotalFeedback/marketplaceOut$marketplaceDaysActive
    out <- dplyr::left_join(out, marketplaceOut[, c("marketplace", "scale")], by = "marketplace")

    out$dailyFrac <- (out$totalFeedback/out$daysActive) / out$scale
    out <- out[, -(ncol(out) - 1)] # remove scale

    ###### days with sales
    # create binary vector
    allDates <- unique(feedbackSubset$date)
    allDates <- sort(allDates)

    out[, allDates] <- NA
    for (i in 1:nrow(out)) {
        tmp <- unique(feedbackSubset$date[feedbackSubset$vendor_hash == out$vendor_hash[i]])
        out[i, allDates] <- as.numeric(allDates %in% tmp)
    }

    ret <- list(out = out, vendorHashes = out$vendor_hash, itemHashes = unique(feedbackSubset$item_hash))

    return(ret)

}



#' Takes subset of users data for some timeframe of interest
#'
#' Users data refers to data on unique user accounts. users
#' needs to contain the following variables:
#' \itemize{
#'     \item{`hash_str`: this is a unique identifier for each user account}
#'     \item{`marketplace`: marketplace that the feedback was left on}
#'     \item{`id`: alias used on marketplace}
#'     \item{`diversity`: diversity coefficient, see Soska and Christin (2015)}
#' }
#'
#' @param users dataframe as described above
#' @param userHashesOfInterest vendor_hashes for accounts that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @return dataframe with `vendor_hash`, `id`, `diversity`
#'
#' @examples
#' \dontrun{
#'     usersOut <- infoFromUsers(users, out$vendorHashes)
#' }
#' @export
#'

infoFromUsers <- function(users, userHashesOfInterest) {
    out <- users[users$hash_str %in% userHashesOfInterest, c("hash_str", "id", "diversity")]
    return(out)
}


#' Get account-level information from items data for some timeframe of interest
#'
#' Items data refers to data on unique items listed. Items needs to contain the
#' following variables:
#' \itemize{
#'     \item{`hash_str`: this is a unique identifier for each item listing}
#'     \item{`marketplace`: marketplace that the item was listed on}
#'     \item{`title`: title of item listing}
#'     \item{`vendor`: vendor account ID that listed the item}
#'     \item{`vendor_hash`: unique identifier for vendor, corresponds to `hash_str` in `users` and feedback}
#'     \item{`prediction`: category predicted for item listing, see Soska and Christin (2015)}
#'     \item{`dosage`: dosage extracted from item title}
#'     \item{`unit`: units extracted from item title}
#' }
#'
#' Account-level variables returned are:
#' \itemize{
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`numTitleTokens`: number of unique tokens from item titles}
#'     \item{`titleTokens`: list item}
#'     \item{`inventory`: list item}
#' }
#'
#' @param items dataframe as described above
#' @param itemHashesOfInterest item_hashes for items that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @param userHashesOfInterest vendor_hashes for accounts that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @return list containing 1. dataframe with vendor_hash and numTitleTokens, 2.
#'   `titleTokens` list, 3. `inventory` list
#'
#' @examples
#' \dontrun{
#'     itemsOut <- infoFromItems(items, out$itemHashes, out$vendorHashes)
#' }
#' @export
#'

infoFromItems <- function(items, itemHashesOfInterest, userHashesOfInterest) {

    itemsSubset <- items[items$hash_str %in% itemHashesOfInterest, ]

    titleTokens <- generateTitleTokens(itemsSubset, userHashesOfInterest)

    out <- data.frame(userHashesOfInterest = userHashesOfInterest, numTitleTokens = unlist(lapply(titleTokens, length), use.names = FALSE), stringsAsFactors = FALSE)

    inventory <- generateInventory(itemsSubset, userHashesOfInterest)

    ret <- list(out = out, titleTokens = titleTokens, inventory = inventory)
    return(ret)
}

# helper function: generates list of title tokens. Processing involved: change punctuation to whitespace, make everything lowercase, tokenize
generateTitleTokens <- function(itemsSubset, userHashesOfInterest) {
    titleTokens <- vector(mode = "list", length = length(userHashesOfInterest))
    for (i in 1:length(titleTokens)) {
        if (i %% 100 == 0) cat(i, ", ")

        tmp <- itemsSubset[which(itemsSubset$vendor_hash == userHashesOfInterest[i]), "title"]

        tmp <- gsub("[[:punct:]]", " ", tmp)
        tmp <- tolower(tmp)
        tmp <- unlist(strsplit(tmp, "\\s+"))
        tmp <- unique(tmp)

        tmp3 <- which(tmp == "")
        if (length(tmp3) > 0) {
            tmp <- tmp[-which(tmp == "")]
        }
        if (length(tmp) > 0) {
            titleTokens[[i]] <- tmp
        }
        names(titleTokens)[i] <- userHashesOfInterest[i]
    }
    return(titleTokens)

}

generateInventory <- function(itemsSubset, userHashesOfInterest) {
    inventory <- vector(mode = "list", length = length(userHashesOfInterest))
    for (i in 1:length(inventory)) {
        if (i %% 100 == 0) cat(i, ", ")

        tmp <- itemsSubset[itemsSubset$vendor_hash == userHashesOfInterest[i], c("prediction", "dosage", "unit")]

        inventory[[i]]$category <- unique(tmp$prediction)

        catDosage <- unique(tmp[!(is.na(tmp$prediction)) & !is.na(tmp$dosage), c("prediction", "dosage")])
        if (nrow(catDosage) > 0) {
            catDosage <- apply(catDosage, 1, function(x) paste(x, collapse = ""))
            catDosage <- gsub(" ", "", catDosage, fixed = TRUE)
            names(catDosage) <- NULL
            inventory[[i]]$catDosage <- catDosage
        }

        catUnit <- unique(tmp[!(is.na(tmp$prediction)) & !is.na(tmp$unit), c("prediction", "unit")])
        if (nrow(catUnit) > 0) {
            catUnit <- apply(catUnit, 1, function(x) paste(x, collapse = ""))
            catUnit <- gsub(" ", "", catUnit, fixed = TRUE)
            names(catUnit) <- NULL
            inventory[[i]]$catUnit <- catUnit
        }

        catDosageUnit <- unique(tmp[!is.na(tmp$prediction) & !is.na(tmp$dosage) & !is.na(tmp$unit), ])
        if (nrow(catDosageUnit) > 0) {
            catDosageUnit <- apply(catDosageUnit, 1, function(x) paste(x, collapse = ""))
            catDosageUnit <- gsub(" ", "", catDosageUnit, fixed = TRUE)
            names(catDosageUnit) <- NULL
            inventory[[i]]$catDosageUnit <- catDosageUnit
        }

        names(inventory)[i] <- userHashesOfInterest[i]
    }
    return(inventory)
}


#' Get account-level information from profile scrapes for some timeframe of
#' interest
#'
#' Profile scrapes data needs to contain the following variables:
#' \itemize{
#'     \item{`date`: date that scrape was done, in UNIX time}
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`profileClean`: scrape of profile page at that time}
#' }
#'
#' We first consider all scrapes within the start and end dates specified. If
#' there are no scrapes available, we look at equal time periods before and
#' after the dates of interest. For example, if we are interested in August
#' 2014, and no scrapes are available then, we consider all scrapes in
#' July-September 2014.
#'
#' Account-level variables returned are:
#' \itemize{
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`numProfileTokens`: number of unique tokens from profile scrapes}
#'     \item{`profileTokens`: list item}
#' }
#'
#' @param profileClean dataframe as described above
#' @param userHashesOfInterest vendor_hashes for accounts that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @param startDate string for start of time frame of interest, e.g. "2014-01-10"
#' @param endDate string for end of time frame of interest (exclusive), e.g.
#'   "2014-01-15" means up to 1/14
#' @return list containing 1. dataframe with vendor_hash and numProfileTokens, 2.
#'   `profileTokens` list
#'
#' @examples
#' \dontrun{
#'     profileCleanOut <- infoFromProfileClean(profileClean, out$vendorHashes,
#'     "2014-08-01", "2014-08-31")
#' }
#' @export
#'
infoFromProfileClean <- function(profileClean, userHashesOfInterest, startDate, endDate) {

    profileCleanSubset <- profileClean[profileClean$vendor_hash %in% userHashesOfInterest, ]
    profileTokens <- generateProfileTokens(profileCleanSubset, userHashesOfInterest, startDate, endDate)

    out <- data.frame(userHashesOfInterest = userHashesOfInterest, numProfileTokens = unlist(lapply(profileTokens, length), use.names = FALSE), stringsAsFactors = FALSE)

    ret <- list(out = out, profileTokens = profileTokens)
    return(ret)
}

# helper function: generates list of profile tokens. Processing involved: change punctuation and \n to whitespace, make everything lowercase, tokenize
generateProfileTokens <- function(profileCleanSubset, userHashesOfInterest, startDate, endDate) {
    # get any profile in period, if none, use same length of time before or after

    startUnix <- as.numeric(as.POSIXct(startDate, format = "%Y-%m-%d"))
    endUnix <- as.numeric(as.POSIXct(endDate, format = "%Y-%m-%d"))

    profileTokens <- vector(mode = "list", length = length(userHashesOfInterest))
    for (i in 1:length(profileTokens)) {
        if (i %% 100 == 0) cat(i, ", ")

        tmp <- profileCleanSubset[profileCleanSubset$vendor_hash == userHashesOfInterest[i], ]
        if (sum(tmp$date >= startUnix & tmp$date < endUnix) > 0) {
            tmp <- tmp[tmp$date >= startUnix & tmp$date < endUnix, ]
        } else {
            newStart <- startUnix - (endUnix - startUnix)
            newEnd <- endUnix + (endUnix - startUnix)
            tmp <- tmp[tmp$date >= newStart & tmp$date < newEnd, ]
        }

        names(profileTokens)[i] <- userHashesOfInterest[i]
        if (nrow(tmp) == 0) next

        profile <- paste(tmp$profileClean, collapse = " ")
        # if (is.null(tmp) || is.na(tmp) || length(tmp) == 0) next

        # now do the profiles
        profile <- gsub("\n", " ", profile, fixed = TRUE)
        profile <- gsub("[[:punct:]]", " ", profile) # replace punctuation with space
        profile <- tolower(profile)
        profile <- unlist(strsplit(profile, "\\s+")) # split on whitespace
        profile <- unique(profile)

        tmp3 <- which(profile == "")
        if (length(tmp3) > 0) {
            profile <- profile[-which(profile == "")]
        }
        if (length(profile) > 0) {
            profileTokens[[i]] <- profile
        }
    }
    return(profileTokens)
}


#' Get account-level information from item description scrapes for some
#' timeframe of interest
#'
#' Item scrapes data needs to contain the following variables:
#' \itemize{
#'     \item{`date`: date that scrape was done, in UNIX time}
#'     \item{`item_hash`: unique identifier for item listed}
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`descriptionClean`: scrape of item description page at that time}
#' }
#'
#' We first consider all scrapes within the start and end dates specified. If
#' there are no scrapes available, we look at equal time periods before and
#' after the dates of interest. For example, if we are interested in August
#' 2014, and no scrapes are available then, we consider all scrapes in
#' July-September 2014.
#'
#' Account-level variables returned are:
#' \itemize{
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`numDescriptionTokens`: number of unique tokens from item description scrapes}
#'     \item{`descriptionTokens`: list item}
#' }
#'
#' @param descriptionClean dataframe as described above
#' @param userHashesOfInterest vendor_hashes for accounts that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @param itemHashesOfInterest item_hashes for items that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @param startDate string for start of time frame of interest, e.g. "2014-01-10"
#' @param endDate string for end of time frame of interest (exclusive), e.g.
#'   "2014-01-15" means up to 1/14
#' @return list containing 1. dataframe with vendor_hash and
#'   numDescriptionTokens, 2. `descriptionTokens` list
#'
#' @examples
#' \dontrun{
#'     descriptionCleanOut <- infoFromDescriptionClean(descriptionClean,
#'     out$vendorHashes, out$itemHashes, "2014-08-01", "2014-08-31")
#' }
#' @export
#'

infoFromDescriptionClean <- function(descriptionClean, userHashesOfInterest, itemHashesOfInterest, startDate, endDate) {
    # get any description in period, if none, use same length of time before or after

    descriptionCleanSubset <- descriptionClean[descriptionClean$item_hash %in% itemHashesOfInterest, ]

    descriptionTokens <- generateDescriptionTokens(descriptionCleanSubset, userHashesOfInterest, startDate, endDate)
    out <- data.frame(userHashesOfInterest = userHashesOfInterest, numDescriptionTokens = unlist(lapply(descriptionTokens, length), use.names = FALSE), stringsAsFactors = FALSE)

    ret <- list(out = out, descriptionTokens = descriptionTokens)
    return(ret)

}

# helper function: generates list of description tokens. Processing involved: change punctuation and \n to whitespace, make everything lowercase, tokenize
generateDescriptionTokens <- function(descriptionCleanSubset, userHashesOfInterest, startDate, endDate) {
        startUnix <- as.numeric(as.POSIXct(startDate, format = "%Y-%m-%d"))
        endUnix <- as.numeric(as.POSIXct(endDate, format = "%Y-%m-%d"))

        descriptionTokens <- vector(mode = "list", length = length(userHashesOfInterest))

    for (i in 1:length(descriptionTokens)) {
        if (i %% 100 == 0) cat(i, ", ")

        tmp <- descriptionCleanSubset[descriptionCleanSubset$vendor_hash == userHashesOfInterest[i], ]
        if (sum(tmp$date >= startUnix & tmp$date < endUnix) > 0) {
            tmp <- tmp[tmp$date >= startUnix & tmp$date < endUnix, ]
        } else {
            newStart <- startUnix - (endUnix - startUnix)
            newEnd <- endUnix + (endUnix - startUnix)
            tmp <- tmp[tmp$date >= newStart & tmp$date < newEnd, ]
        }

        names(descriptionTokens)[i] <- userHashesOfInterest[i]
        if (nrow(tmp) == 0) next

        description <- paste(tmp$descriptionClean, collapse = " ")

        # if (is.null(tmp) || is.na(tmp) || length(tmp) == 0) next
        description <- gsub("\n", " ", description, fixed = TRUE)
        description <- gsub("[[:punct:]]", " ", description) # replace punctuation with space
        description <- tolower(description)
        description <- unlist(strsplit(description, "\\s+")) # split on whitespace
        description <- unique(description)

        tmp3 <- which(description == "")
        if (length(tmp3) > 0) {
            description <- description[-which(description == "")]
        }
        if (length(description) > 0) {
            descriptionTokens[[i]] <- description
        }
    }
    return(descriptionTokens)
}

#' Get list of PGP keys for each account in some timeframe of interest, from
#' scrapes of profiles and item descriptions
#'
#' PGP scrapes data are extracted from profile and item scrapes, and needs to
#' contain the following variables:
#' \itemize{
#'     \item{`date`: date that scrape was done, in UNIX time}
#'     \item{`vendor_hash`: unique identifier for seller account}
#'     \item{`PGPclean`: extracted PGP key}
#' }
#'
#' We first consider all scrapes within the start and end dates specified. If
#' there are no PGP keys available, we look at equal time periods before and
#' after the dates of interest. For example, if we are interested in August
#' 2014, and no keys are available then, we consider all scrapes in
#' July-September 2014.
#'
#' Returns `PGPlist`, a list of PGP keys for accounts that have feedback in the
#' timeframe of interest.
#'
#' @param PGPclean dataframe as described above
#' @param userHashesOfInterest vendor_hashes for accounts that have feedback in
#'   timeframe of interest; can be generated by `infoFromFeedback()`
#' @param startDate string for start of time frame of interest, e.g. "2014-01-10"
#' @param endDate string for end of time frame of interest (exclusive), e.g.
#'   "2014-01-15" means up to 1/14
#' @return `PGPlist` list item
#' @export
#'
generatePGPs <- function(PGPclean, userHashesOfInterest, startDate, endDate) {
    # get any PGP in period, if none, use same length of time before or after

    startUnix <- as.numeric(as.POSIXct(startDate, format = "%Y-%m-%d"))
    endUnix <- as.numeric(as.POSIXct(endDate, format = "%Y-%m-%d"))

    # can subset PGPlist first (using %in% userHashesOfInterest)

    PGPlist <- vector(mode = "list", length = length(userHashesOfInterest))
    for (i in 1:length(PGPlist)) {
        if (i %% 100 == 0) cat(i, ", ")

        tmp <- PGPclean[PGPclean$vendor_hash == userHashesOfInterest[i], ]
        if (sum(tmp$date >= startUnix & tmp$date < endUnix) > 0) {
            tmp <- tmp[tmp$date >= startUnix & tmp$date < endUnix, ]
        } else {
            newStart <- startUnix - (endUnix - startUnix)
            newEnd <- endUnix + (endUnix - startUnix)
            tmp <- tmp[tmp$date >= newStart & tmp$date < newEnd, ]
        }

        names(PGPlist)[i] <- userHashesOfInterest[i]
        if (nrow(tmp) == 0) next

        PGPlist[[i]] <- unique(tmp$PGPclean)
    }
    return(PGPlist)
}






