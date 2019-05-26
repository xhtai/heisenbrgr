#' Run all of step 1
#'
#' @param feedback name of dataframe with feedback data
#' @param items name of dataframe with unique item listings
#' @param users name of dataframe with unique user accounts
#' @param parsedUsers name of dataframe with raw profile scrapes
#' @param parsedItems name of dataframe with raw item scrapes
#' @return feedback, items, users, profileClean, descriptionClean, PGPclean
#' @export

runStep1 <- function(feedback, items, users, parsedUsers, parsedItems) {
    feedback <- cleanFeedback(feedback, items)
    items <- cleanItems(items, feedback)
    users <- cleanUsers(users, feedback)

    out <- cleanParsedUsers(parsedUsers, users)

    outItems <- cleanParsedItems(parsedItems, items)

    PGPclean <- rbind(out$retrievedPGPs, outItems$retrievedPGPs)
    PGPclean <- PGPclean[duplicated(PGPclean) == FALSE, ]

    profileClean <- out$parsedUsers
    descriptionClean <- outItems$parsedItems

    ret <- list(feedback = feedback, items = items, users = users, profileClean = profileClean, descriptionClean = descriptionClean, PGPclean = PGPclean)
    return(ret)
}

#' Run all of step 2 to get account-level information
#'
#' @param feedback processed feedback data
#' @param items processed items data
#' @param users processed users data
#' @param profileClean profile information from profile scrapes
#' @param descriptionClean item descriptions from item scrapes
#' @param PGPclean PGP keys from profile and item scrapes
#' @param startDate start of the period to be considered, in format "YYYY-MM-DD"
#' @param endDate end of the period to be considered
#' @return final, profileTokens, titleTokens, descriptionTokens, inventory, PGPlist
#' @export

runStep2 <- function(feedback, items, users, profileClean, descriptionClean, PGPclean, startDate, endDate) {
    out <- infoFromFeedback(feedback, startDate, endDate)
    usersOut <- infoFromUsers(users, out$vendorHashes)
    itemsOut <- infoFromItems(items, out$itemHashes, out$vendorHashes)
    profileCleanOut <- infoFromProfileClean(profileClean, out$vendorHashes, startDate, endDate)
    descriptionCleanOut <- infoFromDescriptionClean(descriptionClean, out$vendorHashes, out$itemHashes, startDate, endDate)
    PGPout <- generatePGPs(PGPclean, out$vendorHashes, startDate, endDate)

    usersOut <- usersOut[match(out$vendorHashes, usersOut$hash_str), ]
    final <- cbind(out$out, usersOut[, 2:3], numTitleTokens = itemsOut$out[, 2], numProfileTokens = profileCleanOut$out[, 2], numDescriptionTokens = descriptionCleanOut$out[, 2])

    ret <- list(final = final, titleTokens = itemsOut$titleTokens, inventory = itemsOut$inventory, profileTokens = profileCleanOut$profileTokens, descriptionTokens = descriptionCleanOut$descriptionTokens, PGPlist = PGPout)
    return(ret)

}


#' Run all of step 3 to get pairwise comparisons
#'
#' Note that this function cannot be used if you do not have the complete set of
#' account information. If so, step 3 will have to be done manually, removing
#' lines that there is no information on
#'
#' @param final dataframe with account-level information
#' @param profileTokens list item
#' @param titleTokens list item
#' @param descriptionTokens list item
#' @param inventory list item
#' @param PGPlist list item
#' @return allPairwise
#' @export

runStep3 <- function(final, profileTokens, titleTokens, descriptionTokens, inventory, PGPlist) {
    allPairwise <- getPairs(final)
    profileJaccard <- fromList(allPairwise, profileTokens, jaccardSimilarity)
    titleJaccard <- fromList(allPairwise, titleTokens, jaccardSimilarity)
    descriptionJaccard <- fromList(allPairwise, descriptionTokens, jaccardSimilarity)

    catJaccard <- fromList(allPairwise, lapply(inventory, function(x) x$category), jaccardSimilarity)
    catDosageJaccard <- fromList(allPairwise, lapply(inventory, function(x) x$catDosage), jaccardSimilarity)
    catUnitJaccard <- fromList(allPairwise, lapply(inventory, function(x) x$catUnit), jaccardSimilarity)
    catDosageUnitJaccard <- fromList(allPairwise, lapply(inventory, function(x) x$catDosageUnit), jaccardSimilarity)

    PGPmatched <- fromList(allPairwise, PGPlist, PGPmatch)

    absDifferences <- absFromDataframe(allPairwise, final, c("numListingsWithFeedback", "totalFeedback", "dailyFrac", "daysActive", "diversity", "meanPriceSold", "medianPriceSold", "minPriceSold", "maxPriceSold", "priceRange", "numDescriptionTokens", "numTitleTokens", "numProfileTokens"), c(rep(0, 10), rep(1, 3)))

    colNums <- which(substr(names(final), 1, 2) == "20")
    salesDiffs <- fromSalesDays(allPairwise, final[, colNums])

    allPairwise <- cbind(allPairwise, profileJaccard, titleJaccard, descriptionJaccard, catJaccard, catDosageJaccard, catUnitJaccard, catDosageUnitJaccard, PGPmatched, absDifferences, salesDiffs)

    return(allPairwise)
}


#' Run all of step 1 (excluding extracting inventory)
#'
#' @param feedback name of dataframe with feedback data
#' @param items name of dataframe with unique item listings
#' @param users name of dataframe with unique user accounts
#' @param parsedUsers name of dataframe with raw profile scrapes
#' @param parsedItems name of dataframe with raw item scrapes
#' @return feedback, items, users, profileClean, descriptionClean, PGPclean
#' @export

runStep1_2 <- function(feedback, items, users, parsedUsers, parsedItems) {
    feedback <- cleanFeedback(feedback, items)
    items <- cleanItems2(items, feedback)
    users <- cleanUsers(users, feedback)

    out <- cleanParsedUsers(parsedUsers, users)

    outItems <- cleanParsedItems(parsedItems, items)

    PGPclean <- rbind(out$retrievedPGPs, outItems$retrievedPGPs)
    PGPclean <- PGPclean[duplicated(PGPclean) == FALSE, ]

    profileClean <- out$parsedUsers
    descriptionClean <- outItems$parsedItems

    ret <- list(feedback = feedback, items = items, users = users, profileClean = profileClean, descriptionClean = descriptionClean, PGPclean = PGPclean)
    return(ret)
}
