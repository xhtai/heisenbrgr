######################## ADD TO STEP 2 ####################
### i.e. add to account-level information using users$profile and items$description
# these are for when there are no parsed databases to get profile and descriptions, and the single entry in users$profile and items$description for each user account and item listing is a random one from original, unavailable parsed databases

# outStep2$final, outStep2$profileTokens, outStep2$titleTokens, outStep2$descriptionTokens, outStep2$inventory, outStep2$PGPlist
# to update: outStep2$final, outStep2$profileTokens, outStep2$descriptionTokens, outStep2$PGPlist

### first process original users --- note that some hashes might not be in outStep1$users because not in feedback, but doesn't matter for now
# get users$hash_str and users$profile
cleanProfile <- function(users) {

    users$profileClean <- NA
    retrievedPGPs <- data.frame(vendor_hash = character(), PGPclean = character(), stringsAsFactors = FALSE)

    # extract PGP and then check for duplicates again
    for (i in 1:nrow(users)) {
        out <- extractPGPs(users[i, "profile"])
        users$profileClean[i] <- out$outputString
        tmpLength <- length(out$retrievedPGPs)
        if (tmpLength > 0) { # out$retrievedPGPs is a vector
            tmp <- data.frame(vendor_hash = rep(users[i, "hash_str"], tmpLength), PGPclean = out$retrievedPGPs, stringsAsFactors = FALSE)
            retrievedPGPs <- rbind(retrievedPGPs, tmp)
        }
    }

    noInfoVec <- sapply(users$profileClean, FUN = noInfo)
    users$profileClean[noInfoVec == TRUE] <- ""

    retrievedPGPs <- retrievedPGPs[duplicated(retrievedPGPs) == FALSE, ] # repeated PGP in same profile

    ret <- list(retrievedPGPs = retrievedPGPs, users = users)
    return(ret)
}

# helper function: add to list of profile tokens. Processing involved: change punctuation and \n to whitespace, make everything lowercase, tokenize
profileFromUsers <- function(users, profileTokens) {
    # get any profile in period, if none, use same length of time before or after

    for (i in 1:length(profileTokens)) {
        if (i %% 100 == 0) cat(i, ", ")

        profile <- users$profileClean[users$hash_str == names(profileTokens)[i]]

        if (length(profile) == 0) next ## check format of users$profile if there's no info

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
            profileTokens[[i]] <- unique(c(profileTokens[[i]], profile))
        }
    }
    return(profileTokens)
}

### now process original items --- note that some hashes might not be in outStep1$items because not in feedback, but doesn't matter for now
cleanDescriptions <- function(items) {

    items$descriptionClean <- NA
    retrievedPGPs <- data.frame(vendor_hash = character(), PGPclean = character(), stringsAsFactors = FALSE)

    # extract PGP and then check for duplicates again
    for (i in 1:nrow(items)) {
        out <- extractPGPs(items[i, "description"])
        items$descriptionClean[i] <- out$outputString
        tmpLength <- length(out$retrievedPGPs)
        if (tmpLength > 0) { # out$retrievedPGPs is a vector
            tmp <- data.frame(vendor_hash = rep(items[i, "vendor_hash"], tmpLength), PGPclean = out$retrievedPGPs, stringsAsFactors = FALSE)
            retrievedPGPs <- rbind(retrievedPGPs, tmp)
        }
    }

    noInfoVec <- sapply(items$descriptionClean, FUN = noInfo)
    items$descriptionClean[noInfoVec == TRUE] <- ""

    retrievedPGPs <- retrievedPGPs[duplicated(retrievedPGPs) == FALSE, ]

    ret <- list(retrievedPGPs = retrievedPGPs, items = items)
    return(ret)
}

# helper function: adds to list of description tokens. Processing involved: change punctuation and \n to whitespace, make everything lowercase, tokenize
descriptionFromItems <- function(items, descriptionTokens) {

    for (i in 1:length(descriptionTokens)) {
        if (i %% 100 == 0) cat(i, ", ")

        description <- items$descriptionClean[items$vendor_hash == names(descriptionTokens)[i]]

        if (length(description) == 0) next ## check format of items$description if there's no info

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
            descriptionTokens[[i]] <- unique(c(descriptionTokens[[i]], description))
        }
    }
    return(descriptionTokens)
}

# PGPclean is dataframe (cols vendor_hash, PGPclean), PGPlist is list
PGPfromUsersItems <- function(PGPclean, PGPlist) {

    for (i in 1:length(PGPlist)) {
        if (i %% 100 == 0) cat(i, ", ")

        tmp <- PGPclean$PGPclean[PGPclean$vendor_hash == names(PGPlist)[i]]

        if (length(tmp) == 0) next

        PGPlist[[i]] <- unique(c(PGPlist[[i]], tmp))
    }
    return(PGPlist)
}


#################### START HERE #################
users <- read.csv("/media/xtai/4AF561E807105059/markets/data/users_alphabay.csv", colClasses = c("character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character"), na.strings = NULL) # only read hash_str and profile
users_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_emcdda_2018_users.csv", colClasses = c("character", "NULL", "NULL", "NULL", "NULL", "NULL", rep("NULL", 3), "NULL", "NULL", "NULL", "NULL", "character", rep("NULL", 39)), na.strings = NULL) # 8238 obs
users <- rbind(users, users_new)
# > sum(duplicated(users))
# [1] 1638
# > sum(duplicated(users$hash_str))
# [1] 1638
users <- users[duplicated(users$hash) == FALSE, ]

# ITEMS data
items <- read.csv("/media/xtai/4AF561E807105059/markets/data/items_alphabay.csv", colClasses = c("character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL"), na.strings = NULL) # 230643 obs -- only read in the description
# hash_str,marketplace,title,vendor,vendor_hash,total_sales,first_observed,last_observed,prediction,prediction_number,ships_to,ships_from,description,source
items_new <- read.csv("/home/xtai/Desktop/markets/data/analysis_emcdda_2018_items.csv", colClasses = c("character", "NULL", "NULL", "NULL", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "NULL", "NULL"), na.strings = NULL) # 130897 obs
# hash_str,marketplace,title,vendor,vendor_hash,total_sales,first_observed,last_observed,prediction,prediction_number,ships_to,ships_from,description,source,sales
items <- rbind(items, items_new)
# sum(duplicated(items)) # 13140
# sum(duplicated(items$hash_str)) # 13140

items <- items[duplicated(items$hash) == FALSE, ]

#####
load("/home/xtai/Desktop/tmp_9-5/data_10-15/outStep2.Rdata")
# there are 22163 users (out of 22287 users in users.Rdata, 22163 have some feedback)

numPGPs <- unlist(lapply(outStep2$PGPlist, length), use.names = FALSE)
table(numPGPs)
# numPGPs
#     0     1     2     3     4     5     6     7     8     9
# 10538  9315  1799   377    81    38     9     4     1     1

library(heisenbrgr)
out1 <- cleanProfile(users) # out1$retrievedPGPs (3386 rows and 3372 unique vendor_hashes, i.e. some had more than one PGP in profile) and out1$users
out2 <- cleanDescriptions(items) # out2$retrievedPGPs has 918 entries, with 822 unique vendor_hashes, i.e. 822 more with posted PGP key

outStep2$profileTokens <- profileFromUsers(out1$users, outStep2$profileTokens)
outStep2$descriptionTokens <- descriptionFromItems(out2$items, outStep2$descriptionTokens)

PGPclean <- rbind(out1$retrievedPGPs, out2$retrievedPGPs)
PGPclean <- PGPclean[duplicated(PGPclean) == FALSE, ]

outStep2$PGPlist <- PGPfromUsersItems(PGPclean, outStep2$PGPlist)
numPGPs <- unlist(lapply(outStep2$PGPlist, length), use.names = FALSE)
table(numPGPs)
# numPGPs
#     0     1     2     3     4     5     6     7     8     9
# 10042  9757  1849   381    81    38     9     4     1     1

outStep2$final$numProfileTokens <- unlist(lapply(outStep2$profileTokens, length), use.names = FALSE)
outStep2$final$numDescriptionTokens <- unlist(lapply(outStep2$descriptionTokens, length), use.names = FALSE)

