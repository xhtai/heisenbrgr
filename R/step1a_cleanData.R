#' Clean feedback data
#'
#' Given feedback data, with columns `hash_str`, `marketplace`, `item_hash`,
#' `date`, `order_amount_usd`, add `vendor_hash` to link to users data, and delete
#' rows that don't have a vendor match
#'
#' @param feedback name of dataframe with feedback data as described above
#' @param items name of dataframe with unique items, needs to contain
#'   `item_hash` and `vendor_hash` at a minimum
#' @return dataframe with usable feedback data
#' @export

cleanFeedback <- function(feedback, items) {
    feedback <- cbind(feedback, vendor_hash = items$vendor_hash[match(feedback$item_hash, items$hash_str)], stringsAsFactors = FALSE)

    feedback <- feedback[!is.na(feedback$vendor_hash), c("hash_str", "marketplace", "item_hash", "date", "order_amount_usd", "vendor_hash")]

    return(feedback)
}


#' Clean items data
#'
#' Given items data, with columns `hash_str`, `marketplace`, `title`, `vendor`,
#' `vendor_hash`, `prediction`, extract inventory, i.e. add `dosage` and `unit`.
#' Delete rows that have no feedback, i.e. no sales, since we will not be
#' analyzing these.
#'
#' @param items name of dataframe with unique items, described above
#' @param feedback name of dataframe with feedback data, needs at least
#'   `item_hash`
#' @return dataframe with usable items data
#' @export

cleanItems <- function(items, feedback) {
    items <- items[items$hash_str %in% feedback$item_hash, c("hash_str", "marketplace", "title", "vendor", "vendor_hash", "prediction")]

    # get inventory
    itemsTuple <- items[, c("hash_str", "title", "prediction")]
    itemsTuple <- addGreppedPatterns(itemsTuple)
    itemsTuple <- consolidateCols(itemsTuple)

    items <- cbind(itemsTuple, items[, c("marketplace", "vendor", "vendor_hash")])

    return(items)
}

#' Search for inventory-related information within item titles
#'
#' Given items data, with columns `hash_str`, `title`, `prediction`, extract
#' columns grams, ounces, pounds, milligrams, pills, quantity
#'
#' Grams is the information associated with grams, e.g. "half gram" or "10g",
#' etc., and similarly for ounces and pounds. Milligrams can be both milligrams
#' and micrograms. Pills refers to pills, tabs, tablet, blotters. In the absence
#' of any of the preceding information, we extract a beginning numeric quantity
#' in the item title. For example, in the title "10 credit cards," 10 will be
#' extracted.
#'
#' @param itemsTuple name of dataframe with items data, described above
#' @return dataframe with six columns added
#' @export

addGreppedPatterns <- function(itemsTuple) {

    #### grams
    itemsTuple$grams <- extractPattern(itemsTuple, "[0-9./,]+( )?g|[0-9./,]+( )?gr|[0-9./,]+( )?gram|[0-9./,]+( )?grams")
    grams2 <- extractPattern(itemsTuple, "(one|quarter|full|half)( )?g|(one|quarter|full|half)( )?gr|(one|quarter|full|half)( )?gram|(one|quarter|full|half)( )?grams")
    itemsTuple$grams[is.na(itemsTuple$grams)] <- grams2[is.na(itemsTuple$grams)]

    #### same for ounces
    itemsTuple$ounces <- extractPattern(itemsTuple, "[0-9./,]+( )?ounce|[0-9./,]+( )?ounces|[0-9./,]+( )?oz")
    ounces2 <- extractPattern(itemsTuple, "(one|quarter|full|half)( )?ounce|(one|quarter|full|half)( )?ounces|(one|quarter|full|half)( )?oz")
    itemsTuple$ounces[is.na(itemsTuple$ounces)] <- ounces2[is.na(itemsTuple$ounces)]

    ########## pounds: lb, half lb, full lb, HP
    itemsTuple$pounds <- extractPattern(itemsTuple, "[0-9./,]+( )?lb|[0-9./,]+( )?pound")
    pounds2 <- extractPattern(itemsTuple, "(one|quarter|full|half)( )?lb|(one|quarter|full|half)( )?pound")
    itemsTuple$pounds[is.na(itemsTuple$pounds)] <- pounds2[is.na(itemsTuple$pounds)]

    ########## mg, ug
    itemsTuple$milligrams <- extractPattern(itemsTuple, "[0-9./,]+( )?mg|[0-9./,]+( )?ug")

    ########## pills: 10 Pills, 10 tabs, tablet, blotter
    itemsTuple$pills <- extractPattern(itemsTuple, "[0-9]+( )?x|\\([0-9]+\\)|x( )?[0-9]+ |x( )?[0-9]+$|[0-9]+( )?pill|[0-9]+( )?tab|[0-9]+( )?blotter")

    ########## beginning quantity -- only use this if nothing else is available, because other measurements could also be at the beginning
    quantity <- extractPattern(itemsTuple, "^[0-9]+")
    itemsTuple$quantity <- NA

    itemsTuple$quantity[is.na(itemsTuple$grams) & is.na(itemsTuple$ounces) & is.na(itemsTuple$pounds) & is.na(itemsTuple$milligrams) & is.na(itemsTuple$pills)] <- quantity[is.na(itemsTuple$grams) & is.na(itemsTuple$ounces) & is.na(itemsTuple$pounds) & is.na(itemsTuple$milligrams) & is.na(itemsTuple$pills)]

    return(itemsTuple)
}


# helper function
extractPattern <- function(items, pat) {

    out <- rep(NA, length(items$hash_str))
    indices <- grep(pat, items$title, ignore.case = TRUE)
    out[indices] <- regmatches(items$title[indices], regexpr(pat, items$title[indices], ignore.case = TRUE))

    return(out)
}


#' Create dosage and units columns
#'
#' Given items data, with columns `hash_str`, `title`, `prediction`, `grams`,
#' `ounces`, `pounds`, `milligrams`, `pills`, `quantity`, consolidate these into
#' `dosage` and `unit`, where dosage is e.g. "100mg", and unit is some number,
#' e.g. 30 for 30 pills, or 10 for "10 credit cards."
#'
#' For dosage, we first choose the first available value of grams, ounces,
#' pounds and milligrams. Then we process the raw text (change comma to .,
#' change one/quarter/full/half, and fractions into numbers), pasting with the
#' unit to get outputs like "100mg." Note that this process is not foolproof and
#' some dosages will inevitably be missed.
#'
#' Similarly for units we look at pills first, and if this is unavailable we use
#' quantity (note that quantity is only not NA if pills and all dosage cols are
#' NA). This is converted to a numeric variable and output.
#'
#' @param itemsTuple name of dataframe with items data, described above
#' @return dataframe with columns `hash_str`, `title`, `prediction`, `dosage`,
#'   `unit`
#' @export

consolidateCols <- function(itemsTuple) {

    itemsTuple$dosageCols <- apply(itemsTuple[, c("grams", "ounces", "pounds", "milligrams")], 1, function(x) sum(!is.na(x)))
    itemsTuple$quantityCols <- apply(itemsTuple[, c("pills", "quantity")], 1, function(x) sum(!is.na(x)))

    itemsTuple$dosage <- NA
    itemsTuple$unit <- NA

    for (i in 1:nrow(itemsTuple)) {
        if (itemsTuple$dosageCols[i] == 0 & itemsTuple$quantityCols[i] == 0) next

        ### dosage
        if (itemsTuple$dosageCols[i] > 0) {
            tmp <- ifelse(!is.na(itemsTuple$grams[i]), itemsTuple$grams[i], ifelse(!is.na(itemsTuple$ounces[i]), itemsTuple$ounces[i], ifelse(!is.na(itemsTuple$pounds[i]), itemsTuple$pounds[i], itemsTuple$milligrams[i])))
            number <- gsub(",", ".", tmp, fixed = TRUE)
            number <- gsub("one", "1", number, ignore.case = TRUE)
            number <- gsub("half", ".5", number, ignore.case = TRUE)
            number <- gsub("quarter", ".25", number, ignore.case = TRUE)
            number <- gsub("full", "1", number, ignore.case = TRUE)
            number <- gsub("...", "", number, fixed = TRUE) # for things like ......150ug and ...1/2G

            pat <- "[0-9./]+"
            if (length(grep(pat, number)) == 0) {
                # warning("i = ", i, ": no dosage found", immediate. = TRUE)
                next
            } else {
                num <- regmatches(number, regexpr(pat, number))
                if (length(grep("[0-9]/[0-9]", num)) == 1) {
                    num <- eval(parse(text = num)) # only allow things like 1/4, 1/2 -- also allows 200/300mg
                } else {
                    suppressWarnings(num <- as.numeric(num))
                }
                # if (is.na(num)) warning("i = ", i, ": as.numeric warning dosage", immediate. = TRUE)
            }
            measUnit <- ifelse(!is.na(itemsTuple$grams[i]), "g", ifelse(!is.na(itemsTuple$ounces[i]), "oz", ifelse(!is.na(itemsTuple$pounds[i]), "lb", ifelse(length(grep("m", tmp)) == 1, "mg", "ug"))))
            if (!is.na(num)) {
                itemsTuple$dosage[i] <- paste0(num, measUnit)
            }
        }

        ### now do unit
        if (itemsTuple$quantityCols[i] > 0) {

            tmp <- ifelse(!is.na(itemsTuple$pills[i]), itemsTuple$pills[i], itemsTuple$quantity[i])

            pat <- "[0-9]+"
            if (length(grep(pat, tmp)) == 0) {
                # warning("i = ", i, ": no quantity found", immediate. = TRUE)
                next
            } else {
                num <- regmatches(tmp, regexpr(pat, tmp))
                suppressWarnings(num <- as.numeric(num))
                # if (is.na(num)) warning("i = ", i, ": as.numeric warning qty", immediate. = TRUE)
            }

            if (!is.na(num)) {
                itemsTuple$unit[i] <- num
            }
        }
    }
    return(itemsTuple[, c("hash_str", "title", "prediction", "dosage", "unit")])

}




#' Clean users data
#'
#' Given users data, at least with columns `hash_str`, `marketplace`, `id`,
#' `diversity`, delete rows that have no match in feedback data
#'
#' @param users name of dataframe with unique user accounts, as described above
#' @param feedback name of dataframe with feedback data containing `vendor_hash`
#' @return dataframe with usable feedback data
#' @export

cleanUsers <- function(users, feedback) {
    users <- users[users$hash_str %in% feedback$vendor_hash, c("hash_str", "marketplace", "id", "diversity")]
    return(users)
}



#' Clean items data (excluding inventory --- dosage and unit)
#'
#' Given items data, with columns `hash_str`, `marketplace`, `title`, `vendor`,
#' `vendor_hash`, `prediction`, extract inventory, i.e. add `dosage` and `unit`.
#' Delete rows that have no feedback, i.e. no sales, since we will not be
#' analyzing these.
#'
#' @param items name of dataframe with unique items, described above
#' @param feedback name of dataframe with feedback data, needs at least
#'   `item_hash`
#' @return dataframe with usable items data
#' @export

cleanItems2 <- function(items, feedback) {
    items <- items[items$hash_str %in% feedback$item_hash, c("hash_str", "marketplace", "title", "vendor", "vendor_hash", "prediction")]

    # get inventory
    itemsTuple <- items[, c("hash_str", "title", "prediction")]
    # itemsTuple <- addGreppedPatterns(itemsTuple)
    # itemsTuple <- consolidateCols(itemsTuple)

    items <- cbind(itemsTuple, items[, c("marketplace", "vendor", "vendor_hash")])

    return(items)
}
