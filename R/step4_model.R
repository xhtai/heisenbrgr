#' Run classification using random forests
#'
#' Given an input allPairwise dataframe with pairwise similarity metrics, run a
#' random forest model. The outcome variable is `PGPmatched`, a binary variable.
#' This column can be 0, 1 or NA. The cross-validation set is made of non-NA
#' values. The cross-validation set is split into 10 folds, and predictions for
#' each fold are made using the remaining 9 folds. Then the model is trained on
#' the entire cross-validation set, and this model is used to predict on the
#' test set. Predictions on the cross-validation set are returned as `out`, on
#' the test set are `outTest`. The model fit on all the cross-validation data is
#' returned as `fit`.
#'
#' @param allPairwise name of dataframe that contains pairwise similarity
#'   metrics. Needs `PGPmatched` column which is the outcome variable.
#' @param similarityCols indices of columns that should be included in model
#' @return `out`, `outTest`, `fit` as described above
#' @export

predictRF <- function(allPairwise, similarityCols) {
    train <- allPairwise[!is.na(allPairwise$PGPmatched), ]
    test <- allPairwise[is.na(allPairwise$PGPmatched), ]

    trainX <- train[ , similarityCols]
    trainY <- train$PGPmatched

    K = 10
    n <- nrow(train)
    d = ceiling(n/K)
    set.seed(0)
    i.mix = sample(1:n)
    folds = vector(mode = "list", length = K)

    for (i in 1:K) {
        folds[[i]] <- i.mix[((i - 1)*d + 1):(i*d)]
    }

    preds <- rep(NA, nrow(train))

    for (k in 1:K) {
        cat("Fold", k, "\n")
        i.tr <- unlist(folds[-k])
        i.tr <- i.tr[!is.na(i.tr)]
        i.val <- folds[[k]]
        i.val <- i.val[!is.na(i.val)]

        x.tr <- trainX[i.tr, ]
        y.tr <- trainY[i.tr]
        x.val <- trainX[i.val, ]

        set.seed(1)
        tmpFit <- randomForest::randomForest(x = x.tr, y = as.factor(y.tr), ntree = 100)

        preds[i.val] <- predict(tmpFit, x.val, type = "vote")[, 2]
    }

    out <- train[, c("hash1", "hash2", "id1", "id2", "marketplace1", "marketplace2", "PGPmatched")]
    out$preds <- preds

    set.seed(7)
    fit <- randomForest::randomForest(x = trainX, y = as.factor(trainY), ntree = 100)
    predTest <- predict(fit, test[ , similarityCols], type = "vote")[, 2]
    outTest <- test[, c("hash1", "hash2", "id1", "id2", "marketplace1", "marketplace2")]
    outTest$preds <- predTest

    return(list(out = out, fit = fit, outTest = outTest))
}


#' Hierarchical clustering to generate final clusters
#'
#' Given predictions for each pair, generate transitive closures using
#' hierarchical clustering with single linkage.
#'
#' @param pairs pairs with `preds` column, which are predictions from the random
#'   forest model
#' @param final output from Step 2 with all the account-level information
#' @return dataframe of accounts with their final clusters and sizes
#' @export

hierarchical <- function(pairs, final) {
    forHierarchical <- pairs[pairs$preds >= .5, ]

    ############## now get hashes
    hashes <- unique(c(forHierarchical$hash1, forHierarchical$hash2))

    distMat <- matrix(NA, nrow = length(hashes), ncol = length(hashes))
    distMat[lower.tri(distMat)] <- 1

    for (ii in 1:nrow(forHierarchical)) {
        tmpi <- which(hashes == forHierarchical$hash1[ii])
        tmpj <- which(hashes == forHierarchical$hash2[ii])
        i <- max(tmpi, tmpj)
        j <- min(tmpi, tmpj)
        distMat[i, j] <- 1 - forHierarchical$preds[ii]
    }

    distObj <- as.dist(distMat, diag = FALSE, upper = FALSE)
    hcluster <- hclust(distObj, method = "single")

    clustersAll <- cutree(hcluster, h = .5) # CHANGE THIS

    hashesOut <- final[, c("vendor_hash", "id", "marketplace")]
    hashesOut$finalCluster <- clustersAll[match(hashesOut$vendor_hash, hashes)]

    hashesOut$finalCluster[is.na(hashesOut$finalCluster)] <- (max(clustersAll) + 1):(max(clustersAll) + sum(is.na(hashesOut$finalCluster)))

    tmp <- aggregate(hashesOut$finalCluster, by = list(finalCluster = hashesOut$finalCluster), FUN = length)
    names(tmp)[2] <- "size"

    out <- merge(hashesOut[, c("vendor_hash", "id", "marketplace", "finalCluster")], tmp, by.x = "finalCluster", by.y = "finalCluster")

    out <- out[order(-out$size, out$finalCluster, out$id, out$marketplace), ]
    # out <- out[out$size > 1, ] # 2997

    return(out)
}

