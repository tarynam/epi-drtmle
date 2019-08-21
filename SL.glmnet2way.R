SL.glmnet2way<-function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, 
                         nlambda = 100, useMin = TRUE, loss = "deviance", ...) 
{
    .SL.require("glmnet") #keep this one
    if (!is.matrix(X)) {
        X <- model.matrix(~-1 + .^2, X) #updated
        newX <- model.matrix(~-1 + .^2, newX) #updated
    }
    fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights, 
                               lambda = NULL, type.measure = loss, nfolds = nfolds, 
                               family = family$family, alpha = alpha, nlambda = nlambda, 
                               ...)
    pred <- predict(fitCV, newx = newX, type = "response", s = ifelse(useMin, 
                                                                      "lambda.min", "lambda.1se"))
    fit <- list(object = fitCV, useMin = useMin)
    class(fit) <- "SL.glmnet2way" #changed this to match the name
    out <- list(pred = pred, fit = fit)
    return(out)
}

#How does this know to pull from the new glmnet and not the old one???
predict.SL.glmnet2way<-function (object, newdata, remove_extra_cols = T, add_missing_cols = T, 
                                 ...) 
{
    .SL.require("glmnet") #keep this one
    if (!is.matrix(newdata)) {
        newdata <- model.matrix(~-1 + .^2, newdata) #updated
    }
    original_cols = rownames(object$object$glmnet.fit$beta) #This is where I wasn't sure. 
    if (remove_extra_cols) {
        extra_cols = setdiff(colnames(newdata), original_cols)
        if (length(extra_cols) > 0) {
            warning(paste("Removing extra columns in prediction data:", 
                          paste(extra_cols, collapse = ", ")))
            newdata = newdata[, !colnames(newdata) %in% extra_cols, 
                              drop = F]
        }
    }
    if (add_missing_cols) {
        missing_cols = setdiff(original_cols, colnames(newdata))
        if (length(missing_cols) > 0) {
            warning(paste("Adding missing columns in prediction data:", 
                          paste(missing_cols, collapse = ", ")))
            new_cols = matrix(0, nrow = nrow(newdata), ncol = length(missing_cols))
            colnames(new_cols) = missing_cols
            newdata = cbind(newdata, new_cols)
            newdata = newdata[, original_cols]
        }
    }
    pred <- predict(object$object, newx = newdata, type = "response", 
                    s = ifelse(object$useMin, "lambda.min", "lambda.1se"))
    return(pred)
}