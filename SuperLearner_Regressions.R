SL.bartMachine #--> test this
?
SL.mean
SL.step.forward
SL.step.interaction
SL.rpartPrune

SL.xgboost2<-function(..., maxdepth = 2){
    SL.xgboost(..., max_depth = maxdepth)
}
SL.xgboost4<-function(..., maxdepth = 4){
    SL.xgboost(..., max_depth = maxdepth)
}
SL.xgboost6<-function(..., maxdepth = 6){
    SL.xgboost(..., max_depth = maxdepth)
}

SL.earth.cv <- function(..., nfold = 5){
    SL.earth(..., nfold = nfold)
}

SL.ranger2<-function(..., mtry = 2){
    SL.ranger(..., mtry = mtry)
}
SL.ranger4<-function(..., mtry = 4){
    SL.ranger(..., mtry = mtry)
}
SL.ranger6<-function(..., mtry = 6){
    SL.ranger(..., mtry = mtry)
}


SL.myglm.eachworm <- function(Y, X, newX, family, ...){
    rhs_glm_formula <- "SM + ascaris + tricuris + hookworm"
    other_variables <- " + age + sex + hiv + viral.load + vl.ind + ControlQFT + ControlQFT.ind + HB + HB.ind + malariaNEG + malariaPOS + pregnantNEG + pregnantPOS + siteJOOTRH + siteKOMBEWA"
    # the rest of the code takes care of things SuperLearner needs
    glm_formula <- paste0("Y ~ ", rhs_glm_formula, other_variables)
    fit.glm <- glm(glm_formula, data = data.frame(Y = Y, X), family = family)
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.glm"
    out <- list(pred = pred, fit = fit)
    return(out)
}

SL.myglm.totalworm <- function(Y, X, newX, family, ...){
    rhs_glm_formula <- "SM + Number"
    other_variables <- " + age + sex + hiv + viral.load + vl.ind + ControlQFT + ControlQFT.ind + HB + HB.ind + malariaNEG + malariaPOS + pregnantNEG + pregnantPOS + siteJOOTRH + siteKOMBEWA"
    # the rest of the code takes care of things SuperLearner needs
    glm_formula <- paste0("Y ~ ", rhs_glm_formula, other_variables)
    fit.glm <- glm(glm_formula, data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.glm"
    out <- list(pred = pred, fit = fit)
    return(out)
}

#gam is a glm but flexibly (non-parametrically) models continuous variables
#instead use s(age) in a glm that you define like the other glms --> s stands for splines
#basically doesn't force things to be linear
SL.myspline.eachworm<- function(Y, X, newX, family, ...){
    rhs_glm_formula <- "SM + ascaris + tricuris + hookworm"
    other_variables <- " + s(age) + sex + hiv + s(viral.load) + vl.ind + s(ControlQFT) + ControlQFT.ind + s(HB) + HB.ind + malariaNEG + malariaPOS + pregnantNEG + pregnantPOS + siteJOOTRH + siteKOMBEWA"
    # the rest of the code takes care of things SuperLearner needs
    glm_formula <- paste0("Y ~ ", rhs_glm_formula, other_variables)
    fit.glm <- glm(glm_formula, data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.glm"
    out <- list(pred = pred, fit = fit)
    return(out)
}   
SL.myspline.totalworm<- function(Y, X, newX, family, ...){
    rhs_glm_formula <- "SM + s(Number)"
    other_variables <- " + s(age) + sex + hiv + s(viral.load) + vl.ind + s(ControlQFT) + ControlQFT.ind + s(HB) + HB.ind + malariaNEG + malariaPOS + pregnantNEG + pregnantPOS + siteJOOTRH + siteKOMBEWA"
    # the rest of the code takes care of things SuperLearner needs
    glm_formula <- paste0("Y ~ ", rhs_glm_formula, other_variables)
    fit.glm <- glm(glm_formula, data = X, family = family)
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.glm"
    out <- list(pred = pred, fit = fit)
    return(out)
}

#In case modeling on the total data gives us entirely different results than if we modeled separately on HIV

#Writing a glmnet with all 2 way interactions
SL.glmnet2way<-function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, 
                         nlambda = 100, useMin = TRUE, loss = "deviance", ...) {
    SuperLearner:::.SL.require("glmnet") #keep this one
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
predict.SL.glmnet2way<-function (object, newdata, remove_extra_cols = T, add_missing_cols = T, ...) 
{
    SuperLearner:::.SL.require("glmnet") #keep this one
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


SL.stratify.glm <- function(Y, X, newX, stratify_variable = "hiv", ...){
    # subset to folks with stratify variable == 1
    X_strat1 <- X[X[,stratify_variable] == 1, , drop = FALSE]
    # drop stratify variable
    X_strat1 <- X_strat1[ , -which(colnames(X_strat1) == stratify_variable)]
    # same thing for held-out data
    newX_strat1 <- newX[newX[,stratify_variable] == 1, , drop = FALSE]
    newX_strat1 <- newX_strat1[ , -which(colnames(newX_strat1) == stratify_variable)]
    # same thing for outcome
    Y_strat1 <- Y[X[,stratify_variable] == 1]
    glm_strat1 <- glm(Y_strat1 ~ . , data = X_strat1, family = family)
    pred_strat1 <- predict(glm_strat1, newdata = newX_strat1, type = "response")
    
    # now play the same game in folks with stratify variable == 0
    # subset to folks with stratify variable == 0
    X_strat0 <- X[X[,stratify_variable] == 0, , drop = FALSE]
    # drop stratify variable
    X_strat0 <- X_strat0[ , -which(colnames(X_strat0) == stratify_variable)]
    # same thing for held-out data
    newX_strat0 <- newX[newX[,stratify_variable] == 0, , drop = FALSE]
    newX_strat0 <- newX_strat0[ , -which(colnames(newX_strat0) == stratify_variable)]
    # same thing for outcome
    Y_strat0 <- Y[X[,stratify_variable] == 0]
    glm_strat0 <- glm(Y_strat0 ~ . , data = X_strat0, family = family)
    pred_strat0 <- predict(glm_strat0, newdata = newX_strat0, type = "response")
    
    # now wrap up in a way that SuperLearner likes
    fit <- list(object_strat0 = glm_strat0, object_strat1 = glm_strat1, 
                stratify_variable = stratify_variable)
    class(fit) <- "SL.stratify.glm"
    pred <- rep(NA, nrow(newX))
    pred[newX[,stratify_variable] == 1] <- pred_strat1
    pred[newX[,stratify_variable] == 0] <- pred_strat0
    out <- list(pred = pred, fit = fit)
    return(out)
}
predict.SL.stratify.glm <- function(object, newdata, ...){
    # subset newdata based on stratify variable
    newdata_strat1 <- newdata[newdata[,object$stratify_variable] == 1, , drop = FALSE]
    newdata_strat0 <- newdata[newdata[,object$stratify_variable] == 0, , drop = FALSE]
    pred_strat1 <- predict(object$object_strat1, newdata = newdata_strat1, type = "response")
    pred_strat0 <- predict(object$object_strat0, newdata = newdata_strat0, type = "response")
    pred <- rep(NA, nrow(newdata))
    pred[newdata[,stratify_variable] == 1] <- pred_strat1
    pred[newdata[,stratify_variable] == 0] <- pred_strat0 
    return(pred)
}
