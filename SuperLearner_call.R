SL_Q <- SuperLearner(
    # Y is the outcome variable
    Y = dat$ltbi,
    # X is a dataframe of predictor variables, in this casce
    # everything in dat except for TB outcomes
    X = dplyr::select(dat,-ltbi, -hc, -tb), 
    newX = NULL,
    # family set to binomial() for 0/1 outcome
    family = binomial(), 
    # SL.library will be filled more completely later
    SL.library = c("SL.myglm.eachworm"),
    # method specifies how the ensembling is done
    # convex combination negative log-likelihood
    method = "method.CC_nloglik",
    # id specifies a unique subject identifier. data only has one row 
    # per subject, so OK to leave as NULL (default)
    id = NULL, 
    # verbose controls the printing of messages of SuperLearner's progress.
    verbose = FALSE, 
    # control contains options related to logistic ensemble (trimLogit) 
    # and whether to save the fit library to look at individual 
    # algorithms later. We will leave as default
    control = list(saveFitLibrary = TRUE, trimLogit = 0.001),
    # cvControl specifies parameters related to cross validation. Of note
    # the default is for V=10-fold cross validation. See ?SuperLearner
    # for more details
    cvControl = list(V = 10L, stratifyCV = FALSE, shuffle = TRUE, 
                     validRows = NULL)
)
