# if needed, install all the necessary pacakges to execute this demo
install.packages(c("SuperLearner","gam","caret","randomForest","arm","RCurl","MASS","tmle","ggplot2","gbm"))

# load the packages
require(SuperLearner)
require(gam)
require(caret)
require(randomForest)
require(RCurl)
require(MASS)
require(tmle)
require(ggplot2)
require(gbm)
library(RCurl)

# prediction data set
chspred <- read.csv(text = getURL("https://raw.githubusercontent.com/benkeser/sllecture/master/chspred.csv"), header = TRUE)

set.seed(1234)

# execute the call to SuperLearner
sl1 <- SuperLearner(
    # Y is the outcome variable
    Y = chspred$mi,
    # X is a dataframe of predictor variables, in this case
    # everything in chspred except for mi
    X = chspred[,-ncol(chspred)], 
    # newX will be discussed later, for now leave as NULL (default)
    newX = NULL,
    # family will be discussed in more detail when we see how wrappers
    # are written, for now set to binomial() for 0/1 outcome
    family = binomial(), 
    # SL.library (for now) is specified as a vector of names of functions
    # that implement the desired algorithms. SL.glm and SL.mean
    # are included in the Super Learner package
    SL.library = c("SL.glm","SL.earth", "SL.gam", "SL.gbm", "SL.nnet", 
                   "SL.nnls", "SL.polymars", "SL.randomForest", "SL.svm", "SL.ksvm",
                   "SL.bartMachine", "SL.xgboost", "SL.bayesglm", "SL.polymars"),
    # method specifies how the ensembling is done, for now we will use
    # the \sum_{k=1}^K \alpha_k f_{k,n} method by using the deafult
    # option for method (method.NNLS)
    method = "method.CC_nloglik",
    # id specifies a unique subject identifier so that whole subjects 
    # are sampled in CV, not just rows of data. chspred only has one row 
    # per subject, so OK to leave as NULL (default)
    id = NULL, 
    # verbose controls the printing of messages of SuperLearner's progress.
    # We'll leave as FALSE (default) for now
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
sl1

# default call to predict
slPred <- predict(sl1)
# slPred is a list with two components
#   pred = continuous SL predictions
#   library.predict = predictions from each algorithm

# store the continuous SL predictions
cslPred <- slPred$pred

# get the discrete SL predictions
dslPred <- slPred$library.predict[,which(sl1$cvRisk==min(sl1$cvRisk))]





####### statin data
statins <- read.csv(text = getURL("https://raw.githubusercontent.com/benkeser/sllecture/master/statins.csv"), header = TRUE)

# estimate g_0^1
sl.g1 <- SuperLearner(
    # our outcome is now the statin variable
    Y = statins$statin, 
    # our predictors are all variables except for death and statins
    X = statins[,-c(1,ncol(statins))],
    # outcome is binary, so let's use family = binomial()
    family = binomial(),
    # and nnloglik metho
    method=method.NNloglik,
    # simple library for computational efficiency
    SL.library = c("SL.glm","SL.mean")
)

# get predicted probability that statin = 1
g1n <- sl.g1$SL.pred

# the predicted probability that statin = 0 is 1-g1n
g0n <- 1 - g1n

# get predicted probability that statin = observed value
gan <- ifelse(statins$statin==0, g0n[statins$statin==0], g1n[statins$statin==1])

# estimate \bar{Q}_0
sl.Q <- SuperLearner(
    # our outcome is death
    Y = statins$death, 
    # our predictors are all variables other than death
    X = statins[,-ncol(statins)],
    # outcome is binary, so let's use family = binomial()
    family = binomial(),
    # and nnloglik metho
    method=method.NNloglik,
    # simple library for computational efficiency
    SL.library = c("SL.glm","SL.mean")
)

# set up a data frame where everyone has statin=1
statins1 <- statins[,-ncol(statins)]
statins1$statin <- 1

# get \bar{Q}_n^1
Q1n <- predict(sl.Q, newdata=statins1)$pred

# set up a data frame where everyone has statin=0
statins0 <- statins[,-ncol(statins)]
statins0$statin <- 0

# get \bar{Q}_n^0
Q0n <- predict(sl.Q, newdata=statins0)$pred

# get \bar{Q}_n^a
Qan <- ifelse(statins$statin==0, Q0n[statins$statin==0], Q1n[statins$statin==1])


# hiv vaccine data
hiv <- read.csv(text = getURL("https://raw.githubusercontent.com/benkeser/sllecture/master/hiv.csv"), header = TRUE)




