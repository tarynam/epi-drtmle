kenya<-read.csv("/Applications/Old Computer/Epi Project/Data_clean/Total.csv")

pkgs <- c("drtmle","earth","SuperLearner","nloptr", "quadprog","plotmo","plotrix","TeachingDemos")
# see what packages are currently installed
installed_pacakges <- row.names(installed.packages()) # loop over the needed packages
for(p in pkgs){
    # check if package is installed
    already_installed <- p %in% installed_pacakges
    # if not already installed, install it
    if(!already_installed){ install.packages(p)
    }
    # and load package
    library(p, character.only = TRUE) }

# set a seed for reproducibility
set.seed(212)
# sample size
n <- 300
# W1 has Normal distribution, W2 has Uniform distribution 
W1 <- rnorm(n); 
W2 <- runif(n)
# make a data.frame of covariates
W <- data.frame(W1 = W1, W2 = W2)
# pr(A = 1 | W) is logistic linear in W 
g1W <- plogis(-1 + W1 - W2 + W1*W2)
# generate binary treatment
A <- rbinom(n, 1, g1W)
# E[Y | A, W] is logistic linear in A, W 
QAW <- plogis(W1 - W2 + A)
# generate outcome by adding random error 
Y <- rbinom(n, 1, QAW)

set.seed(123)
fit1 <- drtmle(W = W, A = A, Y = Y, a_0 = c(0,1), family = binomial(),
               SL_g = c("SL.earth", "SL.glm"), 
               SL_Q = c("SL.svm", "SL.glm"), 
               SL_gr = c("SL.earth", "SL.glm"), 
               SL_Qr = c("SL.earth", "SL.glm"), 
               stratify = FALSE)

#Still worked but warning messages
#Warning messages:
#1: In is.na(x) : is.na() applied to non-(list or vector) of type 'NULL'

riskRatio <- list(
    f = function(eff){ log(eff) }, 
    f_inv = function(eff){ exp(eff) },
    h = function(est){ est[1]/est[2] },
    fh_grad = function(est){ c(1/est[1],-1/est[2]) }
    )

fit_TB<-drtmle(A=dat$Worm, Y=dat$TB, a_0 = c(0,1), W=dplyr::select(dat, -TB, -Worm),
             family = binomial(),
             SL_g = c("SL.earth", "SL.glm"), 
             SL_Q = c("SL.svm", "SL.gbm"),
             SL_gr = c("SL.earth", "SL.glm"), 
             SL_Qr = c("SL.earth", "SL.glm"),
             stratify = FALSE)

fit_LTBI<-drtmle(A=dat$Worm, Y=dat$LTBI, a_0 = c(0,1), W=dplyr::select(dat, -LTBI, -Worm),
               family = binomial(),
               SL_g = c("SL.earth", "SL.glm"), 
               SL_Q = c("SL.svm", "SL.gbm"),
               SL_gr = c("SL.earth", "SL.glm"), 
               SL_Qr = c("SL.earth", "SL.glm"),
               stratify = FALSE)

fit_HC<-drtmle(A=dat$Worm, Y=dat$HC, a_0 = c(0,1), W=dplyr::select(dat, -HC, -Worm),
                 family = binomial(),
                 SL_g = c("SL.earth", "SL.glm"), 
                 SL_Q = c("SL.svm", "SL.gbm"),
                 SL_gr = c("SL.earth", "SL.glm"), 
                 SL_Qr = c("SL.earth", "SL.glm"),
                 stratify = FALSE)


