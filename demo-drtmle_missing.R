set.seed(1234)
n <- 200
W <- data.frame(W1 = runif(n), W2 = rbinom(n, 1, 0.5))
A <- rbinom(n, 1, plogis(W$W1 + W$W2))
Y <- rbinom(n, 1, plogis(W$W1 + W$W2*A))
DeltaA <- rbinom(n, 1, plogis(2 + W$W1))
DeltaY <- rbinom(n, 1, plogis(2 + W$W2 + A))
A[DeltaA == 0] <- NA
Y[DeltaY == 0] <- NA

# first regress indicator of missing A on W
fit_DeltaA <- glm(DeltaA ~ . , data = W, family = binomial())

# get estimated propensity for observing A
ps_DeltaA <- predict(fit_DeltaA, type = "response")

# now regress A on W | DeltaA = 1
fit_A <- glm(A[DeltaA == 1] ~ . , data = W[DeltaA == 1, , drop = FALSE], 
             family = binomial())

# get estimated propensity for receiving A = 1
ps_A1 <- predict(fit_A, newdata = W, type = "response")
# propensity for receiving A = 0
ps_A0 <- 1 - ps_A1

# now regress DeltaY on A + W | DeltaA = 1. Here we are pooling over 
# values of A (i.e., this is what drtmle does if stratify = FALSE). 
# We could also fit two regressions, one of DeltaY ~ W | DeltaA = 1, A = 1
# and one of DeltaY ~ W | DeltaA = 1, A = 0 (this is what drtmle does if
# stratify = TRUE). 
fit_DeltaY <- glm(DeltaY[DeltaA == 1] ~ . , 
                  data = data.frame(A = A, W)[DeltaA == 1, ], 
                  family = binomial())

# get estimated propensity for observing outcome if A = 1
ps_DeltaY_A1 <- predict(fit_DeltaY, newdata = data.frame(A = 1, W), 
                        type = "response")

# get estimated propensity for observing outcome if A = 0
ps_DeltaY_A0 <- predict(fit_DeltaY, newdata = data.frame(A = 0, W), 
                        type = "response")

# now combine all results into a single propensity score 
gn <- list(
    # propensity for DeltaA = 1, A = 1, DeltaY = 1
    ps_DeltaA * ps_A1 * ps_DeltaY_A1,
    # propensity for DeltaA = 1, A = 0, DeltaY = 1
    ps_DeltaA * ps_A0 * ps_DeltaY_A0
)

# pass in this gn to drtmle
out_fit_ps <-  drtmle(W = W, A = A, Y = Y, stratify = FALSE,
                      # make sure a_0/gn are ordered properly!
                      gn = gn, a_0 = c(1, 0),
                      glm_Q = "W1 + W2*A", glm_Qr = "gn",
                      glm_gr = "Qn", family = binomial())
out_fit_ps


theta<-out_fit_ps$drtmle$est[1]-out_fit_ps$drtmle$est[2]
#the first derivatives of the function with respect to est 1 is 1 and with respect to est 2 is -1
f<-matrix(c(1,-1), nrow=2, ncol=1)
ft<-t(f) #transpose
sigma<-out_fit_ps$drtmle$cov #sigma is the variance covariance matrix
var<- ft %*% sigma %*% f  #the variance for the function is the matrix multiplication of all these things
n <- length(Y) #Why don't we use this? 

ciu <- theta + 1.96 * sqrt(var) #don't use n which I don't understand.
cil <- theta - 1.96 * sqrt(var)

proof<-rbind(
    data.frame(ci(out_fit_ps, contrast = c(1,-1))$drtmle),
    c(theta, cil, ciu))


