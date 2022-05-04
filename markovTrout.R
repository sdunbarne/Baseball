## http://www.espn.com/mlb/player/stats/_/id/30836/mike-trout

## GP	AB	R	H	2B	3B	HR	RBI	BB
## 134 	470 	110 	137 	27 	2 	45 	104 	110
## SO	SB	CS	AVG	OBP	SLG	OPS	WAR
## 120 	11 	2 	.291 	.438 	.645 	1.083 	8.3

## Plate Appearances = AB + BB + HBP = 470 + 110 + 16 = 596

## Outs = AB - H = 470 - 137 = 333

## H = 137
## 2B = 27
## 3B = 2
## HR = 45
## 1B = H - 2B - 3B - HR = 137 - 27 -2 -45 = 63

rho0 <- 333/596
rhoB <- 126/596
rho1 <- 63/596
rho2 <- 27/596
rho3 <- 2/596
rho4 <- 45/596

P0 <- matrix(0, nrow=25, ncol=25)
PB <- matrix(0, nrow=25, ncol=25)
P1 <- matrix(0, nrow=25, ncol=25)
P2 <- matrix(0, nrow=25, ncol=25)
P3 <- matrix(0, nrow=25, ncol=25)
P4 <- matrix(0, nrow=25, ncol=25)

##  State          1   2   3   4   5   6   7   8  9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24
transitionOut <- c(2,  3, 25,  5,  6, 25,  8,  9, 25, 11, 12, 25, 14, 15, 25, 17, 18, 25, 20, 21, 25, 23, 24, 25)
transitionBB  <- c(4,  5,  6, 13, 14, 15, 13, 14, 15, 16, 17, 18, 22, 23, 24, 22, 23, 24, 22, 23, 24, 22, 23, 24)
transition1B  <- c(4,  5,  6, 16, 17, 18,  4,  5,  6,  4,  5,  6, 16, 17, 18, 16, 17, 18, 19, 20, 21, 16, 17, 18)
transition2B  <- c(7,  8,  9, 19, 20, 21,  7,  8,  9,  7,  8,  9, 19, 20, 21, 19, 20, 21,  7,  8,  9, 19, 20, 21)
transition3B  <- c(10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12, 10, 11, 12)
transitionHR  <- c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3)

for (i in 1:24) {
    P0[i, transitionOut[i] ] <- rho0
    PB[i, transitionBB[i]  ] <- rhoB
    P1[i, transition1B[i]  ] <- rho1
    P2[i, transition2B[i]  ] <- rho2
    P3[i, transition3B[i]  ] <- rho3
    P4[i, transitionHR[i]  ] <- rho4
}

P <- P0 + PB + P1 + P2 + P3 + P4
P[25, 25] = 1

M <- matrix(0, nrow=25, ncol=5)
M[22:24,   1] <- 1
M[7:18,  2:3] <- 1
M[19:24, 2:3] <- 2
M[4:12,    4] <- 1
M[13:21,   4] <- 2
M[22:24,   4] <- 3
M[1:3,     5] <- 1
M[4:12,    5] <- 2
M[13:21,   5] <- 3
M[22:24,   5] <- 4

trout <- c(rhoB, rho1, rho2, rho3, rho4)
runsTrout <- M %*% trout

Q <-  P[1:24, 1:24]
cat( solve(diag(24) - Q, runsTrout[1:24]) )

library("markovchain")
nStates <- 25
baseballStates <- as.character(c(1:nStates))
mcBaseball <- new("markovchain", states = baseballStates, byrow=TRUE, transitionMatrix = P, name="mcTrout")
cat( expectedRewards(mcBaseball, 30, runsTrout) )


