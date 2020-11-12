pi_0 <- 0.412
pi_B <- 0.246
pi_1 <- 0.181
pi_2 <- 0.060
pi_3 <- 0.016
pi_4 <- 0.085

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
    P0[i, transitionOut[i] ] <- pi_0
    PB[i, transitionBB[i]  ] <- pi_B
    P1[i, transition1B[i]  ] <- pi_1
    P2[i, transition2B[i]  ] <- pi_2
    P3[i, transition3B[i]  ] <- pi_3
    P4[i, transitionHR[i]  ] <- pi_4
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

ruth <- c(pi_B, pi_1, pi_2, pi_3, pi_4)
runsRuth <- M %*% ruth

Q <-  P[1:24, 1:24]
cat( solve(diag(24) - Q, runsRuth[1:24]) )

library("markovchain")
nStates <- 25
baseballStates <- as.character(c(1:nStates))
mcBaseball <- new("markovchain", states = baseballStates, byrow=TRUE, transitionMatrix = P, name="mcRuth")
cat( expectedRewards(mcBaseball, 30, runsRuth) )


