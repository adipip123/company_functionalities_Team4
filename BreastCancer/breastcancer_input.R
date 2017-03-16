#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  stop("9 arguments must be supplied ", call.=FALSE)
} else{

thetaVector <- list()
thetaVector[[1]] <- array(1,dim = c(6,10))
thetaVector[[2]] <- matrix(1,nrow = 4,ncol = 7)
thetaVector[[3]] <- matrix(1,nrow = 1,ncol = 5)

# to store actual values of computation
nodes1 <- matrix(2,nrow = 10,ncol = 1)
nodes2 <- matrix(2,nrow = 7,ncol = 1)
nodes3 <- matrix(2,nrow = 5,ncol = 1)

mean1 <- c(4.442167,3.150805,3.215227,2.830161,3.234261,3.544656,3.445095,2.869693,1.603221)
sd1 <- c(2.820761,3.065145,2.988581,2.864562,2.223085,3.643857,2.449697,3.052666,1.732674)
nodes1 <- as.numeric(as.character(args))
# nodes1 <- c(1,5,1,1,1,2,1,3,1,1)
nodes1[2] <- (nodes1[2]-mean1[1])/sd1[1] 
nodes1[3] <- (nodes1[3]-mean1[2])/sd1[2] 
nodes1[4] <- (nodes1[4]-mean1[3])/sd1[3] 
nodes1[5] <- (nodes1[5]-mean1[4])/sd1[4] 
nodes1[6] <- (nodes1[6]-mean1[5])/sd1[5] 
nodes1[7] <- (nodes1[7]-mean1[6])/sd1[6] 
nodes1[8] <- (nodes1[8]-mean1[7])/sd1[7] 
nodes1[9] <- (nodes1[9]-mean1[8])/sd1[8] 
nodes1[10] <-(nodes1[10]-mean1[9])/sd1[9]

thetaVector[[1]][1,] <- c( 1.104005, 2.5523746,0.9047006, 1.42413996, 2.4755557,-1.8248911, 2.79408412,-2.2906619,1.730777, 2.0116644)
thetaVector[[1]][2,] <- c(-1.887264, 1.6219369,1.3541722, 0.90427456,-0.5276422, 0.1816634, 2.84357813,-3.0997675,1.567506, 5.1308040)
thetaVector[[1]][3,] <- c( 2.121525, 1.6305754,2.5044159, 1.12112808, 0.8859523, 2.6767029, 4.43541271,-4.5870019,1.227317, 1.4229437)
thetaVector[[1]][4,] <- c( 3.893172,-0.2479299,2.4520286, 0.04235929,-1.4742965,-2.6965036, 1.30559744,-4.0597143,3.543919,-0.7210873)
thetaVector[[1]][5,] <- c( 4.796235,-0.3630921,1.1684880, 0.58081275,-1.3430119,-3.0025778,-0.28847446,-2.4752768,1.744279,-3.3246693)
thetaVector[[1]][6,] <- c(-2.811819, 0.2817386,2.1600171,-0.30675106,-1.1344440,-2.1408464,-0.09744665,-0.8184009,1.626729, 4.1073703)

thetaVector[[2]][1,] <- c(-1.2526016,1.583954,0.4788097, 0.1372948, 1.387663,-3.709899,3.345946)
thetaVector[[2]][2,] <- c( 1.4041289,2.532685,1.6078010, 1.1971912, 1.082807, 3.103837,3.000301)
thetaVector[[2]][3,] <- c( 2.1706602,1.521154,2.5289740, 1.1774346, 1.332713, 3.043386,4.000851)
thetaVector[[2]][4,] <- c(-0.9287337,1.296901,5.1079285,-1.1602423,-2.466328,-3.701066,4.342088)

thetaVector[[3]][1,] <- c(-2.938249,3.851918,-2.559276,-2.800843,12.76194)

# nodes1 <-as.numeric(test_set[i,c(-10)])                   # input layer i.e layer1
nodes2 <- c(1,as.numeric(thetaVector[[1]] %*% nodes1))     # hidden layer1 i.e layer2
nodes2[-1] <- 1/(1+exp(-nodes2[-1]))
nodes3 <- c(1,as.numeric(thetaVector[[2]] %*% nodes2))     # hidden layer2 i.e layer3
nodes3[-1] <- 1/(1+exp(-nodes3[-1]))
h <- sum(as.numeric(thetaVector[[3]] * nodes3))               # output layer i.e layer4
h <- 1/(1+exp(-h))

# saving the vector in a .txt file
thetaVectorSave <- h
write(thetaVectorSave, file = "breastcancer_output.txt",
      ncolumns = if(is.character(thetaVectorSave)) 1 else 11,
      append = FALSE, sep = " ")
}
