# setting the working directory
setwd("Documents/BE/heart")
# Importing the dataset
dataset <- read.csv("heart_disease_male.csv")
# dataset_backup <- dataset

# Data-presprocessing
# dataset$disease = factor(dataset$disease, levels = c("positive", "negative"))
# dataset$disease <- as.numeric(as.character(dataset$disease))
is.factor(dataset$disease)
# converting categorical columns to numeric
dataset$exercice_angina <- ifelse(dataset$exercice_angina=="yes",1,0)
dataset$disease <- ifelse(dataset$disease=="positive",1,0)
dataset$blood_sugar <- ifelse(dataset$blood_sugar=='t',1,0)
# one row of the datset has a '?' at row:64 and col:rest_electro
row_to_keep<-(dataset$rest_electro!='?')
dataset <-dataset[row_to_keep,]
# converting chest_pain col to dummy variables
dataset$cp_asy<-ifelse(dataset$chest_pain=="asympt",1,0)
dataset$cp_asyang<-ifelse(dataset$chest_pain=="atyp_angina",1,0)
dataset$cp_non_anginal<-ifelse(dataset$chest_pain=="non_anginal",1,0)
dataset$cp_typ_angina<-ifelse(dataset$chest_pain=="typ_angina",1,0)
# removing the chest_pain column
dataset = dataset[, -2]
# converting rest_electro colto dummy variables
dataset$rest_electro_left<-ifelse(dataset$rest_electro=="left_vent_hyper",1,0)
dataset$rest_electro_normal<-ifelse(dataset$rest_electro=="normal",1,0)
dataset$rest_electro_st<-ifelse(dataset$rest_electro=="st_t_wave_abnormality",1,0)
# removing the rest_electro column
dataset = dataset[, -4]
# remove overfitting
dataset = dataset[, -13]
dataset = dataset[, -10]
# an extra bias column is added for ANN calculation
dataset$bias <- array(1,dim = c(208,1))
# creating a new dataset to rearrange the columns
dataset_new <-data.frame(dataset$bias,dataset$age,dataset$cp_asy,dataset$cp_asyang,dataset$cp_non_anginal,
                         dataset$rest_bpress,dataset$blood_sugar,dataset$rest_electro_left,dataset$rest_electro_normal,
                         dataset$max_heart_rate,dataset$exercice_angina,dataset$disease)
colnames(dataset_new)<-c("bias","age","cp_asy","cp_asyang","cp_non_anginal",
                         "rest_bpress","blood_sugar","rest_electro_left","rest_electro_normal",
                         "max_heart_rate","exercice_angina","disease")
# checking the dataset for factors and anomalies before splitting
# summary(dataset_new)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset_new$disease, SplitRatio = .5)
training_set <- subset(dataset_new, split == TRUE) #TEMP
test_set <- subset(dataset_new, split == FALSE)
# feature scaling
# dataset_new[, c(2,6,10)] <- scale(dataset_new[, c(2,6,10)])
training_set[, c(2,6,10)] <- scale(training_set[, c(2,6,10)])
test_set[, c(2,6,10)] <- scale(test_set[, c(2,6,10)])

# removing the variables not needed anymore
rm(row_to_keep)
rm(split)
# done with pre_processing

# ALGORITHM BEGINS
# step1: calculate cost(h(x),y) = -log(h(x)) if y=1 or -log(1-h(x)) if y=0
#         this function is taken to make the curve convex
# step2: J(Theta)=1/m * summation(cost) for all rows
# step3: minimize J(Theta)
# once we get theta values,the classifier is ready

# we are creating a new function for log as log(0)=infinity and such a value cannot be handled as numeric
theLog <- function(x){
  if(x==0)  return(-100)
  else  return (log(x))
}

# initialize variables
Yvec <- training_set$disease
cost <- 0
jThetaPrev <- 100000
jTheta <- 100
difference <- 10
nrts <- nrow(training_set)
Hvec <-vector(mode= "double" ,length = nrts)
# thetaVector has to be a multidimensional vector
# each layer has a 2D vector for theta and there are multiple layer(2-3 in our case)
# so there are be a 3D vector to store the coefficents for the ANN 
# creating a 3d vector
thetaVector <- list()
thetaVector[[1]] <- array(1,dim = c(6,11))
thetaVector[[2]] <- matrix(1,nrow = 4,ncol = 7)
thetaVector[[3]] <- matrix(1,nrow = 1,ncol = 5)
thetaVector[[1]][1,] <- c(1,2,0.5,1,3,-2,3,-3,1,0.1,1)
thetaVector[[1]][2,] <- c(1,2.5,1.5,1,1,3,3,-4,2,0.3,1)
thetaVector[[1]][3,] <- c(2,1.5,2.5,1.1,1.3,3,4,-5,1,0.3,1)
thetaVector[[1]][4,] <- c(3,0.3,3,1,-2,-3,2,-3,4,0.4,1)
thetaVector[[1]][5,] <- c(2,0.1,2,3,-3,-4,1,-2,3,0.2,1)
thetaVector[[1]][6,] <- c(-2,1,4,0.1,-0.2,-1.1,1,0.2,2,-2,1)
# thetaVector[[1]][7,] <- c(-2,1,0.3,2.1,-4,-3,2,3,1,0,3)
# thetaVector[[1]][8,] <- c(-2,1,3,1.1,-2,-2,-1,0.2,-1,2,1)
# thetaVector[[1]][9,] <- c(1,-2,3,-2.1,2,-3,-1,1.2,1,4,-2)
# thetaVector[[1]][10,] <- c(1,-4,3,-2.1,1,4,1,1,1,4,-1)
# thetaVector[[2]] <- thetaVector[[1]]
thetaVector[[2]][1,] <- c(1,2,0.5,1,3,-2,3)
thetaVector[[2]][2,] <- c(1,2.5,1.5,1,1,3,3)
thetaVector[[2]][3,] <- c(2,1.5,2.5,1.1,1.3,3,4)
thetaVector[[2]][4,] <- c(3,0.3,3,1,-2,-3,2)
# thetaVector[[2]][5,] <- c(2,0.1,2,3,-3,-4,1,-2,3)
# thetaVector[[2]][6,] <- c(-2,1,4,0.1,-0.2,-1.1,1,0.2,2)

Delta <- list()
Delta[[1]] <- array(0,dim = c(6,11))
Delta[[2]] <- matrix(0,nrow = 4,ncol = 7)
Delta[[3]] <- matrix(0,nrow = 1,ncol = 5)

# to store actual values of computation
nodes1 <- matrix(2,nrow = 11,ncol = 1)
nodes2 <- matrix(2,nrow = 7,ncol = 1)
nodes3 <- matrix(2,nrow = 5,ncol = 1)
delta1 <- matrix(1,nrow = 11,ncol = 1)
delta2 <- matrix(1,nrow = 7,ncol = 1)
delta3 <- matrix(1,nrow = 5,ncol = 1)

# this while loop helps us to generate the coefficients for the classifier equation
# it take approximately --- minutes to run
# has to be run only once because the training set is static(constant) after which the thetaVector can store the value of coefficients
while(difference >= 0.0000000001) {
  cost <- 0
  jThetaPrev <- jTheta
  
  for (i in 1:nrts){
    # Forward Propogation
    # computing the linear sum of weights * node value and applying sigmoid to the computed value
    nodes1 <-as.numeric(training_set[i,c(-12)])                   # input layer i.e layer1
    nodes2 <- c(1,as.numeric(thetaVector[[1]] %*% nodes1))     # hidden layer1 i.e layer2
    nodes2[-1] <- 1/(1+exp(-nodes2[-1]))
    nodes3 <- c(1,as.numeric(thetaVector[[2]] %*% nodes2))     # hidden layer2 i.e layer3
    nodes3[-1] <- 1/(1+exp(-nodes3[-1]))
    h <- sum(as.numeric(thetaVector[[3]] * nodes3))               # output layer i.e layer4
    h <- 1/(1+exp(-h))
    # testing of forward chaining working well(I think so)
    # temp1 <- thetaVector[[2]][7,]    # sum(temp1 * nodes[,2])    # 1/(1+exp(0.4276424))
    Hvec[i] <- h
    cost <- as.numeric(cost - as.numeric(Yvec[i])*theLog(h) - (1-as.numeric(Yvec[i]))*theLog(1-h)) 
    
    # Backward Propogation
    delta_last <- h-Yvec[i]
    # delta(l) = transpose(theta)*delta(l+1) .* g_prime(z(l)) ;where g_prime() is derivative of activation fn
    # g_prime comes out as g_prime(z(l)) = a(l) .* (1-a(l))
    delta3 <- t(thetaVector[[3]])*delta_last * (nodes3*(1-nodes3))
    delta2 <- t(thetaVector[[2]]) %*% delta3[-1] * (nodes2*(1-nodes2))
    # delta[,1] is unused ,as there is no error in the 1st ie input layer;it is simply present for numbering purposes
    Delta[[3]] <- Delta[[3]] + (delta_last*t(nodes3))
    Delta[[2]] <- Delta[[2]] + (delta3[-1] %*% t(nodes2))
    Delta[[1]] <- Delta[[1]] + (delta2[-1] %*% t(nodes1))
  }
  jTheta <- cost / nrts
  
  Delta[[3]] <- Delta[[3]]/nrts
  Delta[[2]] <- Delta[[2]]/nrts
  Delta[[1]] <- Delta[[1]]/nrts
  
  thetaVector[[3]] <- thetaVector[[3]] - Delta[[3]]
  thetaVector[[2]] <- thetaVector[[2]] - Delta[[2]]
  thetaVector[[1]] <- thetaVector[[1]] - Delta[[1]]
  difference <- jThetaPrev - jTheta 
  print(paste (jTheta,difference, sep = " "))
}

# converting the hypothesis into yes or no inorder to check how well the equation fits the training set
Hvec <- ifelse(Hvec >= 0.5 , 1 , 0 )      
# first confusion matrix for our algorithm
confMatrix = table(Yvec, Hvec)
confMatrix

# initializing the test prediction vector
nrts1 <- nrow(test_set)
Y_pred <-vector(mode= "double" ,length = nrts1)
# test set calculations
for (i in 1:nrts1){
  # Forward Propogation
  # computing the linear sum of weights * node value and applying sigmoid to the computed value
  nodes1 <-as.numeric(test_set[i,c(-11)])                   # input layer i.e layer1
  nodes2 <- c(1,as.numeric(thetaVector[[1]] %*% nodes1))     # hidden layer1 i.e layer2
  nodes2[-1] <- 1/(1+exp(-nodes2[-1]))
  nodes3 <- c(1,as.numeric(thetaVector[[2]] %*% nodes2))     # hidden layer2 i.e layer3
  nodes3[-1] <- 1/(1+exp(-nodes3[-1]))
  h <- sum(as.numeric(thetaVector[[3]] * nodes3))               # output layer i.e layer4
  h <- 1/(1+exp(-h))
  # testing of forward chaining working well(I think so)
  # temp1 <- thetaVector[[2]][7,]    # sum(temp1 * nodes[,2])    # 1/(1+exp(0.4276424))
  Y_pred[i] <- h
}

# actual Y values
Yvec1 <- test_set$disease
# converting the hypothesis into yes or no inorder to check how well the equation fits the test set
Y_pred <- ifelse(Y_pred >= 0.5 , 1 , 0 )      
# first confusion matrix for our algorithm
confMatrix1 = table(Yvec1, Y_pred)
confMatrix1
