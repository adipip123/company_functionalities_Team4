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
thetaVector[[1]] <- array(1,dim = c(10,11))
thetaVector[[2]] <- matrix(1,nrow = 10,ncol = 11)
thetaVector[[3]] <- matrix(1,nrow = 1,ncol = 11)
thetaVector[[1]][1,] <- c(1,2,0.5,1,3,-2,3,-3,1,0.1,-1)
thetaVector[[1]][2,] <- c(1,2.5,1.5,1,1,3,3,-4,2,0.3,-0.7)
thetaVector[[1]][3,] <- c(2,1.5,2.5,1.1,1.3,3,4,-5,1,0.3,-0.7)
thetaVector[[1]][4,] <- c(3,0.3,3,1,-2,-3,2,-3,4,0.4,-1)
thetaVector[[1]][5,] <- c(2,0.1,2,3,-3,-4,1,-2,3,0.2,-0.4)
thetaVector[[1]][6,] <- c(-2,1,4,0.1,-0.2,-1.1,1,0.2,2,-2,-1)
thetaVector[[1]][7,] <- c(-2,1,0.3,2.1,-4,-3,2,3,1,0,3)
thetaVector[[1]][8,] <- c(-2,1,3,1.1,-2,-2,-1,0.2,-1,2,1)
thetaVector[[1]][9,] <- c(1,-2,3,-2.1,2,-3,-1,1.2,1,4,-2)
thetaVector[[1]][10,] <- c(1,-4,3,-2.1,1,4,1,1,1,4,-1)
thetaVector[[2]] <- thetaVector[[1]]

Delta <- list()
Delta[[1]] <- array(0,dim = c(10,11))
Delta[[2]] <- matrix(0,nrow = 10,ncol = 11)
Delta[[3]] <- matrix(0,nrow = 1,ncol = 11)

# to store actual values of computation
nodes <- matrix(2,nrow = 11,ncol = 3)
delta <- matrix(1,nrow = 11,ncol = 3)
# this while loop helps us to generate the coefficients for the classifier equation
# it take approximately --- minutes to run
# has to be run only once because the training set is static(constant) after which the thetaVector can store the value of coefficients
while(difference >= 0.0000000001) {
  cost <- 0
  jThetaPrev <- jTheta
  
  for (i in 1:nrts){
    # Forward Propogation
    # computing the linear sum of weights * node value and applying sigmoid to the computed value
    nodes[,1] <-as.numeric(training_set[i,c(-12)])                   # input layer i.e layer1
    nodes[,2] <- c(1,as.numeric(thetaVector[[1]] %*% nodes[,1]))     # hidden layer1 i.e layer2
    nodes[c(-1),2] <- 1/(1+exp(-nodes[c(-1),2]))
    nodes[,3] <- c(1,as.numeric(thetaVector[[2]] %*% nodes[,2]))     # hidden layer2 i.e layer3
    nodes[c(-1),3] <- 1/(1+exp(-nodes[c(-1),3]))
    h <- sum(as.numeric(thetaVector[[3]] * nodes[,3]))               # output layer i.e layer4
    h <- 1/(1+exp(-h))
    # testing of forward chaining working well(I think so)
    # temp1 <- thetaVector[[2]][7,]    # sum(temp1 * nodes[,2])    # 1/(1+exp(0.4276424))
    Hvec[i] <- h
    cost <- as.numeric(cost - as.numeric(Yvec[i])*theLog(h) - (1-as.numeric(Yvec[i]))*theLog(1-h)) 
    
    # Backward Propogation
    delta_last <- h-Yvec[i]
    # delta(l) = transpose(theta)*delta(l+1) .* g_prime(z(l)) ;where g_prime() is derivative of activation fn
    # g_prime comes out as g_prime(z(l)) = a(l) .* (1-a(l))
    delta[,3] <- t(thetaVector[[3]])*delta_last * (nodes[,3]*(1-nodes[,3]))
    delta[,2] <- t(thetaVector[[2]]) %*% delta[c(-1),3] * (nodes[,2]*(1-nodes[,2]))
    # delta[,1] is unused ,as there is no error in the 1st ie input layer;it is simply present for numbering purposes
    Delta[[3]] <- Delta[[3]] + (delta_last*t(nodes[,3]))
    Delta[[2]] <- Delta[[2]] + (delta[c(-1),3] %*% t(nodes[,2]))
    Delta[[1]] <- Delta[[1]] + (delta[c(-1),2] %*% t(nodes[,1]))
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

# clearing the unwanted variables to free memory
rm(temp);rm(temp_vect)
rm(d0);rm(d1);rm(d2);rm(d3);rm(d4);rm(d5);rm(d6);rm(d7);rm(d8);rm(d9);rm(d10)
rm(x1);rm(x2);rm(x3);rm(x4);rm(x5);rm(x6);rm(x7);rm(x8);rm(x9);rm(x10)
rm(difference);rm(jTheta);rm(jThetaPrev);rm(i);rm(alpha);rm(cost);rm(nrts);rm(y);rm(h);rm(thetaX)
rm(X1vec);rm(X2vec);rm(X3vec);rm(X4vec);rm(X5vec);rm(X6vec);rm(X7vec);rm(X8vec);rm(X9vec);rm(X10vec)
rm(d0Vector);rm(d1Vector);rm(d2Vector);rm(d3Vector);rm(d4Vector);rm(d5Vector);rm(d6Vector);rm(d7Vector);rm(d8Vector);rm(d9Vector);rm(d10Vector)

# converting the hypothesis into yes or no inorder to check how well the equation fits the training set
Hvec <- ifelse(Hvec >= 0.5 , 1 , 0 )      
# first confusion matrix for our algorithm
confMatrix = table(Yvec, Hvec)
confMatrix


# initializing the test prediction vector
Y_pred <-vector(mode= "double" ,length = nrts)
# test set calculations
for (i in 1:nrts){
  # Forward Propogation
  # computing the linear sum of weights * node value and applying sigmoid to the computed value
  nodes[,1] <-as.numeric(test_set[i,c(-12)])                   # input layer i.e layer1
  nodes[,2] <- c(1,as.numeric(thetaVector[[1]] %*% nodes[,1]))     # hidden layer1 i.e layer2
  nodes[c(-1),2] <- 1/(1+exp(-nodes[c(-1),2]))
  nodes[,3] <- c(1,as.numeric(thetaVector[[2]] %*% nodes[,2]))     # hidden layer2 i.e layer3
  nodes[c(-1),3] <- 1/(1+exp(-nodes[c(-1),3]))
  h <- sum(as.numeric(thetaVector[[3]] * nodes[,3]))               # output layer i.e layer4
  h <- 1/(1+exp(-h))
  Y_pred[i] <- h
}

# actual Y values
Yvec1 <- test_set$disease
# converting the hypothesis into yes or no inorder to check how well the equation fits the test set
Y_pred <- ifelse(Y_pred >= 0.5 , 1 , 0 )      
# first confusion matrix for our algorithm
confMatrix1 = table(Yvec1, Y_pred)
confMatrix1

# # saving the vector in a .txt file
# thetaVectorSave <- c(0.38574838,-0.07202112,-0.43727087,-3.15996655,-2.19736352,-0.04694301,1.18733454,-1.29032897,-0.04416490,-0.23220314,2.17574439)
# write(thetaVectorSave, file = "cardiology_values.txt",
#       ncolumns = if(is.character(thetaVectorSave)) 1 else 11,
#       append = FALSE, sep = " ")
# # scale_vector <-c(47.932692,8.058679 ,133.629808 ,17.469434 ,137.581731 ,23.934150)
# scale_vector <-c(mean(dataset$age) , sd(dataset$age),
#                  mean(dataset$rest_bpress) , sd(dataset$rest_bpress),
#                  mean(dataset$max_heart_rate) , sd(dataset$max_heart_rate))
# write(scale_vector, file = "cardiology_scaling.txt",
#       ncolumns = if(is.character(scale_vector)) 1 else 11,
#       append = FALSE, sep = " ")

# now that we have found the values of theta,we can use it to for an equation for predicting 
# the condition of the test case
# test_set[, c(1,5,9)] <- scale(test_set[, c(1,5,9)])

# # converting the hypothesis into class values for better prediction
# y_prediction <- ifelse(hypothesis_sigmoid >= 0.5 , 1 , 0 )      
# # first confusion matrix for our algorithm
# confMatrix_test = table(test_set[,11], y_prediction)
# confMatrix_test

# clearing the unwanted variables to free memory
# rm(thetaX_test_set)
# rm(tp_variable1);rm(tp_variable2);rm(tp_variable3);rm(tp_variable4);rm(tp_variable5)
# rm(tp_variable6);rm(tp_variable7);rm(tp_variable8);rm(tp_variable9);rm(tp_variable10)

# # checking the value of the last row of the test set(which is the user value)
# ifelse(y_prediction[tsa]==1,print("You are diagnosed with a heart disease!Please take care."),
#        print("Your test reports are negative.Thank you!"))
