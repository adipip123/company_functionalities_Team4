dataset_new= read.csv('sick-euthyroid.csv',header = F)
dataset_new$bias <- array(1,dim = c(nrow(dataset_new),1))
#renaming the columns to dataset_new
dataset<- data.frame(dataset_new$bias,dataset_new$V2,dataset_new$V3,dataset_new$V4,
                     dataset_new$V5,dataset_new$V6,dataset_new$V7,dataset_new$V8,
                     dataset_new$V9,dataset_new$V10,dataset_new$V11,dataset_new$V12,
                     dataset_new$V13,dataset_new$V14,dataset_new$V16,
                     dataset_new$V18,dataset_new$V20,dataset_new$V22,dataset_new$V24,dataset_new$V1)
colnames(dataset)<-c('Bias','Age','Gender','on_thyroxine','query_on_thyroxine',
                     'on_antithyroid_medication','thyroid_surgery','query_hypothyroid',
                     'query_hyperthyroid','pregnant','sick','tumor','lithium','goitre',
                     'TSH','T3','TT4','T4U','FTI','Result')
# removing missing values
row_to_keep<-(dataset$TSH!='?')
dataset <-dataset[row_to_keep,]
row_to_keep<-(dataset$T3!='?')
dataset <-dataset[row_to_keep,]
row_to_keep<-(dataset$Gender!='?')
dataset <-dataset[row_to_keep,]
#converting to categorical columns to numeric data
dataset$Gender <- ifelse(dataset$Gender=="M",1,0)
# converting from factors to numeric
dataset$Age <- as.numeric(as.character(dataset$Age))
dataset$TSH <- as.numeric(as.character(dataset$TSH))
dataset$T3 <- as.numeric(as.character(dataset$T3))
dataset$TT4 <- as.numeric(as.character(dataset$TT4))
dataset$T4U <- as.numeric(as.character(dataset$T4U))
dataset$FTI <- as.numeric(as.character(dataset$FTI))
dataset$Result<-ifelse(dataset$Result=="negative",0,1)
dataset$on_thyroxine<-ifelse(dataset$on_thyroxine=="f",0,1)
dataset$query_on_thyroxine<-ifelse(dataset$query_on_thyroxine=="f",0,1)
dataset$on_antithyroid_medication<-ifelse(dataset$on_antithyroid_medication=="f",0,1)
dataset$thyroid_surgery<-ifelse(dataset$thyroid_surgery=="f",0,1)
dataset$query_hypothyroid<-ifelse(dataset$query_hypothyroid=="f",0,1)
dataset$query_hyperthyroid<-ifelse(dataset$query_hyperthyroid=="f",0,1)
dataset$pregnant<-ifelse(dataset$pregnant=="f",0,1)
dataset$sick<-ifelse(dataset$sick=="f",0,1)
dataset$tumor<-ifelse(dataset$tumor=="f",0,1)
dataset$lithium<-ifelse(dataset$lithium=="f",0,1)
dataset$goitre<-ifelse(dataset$goitre=="f",0,1)
row_to_keep<-(!is.na(dataset$FTI))
dataset <-dataset[row_to_keep,]
# repacling 338 missing age values with median=57
dataset$Age <- ifelse(is.na(dataset$Age),median(dataset$Age,na.rm = T),dataset$Age)
# checking the dataset
summary(dataset)
# removing unwanted variables
rm(dataset_new);rm(w);rm(row_to_keep)
# feature scaling
dataset[,c(2,15,16,17,18,19)]<-scale(dataset[,c(2,15,16,17,18,19)])
mean1 <-c(mean(dataset$Age),mean(dataset$TSH),mean(dataset$T3),mean(dataset$TT4),mean(dataset$T4U),mean(dataset$FTI))
sd1 <-c(sd(dataset$Age),sd(dataset$TSH),sd(dataset$T3),sd(dataset$TT4),sd(dataset$T4U),sd(dataset$FTI))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(dataset$Result, SplitRatio = .8)
training_set <- subset(dataset, split == TRUE) #TEMP
test_set <- subset(dataset, split == FALSE)
# removing the variables not needed anymore
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
Yvec <- training_set$Result
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
thetaVector[[1]] <- array(1,dim = c(6,19))
thetaVector[[2]] <- matrix(1,nrow = 4,ncol = 7)
thetaVector[[3]] <- matrix(1,nrow = 1,ncol = 5)
thetaVector[[1]][1,] <- c(1,2,0.5,1,3,-2,3,-3,1,0.1,2.5,1.5,1,1,3,3,-4,2,0.3)
thetaVector[[1]][2,] <- c(1,2.5,1.5,1,1,3,3,-4,2,0.3,0.3,3,1,-2,-3,2,-3,4,0.4)
thetaVector[[1]][3,] <- c(2,1.5,2.5,1.1,1.3,3,4,-5,1,0.31,4,0.1,-0.2,-1.1,1,0.2,2,-2,-3.1)
thetaVector[[1]][4,] <- c(3,0.3,3,1,-2,-3,2,-3,4,0.4,0.2,-0.3,1,-2,3,-2,-1,3,-3)
thetaVector[[1]][5,] <- c(2,0.1,2,3,-3,-4,1,-2,3,0.2,0.1,2,3,-3,-4,1,-2,3,0.2)
thetaVector[[1]][6,] <- c(-2,1,4,0.1,-0.2,-1.1,1,0.2,2,-2,3,0.3,3,1,-2,-3,2,-3,4)
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
Delta[[1]] <- array(0,dim = c(6,19))
Delta[[2]] <- matrix(0,nrow = 4,ncol = 7)
Delta[[3]] <- matrix(0,nrow = 1,ncol = 5)

# to store actual values of computation
nodes1 <- matrix(2,nrow = 19,ncol = 1)
nodes2 <- matrix(2,nrow = 7,ncol = 1)
nodes3 <- matrix(2,nrow = 5,ncol = 1)
delta1 <- matrix(1,nrow = 19,ncol = 1)
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
    nodes1 <-as.numeric(training_set[i,c(-20)])                   # input layer i.e layer1
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
  nodes1 <-as.numeric(test_set[i,c(-20)])                   # input layer i.e layer1
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
Yvec1 <- test_set$Result
# converting the hypothesis into yes or no inorder to check how well the equation fits the test set
Y_pred <- ifelse(Y_pred >= 0.5 , 1 , 0 )      
# first confusion matrix for our algorithm
confMatrix1 = table(Yvec1, Y_pred)
confMatrix1

# clearing the unwanted variables to free memory
rm(cost);rm(difference)
rm(delta1);rm(delta2);rm(delta3);rm(delta_last);rm(Delta)
rm(nodes1);rm(nodes2);rm(nodes3);
rm(jTheta);rm(jThetaPrev);rm(i);rm(theLog);rm(nrts);rm(h)
rm(confMatrix);rm(confMatrix1);rm(nrts1);

# saving the vector in a .txt file
thetaVectorSave <- c(0.38574838,-0.07202112,-0.43727087,-3.15996655,-2.19736352,-0.04694301,1.18733454,-1.29032897,-0.04416490,-0.23220314,2.17574439)
write(thetaVectorSave, file = "cardiology_values.txt",
      ncolumns = if(is.character(thetaVectorSave)) 1 else 11,
      append = FALSE, sep = " ")

# scale_vector <-c(47.932692,8.058679 ,133.629808 ,17.469434 ,137.581731 ,23.934150)
scale_vector <-c(mean(dataset$age) , sd(dataset$age),
                 mean(dataset$rest_bpress) , sd(dataset$rest_bpress),
                 mean(dataset$max_heart_rate) , sd(dataset$max_heart_rate))

write(scale_vector, file = "cardiology_scaling.txt",
      ncolumns = if(is.character(scale_vector)) 1 else 11,
      append = FALSE, sep = " ")

# # checking the value of the last row of the test set(which is the user value)
ifelse(y_prediction[tsa]==1,print("You are diagnosed with thyroid.Please take care."),
       print("Your test reports are negative.Thank you!"))