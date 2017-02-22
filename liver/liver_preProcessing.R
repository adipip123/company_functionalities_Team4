dataset_new= read.csv('Indian Liver Patient Dataset (ILPD).csv',header = F)
#renaming the columns to dataset_new
dataset<- data.frame(dataset_new$V1,dataset_new$V2,dataset_new$V3,dataset_new$V4,dataset_new$V5,dataset_new$V6,
                     dataset_new$V7,dataset_new$V8,dataset_new$V9,dataset_new$V10,dataset_new$)
colnames(dataset)<-c('Age','Gender','TB','DB','Alkphos','Sgpt','Sgot','TP','ALB','A_G_Ratio','Selector')
#converting to categorical columns to numeric data
dataset$Male <- ifelse(dataset$Gender=="Male",1,0)
dataset$Female <- ifelse(dataset$Gender=='Female',1,0)
#removing Gender column
dataset<-dataset[,-2]
#removing overfitting
dataset<-dataset[,-12]
dataset$Selector<-ifelse(dataset$Selector==2,0,1)
rm(dataset_new)
dataset<-data.frame(dataset$Age,dataset$Male,dataset$TB,dataset$DB,
                    dataset$Alkphos,dataset$Sgpt,dataset$Sgot,dataset$TP,
                    dataset$ALB,dataset$A_G_Ratio,dataset$Selector)
colnames(dataset)<-c("Age","Male","TB","DB","Alkphos","Sgpt","Sgot","TP","ALB","A_G_Ratio","Selector")
dataset[,c(1,3,4,5,6,7,8,9,10)]<-scale(dataset[,c(1,3,4,5,6,7,8,9,10)])
summary(dataset)

row_to_keep <- !is.na(dataset$A_G_Ratio)
# row_to_keep<-(dataset$rest_electro )
dataset <-dataset[row_to_keep,]

#install.packages('caTools')
#library(caTools)

#set.seed(123)
#split <- sample.split(dataset$Selector, SplitRatio = .8)
#training_set <- subset(dataset, split == TRUE)
#test_set <- subset(dataset, split == FALSE)


#emoving the variables not needed anymore
rm(split)
# rm(dataset_backup)
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
cost <- 0
alpha <- 3

#if you increase alpha beyond this, jtheta will diverge

jThetaPrev <- 100000
jTheta <- 100
difference <- 10
thetaVector <- c(1,1,1,1,1,1,1,1,1,1,1)
nrts <- nrow(dataset)
Hvec <-vector(mode= "double" ,length = nrts)
# temp <-vector(mode= "double" ,length = nrts)
# this while loop helps us to generate the coefficients for the classifier equation
# it take approximately 2-3 minutes to run
# has to be run only once because the training set is static(constant)
# after which the thetaVector can store the value of coefficients

X1vec <- dataset[,1]
X2vec <- dataset[,2]
X3vec <- dataset[,3]
X4vec <- dataset[,4]
X5vec <- dataset[,5]
X6vec <- dataset[,6]
X7vec <- dataset[,7]
X8vec <- dataset[,8]
X9vec <- dataset[,9]
X10vec <- dataset[,10]
Yvec <- dataset[,11]

while(difference != 0) {
  cost <- 0
  jThetaPrev <- jTheta
  
  
  for (i in 1:nrts){
    
    thetaX <- thetaVector[1] + thetaVector[2]*as.numeric(X1vec[i]) + thetaVector[3]*as.numeric(X2vec[i]) +
      thetaVector[4]*as.numeric(X3vec[i]) + thetaVector[5]*as.numeric(X4vec[i]) + thetaVector[6]*as.numeric(X5vec[i]) +
      thetaVector[7]*as.numeric(X6vec[i]) +thetaVector[8]*as.numeric(X7vec[i]) + thetaVector[9]*as.numeric(X8vec[i]) + 
      thetaVector[10]*as.numeric(X9vec[i]) + thetaVector[11]*as.numeric(X10vec[i])
    h <- 1/(1+exp(-thetaX))
    Hvec[i] <- h
    cost <- as.numeric(cost - as.numeric(Yvec[i])*theLog(h) - (1-as.numeric(Yvec[i]))*theLog(1-h)) 
  }
  jTheta <- cost / nrts
  
  temp <- (Hvec-Yvec)
  d0Vector <- temp      #this vector has no multiplicand as it is related to the intercept and no column
  d1Vector <- temp*X1vec
  d2Vector <- temp*X2vec
  d3Vector <- temp*X3vec
  d4Vector <- temp*X4vec
  d5Vector <- temp*X5vec
  d6Vector <- temp*X6vec
  d7Vector <- temp*X7vec
  d8Vector <- temp*X8vec
  d9Vector <- temp*X9vec
  d10Vector <- temp*X10vec
  
  d0 <- sum(d0Vector)/nrts
  d1 <- sum(d1Vector)/nrts
  d2 <- sum(d2Vector)/nrts
  d3 <- sum(d3Vector)/nrts
  d4 <- sum(d4Vector)/nrts
  d5 <- sum(d5Vector)/nrts
  d6 <- sum(d6Vector)/nrts
  d7 <- sum(d7Vector)/nrts
  d8 <- sum(d8Vector)/nrts
  d9 <- sum(d9Vector)/nrts
  d10 <- sum(d10Vector)/nrts
  
  thetaVector[1] <- thetaVector[1] - alpha*d0
  thetaVector[2] <- thetaVector[2] - alpha*d1
  thetaVector[3] <- thetaVector[3] - alpha*d2
  thetaVector[4] <- thetaVector[4] - alpha*d3
  thetaVector[5] <- thetaVector[5] - alpha*d4
  thetaVector[6] <- thetaVector[6] - alpha*d5
  thetaVector[7] <- thetaVector[7] - alpha*d6
  thetaVector[8] <- thetaVector[8] - alpha*d7
  thetaVector[9] <- thetaVector[9] - alpha*d8
  thetaVector[10] <- thetaVector[10] - alpha*d9
  thetaVector[11] <- thetaVector[11] -alpha*d10
  difference <- jThetaPrev - jTheta 
  print(paste (jTheta,difference, sep = " "))
}


Hvec <- ifelse(Hvec >= 0.5 , 1 , 0 )      
# first confusion matrix for our algorithm
confMatrix = table(Yvec, Hvec)
confMatrix
