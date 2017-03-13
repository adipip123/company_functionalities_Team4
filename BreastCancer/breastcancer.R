# Importing the dataset
dataset <- read.csv("breast-cancer-wisconsin.csv",header = F)
# dataset_backup <- dataset
dataset<-dataset[,-1]

# creating a new dataset to rearrange the columns
dataset <-data.frame(dataset$V2,dataset$V3,dataset$V4,dataset$V5,
                         dataset$V6,dataset$V7,dataset$V8,dataset$V9,
                         dataset$V10,dataset$V11)
colnames(dataset)<-c("Clump_Thickness","CellSizeUniformity","CellShapeUniformity","Marginal_Adhesion",
                     "Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Result")
dataset$bias <- array(1,dim = c(nrow(dataset),1))
dataset <-data.frame(dataset$bias,dataset$Clump_Thickness,dataset$CellSizeUniformity,dataset$CellShapeUniformity,
                     dataset$Marginal_Adhesion,dataset$Single_Epithelial_Cell_Size,dataset$Bare_Nuclei,dataset$Bland_Chromatin,
                     dataset$Normal_Nucleoli,dataset$Mitoses,dataset$Result)
colnames(dataset)<-c("Bias","Clump_Thickness","CellSizeUniformity","CellShapeUniformity","Marginal_Adhesion",
                            "Single_Epithelial_Cell_Size","Bare_Nuclei","Bland_Chromatin","Normal_Nucleoli","Mitoses","Result")


dataset$Result <- ifelse(dataset$Result==2,0,1)
row_to_keep<-(dataset$Bare_Nuclei!='?')
dataset <-dataset[row_to_keep,]
# converting one factor column into numeric
dataset$Bare_Nuclei <- as.numeric(as.character(dataset$Bare_Nuclei))

# checking the dataset for factors and anomalies before splitting
# summary(dataset_new)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# feature scaling
mean1 <-c(mean(dataset$Clump_Thickness),mean(dataset$CellSizeUniformity),mean(dataset$CellShapeUniformity),
          mean(dataset$Marginal_Adhesion),mean(dataset$Single_Epithelial_Cell_Size),mean(dataset$Bare_Nuclei),
          mean(dataset$Bland_Chromatin),mean(dataset$Normal_Nucleoli),mean(dataset$Mitoses))
sd1 <- c(sd(dataset$Clump_Thickness),sd(dataset$CellSizeUniformity),sd(dataset$CellShapeUniformity),
         sd(dataset$Marginal_Adhesion),sd(dataset$Single_Epithelial_Cell_Size),sd(dataset$Bare_Nuclei),
         sd(dataset$Bland_Chromatin),sd(dataset$Normal_Nucleoli),sd(dataset$Mitoses))

dataset[,c(-1,-11)] <- scale(dataset[, c(-1,-11)])
library(caTools)
set.seed(123)
split <- sample.split(dataset$Result, SplitRatio = .8)
training_set <- subset(dataset, split == TRUE) #TEMP
test_set <- subset(dataset, split == FALSE)

# removing the variables not needed anymore
rm(row_to_keep)
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
thetaVector[[1]] <- array(1,dim = c(6,10))
thetaVector[[2]] <- matrix(1,nrow = 4,ncol = 7)
thetaVector[[3]] <- matrix(1,nrow = 1,ncol = 5)
thetaVector[[1]][1,] <- c(1,2,0.5,1,3,-2,3,-3,1,0.1)
thetaVector[[1]][2,] <- c(1,2.5,1.5,1,1,3,3,-4,2,0.3)
thetaVector[[1]][3,] <- c(2,1.5,2.5,1.1,1.3,3,4,-5,1,0.3)
thetaVector[[1]][4,] <- c(3,0.3,3,1,-2,-3,2,-3,4,0.4)
thetaVector[[1]][5,] <- c(2,0.1,2,3,-3,-4,1,-2,3,0.2)
thetaVector[[1]][6,] <- c(-2,1,4,0.1,-0.2,-1.1,1,0.2,2,-2)
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
Delta[[1]] <- array(0,dim = c(6,10))
Delta[[2]] <- matrix(0,nrow = 4,ncol = 7)
Delta[[3]] <- matrix(0,nrow = 1,ncol = 5)

# to store actual values of computation
nodes1 <- matrix(2,nrow = 10,ncol = 1)
nodes2 <- matrix(2,nrow = 7,ncol = 1)
nodes3 <- matrix(2,nrow = 5,ncol = 1)
delta1 <- matrix(1,nrow = 10,ncol = 1)
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
    nodes1 <-as.numeric(training_set[i,c(-10)])                   # input layer i.e layer1
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
  nodes1 <-as.numeric(test_set[i,c(-10)])                   # input layer i.e layer1
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

thetaVector[[1]][1,] <- c( 0.8994418, 2.305433,0.6608289, 1.2135491, 2.6736151,-2.061957, 2.9914579,-2.58318003,1.508388, 1.31004988)
thetaVector[[1]][2,] <- c(-0.8318523, 2.304946,1.0765075, 0.9392233, 0.5463855, 1.007979, 3.9487657,-3.07417550,2.062788, 3.81668193)
thetaVector[[1]][3,] <- c( 2.2123960, 1.617580,2.4704539, 1.0836505, 0.9110368, 2.594223, 4.4523135,-4.60819612,1.207236, 1.51936531)
thetaVector[[1]][4,] <- c( 3.9713265,-0.338702,2.5003158, 0.1665377,-1.6454859,-2.758862, 1.1059235,-3.96152141,3.615237,-0.37543658)
thetaVector[[1]][5,] <- c( 4.6622560,-1.047402,0.7647223, 1.2439348,-2.6128863,-2.765602,-0.9145395,-2.82973174,2.625558,-1.05261068)
thetaVector[[1]][6,] <- c(-3.0935496, 1.068330,3.6763729,-0.4351644,-0.5528682,-1.690256, 1.4366553, 0.07022067,1.642077,-0.03984931)

thetaVector[[2]][1,] <- c(-0.3135374,1.924870,0.288705,0.5729178, 2.213567,-2.799968,2.992319)
thetaVector[[2]][2,] <- c( 1.3180953,2.524378,1.603968,1.1343791, 1.067275, 3.084568,3.003460)
thetaVector[[2]][3,] <- c( 2.1218275,1.514147,2.525344,1.1409179, 1.325650, 3.034326,4.001421)
thetaVector[[2]][4,] <- c( 0.3711327,1.969175,4.632560,0.4365460,-2.395922,-3.400496,2.433149)

thetaVector[[3]][1,] <- c(-2.505007,1.36726,-2.166541,-2.387827,9.395507)

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
ifelse(y_prediction[tsa]==1,print("You are diagnosed with a heart disease!Please take care."),
       print("Your test reports are negative.Thank you!"))