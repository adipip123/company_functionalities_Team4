dataset_new= read.csv('sick-euthyroid.csv',header = F)

#renaming the columns to dataset_new
dataset<- data.frame(dataset_new$V2,dataset_new$V3,dataset_new$V4,
                     dataset_new$V5,dataset_new$V6,dataset_new$V7,dataset_new$V8,
                     dataset_new$V9,dataset_new$V10,dataset_new$V11,dataset_new$V12,
                     dataset_new$V13,dataset_new$V14,dataset_new$V16,
                     dataset_new$V18,dataset_new$V20,dataset_new$V22,dataset_new$V24,dataset_new$V1)
colnames(dataset)<-c('Age','Gender','on_thyroxine','query_on_thyroxine',
                     'on_antithyroid_medication','thyroid_surgery','query_hypothyroid',
                     'query_hyperthyroid','pregnant','sick','tumor','lithium','goitre',
                     'TSH','T3','TT4','T4U','FTI','Result')


row_to_keep<-(dataset$TSH!='?')
dataset <-dataset[row_to_keep,]
rm(row_to_keep)

row_to_keep<-(dataset$T3!='?')
dataset <-dataset[row_to_keep,]
rm(row_to_keep)

row_to_keep<-(dataset$Gender!='?')
dataset <-dataset[row_to_keep,]
rm(row_to_keep)


dataset$Age=ifelse(is.na(dataset$Age),
                   ave(dataset$Age, FUN = function(x) mean(x, na.rm=TRUE)),
                   dataset$Age)

summary(dataset)

#converting to categorical columns to numeric data
dataset$Male <- ifelse(dataset$Gender=="M",1,0)
#removing Gender column
dataset<-dataset[,-2]
#removing overfitting

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
