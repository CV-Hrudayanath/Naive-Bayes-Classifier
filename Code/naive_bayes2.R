data <- read.csv(file = "cancer_actual.data" ,header = FALSE, sep = ",")
#data$V2[1]

index <- 1:683
index <- sample(index)
train_set <- index[1:341]
test_set <- index[342:683]

train_size <- 341

mean_matrix <- matrix(nrow = 10 , ncol = 2)
sd_matrix <- matrix(nrow = 10 , ncol = 2)
confusion_matrix <- matrix(0, nrow = 2, ncol = 2)
dimnames(confusion_matrix) = list(c("benign","malignant"),c("benign","malignant"))

class1<-0
class2<-0


for (i in 2:10) {
     column <- data[i]
     count1 <- 0
     count2 <- 0
     for ( j in train_set)
     {
         if(data$V11[j] == 2)
         {
           class1[count1] =  column[j,1]
           count1 <- count1 + 1
           print('benign')
         }
         else
         {
           class2[count2] = column[j,1]
           count2 <- count2 + 1
           print('malignant')
           
         }
     }
     mean_matrix[i,1] <- mean(class1)
     sd_matrix[i,1] <- sd(class1)
     
     mean_matrix[i,2] <- mean(class2)
     sd_matrix[i,2] <- sd(class2)
     
     
     
     #me[i] <- mean(column[,1])
     #sd[i] <- sd(column[,1])
     print(mean_matrix)
     print(sd_matrix)
}

# Test data classification using naive bayes classifier

correct<-0
for( i in test_set)
{
     print(i)
     ans1<-0
     ans2<-0
     likelihood_benign <-1
     likelihood_malignant <- 1
     for(j in 2:10)
     {
         
         column <- data[j]
         x <- column[i,1]
         # Likelihood of Benign
         ans1[j-1] <- gaussianpdf(mean_matrix[j,1], sd_matrix[j,1], x)
         likelihood_benign <- likelihood_benign*ans1[j-1] 
         #print(ans1)
         
         # Likelihood of malignant
         ans2[j-1] <- gaussianpdf(mean_matrix[j,2],sd_matrix[j,2],x)
         likelihood_malignant <- likelihood_malignant*ans2[j-1]
         #print(ans2)
     }
     prob_benign <- likelihood_benign/(likelihood_benign+likelihood_malignant)
     prob_malignant <- likelihood_malignant/(likelihood_malignant+likelihood_benign)
     #print('prob.. are')
     print(prob_benign)
     print(prob_malignant)
     if(prob_benign > prob_malignant)
     {
         # Test sample belongs to benign class
         final_class <- 2
     }
     else
     {
       final_class <- 4
     }
     
     if( final_class == data$V11[i])
     {
           correct <- correct + 1
     }
     else
     {
       print(data[i,])
     }
     confusion_matrix[(data$V11[i])/2 , final_class/2] <- confusion_matrix[(data$V11[i])/2,final_class/2] + 1
  
}
print("No of correctly matched")
print(correct)
accuracy <- correct/train_size
print("Accuracy is")
print(accuracy)
print("confusion matrix")
print(confusion_matrix)