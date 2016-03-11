data <- read.csv(file = "bank-additional-full.csv" ,header = TRUE, sep = ";")

job = matrix( 0 , nrow = 12 , ncol = 2)
dimnames(job) = list(c("admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")
, c("no", "yes"))


marital = matrix(0 ,nrow = 4 , ncol = 2)
dimnames(marital) = list(c("divorced","married","single","unknown"),c("no","yes"))

education = matrix(0, nrow =8 , ncol = 2)
dimnames(education) = list(c("basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown"),c("no","yes"))

default_attr = matrix(0, nrow = 3 , ncol = 2)
dimnames(default_attr) = list(c("no","unknown","yes"),c("no","yes"))

housing = matrix(0, nrow =3 , ncol =2)
dimnames(housing) = list(c("no","unknown","yes"),c("no","yes"))

loan = matrix(0, nrow =3 , ncol =2)
dimnames(loan) = list(c("no","unknown","yes"),c("no","yes"))

contact = matrix(0, nrow = 2, ncol = 2)
dimnames(contact) = list(c("cellular","telephone"),c("no","yes"))

month = matrix(0, nrow = 10 , ncol = 2)
dimnames(month) = list(c("apr","aug","dec","jul","jun","mar","may","nov","oct","sep"),c("no","yes"))

day = matrix(0, nrow = 5 , ncol = 2)
dimnames(day) = list(c("fri","mon","thu","tue","wed"),c("no","yes"))

poutcome = matrix(0, nrow =3 , ncol = 2)
dimnames(poutcome) = list(c("failure","nonexistent","success"),c("no","yes"))

confusion_matrix = matrix(0, nrow = 2 , ncol = 2)
dimnames(confusion_matrix) = list(c("no","yes"),c("no","yes"))


index <- 1:41188
index <- sample(index)
train_set <- index[1:20594]
test_set <- index[20594:41188]

train_size <- 20594



class_count <- 0 
class_count[1] <- 0 
class_count[2] <- 0
for (i in train_set)
{
    p1 <- data$marital[i]
    p2 <- data$job[i]
    p3 <- data$education[i]
    p4 <- data$default[i]
    p5 <- data$housing[i]
    p6 <- data$loan[i]
    p7 <- data$contact[i]
    p8 <- data$month[i]
    p9 <- data$day_of_week[i]
    p10 <- data$poutcome
    
    out <- data$y[i]
    marital[p1,out] <- marital[p1,out] + 1
    job[p2,out] <- job[p2,out] + 1
    education[p3,out] <- education[p3,out] + 1
    default_attr[p4,out] <- default_attr[p4,out] + 1
    housing[p5,out] <- housing[p5,out] + 1
    loan[p6,out] <- loan[p6,out] + 1
    contact[p7,out] <- contact[p7,out] + 1
    month[p8,out] <- month[p8,out] + 1
    day[p9,out] <- day[p9,out] + 1
    poutcome[p10,out] <- poutcome[p10,out] + 1
    
    
    if( out == "no")
    {
      class_count[1] <- class_count[1] + 1
    }
    else
    {
      class_count[2] <- class_count[2] + 1
    }
    #print(marital)
    
}
print(class_count[1])
print(class_count[2])
total_count <- class_count[1] + class_count[2]

print(marital)
for( j in 1:2)
{
  marital[,j] <- marital[,j]/class_count[j]
  job[,j] <- job[,j]/class_count[j]
  education[,j] <- education[,j]/class_count[j]
  default_attr[,j] <- default_attr[,j]/class_count[j]
  housing[,j] <- housing[,j]/class_count[j]
  loan[,j] <- loan[,j]/class_count[j]
  contact[,j] <- contact[,j]/class_count[j]
  month[,j] <- month[,j]/class_count[j]
  day[,j] <- day[,j]/class_count[j]
  poutcome[,j] <- poutcome[,j]/class_count[j]
}
#print(marital)

print(marital)
print(job)
print(education)
print(default_attr)
print(housing)
print(loan)
print(contact)

correct <- 0

for (i in test_set) 
{
  p1 <- data$marital[i]
  p2 <- data$job[i]
  p3 <- data$education[i]
  p4 <- data$default[i]
  p5 <- data$housing[i]
  p6 <- data$loan[i]
  p7 <- data$contact[i]
  p8 <- data$month[i]
  p9 <- data$day_of_week[i]
  p10 <- data$poutcome[i]
  
  #print(default)
  
  # Likelihood of no
  prob_no <- marital[p1,1]*job[p2,1]*education[p3,1]*default_attr[p4,1]*housing[p5,1]*loan[p6,1]*contact[p7,1]*month[p8,1]*day[p9,1]*poutcome[p10,1]*(class_count[1]/total_count)
  
  # Likelihood of yes
  prob_yes <- marital[p1,2]*job[p2,2]*education[p3,2]*default_attr[p4,2]*housing[p5,2]*loan[p6,2]*contact[p7,2]*month[p8,1]*day[p9,1]*poutcome[p10,1]*(class_count[2]/total_count)
  
  total_prob <- prob_no + prob_yes
  #print(prob_no)
  #print(prob_yes)
  prob_no <- prob_no / total_prob
  prob_yes <- prob_yes / total_prob

  if(prob_no >= prob_yes)
  {
    class_label <- "no"
  }
  else
  {
    class_label <- "yes"
  }
  if( data$y[i] == class_label)
  {
      correct <- correct + 1
  }
  #print(data$y[i])
  #print(class_label)
  confusion_matrix[data$y[i],class_label] <- confusion_matrix[data$y[i],class_label] + 1
  #print(prob_no)
  #print(prob_yes)
  print(i)
  
}
print(correct)
print(confusion_matrix)
