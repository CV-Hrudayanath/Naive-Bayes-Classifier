gaussianpdf<-function(mean, standard_deviation , input)
{
   ans <- 1/(standard_deviation*sqrt(2*pi))
   ans <- ans * exp((-0.5)*(((input - mean)*(input - mean))/(standard_deviation*standard_deviation)))
   return (ans)
}


