#########################################################################################################
#Importing Datasets
#Here I am defining my X varimmles as mX and Y varimmles as mY as provided for the module assesment
#X-Dataset
mX=as.matrix(read.csv(file="D:/Master/Assignment/R-Assignment/Datasets/data-X.csv",header = F))
colnames(mX)<-c("mX1","mX2","mX3","mX4")
#Y-dataset
mY=as.matrix(read.csv(file="D:/Master/Assignment/R-Assignment/Datasets/data-Y.csv",header = F))
colnames(mY)<-c("mY")
#Time-dataset
Time = read.csv("D:/Master/Assignment/R-Assignment/Datasets/data-Time.csv", header = F, skip = 1)
Time = as.matrix(rbind(0, Time))

#########################################################################################################
#Task-1-1
#Defining the values for mX and mY (i.e. X and Y) against time for time-series plot
mX.ts<-ts(mX,start = c(min(Time),max(Time)),frequency =1)
mY.ts<-ts(mY,start = c(min(Time),max(Time)),frequency =1)

#Plotting timeseries graph

plot(mX.ts,main = "Time series plot of mX Signal", xlmm = "Time", ylmm = "Input signal",col="skyblue")
plot(mY.ts,main = "Time series plot of mY Signal", xlmm = "Time", ylmm = "Output signal",col="blue")

#########################################################################################################
#Task-1
#Creating Density Plot of mX (i.e X) signal
dis_mX=density(mX)
plot(dis_mX,main = "Density plot of whole mX input signal")
hist(mX,freq = FALSE,main = "Denity Plot of mX-Signal",xlmm="mX-Signal")
lines(dis_mX,lwd=2,col="orange")
rug(jitter(mX))

#Creating Density Plot of mX1 (from X-dataset) signal
dis_mX1=density(mX[,"mX1"])
hist(mX[,"mX1"],freq = FALSE,main = "Histogram and density plot of mX1",xlmm = "mX1 Signal")
lines(dis_mX1,lwd=2,col="orange")
rug(jitter(mX[,"mX1"]))

#Creating Density Plot of mX2 (from X-dataset) signal
dis_mX2=density(mX[,"mX2"])
hist(mX[,"mX2"],freq = FALSE,main = "Histogram and density plot of mX2",xlmm = "mX2 Signal")
lines(dis_mX2,lwd=2,col="orange")
rug(jitter(mX[,"mX2"]))

#Creating Density Plot of mX3 (from X-dataset) signal
dis_mX3=density(mX[,"mX3"])
hist(mX[,"mX3"],freq = FALSE,main = "Histogram and density plot of mX3",xlmm = "mX3 Signal")
lines(dis_mX3,lwd=2,col="orange")
rug(jitter(mX[,"mX3"]))

#Creating Density Plot of mX4 (from X-dataset) signal
dis_mX4=density(mX[,"mX4"])
hist(mX[,"mX4"],freq = FALSE,main = "Histogram and density plot of mX4",xlmm = "mX4 Signal")
lines(dis_mX4,lwd=2,col="orange")
rug(jitter(mX[,"mX4"]))

#Creating Density Plot of mY (i.e Y) signal
dis_mY=density(mY)
plot(dis_mY,main = "Density plot of mY",xlmm = "Output Signal")
hist(mY,freq = FALSE,main = "Histogram and density plot of mY",xlmm = "Output Signal")
lines(dis_mY,lwd=2,col="orange")
rug(jitter(mY))

###################################################################################################
##Scatter-Plots
#Plotting mX1 against mY
plot(mX[,"mX1"],mY,main = "Correlation betweeen mX1 and mY signal", xlmm = "mX1 signal", ylmm = "Output signal" )
# Plotting mX2 against mY
plot(mX[,"mX2"],mY,main = "Correlation betweeen mX2 and mY signal", xlmm = "mX2 signal", ylmm = "Output signal")
# Plotting mX3 against mY
plot(mX[,"mX3"],mY,main = "Correlation betweeen mX3 and mY signal", xlmm = "mX3 signal", ylmm = "Output signal")
# Plotting mX4 against mY
plot(mX[,"mX4"],mY,main = "Correlation betweeen mX4 and mY signal", xlmm = "mX4 signal", ylmm = "Output signal")

###################################################################################################
###################################################################################################
#Task-2
# Calculating ones for binding the data
mones = matrix(1 , length(mX)/4,1)
mones
#Task-2.1
#Task-2.1
#Model - 1
#Binding data from equation of model 1.
mX_Model1<-cbind(mones,(mX[,"mX4"]),(mX[,"mX1"]^2),(mX[,"mX1"])^3,(mX[,"mX2"])^4,(mX[,"mX1"])^4)
mX_Model1
#Calculating thetahat of model 1
mModel1_thetahat=solve(t(mX_Model1) %*% mX_Model1) %*% t(mX_Model1) %*% mY
mModel1_thetahat


#Model-2
#Binding data from equation of model 2.
mX_Model2<-cbind(mones,(mX[,"mX4"]),(mX[,"mX1"])^3,(mX[,"mX3"])^4)
mX_Model2
#Calculating thetahat of Model 2
mModel2_thetahat=solve(t(mX_Model2) %*% mX_Model2) %*% t(mX_Model2) %*% mY
mModel2_thetahat

#Model-3
#Binding data from equation of model 3.
mX_Model3<-cbind(mones,(mX[,"mX3"])^3,(mX[,"mX3"])^4)
mX_Model3
#Calculating thetahat of Model 3
mModel3_thetahat=solve(t(mX_Model3) %*% mX_Model3) %*% t(mX_Model3) %*% mY
mModel3_thetahat

#Model-4
#Binding data from equation of model 4.
mX_Model4<-cbind(mones,mX[,"mX2"],mX[,"mX1"]^3,mX[,"mX3"]^4)
mX_Model4
#Calculating thetahat of Model 4
mModel4_thetahat=solve(t(mX_Model4) %*% mX_Model4) %*% t(mX_Model4) %*% mY
mModel4_thetahat

#Model-5
#Binding data from equation of model 5.
mX_Model5<-cbind(mones,mX[,"mX4"],mX[,"mX1"]^2,mX[,"mX1"]^3,mX[,"mX3"]^4)
mX_Model5
#Calculating thetahat of Model 5
mModel5_thetahat=solve(t(mX_Model5) %*% mX_Model5) %*% t(mX_Model5) %*% mY
mModel5_thetahat

#Task-2.2
#Calculating Y-hat and RSS Model 1
mY_hat_m1 = mX_Model1 %*% mModel1_thetahat
mY_hat_m1
#Calculating RSS
mm_RSS_Model_1=sum((mY-mY_hat_m1)^2)
mm_RSS_Model_1


#Calculating Y-hat and RSS of model 2
mY_hat_m2 = mX_Model2 %*% mModel2_thetahat
mY_hat_m2
#Calculating RSS
mm_RSS_Model_2=sum((mY-mY_hat_m2)^2)
mm_RSS_Model_2

#Calculating Y-hat and RSS of model 3
mY_hat_m3 = mX_Model3 %*% mModel3_thetahat
mY_hat_m3
#Calculating RSS
mm_RSS_Model_3=sum((mY-mY_hat_m3)^2)
mm_RSS_Model_3

#Calculating Y-hat and RSS of model 4
mY_hat_m4 = mX_Model4 %*% mModel4_thetahat
mY_hat_m4
#Calculating RSS
mm_RSS_Model_4=sum((mY-mY_hat_m4)^2)
mm_RSS_Model_4

#Calculating Y-hat and RSS of model 5
mY_hat_m5 = mX_Model5 %*% mModel5_thetahat
mY_hat_m5
#Calculating RSS
mm_RSS_Model_5=sum((mY-mY_hat_m5)^2)
mm_RSS_Model_5

#Task-2.3
#Likelihood and Variance
N=length(mY)
#Calculating the Variance of Model 1
mm_Variance_Model1=mm_RSS_Model_1/(N-1)
mm_Variance_Model1

#Calculating the log-likelihood of Model 1
mm_Likelihood_Model_1=-(N/2)*(log(2*pi))-(N/2)*(log(mm_Variance_Model1))-(1/(2*mm_Variance_Model1))*mm_RSS_Model_1
mm_Likelihood_Model_1

#Calculating the Variance of Model 2
mm_Variance_Model2=mm_RSS_Model_2/(N-1)
mm_Variance_Model2

#Calculating the log-likelihood of Model 2
mm_Likelihood_Model_2=-(N/2)*(log(2*pi))-(N/2)*(log(mm_Variance_Model2))-(1/(2*mm_Variance_Model2))*mm_RSS_Model_2
mm_Likelihood_Model_2

#Calculating the Variance of Model 3
mm_Variance_Model3=mm_RSS_Model_3/(N-1)
mm_Variance_Model3

#Calculating the log-likelihood of Model 3
mm_Likelihood_Model_3= -(N/2)*(log(2*pi))-(N/2)*(log(mm_Variance_Model3))-(1/(2*mm_Variance_Model3))*mm_RSS_Model_3
mm_Likelihood_Model_3

#Calculating the Variance of Model 4
mm_Variance_Model4=mm_RSS_Model_4/(N-1)
mm_Variance_Model4

#Calculating the log-likelihood of Model 4
mm_Likelihood_Model_4= -(N/2)*(log(2*pi))-(N/2)*(log(mm_Variance_Model4))-(1/(2*mm_Variance_Model4))*mm_RSS_Model_4
mm_Likelihood_Model_4

#Calculating the Variance of Model 5
mm_Variance_Model5=mm_RSS_Model_5/(N-1)
mm_Variance_Model5

#Calculating the log-likelihood of Model 5
mm_Likelihood_Model_5= -(N/2)*(log(2*pi))-(N/2)*(log(mm_Variance_Model5))-(1/(2*mm_Variance_Model5))*mm_RSS_Model_5
mm_Likelihood_Model_5


#Task-2.4
#Calculating AIC and BIC for Model-1
mm_K_Model1<-length(mModel1_thetahat)
mm_K_Model1
mm_AIC_Model1=2*mm_K_Model1-2*mm_Likelihood_Model_1
mm_AIC_Model1
mm_BIC_Model1=mm_K_Model1*log(N)-2*mm_Likelihood_Model_1
mm_BIC_Model1

#Calculating AIC and BIC for Model-2
mm_K_Model2<-length(mModel2_thetahat)
mm_K_Model2
mm_AIC_Model2=2*mm_K_Model2-2*mm_Likelihood_Model_2
mm_AIC_Model2
mm_BIC_Model2=mm_K_Model2*log(N)-2*mm_Likelihood_Model_2
mm_BIC_Model2

#Calculating AIC and BIC for Model-3
mm_K_Model3<-length(mModel3_thetahat)
mm_K_Model3
mm_AIC_Model3=2*mm_K_Model3-2*mm_Likelihood_Model_3
mm_AIC_Model3
mm_BIC_Model3=mm_K_Model3*log(N)-2*mm_Likelihood_Model_3
mm_BIC_Model3

#Calculating AIC and BIC for Model-4
mm_K_Model4<-length(mModel1_thetahat)
mm_K_Model4
mm_AIC_Model4=2*mm_K_Model4-2*mm_Likelihood_Model_4
mm_AIC_Model4
mm_BIC_Model4=mm_K_Model4*log(N)-2*mm_Likelihood_Model_4
mm_BIC_Model4

#Calculating AIC and BIC for Model-5
mm_K_Model5<-length(mModel5_thetahat)
mm_K_Model5
mm_AIC_Model5=2*mm_K_Model5-2*mm_Likelihood_Model_5
mm_AIC_Model5
mm_BIC_Model5=mm_K_Model5*log(N)-2*mm_Likelihood_Model_5
mm_BIC_Model5


#Task-2.5
#Error of Model-1
mm_Model1_error <- mY-mY_hat_m1
qqnorm(mm_Model1_error, col = "orange",main = "QQ plot of model 1")
qqline(mm_Model1_error, col = "darkred",lwd=1)

#Error of Model-2
mm_Model2_error <- mY-mY_hat_m2 
qqnorm(mm_Model2_error, col = "orange",main = "QQ plot of model 2")
qqline(mm_Model2_error, col = "darkred")

#Error of Model-3
mm_Model3_error <- mY-mY_hat_m3
qqnorm(mm_Model3_error, col = "orange",main = "QQ plot of model 3")
qqline(mm_Model3_error, col = "darkred")

#Error of Model-4
mm_Model4_error <- mY-mY_hat_m4
qqnorm(mm_Model4_error, col = "orange",main = "QQ plot of model 4")
qqline(mm_Model4_error, col = "darkred")

#Error of Model-5
mm_Model5_error <- mY-mY_hat_m5
qqnorm(mm_Model5_error, col = "orange",main = "QQ plot of model 5")
qqline(mm_Model5_error, col = "darkred")

#Task-2.7
#install package tidymodels
#Split-y

############## task 2.7
###############Split-y
mm_split_y<-initial_split(data = as.data.frame(mY),prop=.7)
mY_training_set<-training(mm_split_y)
mY_training_set
mY_testing_set<-as.matrix(testing(mm_split_y))
mY_testing_set
mY_training_data<-as.matrix(mY_training_set)
mY_training_data

#Split-x
mm_split_x<-initial_split(data = as.data.frame(mX),prop=.7)
mX_training_set<-training(mm_split_x)
mX_training_set
mX_testing_set<-as.matrix(testing(mm_split_x))
mX_testing_set
mX_testing_data<-as.matrix(mX_testing_set)
mX_testing_data
mX_training_data<-as.matrix(mX_training_set)
mX_training_data

mX_training_one=matrix(1 , length(mX_training_set$mX2),1)
mX_training_model<-cbind(mX_training_one,mX_training_set[,"mX2"],(mX_training_set[,"mX1"])^3,(mX_training_set[
  ,"mX3"])^4)
mm_training_thetahat=solve(t(mX_training_model) %*% mX_training_model) %*% t(mX_training_model) %*% mY_training_data

mm_training_thetahat


###
### Model out/Prediction
mY_testing_hat = mX_testing_data %*% mm_training_thetahat
mY_testing_hat
mm_RSS_testing=sum((mY_testing_set-mY_testing_hat)^2)
mm_RSS_testing

t.test(mY_training_data, mu=500, alternative="two.sided", conf.level=0.95)

mm_C_I1=-0.2783950
mm_C_I2=0.2762101

mm_p2 <- plot(density(mY_training_data), col="blue", lwd=2, main="Distribution of Training Data")
mmline(v=mm_C_I1,col="red", lty=2)
mmline(v=mm_C_I2,col="red", lty=2)
mm_thetaHat_training =solve(t(mX_training_data) %*% mX_training_data) %*% t(mX_training_data) %*%
  mY_training_data
mm_thetaHat_training
length(mm_thetaHat_training)
mm_dis_test=density(mY_training_data)
plot((mm_dis_test))
plot(mm_dis_test,main = "Density plot of b-Y Signal")
### Calculating Confidential interval
##(95%) Confidential interval
mm_z=1.96 
mm_error=((mY_testing_set-mY_testing_hat))
mm_error
mm_n_len=length(mY_testing_hat)
mm_n_len
C_I_1= mm_z*sqrt((mm_error*(1-mm_error))/mm_n_len)
C_I_1
C_I_2= mm_z*sqrt((mm_error*(1+mm_error))/mm_n_len)
C_I_2


#################Error bar of Model 2
mm_plot_data = data.frame(
  mm_x_Val = mX_Model2,
  mm_y_Val = mY,
  mm_SD = sqrt(mm_Variance_Model2)  
)



mm_plot<-ggplot(mm_plot_data) +
  geom_bar( aes(x=mm_x_Val.2, y=mY), stat="identity", alpha=0.7) +
  geom_errorbar( aes(x=mm_x_Val.2, ymin=mY-mm_SD, ymax=mY+mm_SD), width=0.2, colour="purple", alpha=0.5, linewidth=1)

print (mm_plot)


##Task 3
## Model 3 will be used, parameter are selected and kept constant.
mm_arr_1=0
mm_arr_2=0
f_value=0
s_value=0
mModel2_thetahat
#values from mm_thetahat
thetebias <- 0.483065688 #choosen parameter
mm_thetaone <-  0.143578928 # chosen prarameter
mm_thetatwo <- 0.010038614 # constant value
mm_thetafour <- 0.001912836 # constant value
Epison <- mm_RSS_Model_2 * 2 ## fixing value of eplision
num <- 100 #number of iteration
##Calculating Y-hat for performing rejection ABC
counter <- 0
for (i in 1:num) {
  range1 <- runif(1,-0.483065688,0.483065688) # calculating the range
  range1
  range2 <- runif(1,-0.143578928,0.143578928)
  New_mm_thetahat <- matrix(c(range1,range2,mm_thetatwo,mm_thetafour))
  New_Y_Hat <- mX_Model2 %*% New_mm_thetahat ## New Y hat
  new_RSS <- sum((mY-mm_New_Y_Hat)^2)
  new_RSS
  if (new_RSS > Epison){
    mm_arr_1[i] <- range1
    mm_arr_2[i] <- range2
    counter = counter+1
    f_value <- matrix(mm_arr_1)
    s_value <- matrix(mm_arr_2)
  }
}
hist(f_value)

hist(s_value)
###ploting the graph
plot(f_value,s_value, col = c("red", "blue"), main = "Joint and Marginal Posterior Distribution")

