#Question 03
Y<-c(67.50,75.44, 109.7,105.44, 109.4, 85.83,  76.7,129.42,104.24,125.83,153.99,152.92,160.03,176.33,174.53)
X<-c(80.09,72.57,112.14,121.57,125.60,131.48,131.54,145.60,168.56,171.48,203.54,222.85,230.93,232.99,261.18)

ols<-lm(Y~X)
summary(ols)
mse<-summary(ols)$sigma**2 
mse

data<-data.frame(Y,X)
data

data1<-data[order(data$X),] 
data1  
x<-data1$X;x
y<-matrix(data1$Y);y


##Wald Method####
z<-array(length(x))
for (i in 1:length(x)) {
  if(x[i]<=median(x))
    z[i]=-1
  else
    z[i]=1
}
x.mat<-cbind(1,x)
x.mat
z.mat<-cbind(1,z)
z.mat

###Estimation of parameters#####
t1<-solve(t(z.mat)%*%x.mat);t1
t2<-t(z.mat)%*%y;t2
b.hat<-t1%*%t2;b.hat  			
a.iv.hat<-b.hat[1];a.iv.hat
b.iv.hat<-b.hat[2];b.iv.hat

#####Variance####
###(part-B)
v1<-solve(t(z.mat)%*%x.mat)
v2<-t(z.mat)%*%(z.mat)
v3<-solve(t(x.mat)%*%z.mat)
var.mat<-mse*(v1%*%v2%*%v3);var.mat   							     
var.a.iv<-var.mat[1,1];var.a.iv   
var.b.iv<-var.mat[2,2];var.b.iv 

#Bartlett method######
n1<-round(length(X)/3);n1
n2<-2*n1+1;n2
x1<-c(x[1:n1],x[n2:length(x)]);x1
y1<-matrix(c(y[1:n1],y[n2:length(x)]));y1

z<-array(length(x1))
for (i in 1:length(x1)) {
  if(x1[i]<=median(x1))
    z[i]=-1
  else
    z[i]=1
}
x.mat<-cbind(1,x1);x.mat
z.mat<-cbind(1,z);z.mat

###Estimation of parameters#####
t1<-solve(t(z.mat)%*%x.mat);t1
t2<-t(z.mat)%*%y1;t2
b.hat<-t1%*%t2;b.hat  			
a.iv.hat<-b.hat[1];a.iv.hat
b.iv.hat<-b.hat[2];b.iv.hat

#####Variance####
v1<-solve(t(z.mat)%*%x.mat)
v2<-t(z.mat)%*%(z.mat)
v3<-solve(t(x.mat)%*%z.mat)
var.mat<-mse*(v1%*%v2%*%v3);var.mat   							     
var.a.iv<-var.mat[1,1];var.a.iv   
var.b.iv<-var.mat[2,2];var.b.iv 

#Durbin method##
z<-rank(x)				
x.mat<-cbind(1,x);x.mat
z.mat<-cbind(1,z);z.mat

###Estimation of parameters#####
t1<-solve(t(z.mat)%*%x.mat);t1
t2<-t(z.mat)%*%y;t2
b.hat<-t1%*%t2;b.hat  			
a.iv.hat<-b.hat[1];a.iv.hat
b.iv.hat<-b.hat[2];b.iv.hat

#####Variance####
v1<-solve(t(z.mat)%*%x.mat)
v2<-t(z.mat)%*%(z.mat)
v3<-solve(t(x.mat)%*%z.mat)
var.mat<-mse*(v1%*%v2%*%v3);var.mat   							     
var.a.iv<-var.mat[1,1];var.a.iv   
var.b.iv<-var.mat[2,2];var.b.iv     



#question 4
y<-c( 16.68, 11.50, 12.03, 14.88, 13.75, 18.11, 8.00, 17.83, 79.24, 21.50, 40.33, 21.00, 13.50,
      19.75, 24.00 ,29.00, 15.35, 19.00, 9.50, 35.10, 17.90, 52.32, 18.75 ,19.83, 10.75)
x<-c(560, 220, 340, 80, 150, 330, 110 , 210, 1460, 605, 688, 215, 255 , 462, 448, 776,
     200, 132 , 36, 770, 140, 810 , 450, 635, 150)
model<-lm(y~x);model
plot(x,y)
abline(model)
###High leverage value
d=(x-mean(x))^2/(sum((x-mean(x))^2));d
n=length(x);n
hii=(1/n)+d
hii
######
h<-hat(x);h
###Cut-off Point#####
p=2
CP=2*(p/n);CP ### Twice the mean rule
CP>hii
which(hii>CP)
#####outlier
r<-model$residuals;r
msr<-sum(r^2)/(n-p);msr
ar<-abs(r);ar
di<-ar/sqrt(msr);di ##Standardized residuals
out<-di[di>3];out
#or
ri<-ar/sqrt(msr*(1-h));ri ##Studentized residuals
out<-ri[ri>3];out
#### Influential Observation#####
cdi<-(h*ri^2)/(p*(1-h));cdi
IO<-cdi[cdi>1];IO
###
cooks.distance(model)
####
y.hat=4.96116+.04257*x
y.hat
ei=y-y.hat;ei
sl.<-c(1:25)
data2=data.frame(sl.,y,x,y.hat,ei,hii,di,ri,cdi);data2
View(data2)

# ️egulo enmi dilam pore customize korar jonno,  Change 9th Y
# y2 <- y; y2[9] <- 65.24
# y3 <- y; x3 <- x
# y3[c(9,22)] <- c(65.24, 35.32)
# analyze_regression <- function(x, y) {return(data2)}


#question 5

years <- 1976:1980
fires <- c(16694, 12271, 12904, 14036, 13874)
plot(years, fires, main="Plot the Data to check outlier", col="red")
plot(years, fires, main="LS Model Fit line", col = "red")
abline( lm ( fires ~ years ), col= 4)

n=5
p=1
h = ((n/2)+(p+1)/2) #(eta =3dhorte hobe) of obs. Each group

## Create afunction to collect median squre of errors
med.er.sq <- function( x, y){
  new <- data.frame(y, x)
  model <- lm(y ~ x)
  errors <- y - predict(model, new)
  med.er.sqr <- median(errors^2)
  return (med.er.sqr)
}

## medain Erros Squre calculation
e1 <- med.er.sq(years[c(1,2,3)], fires[c(1,2,3)]) ## 1.line
e2 <- med.er.sq(years[c(1,2,4)], fires[c(1,2,4)]) ## 2.line
e3 <- med.er.sq(years[c(1,2,5)], fires[c(1,2,5)]) ## 3.line
e4 <- med.er.sq(years[c(1,3,4)], fires[c(1,3,4)]) ## 4.line
e5 <- med.er.sq(years[c(1,3,5)], fires[c(1,3,5)]) ## 5.line
e6 <- med.er.sq(years[c(1,4,5)], fires[c(1,4,5)]) ## 6.line
e7 <- med.er.sq(years[c(2,3,4)], fires[c(2,3,4)]) ## 7.line
e8 <- med.er.sq(years[c(2,3,5)], fires[c(2,3,5)]) ## 8.line
e9 <- med.er.sq(years[c(2,4,5)], fires[c(2,4,5)]) ## 9.line
e10 <- med.er.sq(years[c(3,4,5)], fires[c(3,4,5)]) ##10.lines

## Find least median squre of errors
e <-c(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
summary(e)
which.min(e)

## Fitted LMS model
x <- years[c(2,3,5)]
y <- fires[c(2,3,5)]
lms.model <- lm(y ~ x)
lms.model
## Graph of LMS
plot(years, fires, main= "LMS model line")
abline(lms.model, col= "red")


#question 06
## U.S Air Force Data
## Cost and Weight
cost <- c(2449, 2248, 3545, 789, 1619, 2079, 918, 1231, 3641, 4314, 2628, 3989,
          2308, 376, 5428, 2786, 2497, 5551, 5208, 5438)
weight <-c(90, 87, 38, 28, 28, 23, 21, 17, 27, 39, 34, 46, 80, 14, 48, 38, 73, 40, 44,
           41)
## Graph
plot(weight, cost, main = "U.S Air Force Data ")
abline(lm(cost~weight), col = "red") ## SL fit line
## create a data frame
myData <- data.frame(cost,weight)
## LS model Fit And Take Residuals
LS <- lm(cost ~ weight, data= myData)
errors <- LS$residuals
myData$residual <- errors
sort(errors)
## Graph of Residual
plot(weight, errors, main = "Residual plot")
## trimmed the data
n = 20
p = 1
alpha = .20 ## 20% data trimmed
h = ((1-alpha)*n)+((p+1)/2)
h= 17 ## 3 large residual omit from data respectively
## trim data collection
t1.data <- myData[myData$residual < 2405, ]
t1.data
## model fit with trim data
LTS_model_1 <- lm(cost ~ weight,data = t1.data)
## See the model cofficient
LTS_model_1
##Graph
plot(weight, cost, main = "Frist LTS MOdel line ",
     col = "red")
abline(LTS_model_1, col = 4)
abline(lm(cost~weight), col = 6)

## again check residual omit h obs.
t1.data$residual <- LTS_model_1$residuals
n=17
alpha = .2
p=1
h = ((1-alpha)*n)+((p+1)/2) ## h =14.6 close to 15
# sort Residuals
sort(t1.data$residual) ## sort residuals and omit 2 large residauls.
## trim two data
t2.data <- t1.data[t1.data$residual < 1857, ]

LTS_model_2 <- lm(cost ~ weight, data = t2.data )
## LTS 2 model
## trimmed three data
t3.data <- t2.data[t2.data$residual < 1655, ]
LTS_model_3 <- lm(cost ~ weight, data = t3.data )
## LTS 3 model
t3.data$residual <- LTS_model_3$residuals
sort(t3.data$residual)
## trimmed four data
t4.data <- t3.data[t3.data$residual < 1406, ]
LTS_model_4 <- lm(cost ~ weight, data = t4.data )
## LTS 4 model
t4.data$residual <- LTS_model_4$residuals
sort(t4.data$residual)

## trim five data
t5.data <- t4.data[t4.data$residual < 1071.84893, ]
LTS_model_5 <- lm(cost ~ weight, data = t5.data )
## LTS 5 model

## all model graph
plot(weight, cost, main = "All Model graph ")
abline(lm(cost~weight))
abline(LTS_model_1, col= 2)
abline(LTS_model_2, col= 3)
abline(LTS_model_3, col= 4)
abline(LTS_model_4, col= 5)
abline(LTS_model_5, col= 6)

#######another method
cost <- c(2449, 2248, 3545, 789, 1619, 2079, 918, 1231, 3641, 4314, 2628, 3989,
          2308, 376, 5428, 2786, 2497, 5551, 5208, 5438)
weight <-c(90, 87, 38, 28, 28, 23, 21, 17, 27, 39, 34, 46, 80, 14, 48, 38, 73, 40, 44,
           41)

myData = data.frame(cost, weight)
ls_model = lm(cost ~ weight, data = myData)
myData$residuals = abs(ls_model$residuals)

LTS_step = function(data, alpha = 0.2, p =1) {
  n = nrow(data)
  h = round((1 - alpha)*n + (p+1)/2)
  
  # sort by smallest absolute residuals
  sort_data = data[order(data$residuals), ]
  # keep 1st h data
  trimmed_data = sort_data[1:h, ]
  
  # Fit new model
  model = lm(cost ~ weight, data = trimmed_data)
  trimmed_data$residuals = abs(model$residuals)
  list(model = model, data = trimmed_data)
}

# Iteratively apply LTS trimming steps
step1 = LTS_step(myData)
step2 = LTS_step(step1$data)
step3 = LTS_step(step2$data)
step4 = LTS_step(step3$data)
step5 = LTS_step(step4$data)
step6 = LTS_step(step5$data)

# Final Model
model_ls = ls_model
model_lts1 = step1$model
model_lts2 = step2$model
model_lts3 = step3$model
model_lts4 = step4$model
model_lts5 = step5$model
model_lts6 = step6$model
# Plot models
plot(weight, cost, main = "All model graph")
abline(model_ls, col=1)
abline(model_lts1, col=2)
abline(model_lts2, col=3)
abline(model_lts3, col=4)
abline(model_lts4, col=5)
abline(model_lts5, col=6)
abline(model_lts6, col=7)
legend("topright", c("LS", paste0("LTS", 1:6)), col = 1:7, lty = 1)


#binary _ dependent(terminant) _ covariTE_categorical_option(hosmer lemeshow)
#classification ta, var in eq, hosmer,model

#multinomial logistic _ dependent(bfast) _ factorial(nominal,ordinal)_cova(other/continous)
#comment-parameter estimation- 

#poisson _ gene linear mod-type(poisson)-response(dependent(award))-response(pre(prog),cov(math))
#option(descending)_model(prog,math)_statistics(include exponen)
#comment_omnibus test_parameter esti


