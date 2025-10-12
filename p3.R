a <- c(48, 50, 51, 49, 50, 52, 48, 49, 51, 50)
b <- c(52, 53, 55, 54, 52, 53, 51, 50, 54, 53)
c <- c(55, 54, 56, 57, 55, 56, 54, 55, 57, 56)
d <- c(44, 45, 54, 52, 53, 51, 49, 53, 48, 55)
e <- c(10, 12, 13, 15, 16, 18, 19, 21, 22, 22)
f <- c(8,  9,  11, 13, 15, 16, 18, 19, 20, 9)
## Single mean test null:mu_a=50 vs alt:mua !=50,alt:mua < 50,alt:mua > 50

t.test(a,mu=50,alternative = "two.sided") ## two tailed test 

t.test(a,mu=50,alternative = "less") ## left tailed test 

t.test(a,mu=50,alternative = "greater") ## right tailed tes 


## single variance test , null:sigma_a_sq=4 vs alt:sigma_a_sq != 4,alt:sigma_a_sq < 4,
##alt:sigma_a_sq >4


n=length(a)
var_a=var(a)
sigma0=4
chi=(n-1)*var_a/sigma0
chi
df=9
p_value_two <- 2 * min(pchisq(chi, df), 1 - pchisq(chi, df)) ## two tail
p_value_two

p_value_left <-  pchisq(chi, df) ## left tail
p_value_left

p_value_right <-  1-pchisq(chi, df) ## right tail
p_value_right

## zero (0) correlation test,null: rho_ab=0 vs alt:rho_ab !=0,alt:rho_ab <0,alt:rho_ab >0


library(psych)
cor.test(a, b, alternative = "two.sided") ## two tail

cor.test(a, b, alternative = "less") ## left tail

cor.test(a, b, alternative = "greater") ## right tail

## specipic corr test

##null: rho_ab=0.5 vs alt:rho_ab !=0.5,alt:rho_ab <0.5,alt:rho_ab >0.5
rho0 <- 0.5
n <- length(a)
r <- cor(a, b)


z_r <- 0.5 * log((1 + r) / (1 - r))
z_rho0 <- 0.5 * log((1 + rho0) / (1 - rho0))


Z <- (z_r - z_rho0) / sqrt(1 / (n - 3))
Z

# Two-tailed p-value
p_two_tail <- 2 * (1 - pnorm(abs(Z)))
p_two_tail

# Right-tailed (H1: rho > rho0)
p_right <- 1 - pnorm(Z)
p_right
# Left-tailed (H1: rho < rho0)
p_left <- pnorm(Z)

p_left






## equality of two mean test

## null:mu_a=mu_b alt : mu_a!=mu_b, alt=mu_a > mu_b,alt= mu_a < mu_b
t.test(a,b,alternative = "two.sided",var.equal = F) ## two tailed

t.test(a,b,alternative = "greater",var.equal = F) ## right tailed

t.test(a,b,alternative = "less",var.equal = F) ## left tailed



## equality of two variance test 
## null:sigma_a^2=sigma_b^2 vs alt: sigma_a^2 !=sigma_b^2, alt:sigma_a^2 > sigma_b^2 , alt:sigma_a^2=sigma_b^2

s_a_square=var(a);s_a_square
s_b_square=var(b);s_b_square

f_two=(s_b_square)/(s_a_square) ## f=larger sample variance/smaller sample vaarinace
f_two
p_two_tail=2*min(pf(f_two,9,9),1-pf(f_two,9,9)) ## two tailed
p_two_tail


## right tailed

f_r=s_a_square/s_b_square

p_right=1-pf(f_r,9,9) 

p_right


## left tailed

f_l=s_b_square/s_a_square

p_left=pf(f_l,9,9) 

p_left

##equality of two correlation test between ab and cd

## null: rho_ab=rho_cd vs alt: rho_ab != rho_cd, alt :rho_ab > rho_cd,alt:rho_ab<rho_cd
##install.packages("cocor")
library(cocor)

# Example: test equality of two independent correlations

r1=cor(a,b)
r2=cor(c,d)
cocor.indep.groups(r1=r1, r2=r2, n1=10, n2=10, alternative="two.sided")

cocor.indep.groups(r1=r1, r2=r2, n1=10, n2=10, alternative="greater")

cocor.indep.groups(r1=r1, r2=r2, n1=10, n2=10, alternative="less")



## several mean test
## null: mu_a=mu_b=mu_c=mu_d=mu_e vs alt : aat least one equality doesn't hold

#Combine into long format

group <- factor(rep(c("a","b","c","d"), each=10))
values <- c(a,b,c,d)
data <- data.frame(group, values)

# ANOVA test
anova_result <- aov(values ~ group, data=data)

# Summary
summary(anova_result)


## equality of several variance test
bartlett.test(values ~ group, data = data)


## equality of several correaltion test
## problem 2023 (3)
equality_several_corr <- function(r, n) {
  z <- 0.5 * log((1 + r)/(1 - r))  # Fisher z
  w <- n - 3                        # weights
  z_bar <- sum(w * z)/sum(w)        # weighted mean
  chi2 <- sum(w * (z - z_bar)^2)    # test statistic
  df <- length(r) - 1               # degrees of freedom
  p_value <- 1 - pchisq(chi2, df)   # p-value
  return(list(chi2 = chi2, df = df, p_value = p_value))
}

# Example usage
r <- c(0.39, 0.61, 0.43,0.54,0.48)
n <- c(21, 26, 19,28,25)
equality_several_corr(r, n)


## non parametric test
## ks test

# Household expenditure data

men <- c(497, 839, 798, 892, 1585, 755, 338, 617, 208, 1641,
         1180, 619, 253, 661, 1981, 1746, 1865, 238, 1199, 1524)

women <- c(820, 164, 921, 488, 721, 614, 801, 396, 864, 845,
           404, 781, 457, 1029, 1047, 552, 718, 495, 382, 1090)

# Kolmogorov–Smirnov two-sample test
ks_result <- ks.test(men, women)

# Display result
ks_result



##kruskal wali

a = c(5.33, 4.49, 5.22, 4.87, 4.66, 5.18, 5.38, 5.44, 4.81, 5.12)
b = c(5.30, 5.73, 4.87, 5.36, 4.92, 6.30, 4.86, 3.70, 5.28, 5.23)
c = c(4.36, 4.73, 6.18, 5.14, 5.76, 3.80, 4.65, 4.00, 5.83, 5.97)
d = c(3.73, 3.94, 5.66, 4.20, 4.17, 4.78, 3.67, 5.51, 4.70, 4.62)
e = c(4.40, 5.71, 5.36, 5.41, 4.75, 4.63, 4.57, 5.25, 5.65, 5.12)
y=data.frame(a,b,c,d,e)
boxplot(y,col=c(2,3,4,5,6),ylim=c(3,7),main="Boxplot")


library(tidyr)
long_data <- pivot_longer(y, cols = everything(), names_to = "Company", values_to = "Value")


kruskal_result <- kruskal.test(Value ~ Company, data = long_data)


kruskal_result

#------
  x=c(32.5,78.4,49.9,38.5,33.7,61.4,29.7,50.5,27.3,44.4,54.7,62,47.2,33.5,47.4,59.2,
      49.7,67,39.8,46.8,56.5,26.3,75.6,68,68.1,70.4,65.7,68.3,40.7,24.4,56.2,31.8)

mu0=35; mu1=57; sigma2=50
alpha=0.05; beta= 0.05

A=log((1 - beta) / alpha);A
B=log(beta / (1 - alpha));B

logLR=cumsum((mu1 - mu0) * (x - (mu1 + mu0)/2) / sigma2)
logLR

#criterion of judgement? 90% Ci for mu1-mu2
mean1 <- mean(a)
mean2 <- mean(b)
sd1 <- sd(a)
sd2 <- sd(b)
n1 <- length(a)
n2 <- length(b)
#pooled sd devi
sp <- sqrt(((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2))

#se
se <- sp * sqrt(1/n1+1/n2)
tcal = (mean1-mean2)/se
tcal
#C tvalue 
t_crit <- qt(0.95,df=n1+n2-2)
t_crit
#CI 
lower <- (mean1-mean2) - t_crit*se
upper <- (mean1-mean2) + t_crit*se
c(lower,upper)


#Making decision
for (i in 1:length(logLR)) {
  if (logLR[i] >= A) {
    decision <- "Reject H0"
    break
  } else if (logLR[i] <= B) {
    decision <- "Accept H0"
    break 
  }
}
if (!exists("decision")) {
  decision <- "No decision"
}

cat("SPRT Decision:", decision, "\n")

---------------------------------------------------------------------------------------------------------
#single popn mean

# Let us assume that x1, x2, …, xn be a random sample of n observation drawn from a normal population with mean µ and σ2.
# We want to test the following hypothesis at 100α% level of significance.
# One tail test:
#   Right tail side:       
#   H0: µ= µ0
# H1: µ>µ0
# H0: µ=µ0
# H1: µ<µ0
# Two tail test:
#   H0: µ=µ0
# H1: µ≠µ0
# Now we shall consider two cases:
#   Case I: The population variance is known:
#   Let us assume that the population variance σ2 is known. Then to test the above 
# s, we use the test statistic z defined by
# Z=(x ̅-µ)/(σ/n)  ~N(0,1); where x ̅=1/n∑xi
# Under the null hypothesis the test statistic becomes
# Z=(x ̅-µ)/(σ/n)  ~N(0,1)
# For right tail test, H0: µ=µ0  against H1: µ>µ0  , the critical region w of size 
# α is given by w:[z>zα]; where zα is the upper 100α% point of the unit normal distribution/population.
# For left tail test, H0: µ=µ0  against H1: µ<µ0  , the critical region w of size α is given 
# by w:[z<-zα]; where  - zα is the lower 100 α% point of the unit normal distribution/population.
# For two tail test, H0: µ=µ0 against H1: µ≠µ0 , the critical region w of size is given by
# w:[ z<-zα/2 , z>zα/2]; where is the upper 50 α% point of unit normal population and - zα/2
# is the lower 50% point of unit normal population.
# 
# Decision/comment: If the calculated value of the test statistics z, falls in critical region w,
# the null hypothesis may be rejected in favor of the alternative hypothesis at 100 α% level of significance;
# otherwise the null hypothesis may be failed to reject.
# 
# case-II: Population variance σ^2 is unknown:
#   Let us assume that the population variance  σ^2is unknown but estimated by S^2=1/((n-1)) 
# ∑_(i=0)^n▒( 〖x_i-x ̅)〗^2 then we use the test statistic t, defined by 
# t=(x ̅-μ)/(s/√n)~t_(n-1); where, x ̅=1/n ∑_(i=1)^n▒x_i 
# Under the null hypothesis the test statistic becomes 
# t=(x ̅-μ_0)/(s/√n)~t_(n-1)
# For right tail test, H_0:μ_= μ_0,vs H_1:μ>μ_0, the critical region w  of size α,is given by 
# w:[t>t_(∝,n-1)];where t_(∝,n-1) is the upper 100∝% point of the t distribution with (n-1) df.
# For left tail test, H_0:μ_= μ_0,vs H_1:μ<μ_0,the critical region w of size α,is given by 
# w:[t<-t_(∝,n-1)];where -t_(∝,n-1) is the lower 100∝% point of the t distribution with (n-1) df.
# For two tail test , H_0:μ_= μ_0,vs H_1:μ≠μ_0,the critical region w of size α,is given by
# w:[t<-t_(∝/2,n-1),t>t_(∝/2,n-1)];where, t_(∝/2,n-1)is the upper 50∝% point of the t-distribution with (n-1) df 
#and -t_(∝/2,n-1) is the lower 50∝% point of the t-distribution with (n-1) df.
# #Decision/Comment: If the calculated value of the test statistic t falls in the critical region w,
# then the null hypothesis may be rejected in favor of the alternative hypothesis at 100∝%  level of significance; 
# otherwise the null hypothesis may be failed to reject.
-----------------------------------------------------------------------------------------------------------------
# Test for equality of two population means:
  
#   Let, x_11,x_12,….x_(〖1n〗_1 )and x_(21,) x_22….x_(〖2n〗_2 )be two independent random samples 
# which have come from normal populations with means μ_1 and 〖 μ〗_2 ; and variances 〖σ_1〗^2 and  
# 〖σ_2〗^2 respectively. We want to test the following hypothesis at 100∝% level of significance.
# One tail test:
#   (i) Right tail test: H_0:μ_(1=) μ_2,vs H_1:μ_1>μ_2
# (ii) Left tail test: H_0:μ_(1=) μ_2,vs H_1:μ_1<μ_2
# # Two tail test: H_0:μ_(1=) μ_2,vs H_1:μ_1≠μ_2
# Now we shall consider 3 cases:
#   #Case-I: Population variances are known but unequal: 
#   Let us assume that the population variances 〖σ_1〗^2  and 〖σ_2〗^2 are known but unequal.
# 
# Then to test the above hypothesis,we use the test statistic Z ,defined by
# Z=(〖(x ̅〗_1-x ̅_2)-(μ_(1-) μ_2))/√(〖σ_1/n_1 〗^2+〖σ_2〗^2/n_2 )~N(0,1)
# Where, x ̅_1=1/n_1  ∑_(i=0)^(n_1)▒x_1i 
# x ̅_2=1/n_2  ∑_(i=0)^(n_2)▒x_2i 
# Under the null hypothesis the test statistic becomes,
# 1.For right tail test ,H_0:μ_(1=) μ_2,vs H_1:μ_1>μ_2,the critical region w of size 
# α,is given by w:[Z>Zα];
# Where Z_α is the 100α% upper point of the unit normal distribution.
# 2. For left tail test,〖 H〗_0:μ_(1=) μ_2,vs H_1:μ_1<μ_2,the critical region w of
# size α,is given by w:[Z<-Zα];
# Where -Z_α is the 100α% lower point of the unit normal distribution.
# 3. . For two tail test ,H_0:μ_(1=) μ_2,vs μ_1≠μ_2,the critical region w of size α,
# is given by w:[Z<-Z_(∝/2)] Where,
# Z_(∝/2) is the upper 100∝/2% =50% point of the unit normal distribution and -Z_(∝/2) 
# is the lower 50∝% point of the unit normal distribution.
# #Decision/comment: If the calculated value of the test statistic Z falls in critical
# region w, then the null hypothesis may be rejected in favour of the alternative hypothesis
# at 100∝% level of significance;otherwise the null hypothesis may be failed to reject.
# #case-ii:population variances are equal but unknown:
# Let us assume that the population variances 〖∂_1〗^(2 )and 〖∂_2〗^(2 )are equal 
# but unknown and is estimated by
# S^2=(∑▒〖〖(x_1i-x ̅_1)〗^2+∑▒〖(x_2i-x ̅_2)〗^2 〗)/(n_1+n_2-2), then to test the
# above hypothesis ,we use the test statistic t,defined by
# t=(x ̅_1-x ̅_2-(μ_(1-) μ_2 ))/(S√(1/n_1 +1/n_2 ))~t_(n_1+n_2-2)
# Under the null hypothesis the test statistic becomes
# t=(x ̅_1-x ̅_2)/(S√(1/n_1 +1/n_2 ))~t_v ,where v=n_1+n_2-2
# For right tail test ,H_0:μ_(1=) μ_2,vs H_1:μ_1>μ_2,the critical region w  of size
# α,is given by w:[t>t_(∝,v)];where t_(∝,v) is the upper 100∝% point of the t distribution with v df.
# For left tail test ,H_0:μ_(1=) μ_2,vs H_1:μ_1<μ_2,the critical region w of size 
# α,is given by w:[t<-t_(∝,v)];where -t_(∝,v) is the lower 100∝% point of the t distribution with v df.
# For two tail test , H_0:μ_(1=) μ_2,vs H_1:μ_1≠μ_2,the critical region w of size α,is 
# given by w:[t<-t_(∝/2,v),t>t_(∝/2,v)];where, t_(∝/2,v)is the upper 100∝/2%=50∝% point o
# f the t-distribution with v df and -t_(∝/2,v) is the lower 50∝% point of the t-distribution with v df.
# #Decision/comment: If the calculated value of the test statistic t falls in critical region
# w, then the null hypothesis may be rejected in favour of the alternative hypothesis at 100∝% 
# level of significance;otherwise the null hypothesis may be failed to reject.
# 
# #Case-iii: population variances are unknown and unequal:
# Let us assume that the population variances 〖∂_1〗^(2 )and 〖∂_2〗^(2 )are unequal 
# and unknown but 〖∂_1〗^2 is estimated by ,
# 〖S_1〗^2=1/((n_1-1)) ∑_(i=0)^(n_1)▒( 〖x_1i-x ̅_1)〗^2 and 〖∂_2〗^(2 )is estimated by 
# 〖S_2〗^2=1/((n_2-1) ) ∑_(i=0)^(n_1)▒( 〖x_2i-x ̅_2)〗^2.
# Then we use the test statistic t’,defined by
# t'=(x ̅_1-x ̅_2-(μ_(1-) μ_2 ))/√(〖S_1〗^2/n_1 +〖S_2〗^2/n_2 )~t_(n_1+n_2-2)
# Under the null hypothesis the test statistic becomes
# t'=(x ̅_1-x ̅_2)/√(〖S_1〗^2/n_1 +〖S_2〗^2/n_2 )~t_(n_1+n_2-2)
# (i)For right tailed test ,H_0:μ_(1=) μ_2,〖vs H〗_1:μ_1>μ_2,the critical region w 
# of size α,is given by w:[t’>(w_1 t_1+w_2 t_2)/(w_1+w_2 )];where w_1=〖S_1〗^2/n_1  
# ,w_2=〖S_2〗^2/n_2   and t_1=t_(∝,n_1-1) is tabulated upper 100∝% point and t_2=t_(∝,n_2-1) 
# is tabulated upper 100∝% point.
# (ii) For right tailed test ,H_0:μ_(1=) μ_2,〖vs H〗_1:μ_1<μ_2,the critical region w 
# of size α,is given by w:[t’<-(w_1 t_1+w_2 t_2)/(w_1+w_2 )];where w_1=〖S_1〗^2/n_1  
# ,w_2=〖S_2〗^2/n_2   and t_1=t_(∝,n_1-1) is tabulated upper 100∝% point and t_2=t_(∝,n_2-1) 
# is tabulated upper 100∝% point.
# (iii) ) For two tailed test ,H_0:μ_(1=) μ_2,〖vs H〗_1:μ_1≠μ_2,the critical region w  
# of size α,is given by w:[t’<-(w_1 t_1+w_2 t_2)/(w_1+w_2 ),t’>(w_1 t_1+w_2 t_2)/(w_1+w_2 )];
# where w_1=〖S_1〗^2/n_1   ,w_2=〖S_2〗^2/n_2  and t_1=t_(∝/2,n_1-1) is tabulated.
# # Comment/decision: If the calculated value of the test statistic tˊ falls in critical 
# region w, Then the null hypothesis may be rejected at 100 α% level of significance in favor 
  #of alternative hypothesis ; otherwise the null hypothesis may be failed to reject.
# -----------------------------------------------------------------------------------------------
# 
# Question: Test for a single population variance:
# 
#   Describe how will you perform the following statistical tests stating the assumption.
# (i). H0 : σ²= σ₀² vs H1 : σ²> σ₀²	 (ii). H0 : σ²= σ₀² vs H1 : σ²< σ₀²
# (iii). H0 : σ²= σ₀² vs H1 : σ²≠ σ₀².
# Answer: Let x1,x2,…,xn be a set of n independent observations drawn from a normal population with mean µ and variance σ². We want to test the following hypothesis at 100 α% level of significance.
# One tailed test :
#   Right tailed test :                                                         Left tailed test:
#   H0 : σ²= σ₀² vs H1 : σ²> σ₀²                                                          H0 : σ²= σ₀² vs H1 : σ²< σ₀²
# Two tailed test:
#   H0 : σ²= σ₀² vs H1 : σ²≠ σ₀².
# To test the above hypothesis , we use the test statistic χ², defined by
# χ²=((n-1) s^2)/σ²~ 〖  χ²〗_(n-1)  where s^2=1/(n-1) ∑_(i=1)^n▒〖(xi-x̄〗)2
# under the null hypothesis , the test statistic becomes
# χ²=((n-1) s^2)/(σ₀²)~ 〖  χ²〗_(n-1)
# (i). For right tailed test , H0 : σ²= σ₀² against H1 : σ²> σ₀²  , the critical region w of size α is given by w:[ χ²> χ²α,n-1 ] ; where χ²α,n-1 is the upper 100 α% point of χ² distribution with (n-1) df.
# (ii). For left  tailed test ,   H0 : σ²= σ₀² against H1 : σ²< σ₀² , the critical region w of size α given by
# w:[ χ²< χ²(1-α),(n-1) ]; where  χ²(1-α),(n-1) is the upper  100(1-α)% point of the χ² distribution with 
# (n-1)   df.
# (iii). For two tailed test , H0 : σ²= σ₀² against H1 : σ²≠ σ₀², the critical region w of size α is given by w:[ χ²< χ²(1- 〖α/〗_2),(n-1) ; χ²> χ²(α/2,n-1)]; where χ²(α/2,n-1) is the upper 100 (1-α/2)% point of χ² distribution with (n-1) df.
# Decision/comment:If the calculated value of the test statistic χ² falls in critical region w, the null hypothesis may be rejected in favor of alternative hypothesis at 100 % level of significance ; otherwise the null hypothesis may be failed to reject.
# -----------------------------------------------------------------------------------------------------------------
#   Question: Test for equality of two population variance 
# 
# # Describe how will you perform the following statistical tests stating the assumption.
# (i). H0 : σ²= σ₀² vs H1 : σ²> σ₀²	        (ii). H0 : σ²= σ₀² vs H1 : σ²< σ₀²
# (iii). H0 : σ²= σ₀² vs H1 : σ²≠ σ₀².
# Answer: Suppose we have two samples.The first sample contains n1 observations and has sample variance s12 .The second sample contains n2 observations and has sample variance s22.
# Here we assume that the first sample comes form a normal population with mean µ1 and variance σ12. The second sample comes from a normal distribution with mean µ2 and variance σ22. All the observation are independent.
# We want to test the following hypothesis at 100α% level of significance.
# (i).Right tailed test:
#   H0 : σ²= σ₀² 
# H1 : σ²> σ₀² 
# To test the above hypothesis, we want to use the test statistic F, defined by
# F=(〖s1〗^2/〖σ1〗^2)/(〖s2〗^2/〖σ2〗^2 ) ~ F(n1-1),(n2-1) 
# Under the null hypothesis the test statistic becomes 
# F=〖s1〗^2/〖s2〗^2  ~ F(n1-1),(n2-1) 
# The critical region w of size α is given by
# W:[F> Fα,(n1-1),(n2-1)]; where Fα,(n1-1),(n2-1) is the upper 100α% point of F distribution with (n1-1),(n2-1) df.
# (ii). Left tailed test : 
#   H0 : σ²= σ₀² 
# H1 : σ²< σ₀²
# To test the above hypothesis, we use the test statistic F, defined by
# F=(〖s2〗^2/〖σ2〗^2)/(〖s1〗^2/〖σ1〗^2 ) ~ F(n2-1),(n1-1) 
# under the null hypothesis the test statistic becomes
# F=〖s2〗^2/〖s1〗^2  ~ F(n2-1),(n1-1) 
# 
# The critical region w of size α is given by
# w:[F < F ,(n2-1),(n1-1)]; where ,F  ,(n2-1),(n1-1) is the upper 100α% point of F distribution with (n2-1)(n1-1)df.
# (ii)Two tail test:
#   H0:σ_1^2=σ_2^2
# H1: σ_1^2≠σ_2^2
# To test the above hypothesis, we use the test statistics F, defined by
# 
# F=(Larger sample variance)/(smaller sample variance)~F(p,q)
# 
# Where, P is the df corresponding to the larger sample variance and q is the df corresponding to the smaller sample variance.
# 
# The critical region w of size α is given by
# W:[p<F_((1-α/2),(p,q) )]; where F_(α/2,(p,q)) is the upper 50α% point of F distribution with (p, q) df and   F_((1-α/2),(p,q) )  is the upper 50(1-α/2)%  point of F distribution with (p, q) df.
# 
# Decision/comment: For (i), (ii) and (iii) if the calculated value of the test statistics F, falls in critical region w, the null hypothesis may be rejected in favor of the alternative hypothesis at 100α% level of significance; otherwise, the null hypothesis may be failed to reject.
# ---------------------------------------------------------------------------------------------------------------------------------
# QUESTION: Test for equality of several population variances 
# 
# Bartlet’s test of hypothesis for testing equality of several variances.
# Describe how will you perform the following statisticsal test stating assumption:
#   H0: σ_1^2=σ_2^212=...= σ k2                                                                                       
# H1: H0 is not true
# ANSWER:
#   Let us assume that k independent samples.The ith(i=1,2,…,k) sample contains ni observation and have sample variances Si2.
# Let n=∑_(i=1)^k▒n_i .All samples come from  normal population.
# The mean and variance of the first population µ1 and s21 and mean and variance of second population is m2 and s22 respectively and so on.
# We want to test the hypothesis that the variances are equal at 100% level of significance.
# H0: σ_1^2=σ_2^212=...= σ k2
# H1: H0 is not true 
# Test statistic is c2  defined by
# c2=  1/c [(n-k)logS2-∑_(i=1)^k▒〖(n_i-1)〗logSi2]~ c2(k-1)
# Where, c=1+1/3(k+1) [ ∑_(i=1)^k▒1/(ni-1)-1/(n-k)]S2=(∑_(i=1)^k▒〖(n_i-1) 〖S_i〗^2 〗)/(n-k)
# Si2=1/(n_i-1) ∑_(j=1)^k▒(xij-(x_i ) ̅)2 ;     i=1,2,…,k
# 
# Decision/Comment:If the calculated value of  c2 is greater than the tabulated value of c2 ,i.e. c2cal> c2a,(k-1)  then we may reject our null hypothesis in favour of the alternative hypothesis at 100% level of significance; otherwise the null hypothesis may be failed to reject.     
# --------------------------------------------------------------------------------------------------------------------------------------------
#   Question: Test for zero correlation coefficient: 
# H0:ρ  = 0 vs H1: ρ > 0
# H0:ρ=0 vs H1: ρ< 0
# H0:ρ=0 vs H1: ρ  ≠0
# Answer:
#   Suppose we are given a set of n independent pairs of observations from a bivariate normal population with correlation coefficient ρ .
# Let, r be the sample correlation coefficient. We want to test the following hypothesis 100α% level of significance.
# Right tail test:                                             Lest tail test:
#   H0:ρ= 0 						H0:ρ= 0
# vs H1: ρ>0					vs H1: ρ < 0
# Two tail test:	H0: ρ=0 vs H1: ρ≠0 
# To test the above hypothesis, we use the test statistic ‘t’ defined by 
# t = (r√(n-2)  )/√(1-r^2 ) ~ t_(n-2)
# (i). For right tail test, H0:ρ= 0 vs H1: ρ >0, the critical region w of size α is given by w: [t > tα, n-2]; where tα, n-2 is the upper 100 α% point of the unit t distribution with (n-2) df.
# (ii). For left tail test, H0:ρ= 0 vs H1: ρ<02 , the critical region w of size α is given by w:[t < -tα, n-2 ]; where -tα, n-2 is the lower 100 α% point of the unit t distribution with (n-2) df.
# (iii). For two tail test, H0:ρ= 0 vs H1: ρ≠0, the critical region w of size α is given by w: [t < -t_(α/2,n-2),t>t_(α/2,n-2)]; where t_(α/2,n-2) is the upper 50 α% point of t-distribution with (n-2) df and -t_(α/2,n-2) the lower 50 α% point of the t distribution with (n-2) df.
# Decision/Comment: If the calculated value of the test statistics ‘t’ falls in critical region w, the null hypothesis may be rejected in favor of the alternative hypothesis at 100 α% level of significance; otherwise the null hypothesis may be failed to reject.
# ------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Question: Test for a specific value of correlation coefficient:
# H0:ρ  = ρ_0  vs H1: ρ > ρ_0
# H0:ρ=ρ_0  vs H1: ρ<ρ_0
# H0:ρ=ρ_0 vs H1: ρ≠ρ_0
# Answer:
#   Suppose we are given a set of n independent pairs of observations (x1, y1), (x2, y2), …, (xn, yn). We assume bivariate normal population with correlation coefficient ρ. The n pairs are independent. We want to test the following hypothesis at 100α% level of significance.
# Right tail test:       Lest tail test:
#   H0:ρ= ρ_0						H0:ρ= ρ_0
# vs H1: ρ>ρ_0					vs H1: ρ < ρ_0
# 
# Two tail test:	H0: ρ=ρ_0 vs H1: ρ≠ρ_0 
# To test the above hypothesis, we use the test statistic ‘T’ defined by 
# T = √(n-3) (z-m)~N(0,1)
# Where ‘T’ is treated as a standardized normal variate.
# Where, z = (1 )/2 〖log〗_e  (1+r )/(1-r)   and m=  (1 )/2 〖log〗_e  (1+ρ_0  )/(1-ρ_0 )
# (i). For right tail test, H0:ρ= ρ_0 vs H1: ρ >ρ_0, the critical region w of size α is given by w: [T > Tα]; where Tα is the upper 100 α% point of the unit normal distribution.
# (ii). For left tail test, H0:ρ= ρ_0 vs H1: ρ< ρ_0, the critical region w of size α is given by w: [T < -Tα]; where -Tα is the upper 100 α% point of the unit normal distribution.
# (iii). For two tail test, H0:ρ= ρ_0  vs H1: ρ≠ρ_0, the critical region w of size α is given by w: [T < -T_(α/2),T>t_(α/2)]; where T_(α/2) is the upper 50 α% point of the unit normal distribution and -T_(α/2) is the lower 50 α% point of the unit normal distribution.
# Decision/Comment: If the calculated value of the test statistics T falls in critical region w, the null hypothesis may be rejected in favor of the alternative hypothesis at 100 α% level of significance; otherwise the null hypothesis may be failed to reject.
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Question: Equality of several correlation test or test   for the homogeneity of a set of correlation coefficients;
# H0:r1= r2= ...=rk                                                                                       
# H1: H0 is not true
# ANSWER: 
#   Let r1,r2,…,rk  be k correlation coefficients based on samples of sizes n1,n2,…,nk . We wish to test the hypothesis that these coefficients have been obtained from populations with the same coefficient.  
# thus 
# H0:r1=r2=...=rk  
# H1: H0 is not true
# By means of Fisher’s transformation We first obtain z1,z2,…,zk corresponding to r1,r2,…,rk  respectively.Then the test statistics is 
# c2=∑_(i=1)^k▒〖(n_i-3) z_i 〗 -  〖{∑_(i=1)^k▒〖(n_i-3) z_i}〗〗^2/(∑_(i=1)^k▒〖(ni-3)〗)
# Which is distributed as approximately c2 with (k-1) df.
# It is assumed that the correlation coefficients are obtained from independent random samples which originate from bivariate normal populations.
# 
# Decision/Comment:If the calculated value of c2 is greater than tabulated value of c2,i.e. c2cal> c2a,(k-1)  ,then we may reject our null hypothesis in favour of the alternative hypothesis at 100% level of significance ;otherwise null hypothesis may be failed to reject.
# ------------------------------------------------------------------------------------------------------------------------------
# QUESTION:  Equality of two correlation test:
# H0 : r1= r2
# H1: r1 ≠ r2
# ANSWER:
#   The  correlation coefficients derived from two samples of sizes n1 and n2 respectively are r1 and r2
# The null hypothesis and alternative hypothesis is
# H0 : r1= r2
# H1: r1 ≠ r2
# Let z1=1/2 loge(1+r_1)/(1-r_1 ) and z2= 1/2 loge(1+r_2)/(1-r_2 )
# Then under null hypothesis (z1-z2) is approximately normally distributed with zero mean and variance □(1/(n_1-3))+□(1/(n_2-3))
# 
# Hence the test statistic d is
# D=((z_1-z_2 ))/√(□(1/(n_1-3))+□(1/(n_2-3))) ;
# Which is treated as standardized normal variate.
# This is usually two tailed test and critical values at 5% level of significance are ±1.96
#Comment/Decision: If ldl≥1.96 , then the null hypothesis may reject in the favour of alternative hypothesis; otherwise the null hypothesis may be failed to reject.
