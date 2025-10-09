#####################
# load libraries
setwd("~/GitHub/StatsI_2025/problemSets/PS01/my_answers")
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#We will start finding the mean of our data set to then be able to calculate
#the S:
  
mean_y <- sum(y)/length(y)
mean_y

#To find the sum of errors, we need to create a vector with the sum of the
#demeaned values of y

demeaned <- y - mean_y

#And then, the sum of squared errors

squaredError <- sum(demeaned^2)
squaredError

#I now calculate the variance and the S

variance <- squaredError/(length(y)-1)
S <- sqrt(variance)
S

#Now we need to calculate the mean standard error (SE), thus, S/sqrt(n)

SE <- S/sqrt(length(y))
SE

#Now I'll find the t value, consdiering that the DF = 24 (as length(y) = 25)

tcrit <- qt(0.95, df=24)
tcrit

#Now I will get the Margin of Error (ME), this is, "tcrit x SE"

ME <- tcrit*SE
ME

#Now I just need to define the interval limits according to mean_y

lower <- mean_y - ME
upper <- mean_y + ME
ci_90_y <- c(lower, upper)
ci_90_y

#This means that the 90% confidence interval for the average student IQ
#in the school is [93.96, 103.92], meaning that, with 90% of confidence,
#the real average IQ for this school's students is between both values.

#2. Next, the school counselor was curious whether the average student IQ
#in her school is higher than the average IQ score (100) among all the
#schools in the country. Using the same sample, conduct the appropriate
#hypothesis test with alfa = 0.05.

#To do this, we have a H0 that mu = 100 and a H1 that mu > 100. I need to
#do a T test.

t <- (mean_y - 100)/SE
t

alfa <- 0.05
df <- length(y)-1
tcrit <- qt(1-alfa, df)
tcrit

#Now, I must compare t (observed) and tcrit

t
tcrit

#As t is clearly lower than t, I can't reject the H0. Therefore, I don't have strong evidence to tell the school counselor whether the average student IQ in her school is higher than the average IQ score (100) among all the schools in the country.

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)


View(expenditure)


#2.1. Please plot the relationships among Y, X1, X2, and X3? What are the
#correlations among them (you just need to describe the graph and the
#relationships among them)?

#I start selecting the variables I'm interested in:


data_sel <- expenditure[c("Y","X1","X2","X3")]

#And now I can plot the relationship amongst them:

pdf("pdf1.pdf")
plot(data_sel)


#Now, it's time to calculate the correlations between these variables (using the function cor():


table_correlation <- cor(data_sel)
table_correlation

#Interpretation:

#As seen in the graphs, the correlations between all four variables are
#always positive, also their strength varies.

#Y is moderately correlated with all other three, but especially with X1.
#X1, however, is more strongly correlated with X3, while really weakly
#with X2. Finally, X2 and X3 are also weakly correlated.

#2.2.  Please plot the relationship between Y and Region? On average,
#which region has the highest per capita expenditure on housing assistance?

#I will use the function boxplot(), asking r to use data from my dataset
#"expenditure" and to use "Region" as the X and Y as the Y.

boxplot(Y ~ Region, data = expenditure,
        main = "Per capita expenditure on housing assistance per Region",
        xlab = "Region",
        ylab = "Per capita expenditure on housing assistance",
        col = "red")

#According to the graph, region 4 has the highest per capita expenditure
#on housing assistance.

#But we can also calculate the mean per region:

mean_by_region <- tapply(expenditure$Y, expenditure$Region, mean)
mean_by_region

#Now, I confirm that my observation of the graph was correct.

#2.3. Please plot the relationship between Y and X1? Describe this graph
#and the relationship. Reproduce the above graph including one more
#variable Region and display different regions with different types of
#symbols and colors.

#I will use the function boxplot(), asking r to use data from my dataset "data" and to use "X1" as the X and Y as the Y.

pdf("pdf2.pdf")
boxplot(Y ~ X1, data = expenditure,
        main = "Per capita expenditure on housing assistance per X1",
        xlab = "X1",
        ylab = "Per capita expenditure on housing assistance",
        col = "green")

#In general, we can see that the per capita expenditure on housing
#assistance increases as the value of X1 does. This is coherent with
#the correlation or r=0.53 that we have seen previously.

library(ggplot2)
pdf("pdf3.pdf")
ggplot(data = expenditure, aes(x=X1, y=Y, color=as.factor(Region), shape=
                          as.factor(Region)))+geom_point(size=4)+labs(title = "Correlation between oer capita expenditure on housing assistance and X1 per Region",
                                                                      x = "X1",
                                                                      y = "Per capita expenditure on housing assistance",
                                                                      color = "Region",
                                                                      shape = "Region")

#We can now see that there are important regional differences. The
#graphic shows, as we have previously seen, that the per capita
#expenditure on housing assistance is, indeed, highest in Region 4 and
#lowest in Region 3. Also, Regions 3 and 4 seem to be quite homogeneous
#in both dimensions (variables X1 and Y).
