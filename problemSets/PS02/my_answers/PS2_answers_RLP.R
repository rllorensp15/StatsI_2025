data <- matrix(c(14, 6, 7,7, 7, 1), nrow = 2,byrow = TRUE)
rownames(data) <- c("Upper", "Lower")
colnames(data) <- c("Not_Stopped", "Bribe_Requested", "Stopped_Warning")
data
#Now I will calculate the expected values under the assumption of independence.
#We will start by summing all the rows, columns, and the table as a whole.
row_totals <- rowSums(data)
column_totals <- colSums(data)
table_total <- sum(data)
#I will use the function "outer" to calculate the Outer Product of the two
#vectors "column_totals" and "row_totals" and then divide it by "table_total"
expected <- outer(row_totals,column_totals)/table_total
expected
#Now I've a table with the expected values under the assumption of independence.
#With this, I can apply the chi square statistic formula:
chi_square_stat <- sum((data-expected)^2/expected)
chi_square_stat
#Now, to check if I did it right, I will ask R to calculate the value too:
chisq.test(data)
#In effect, the value remains 3.7912, which is the same I obtained manually.
  
#To calculate the p-value, I already have the chi_square_stat, but I need to
#calculate the degrees of freedom too.
#The DF are just a multiplication of the n of rows - 1 x the n of columns - 1:
df <- (nrow(data) - 1) * (ncol(data) - 1)
df
#The DF are therefore 2.
#Now, I will use the pchisq function:
p_value <- 1 - pchisq(chi_square_stat, df)
#I wil calculate the critical value for alfa = 0.1
qchisq(0.9, df = 2)
#The value is 4.6052. Thus, for my CI, the chi_square_state is still
#smaller than the critical value (3.8 < 4.6), meaning that I can't reject
#the null hypothesis.

#Now, I will calculate the standardised residuals for each cell.
#I already have the matrix "data" and the matrix "expected".
#I can calculate the simple ones like that:
std_res <- (data - expected) / sqrt(expected)
std_res
#But the adjusted ones correct biases and are thus better for interpretation:
row_prop <- row_totals/table_total
column_prop <- column_totals/table_total
adj_std_res <- (data - expected) / sqrt(expected * (1 - row_prop[row(data)]) * (1 - column_prop[col(data)]))
adj_std_res

#After I've downloaded the data set in a CSV format, I ask R to read it while
#renaming it "data_India".

data_India <- read.csv("C:/Users/Usuario/Documents/GitHub/StatsI_2025/problemSets/PS02/my_answers/women_in_India.csv")
View(data_India)

#To run a bivariate regression, I need to create a model that crosses the two
#variables ("water" and "female").
model1 <- lm(water ~ female, data = data_India)
summary(model1)

#Now, I will run a bivariate regression to crosses the two variables ("water"
#and "reserved").
model2 <- lm(water ~ reserved, data = data_India)
print(summary(model2))

#However, a model that controls for the other variables might be more explanatory.
model3 <- lm(water ~ female + reserved + irrigation, data = data_India)
summary(model2)
