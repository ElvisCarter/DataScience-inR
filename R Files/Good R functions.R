#load dataset

math_student_data_url <- "https://raw.githubusercontent.com/arunk13/MSDA-Assignments/master/IS607Fall2015/Assignment3/student-mat.csv";
math_student_data <- read.table(file = math_student_data_url, header = TRUE, sep = ";");

# select important columns
working_student_dataset <- math_student_data[c(31, 32, 33)];

 
#Calculate covariance matrix

c <- cov(working_student_dataset)

# Scatter plot 
plot(working_student_dataset$G1, working_student_dataset$G2, xlab="G1", ylab="G2", pch=19)
 
# Histogram 
hist(working_student_dataset$G2)

working_student_dataset <- math_student_data[c(1,31,32,33)];

gp_students <- working_student_dataset[working_student_dataset$school=='GP',]

ms_students <- working_student_dataset[working_student_dataset$school=='MS',]

#Boxplot
boxplot(gp_students$G1,ms_students$G1,
        xlab="School name", ylab="G1")

# Supervised scatter
plot(working_student_dataset$G1, working_student_dataset$G2, col=c("red","blue")[working_student_dataset$school],pch=19)

# Calculate zscore 
zscore <- working_student_dataset

zscore$G1 <- (working_student_dataset$G1 - mean(working_student_dataset$G1)) / sd(working_student_dataset$G1)
zscore$G2 <- (working_student_dataset$G2 - mean(working_student_dataset$G2)) / sd(working_student_dataset$G2)
zscore$G3 <- (working_student_dataset$G3 - mean(working_student_dataset$G3)) / sd(working_student_dataset$G3)

#
zscore$sch <- NA
zscore$sch[zscore$school=='GP'] <- 0
zscore$sch[zscore$school=='MS'] <- 1

model <- lm(G3 ~ G2 + G1,data = zscore)

# summary about the linear model, includes R square
str(summary(model))

# Classification Tree with rpart
library(rpart)

# grow tree
fit <- rpart(G3 ~ G1 + G2,
             method="anova", data=zscore, minsplit = 20, maxdepth = 20)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

 
# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

