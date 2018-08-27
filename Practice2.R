

attach(mtcars)

with(mtcars, {
  summary(mpg, disp, wt)
  plot(mpg, disp)
  plot(mpg, wt)
})

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)

summary(patientdata)

# List

g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")

mylist <- list(title=g, ages=h, j, k)

mylist[[3]][2,1]


x <- c(8, 6, 4)
x[7] <- 10
x




mydata <- matrix(rnorm(30), nrow=6)

mydata

apply(mydata, 1,mean)





Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster <- data.frame(Student, Math, Science, English,
                       stringsAsFactors=FALSE)

z<- scale(roster[,2:4])
score <- apply(z, 1, mean)

roster <- cbind(roster, score)
y <- quantile(score, c(.8,.6,.4,.2))

roster$grade[score >= y[1]] <- "A"
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"



name <- strsplit((roster$Student), " ")
lastname <- sapply(name, "[", 2)
firstname <- sapply(name, "[", 1)
roster <- cbind(firstname,lastname, roster[,-1])
roster <- roster[order(lastname,firstname),]



for (i in 1:10) print("Hello")



ifelse(score > 0.5, print("Passed"), print("Failed"))
outcome <- ifelse (score > 0.5, "Passed", "Failed")




x <- sample(LETTERS[1:10], 100, replace = TRUE)
x
sort(x)
order(x)
x[order(x)]


a1 <- c(1,2,3,4)
a2 <- c("A", "B", "C","D")
A <- data.frame(a1,a2)
A

b1 <- c(1,2,3,4)
b2 <- c("A1", "B1", "C1","D1")
B <- data.frame(b1,b2)
B


C <- merge(A,B, by.x = "a1", by.y = "b1")

install.packages("vcd")
library(vcd)
head(Arthritis)



table(Arthritis$Improved)
mytable <- with(Arthritis, table(Improved))

prop.table(mytable)*100

mytable <- xtabs(~ Treatment + Improved, data = Arthritis)

table(Arthritis$Treatment, Arthritis$Improved, Arthritis$Sex)

xtabs(~ Treatment  + Sex + Improved, data = Arthritis)


chisq.test(mytable)



tapply(iris$Sepal.Length,iris$Species,sum)
tapply(iris$Sepal.Length, iris$Species, sum)
tapply(iris$Sepal.Length, iris$Species, mean)

x <- round(rnorm(20, 10, 5))
x

unique(x)

Mode <- function(x){
  a = table(x) # x is a vector
  return(a[which.max(a)])
}


which.max(table(x))
Mode(x)





