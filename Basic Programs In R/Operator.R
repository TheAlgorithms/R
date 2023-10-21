#Assignment using equal operator
var.1 = c(0,1,2,3)

#Assignment using leftward operator.
var.2 <- c("learn","R")

#Assignment using rightward operator.
c(TRUE,1) -> var.3

print(var.1)
cat ("var.1 is",var.1,"\n")
cat ("var.2 is",var.2,"\n")
cat ("var.3 is",var.3,"\n")

var_x <- "Hello"
cat("The class of var_x is",class(var_x),"\n")

var_x <- 34.5
cat("Now the class of var_x is",(var_x),"\n")

var_x <- 27L
cat ("Next the class of var_X becomes",class(var_x),"\n")