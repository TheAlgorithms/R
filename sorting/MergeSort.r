 merge.merge <- function(a, b) {
   r <- numeric(length(a)+length(b))
   ai <- 1
   bi <- 1
   j<-1
   for(j in 1:length(r)) {
     if((ai<=length(a) && a[ai]<b[bi]) || bi>length(b)) {
       r[j] <- a[ai] + ai <- ai+1
     } ## end if
     else {
       r[j] <- b[bi] + bi <- bi+1
     } ## end else
   } ## end for
   r
 }
 
 merge.sort <- function(x) {
  if(length(x)>1) {
    q <- ceiling(length(x)/2)
    a <- merge.sort(x[1:q])
    b <- merge.sort(x[(q+1):length(x)])
    merge.merge(a, b)
  } ## end if
  else {
    x
  } ## end else
}

# Example:
# merge.sort(sample(1:100,10))
# [1]  6 24 25 39 49 42 44 51 70 100
