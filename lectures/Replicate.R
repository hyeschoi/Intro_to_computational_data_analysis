# import arguments from command line
# it import a character vector containing strings that is separated by ' '(space) 
# in the command line input

args <- commandArgs(TRUE)
ndice <- args[1]
nroll <- args[2]
nsample1 <- 100
nsample2 <- 100


if (length(args)>=3){
  nsmample1 <- as.numeric(args[3])
}
if (length(args)>=4){
  nsmample2 <- as.numeric(args[4])
}

set.seed(0)


rolls <- function(){
result <- logical(0)
for (j in 1:nroll){
numbers <- numeric(0)
success <- logical(0)
for(i in 1:ndice){
  numbers[i] <-sample(1:6, ndice, TRUE)
  success[i] <- (6 ==numbers[i])
}
result[j] <- all(success)
}
any(result)
}

prop<- function(x){
  table(x)['TRUE']/length(x)
}


wins <- replicate(nsample1, rolls())
print(prop(wins))



wins <- replicate(nsample2, replicate(nsample1,rolls()))
wins_sample <- apply(wins, 2, prop)
hist(wins_sample)


########### Donovan's answer
roll <- function(){
  res <- sample(1:6, ndice, TRUE)
  all(res ==6)
}

trial <- function(){
  any (replicate(nroll, roll()))
}

print(mean (replicate(nsample1,trial())))

probs <- apply(replicate(nsample2, replicate(nsample1, trial())),2, mean)
hist(probs)