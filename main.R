# Algorytmy Genetyczne

# Zadanie 1 ----
cities=read.csv(file.choose(), header = TRUE, sep=",")
View(cites)

# Zadanie 2 ----

totalDistance = function(visitedCities, distances=cities){
  visitedCities = c(visitedCities, visitedCities[1])
  route = embed(visitedCities, 2)[,2:1]
  distanceSum = sum(distances[route])
  return(distanceSum)
}

# Zadanie 3 ----

permu <- funtion(perm, fun, current=NULL){
  for(i in 1:length(perm)){
    fix <- c(current, perm[i])
    rest <-prem[-i]
    
    if(!length(rest)){
      result <-fun(fix)
      if(result<bestResult){
        assign("bestResult", result, envir = .GlobalEnv)
        print (bestResult)
      }
    }
    
    if(length(rest)){
      result <-permu(rest, fun, fix)
    }
  }
}

# [...] (n-1)!/2

# Zadanie 5 ----
require('GA')
library('GA')

# Zadanie 6 ----

costFunction <-function(cfVisitedCities, ...){
  -(totalDistance(cfVisitedCities,...))
}

# Zadanie 7 ----
?GA
?ga
GA.fit <- ga(
  type = "permutation"
  ,fitness = costFunction
  ,min = 1
  ,max = length(cites)
  ,maxiter = 500)
max(GA.fit@fitness)
plot(GA.fit)



