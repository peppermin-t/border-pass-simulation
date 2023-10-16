## Assumptions:
# 1. No queues at the start of the period, i.e., nf = nb = 0 
# 2. Check-in closes 30 mins before departure, i.e., no new arriving cars
# 3. There are no extreme events that will affect the output, e.g., system broke down during the process
# 4. The arrival car takes 0 seconds to join the queue 

## Constants in the model:
# mf is the number of french passport control stations
# mb is the number of british passport control stations
# maxb is the maximum british queue length (per station)
# a.rate is the probability of a car arriving each second

# tf, the processing time for a french station is uniformly distributed between tmf and tmf+trf
# tb, the processing time for a british station is uniformly distributed between tmb and tmb+trb

## For each simulation second, the output should contain:
# nf: the vector of average length of french queues (measured by # cars/mf, length(nf)=7200)
# nb: the vector of average length of british queues (measured by # cars/mb, length(nb)=7200)
# eq: the vector of average expected waiting time for a car at the start of the french queue ()

# Stages: nf, nb
# Update every second for:
#   nf, according to a.rate and 
#   nb, according to 
# If there is a car arriving at French station, 
# compute tf (= the time until the car leave for British station)
# Default simulation parameters: average rate of processing passports by French and British = arrival rate (a.rate)
# 

# n is the number of simulations (initially set as 100)
# In this case, we want number of seconds in a 1.5 hour period. 
nt = 2*60*60 # 7200 seconds, thus 7200 number of simulated seconds (or in 2h timeframe, the rest .5h 0 new arriving cars?)
# 30 mins are deducted as we assume there are no new cars arrive in the final half hour..?


## Simulation model of cars at a French ferry terminal
## (passing through French and then British passport control)
qsim <- function(n=nt,mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20){
  nf <- rep(0,n) ## initialise average length of French queues
  nb <- rep(0,n) ## initialise average length of British queues
  fq <- list(f1=0, f2=0, f3=0, f4=0, f5=0) ## initialise length of queue in each of the 5 French stations
  x <- c(x1=0, x2=0, x3=0, x4=0, x5=0) # initialise arrival time (seconds) of each car in each x1 to x5 french station
  y <- c(y1=0, y2=0, y3=0, y4=0, y5=0) # initialise processing time (seconds) for each car in each y1 to y5 french station
  
  tf <- runif(n, min = tmf, max = tmf+trf) # n simulations of processing times at each French station
  tb <- runif(n, min = tmb, max = tmb+trb) # n simulations of processing times at each British station
  
  a.car <- rpois(n, a.rate) ## indication of arrival car per second
  
  for(i in 1:n){ ## loop over seconds
    
    # At the French border
    if(a.car[i]==1){
      min.fq = sample(lapply(fq, min),1) # find station with shortest queue 
      # (use lapply to make it stay as a list? OR 
      # use "which.min" to return indices of the min - see below)
      # fq[[which.min(fq)]] = fq[[which.min(fq)]]+1
      fq = min.fq[[1]] + 1 # shortest french queue will have one more car (how to combine this new list back to our list?)
      x[[1]] = c(x[[1]], i) # input arrival time of each new car in the "first" station
      y[[1]] = c(y[[1]], tf[i]) # input processing time for each car in the "first" station
      
    }
    else (a.car[i]==0){ # error here - to be checked
      fq = fq # length of french queue remains the same
      fs[i] = 0 # no arrival cars, thus no station is selected
      # shld we record arrival time and processing time as "0", if there is no car? 
    }
    
    nf[i] = sum(unlist(fq))/mf #input average length of french queue
    y[y>=0] = y[y>=0]-1 # decrease processing time by one sec before simulating the next second, should not fall below zero
    
    # Determine which car will move to British border
    fq[which(y==0)&fq!=0] = fq[which(y==0)&fq!=0] - 1 # When the processing time turns zero and length of fq is not zero, the queue has one car less 
    # AND One of the British stations will have one more car
    # Each station can only process one car at a time
  }
  list(nf=nf, nb=nb, eq=eq) # output of the model
}

### Removed:
fq = c(f1=0, f2=0, f3=0, f4=0, f5=0) ## initialise length of queue in each French station (or assign 1to5 representing each station)
