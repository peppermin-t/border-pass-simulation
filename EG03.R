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
  nf <- rep(0,nt) ## initialise average length of French queues
  nb <- rep(0,nt) ## initialise average length of British queues
  fq = c(f1=0, f2=0, f3=0, f4=0, f5=0) ## initialise length of queue in each French station (or assign 1to5 representing each station)
  a.car <- rpois(nt, a.rate) ## indication of arrival car per second (/ simulation?)
  for(i in 1:nt){ ## loop over seconds
    tf <- runif(n, min = tmf, max = tmf+trf) # n simulations of processing times at each French station
    tb <- runif(n, min = tmb, max = tmb+trb) # n simulations of processing times at each British station
    fq[1] # # cars in each station
    if(a.car[i]==1) f1 = f1 + 1 # 1st french queue will have one more car
    else (a.car[i]==0) fq = fq # length of french queue remains the same
    
    if(i == tf) # When the simulation second matches one of the uniform random numbers generated, one car will leave from French to British station
                  # Maybe need to sort the uniform random numbers generated?
                  # Each station can only process one car at a time
  }
}

### Removed:
