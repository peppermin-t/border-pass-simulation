##########################################
#  Simulation of passport control queues #
##########################################


# Members and Contribution Breakdown



# This R code consists of a model that will help simulating the passport
# control queues, particularly on simulating cars moving through French, followed
# by British passport control at a French ferry terminal. The main function used
# for such simulation is named as qsim.

# Model Assumptions:
# 1. There are no queues at the start of the period, i.e., nf = nb = 0
# 2. Check-in closes 30 mins before departure, i.e., no new arriving cars
# 3. There are no extreme events that will affect the output, e.g., system broke down during the process
# 4. The arrival car takes 0 seconds to join the queue
#    - "queue" refers to the French queue at the start and British queue during transmission

# Constants in the qsim model:
# 1. mf is the number of french passport control stations
# 2. mb is the number of british passport control stations
# 3. maxb is the maximum british queue length (per station)
# 4. a.rate is the probability of a car arriving each second

# Parameters in the qsim model: 
# 1. tf, the processing time for a french station is uniformly distributed between tmf and tmf+trf
# 2. tb, the processing time for a british station is uniformly distributed between tmb and tmb+trb
# # Default simulation parameters: average rate of processing passports by French and British = arrival rate (a.rate)

# For each simulation second, the output should contain:
# 1. nf: the vector of average length of french queues (measured by # cars/mf, length(nf)=7200)
# 2. nb: the vector of average length of british queues (measured by # cars/mb, length(nb)=7200)
# 3. eq: the vector of average expected waiting time for a car at the start of the french queue

# Stages: nf, nb
# We update every second for:
# 1. nf, according to a.rate and
# 2. nb, according to processing time assigned to each car and the length of British queue
# # If there is a car arriving at French station,
# # compute tf (= the time until the car leave for British station)


# n is the number of simulated seconds.
# In this case, we are simulating queues for 2 hours, thus n=7200


# Define "countdown" function
countdown <- function(xx) {

  # extract index of numbers in a vector xx that are greater than zero
  ii <- which(xx > 0)

  # these number will decrease by one
  xx[ii] <- xx[ii] - 1

  # return vector xx
  xx
}

# Define "insert_cars" function
insert_cars <- function(queues, times_left, tm, tr) {

  # extract the index of the shortest queue
  ii <- which.min(queues)

  # this queue will increase by one (car) 
  queues[ii] <- queues[ii] + 1

  # if that queue has only one (car) 
  if (queues[ii] == 1)

    # assign processing time with the given parameters tm and tr
    # the random processing time is generated using sample function..?
    # !!!! This line means we are assigning the same processing time for cars..?
    times_left[ii] <- sample(tm:(tm + tr), 1)

  # Return outputs in a list
  list(q=queues, cd=times_left)
}

# Define "update_stations" function
update_stations <- function(queues, times_left, ii, tm, tr) {

  # queues at each station will have one car less
  queues[ii] <- queues[ii] - 1

  # extract index of queues that has no cars
  ii_no_car <- which(queues == 0)

  # extract index of queues that has cars..? (could have used which(queues!=0)..?)
  ii_countdown <- setdiff(ii, ii_no_car)

  # assign processing times for th
  times_left[ii_countdown] <- sample(tm:(tm + tr), length(ii_countdown), replace = TRUE)


  times_left[ii_no_car] <- -1
  list(q=queues, cd=times_left)
}

# Define "qsim" function
qsim <- function(mf=5, mb=5, a.rate=.1, trb=40, trf=40, tmb=30, tmf=30, maxb=20) {

  # Number of simulated seconds
  n <- 2 * 60 * 60 # 7200
  n_ <- 0.5 * 60 * 60

  car_coming <- runif(n - n_) < a.rate

  french.times_left <- rep(-1, mf)  # pos int for time, 0 for countdown over, -1 for no cars ? optimize
  french.queues <- rep(0, mf)

  brit.times_left <- rep(-1, mb)
  brit.queues <- rep(0, mb)

  nf <- nb <- eq <- rep(0, n) # initialise outputs of the model

  for (i in 1:n) { ## loop over seconds

    # general updates
    french.times_left <- countdown(french.times_left)
    brit.times_left <- countdown(brit.times_left)

    ## Entering French border
    if (i < length(car_coming) && car_coming[i]) {
      french <- insert_cars(french.queues, french.times_left, tmf, trf)
      french.times_left <- french$cd
      french.queues <- french$q
    }

    ## Transmission (Moving from French border to British border)
    idx_rmv <- which(french.times_left == 0)
    if (length(idx_rmv) != 0 && sum(brit.queues) < mb * maxb) {
      # Exit from French border
      if (length(idx_rmv) != 1)
        idx_rmv <- sample(idx_rmv, min(mb * maxb - sum(brit.queues), length(idx_rmv)))

      transmit <- length(idx_rmv)
      french <- update_stations(french.queues, french.times_left, idx_rmv, tmf, trf)
      french.times_left <- french$cd
      french.queues <- french$q

      # Entering British border
      for (j in 1:transmit) {
        brit <- insert_cars(brit.queues, brit.times_left, tmb, trb)
        brit.times_left <- brit$cd
        brit.queues <- brit$q
      }
    }

    # Exit from British border
    idx_rmv <- which(brit.times_left == 0)
    if (length(idx_rmv) != 0) {
      brit <- update_stations(brit.queues, brit.times_left, idx_rmv, tmb, trb)
      brit.times_left <- brit$cd
      brit.queues <- brit$q
    }

    # Outputs

    # nf and nb are computed by a vector with "mf" and "mb" number of lists
    nf[i] <- mean(french.queues)
    nb[i] <- mean(brit.queues)

    # eq is computed based on the sum of 
    # 1. multiplication of nf and average processing time at each French station
    # 2. multiplication of nb and average processing time at each British station
    eq[i] <- nf[i] * (tmf + trf / 2) + nb[i] * (tmb + trb / 2)
  }

  # Print the outputs as vectors
  list(nf=nf, nb=nb, eq=eq)
}


plot_qsim <- function(res, params) {
  x_indices <- seq_along(res$nf) #将nf的index对应到x轴

  #在左上角的图中画出nf的点状图
  plot(x_indices, res$nf, col = "red", xlab = "current time/s", ylab = "queue lengths",
     main = paste("queue lengths(", params, ")"),
     xlim = c(0, 7300), ylim = c(0, 20))
  legend("topright", legend = c("nf", "nb"), col = c("red", "blue"),
     pch = 1, x.intersp = 0.5, y.intersp = 0.5) #给出对应颜色的批注
  points(x_indices, res$nb, col="blue") #在第一张图中添加nb的点状图

  plot(x_indices, res$eq, col = "green", xlab = "current time/s", ylab = "queue time",
     main = paste("expected queue time(", params, ")"),
     xlim = c(0, 7300), ylim = c(0, 2000))
  legend("topright", legend = "eq", col = "green", pch = 1, x.intersp = 0.5, y.intersp = 0.5)
}

par(mfrow=c(2, 2)) #创建4张图的模型

plot_qsim(qsim(), "tmb=30")

plot_qsim(qsim(tmb=40), "tmb=40")


failed <- logical(100)  #设置空的vector

#将qsim重复跑一百次，记录下最后一秒的预计排队数
for (i in 1:100) {
    res <- qsim(tmb=40)
    failed[i] <- res$nf[length(res$nf)] + res$nb[length(res$nb)] != 0
}

prob <- mean(failed)
print(prob)

# 用点还是用线？
# 100次实验，tmb=40？
# 坐标
# 用不用考虑结尾的nf？