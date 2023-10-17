# set.seed(5)

countdown <- function(xx) {
    ii <- which(xx > 0)
    xx[ii] <- xx[ii] - 1
    xx
}
insert_cars <- function(queues, countdowns, tm, tr) {
    # find queue and insert
    ii <- which.min(queues)
    queues[ii] <- queues[ii] + 1
    if (queues[ii] == 1)
        countdowns[ii] <- ceiling(runif(1, tm, tm + tr))
    list(q=queues, cd=countdowns)
}
update_stations <- function(queues, countdowns, ii, tm, tr) {
    queues[ii] <- queues[ii] - 1

    ii_no_car <- which(queues == 0)
    ii_countdown <- setdiff(ii, ii_no_car)
    countdowns[ii_countdown] <- ceiling(runif(length(ii_countdown), tm, tm + tr))
    countdowns[ii_no_car] <- -1
    list(q=queues, cd=countdowns)
}

qsim <- function(mf=5, mb=5, a.rate=.1, trb=40, trf=40, tmb=30, tmf=30, maxb=20) {
    car_coming <- runif(7200 - 1800) < a.rate

    french.countdowns <- rep(-1, mf)  # pos int for time, 0 for countdown over, -1 for no cars ? optimize
    french.queues <- rep(0, mf)

    brit.countdowns <- rep(-1, mb)
    brit.queues <- rep(0, mb)

    nf <- nb <- eq <- rep(0, 7200)

    for (i in 1:7200) {
        # general updates
        french.countdowns <- countdown(french.countdowns)
        brit.countdowns <- countdown(brit.countdowns)

        # Entrance
        if (i < length(car_coming) && car_coming[i]) {
            french <- insert_cars(french.queues, french.countdowns, tmf, trf)
            french.countdowns <- french$cd
            french.queues <- french$q
        }

        # Middle
        idx_rmv <- which(french.countdowns == 0)
        if (length(idx_rmv) != 0 && sum(brit.queues) != mb * maxb) {
            # Freanch Exit
            if (length(idx_rmv) != 1)
                idx_rmv <- sample(idx_rmv, min(mb * maxb - sum(brit.queues), length(idx_rmv)))

            transmit <- length(idx_rmv)
            french <- update_stations(french.queues, french.countdowns, idx_rmv, tmf, trf)
            french.countdowns <- french$cd
            french.queues <- french$q

            # Brit
            for (j in 1:transmit) {
                brit <- insert_cars(brit.queues, brit.countdowns, tmb, trb)
                brit.countdowns <- brit$cd
                brit.queues <- brit$q
            }
        }

        # Exit
        idx_rmv <- which(brit.countdowns == 0)
        if (length(idx_rmv) != 0) {
            brit <- update_stations(brit.queues, brit.countdowns, idx_rmv, tmb, trb)
            brit.countdowns <- brit$cd
            brit.queues <- brit$q
        }

        # result update
        nf[i] <- mean(french.queues)
        nb[i] <- mean(brit.queues)
        eq[i] <- nf[i] * (tmf + trf / 2) + nb[i] * (tmb + trb / 2)
    }

    list(nf=nf, nb=nb, eq=eq)
}

qsim()
