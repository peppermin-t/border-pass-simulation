qsim <- function(mf=5, mb=5, a.rate=.1, trb=400, trf=4, tmb=300, tmf=3, maxb=20) {
    len <- function(queues) {
        sapply(queues, length)
    }
    countdown <- function(xx) {
        ii <- which(xx > 0)
        xx[ii] <- xx[ii] - 1
        xx
    }
    top <- function(xx) {
        sapply(xx, function(x) x[[1]])
    }
    pop <- function(xx) {
        for (i in seq_along(xx)) {
            xx[[i]] <- xx[[i]][-1]
        }
        xx
    }
    u <- runif(7200 - 1800)
    car_coming <- u < a.rate

    middle <- c()

    french.countdowns <- as.vector(rep(-1, mb))
    # pos int for time, 0 for countdown over, -1 for no cars
    french.queues <- vector("list", mf)

    brit.countdowns <- as.vector(rep(-1, mb))
    brit.queues <- vector("list", mb)

    nf <- nb <- rep(0, 7200)
    # TODO: eq

    for (i in 1:7200) {
        # general updates
        french.countdowns <- countdown(french.countdowns)
        brit.countdowns <- countdown(brit.countdowns)

        # French
        if (i < length(u) && car_coming[i]) {
            # find queue and insert
            idx_insert <- which.min(len(french.queues))
            french.queues[[idx_insert]] <- c(french.queues[[idx_insert]], i)
            if (len(french.queues)[idx_insert] == 1)
                french.countdowns[idx_insert] <- round(runif(1, tmf, tmf + trf))
        }
        # detect whether a car is finished
        idx_rmv <- which(french.countdowns == 0)
        if (length(idx_rmv) != 0 && sum(len(brit.queues)) != mb * maxb) {
            # the brits queue is not full
            if (length(idx_rmv) != 1)
                idx_rmv <- sample(idx_rmv, min(mb * maxb - sum(len(brit.queues)), length(idx_rmv)))
            # french side
            car_out <- top(french.queues[idx_rmv])
            french.queues[idx_rmv] <- pop(french.queues[idx_rmv])

            idx_no_car <- which(len(french.queues) == 0)
            idx_countdown_renew <- setdiff(idx_rmv, idx_no_car)
            french.countdowns[idx_countdown_renew] <- round(runif(length(idx_countdown_renew), tmf, tmf + trf))
            # the rest of them are still 0
            french.countdowns[idx_no_car] <- -1  # -xx has to ensure that xx is not null
            # brit side
            middle <- c(car_out)
        }

        # Brit
        if (length(middle) != 0) {
            # after filtering from french queues, the middle cars definitely have place
            # find queue and insert
            for (start_time in middle) {
                idx_insert <- which.min(len(brit.queues))
                brit.queues[[idx_insert]] <- c(brit.queues[[idx_insert]], start_time)
                if (len(brit.queues)[idx_insert] == 1)
                    brit.countdowns[idx_insert] <- round(runif(1, tmb, tmb + trb))
            }
        }
        # detect whether a car is finished
        idx_rmv <- which(brit.countdowns == 0)
        if (length(idx_rmv) != 0) {
            car_out <- i - top(brit.queues[idx_rmv])
            brit.queues[idx_rmv] <- pop(brit.queues[idx_rmv])

            idx_no_car <- which(len(brit.queues) == 0)
            idx_countdown_renew <- setdiff(idx_rmv, idx_no_car)
            brit.countdowns[idx_countdown_renew] <- round(runif(length(idx_countdown_renew), tmb, tmb + trb))
            brit.countdowns[idx_no_car] <- -1
        }
        nf[i] <- mean(len(french.queues))
        nb[i] <- mean(len(brit.queues))
    }

    list(nf=nf, nb=nb)
}

set.seed(5)
qsim()

# problem1: random for queue selecting
# no random. I think it maybe doesn't affect the simulation result.
# idx_insert <- which(length(french.queues) == min(length(french.queues)))
            # if (length(idx_insert) != 1) {
            #     idx_insert <- sample(idx_insert)
            # }
# problem2: runif rounding
# problem3: eq
# problem4: setdiff
# problem5: processing time generated in the front or 实时的