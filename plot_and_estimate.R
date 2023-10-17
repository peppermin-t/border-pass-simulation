#plot

#qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

#测试数据
nf<-c(5,6,2,3,6,5,7,8,3,5,8,0,0)
nb<-c(3,4,5,6,7,6,5,6,6,7,0,6,0)
eq<-c(3,3,3,4,5,3,3,4,6,4,5,4,5)

par(mfrow=c(2,2)) #创建4张图的模型
x_indices <- seq_along(nf) #将nf的index对应到x轴

#在左上角的图中画出nf的点状图
plot(x_indices, nf, col = "red", xlab = "current time/s", ylab = "queue lengths", main = "queue lengths( tmb=30)",
     xlim = c(0, 20), ylim = c(0, 10))
legend("topright", legend = c("nf", "nb"), col = c("red", "blue"), pch = 1, x.intersp = 0.5, y.intersp = 0.5) #给出对应颜色的批注
points(x_indices, nb, col="blue") #在第一张图中添加nb的点状图

plot(x_indices, eq, col = "green", xlab = "current time/s", ylab = "queue time", main = "expected queue time( tmb=30)",
     xlim = c(0, 20), ylim = c(0, 10))
legend("topright", legend = "nf", col = "green", pch = 1, x.intersp = 0.5, y.intersp = 0.5)

#qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)

#测试数据
nf<-c(3,3,3,4,5,3,3,4,6,4,5,4,5)
nb<-c(5,6,2,3,6,5,7,8,3,5,8,0,0)
eq<-c(3,4,5,6,7,6,5,6,6,7,9,6,0)

plot(x_indices, nf, col = "red", xlab = "current time/s", ylab = "queue lengths", main = "queue lengths( tmb=40)",
     xlim = c(0, 20), ylim = c(0, 10))
legend("topright", legend = c("nf", "nb"), col = c("red", "blue"), pch = 1, x.intersp = 0.5, y.intersp = 0.5)
points(x_indices, nb, col="blue")

plot(x_indices, eq, col = "green", xlab = "current time/s", ylab = "queue time", main = "expected queue time( tmb=40)",
     xlim = c(0, 20), ylim = c(0, 10))
legend("topright", legend = "nf", col = "green", pch = 1, x.intersp = 0.5, y.intersp = 0.5)

#estimate

#测试数据
qsim <- function(trb,trf,tmb){
  nb <-c(2*trb,3*trb,4*trb,5*trb,6*trb,7*trb,8*trb,9*trb)
  nf <-c(2*trf,3*trf,4*trf,5*trf,6*trf,7*trf,8*trf,9*trf)
  eq <-c(2*tmb,3*tmb,4*tmb,5*tmb,6*tmb,7*tmb,8*tmb,9*tmb)
  return(list(nf = nf, nb = nb, eq = eq))
}
result <- qsim(trb=40,trf=40,tmb=30)
nf <- result$nf
nb <- result$nb
eq <- result$eq


last_nf_values <- numeric(100)
last_nb_values <- numeric(100)

for (i in 1:100) {
  result <- qsim(trb=20,trf=40,tmb=30)
  last_nf <- result$nf[length(result$nf)]
  last_nb <- result$nb[length(result$nb)]
  last_nf_values[i] <- last_nf
  last_nb_values[i] <- last_nb
}

#测试数据
last_nf_values<-c(5,6,2,3,6,5,7,8,3,5,8,0,0)
last_nb_values<-c(3,4,5,6,7,6,5,6,6,7,0,6,0)

queue_matrix <- cbind(last_nf_values, last_nb_values)
still_queue <- sum(queue_matrix[, 1] != 0 | queue_matrix[, 2] != 0)
prob <- still_queue/100
print(prob)




####
####
#正式

qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)

par(mfrow=c(2,2)) #创建4张图的模型
x_indices <- seq_along(nf) #将nf的index对应到x轴

#在左上角的图中画出nf的点状图
plot(x_indices, nf, col = "red", xlab = "current time/s", ylab = "queue lengths", main = "queue lengths( tmb=30)",
     xlim = c(0, 7300), ylim = c(0, 100))
legend("topright", legend = c("nf", "nb"), col = c("red", "blue"), pch = 1, x.intersp = 0.5, y.intersp = 0.5) #给出对应颜色的批注
points(x_indices, nb, col="blue") #在第一张图中添加nb的点状图

plot(x_indices, eq, col = "green", xlab = "current time/s", ylab = "queue time", main = "expected queue time( tmb=30)",
     xlim = c(0, 7300), ylim = c(0, 1000))
legend("topright", legend = "nf", col = "green", pch = 1, x.intersp = 0.5, y.intersp = 0.5)

qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=40,tmf=30,maxb=20)

plot(x_indices, nf, col = "red", xlab = "current time/s", ylab = "queue lengths", main = "queue lengths( tmb=40)",
     xlim = c(0, 7300), ylim = c(0, 100))
legend("topright", legend = c("nf", "nb"), col = c("red", "blue"), pch = 1, x.intersp = 0.5, y.intersp = 0.5)
points(x_indices, nb, col="blue")

plot(x_indices, eq, col = "green", xlab = "current time/s", ylab = "queue time", main = "expected queue time( tmb=40)",
     xlim = c(0, 7300), ylim = c(0, 1000))
legend("topright", legend = "nf", col = "green", pch = 1, x.intersp = 0.5, y.intersp = 0.5)

last_nf_values <- numeric(100) #设置空的vector
last_nb_values <- numeric(100)

#将qsim重复跑一百次，记录下最后一秒的预计排队数
for (i in 1:100) {
  result <- qsim(mf=5,mb=5,a.rate=.1,trb=40,trf=40,tmb=30,tmf=30,maxb=20)
  last_nf <- result$nf[length(result$nf)]
  last_nb <- result$nb[length(result$nb)]
  last_nf_values[i] <- last_nf
  last_nb_values[i] <- last_nb
}

queue_matrix <- cbind(last_nf_values, last_nb_values) 
still_queue <- sum(queue_matrix[, 1] != 0 | queue_matrix[, 2] != 0) #nf或者nb其中一边排队人数不为0，就代表没处理完
prob <- still_queue/100
print(prob)
