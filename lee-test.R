# library(systemfit)
# It was expected to use this, but it wasn't needed


# main function to give t-statistic for test
lee_one_break_test <- function(yt,lambda){
  l <- length(yt)
  tb <- floor(lambda*l)
  df <- data.frame(yt)
  df$one <- 1
  df$time <- (1:l)
  df$dt <- df$one
  df$dtt <- df$time - tb
  for (t in 1:tb){
    df$dt[t] <- 0
    df$dtt[t] <- 0
  }
  new_df <- data.frame(yt=diff(df$yt), dt=diff(df$dt), dtt=diff(df$dtt))
  # new_df$one <- diff(df$one)
  # print(new_df)
  st <- residuals(lm(yt~dt+dtt,data=new_df))
  reg_df <- new_df[2:(l-1),]
  reg_df$st_1 <- st[1:(l-2)]
  lin_mod <- lm(yt~dt+dtt+st_1,data=reg_df)
  tval <- summary(lin_mod)$coefficients[,'t value']['st_1']
  return(tval)
}


# syntax to get statistic for a series at a given breakpoint
lee_one_break_test(rnorm(1000),0.61)


# run some simulations to get distribution of statistic
# distribution to be used for critical values
num_sim <- 500
tvals <- numeric(num_sim)
times <- integer(num_sim)
for (i in 1:num_sim){
  
  # assumed these coefficients (completely arbitrary)
  # TODO: check if test sensitive to choice of coefficients
  d1 <- 1
  d2 <- 2
  d3 <- 3
  d4 <- 4
  phi <- 1
  
  # create a time series of 100 points with break at 50
  l <- 100
  tb <- 50
  et <- rnorm(l)
  xt <- cumsum(et)
  one <- 1
  time <- (1:l)
  dt <- one
  dtt <- time - tb
  for (t in 1:tb){
    dt[t] <- 0
    dtt[t] <- 0
  }
  
  # generate yt
  yt <- d1*one + d2*time + d3*dt + d4*dtt + phi*xt
  
  # get t-stat for different potential breakpoints
  # choose the one with best statistic
  # store the calculated breakpoint and statistic
  min_t <- 1000 #arbitrary large value
  min_time <- 0
  for (tb in 1:l){
    t <- lee_one_break_test(yt,tb/l)
    if (t<min_t){
      min_t <- t
      min_time <- tb
    }
  }
  tvals[i] <- min_t
  times[i] <- min_time
  # print(i)
}


#   eq1 <- y ~ ylag + xlag + xl + yl
#   eq2 <- x ~ ylag + xlag + xl + yl
#   sol <- systemfit(formula=c(eq1,eq2))
#   return(sol)
