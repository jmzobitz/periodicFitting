# Make a synthetic data set for testing, using three linear functions

x=seq(0,0.1,length.out=10)
y=seq(0.1,0.8,length.out=10)
z=seq(0.8,1,length.out=10)

fracTime = c(x,y,z)
synthetic=c(10*x,-1/0.6*(y-0.7),.166666/.2*(z-1))

nTimes=10
value=rep(synthetic,times=nTimes)
time=rep(seq(2007,2007+nTimes-1,length.out =nTimes),each=length(time))+rep(fracTime,times=nTimes)

newvalue = value + 3-0.001*time + 0.1*rnorm(length(synthetic)*nTimes)

synthetic_data <- data.frame(date=date_decimal(time),time,product="synthetic",value=newvalue) %>%
  mutate(date=floor_date(date,unit="day"))

use_data(synthetic_data,overwrite = TRUE)
