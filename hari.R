
#mapping the route

library(ggmap)

from <- c("Jersey City , USA")
to <- c("Liberty Mutual Tower, Boston, USA")
mapdist(from, to)
route_df <- route(
  from,
  to,
  structure = 'route',
  mode = 'driving',+alternatives = TRUE
)
qmap('New York, USA', zoom = 7) +
  geom_path(
    aes(x = lon, y = lat),
    colour = 'red',
    size = 1.5,
    data = route_df,
    lineend = 'round'
  )

#route analysis
library(mice)
library(VIM)

aggr(nyc)

#Removing missing values

motor = nyc[complete.cases(nyc),]
motor1 = motor[1:1000,]
motor2 = motor[1001:1100,]

aggr(motor)

y <- lm(motor1$NUMBER.OF.PERSONS.INJURED ~ motor1$DATE + motor1$TIME + motor1$LATITUDE + motor1$LONGITUDE + motor1$NUMBER.OF.PERSONS.KILLED
        , data = motor1)

summary(y)

library(car)
influencePlot(y)
plot(y)

#safety index analysis(with error)

pred33 <- predict(y, newdata=motor2,type="response")

fitted.results <- ifelse(pred33 > 0.75,1,0)

table(fitted.results, motor2$NUMBER.OF.PERSONS.INJURED)

#time-series analysis

ts1 <- ts(nyc, start=c(2009, 1), end=c(2014, 12), frequency=12)
plot(ts1)

fit <- arima(ts1, order=c(1,0,1)
             
# predictive accuracy

library(forecast)

accuracy(fit)
forecast(fit, 5)
plot(forecast(fit, 5))

# Automated forecasting using an ARIMA model

fit <- auto.arima(ts1)


