combination_emax <- "
model{
for(i in 1:N){
resp[i] ~ dbin(p[i],n[i])
logit(p[i]) <- theta0 + emax1*dose1[i]/(ed50_1+dose1[i]) + emax2*dose2[i]/(ed50_2+dose2[i]) +
               emax12*dose12[i]/(ed50_12+dose12[i])
}

theta0 ~ dnorm(0,0.25)
emax1 ~ dnorm(0,0.25) # add correlation later...
emax2 ~ dnorm(0,0.25)
emax12 ~ dnorm(0,0.25)
ed50_1 ~ dnorm(100,0.0001)I(0,)
ed50_2 ~ dnorm(100,0.0001)I(0,)
ed50_12 ~ dnorm(400,0.00001)I(0,)

for(i in 1:N){
prob[i] <- ilogit(theta0 + emax1*dose1[i]/(ed50_1+dose1[i]) + emax2*dose2[i]/(ed50_2+dose2[i]) +
               emax12*dose12[i]/(ed50_12+dose12[i]))
}

}
" 

combination_linear <- "
model{
for(i in 1:N){
resp[i] ~ dbin(p[i],n[i])
logit(p[i]) <- theta0 + theta1*dose1[i] + theta2*dose2[i] + theta12*dose12[i]
}

theta0 ~ dnorm(0,0.25)
theta1 ~ dnorm(0,0.25)
theta2 ~ dnorm(0,0.25)
theta12 ~ dnorm(0,0.25)

for(i in 1:N){
prob[i] <- ilogit(theta0 + theta1*dose1[i] + theta2*dose2[i] + theta12*dose12[i])
}

}
" 