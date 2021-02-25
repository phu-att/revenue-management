######################################################################
######################################################################
######################################################################
########################## Question 1 ################################
######################################################################
######################################################################

#Question 1. - Single resource, two fare class setting using Poisson distribution

#1.a) i)
# preparation of variables using values specified in the question

meanlow=50          # Mean Demand for Low-Fare, Poisson
meanhigh =20           # Mean Demand for High-Fare, Poisson
pricelow=1.00           # Price for Low-Fare
pricehigh =1.50          # Price for Low-Fare
capacity=50    # Capacity 
expectedrev =rep(0,capacity+1) #initialization of set of values for expected revenue (51 in number)
for (i in 1:1){
  protect=i-1 #initialization of protection variable
  lowfare_avail =capacity-protect;
  expectedrev[i]=0;
  for(demandlow in 0:200){
    #selection of values from available commodities of low fare and mean demand for low fare
    lowfare_sold=min(lowfare_avail,demandlow) 
    #recalculation of high fare
    highfare_remain=capacity-lowfare_sold
    for(demandhigh in 0:200){
      #selection of values from available commodities of high fare and mean demand for high fare
      highfare_sold=min(highfare_remain,demandhigh)
      #calculation of revenue for the each iteration using variables selected for that particular iteration
      iter_revenue=pricelow*lowfare_sold+pricehigh*highfare_sold
      #assigning values as calculated
      expectedrev[i]=expectedrev[i]+iter_revenue*dpois(demandlow,meanlow)*dpois(demandhigh,meanhigh)
    }
  }
}
RevenueFCFS=expectedrev[1]
print(paste("Lower Bound for Expected Revenue in FCFS setting is:", round(RevenueFCFS,1)))

#................................................................#
#1.a) ii)


meanlow=50          # Mean Demand for Low-Fare, Poisson
meanhigh=20           # Mean Demand for High-Fare, Poisson
pricelow=1.00           # Price for Low-Fare
pricehigh=1.50          # Price for Low-Fare
capacity=50    # Capacity 
upperbound_exprev=0 #creation of expected revenue upper bound variable 
for(demandlow in 0:200){
  for(demandhigh in 0:200){
    highfare_sold=min(demandhigh,capacity) #selection of value from demand for high fare and capacity
    lowfare_remain=capacity-highfare_sold #calculation of remaining commodities for low fare
    #selection of values from remaining commodities of low fare and mean demand for low fare
    lowfare_sold=min(demandlow,lowfare_remain)
    iter_revenue=pricelow*lowfare_sold+pricehigh*highfare_sold #iteration's revenue calculation
    #upper bound expected revenue calculation 
    upperbound_exprev=upperbound_exprev+iter_revenue*dpois(demandlow,meanlow)*dpois(demandhigh,meanhigh)
  }
}
print(paste("Upper Bound for Expected Revenue (Perfect Foresight) in FCFS setting is:", round(upperbound_exprev,1)))

###################################################################
#1.b)

meanlow=50          # Mean Demand for Low-Fare, Poisson
meanhigh=20           # Mean Demand for High-Fare, Poisson
pricelow=1.00           # Price for Low-Fare
pricehigh=1.50          # Price for Low-Fare
capacity=50    # Capacity 
exprev=rep(0,capacity+1) #initialization of set of values for expected revenue (51 in number)
for (i in 1:(capacity+1)){
  protect=i-1 #initialization of protection variable
  lowfare_avail=capacity-protect; #available low-fare calculated 
  exprev[i]=0;
  for(demandlow in 0:200){
    lowfare_sold=min(lowfare_avail,demandlow) #minimum value of the available low fare & mean low demand is calculated
    highfare_remain=capacity-lowfare_sold #remaining high fare 
    for(demandhigh in 0:200){
      highfare_sold=min(highfare_remain,demandhigh) #minimum value of the remaining high fare & mean high demand  is calculated
      iter_revenue=pricelow*lowfare_sold+pricehigh*highfare_sold #revenue for the iteration is calculated
      exprev[i]=exprev[i]+
        iter_revenue*dpois(demandlow,meanlow)*dpois(demandhigh,meanhigh) #expected revenue for the iteration
    }
  }
}
protect_index = which(exprev == max(exprev)) #conditional selection to retrieve index of the value which satisfies the condition 
protect_best=protect_index-1 #since indexing in R begins from 1
exprev_optim=max(exprev)
print(paste("The Optimal Protection Level for High-Fare Demand:", protect_best))

booking_limit=capacity-protect_best #calculation of booking-limit 
print(booking_limit)


#visualization
xaxis=0:capacity
plot(xaxis, col = rep(1:3, each = 10), exprev,pch = 10, cex = 0.5,las=1, xaxt="n",
     xlab="Protected value",ylab="Expected Revenue (in £)",cex.lab=1, cex.axis=1, cex.main=1.5)
xticks <- seq(0, capacity, by=50)
axis(side = 1, at = xticks)
axis(side = 1, at = protect_best)
lines(c(protect_best,protect_best),c(0, max(exprev)),lty=7)
axis(side = 2, at = round(max(exprev),2),las=1)
lines(c(0,protect_best),c(max(exprev), max(exprev)),lty=7)
###############################################################################
#1.d)

meanlow=15          # Mean Demand for Low-Fare, Poisson
meanhigh=35           # Mean Demand for High-Fare, Poisson
pricelow=1.00           # Price for Low-Fare
pricehigh=1.50          # Price for Low-Fare
capacity=50    # Capacity 
exprev=rep(0,capacity+1) #initialization of set of values for expected revenue (51 in number)
for (i in 1:(capacity+1)){
  protect=i-1 #initialization of protection variable
  lowfare_avail=capacity-protect; #available low-fare calculated 
  exprev[i]=0;
  for(demandlow in 0:200){
    lowfare_sold=min(lowfare_avail,demandlow) #minimum value of the available low fare & mean low demand is calculated
    highfare_remain=capacity-lowfare_sold #remaining high fare 
    for(demandhigh in 0:200){
      highfare_sold=min(highfare_remain,demandhigh) #minimum value of the remaining high fare & mean high demand  is calculated
      iter_revenue=pricelow*lowfare_sold+pricehigh*highfare_sold #revenue for the iteration is calculated
      exprev[i]=exprev[i]+
        iter_revenue*dpois(demandlow,meanlow)*dpois(demandhigh,meanhigh) #expected revenue for the iteration
    }
  }
}
protect_index = which(exprev == max(exprev)) #conditional selection to retrieve index of the value which satisfies the condition 
protect_best=protect_index-1 #since indexing in R begins from 1
exprev_optim=max(exprev)
print(paste("The Optimal Protection Level for High-Fare Demand:", protect_best))

booking_limit=capacity-protect_best #calculation of booking-limit 
print(booking_limit)


#visualization
xaxis=0:capacity
plot(xaxis, col = rep(1:3, each = 10), exprev,pch = 10, cex = 0.5,las=1, xaxt="n",
     xlab="Protected value",ylab="Expected Revenue (in £)",cex.lab=1, cex.axis=1, cex.main=1.5)
xticks <- seq(0, capacity, by=50)
axis(side = 1, at = xticks)
axis(side = 1, at = protect_best)
lines(c(protect_best,protect_best),c(0, max(exprev)),lty=7)
axis(side = 2, at = round(max(exprev),2),las=1)
lines(c(0,protect_best),c(max(exprev), max(exprev)),lty=7)
###################################################################
#1.c)

print(paste("The Expected Daily Revenue In The New Method Is:", max(exprev)))
x = max(exprev)
y = mean(c(RevenueFCFS,upperbound_exprev))
print(paste("Percentage change is:", ((x-y)/x)*100))
######################################################################



######################################################################
######################################################################
######################################################################
########################## Question 2 ################################
######################################################################
######################################################################

# Question 2 (multiple resources with mixed arrivals) 2 resouces 6 products

# set initial variables
price = c(150,100,120,80,250,150) # price of each product -> c(product1, product2, ..., product6)
demand = c(30,60,30,60,30,60) # demand of each product -> c(product1, product2, ..., product6)
tt = 300 # length of time horizon
c1 = 100 # a capacity for day1
c2 = 100 # a capacity for day2
received_prob = c(1/10, 1/5, 1/10, 1/5, 1/10, 1/5) # probabilities of arrival of each product -> c(product1, product2, ..., product6)

################# a.) #################
## preparation before applying the Dynamic programming
# create an emthy array of value function v(x1,x2,tt) to be later populated
# the vale function is the maximum expected reward, where 
# x1 is the remaining capacity of day1 
# x2 is the remaining capacity of day2
# tt is the time period
v=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# to keep track of optimal decisions, empty arrays are created
# acceptance decision for a product 1 arrival: accept1(x1,x2,t):
accept1=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1)) 
# acceptance decision for a product 2 arrival: accept2(x1,x2,t):
accept2=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 3 arrival: accept3(x1,x2,t):
accept3=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 4 arrival: accept4(x1,x2,t):
accept4=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 5 arrival: accept5(x1,x2,t):
accept5=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 6 arrival: accept6(x1,x2,t):
accept6=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# calculate the probability of no arrival 
sum_received_prob = sum(received_prob)
non_received_prob = 1 - sum_received_prob # probability when we do not receive any requests

# initialization of terminal values -> v(x1,x2,0) = 0 for all x1 and x2
for(i in 1:(c1+1)){
  for(j in 1:(c2+1)){
    v[i,j,1]=0; # all unsold tickets are worthless at the end of horizon, i.e., t=0 (index = 1)
  }
}

# start the clock!
ptm <- proc.time()
# populate the Dynamic programing 
for(t in 2:(tt+1)){ #2:tt+1 (start with 2 because as t = 1, we assume that all unsold seats are worthless)
  for(i in 1:(c1+1)){ #1:c1+1
    for(j in 1:(c2+1)){ #1:c2+1
      
      # for no arrivals:
      vforarrival0=v[i,j,t-1];
      
      # for product 1 arrival:
      # default not accept unless profitable to accept
      vforarrival1=v[i,j,t-1];
      accept1[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival1=max(price[1]+v[i-1,j,t-1],v[i,j,t-1]);
        # record the decision in the accept1 variable:
        if(price[1]+v[i-1,j,t-1]>v[i,j,t-1]){
          accept1[i,j,t]=1;
        }
      }
      
      # for product 2 arrival:
      # default not accept unless profitable to accept
      vforarrival2=v[i,j,t-1];
      accept2[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival2=max(price[2]+v[i -1,j,t-1],v[i,j,t-1]);
        # record the decision in the accept2 variable:
        if(price[2]+v[i - 1,j,t-1]>v[i,j,t-1]){
          accept2[i,j,t]=1;
        }
      }
      
      # For product 3 arrival:
      # default not accept unless profitable to accept
      vforarrival3=v[i,j,t-1];
      accept3[i,j,t]=0;
      # If resources available:
      if(j>1){
        vforarrival3=max(price[3]+v[i,j-1,t-1],v[i,j,t-1]);
        # record the decision in the accept3 variable:
        if(price[3]+v[i,j-1,t-1]>v[i,j,t-1]){
          accept3[i,j,t]=1;
        }
        
      }
      
      # for product 4 arrival:
      # default not accept unless profitable to accept
      vforarrival4=v[i,j,t-1];
      accept4[i,j,t]=0;
      # If resources available:
      if(j>1){
        vforarrival4=max(price[4]+v[i,j-1,t-1],v[i,j,t-1]);
        # record the decision in the accept4 variable:
        if(price[4]+v[i,j-1,t-1]>v[i,j,t-1]){
          accept4[i,j,t]=1;
        }
      }
      
      # for product 5 arrival:
      # default not accept unless profitable to accept
      vforarrival5=v[i,j,t-1];
      accept5[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival5=max(price[5]+v[i-1,j-1,t-1],v[i,j,t-1]);
          # record the decision in the accept3 variable:
          if(price[5]+v[i-1,j-1,t-1]>v[i,j,t-1]){
            accept5[i,j,t]=1;
          }
        }
      }
      # for product 6 arrival:
      # default not accept unless profitable to accept
      vforarrival6=v[i,j,t-1];
      accept6[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival6=max(price[6]+v[i-1,j-1,t-1],v[i,j,t-1]);
          # record the decision in the accept3 variable:
          if(price[6]+v[i-1,j-1,t-1]>v[i,j,t-1]){
            accept6[i,j,t]=1;
          }
        }
      }
      
      # calculate the overall value function:
      v[i,j,t]=non_received_prob*vforarrival0+
        received_prob[1]*vforarrival1+
        received_prob[2]*vforarrival2+
        received_prob[3]*vforarrival3+
        received_prob[4]*vforarrival4+
        received_prob[5]*vforarrival5+
        received_prob[6]*vforarrival6;
    }
  }
}
# stop the clock
proc.time() - ptm # ~ 13.4 seconds
max(v) # 22679.83 

# visualization
## first product at tt = 101
acceptance<-accept1[2:101,2:101,101];
xaxis<-1:c1
yaxis<-1:c2
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'HF day1, 100 periods remaining')
## first product at tt = 201
acceptance<-accept1[2:101,2:101,201];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'HF day1, 200 periods remaining') 
## second product at tt = 101
acceptance<-accept2[2:101,2:101,101];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'LF day1, 100 periods remaining')
## second product at tt = 201
acceptance<-accept2[2:101,2:101,201];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'LF day1, 200 periods remaining')
## thrid product at tt = 101
acceptance<-accept3[2:101,2:101,101];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'HF day2, 100 periods remaining') 
## thrid product at tt = 201
acceptance<-accept3[2:101,2:101,201];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'HF day2, 200 periods remaining') 
## forth product at tt = 101
acceptance<-accept4[2:101,2:101,101] 
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'LF day2, 100 periods remaining ')
## forth product at tt = 201
acceptance<-accept4[2:101,2:101,201] 
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'LF day2, 200 periods remaining')
## fifth product at tt = 101
acceptance<-accept5[2:101,2:101,101];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity",main = 'HF day1&2, 100 periods remaining')  
## fifth product at tt = 201
acceptance<-accept5[2:101,2:101,201];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'HF day1&2, 200 periods remaining')  
## sixth product at tt = 101
acceptance<-accept6[2:101,2:101,101];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'LF day1&2, 100 periods remaining')  
## sixth product at tt = 201
acceptance<-accept6[2:101,2:101,201];
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               zlim = range(acceptance, finite = TRUE),
               key.axes = axis(3, seq(0, 1, by = 1)),
               nlevels = 2,color.palette = rainbow,
               xlab="Remaining Day 1 Capacity",
               ylab="Remaining Day 2 Capacity", main = 'LF day1&2, 200 periods remaining')  

################# b.) #################
# FCFS
## preparation before applying the Dynamic programming
# create an emthy array of value function v(x1,x2,tt) to be later populated
# the vale function is the maximum expected reward, where 
# x1 is the remaining capacity of day1 
# x2 is the remaining capacity of day2
# tt is the time period

v=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# to keep track of optimal decisions, empty arrays are created
# acceptance decision for a product 1 arrival: accept1(x1,x2,t):
accept1=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1)) 
# acceptance decision for a product 2 arrival: accept2(x1,x2,t):
accept2=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 3 arrival: accept3(x1,x2,t):
accept3=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 4 arrival: accept4(x1,x2,t):
accept4=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 5 arrival: accept5(x1,x2,t):
accept5=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 6 arrival: accept6(x1,x2,t):
accept6=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# get the probability when we do not receive any requests
sum_received_prob = sum(received_prob)
non_received_prob = 1 - sum_received_prob 

# initialization of terminal values -> v(x1,x2,0) = 0 for all x1 and x2
for(i in 1:(c1+1)){
  for(j in 1:(c2+1)){
    v[i,j,1]=0; # all unsold tickets are worthless at the end of horizon, i.e., t=0 (index = 1)
  }
}

# start the clock!
ptm <- proc.time()
# populate the Dynamic programing 
for(t in 2:(tt+1)){ #2:tt+1 (start with 2 because as t = 1, we assume that all unsold seats are worthless)
  for(i in 1:(c1+1)){ #1:c1+1
    for(j in 1:(c2+1)){ #1:c2+1
      
      # for no arrivals:
      vforarrival0=v[i,j,t-1];
      
      # for product 1 arrival:
      # default not accept unless able to accept
      vforarrival1=v[i,j,t-1];
      accept1[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival1=price[1]+v[i-1,j,t-1];
        # record the decision in the accept1 variable:
        accept1[i,j,t]=1;
      }
      
      
      # for product 2 arrival:
      # default not accept unless able to accept
      vforarrival2=v[i,j,t-1];
      accept2[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival2=price[2]+v[i -1,j,t-1]
        # record the decision in the accept2 variable:
        accept2[i,j,t]=1;
        
      }
      
      # for product 3 arrival:
      # default not accept unless able to accept
      vforarrival3=v[i,j,t-1];
      accept3[i,j,t]=0;
      # If resources available:
      
      if(j>1){
        vforarrival3=price[3]+v[i,j-1,t-1]
        # record the decision in the accept3 variable:
        accept3[i,j,t]=1;
      }
      
      
      
      # for product 4 arrival:
      # default not accept unless able to accept
      vforarrival4=v[i,j,t-1];
      accept4[i,j,t]=0;
      # If resources available:
      
      if(j>1){
        vforarrival4=price[4]+v[i,j-1,t-1]
        # record the decision in the accept4 variable:
        accept4[i,j,t]=1;
      }
      
      
      # for product 5 arrival:
      # default not accept unless able to accept
      vforarrival5=v[i,j,t-1];
      accept5[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival5=price[5]+v[i-1,j-1,t-1]
          # record the decision in the accept5 variable:
          accept5[i,j,t]=1;
        }
        
      }
      # for product 6 arrival:
      # default not accept unless able to accept
      vforarrival6=v[i,j,t-1];
      accept6[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival6=price[6]+v[i-1,j-1,t-1]
          # record the decision in the accept6 variable:
          accept6[i,j,t]=1;
        }
        
      }
      
      # get the overall value function:
      v[i,j,t]=non_received_prob*vforarrival0+
        received_prob[1]*vforarrival1+
        received_prob[2]*vforarrival2+
        received_prob[3]*vforarrival3+
        received_prob[4]*vforarrival4+
        received_prob[5]*vforarrival5+
        received_prob[6]*vforarrival6;
    }
  }
}
# stop the clock!
proc.time() - ptm # ~ 4.7 seconds
max(v) # 19719.77

################# c.) #################
# load important library
library(lpSolve)

# populate objective function
# revenues obtained from selling each product
obj.fun = c(150,100,120,80,250,150) # obj: maximize the objective function -> c(product1, product2, ..., product6)

# given expected demand
expdemands = c(30,60,30,60,30,60) # -> c(product1, product2, ..., product6)

# populate constraints
## total allocation for day1
AllocateLessThanCapacityDay1 = c(1,1,0,0,1,1)
## total allocation for day2
AllocateLessThanCapacityDay2 = c(0,0,1,1,1,1)
## demand limits
AllocateLessThanDemand<-diag(1, 6, 6)

# combine the left hand side constraints
constr <- rbind(AllocateLessThanCapacityDay1,AllocateLessThanCapacityDay2
                ,AllocateLessThanDemand)

# set the constraint directions
constr.dir <- c(rep("<=", 8))
# set the constraint right hand side
rhs <- c(100,100,expdemands)
# calculate the optimal allocation
optairline <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
optairline$solution   
# get the optimal revenue
revenueLP<-optairline$objval
print(revenueLP) # 22800 a little more than DP with randomness, 
# cause we allow demands to be deterministic in this case

# get the bid Prices for capacity constraints
bidprices<-optairline$duals[1:2]

# The bid price of the first day
print(paste("The Bid Price for the first day ticket is:",bidprices[1])) # day1
# The bid price of the second day
print(paste("The Bid Price for the second day ticket is:",bidprices[2])) # day2

# preparation before applying the FCFS based on the obtained bid prices
# create an emthy array of value function v(x1,x2,tt) to be later populated
# the vale function is the maximum expected reward, where 
# x1 is the remaining capacity of day1 
# x2 is the remaining capacity of day2
# tt is the time period
v=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# to keep track of optimal decisions, empty arrays are created
# acceptance decision for a product 1 arrival: accept1(x1,x2,t):
accept1=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1)) 
# acceptance decision for a product 2 arrival: accept2(x1,x2,t):
accept2=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 3 arrival: accept3(x1,x2,t):
accept3=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 4 arrival: accept4(x1,x2,t):
accept4=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 5 arrival: accept5(x1,x2,t):
accept5=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 6 arrival: accept6(x1,x2,t):
accept6=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# initialization of terminal values -> v(x1,x2,0) = 0 for all x1 and x2
for(i in 1:(c1+1)){
  for(j in 1:(c2+1)){
    v[i,j,1]=0; # all unsold tickets worthless at the end of horizon, i.e., t=0 (index = 1). }
  }
}

# start the clock!
ptm <- proc.time()
# initiate the dynamic programming algorithm 
for(t in 2:(tt+1)){ #2:tt+1 (start with 2 because as t = 1, we assume that all unsold tickets are worthless)
  for(i in 1:(c1+1)){ #1:c1+1
    for(j in 1:(c2+1)){ #1:c2+1
      
      # for no arrivals:
      vforarrival0=v[i,j,t-1];
      
      # for product 1 arrival:
      # default not accept unless able to accept
      vforarrival1=v[i,j,t-1];
      accept1[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival1=price[1]+v[i-1,j,t-1];
        accept1[i,j,t]=1; # record the decision
      }
      
      
      # for product 2 arrival:
      # default not accept unless able to accept
      vforarrival2=v[i,j,t-1];
      accept2[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival2=price[2]+v[i -1,j,t-1]
        accept2[i,j,t]=1; # record the decision
        
      }
      
      # for product 3 arrival:
      # default not accept unless able to accept
      vforarrival3=v[i,j,t-1];
      accept3[i,j,t]=0; # record the decision
      # If resources available:
      if(j>1){
        vforarrival3=price[3]+v[i,j-1,t-1]
        accept3[i,j,t]=1; # record the decision
      }
      
      
      
      # for product 4 arrival:
      # default not accept unless able to accept
      vforarrival4=v[i,j,t-1];
      accept4[i,j,t]=0;
      # If resources available:
      if(j>1){
        vforarrival4=price[4]+v[i,j-1,t-1]
        accept4[i,j,t]=1; # record the decision
      }
      
      
      # for product 5 arrival:
      # default not accept unless able to accept
      vforarrival5=v[i,j,t-1];
      accept5[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival5=price[5]+v[i-1,j-1,t-1]
          accept5[i,j,t]=1;# record the decision
        }
        
      }
      # for product 6 arrival:
      # do not accept as the ticket price is less than 
      # that of the combined bid price (see accompanying document for more details)
      vforarrival6=v[i,j,t-1];
      accept6[i,j,t]=0;
      
      
      # get the overall value function:
      v[i,j,t]=non_received_prob*vforarrival0+
        received_prob[1]*vforarrival1+
        received_prob[2]*vforarrival2+
        received_prob[3]*vforarrival3+
        received_prob[4]*vforarrival4+
        received_prob[5]*vforarrival5+
        received_prob[6]*vforarrival6;
    }
  }
}
# stop the clock!
proc.time() - ptm # ~ 4.518 seconds
max(v) # 21915.6


################# d.) #################
# FCFS for HF classes, dynamic for LF classes, ignore the LF day1&day2 
# based on the information obtained from previous analysis (price of LF day1&day2 < bid price day1 + bid price day2)
# set initial variables
price = c(150,100,120,80,250,150) #-> c(product1, product2, ..., product6)
demand = c(30,60,30,60,30,60)  # -> c(product1, product2, ..., product6)
tt = 300 # length of time horizon
c1 = 100 # capacity for day1
c2 = 100 # capacity for day2
received_prob = c(1/10, 1/5, 1/10, 1/5, 1/10, 1/5) # -> c(product1, product2, ..., product6)

## preparation before applying the Dynamic programming
# create an emthy array of value function v(x1,x2,tt) to be later populated
# the vale function is the maximum expected reward, where 
# x1 is the remaining capacity of day1 
# x2 is the remaining capacity of day2
# tt is the time period
v=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

# to keep track of optimal decisions, empty arrays are created
# acceptance decision for a product 1 arrival: accept1(x1,x2,t):
accept1=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1)) 
# acceptance decision for a product 2 arrival: accept2(x1,x2,t):
accept2=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 3 arrival: accept3(x1,x2,t):
accept3=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 4 arrival: accept4(x1,x2,t):
accept4=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 5 arrival: accept5(x1,x2,t):
accept5=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))
# acceptance decision for a product 6 arrival: accept6(x1,x2,t):
accept6=array(rep( 0, len=(c1+1)*(c2+1)*(tt+1)), dim=c(c1+1,c2+1,tt+1))

sum_received_prob = sum(received_prob)
non_received_prob = 1 - sum_received_prob # probability when we do not receive any requests

# initialization of terminal values -> v(x1,x2,0) = 0 for all x1 and x2
for(i in 1:(c1+1)){
  for(j in 1:(c2+1)){
    v[i,j,1]=0; # all unsold tickets are worthless at the end of horizon, i.e., t=0 (index = 1). 
  }
}

# start the clock!
ptm <- proc.time()
# populate the dynamic programing 
for(t in 2:(tt+1)){ #2:tt+1 (start with 2 because as t = 1, we assume that all unsold tickets are worthless)
  for(i in 1:(c1+1)){ #1:c1+1
    for(j in 1:(c2+1)){ #1:c2+1
      
      # for no arrivals:
      vforarrival0=v[i,j,t-1];
      
      # for product 1 arrival:
      # default not accept unless able to accept
      vforarrival1=v[i,j,t-1];
      accept1[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival1=price[1]+v[i-1,j,t-1];
        accept1[i,j,t]=1; # record the decision
      }
      
      # for product 2 arrival:
      # default not accept unless profitable to accept
      vforarrival2=v[i,j,t-1];
      accept2[i,j,t]=0;
      # If resource available:
      if(i>1){
        vforarrival2=max(price[2]+v[i -1,j,t-1],v[i,j,t-1]);
        # evaluate whether to accept the request
        if(price[2]+v[i - 1,j,t-1]>v[i,j,t-1]){
          accept2[i,j,t]=1; # record the decision
        }
      }
      
      # For product 3 arrival:
      # default not accept unless able to accept
      vforarrival3=v[i,j,t-1];
      accept3[i,j,t]=0;
      # If resources available:
      if(j>1){
        vforarrival3=price[3]+v[i,j-1,t-1]
        accept3[i,j,t]=1; # record the decision
      }
      
      # For product 4 arrival:
      # default not accept unless profitable to accept
      vforarrival4=v[i,j,t-1];
      accept4[i,j,t]=0; 
      # If resources available:
      
      if(j>1){
        vforarrival4=max(price[4]+v[i,j-1,t-1],v[i,j,t-1]);
        # evaluate whether to accept the request
        if(price[4]+v[i,j-1,t-1]>v[i,j,t-1]){
          accept4[i,j,t]=1; # record the decision
        }
      }
      
      
      # for product 5 arrival:
      # default not accept unless able to accept
      vforarrival5=v[i,j,t-1];
      accept5[i,j,t]=0;
      # If resources available:
      if(i>1){
        if(j>1){
          vforarrival5=price[5]+v[i-1,j-1,t-1]
          accept5[i,j,t]=1; # record the decision
        }
        
      }
      # for product 6 arrival:
      # do not accept as the ticket price is less than
      # that of the combined bid price (see accompanying document for more details)
      vforarrival6=v[i,j,t-1];
      accept6[i,j,t]=0;
      
      # get the overall value function:
      v[i,j,t]=non_received_prob*vforarrival0+
        received_prob[1]*vforarrival1+
        received_prob[2]*vforarrival2+
        received_prob[3]*vforarrival3+
        received_prob[4]*vforarrival4+
        received_prob[5]*vforarrival5+
        received_prob[6]*vforarrival6;
    }
  }
}
# stop the clock!
proc.time() - ptm # ~ 7.4 seconds
max(v) # 22664.31

