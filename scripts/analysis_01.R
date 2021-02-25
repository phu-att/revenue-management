# Question 3
### Method 1 ###
# initiate values to be used in the following analyses
### P1 < P2; mP1 > mP2
mP1 = 70 # mean demand for product 1, Poisson
mP2 = 30 # mean demand for product 2, Poisson
price = c(690, 800) # price for product 1 and product 2
pP1 = 690 # price for product 1
pP2 = 800 # price for product 2
capacity = 60 # capacity 
################ identify rooms to reserve for product2 (P2) ################ 
ExpRevenue=rep(0,capacity+1)
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200) # product 1, loop over 0 to 200, as to ascertain the ranges 
                   # of Poison probability given the mean the but can adjust the ranges 
    {
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200) # product 2, loop over 0 to 200, as to ascertain the ranges 
                     # of Poison probability given the mean the but can adjust the ranges 
      {
      soldHighFare=min(remainforHighFare,dH)
      RevenueThisIter=pP1*soldLowFare+pP2*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
      RevenueThisIter*dpois(dL,mP1)*dpois(dH,mP2)
    }
  }
}

# identify the optimal protection level
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
print(paste("The Optimal Protection Level for Product 2 Demand:", ProtectBest))
# identify the obtained revenue
OptimalExpRevenue=max(ExpRevenue) # 43772.45
print(paste("The Corresponding Optimal Revenue:", OptimalExpRevenue))

# plot expected revenue vs protection level
xaxis=0:capacity
plot(xaxis,ExpRevenue/1000,pch = 16, cex = 0.5,las=1, xaxt="n",
     xlab="Guestrooms Protected",ylab="Expected Revenue (in £ thousands)",cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
xticks <- seq(0, capacity, by=50)
axis(side = 1, at = xticks) 
axis(side = 1, at = ProtectBest) 
lines(c(ProtectBest,ProtectBest),c(0, max(ExpRevenue)/1000),lty=2)
axis(side = 2, at = round(max(ExpRevenue)/1000,2),las=1)
lines(c(0,ProtectBest),c(max(ExpRevenue)/1000, max(ExpRevenue)/1000),lty=2)

# identify marginal gain vs marginal loss
critFrac = (800 - 690)/800
protect<-qpois(critFrac, mP2)
ExpGain=rep(0,capacity+1)
ExpLoss=rep(0,capacity+1)
for (i in 0:(capacity+1)){
  protect=i-1
  ExpGain[i]=(1-ppois(protect,mP2))*(pP2-pP1)
  ExpLoss[i]=ppois(protect,mP2)*pP1
}
xaxis=0:capacity
plot(xaxis,ExpGain,type="p",cex=0.5,col="blue",xaxt="n",
     xlab = "Protection Level", ylab = "Marginal Gain (Blue) and Loss (Red)", ylim = c(0, 200))
lines(xaxis,ExpLoss,type="p",cex=0.5,col="red")
xtick<-seq(0, 60, by=5)
axis(side=1, at=xtick)

################  Optimal Admission Decisions for 2 Fare Classes with Sequential Arrivals ################
### Method 2 ###
# initiate values to be used in the following analyses
j = 2 # number of products
expd<-c(30,70);   # expected demand for product 2 and product 1 (Poisson)
price = c(800, 690) # price for product 2 and product 1
capacity = 60 # capacity 
# v[j,x] is the optimal total expected revenue from
# j fare classes with remaining capacity just before facing the demand for fare class j.
# j=1 is the end of horizon and assumed that all unsold rooms
# are worthless at this stage, j=2 is the last stage (product 2 arrivals)
# initiate an empty v matrix with 3 rows and 61 columns to store value functions later
v =matrix(0, nrow = (j+1), ncol = (capacity+1));  
# initiate an empty ybest matrix with 3 rows and 61 columns to store a number of reserved rooms
# for product 2 
ybest = matrix(0, nrow = j+1, ncol = (capacity+1));

# Dynamic Programming Recursion
for(i in 2:(j+1)) { # i=2 is stage 1 (i.e., product 2 arrivals), i=3 is stage 2 (i.e., product 1 arrivals)
  for(n in 1:(capacity+1))
    {
    x=n-1; # inventory level
    valuebest= -10000 # to be compared 
    for(y in (0:x)){ # protect for future stages
      avail=x-y; # available for this stage
      value=0; # to start computing the expected revenue
      for(d in (0:200)){
        sold=min(avail,d);
        value=value+
          dpois(d, expd[i-1])*(price[i-1]*sold+v[i-1,n-sold]);
      }
      if(value>valuebest){
        ybest[i,n]= y # to store optimal protected rooms
        valuebest=value
      }
    }
    v[i,n]=valuebest
  }
}

max(v) # 43772.45 same result as Method 1
max(ybest) # optimal protection -> 24 rooms
# visualize Value of an additional unit of capacity
xtick<-0:capacity
plot(xtick[2:(capacity+1)],v[3,2:(capacity+1)]-v[3,1:(capacity)],
     pch = 19, col = "blue", type = "p",
     xlab="Capacity", ylab="Marginal Value of Capacity at Stage 2")
# it indicates that the marginal revenue drops massively for any additional capacity
# when the capacity exceeds 24 as the demand is limited

################   Optimal Admission Decision for Two Fare Classes with Mixed Arrivals ################ 
### Method 3 ###
# initiate values to be used in the following analyses
tt = 200 # split time into 200 tiny periods
arrival_prob = c(0.25, 0.65, 0.1) # probabilities of arrivals (product 2, product 1, no arrival)
                                  # for a given period
price = c(800, 690) # price for product 2 and product 1
capacity = 60 # capacity

# create an empty array of value function v(x1,tt) to be later populated
# the vale function is the maximum expected reward, where 
# x1 is the remaining capacity  
# tt is the time period
v=matrix(rep( 0, len=(capacity+1)*(tt+1)), nrow=capacity+1);
# to keep track of optimal decisions, an empty array is created
# acceptance decision for a product 1 arrival: accept2(x1,x2,t):
accept2=matrix(rep( 0, len=(capacity+1)*(tt+1)), nrow=capacity+1); # decision for product 2

# Dynamic Programming Recursion
for(t in 2:(tt+1)){ #2:tt+1
  for(i in 1:(capacity+1)){ #1:capacity+1
    
    # for no arrivals:
    vtogo0=v[i,t-1];
    
    # for Product 2 arrival:
    vtogo1=v[i,t-1]; # default
    # If resource available:
    if(i>1){
      vtogo1=price[1]+v[i-1,t-1];
    }
    
    # for Product 1 arrival:
    vtogo2=v[i,t-1];
    accept2[i,t]=0;
    # If resource available:
    if(i>1){
      vtogo2=max(price[2]+v[i-1,t-1],v[i,t-1]);
      # Recording the decision in the accept2 variable:
      if(price[2]+v[i-1,t-1]>v[i,t-1]){
        accept2[i,t]=1;
      }
    }
    
    # compute the overall value function:
    v[i,t]=arrival_prob[3]*vtogo0+arrival_prob[1]*vtogo1+arrival_prob[2]*vtogo2;
  }
}
max(v) # 46845.32

# visualize the optimal policy structure
acceptance<-t(accept2[2:61,2:201]); # transpose of accept2 (horizontal:time)
xaxis<-1:tt
yaxis<-1:capacity
filled.contour(xaxis,yaxis,acceptance,xaxt="n",yaxt="n",
               key.axes = axis(4, seq(0, 1, by = 1)), nlevels = 2,
               color.palette = rainbow,
               xlab="Remaining Time", ylab="Remaining Number of Rooms") 
# this can be used to evaluate real time whether to accept product 1 at a particular 
# remaining capacity and time remaining

################  LP solution ################ 
# load necessary library
library(lpSolve)

# setting up the LP formulation:

# The problem has 5 decision variables:
# P1: allocations for Product 1 buyers
# P2: allocations for Product 2 buyers
# P3: allocations for Product 3 buyers
# P4: allocations for Product 4 buyers
# P5: allocations for Product 5 buyrts

# objective function foefficients -> to maximize
obj.fun <- c(690,800,80, 400, 450); # i.e., c(product1, product2, ..., product5)

# Constraint Coefficients
# first, consider the demand constraints.
# allocations for each type of reservation
# should be less than or equal to the anticipated demand.
# The coefficients for this constraint requires
# a diagonal matrix of size 5.
AllocateLessThanDemand<-diag(1, 5, 5)

# second, consider the maximum allocation constraint
# the hotel can allocate at most 60 rooms each day
# total allocation for Tuesday is P3 + P5
AllocateLessThanCapacityTU<-c(0,0,1,0,1)
# total allocation for Wed is P3 + P5 + P4
AllocateLessThanCapacityWED<-c(0,0,1,1,1)
# total allocation for TH is P2 + P5 + P4
AllocateLessThanCapacityTH<-c(0,1,0,1,1)
# total allocation for Friday is P1 + P2
AllocateLessThanCapacityFRI<-c(1,1,0,0,0)
# total allocation for SAT is P1 + P2
AllocateLessThanCapacitySAT<-c(1,1,0,0,0)
# total allocation for SUN is P1 + P2
AllocateLessThanCapacitySUN<-c(1,1,0,0,0)

# Bring all constraint coefficients together to form the
# constraint coefficient matrix
constr <- rbind(AllocateLessThanDemand,
                AllocateLessThanCapacityTU,AllocateLessThanCapacityWED,
                AllocateLessThanCapacityTH, AllocateLessThanCapacityFRI,
                AllocateLessThanCapacitySAT, AllocateLessThanCapacitySUN);

# set the constraint direction
constr.dir <- c(rep("<=", 5), rep("<=", 6));

# set the constraint right hand side (see accompanying document for more details)
rhs <- c(70, 30, 100, 50, 35,60,60,60,60,60,60 )

# solving the LP -> to maximize revenue
prod.sol <- lp("max", obj.fun, constr, constr.dir, rhs, compute.sens=TRUE)
# optimal solution (allocation) 
prod.sol$solution

# optimal objective function value
prod.sol$objval # optimal revenue 67150

# sensitivity analysis
## ranges of coefficents to retain the same optimal revenue
prod.sol$sens.coef.from
prod.sol$sens.coef.to 

# check the shadow prices of each resource
prod.sol$duals[1:length(constr.dir)]  # indicates that an demand increase of product5 by a unit
# could increase the revenue by 50
# check the ranges to which the shadow prices remain valid
prod.sol$duals.from[1:length(constr.dir)]
prod.sol$duals.to[1:length(constr.dir)]


