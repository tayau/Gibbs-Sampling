#*****************************************************
#                                                    
#  Trevor Yau                 
#  
#
#  Last Revision: 04/02/17
#                                                    
#*****************************************************

#########################################################################################################

rm(list=ls())

### Part B: Coding a Gibbs Sampler to estimate posterior mean and posterior std deviation for theta1 and theta2
n1<-1000
n2<-500
x1<-200
x2<-110
m=10100
burn=100

#Creating containers for theta1 and theta2
theta1.cont <- rep(0, times=m)
theta2.cont <- rep(0, times=m)

#Initializing theta 1 first
theta.0<-x2/n2

#Generates a new theta2 from the given theta1
get.theta.2<-function(theta){
  theta2 = rpois(1, theta)
  if(theta2<0.1){
    theta2<-0.1
  }else{theta2 = theta2
  }
  #creating a while loop so that it meets our conditions that that 0.1<theta1<theta2
  while(theta2<theta){
    theta2 = rpois(1, theta)
  }
  return(theta2)
}

#Generates a new theta1 for the given theta2
get.theta.1<-function(theta){
  theta1 = rpois(1, theta)
  while(theta<theta1){
    theta1 = rpois(1, theta)
  }
  if(theta1<0.1){
    theta1<-0.1
  }else{theta1 = theta1
    
  }
  
  #creating a while loop so that it meets our conditions that that 0.1<theta1<theta2
  return(theta1)
}

#generating our initial values for theta1 and theta2
theta1.cont[1] = get.theta.1(theta.0)
theta2.cont[1] = get.theta.2(theta1.cont[1])

#running are gibbs sampling algorithm
for(i in 2:m){
  theta1.cont[i] = get.theta.1(theta2.cont[i-1])
  theta2.cont[i] = get.theta.2(theta1.cont[i])
}

#Remove the first 100 burn in observations
new.start = 1+burn
theta1.new = theta1.cont[new.start:m]
theta2.new = theta2.cont[new.start:m]
m.new = m - burn

prop=10
#Keep only 1 in every "prop" observations to simulate independence
m.newer = m.new/prop
theta1.newer = rep(0, times=m.new)
theta2.newer = rep(0, times=m.new)
for(i in 1:m.newer){
  j = i*prop
  theta1.newer[i] = theta1.new[j]
  theta2.newer[i] = theta2.new[j]
}



#posterior mean and std dev for theta2 are equal for Poisson
sum(theta1.newer)/length(theta1.newer)
#posterior mean and std dev for theta1 are equal for Poisson
sum(theta2.newer)/length(theta2.newer)

