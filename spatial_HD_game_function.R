#Single shot spatially-explicit HD game

##highly highly recommend sticking all of this code into chatgpt 

#make sure R is clear of old variables
rm(list=ls())

#set the game in a function to pass parameters into the function
#so i think this lets you run the spatial_HD_game code but in a function
#just so you can change the variables more easily 

hd_sp_game=function(nd,c,rmn, threshold){
  
#set grids (of dimenson n by n) for the strategies, the resource distribution, 
#payoffs and hawk neighbourhood
#so if nd is 4, the grid is 4x4


grid=array(0,dim=c(nd,nd))
resource=array(0,dim=c(nd,nd))
payoff=array(0,dim=c(nd,nd))
htot=array(0,dim=c(nd,nd))

#use set.seed to fix random number generator
set.seed(100)

#set cost of contest
##Except we don't because you get it from function


#initialize the grid randomly with resource (with a mean of rmn)
#using a poisson distribution to populate the array with random values,
#the values have a mean of rmn so you can change it in the function


for(i in 1:nd){
    for(j in 1:nd){
      resource[i,j]=rpois(1,rmn)
    }
}

#initialize the grid randomly with hawks using a uniform random number 
#generator threshold and a conditional statement

#Each cell in the grid is assigned a Hawk (1.0) or Dove (0.0) 
#depending on whether a random number exceeds the threshold 
#(determined by runif(1, 0, 1)) - a higher threshold results in fewer
#hawks as it increases the likelihood of a dove

for(i in 1:nd){
  for(j in 1:nd){
    if(runif(1,0,1)>threshold) grid[i,j]=1.0 else grid[i,j]=0.0
  }
}

#set rules using conditional statements

for(i in 1:nd){
  for(j in 1:nd){
    
    # boundary conditions are periodic 
    
    am=0
    ap=0
    
    bm=0
    bp=0
    
    if(i-1==0) am=nd else am=i-1
    if(i+1>nd) ap=1 else ap=i+1
    
    if(j-1==0) bm=nd else bm=j-1
    if(j+1>nd) bp=1 else bp=j+1
    
     tmp=0.0
     if(grid[ap,j]==1) tmp=tmp+1.0
     if(grid[i,bp]==1) tmp=tmp+1.0
     if(grid[am,j]==1) tmp=tmp+1.0
     if(grid[i,bm]==1) tmp=tmp+1.0
     htot[i,j]=tmp
  }
}

#for each cell evaluate neighbourhood and return payoff

for(i in 1:nd){
  for(j in 1:nd){
    
    # boundary conditions are periodic 
    am=0
    ap=0
    
    bm=0
    bp=0
    
    if(i-1==0) am=nd else am=i-1
    if(i+1>nd) ap=1 else ap=i+1
    
    if(j-1==0) bm=nd else bm=j-1
    if(j+1>nd) bp=1 else bp=j+1
    
  
    #rules of the spatial game as conditional statements for a focal cell grid[i,j]
    
    if(grid[i,j]==0){# for a dove
      
      if(grid[ap,j]==0 && grid[am,j]==0 && grid[i,bp]==0 && grid[i,bm]==0) payoff[i,j]=resource[i,j]/5 #all neighbours are doves
      if(grid[ap,j]==1 || grid[am,j]==1 || grid[i,bp]==1 || grid[i,bm]==1) payoff[i,j]=0.0} #at least one neighbour is a hawk
    
    else{ #for a hawk
      
     if(grid[ap,j]==0 && grid[am,j]==0 && grid[i,bp]==0 && grid[i,bm]==0) payoff[i,j]=resource[i,j] #all neighbours are doves
     if(grid[ap,j]==1 && grid[am,j]==1 && grid[i,bp]==1 && grid[i,bm]==1) payoff[i,j]=c+resource[i,j]/5 #all neighbours are hawks
     if(grid[ap,j]==1 || grid[am,j]==1 || grid[i,bp]==1 || grid[i,bm]==1) payoff[i,j]=c+resource[i,j]/(htot[i,j]+1)} #some neighbours are hawks
     
     
  }
}

#function has to return a fitness value
#measures of fitness

#total fitness 
sum(payoff)

#average fitness 
#mean(payoff)

#variance in fitness 
#vp=sum((payoff-mean(payoff))^2/(nd*nd-1))
#vp

#coefficient of variation in fitness
#mean(payoff)/sqrt(vp)
}


##basically what this bit is doing is running the function 10 times with 
##different values for the threshold - this means it's changing the proportion
##of hawks and doves in the population every time 

##higher threshold means fewer hawks - this shows the relationship between
##the threshold and the overall fitness payoff in the population 
ff=array(0,dim=c(10))
threshold=seq(0,1,length=10)

for(k in 1:10)
ff[k]=hd_sp_game(4,25,50,threshold[k])

plot(threshold,ff)

