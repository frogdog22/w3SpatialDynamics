#Single shot spatially-explicit HD game

#make sure R is clear of old variables
rm(list=ls())

#set grids (of dimenson nd by nd) for the strategies, the resource distribution, payoffs and hawk neighbourhood

nd=4

grid=array(0,dim=c(nd,nd))
resource=array(0,dim=c(nd,nd))
payoff=array(0,dim=c(nd,nd))
htot=array(0,dim=c(nd,nd))

#use set.seed to fix random number generator
set.seed(100)

#set cost of contest
c=25

#initialize the grid randomly with resource (with a mean of rmn)

rmn=50

for(i in 1:nd){
    for(j in 1:nd){
      resource[i,j]=rpois(1,rmn)
    }
}

#initialize the grid randomly with hawks using a uniform random number generator threshold and a conditional statement

threshold=0.5

for(i in 1:nd){
  for(j in 1:nd){
    if(runif(1,0,1)>threshold) grid[i,j]=1.0 else grid[i,j]=0.0
  }
}

#set rules using conditional statements

for(i in 1:nd){
  for(j in 1:nd){
    
    # boundary conditions are periodic and wrap round
    
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
    
    # boundary conditions are periodic and wrap round
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
     if(grid[ap,j]==1 && grid[am,j]==1 && grid[i,bp]==1 && grid[i,bm]==1) payoff[i,j]=c-resource[i,j]/5 #all neighbours are hawks
     if(grid[ap,j]==1 || grid[am,j]==1 || grid[i,bp]==1 || grid[i,bm]==1) payoff[i,j]=c-resource[i,j]/(htot[i,j]+1)} #some neighbours are hawks
     
     
  }
}

grid
payoff

#measures of fitness


#total fitness 
sum(payoff)

#average fitness 
mean(payoff)

#variance in fitness 
vp=sum((payoff-mean(payoff))^2/(nd*nd-1))
vp

#coefficient of variation in fitness
mean(payoff)/sqrt(vp)

#fitness only for hawks
#tmp=0
#for(i in 1:nd){
#  for(j in 1:nd){
#    if(grid[i,j]==1) tmp=payoff[i,j]}}
#sum(tmp)