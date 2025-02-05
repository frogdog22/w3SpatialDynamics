#game of life

#Any live cell less than two live neighbours dies, as if through lack of support. 
#Any live cell with two or three live neighbours lives on to the next generation. 
#Any live cell with more than three live neighbours dies, as if by overpopulation. 
#Any dead cell with exactly three live neighbours becomes a live cell, 
#as if by reproduction.

setwd("~/desktop/")
quartz()

#determine grid size,generations, 
nd=100
gens=1500

#define mutation event time and probability of mutants
mgn=1
mutl=0.2


#define and initialize a nd by nd grid
res=array(0,dim=c(nd,nd))

#define a vector for time series
nsize=NULL

#res[5,9]=1
#res[6,8]=1
#res[4,7]=1
#res[5,7]=1
#res[6,7]=1

#res[9,9]=1
#res[10,8]=1
#res[8,7]=1
#res[9,7]=1
#res[10,7]=1

#res[24,25]=1
#res[23,26]=1
#res[22,24]=1
#res[22,25]=1
#res[22,26]=1

#res[25,29]=1
#res[26,28]=1
#res[24,27]=1
#res[25,27]=1
#res[26,27]=1

par(mfrow=c(1,1),cex.main=0.6)
heatmap(res, Rowv = NA, Colv = NA, scale = "none",col=c("lightblue","blue"),
        labRow = FALSE,labCol= FALSE)

t=1
while(t < gens){
tmp=array(0,dim=c(nd,nd))

for(i in 1:nd){
  for(j in 1:nd){
    
    am=0
    ap=0
    
    bm=0
    bp=0
    
    # determine number of live neighbours with periodic boundaries and 
    # Moore neighbourhood
    
    if(i-1==0) am=nd else am=i-1
    if(i+1>nd) ap=1 else ap=i+1
    
    if(j-1==0) bm=nd else bm=j-1
    if(j+1>nd) bp=1 else bp=j+1
    
    neigh_number=res[am,j]+res[ap,j]+res[i,bp]+res[i,bm]+res[am,bm]+res[ap,bm]+res[ap,bp]+res[am,bp]

    #conway's rules based on neighbours
    if(res[i,j]==1 && neigh_number<2) tmp[i,j]=0.0 
    if(res[i,j]==1 && neigh_number==2 || neigh_number==3) tmp[i,j]=1.0 
    if(res[i,j]==1 && neigh_number>3) tmp[i,j]=0.0
    if(res[i,j]==0 && neigh_number==3) tmp[i,j]=1.0
  }
}


if(t==mgn){
  for(im in 1:nd){
    for(il in 1:nd){
      aa=runif(1,0,1)
      if(aa<mutl) tmp[im,il]=1.0 else tmp[im,il]=tmp[im,il]}}}
      

res=tmp
Sys.sleep(0.1)
heatmap(res, Rowv = NA, Colv = NA, scale = "none",col=c("lightblue","blue"),
        labRow = FALSE,labCol= FALSE, main=paste("Generation ", t))
t=t+1
nsize=c(nsize,sum(res))
}

fy=NULL
for(i in 2:gens-1)
fy=c(fy,(nsize[i-1]-nsize[i])/nsize[i-1])
hist(fy)
mean(fy)

