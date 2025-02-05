#Hawk-Dove game - ESS outcomes

#set parameters
Q=100
c=seq(Q/2,200,length=200)

#set payoffs
A=(Q/2)-c
B=Q
C=Q/2

#evaluate proportion hawks
h=(C-B)/(A-B+C)

#plot a basic graph
par(mfrow=c(1,1))
plot(c,h)