

tr<-function(x0,y0,dx,dy,t,thresh,n=0){
    if(t<0) return(n)
    
    dtx<-if(dx>0) (floor(x0)+1-x0)/dx else (floor(x0)-x0)/dx
    dty<-if(dy>0) (floor(y0)+1-y0)/dy else (floor(y0)-y0)/dy
    dt<-min(dtx,dty)+0.001
    
#    if(dt>thresh)
  #      tr(x0+dx*dt,y0+dy*dt,dx,dy,t-dt,thresh,n+2)
#    else
        tr(x0+dx*dt,y0+dy*dt,dx,dy,t-dt,thresh,n+1)
}

n<-10000
l<-200

# random point in box
#bxs0<-runif(n)
#bys0<-runif(n)
#bas<-runif(n)*pi*0.5
#points(bas,Map(tr,bxs0,bys0,cos(bas),sin(bas),l,i*0.1),col=hsv(i/15,alpha=0.3))

# difference between box and gaussian did no affect T.

png('2d.png',width=1000,height=1000)
plot(c(),pch=20,xlab='angle',ylab='# of cells',xlim=c(0,0.25*pi),ylim=c(l*1,l*1.8))

for(i in 0){
    gxs0<-rnorm(n)*0
    gys0<-rnorm(n)*0
    gas<-runif(n)*pi*0.25

    points(gas,Map(tr,gxs0,gys0,cos(gas),sin(gas),l,0.8+i*0.03),col=hsv(i/10,alpha=0.3))
    points(gas,(sin(2*gas)*(sqrt(2)-1)+1)*l)
}
grid()

