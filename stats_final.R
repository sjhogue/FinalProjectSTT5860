#constant
n=100000
#initializing outer loop variables
psampsumstored=rep(0,100)
pboxsumstored=rep(0,100)
psumstored=rep(0,100)
equalequalstored=rep(0,100)
greaterequalstored=rep(0,100)
lesserequalstored=rep(0,100)
totalsum1=rep(0,100)
totalsum2=rep(0,100)

for (k in 1:100){
  #initializing inner-loop variables
  sum1=0
  sum2=0
  psampsum=0
  psum=0
  pboxsum=0
  equalequal=0
  lesserequal=0
  greaterequal=0
  
  for (i in 1:n){
    
    nblack1=sample(1:4,1) #randomly choose the number of black marbles
    nwhite1=5-nblack1
    nblack2=sample(1:4,1) #randomly choose the number of black marbles
    nwhite2=5-nblack2
    box1=c(rep("black",nblack1),rep("white",nwhite1)) #putting marbles in box1
    box2=c(rep("black",nblack2),rep("white",nwhite2)) #putting marbles in box2
    p1=sample(c(0,0/6,1/6,2/6,3/6,4/6,5/6,6/6),1) #randomly choose probability for box1
    p2=1-p1 #set probability for box2 as compliment of box1

    chosen=runif(1) #random number used to determine which box is chosen
  
    ###########################################if box1 gets chosen######################################################
    if (chosen<p1){
      probwin[i]=p1 #store the probability of the box that won
      
      sum1=sum1+1 #counts the number of wins
      samp=sample(box1,5,replace=T) #sample without replacement
      
      bsamp=0
      wsamp=0
      for (i in 1:length(samp)){ #sum number of blacks and whites in the sample
        if (samp[i]=="black"){
          bsamp=bsamp+1
        }else{
          wsamp=wsamp+1
        }
      }
      
      psamp1=(nblack1/5)^bsamp*(nwhite1/5)^wsamp #calculate probability that box1 would produce the sample
      psamp2=(nblack2/5)^bsamp*(nwhite2/5)^wsamp # calculate probability that box2 would produce the sample
      
      if (p1>p2){ #count number of times the chosen box probability is greater than the other
        pboxsum=pboxsum+1
      }
      if (psamp1>psamp2){ #count number of times the chosen box probability of matching the sample is greater than the other
        psampsum=psampsum+1
      }
      if (p1>p2 & psamp1>psamp2){ #count number of times both occur
        psum=psum+1
      }
      
      if (p1==p2){
        if(psamp1>psamp2){
          greaterequal=greaterequal+1
        }else if (psamp2>psamp1){
          lesserequal=lesserequal+1
        }else{
          equalequal=equalequal+1
        }
      }
      
    } else{
      ###############################################if box2 gets chosen################################################
      
      probwin[i]=p2 #store value for probability of winning box
      sum2=sum2+1 #counts the number of wins
      samp=sample(box2,5,replace=T)

      bsamp=0
      wsamp=0
      for (i in 1:length(samp)){  #sum number of blacks and whites in the sample
        if (samp[i]=="black"){
          bsamp=bsamp+1
        }else{
          wsamp=wsamp+1
        }
      }
      
      psamp1=(nblack1/5)^bsamp*(nwhite1/5)^wsamp #calculate probability that box1 would produce sample
      psamp2=(nblack2/5)^bsamp*(nwhite2/5)^wsamp # and calculate probability that box2 would produce sample

      if (p2>p1){ #count number of times the chosen box probability is greater than the other
        pboxsum=pboxsum+1
      }
      if (psamp2>psamp1){ #count number of times the chosen box probability of matching the sample is greater than the other
        psampsum=psampsum+1
      }
      if (p2>p1 & psamp2>psamp1){ #count number of times both occur
        psum=psum+1
      }
      if (p1==p2){
        if(psamp1>psamp2){
          lesserequal=lesserequal+1
        }else if (psamp2>psamp1){
          greaterequal=greaterequal+1
        }else{
          equalequal=equalequal+1
        }
      }
    }
  }
  
  #store those values for computing average
  psampsumstored[k]=psampsum /n
  pboxsumstored[k]=pboxsum/n
  psumstored[k]=psum/n
  equalequalstored[k]=equalequal/n
  lesserequalstored[k]=lesserequal/n
  greaterequalstored[k]=greaterequal/n
  totalsum1[k]=sum1
  totalsum2[k]=sum2
}
mean(psampsumstored)
mean(pboxsumstored)
mean(psumstored)
mean(lesserequalstored)*n/12662
mean(greaterequalstored)*n/12662
mean(equalequalstored)*n/12662
mean(totalsum1)
mean(totalsum2)


#calculating the probability that a probability will produce 
sum1=sum2=sum3=sum4=sum5=0
for (i in 1:length(probwin)){
if (probwin[i]==1/6){
  sum1=sum1+1
}else if (probwin[i]==2/6){
  sum2=sum2+1
}else if (probwin[i]==3/6){
  sum3=sum3+1
}else if (probwin[i]==4/6){
  sum4=sum4+1
}else if (probwin[i]==5/6){
  sum5=sum5+1
}
}

x=sum1/(sum1+sum5)
y=sum2/(sum2+sum4)
w=sum3/(sum3*2)
z=sum4/(sum2+sum4)
v=sum5/(sum1+sum5)

plot(y=c(0,x,y,w,z,v,1),x=c(0,1/6,2/6,3/6,4/6,5/6,1),ylab="probability of box getting chosen",xlab="probabilities")

plo(0+x+y+z+w+v+1)/7

a= hist(probwin) #there is what seems to be a linear growth in occurance as the probabilities increase
a=c(0,a$counts[1],a$counts[4],a$counts[7],a$counts[11],a$counts[14],a$counts[17])
#this takes the total number of occurences for each probability and divides them by the total 
#number of of runs in the simulation

a=a/n
a #these are the probabilities of boxes with probabilites 0/6,1/6,2/6,3/6,4/6,5/6, and 6/6 winning
sum(a) #they sum to one


