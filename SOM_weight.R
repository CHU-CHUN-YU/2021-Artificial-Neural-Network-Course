ix1=c(1.1,1.3,0.8,0.5,1.6,1.1,0.6,0.2,1.3)
ix2=c(1.2,1.4,1.2,0.3,0.9,1.8,0.9,1.8,0.5)

x1=c(1.6,0.2,0.9,1.8,0.2,0.1,1.8,0.8,0.7,1.3)
x2=c(0.7,0.2,0.6,1.9,1.9,1.2,0.6,0.9,0.3,0.2)

n1=c(0,1,2,0,1,2,0,1,2)
n2=c(2,2,2,1,1,1,0,0,0)
num=c(1:9)
neuron=cbind(n1,n2,num)

data=cbind(x1,x2)
weight_ini=cbind(ix1,ix2)
W=array(NaN, dim=c(9,2,10))
W_c=array(NaN, dim=c(9,2,10))
######################
R0=2
k=0
R_K=R0*exp(k)
eta_k=1
weight=weight_ini
########################
for (n in 1:10){

  dist=0
  for (i in 1:9){
    dist[i]=sqrt(sum((data[n,]-weight[i,])^2))
  }
  winner=which.min(dist)
  ################
  h_fun<-function(rq,rj,R_K){
    value=exp((-(sqrt(sum((rq-rj)^2)))^2)/(2*R_K^2))
    return(value)
  }
  ####################
  q=winner
  change_weight_matrix=matrix(NaN,9,2)
  
  for(j in 1:9){
    if(j==winner){
      change_weight_matrix[winner,]=c(0,0)}
    else{
      rq=neuron[winner,1:2]
      rj=neuron[j,1:2] #1
      h=h_fun(rq,rj,R_K)
      change_w=h*(data[n,]-weight[j,])
      change_weight_matrix[j,]=change_w
    }
  }
  
  weight=weight+change_weight_matrix
  W_c[,,n]=change_weight_matrix
  W[,,n]=weight
  
}