  t=0
  for(i in 1:10000){
  C=1
  A=4
  B=9
  d=sample(0:1,8, replace=T, prob=c(0.5,0.5))
  while(C<8 & A>0){
    if (d==0){A=A-1
    C=C+1}
    else{B==B-1
    C=C+1}
  }
  if (A==0){t=t+1}
  }
  print(t/10000)
  
