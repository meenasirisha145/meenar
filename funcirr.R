IRR=function(Duration,inst_lft,inst_nonlft,lft_time)
{
  yield=Duration*inst_lft+(lft_time-1)*1000
  totalamt=(lft_time-1)*inst_lft+(Duration-lft_time+1)*inst_nonlft
  irr=(-yield+totalamt)/totalamt
  return(irr)
}

ii=IRR(40,5000,7000,4)
ii
ii2=IRR(40,5000,7000,39)
ii2
