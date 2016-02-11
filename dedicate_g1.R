# Function takes in bond portfolio parameters and returns a .lp object

# Portfolio bond parameters:
  # p = price of each of the candidate bonds
  # c = coupons for each of the candidate bonds
  # m = maturity period for each bond
  # l = liabilities for each period (periods must be in regular intervals, and the same interval rate 
    # as the coupon payouts)

  # p, c, and l must all be of the same length, and indexed in the same order (bond 1 is in first 
    # position in all three lists)

dedicate_g1 = function (p, c, m, l) {
  my.lp = make.lp(length(l),length(p)+length(l)-1)
  
  i=1
  while (i <= length(m)) { # cycles through the bonds
    v = c(rep(0,length(l))) # assigns all variables 0s
    v[1:m[i]] = c[i] # assigns coupons for all eligible years
    v[m[i]] = c[i]+100 # adds payout for final year
    set.column(my.lp,i,v) # writes to columns
    i = i+1 # increments
  }
  
  k=1
  while (k <= (length(l)-1)) { # cycles through years
    z = c(rep(0,length(l))) # assigns all excess cash variables to 0
    z[k] = -1 # assigns -1 to current year excess cash
    if (k < length(l)) { 
      z[k+1] = 1 # assigns 1 to previous year excess cash
    }
    set.column(my.lp,k+length(m),z)
    k = k+1 # increments
  }
  
  set.objfn(my.lp, c(p, rep(0,(length(l)-1))))
  set.constr.type(my.lp, rep(">=",length(l)))
  set.rhs(my.lp, l)
  
  return(my.lp)
}