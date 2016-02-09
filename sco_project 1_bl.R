library('lpSolveAPI')

#decision variables: (x1), ... , (x10) amount of each bond
#                    (z1), ... , (z7) excess cash at the end of each year

#minimize: 102(x1) + 99(x2) + 101(x3) + 98(x4) + 98(x5) + 104(x6) + 100(x7) + 101(x8) + 102(x9) + 94(x10)
        # cost of each bond

#subject to:

# year 1: 12000 = 100(x1) + 5(x1) + 3.5(x2) + 5(x3) + 3.5(x4) + 4(x5) + 9(x6) + 6(x7) + 8(x8) + 9(x9) + 7(x10) - (z1)
# year 2: 18000 = 
# year 3: 20000 = 
# year 4: 20000 = 

# constraints = years (8), bond coupon equality (10)
# variables = bond payouts (10), coupon payouts (10), excess cash (7)

p = c(102,99,101,98,98,104,100,101,102,94) #price
c = c(5,3.5,5,3.5,4,9,6,8,9,7) #coupon
m = c(1,2,2,3,4,5,5,6,7,8) #maturity
l = c(12000,18000,20000,20000,16000,15000,12000,10000) #liabilities (RHS)
rhs = c(l,rep(0,10))

#A = matrix(0,8,10)
my.lp = make.lp(length(l),length(p)) #constraints, variables

i=1
while (i <= length(m)) { # cycles through the bonds
  v = c(rep(0,length(l))) # assigns all variables 0s
  v[1:m[i]] = c[i] # assigns coupons for all eligible years
  v[m[i]] = c[i]+100 # adds payout for final year
  set.column(my.lp,i,v) # writes to columns
  #A[,i] = v
  i = i+1 # increments
}



#trying to set the rest of the values in the my.lp object so we can view it
set.objfn(my.lp, p)
set.constr.type(my.lp, rep("=",8))
set.rhs(my.lp, l)
print.lpExtPtr(my.lp)
#HOW DO WE VIEW A MY.LP??






