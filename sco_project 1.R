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

# contstraints = years (8), bond coupon equality (10)
# variables = bond payouts (10), coupon payouts (10), excess cash (7)

p = c(102,99,101,98,98,104,100,101,102,94) #price
c = c(5,3.5,5,3.5,4,9,6,8,9,7) #coupon
m = c(1,2,2,3,4,5,5,6,7,8) #maturity
l = c(12000,18000,20000,20000,16000,15000,12000,10000) #liabilities (RHS)
rhs = c(l,rep(0,10))

my.lp = make.lp(length(l)+length(p), length(p)*2+length(l)-1) #constraints, variables


#set the variables for face values
for(i in c(1:length(l))){ # cycles through the liabilities (years)
  for (j in m) { # cycles through the maturities
    if (i == j) { #check for maturity equaling year
      v = c(rep(0,18)) 
      v[i] = 1 # if so, enters that variable into the right place in the matrix (otherwise variables are zero)
      set.column(my.lp,i,v)
    }
  }
}


set.column(my.lp, c(11:27), rep(0,18))
set.objfn(my.lp, rep(0,27))
set.constr.type(my.lp, rep("=",18))
set.rhs(my.lp, rhs)
my.lp
#HOW DO WE VIEW A MY.LP??

typeof(my.lp)
