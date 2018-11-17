#******************************************MABA COMPUTING FOR ANALYTICS******************************************************
#**********************************************MACHINE PROBLLEM*************************************************************

#Submitted by: JADE A. MATULA
#Nov 14, 2018


#******************************************************************************************************************
#This functions remove the NA values from a vector



Remove_NA <- function(sample_vec) {
  new_list <- c()
  for (i in 1:length(sample_vec)) {
    if(!is.na(sample_vec[i])) {new_list <-c(new_list, sample_vec[i])
    }
  }
  return(new_list)
}

x <- c(1, NA, 3, NA)

Remove_NA(x)

#************************************************************************************************************************

#This function computes for my net pay at work


mynetpay <- function( basic,tax = 0, allow_wtax = 0, lwop_days = 0, m.pay = 13, working_days = 22) {
  ap = (basic + tax) * m.pay
  if(ap <= 250000) {
      net= ap
  } else if (ap <= 400000) {
      net= ap - (ap - 250000) * 0.2
  } else if (ap <= 800000) {
      net = ap - (ap - 80000) * 0.25 - 30000
  } else if (ap <= 200000) {
      net = ap - (ap - 800000) * 0.30- 130000
  } else if (ap <= 8000000) {
     net = ap - (ap - 2000000) * 0.32 - 490000
  } else {
      net = ap - (ap - 8000000) * 0.35 - 2410000
  }

  mnet = net * 1./m.pay
  net.final = mnet + tax - (mnet * 1./working_days) * lwop_days
  return (net.final)
}

mynetpay(70000, 5000, 3000, 2, 13, 22)
#*********************************************************************************************************************

#This function checks if an integer is a PRIME  no. and returns a value of either TRUE or FALSE

Check_Prime <- function(num) {return (sum(num/1:num == num %/%  1:num) == 2) }

Check_Prime(10)


#*********************************************************************************************************************
#This function computes for the compound interest of an investment 
  
  
  #The concept of compound interest is that interest is added back to the principal sum so that interest is
  #earned on that added interest during the next compounding period.
  
  #Compound Interest Formula 
  # A= P(1+ (r/n)^nt
  #Where:  A- Amount
  #        P- Principal
  #        n- Number of times the insterest is compounded per year
  #        r- Interest Rate
#        t- Time (in years)


compound.interest <-function(p,ir,nt,t){
  return (p* (1 + (ir/nt)^(nt*t)))
}


compound.interest(5000,1,1,1)

#******************************************************************************************************************
#This function accepts a POSIXct as argument and outputs the day of the week as characters
  
date= as.POSIXct(as.Date ("01/01/1970"), format = "%m/%d/%Y")

what.day <-function(date){
    days <- c( "Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday") 
    unc <- ((unclass(date) %% 7) + 7)
    return(days[unc])
  }

  

date <- as.Date("2018-11-14")
what.day(date)

date <-as.Date()
#*******************************************************************************************************************
#This function solves for the determinant of a matrix

        Determinant <- function(myDet){
              a <- myDet[1,1]
              b <- myDet[1,2]
              c <- myDet[2,1]
              d <- myDet[2,2]
            return (a*d - b*c)} #determinant formula
                                   
             A = matrix(1:4,2,2) 
                                   
            Determinant(A)

#*****************************************************************************************************************


