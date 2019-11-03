#Get Present Values of Cashflows
#Using formula PV = FV/ (1+r)^t, where r is the interest rate and t is time
PVs <- 0
PresentValues <- function(cashFlows, interestRate) {
  for (i in 1:length(cashFlows)) {
    PVs[i] <- cashFlows[i]/(1 + interestRate)^(i-1)
  }
  PVs
}


#Get NPV - Net Present Value
#NPV Formula = CF0 + PV1 + PV2 +....+ PVn (CF0 is initial cash flow, PV is present value of future cash flow)
NetPresentValue <- function (cashFlows, interestRate) {
  NPV <- sum(PresentValues(cashFlows, interestRate)) #Present Value of CF0 = CF0 so we don't need to add again
  NPV
}

#Get PI - Profitability Index
#Profitability Index Formula: (NPV + Initial Investment)/Initial Investment
ProfitabilityIndex <- function(cashFlows, interestRate) {
  PI <- (NetPresentValue(cashFlows,interestRate) + abs(cashFlows[1]))/abs(cashFlows[1]) #abs: get absolute value 
  PI
}

#Get Payback Period
#Payback Period Calculation Formula: Payback Period = A +	B/C
#A is the last period with a negative cumulative cash flow
#B is the absolute value of cumulative cash flow at the end of the period A
#C is the total cash flow during the period after A
PayBack <- function(cashFlows, interestRate) {
  value <- cashFlows[1]
  PB <- 0
  for (i in 2:length(cashFlows)) {
    value <- value + cashFlows[i]   
    if (value >= 0) {
      PB = i- 2 + abs(value - cashFlows[i])/cashFlows[i]
      break
    }
  }
  PB
}

# Complete Output:
# A list with data table showing the year, cash flows, and present values of an investment,
# Net Present Value, Profitability Index, and Payback Period as calculated above
myNPV <- function (cashFlows, interestRate) {
  list("Table of Present Values" = data.frame(Year = 0:(length(cashFlows)-1), 
                                              "CashFlows"     = cashFlows,
                                              "PresentValues" = round(PresentValues(cashFlows, interestRate), digits = 2)),
       "Net Present Value"   = round(NetPresentValue(cashFlows, interestRate), digits = 2),
       "Profitability Index" = round(ProfitabilityIndex(cashFlows, interestRate), digits = 2),
       "Payback Period"     = PayBack(cashFlows, interestRate)
  )
}

#Try with Defined Inputs
myNPV(c(-10, 2, 4, 5, 9, 6), 0.12)
