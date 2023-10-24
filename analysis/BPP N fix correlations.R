### CCF correlations between N isotope and cyanobacterial pigments
setwd("buffalo-pelican/data")

### extracted columns of data
## Pelican
delN = scan("PelN15.csv")
Canth = scan("PelCan.csv")
Aphan = scan("PelAphan.csv")

delN =ts(delN)
Canth =ts(Canth)
Aphan = ts(Aphan)

## ccf and plot
ccf(delN, Canth)
ccf(delN, Aphan)

### Then same with Buffalo Pound data
NBP = scan("BPN15.csv")
CBP = scan("BPCan.csv")
APB = scan("BPAphan.csv")

NBP = ts(NBP)
CBP = ts(CBP)
APB = ts(APB)

## ccf and plot
ccf(NBP, CBP)
ccf(NBP, APB)
