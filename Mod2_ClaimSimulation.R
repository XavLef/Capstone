# Install and load R package "cascsim", an open-source R package built to provide claim fitting and simulation capabilities, supported by reporting functionality.
#install.packages("cascsim")
library(cascsim)

# Load claim data used for case study
claiminput <- data(claimdata) # # PAS UTILISÉ AILLEURS, SAUF SI ON AVAIT
                                # À ESTIMER LES PARAMÈTRES.

lines <- c("Auto","Auto","Liab","Liab")
types <- c("N","H","N","H")
classes <- c("RBNER","ROPEN","IBNR","UPR")
lines <- unique(lines)
types <- unique(types)

# Set claim development factors for calculating IBNER
D1<- new("DevFac",FacID="D1",FacModel= FALSE,meanList =c(1.2,1.15,1.1,1.05,1),
         volList =c(0.048795533636275,6.33648966438753E-02,7.91297479362304E-02,
                    0.09743982905669,0))

# Set default reopenness probability
D2<- new("DevFac",FacID="D2",FacModel= FALSE,meanList =c(0.02,0.015,0.01,0.005,0),
         volList =c(0,0,0,0,0))

# Set default reopened claim development factors
D3<- new("DevFac",FacID="D3",FacModel= FALSE,meanList =c(1.05,1.1,1,1,1),
         volList =c(1.50794588175691E-04,3.67511835739522E-03,4.13492636630684E-03,
                    9.38023951245216E-03,0))

# Set default function for LAE assumption
F1<- new("DevFac",FacID="F1",FacModel= TRUE,fun="Linear",paras =c(0.1,0.01,0.005,0.002,5))
#LAE=0.1+0.01*development year+0.005*ultimate loss +0.002* o/s ratio + 5*N(0,1)

# Set flat exposure index assuming business volume is steady
I1<- new("Index",indexID="I1",tabulate= TRUE,startDate=as.Date("2017-01-01"),
         monthlyIndex =rep(1,360))

#Create the claim object AutoN
AutoN <- new("ClaimType", line = "Auto", claimType = "N")

##Distribution assumption setting for LoB "Auto" Type "N"##
#set exposure information as an index
AutoN@exposureIndex <- I1 # level exposure across time

#set monthly frequency distribution assumption
AutoN@frequency <- new("Poisson", p1 =30.233) #from Table 3.6

#set severity trend
AutoN@severityIndex <- setIndex(new("Index",indexID="I2",
                                    tabulate= FALSE,
                                    startDate=as.Date("2017-01-01"),
                                    annualizedRate = 0.02)) #assuming a 2% annual inflation

#set severity distribution assumption
AutoN@severity <- new("Lognormal", p1 =1.956, p2 =2.984) #from Table 3.6

#set deductible distribution assumption. Here we set a universal deductible of 100.
AutoN@deductible <- new("Empirical", empirical=matrix(c(0,1,100,100),2,2))

#set limit distribution assumption. Here we set a universal limit of 1e8.
AutoN@limit <- new("Empirical", empirical=matrix(c(0,1,1e8,1e8),2,2))

#set the probability of zero-amount claims (excluding those small claims below deductibles).
AutoN@p0<- new("DevFac",meanList=c(0,0),volList=c(0,0))

#set report lag assumption
AutoN@reportLag <-new("Lognormal", p1 =1.346, p2 =0.874) #from Table 3.6

#set settlement lag assumption
AutoN@settlementLag <- new("Lognormal", p1 =1.905, p2 =0.995) #from Table 3.6

#set the copula among severity, report lag and settlement lag
AutoN@iCopula <- TRUE #use copula
AutoN@ssrCopula <- new("CopulaObj", type ="clayton", dimension = 3, param = 0.0015)#A Clayton Copula from Table 3.6
AutoN@ssrCopula@marginal <- c(AutoN@severity,AutoN@settlementLag,AutoN@reportLag) #Set marginal distributions

#Set loss adjustment expense assumption
AutoN@laeDevFac <- F1

##IBNER assumption setting##
#IBNER loss development factor (from incurred loss to ultimate loss)
AutoN@fIBNER <- D1

##Claim Reopenness assumption setting##
#Reopen probability
AutoN@reopen <- D2

#Reopen loss development factor
AutoN@roDevFac <- D3

#Reopen lag assumption. Time lag between last close date and reopen date
AutoN@reopenLag <- new("Exponential", p1 =0.01)

#Resettlement lag assumption. Time lag between reopen date and resettle date
AutoN@resettleLag <- new("Exponential", p1 =0.25)

#Create the claim object AutoH (LoB Auto and Type H)
## Constat: On peut changer les items en plusieurs lignes ou tout faire via les
##          arguments en un appel de fonction.
AutoH<-new("ClaimType",
line = "Auto",
claimType = "H",
exposureIndex = I1,
frequency = new("Poisson", p1 =10.25,truncated = FALSE), #from Table 3.6
severityIndex = I1,
severity = new("Lognormal", p1 =1.971, p2 =3.43,truncated = FALSE), #from Table 3.6
reportLag = new("Gamma", p1 =1.352, p2 =7.694,truncated = FALSE), #from Table 3.6
settlementLag = new("Gamma", p1 =1.191, p2 =17.46, truncated = FALSE), #from Table 3.6
iCopula = TRUE,
ssrCopula =new("CopulaObj", type ="normal", dimension = 3, param = c(-0.0337,-0.0161,0.1001)), #from Table 3.6
laeDevFac = F1,
fIBNER = D1,
reopen = D2,
roDevFac = D3,
reopenLag = new("Exponential", p1 =0.01),
resettleLag = new("Exponential", p1 =0.25)
)
AutoH@ssrCopula@marginal = c(AutoH@severity,AutoH@reportLag,AutoH@settlementLag)

# The parameters for LoB/Type can be set individually as above, or in one initialization function, as below
#Create the claim object LiabN (LoB Liab and Type N)
LiabN<-new("ClaimType",
line = "Liab",
claimType = "N",
exposureIndex = I1,
frequency = new("NegativeBinomial", p1 =19.737, p2 =0.479, truncated = FALSE), #from Table 3.6
severityIndex = I1,
severity = new("Gamma", p1 =29.236, p2 =0.053,truncated = FALSE), #from Table 3.6
reportLag = new("Gamma", p1 =1.072, p2 =46.655,truncated = FALSE), #from Table 3.6
settlementLag = new("Gamma", p1 =1.096, p2 =46.18, truncated = FALSE), #from Table 3.6
iCopula = TRUE,
ssrCopula =new("CopulaObj", type ="joe", dimension = 3, param = 1.001), #from Table 3.6
laeDevFac = F1,
fIBNER = D1,
reopen = D2,
roDevFac = D3,
reopenLag = new("Exponential", p1 =0.01),
resettleLag = new("Exponential", p1 =0.25)
)
LiabN@ssrCopula@marginal = c(LiabN@severity,LiabN@reportLag,LiabN@settlementLag)

#Create the claim object LiabH (LoB Liab and Type H)
LiabH<-new("ClaimType",
line = "Liab",
claimType = "H",
exposureIndex = I1,
frequency = new("NegativeBinomial", p1 =10.542, p2 =0.708, truncated = FALSE), #from Table 3.6
severityIndex = I1,
severity = new("Lognormal", p1 =7.254, p2 =0.344,truncated = FALSE), #from Table 3.6
reportLag = new("Gamma", p1 =1.017, p2 =90.34,truncated = FALSE), #from Table 3.6
settlementLag = new("Gamma", p1 =1.096, p2 =80.865, truncated = FALSE), #from Table 3.6
iCopula = TRUE,
ssrCopula =new("CopulaObj", type ="joe", dimension = 3, param = 1.001), #from Table 3.6
laeDevFac = F1,
fIBNER = D1,
reopen = D2,
roDevFac = D3,
reopenLag = new("Exponential", p1 =0.01),
resettleLag = new("Exponential", p1 =0.25)
)
LiabH@ssrCopula@marginal = c(LiabH@severity,LiabH@reportLag,LiabH@settlementLag)

##Set up simulation project
# Add combinations of LoB and Type objects to be analyzed using the simulation process
objlist <- list(AutoN,AutoH,LiabN,LiabH)

simobj <- new("Simulation", lines=lines, types=types, claimobjs= objlist)

#Set working folder
simobj@workingFolder <- getwd() #input your own working folder
#Set the number of simulations to run
simobj@simNo <- 2 #100

#Set the number of cores. If greater than 1, parallel computing will be used
simobj@ncores <- 1

#Set the claim classes to run
simobj@iRBNER <-TRUE #run open claims
simobj@iROPEN <-TRUE #run claim reopenness
simobj@iIBNR <-TRUE #run IBNR
simobj@iUPR <-TRUE #run future claims

#Set frequency copula among LoBs
simobj@iCopula <- TRUE
simobj@freqCopula <-new("CopulaObj", type ="frank", dimension =2, param = -0.8872) #from Table 3.6

#Set dates
start_date = as.Date("2012-01-01") #date from which claims are considered for future payments simulation
evaluation_date = as.Date("2016-12-31") #valuation date
future_date = as.Date("2022-12-31") #date till when future claims are simulated
set.seed(20240429)
#Run simulation and generate individual claim level data
simdata <- claimSimulation(simobj, claimdata, startDate = start_date,
                           evaluationDate = evaluation_date,
                           futureDate = future_date)

#Summarizing generated data to output descriptive statistics
simSummary <- simSummary(simobj,simdata, startDate = start_date,
                         evaluationDate = evaluation_date,
                         futureDate = future_date)

#Generating Triangles
simTriangle <- simTriangle(simobj,claimdata,simdata,
                           startDate = start_date,
                           evaluationDate = evaluation_date,
                           futureDate = future_date)

#Generating html report that includes results and assumptions
simReport(simobj, simSummary, simTriangle,
          startDate = start_date,
          evaluationDate = evaluation_date,
          futureDate = future_date)

