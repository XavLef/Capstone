# Install and load R package "cascsim", an open-source R package built to provide claim fitting and simulation capabilities, supported by reporting functionality.
#install.packages("cascsim")
library(cascsim)

# Load claim data used for case study
data <- data(claimdata)

lines <- c("Auto","Auto","Liab","Liab")
types <- c("N","H","N","H")
classes <- c("RBNER","ROPEN","IBNR","UPR")
lines <- unique(lines)
types <- unique(types)

# Set up combinations of line Auto, type N with default assumptions.
AutoN<-new("ClaimType", line = "Auto", claimType = "N", iCopula = TRUE)
AutoH<-new("ClaimType", line = "Auto", claimType = "H", iCopula = TRUE)
LiabN<-new("ClaimType", line = "Liab", claimType = "N", iCopula = TRUE)
LiabH<-new("ClaimType", line = "Liab", claimType = "H", iCopula = TRUE)

# Add combinations of LoB and Type objects to be analyzed using the fitting process
objlist <- list(AutoN,AutoH,LiabN,LiabH)

# Claim analysis working directory. Change to an existing folder in your computer.
wd  <- getwd()

# Initialize simulation object with default values
simobj <- new("Simulation",
		lines=lines,
		types=types,
		claimobjs=objlist,
		workingFolder= wd)

# Set if frequency copula should be considered, and the default copula for frequency copula initialization
simobj@iCopula <- TRUE
simobj@freqCopula <-new("CopulaObj", type ="normal", dimension =2, param = c(0.0))

# Perform claim fitting --> POUR L'AJUSTEMENT
simobj <- claimFitting(simobj, claimdata,
				startDate = as.Date("2012-01-01"), #starting date of the study period
				evaluationDate = as.Date("2016-12-31"), #end date of the study period/valuation date
				discreteDist = c("Poisson","NegativeBinomial","Geometric"),
				#pool of discrete distributions to be tested
				continuousDist = c("Normal","Lognormal","Pareto","Weibull", "Gamma","Uniform","Exponential"),
				#pool of discrete distributions to  be tested
				copulaList = c("normal","clayton","gumbel","frank","joe", "t"))

#pool of copula functions to be tested


# In the specified working folder in the program, three major output files are generated:
#fitsummary.csv that contains distribution fitting information as shown in Table 3.2 and Table 3.3
#fitsumcop.csv that contains copula fitting information as shown in Table 3.5
#/fit/fitreport.html that contains summary and visualization