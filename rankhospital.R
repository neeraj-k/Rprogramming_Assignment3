

rankhospital <- function(state, outcome, num) {
	ocm <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	ocmForState <- subset(ocm, ocm$State == state)

	if (nrow(ocmForState) == 0) {
		stop("invalid state")
	}

	if (num != "best" && num != "worst" && num > nrow(ocmForState)) {
		return("NA")
		stop()
	}

	if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
		stop("invalid outcome")
	}

	HospitalsInState <- ocmForState$Hospital.Name

	if (outcome == "heart attack") {
		MortalityRate <- ocmForState[,11]
	} else if (outcome == "heart failure") {
		MortalityRate <- ocmForState[,17]
	} else if (outcome == "pneumonia") {
		MortalityRate <- ocmForState[,23]
	}

	df <- cbind(HospitalsInState, MortalityRate)
	dfwona <- subset(df, df[,2] != "Not Available")

	# order the data frame alphabetically by hospital names
	d <- dfwona[order(dfwona[,1]),]

	# again order the data frame based on mortality rate
	df <- d[order(as.numeric(d[,2])),]

	if (num == "best") {
		return(df[[1,1]])
	} else if (num == "worst") {
		return(df[[nrow(df),1]])
	} else {
		return(df[[num,1]])
	}
}