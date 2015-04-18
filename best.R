best <- function(state, outcome) {

# Validate that outcome is in the defined set before proceeding
if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
	stop ("invalid outcome")
} else {
	# Get column index with the mortality rate we want
	if (outcome=="heart attack") {
		col_index=11
	}
	if (outcome=="heart failure") {
		col_index=17
	}
	if (outcome=="pneumonia") {
		col_index=23
	}
}

# Read data from file, saving the outcome we care about and converting mortality to numeric
raw_data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
data <- raw_data[,c(2,7,col_index)]
names(data) <- c("hospital","state",outcome)
data[,outcome] <- as.numeric(data[,outcome])
c <- complete.cases(data)
data <- data[c,]

# At least one hospital must exist in the state data to return results
state_data <- data[data$state==state,]
if (nrow(state_data)==0) {
	stop("invalid state")
}

# Sort state data, then check for a match of low mortality
state_data <- state_data[order(state_data[,outcome]),]
min_mort <- state_data[1,outcome]

# Count number of tied hospitals
i = 1
repeat {
	if (state_data[i+1,outcome]>min_mort) { break }
	i <- i + 1
}
best_hospitals <- state_data[1:i,]

# Function needs to return only the lowest alphabetical hospital in case of tie
best_hospitals <- best_hospitals[order(best_hospitals[,"hospital"]),]
best_hospitals[1,"hospital"]

}