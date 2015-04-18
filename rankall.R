rankall <- function(outcome, num = "best") {

	# Validate that outcome is in the defined set before proceeding
	if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
		stop ("invalid outcome")
	} else {
		# Get column index with the mortality rate we want
		if (outcome=="heart attack") { col_index=11	}
		if (outcome=="heart failure") {	col_index=17 }
		if (outcome=="pneumonia") {	col_index=23 }
	}

	# Read data from file, saving the outcome we care about and converting mortality to numeric
	raw_data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	data <- raw_data[,c(2,7,col_index)]
	names(data) <- c("hospital","state",outcome)
	data[,outcome] <- as.numeric(data[,outcome])
	c <- complete.cases(data)
	data <- data[c,]
	data <- data[order(data[,"state"],data[,outcome],data["hospital"]),]

	state_list <- unique(data$state)
	state_list <- state_list[order(state_list)]

	hospital_list <- vector(length=length(state_list))
	i <- 1
	for (state in state_list) {
		state_data <- data[data$state==state,]

		# Convert text rankings to numeric values
		if (num == "best") { pos <- 1 }
		if (num == "worst") { pos <- nrow(state_data) }
		if (is.numeric(num)) {	pos <- num }
	
		if (pos <= nrow(state_data)) {
			hospital_list[i] <- state_data[pos,"hospital"]
		} else {
			hospital_list[i] <- NA
		}
		i <- i+1
	
	}

	rank_bystate <- data.frame()
	rank_bystate <- cbind(hospital_list,state_list)
	colnames(rank_bystate) <- c("hospital","state")
	rownames(rank_bystate) <- state_list
	rank_bystate <- as.data.frame(rank_bystate)

	return(rank_bystate)
}