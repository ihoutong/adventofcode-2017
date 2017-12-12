input <- as.numeric(unlist(strsplit("10,3,15,10,5,15,5,15,9,2,5,8,5,2,3,6", ",")));


solution_part_1 <- function (input){
	total_configurations <- c(toString(input));
	cycle_num <- 0;
	input_length <- length(input);
	while (TRUE){
		value <- max(input);
		starting_key <- which(input == value)[1];
		input[starting_key] <- 0;
		for (i in 1:value){
			next_block <- starting_key + i;
			if (next_block > input_length){
				#next_block <- next_block %% input_length;
				next_block <- next_block - input_length;
			}
			input[next_block] <- input[next_block] + 1;
		}
		cycle_num <- cycle_num + 1;

		input_as_string <- toString(input);
		does_configuration_exist <- total_configurations == input_as_string;
		if (input_as_string %in% total_configurations){
			return (cycle_num);
		}
		total_configurations <- c(total_configurations, input_as_string);
	}
}

solution_part_1(input);

solution_part_2 <- function (input){
	total_configurations <- c(toString(input));
	cycle_num <- 0;
	input_length <- length(input);
	while (TRUE){
		value <- max(input);
		starting_key <- which(input == value)[1];
		input[starting_key] <- 0;
		for (i in 1:value){
			next_block <- starting_key + i;
			if (next_block > input_length){
				#next_block <- next_block %% input_length;
				next_block <- next_block - input_length;
			}
			input[next_block] <- input[next_block] + 1;
		}
		cycle_num <- cycle_num + 1;

		input_as_string <- toString(input);
		does_configuration_exist <- total_configurations == input_as_string;
		if (input_as_string %in% total_configurations){
			#minus 1 from the return from which because R counts from 1
			return ((cycle_num - (which(total_configurations == input_as_string)[1] - 1)));
		}
		total_configurations <- c(total_configurations, input_as_string);
	}
}

solution_part_2(input);
