input <- read.csv("./day2_input.csv", FALSE);

solution_part_1 <- function (input){
	return (max(input) - min(input));
}

sum(apply(input, 1, solution_part_1));

solution_part_2 <- function (input){
	for (i in 1:length(input)){
		index <- input%%input[i];
		if (length(input[!index]) == 2){
			return (max(input[!index]) %/% min(input[!index]));
		}
	}
}

sum(apply(input, 1, solution_part_2));
