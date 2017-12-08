input <- 347991;

size <- 1;
while (TRUE){
	size <- size + 2;
	if (input <= size * size){
		break;
	}
}
size;