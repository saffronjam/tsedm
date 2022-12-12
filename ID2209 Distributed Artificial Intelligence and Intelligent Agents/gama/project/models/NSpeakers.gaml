// Bonus 1 and basic assignment
model NSpeakers

global {
	int size <- 7;
	
	init {
		create Speaker number:size;
		create Square number:size*size;
		
		loop counter from: 0 to: size-1{
			Speaker speaker <- Speaker[counter];
			let temp <- speaker.setId(counter);
			temp <- speaker.setId(counter);
			temp <- speaker.setY(-1);
			temp <- speaker.calculateLocation();
		}
		
		loop row from: 0 to: size - 1{
			loop col from: 0 to: size - 1{
				Square square <- Square[row*size+col];
				let temp <-	square.setPos(row, col);
			} 
		}
	}
}

species Square{
	int col;
	int row;
	
	action setPos(int r, int c){
		row <- r;
		col <- c;
			location <- {(col/size)*100 + 50/size, (row/size)*100 + 50/size};
	}
	
	aspect base {
		rgb agentColor <- rgb("brown");
		if(col mod 2 = 0){
			if(row mod 2 = 0){			
				agentColor <- rgb("brown");	
			}else{
				agentColor <- rgb("beige");
			}	
		}else{
			if(row mod 2 = 0){			
				agentColor <- rgb("beige");	
			}else{
				agentColor <- rgb("brown");
			}	
		}
		draw square(100/size) color: agentColor;
		
	}
}

species Speaker{
	int id;
	int y;
	bool startTurn <- false;
	
	action setId(int idNumber){
		id <- idNumber;
	}
	
	action setY(int newY){
		y <- newY;
	}
	
	action calculateLocation{
		if (y < 0){
			location <- {(id/size)*100 + 50/size, -20/size};
		}
		else
		{
			location <- {(id/size)*100 + 50/size, (y/size)*100 + 50/size};
		}
	}
	
	bool isSafe(int leftY, int upY, int downY){
		write "isSafe " + id + '(' + y + '): ' + leftY + ' ' + upY + ' ' + downY;
		
		/* Check this row on left side */
		/* Check upper diagonal on left side */
        /* Check lower diagonal on left side */
        if(y = leftY or y = upY or y = downY){
        	write 'not safe!';
			return false;
		}
		
		if(id = 0){
        	write 'safe!';
			return true;
		}
		
		bool predSafe;
		
		ask Speaker[id-1]{
			predSafe <- self.isSafe(leftY, upY - 1, downY + 1);
		}
		
        return predSafe;
	}
	
	action startNextTurn{
		startTurn <- true;
	}
	
	
	reflex startTurn when: startTurn{
		startTurn <- false;
		
		write 'Start turn: ' + id + ' y=' + y;
		
		if (y >= size){
			y <- -1;
		}
		
		y <- y + 1;
		
		if(id = 0){
			ask Speaker[id+1]{let tmp <- self.startNextTurn();}
		}else{
			if (y >= size){
				if(id != 0){
					ask Speaker[id-1]{let tmp <- self.startNextTurn();}
				}
			}
			else{
				ask Speaker[id-1]{
					if(self.isSafe(myself.y, myself.y-1, myself.y+1)){
						if (myself.id != size-1){
							ask Speaker[myself.id+1]{let tmp <- self.startNextTurn();}
						}	
					}else{
						myself.startTurn <- true;
					}
				}
			}
		}	
    }
	
	reflex setNewLocation{
		do calculateLocation();
	}
	
	reflex startGame when: time = 0 and id = 0{
		startTurn <- true;		
	}
	
	aspect base {
		rgb agentColor <- rgb("black");
		if (y < 0 or y >= size){
			agentColor <- rgb("gray");
		}else{
			ask Speaker[id-1]{
				if(not self.isSafe(myself.y, myself.y-1, myself.y+1)){
					agentColor <- rgb("red");
				}		
			}
			
		}
				
		draw circle(20/size) color: agentColor;
	}
}

experiment nspeakers type:gui{
	output{
		display myDisplay {
			species Square aspect:base;
			species Speaker aspect:base;
		}	
	}
}




