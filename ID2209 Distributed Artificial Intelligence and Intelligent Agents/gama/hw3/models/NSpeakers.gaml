// Bonus 1 and basic assignment
model NSpeakers

global {
	int size <- 4;
	
	init {
		create Speaker number:size;
		
		loop counter from: 0 to: size-1{
			Speaker speaker <- Speaker[counter];
			let temp <- speaker.setId(counter);
			temp <- speaker.setId(counter);
			temp <- speaker.setY(-1);
			temp <- speaker.calculateLocation();
		}
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
			location <- {(id/size)*80 + 10, 5};
		}
		else
		{
			location <- {(id/size)*80 + 10, (y/size)*80 + 10};
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
				
		draw triangle(2) color: agentColor;
	}
}

experiment nspeakers type:gui{
	output{
		display myDisplay {
			species Speaker aspect:base;
		}	
	}
}




