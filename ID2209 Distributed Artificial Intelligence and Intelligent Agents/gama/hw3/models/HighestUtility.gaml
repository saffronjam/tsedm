/**
* Name: HighestUtility
* Based on the internal empty template. 
* Author: pierrelf
* Tags: 
*/


model HighestUtility

global {
	int numberOfPersons <- 200;
	int numberOfScenes <- 4;
	
	init { 
		create Person number:numberOfPersons;
		create Scene number:numberOfScenes;
		
		list<point> points <- [{10,10}, {90,10}, {10,90}, {90,90}];
		
		loop counter from: 0 to: numberOfScenes - 1{
			Scene scene <- Scene[counter];
			let temp <- scene.setPosition(points at counter);
		}
	}
}

species Scene skills:[fipa]{
	
	action setPosition(point pos){
		location <- pos;
	}
	
	
	aspect base {
		rgb agentColor <- rgb("black");
		draw square(15) color: agentColor;
	}
}

species Person skills:[fipa, moving]{
	
	aspect base{
		rgb agentColor <- rgb("black");
		draw circle(1) color: agentColor;
	}
}


experiment HighestUtility type:gui{
	output{
		display myDisplay {
			species Person aspect:base;
			species Scene aspect:base;
		}	
	}
}
