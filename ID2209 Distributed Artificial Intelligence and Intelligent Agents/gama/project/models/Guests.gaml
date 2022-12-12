model Guests

/* Insert your model definition here */

// Chart (series)
// https://gama-platform.org/wiki/DefiningCharts#various-types-of-charts
global {
    int utility <- 2 update: utility*2;
	int numberOfPersons <- 50;
	int numberOfScenes <- 3;
	int numberOfRestaurants <- 3;
	
	image_file steak_shape const: true <- file('../images/steak.png');
	image_file egg_shape const: true <- file('../images/egg.png');
	image_file sallad_shape const: true <- file('../images/broccoli.png');
	image_file restaurant_shape const: true <- file('../images/restaurant.png');
	image_file restaurant_vegetarian_shape const: true <- file('../images/restaurant_vegetarian.png');
	image_file restaurant_vegan_shape const: true <- file('../images/restaurant_vegan.png');
	image_file scene_shape const: true <- file('../images/scene.png');
	image_file terrain <- image_file("../images/soil.jpg");
	
	init { 
		create Guest number:numberOfPersons;
		create Restaurant number:numberOfRestaurants;
		create Scene number:numberOfScenes;
		list<string> foodTypes <- ["meat","vegetarian","vegan"];
		
		loop counter from: 0 to: numberOfRestaurants - 1{
			Restaurant restaurant <- Restaurant[counter];
			let spacing <- 100/numberOfRestaurants;
			let temp <- restaurant.setPosition({spacing * counter+ spacing / 2, 10});
			temp <- restaurant.setFood(foodTypes at counter);
		}
		
		loop counter from: 0 to: numberOfScenes - 1{
			Scene scene <- Scene[counter];
			let spacing <- 100/numberOfScenes;
			let temp <- scene.setPosition({spacing * counter + spacing / 2, 90});
		}
	}
}

species Guest skills:[fipa, moving]{
	string foodTrait <- (1 among ["meat","vegetarian","vegan"])[0];
	string musicTrait <- (1 among ["rock","disco","pop"])[0];
	string crowdTrait <- (1 among ["introvert","extrovert"])[0];
	point destination <- nil;
	
	reflex walk{
		do wander;
	}
	
	reflex walkTo when: destination != nil{
		do goto target: destination;
	}

	aspect base{
		switch foodTrait{
			match "meat"{draw steak_shape size: 5;}
			match "vegetarian"{draw egg_shape size: 5;}
			match "vegan"{draw sallad_shape size: 5;}
		}
	}
}

species Scene skills:[fipa]{
	string musicTrait <- (1 among ["rock","disco","pop"])[0];
	
	action setPosition(point pos){
		location <- pos;
	}

	aspect base {
		draw scene_shape size: 15;
	}
}


species Restaurant skills:[fipa]{
	string foodTrait;
	action setFood(string food){
		foodTrait <- food;
	}
	action setPosition(point pos){
		location <- pos;
	}
	
	aspect base {
		switch foodTrait{
			match "meat" {draw restaurant_shape size: {15,15};}
			match "vegetarian" {draw restaurant_vegetarian_shape size: {15,15};}
			match "vegan" {draw restaurant_vegan_shape size: {15,15};}	
		}
	}
}


experiment Guests type:gui{
	output{
		display guests{
			image terrain refresh: true;
			species Guest aspect:base;
			species Scene aspect:base;
			species Restaurant aspect:base;
		}
		display chart{
			chart "utility" type: series {
	            data "utility" value: utility color: #red;
	        }
		}
	}
}
