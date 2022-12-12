model Guests

/* Insert your model definition here */

// Chart (series)
// https://gama-platform.org/wiki/DefiningCharts#various-types-of-charts
global {
	int num_guests <- 50;
	int num_scenes <- 1;
	int num_restaurants <- 3;
	
	image_file steak_shape const: true <- image_file('../images/steak.png');
	image_file egg_shape const: true <- image_file('../images/egg.png');
	image_file sallad_shape const: true <- image_file('../images/broccoli.png');
	image_file steak_mask_shape const: true <- image_file('../images/steak-mask.png');
	image_file egg_mask_shape const: true <- image_file('../images/egg-mask.png');
	image_file sallad_mask_shape const: true <- image_file('../images/broccoli-mask.png');
	image_file restaurant_shape const: true <- image_file('../images/restaurant.png');
	image_file restaurant_vegetarian_shape const: true <- image_file('../images/restaurant_vegetarian.png');
	image_file restaurant_vegan_shape const: true <- image_file('../images/restaurant_vegan.png');
	image_file scene_shape const: true <- image_file('../images/scene.png');
	image_file terrain const: true <- image_file("../images/soil.jpg");
	
	init { 
		create Guest number:num_guests;
		create Restaurant number:num_restaurants;
		create Scene number:num_scenes;
		list<string> food_types <- ["meat","vegetarian","vegan"];
		
		loop counter from: 0 to: num_restaurants - 1{
			Restaurant restaurant <- Restaurant[counter];
			let spacing <- 100/num_restaurants;
			let temp <- restaurant.setPosition({spacing * counter+ spacing / 2, 10});
			temp <- restaurant.setFood(food_types at counter);
		}
		
		loop counter from: 0 to: num_scenes - 1{
			Scene scene <- Scene[counter];
			let spacing <- 100/num_scenes;
			let temp <- scene.setPosition({spacing * counter + spacing / 2, 90});
		}
	}
	
	int scene_meat -> {length(agents_overlapping(Scene[0]) where (Guest(each).food_trait = "meat" ))};
	int scene_vegetarian -> {length(agents_overlapping(Scene[0]) where (Guest(each).food_trait = "vegetarian" ))};
	int scene_vegan ->  {length(agents_overlapping(Scene[0]) where (Guest(each).food_trait = "vegan" ))};
	
	int restaurant_meat -> {length(agents_overlapping(Restaurant[0]))};
	int restaurant_vegetarian -> {length(agents_overlapping(Restaurant[1]))};
	int restaurant_vegan ->  {length(agents_overlapping(Restaurant[2]))};
	
	int meat_roaming -> {length(Guest where (Guest(each).food_trait = "meat" and Guest(each).destination_point != nil and Guest(each).destination_point.y > 40 and Guest(each).destination_point.y < 60 and Guest(each).crowd_trait != "introvert"))};
	int introverts_roaming ->  {length(Guest where (Guest(each).destination_point != nil and Guest(each).destination_point.y > 40 and Guest(each).destination_point.y < 60 and Guest(each).crowd_trait = "introvert" ))};
}

species Guest skills:[fipa, moving]{
	string food_trait <- (1 among ["meat","vegetarian","vegan"])[0];
	string music_trait <- (1 among ["rock","disco","pop"])[0];
	string crowd_trait <- (1 among ["introvert","extrovert","extrovert","extrovert"])[0];
	int hunger <- 0;
	point destination_point <- nil;
	bool destination_reached <- true;

	reflex getHungry{
		hunger <- hunger + 1;
	}
	
	action setDestination(point dest){
		if destination_reached{
			if dest = nil{
				dest <- {rnd(10,90), rnd(40,60)};
			}
			destination_reached <- false;
			destination_point <- dest;
		}
	}
	
	reflex arrive when: destination_point != nil and self distance_to destination_point < 2{
		destination_reached  <- true;
	}
	
	reflex eat when: destination_point != nil and self distance_to destination_point < 2 and hunger > 200 and location.y < 40{
		hunger <- 0;
	}
	
	reflex walkTo when: destination_point != nil{
		do goto target: destination_point;
	}
	
	reflex checkWithScenes when: time mod 5 = 0{
		do start_conversation to: list(Scene) protocol: 'fipa-propose' performative: 'propose' contents:[];
	}
	
	reflex receive when: !empty(accept_proposals){
		loop msg over: accept_proposals {
			string dummy <- msg.contents;
			
			if msg.sender = Scene[0] {
				list<string> scene_traits <- msg.contents[0];
				list<int> scene_counter <- msg.contents[1];
		
				if(music_trait in scene_traits){
					
					if (crowd_trait = "introvert"){
						let sum <- scene_counter[0] + scene_counter[1] + scene_counter[2];
						if(sum > 20){
							do setDestination(nil);
							do start_conversation to: list(Restaurant) protocol: 'fipa-propose' performative: 'propose' contents:[];	
							return;
						}
					}
				
					switch food_trait{
						match "meat"{
							if (scene_counter[2] < 8){
								do setDestination(agent(msg.sender).location);
							}else{
								do setDestination(nil);
								return;
							}
						}
						match "vegetarian"{
							if (scene_counter[0]+scene_counter[2] < 25){
								do setDestination(agent(msg.sender).location);
							}
						}
						match "vegan"{
							if (flip(0.5)){
								do setDestination(agent(msg.sender).location);
							}
						}
					}
				}else{
					// If no scene is interesting, go eat food
					do start_conversation to: list(Restaurant) protocol: 'fipa-propose' performative: 'propose' contents:[];	
				} 
				
			} else {
				string restaurant_food_trait <- msg.contents[0];
				int restaurant_crowd <- int(msg.contents[1]);
				
				if (crowd_trait = "introvert"){
					if(restaurant_crowd > 10 and hunger > 400){
						write "" + restaurant_crowd + " destination_reached " + destination_reached;
						do setDestination(agent(msg.sender).location);
						return;
					}
					if(restaurant_crowd > 10){
						do setDestination(nil);
						return;
					}
				}
				
				if hunger > 200{							
					if food_trait = "vegetarian" and restaurant_food_trait = "vegan" and flip(0.5){
						do setDestination(agent(msg.sender).location);
						return;
					}
					if restaurant_food_trait = food_trait {
						do setDestination(agent(msg.sender).location);
					}
				}
			}
		}
	}

	aspect base{
		if(crowd_trait = "introvert"){
			switch food_trait{
				match "meat"{draw steak_mask_shape size: 4;}
				match "vegetarian"{draw egg_mask_shape size: 4;}
				match "vegan"{draw sallad_mask_shape size: 4;}
			}	
		}else{
			switch food_trait{
				match "meat"{draw steak_shape size: 5;}
				match "vegetarian"{draw egg_shape size: 5;}
				match "vegan"{draw sallad_shape size: 5;}
			}
		}
		
	}
}

species Scene skills:[fipa]{
	list<string> music_trait <- 2 among ["rock","disco","pop"];
	
	action setPosition(point pos){
		location <- pos;
	}
	
	reflex newArtist when: time mod 400 = 0{
		let prev <- music_trait;
		music_trait <- 1 among ["rock","disco","pop"];
		write prev + " to " + music_trait;
	}
	
	reflex recieveProposals when: !empty(proposes) {
			loop msg over: proposes {
				
				let counter <- [0,0,0];
				loop neighbor over: (agents_overlapping(self)){
					switch Guest(neighbor).food_trait{
						match "meat"{counter[0] <- counter[0]+1;}
						match "vegetarian"{counter[1] <- counter[1]+1;}
						match "vegan"{counter[2] <- counter[2]+1;}
					}
				}
				do accept_proposal message: msg contents: [music_trait, counter];
				string dummy <- msg.contents;
			}
	}

	aspect base {
		draw scene_shape size: 15;
	}
}

species Restaurant skills:[fipa]{
	string food_trait;
	action setFood(string food){
		food_trait <- food;
	}
	action setPosition(point pos){
		location <- pos;
	}
	
	reflex recieveProposals when: !empty(proposes) {
		loop msg over: proposes {				
			do accept_proposal message: msg contents: [food_trait, length(agents_overlapping(self))];
			string dummy <- msg.contents;
		}
	}
	
	aspect base {
		switch food_trait{
			match "meat" {draw restaurant_shape size: {15,15};}
			match "vegetarian" {draw restaurant_vegetarian_shape size: {15,15};}
			match "vegan" {draw restaurant_vegan_shape size: {15,15};}	
		}
	}
}


experiment Guests type:gui{
	output{
		display guests type: opengl show_fps: true antialias: false{
			image terrain;
			species Scene aspect:base;
			species Restaurant aspect:base;
			species Guest aspect:base;
		}
		display chart{
			chart "utility" type: series {
	            data "utility" value: 1/(introverts_roaming+meat_roaming+1) color: #red;
	        }
		}
		
		monitor "scene_meat" value: scene_meat;
		monitor "scene_vegetarian" value: scene_vegetarian;
		monitor "scene_vegan" value: scene_vegan;
		monitor "scene_total" value: scene_meat+scene_vegetarian+scene_vegan;
		
		monitor "restaurant_meat" value: restaurant_meat;
		monitor "restaurant_vegetarian" value: restaurant_vegetarian;
		monitor "restaurant_vegan" value: restaurant_vegan;
		
		monitor "introverts_roaming" value: introverts_roaming;
		monitor "meat_roaming" value: meat_roaming;
	}
}