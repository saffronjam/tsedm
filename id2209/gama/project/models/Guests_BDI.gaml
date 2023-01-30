model Guests

/* Insert your model definition here */

// Chart (series)
// https://gama-platform.org/wiki/DefiningCharts#various-types-of-charts
global {
	int num_guests <- 10;
	int num_scenes <- 5;
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
		create Restaurant number:num_restaurants;
		create Scene number:num_scenes;
		create Guest number:num_guests;
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
	
	//string mine_at_location <- "mine_at_location";
	string hype_scenes <- "hype_scenes";
	string good_restaurants <- "good_restaurants";
	
	//string empty_mine_location <- "empty_mine_location";
	string no_longer_hype <- "no_longer_hype";
	
	//predicate mine_location <- new_predicate(mine_at_location) ;
	predicate scene_found <- new_predicate(hype_scenes);
	
	//predicate choose_gold_mine <- new_predicate("choose a gold mine");
    predicate choose_hype_scene <- new_predicate("choose a hype scene");
    predicate choose_restaurant <- new_predicate("choose_restaurant");
    
    //predicate has_gold <- new_predicate("extract gold");
    predicate dance_at_scene <- new_predicate("dance_at_scene");
    predicate is_hungry <- new_predicate("is_hungry");    
    
    //predicate find_gold <- new_predicate("find gold") ;
    predicate find_hype <- new_predicate("find hype");
    predicate find_food <- new_predicate("find_food");
    
    //predicate sell_gold <- new_predicate("sell gold") ;
    predicate eat_food <- new_predicate("eat_food");   
    
}

species Guest skills:[moving] control: simple_bdi{
	string food_trait <- (1 among ["meat","vegetarian","vegan"])[0];
	string music_trait <- (1 among ["rock","disco","pop"])[0];
	point target <- nil;
	float view_dist <- 1000.0;
	int hunger <- 0;
	int hunger_threshold <- 300;
	
	init {
    	do add_desire(find_hype);
    }
    
    reflex becomeHungry{
    	hunger <- hunger + rnd(0,3);
    }
    
    reflex isHungry when: hunger > hunger_threshold {
		do add_belief(is_hungry);
    }
    
    perceive target: Scene where (each.music_trait = music_trait) in: view_dist {
	    focus id: hype_scenes var:location;
	    ask myself {
	        do remove_intention(find_hype, false);
	    }
    }
    
    perceive target: Restaurant where (each.food_trait = food_trait) in: view_dist {
	    focus id: good_restaurants var:location;
	    ask myself {
	        do remove_intention(find_food, false);
	    }
    }
    
    rule belief: scene_found new_desire: dance_at_scene strength: 2.0;
    rule belief: is_hungry new_desire: eat_food strength: 3.0;
  
	plan find_scene intention: find_hype {
		do wander amplitude:0.1 speed:1.0;
	}
	
	plan find_restaurant intention: find_food {
		do wander amplitude:0.1 speed:1.0;
	}
	
    plan goto_scene intention: dance_at_scene {
	    if (target = nil) {
	        do add_subintention(get_current_intention(), choose_hype_scene, true);
	        do current_intention_on_hold();
	    } else {
	        do goto target: target ;
	        if (target = location)  {
		        Scene current_scene <- Scene first_with (target = each.location);
		        if current_scene.music_trait != music_trait {
		            do remove_belief(new_predicate(hype_scenes, ["location_value"::target]));
		        }
		        target <- nil;
	        }
	    }   
    }
    
    plan choose_best_scene intention: choose_hype_scene instantaneous: true {
	    list<point> possible_scenes <- get_beliefs_with_name(hype_scenes) collect (point(get_predicate(mental_state (each)).values["location_value"]));
	    if (empty(possible_scenes)) {
	        do remove_intention(dance_at_scene, true); 
	    } else {
	        target <- (possible_scenes with_min_of (each distance_to self)).location;
	    }
	    do remove_intention(choose_hype_scene, true); 
    }
    
	plan goto_food intention: eat_food {
	    if (target = nil) {
	        do add_subintention(get_current_intention(), choose_restaurant, true);
	        do current_intention_on_hold();
	    } else {
	        do goto target: target ;
	        if (target = location){
	    		hunger <- 0;
	    		do remove_belief(is_hungry);
	        	do remove_intention(eat_food, true); 
		        //Scene current_scene <- Scene first_with (target = each.location);
		        //if not current_scene.hype {
		        //    do remove_belief(new_predicate(hype_scenes, ["location_value"::target]));
		        //}
		        target <- nil;
	        }
	    }   
    }
    
    plan choose_restaurant intention: choose_restaurant {
	    list<point> possible_restaurants <- get_beliefs_with_name(good_restaurants) collect (point(get_predicate(mental_state (each)).values["location_value"]));
	    if(empty(possible_restaurants)){
	        do remove_intention(eat_food, true); 
	    }else{
	        target <- (possible_restaurants with_min_of (each distance_to self)).location;
	    }
	    do remove_intention(choose_restaurant, true);
    }

	aspect base{
		switch food_trait{
			match "meat"{draw steak_shape size: 5;}
			match "vegetarian"{draw egg_shape size: 5;}
			match "vegan"{draw sallad_shape size: 5;}
		}
		
		if(hunger > hunger_threshold){
			draw "hungry " + hunger size: 10 color: #black font:font("Helvetica", 20 , #plain) border:#red;
		}else{
			draw music_trait size: 10 color: #black font:font("Helvetica", 20 , #plain) border:#red;
		}	
	}
}

species Scene skills:[fipa]{
	string music_trait <- (1 among ["rock","disco","pop"])[0];
	
	action setPosition(point pos){
		location <- pos;
	}
	
	reflex newArtists when: time mod 200 = 0{
		music_trait <- (1 among ["rock","disco","pop"])[0];
	}
	
	aspect base {
		draw scene_shape size: 15;
			draw music_trait size: 10 color: #black font:font("Helvetica", 20 , #plain) border:#red;
	}
}

species Restaurant{
	string food_trait;
	action setFood(string food){
		food_trait <- food;
	}
	
	action setPosition(point pos){
		location <- pos;
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
		display guests {
			image terrain;
			species Scene aspect:base;
			species Restaurant aspect:base;
			species Guest aspect:base;
		}
	}
}