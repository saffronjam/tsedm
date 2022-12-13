model Guests

/* Insert your model definition here */

// Chart (series)
// https://gama-platform.org/wiki/DefiningCharts#various-types-of-charts
global {
	int num_guests <- 10;
	int num_scenes <- 5;
	int num_restaurants <- 1;
	
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
		}
		
		loop counter from: 0 to: num_scenes - 1{
			Scene scene <- Scene[counter];
			let spacing <- 100/num_scenes;
			let temp <- scene.setPosition({spacing * counter + spacing / 2, 90});
		}
	}
	
	//string mine_at_location <- "mine_at_location";
	string hype_scenes <- "hype_scenes";
	
	//string empty_mine_location <- "empty_mine_location";
	string no_longer_hype <- "no_longer_hype";
	
	//predicate mine_location <- new_predicate(mine_at_location) ;
	predicate scene_found <- new_predicate(hype_scenes);
	
	//predicate choose_gold_mine <- new_predicate("choose a gold mine");
    predicate choose_hype_scene <- new_predicate("choose a hype scene");
    
    //predicate has_gold <- new_predicate("extract gold");
    predicate dance_at_scene <- new_predicate("dance_at_scene");
    predicate is_hungry <- new_predicate("is_hungry");    
    
    //predicate find_gold <- new_predicate("find gold") ;
    predicate find_hype <- new_predicate("find hype");
    
    //predicate sell_gold <- new_predicate("sell gold") ;
    predicate eat_food <- new_predicate("eat_food");   
    
    
    reflex newArtists when: time mod 400 = 0{
    	list<Scene> hyped <- 2 among Scene;
    	
    	loop scene over: hyped{
    		let tmp <- scene.setHype(true);
    	}
    	loop scene over: Scene-hyped{
    		let tmp <- scene.setHype(false);
    	}
	}
    
}

species Guest skills:[moving] control: simple_bdi{
	point target <- nil;
	Restaurant the_restaurant <- Restaurant[0];
	int food_eaten <- 0;
	float view_dist <- 100.0;
	int hunger <- 0;
	
	init {
    	do add_desire(find_hype);
    }
    
    reflex becomeHungry{
    	hunger <- hunger + rnd(0,1);
    }
    
    reflex isHungry when: hunger > 500 {
		do add_belief(is_hungry);
    }
    
    perceive target: Scene where (each.hype) in: view_dist {
	    focus id: hype_scenes var:location;
	    ask myself {
	        do remove_intention(find_hype, false);
	    }
    }
    
    rule belief: scene_found new_desire: dance_at_scene strength: 2.0;
    rule belief: is_hungry new_desire: eat_food strength: 3.0;
  
	plan lets_wander intention: find_hype {
		do wander;
	}
	
    plan goto_scene intention: dance_at_scene {
	    if (target = nil) {
	        do add_subintention(get_current_intention(), choose_hype_scene, true);
	        do current_intention_on_hold();
	    } else {
	        do goto target: target ;
	        if (target = location)  {
		        Scene current_scene <- Scene first_with (target = each.location);
		        if current_scene.hype {
		            //do add_belief(dance_at_scene);
		            //ask current_mine {quantity <- quantity - 1;}    
		        } else {
		            do remove_belief(new_predicate(hype_scenes, ["location_value"::target]));
		        }
		        target <- nil;
	        }
	    }   
    }
    
    plan choose_best_scene intention: choose_hype_scene instantaneous: true {
	    list<point> possible_scenes <- get_beliefs_with_name(hype_scenes) collect (point(get_predicate(mental_state (each)).values["location_value"]));
	    write length(possible_scenes);
	    if (empty(possible_scenes)) {
	        do remove_intention(dance_at_scene, true); 
	    } else {
	        target <- (possible_scenes with_min_of (each distance_to self)).location;
	    }
	    do remove_intention(choose_hype_scene, true); 
    }
    
    plan goto_food intention: eat_food {
	    do goto target: the_restaurant;
	    if (the_restaurant.location = location)  {
	        do remove_belief(is_hungry);
	        do remove_intention(eat_food, true);
	        food_eaten <- food_eaten + 1;
	    	hunger <- 0;
	    }
    }

    

	aspect base{
		if(hunger > 500){
			draw steak_shape size: hunger/100;
		}else{
			draw sallad_mask_shape size: 4;
		}
	}
}

species Scene skills:[fipa]{
	bool hype;
	
	action setPosition(point pos){
		location <- pos;
	}
	
	action setHype(bool h){
		hype <- h;
	}

	aspect base {
		if(hype){
			draw scene_shape size: 25;
		}else{
			draw scene_shape size: 10;
		}
	}
}

species Restaurant{
	action setPosition(point pos){
		location <- pos;
	}
	
	aspect base {
		draw restaurant_shape size: {15,15};
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
	}
}