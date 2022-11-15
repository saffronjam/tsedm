model Festival

global {
	int numberOfPeople <- 10;
	int numberOfStores <- 4;
	int distanceThreshold <- 2;
	
	init {
		create Person number:numberOfPeople;
		create SecurityGuard number:1;
		
		point infoCenterPosition <- {rnd(0, 100), rnd(0, 100)};
				
		create Store number:numberOfStores;
		create InfoCenter number:1;
				
		loop counter from: 1 to: numberOfPeople {
        	Person person <- Person[counter - 1];
        	person <- person.setName(counter);
        	
        	person <- Person[counter - 1];
        	person <- person.setInfoCenterPoint(infoCenterPosition);
        }
        		
		loop counter from: 1 to: numberOfStores {
        	Store store <- Store[counter - 1];
			store <- store.setName(counter);
				
			store <- Store[counter - 1];
        	bool type <- flip(0.5);
        	store <- store.setType(type);
        		
			store <- Store[counter - 1];
			InfoCenter infoCenter <- InfoCenter[0];
        	infoCenter <- infoCenter.addStore(store);
        	
        	infoCenter <- InfoCenter[0];
        	infoCenter <- infoCenter.setPosition(infoCenterPosition);
        	
			SecurityGuard securityGuard <- SecurityGuard[0];
        	infoCenter <- InfoCenter[0];
        	infoCenter <- infoCenter.setGuard(securityGuard);
        }
	}
}


species Person skills:[moving]
{
	int hunger <- 0;
	int maxHunger <- rnd(170, 220);
	int thirst <- 0;
	int maxThirst <- rnd(170, 220);
	point targetPoint <- nil;
	string personName <- "Undefined";
	point infoCenterPoint <- nil;
	
	// Challenge 1: Small brain
	// Remember the last shop points, and when hungry or thirsty
	// decide whether to go to the last shop or ask for a new one
	// However, to easily compare we only add this brain to roughly half the agents
	point lastRestaurant <- nil;
	point lastBar <- nil;	
	bool hasBrain <- flip(0.5);
	float distanceTraveled <- 0.0;
	
	// Challenge 2: Bad actors
	// Some actors are born to be bad and must therefore be killed
	bool isBad <- flip(0.5);
	
	action setName(int num) {
		personName <- "Person " + num;
	}
	
	action setInfoCenterPoint(point infoCenter){
		infoCenterPoint <- infoCenter;
	}
	
	aspect base {
		rgb agentColor <- rgb(hunger, 0, thirst);
	
		if (isBad){
			agentColor <- rgb("orange");
		}
				
		draw circle(1) color: agentColor;
	}
	
	// Challenge 1
	reflex makeStoreDecision when: (hunger > maxHunger or thirst > maxThirst) and targetPoint = nil {
		bool goToNewStore <- flip(0.1);
		if (goToNewStore or !hasBrain) {
			targetPoint <- infoCenterPoint;
		} else {
			if (hunger > maxHunger) {
				targetPoint <- lastRestaurant;
			} else {
				targetPoint <- lastBar;
			}
		}
		
		if (targetPoint != nil) {
			distanceTraveled <- distanceTraveled + location distance_to targetPoint;
			write personName + " travelled " + int(distanceTraveled) + " (" + hasBrain + ") ";
		}
		
	}
	
	reflex beIdle when: targetPoint = nil
	{ 
		thirst <- thirst + rnd(1,5);
		hunger <- hunger + rnd(1,3);
		do wander;
	}
	
	reflex moveToTarget when: targetPoint != nil
	{
		do goto target:targetPoint;
	}
	
	reflex getInfo when: targetPoint != nil 		
		and location distance_to(targetPoint) < 2 
		and !empty(InfoCenter at_distance distanceThreshold) {
		
		// Info center reached
		ask InfoCenter at_distance distanceThreshold {
			if(myself.thirst > myself.maxThirst){
				myself.targetPoint <- self.findBar();	
				myself.lastBar <- myself.targetPoint;
			}
			else if(myself.hunger > myself.maxHunger){
				myself.targetPoint <- self.findRestaurant();
				myself.lastRestaurant <- myself.targetPoint;
			}
			else {
				myself.targetPoint <- nil;
			}
		}
		
	}
	
	reflex enterStore when: targetPoint != nil 
		and location distance_to(targetPoint) < 2 
		and !empty(Store at_distance distanceThreshold) {
			
		// Store reached 
		ask Store at_distance distanceThreshold {
			if (self.hasFood){
				myself.hunger <- 0;
				myself.targetPoint <- nil;
			} else {
				myself.thirst <- 0;
				myself.targetPoint <- nil;
			}
		}
	}
}

species Store {
	bool hasFood <- false;
	string storeName <- "Undefined";
	
	action setName(int num) {
		storeName <- "Store " + num;
	}
	
	action setType(bool type) {
		hasFood <- type;
	}
	
	aspect base {
		rgb agentColor <- rgb("lightgray");
		
		if (hasFood){
			agentColor <- rgb("red");
		} else {
			agentColor <- rgb("blue");
		}
		
		draw square(2) color: agentColor;
	}
}

species SecurityGuard skills:[moving] {
	Person target;
	
	action setTarget(Person t) {
		target <- t;
	}
	
	aspect base {
		rgb agentColor <- rgb("green");
		draw triangle(2) color: agentColor;
	}
	
	reflex moveToTarget when: target != nil
	{
		do goto target:target;
	}
	
	reflex killBadActor when: !empty(Person at_distance distanceThreshold) {
		ask Person at_distance distanceThreshold {
			if(self.name = myself.target.name){
				write "Bad actor killed " + self.name;
				let res <- self.die();
			}
		}	
	}
}

species InfoCenter {
	list<Store> restaurants <- [];
	list<Store> bars <- [];
	SecurityGuard securityGuard;
	init{
		location <- {0,0};
	}
	
	action addStore(Store store) {
		if(store.hasFood){
			restaurants <- restaurants + store;
		} else {
			bars <- bars + store;
		}
	}
	
	action setPosition(point pos){
		location <- pos;
	}
	
	action setGuard(SecurityGuard guard){
		securityGuard <- guard;
	}
	
	point findBar{
		return (1 among bars)[0].location;
	}
	
	point findRestaurant{
		return (1 among restaurants)[0].location;
	}
	
	aspect base {
		rgb agentColor <- rgb("black");
		draw triangle(2) color: agentColor;
	}

	reflex discoverBadActor when: !empty(Person at_distance distanceThreshold) {
		ask Person at_distance distanceThreshold {
			if(self.isBad){
				write "Bad actor found " + self.name;
				let res <- myself.securityGuard.setTarget(self);
			}
		}	
	}
}

experiment festival type:gui {
	output {
		display myDisplay {
			species Person aspect:base;
			species Store aspect:base;
			species InfoCenter aspect:base;
			species SecurityGuard aspect:base;
		}
	}
}