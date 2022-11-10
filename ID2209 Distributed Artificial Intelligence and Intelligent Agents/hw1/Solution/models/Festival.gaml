model Festival

global {
	int numberOfPeople <- 10;
	int numberOfStores <- 4;
	int distanceThreshold <- 2;
	
	init {
		create Person number:numberOfPeople;
		
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
        
        }
	}
}


species Person skills:[moving]
{
	int hunger <- 0;
	int maxHunger <- rnd(500, 1000);
	int thirst <- 0;
	int maxThirst <- rnd(500, 1000);
	point targetPoint <- nil;
	string personName <- "Undefined";
	point infoCenterPoint <- nil;
	
	action setName(int num) {
		personName <- "Person " + num;
	}
	
	action setInfoCenterPoint(point infoCenter){
		infoCenterPoint <- infoCenter;
	}
	
	aspect base {
		rgb agentColor <- rgb("lightgray");
		
		if (hunger > maxHunger and thirst > maxThirst) {
			agentColor <- rgb("purple");
		} else if (thirst > maxThirst) {
			agentColor <- rgb("blue");
		} else if (hunger > maxHunger) {
			agentColor <- rgb("red");
		} 
				
		draw circle(1) color: agentColor;
	}
	
	reflex lookForInfoCenter when: (hunger > maxHunger or thirst > maxThirst) and targetPoint = nil 
	{
		write personName + " going to info center";
		targetPoint <- infoCenterPoint; // Go to info center
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
		ask InfoCenter at_distance distanceThreshold {
			write myself.personName + " is at info center";
			if(myself.thirst > myself.maxThirst){
				myself.targetPoint <- self.findBar();	
				write myself.personName + " going to bar at " + myself.targetPoint;
			}
			else if(myself.hunger > myself.maxHunger){
				myself.targetPoint <- self.findRestaurant();
				write myself.personName + " going to restaurant at " + myself.targetPoint;
			}
			else {
				myself.targetPoint <- nil;
			}
		}
	}
	
	reflex enterStore when: targetPoint != nil 
		and location distance_to(targetPoint) < 2 
		and !empty(Store at_distance distanceThreshold) {
		ask Store at_distance distanceThreshold {
			if (self.hasFood){
				myself.hunger <- 0;
				myself.targetPoint <- nil;
				write myself.personName + " eating at " + self.storeName;
			} else {
				myself.thirst <- 0;
				myself.targetPoint <- nil;
				write myself.personName + " drinking at " + self.storeName;
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

species InfoCenter {
	list<Store> restaurants <- [];
	list<Store> bars <- [];
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
	
	
	action setName(int s){
		write "message " + s;
	}
	
	point findBar{
		write "restaurants " + restaurants + " bars " + bars;	
		return (1 among bars)[0].location;
	}
	
	point findRestaurant{
		
		write "restaurants " + restaurants + " bars " + bars;	
		return (1 among restaurants)[0].location;
	}
	
	aspect base {
		rgb agentColor <- rgb("black");
		draw triangle(2) color: agentColor;
	}
}

experiment festival type:gui {
	output {
		display myDisplay {
			species Person aspect:base;
			species Store aspect:base;
			species InfoCenter aspect:base;
		}
	}
}