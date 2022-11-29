/**
* Name: CrowdHighestUtility
* Based on the internal empty template. 
* Author: pierrelf
* Tags: 
*/


model CrowdHighestUtility

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
		
		Person leader <- Person[0];
		let temp <- leader.becomeLeader();
	}
	
}

species Scene skills:[fipa]{
	float lightShow <- rnd(0.0,1.0);
	float speakers <- rnd(0.0,1.0);
	float band <- rnd(0.0,1.0);
	
	action setPosition(point pos){
		location <- pos;
	}
	
	reflex receiveRequests when: not empty(requests){
		loop msg over: requests{
			do agree message: msg contents: [lightShow, speakers, band];
			do inform message: msg contents: [];
			
			let discard <- msg.contents;
		}	
	}
	
	reflex artistChange when: flip(0.005){
		lightShow <- rnd(0.0,1.0);
		speakers <- rnd(0.0,1.0);
		band <- rnd(0.0,1.0);
	}
	
	aspect base {
		rgb agentColor <- rgb(lightShow*255,speakers*255,band*255,0.5);
		draw square(15) color: agentColor;
	}
}

species Person skills:[fipa, moving]{
	float lightShow <- rnd(0.0,1.0);
	float speakers <- rnd(0.0,1.0);
	float band <- rnd(0.0,1.0);
	float crowdMass <- rnd(0.0,1.0);
	bool leader <- false;
	
	float maxUtility <- 0.0;
	Scene bestScene <- nil;
	Scene candidateScene <- nil;
	int sceneCounter <- 0;
	Scene leaderScene <- nil;
	
	action becomeLeader{
		leader <- true;
		crowdMass <- 0.0;
	}
	
	reflex informDestination when: leader{
		do start_conversation to: list(Person) protocol: 'no-protocol' performative:'propose' contents:[bestScene];
	}
	
	reflex receivePropose when: not empty(proposes){
		loop msg over: proposes{
			
			if(not leader){
				leaderScene <- Scene(msg.contents[0]);
				if(crowdMass > 0.3){
					bestScene <- leaderScene;
				}	
			}
			
			let discard <- msg.contents;
			do end_conversation message: msg contents: [];
		}
	}
	
	reflex askScenesWassup when: flip(0.1){
		do start_conversation to: list(Scene) protocol: 'fipa-request' performative:'request' contents:[];
	}
	
	reflex discardInforms when: not empty(informs){
		loop msg over: informs{
			let discard <- msg.contents;
		}	
	}
	
	reflex receiveAgrees when: not empty(agrees){
		loop msg over: agrees{
			sceneCounter <- sceneCounter + 1;
			if (sceneCounter > numberOfScenes){
				sceneCounter <- 0;
				maxUtility <- 0.0;
			}
			
			float sceneLightShow <- float(msg.contents[0]);
			float sceneSpeakers <- float(msg.contents[1]);
			float sceneBand <- float(msg.contents[2]);
			
			float sceneUtility <- lightShow*sceneLightShow + speakers*sceneSpeakers + band*sceneBand;
			
			if(sceneUtility > maxUtility){
				if(crowdMass < 0.5 and Scene(msg.sender) != leaderScene){
					candidateScene <- Scene(msg.sender);
					maxUtility <- sceneUtility; 	
				}
			}
			
			if(sceneCounter = numberOfScenes){
				bestScene <- candidateScene;
			}
			
			let discard <- msg.contents;
		}
	}
	
	reflex dance when: bestScene != nil and self distance_to bestScene < 5{
		do wander;
	}
	
	reflex walkToScene when: bestScene != nil and self distance_to bestScene >= 5{
		do goto target: bestScene;
	}

	aspect base{
		if (leader){
			rgb agentColor <- rgb("black");
			draw circle(3) color: agentColor;
		}else{
			rgb agentColor <- rgb(lightShow*255,speakers*255,band*255,0.5);
			draw circle(1) color: agentColor;
		}

	}
}


experiment CrowdHighestUtility type:gui{
	output{
		display myDisplay {
			species Person aspect:base;
			species Scene aspect:base;
		}
	}
}
