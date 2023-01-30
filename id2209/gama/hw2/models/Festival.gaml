// Bonus 1 and basic assignment
model Festival

global {
	int numberOfBidders <- 5;
	int numberOfAuctioneers <- 3;
	list<string> allItems <- ["tshirt", "hoodie", "socks", "shoes", "hat", "jacket", "cap", "bag"];
	
	init{
		create Auctioneer number: numberOfAuctioneers;
		create Bidder number: numberOfBidders;
	}
}

species Auctioneer skills: [fipa]{
	int startPrice <- rnd(900,1000);
	int minPrice <- rnd(200,500);
	int price <- startPrice;
	int rejectCount <- 0;
	Bidder winner <- nil;
	string item <- (1 among allItems)[0];
	
	reflex startAuction when: time = 1{
		write '' + time + " " + name + ": Begin auction";
		do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:[item, price];
	}
	
	reflex receive_refuse_messages when: !empty(refuses) {
		loop refuseMsg over: refuses {
			
			int bidderCount <- length(list(Bidder));
			rejectCount <- rejectCount + 1;
			//write '' + time + " " + name + ": " + agent(refuseMsg.sender).name + " refused. " + rejectCount + "/" + bidderCount + " rejected";
			
			if(rejectCount = bidderCount){
				rejectCount <- 0;
				price <- price - 10;
				if(price < minPrice){
					write('' + time + " " + name + ": Auction ended due to low price");
				}else{
					write '' + time + " " + name + ": auction of " + item + " restarting at price " + price;
					do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:[item, price];	
				}
			}
							
			// Read content to remove the message from proposes variable.
			string dummy <- refuseMsg.contents;
		}
	}
	
	reflex recieveProposals when: !empty(proposes) {
		loop proposeMsg over: proposes {			
			if (winner != nil) {
				write('' + time + " " + name + ": " + agent(proposeMsg.sender).name + " has not won. Price " + proposeMsg.contents[1]);
				do reject_proposal message: proposeMsg contents: [item, price, 'no win'];
			} else {
				winner <- agent(proposeMsg.sender);
				write('' + time + " " + name + ": " + winner.name + " has won at price " + proposeMsg.contents[1]);
				do accept_proposal message: proposeMsg contents: [item, price, 'win'];
			}
			
			
			// Read content to remove the message from proposes variable.
			string dummy <- proposeMsg.contents;
		}
	}
}

species Bidder skills: [fipa]{
	int maxPrice <- rnd(300,800);
	list<string> interests <- 3 among allItems;
	
	reflex recieveCalls when: !empty(cfps) {
		loop cfpMsg over: cfps {
			//write '' + time + ' ' + name + ' receives a cfp message from ' + agent(cfpMsg.sender).name + ' with content: ' + cfpMsg.contents;
			list contents <- cfpMsg.contents;
			int currentPrice <- int(contents at 1);
			string item <- string(contents at 0);
			
			if(not(item in interests)){
				write('' + time + ' ' + name + " did not want the " + cfpMsg.contents[0] + " because it's not interesting. It only wants " + interests);
				do refuse message: cfpMsg contents: [item];
			}
			else if (currentPrice > maxPrice) {
				write('' + time + ' ' + name + " did not want the " + cfpMsg.contents[0] + " at price " + cfpMsg.contents[1] + " because its price is " + maxPrice);
				do refuse message: cfpMsg contents: [item];
			} else {
				do propose message: cfpMsg contents:[item, currentPrice];
			}
			
		}
	}
	
	reflex recieveRejectProposals when: !empty(reject_proposals) {
		loop rejectMsg over: reject_proposals {
			write '' + time + ' ' + name + ' is rejected at price ' + rejectMsg.contents[1];
			
			// Read content to remove the message from reject_proposals variable.
			string dummy <- rejectMsg.contents[0];
		}
	}
	
	reflex recieveAcceptProposals when: !empty(accept_proposals) {		
		loop acceptMsg over: accept_proposals {
			write '' + time + ' ' + name + ' has won for price ' + acceptMsg.contents[1];
			do inform message: acceptMsg contents:["Inform from " + name];
			// Read content to remove the message from accept_proposals variable.
			string dummy <- acceptMsg.contents[0];
		}
	}
}
experiment festival {}




