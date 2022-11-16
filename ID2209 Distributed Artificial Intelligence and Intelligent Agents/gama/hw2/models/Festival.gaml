model Festival

global {
	int numberOfBidders <- 5;
	
	init{
		create Auctioneer;
		create Bidder number: numberOfBidders;
	}
}

species Auctioneer skills: [fipa]{
	int startPrice <- 2000;
	int minPrice <- 200;
	int price <- startPrice;
	int rejectCount <- 0;
	Bidder winner <- nil;
	
	reflex startAuction when: time = 1{
		write "Auctioneer: Begin auction";
		do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:["tshirt", price];
	}
	
	reflex receive_refuse_messages when: !empty(refuses) {
		loop refuseMsg over: refuses {
			write '(Time ' + time + '): ' + agent(refuseMsg.sender).name + ' refused.';
			
			
			int bidderCount <- length(list(Bidder));
			rejectCount <- rejectCount + 1;
			write ("Reject count " + rejectCount + " of " + bidderCount);
			
			if(rejectCount = bidderCount){
				write "auction restarting";
				rejectCount <- 0;
				price <- price - 10;
				if(price < minPrice){
					write("Auction ended due to low price");
				}else{
					do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:["tshirt", price];	
				}
			}
							
			// Read content to remove the message from proposes variable.
			string dummy <- refuseMsg.contents;
		}
	}
	
	reflex recieveProposals when: !empty(proposes) {
		loop proposeMsg over: proposes {			
			if (winner != nil) {
				write("Auctioneer: " + agent(proposeMsg.sender).name + " has not won. Price " + proposeMsg.contents[1]);
				do reject_proposal message: proposeMsg contents: ['tshirt', price, 'no win'];
			} else {
				winner <- agent(proposeMsg.sender);
				write("Auctioneer: " + winner.name + " has won at price " + proposeMsg.contents[1]);
				do accept_proposal message: proposeMsg contents: ['tshirt', price, 'win'];
			}
			
			
			// Read content to remove the message from proposes variable.
			string dummy <- proposeMsg.contents;
		}
	}
}

species Bidder skills: [fipa]{
	int maxPrice <- rnd(100,100);
	
	reflex recieveCalls when: !empty(cfps) {
		loop cfpMsg over: cfps {
			write '(Time ' + time + '): ' + name + ' receives a cfp message from ' + agent(cfpMsg.sender).name + ' with content: ' + cfpMsg.contents;
			list contents <- cfpMsg.contents;
			int currentPrice <- int(contents at 1);
			
			if (currentPrice < maxPrice) {
				do propose message: cfpMsg contents:["tshirt", currentPrice];
			} else {
				write(name + " did not want the " + cfpMsg.contents[0] + " at price " + cfpMsg.contents[1] + " because its price is " + maxPrice);
				do refuse message: cfpMsg contents: ["tshirt"];
			}
			
		}
	}
	
	reflex recieveRejectProposals when: !empty(reject_proposals) {
		write('reject_proposals');		
		loop rejectMsg over: reject_proposals {
			write '(Time ' + time + '): ' + name + ' is rejected. at price ' + rejectMsg.contents[1];
			
			// Read content to remove the message from reject_proposals variable.
			string dummy <- rejectMsg.contents[0];
		}
	}
	
	reflex recieveAcceptProposals when: !empty(accept_proposals) {		
		write('accept_proposals');		
		loop acceptMsg over: accept_proposals {
			write '(Time ' + time + '): ' + name + ' is accepted. for price ' + acceptMsg.contents[1];
			do inform message: acceptMsg contents:["Inform from " + name];
			// Read content to remove the message from accept_proposals variable.
			string dummy <- acceptMsg.contents[0];
		}
	}
}
experiment festival {}




