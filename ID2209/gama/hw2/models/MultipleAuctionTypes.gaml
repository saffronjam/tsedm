// bonus 2
model MultipleAuctionTypes

global {
	int numberOfBidders <- 5;
	list<string> allItems <- ["tshirt", "hoodie", "socks", "shoes", "hat", "jacket", "cap", "bag"];
	
	init{
		create DutchAuctioneer number: 1;
		create SealedBidAuctioneer number: 1;
		create VickreyAuctioneer number: 1;
		create Bidder number: numberOfBidders;
	}
}

species DutchAuctioneer skills: [fipa]{
	int startPrice <- rnd(900,1000);
	int minPrice <- rnd(200,500);
	int price <- startPrice;
	int rejectCount <- 0;
	Bidder winner <- nil;
	string item <- (1 among allItems)[0];
	string auctionType <- "dutch";
	string prefix <- ' ' + name + ": " + " (" + auctionType + ") ";
	
	
	reflex startAuction when: time = 1{
		write '' + time + prefix + "Begin " + auctionType + " auction";
		do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:[auctionType, item, price];
	}
	
	reflex receive_refuse_messages when: !empty(refuses) {
		loop refuseMsg over: refuses {
			
			int bidderCount <- length(list(Bidder));
			rejectCount <- rejectCount + 1;
			
			if(rejectCount = bidderCount){
				rejectCount <- 0;
				price <- price - 10;
				if(price < minPrice){
					write '' + time + prefix + "Auction ended due to low price";
				}else{
					write '' + time + prefix + "auction of " + item + " restarting at price " + price;
					do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:[auctionType, item, price];	
				}
			}
							
			// Read content to remove the message from proposes variable.
			string dummy <- refuseMsg.contents;
		}
	}
	
	reflex recieveProposals when: !empty(proposes) {
		loop proposeMsg over: proposes {			
			if (winner != nil) {
				write '' + time + prefix + agent(proposeMsg.sender).name + " has not won. Price " + proposeMsg.contents[1];
				do reject_proposal message: proposeMsg contents: [auctionType, item, price];
			} else {
				winner <- agent(proposeMsg.sender);
				write '' + time + prefix + winner.name + " has won at price " + proposeMsg.contents[1];
				do accept_proposal message: proposeMsg contents: [auctionType, item, price];
			}
			
			// Read content to remove the message from proposes variable.
			string dummy <- proposeMsg.contents;
		}
	}
}

species SealedBidAuctioneer skills: [fipa]{
	string item <- (1 among allItems)[0];
	string auctionType <- "sealedbid";
	string prefix <-  ' ' + name + ": " + " (" + auctionType + ") ";
	message highestBidMessage <- nil;
	int highestBid <- 0;
	
	reflex startAuction when: time = 1{
		write '' + time + prefix + "Begin " + auctionType + " auction";
		do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:[auctionType, item];
	}
	
	reflex recieveProposals when: length(proposes) + length(refuses) = numberOfBidders {
		loop proposeMsg over: proposes {	
			int bidPrice <- int(proposeMsg.contents[0]);
			if (bidPrice > highestBid){
				highestBid <- bidPrice;
				highestBidMessage <- proposeMsg;
			}
			
			// Read content to remove the message from proposes variable.
			string dummy <- proposeMsg.contents;
		}
		
		do accept_proposal message: highestBidMessage contents: [auctionType, item, highestBid];
	}
}

species VickreyAuctioneer skills: [fipa]{
	string item <- (1 among allItems)[0];
	string auctionType <- "vickrey";
	string prefix <- ' ' + name + ": " + " (" + auctionType + ") ";
	message highestBidMessage <- nil;
	int highestBid <- 0;
	int secondHighestBid <- 0;
	
	reflex startAuction when: time = 1{
		write '' + time + prefix + "Begin " + auctionType + " auction";
		do start_conversation to: list(Bidder) protocol: 'fipa-contract-net' performative: 'cfp' contents:[auctionType, item];
	}
	
	reflex recieveProposals when: length(proposes) + length(refuses) = numberOfBidders {
		loop proposeMsg over: proposes {	
			int bidPrice <- int(proposeMsg.contents[0]);
			if (bidPrice > highestBid){
				secondHighestBid <- highestBid;
				highestBid <- bidPrice;
				highestBidMessage <- proposeMsg;
			}
			
			// Read content to remove the message from proposes variable.
			string dummy <- proposeMsg.contents;
		}
		
		do accept_proposal message: highestBidMessage contents: [auctionType, item, secondHighestBid];
	}
}


species Bidder skills: [fipa]{
	int maxPrice <- rnd(300,800);
	list<string> interests <- 6 among allItems;
	string prefix <- ' ' + name + ": ";
	
	reflex recieveCalls when: !empty(cfps) {
		loop cfpMsg over: cfps {
			
			list contents <- cfpMsg.contents;
			string auctionType <- contents[0];
			switch auctionType{
				match "dutch"{
					string item <- string(contents at 1);
					int currentPrice <- int(contents at 2);
					
					if(not(item in interests)){
						//write '' + time + prefix + " (" + auctionType + ") " + " did not want the " + item + " because it's not interesting. It only wants " + interests;
						do refuse message: cfpMsg contents: [item];
					}
					else if (currentPrice > maxPrice) {
						//write '' + time + prefix + " (" + auctionType + ") " + " did not want the " + item + " at price " + currentPrice + " because its target is " + maxPrice;
						do refuse message: cfpMsg contents: [item];
					} else {
						do propose message: cfpMsg contents:[item, currentPrice];
					}
				}
				match "sealedbid"{
					string item <- string(contents at 1);
					if(not(item in interests)){
						//write '' + time + prefix + " (" + auctionType + ") " + " did not want the " + item + " because it's not interesting. It only wants " + interests;
						do refuse message: cfpMsg contents: [maxPrice];
					}else{
						do propose message: cfpMsg contents:[maxPrice - rnd(0, int(maxPrice/2))];
					}
				}
				match "vickrey"{
					string item <- string(contents at 1);
					if(not(item in interests)){
						//write '' + time + prefix + " (" + auctionType + ") " + " did not want the " + item + " because it's not interesting. It only wants " + interests;
						do refuse message: cfpMsg contents: [maxPrice];
					}else{
						do propose message: cfpMsg contents:[maxPrice];
					}
				}
			}
			
			
		}
	}
	
	reflex recieveRejectProposals when: !empty(reject_proposals) {
		loop rejectMsg over: reject_proposals {
			
			list contents <- rejectMsg.contents;
			string auctionType <- contents[0];
			switch auctionType{
				match "dutch"{
					string item <- contents at 1;
					int currentPrice <- int(contents at 2);
					write '' + time + prefix + " (" + auctionType + ") " + ' is rejected at price ' + currentPrice;
				}
			}
			
			// Read content to remove the message from reject_proposals variable.
			string dummy <- rejectMsg.contents;
		}
	}
	
	reflex recieveAcceptProposals when: !empty(accept_proposals) {		
		loop acceptMsg over: accept_proposals {
			list contents <- acceptMsg.contents;
			string auctionType <- contents[0];
			switch auctionType{
				match "dutch"{
					string item <- contents at 1;
					int winningPrice <- int(contents at 2);
					write '' + time + prefix + " (" + auctionType + ") " + ' has won ' + item + ' for price ' + winningPrice;
					do inform message: acceptMsg contents:["Inform from " + name];
				}
				match "sealedbid"{
					string item <- contents at 1;
					int winningPrice <- int(contents at 2);
					write '' + time + prefix + " (" + auctionType + ") " + ' has won ' + item + ' for price ' + winningPrice;
				}
				match "vickrey"{	
					string item <- contents at 1;
					int winningPrice <- int(contents at 2);
					write '' + time + prefix + " (" + auctionType + ") " + ' has won ' + item + ' for price ' + winningPrice;
				}
			}
			
			// Read content to remove the message from accept_proposals variable.
			string dummy <- acceptMsg.contents;				
		}
	}
}
experiment multipleAuctionTypesFestival {}




