/**
* Name: generatesocialnetwork
* Based on the internal empty template. 
* Author: loic
* Tags: 
*/


model innodiffarg

import "Individual.gaml"

global{
	
	action generateSocialNetwork(list<Individual> individuals_to_connect, int K, float beta){
		map<Individual,list<Individual>> links;
		int pop_size <- length(individuals_to_connect);
		
		//generating base lattice according to K
		loop i from:0 to: pop_size-1{
			links[individuals_to_connect[i]] <- getNeighbors(individuals_to_connect,K,i);
		}
		
		//rewiring
		loop times: int(pop_size*beta){
			Individual i <- one_of(Individual);
			Individual indiv2remove <- one_of(links[i]);
			Individual indiv2connect <- one_of(Individual);
			loop while:(indiv2connect = i) or (links[i] contains indiv2connect){
				indiv2connect <- one_of(Individual);
			}
			remove indiv2remove from: links[i];
			links[i] << indiv2connect;
		}
		// attributing relatives
		ask Individual{
			relatives <- links[self];
		}
	}

	list<Individual> getNeighbors(list<Individual> individuals_to_connect, int K, int index){
		list<Individual> neighbors <- [];
		int pop_size <- length(individuals_to_connect);
		
		loop i from: 1 to: int(K/2){
			if (index + i > pop_size -1){
				neighbors << individuals_to_connect[index + i - pop_size];
			}else{
				neighbors << individuals_to_connect[index + i];
			}
			if (index - i < 0){
				neighbors << individuals_to_connect[index - i + pop_size];
			}else{
				neighbors << individuals_to_connect[index - i];
			}
		}
		return neighbors;
	}
	
}