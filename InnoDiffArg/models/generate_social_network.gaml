/**
* Name: generatesocialnetwork
* Based on the internal empty template. 
* Author: loic
* Tags: 
*/


model innodiffarg

import "Individual.gaml"

global{
	
	action generateSmallWorldSocialNetwork(list<Individual> individuals_to_connect, int K, float beta){
		map<Individual,list<Individual>> links;
		int pop_size <- length(individuals_to_connect);
		
		// this part computes agents locations to form a lattice ring
		float inter <- 2 * #pi / pop_size;
		list<point> indiv_locations <- [];
		float x <- 0.0;
		loop times: pop_size {
			indiv_locations << point(cos_rad(x)*80,sin_rad(x)*80);
			x <- x +  inter;
		}
		
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
		// attributing relatives and location on the ring
		int cpt <- 0;
		ask Individual{
			relatives <- links[self];
			location <- indiv_locations[cpt];
			cpt <- cpt+1;
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
	
	action generateRegularSocialNetwork(list<Individual> individuals_to_connect, string neighborhood_type){
		float grid_width <- ceil(sqrt(length(individuals_to_connect)));
		list<list<Individual>> individuals_grid <- [];
		int x <- 0;
		int y <- 0;
		loop indiv over: individuals_to_connect{
			if x = 0{
				individuals_grid << [indiv];
			}else{
				individuals_grid[y] << indiv;
			}
			ask indiv{
				location <- point(x*10,y*10);
				if y = 0 {
					if x > 0{
						relatives << individuals_grid[y][x-1];
					}
				}else{
					if x = 0{
						relatives << individuals_grid[y-1][x];
						if neighborhood_type = "Moore"{ 
							relatives << individuals_grid[y-1][x+1];
						}
					}else{
						relatives <- relatives + [individuals_grid[y-1][x],individuals_grid[y][x-1]];
						if neighborhood_type = "Moore"{
							relatives << individuals_grid[y-1][x-1];
							if x < grid_width-1{relatives << individuals_grid[y-1][x+1];}
						}
					}
				}
				ask relatives{
					relatives << myself;
				}
			}
			if x = grid_width - 1{
				x <- 0;
				y <- y+1;
			}else{
				x <- x+1;	
			}
		}
	}
	
	action generateRandomSocialNetwork(list<Individual> individuals_to_connect, int mean_arity){
		// this part computes agents locations to form a lattice ring
		int pop_size <- length(individuals_to_connect);
		float inter <- 2 * #pi / pop_size;
		list<point> indiv_locations <- [];
		float x <- 0.0;
		loop times: pop_size {
			indiv_locations << point(cos_rad(x)*80,sin_rad(x)*80);
			x <- x +  inter;
		}
	}
	
	action generateScaleFreeSocialNetwork(list<Individual> individuals_to_connect, int K, int m0){
		map<Individual,int> degree;
		map<Individual,list<Individual>> links;
		int nb_link <- 0;
		// this part computes agents locations to form a lattice ring
		int pop_size <- length(individuals_to_connect);
		float inter <- 2 * #pi / pop_size;
		list<point> indiv_locations <- [];
		float x <- 0.0;
		loop times: pop_size {
			indiv_locations << point(cos_rad(x)*80,sin_rad(x)*80);
			x <- x +  inter;
		}
		
		loop times: m0{
			Individual i <- one_of(individuals_to_connect - links.keys);
			links[i] <- [];
			degree[i] <- 0;
		}
		
		loop indiv over: links.keys{
			loop times: K{
				links[indiv] << one_of(links.keys - [indiv,links[indiv]]);
				degree[indiv] <- degree[indiv] +1;
			}
			nb_link <- nb_link +K;
		}
		map<Individual,float>linking_probability;
		float sum;
		float r;
		float acc;
		loop indiv over: individuals_to_connect - links.keys{
			loop times: K{
				linking_probability<-[];
				sum <- 0.0;
				//we compute probability to link each individual already in the network
				loop target over: links.keys - links[indiv]{
					linking_probability[target] <- degree[target] / nb_link;
					sum <- linking_probability[target];
				}
				// we pick an individual
				r <- rnd(sum);
				acc <- 0.0;
				loop target over: linking_probability.keys{
					acc <- acc + linking_probability[target];
					if r < acc{
						links[indiv] << target;
						break;
					}
				}
			}
		}
		
	}
	
}