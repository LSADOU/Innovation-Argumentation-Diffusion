/**
* Name: generateindividuals
* Based on the internal empty template. 
* Author: loic
* Tags: 
*/


model innodiffarg

import "main.gaml"
import "Individual.gaml"


global{
	
	file TPB_csv <- csv_file(""+csv_directory+TPB_csv_namefile, ",", string, TPB_csv_has_header);
	matrix TPB_data <- matrix(TPB_csv);
	map<int,map<string,pair<float,float>>> TPBmap <- [];
	int nb_clusters <- 0;
	
	action readTPBdata{
		
		loop line over: rows_list(TPB_data){
			if line[0] = "0"{
				if  line[2] = "mean" {
					TPBmap[int(line[1])] <- [
						"attitude"::pair(float(line[3]),0.0),
						"attitude uncertainty"::pair(float(line[4]),0.0),
						"subjective norm"::pair(float(line[5]),0.0),
						"subjective norm uncertainty"::pair(float(line[6]),0.0),
						"PBC"::pair(float(line[7]),0.0)
					];
				}else{
					TPBmap[int(line[1])]["attitude"] <- TPBmap[int(line[1])]["attitude"].key::float(line[3]);
					TPBmap[int(line[1])]["attitude uncertainty"] <- TPBmap[int(line[1])]["attitude uncertainty"].key::float(line[4]);
					TPBmap[int(line[1])]["subjective norm"] <- TPBmap[int(line[1])]["subjective norm"].key::float(line[5]);
					TPBmap[int(line[1])]["subjective norm uncertainty"] <- TPBmap[int(line[1])]["subjective norm uncertainty"].key::float(line[6]);
					TPBmap[int(line[1])]["PBC"] <- TPBmap[int(line[1])]["PBC"].key::float(line[7]);
				}
			}
		}
		nb_clusters <- length(TPBmap.keys);
		//write ""+ nb_clusters +" individual clusters data loaded";
	}
	
	action generatePopulation {
		
		do readTPBdata;
		
		// this part computes agents locations to form a lattice ring
		float inter <- 2 * #pi / population_size;
		list<point> indiv_locations <- [];
		float x <- 0.0;
		loop times: population_size {
			indiv_locations << point(cos_rad(x)*80,sin_rad(x)*80);
			x <- x +  inter;
		}
		
		int cpt <- 0;
		
		create Individual number: population_size{
			
			argumentation_graph <- graph([]);
			list<argument> args <- rnd(1,nb_max_known_arguments) among A;
			loop a over: args {
				known_arguments << a;
				 do add_argument(a,global_argumentation_graph);
			}
			write argumentation_graph;
			loop criterion over: arguments_criteria{
				crit_importance[criterion] <- rnd(0.0, 1.0);
			}
			loop source over: source_types{
				source_type_confidence[source] <- rnd(1.0);
			}
		 	int cluster <- rnd(1,nb_clusters);
			
			attitude <- myself.getAttitude(cluster);
			attitude_weight <- weight_attitude_nonadopters;
			attitude_uncertainty <- myself.getAttitudeUncertainty(cluster)/2;
	
			subjective_norm <- myself.getSubjectiveNorm(cluster);
			subjective_norm_weight <- weight_subnorm_nonadopters;
			subjective_norm_uncertainty <- myself.getSubjectiveNormUncertainty(cluster)/2;
	
			perceived_behavioural_control <- myself.getPBC(cluster);
			perceived_behavioural_control_weight <- weight_PBC_nonadopters;
			
			do updateInformed;
			do updateAttitude;
			do updateIntentionValues;
			do updateInterest;
			do updateDecisionState;
			
			location <- indiv_locations[cpt];
			id <- cpt;
			cpt <- cpt+1;
		}
	}
	
	float getAttitude (int cluster){
		return gauss(TPBmap[cluster]["attitude"].key,TPBmap[cluster]["attitude"].value);
	}
	float getAttitudeUncertainty(int cluster){
		return gauss(TPBmap[cluster]["attitude uncertainty"].key,TPBmap[cluster]["attitude uncertainty"].value);
	}
	float getSubjectiveNorm(int cluster){
		return gauss(TPBmap[cluster]["subjective norm"].key,TPBmap[cluster]["subjective norm"].value);
	}
	float getSubjectiveNormUncertainty(int cluster){
		return gauss(TPBmap[cluster]["subjective norm uncertainty"].key,TPBmap[cluster]["subjective norm uncertainty"].value);
	}
	float getPBC(int cluster){
		return gauss(TPBmap[cluster]["PBC"].key,TPBmap[cluster]["PBC"].value);
	}

	
}