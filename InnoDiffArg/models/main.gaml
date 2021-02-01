/**
* Name: main
* Based on the internal skeleton template. 
* Author: loic
* Tags: 
*/

model innodiffarg

import "import_arguments.gaml"
import "generate_individuals.gaml"
import "generate_social_network.gaml"

global {
	

	string csv_directory <- "../includes/";
	bool arg_csv_has_header <- true;
	string arg_csv_namefile <- "MyChoice_argument.csv";
	bool TPB_csv_has_header <- true;
	string TPB_csv_namefile <- "values_TPB.csv";
	
	int population_size <- 60;
	float social_impact_param <- 0.1;
	int nb_relevents_args <- 2;
	int nb_max_known_arguments <- 7;
	int p <- 10;
	int q <- 15;
	
	float adoption_threshold <- 0.56;
	float avg_network_degree <- 4.60;
	
	float weight_attitude_nonadopters <- 0.229;
	float weight_subnorm_nonadopters <- 0.610;
	float weight_PBC_nonadopters <- 0.161;
	
	float weight_attitude_nadopters <- 0.321;
	float weight_subnorm_adopters <- 0.158;
	float weight_PBC_adopters <- 0.521;  

	list<argument> A <- [];
	map<argument,list<argument>> r <- [];
	
	list source_types <- [];
	list arguments_criteria <- [];
	
	init{
		write "***** start initialisation *****";
		do readArg;
		do computeAttacks;
		do generatePopulation;
		do generateSocialNetwork(Individual.population,2,0.3);
		write "***** end initialisation *****";
	}
}

experiment main type: gui {
	float minimum_cycle_duration <- 0.1;
	map<string,int>decision_state_distribution;
	
	reflex update_decision_state_distribution{
		decision_state_distribution <- [];
		loop indiv over: Individual.population{
			decision_state_distribution[indiv.decision_state] <- decision_state_distribution.keys contains indiv.decision_state ? decision_state_distribution[indiv.decision_state]+1 : 1;
		}
	}
	
	output {
		display MyDisplay type: opengl draw_env:false{
	    	species Individual aspect: basic;
		}
		display intention_chart {
			chart "intention distribution according simulation cycles" type: xy series_label_position:none y_range:{-1,1}{
				datalist Individual collect each.name value: Individual collect each.intention color:#black marker: false thickness:2.0;
			}
			/*chart "intention distribution pie" type: pie {
				data "intention [-1;-0.5]" value: Individual count (each.intention <= -0.5) color:#red;
				data "intention [-0.5;0.0]" value: Individual count (each.intention > -0.5 and each.intention <= 0.0) color:#pink;
				data "intention [0.0;0.5]" value: Individual count (each.intention > 0.0 and each.intention <= 0.5) color:#lightgreen;
				data "intention [0.5;1]" value: Individual count (each.intention > 0.5 and each.intention <= 1) color:#green;
			}*/
		}
		display state_chart {
			chart "decision state distribution according simulation cycles" type: histogram{
				datalist decision_state_distribution.keys value: decision_state_distribution.keys collect decision_state_distribution[each] color:#blue;
			}
		}
	}
}
