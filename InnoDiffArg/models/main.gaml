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
	int scale <- 10;
	
	int population_size <- 60;
	float social_impact_param <- 0.1;
	int nb_relevents_args <- 4;
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

	graph<argument,unknown> global_argumentation_graph;
	
	list source_types <- [];
	list arguments_criteria <- [];
	
	string type_explo <- "normal";//"stochasticity";
	int nb_attacks <- 1;
		
	int nb_fake_news <- 0;
	float mean_intention;
	float rate_adoption;
	float pol ;
	bool save_result_in_csv <- false;
	
	list<argument> A -> {global_argumentation_graph.vertices};
	
	init{
		//write "***** start initialisation *****";
		create Boundaries;
		do readArg;
		do computeAttacks;
		
		if (nb_fake_news > 0) {
			
		}
		list<string> arg_types <- remove_duplicates(A accumulate list<string>(each.criteria.keys));
		if (nb_fake_news > 0) {
			loop i from: 1 to: nb_fake_news {
				string type_argument <- one_of(arg_types);
				argument a <- argument(["id":: "fake_new_ " + i ,"option"::"", "conclusion"::"-", "criteria"::[type_argument::1.0], "source_type"::"Autre site Web"]);
				list<argument> args <-  (A where ((each.conclusion = "+") and (type_argument in each.criteria.keys))) ;
				if not empty(args) {
					list<argument> args_attacks <- nb_attacks among args;
					loop ag over: args_attacks {
						if (ag != nil) {
							bool is_added <- add_attack(a,ag,global_argumentation_graph);
							is_added <-add_attack(ag,a,global_argumentation_graph);
						} 
						
						
					}
				}
			}
		}		
		do generatePopulation;
		do generateSocialNetwork(Individual.population,4,0.2);
	}
	
	reflex save_result when: every(50 #cycle) and save_result_in_csv{
		
		 pol <- polarization();
		 mean_intention <- Individual mean_of (each.intention);
		 rate_adoption <- (Individual count (each.decision_state="adoption" or each.decision_state="satisfied" or each.decision_state="unsatisfied"))/length(Individual.population);
		
			
		string 	results <- ""+ int(self)+"," + seed+","+nb_fake_news+","+ cycle + ","+	pol+"," +mean_intention+","+rate_adoption;
		
		
		save results to: type_explo + "/results_" + type_explo+ "_"+nb_fake_news+".csv" type:text rewrite: false;
	}
	
	float polarization{
		list<float> dists;
		int N <- length(Individual) - 1;
		loop i from: 0 to: N {
			Individual pi <- Individual(i);
			loop j from: 0 to: N {
				if (i != j) {
					Individual pj <- Individual(j);
					dists << abs(pi.intention - pj.intention);
				}
			}	
		}
		float mean_val <- mean(dists);
		float polarization;
		loop v over: dists {
			polarization <- polarization + ((v - mean_val) ^ 2);
		}
		polarization <- polarization / (1 * (N + 1) * N);
		return polarization;
	}
	
}

species Boundaries{
	aspect intention_overview{
		
		point center_low <- {0.0,0.0,0.0};
		point left_low <- {-1.0*scale,0.0,0.0};
		point right_low <- {1.0*scale,0.0,0.0};
		point center_high <- {0.0,population_size*scale,0.0};
		point left_high <- {-1.0*scale,population_size*scale,0.0};
		point right_high <- {1.0*scale,population_size*scale,0.0};
		
		draw "-1" color: #black size: 3*scale at: {left_low.x, left_low.y-1*scale} font: font("Helvetica", 3*scale , #plain);
		draw "1" color: #black size: 3*scale at: {right_low.x, right_low.y-1*scale} font: font("Helvetica", 3*scale , #plain);
		draw "0" color: #black size: 3*scale at: {center_low.x, center_low.y-1*scale} font: font("Helvetica", 3*scale , #plain);
		draw line([center_low,center_high]) color: #gray;
		draw line([left_low,left_high]) color: #gray;
		draw line([right_low,right_high]) color: #gray;
	}
}

experiment test_fake_news repeat: 100 type: batch until: cycle = 500 {
	parameter nb_fake_news var: nb_fake_news among: [100,50,10,5,0];
		parameter save_result_in_csv var: save_result_in_csv <- true;
		parameter type_explo var: type_explo <- "normal";
	
	reflex end_sim {
		write "num fake news: " + nb_fake_news + " mean intention: " + simulations mean_of each.mean_intention + " mean polarization: " + simulations mean_of each.pol + " mean_adoptions: " + simulations mean_of each.rate_adoption;
	}
}

experiment test_stochasticity repeat: 500 type: batch until: cycle = 500 {
	parameter save_result_in_csv var: save_result_in_csv <- true;
	parameter type_explo var: type_explo <- "stochasticity";
	
}
experiment main type: gui {
//	float minimum_cycle_duration <- 0.1;
	
	map<string,int>argument_distribution <- [];
	map<string,int>decision_state_distribution <- [];
	list<string> possible_states <- ["information request", "not concerned", "no adoption", "pre adoption", "adoption", "satisfied", "unsatisfied"];
	
	
	reflex update_decision_state_distribution{
		loop state over:possible_states{
			decision_state_distribution[state] <- 0;
		}
		loop indiv over: Individual.population{
			decision_state_distribution[indiv.decision_state] <- decision_state_distribution[indiv.decision_state] + 1;
		}
	}
	
	reflex update_arguments_distribution{
		loop argu over:A {
			argument_distribution[argu.id] <- 0;
		}
		loop argu over: Individual.population accumulate each.known_arguments{
			argument_distribution[argu.id] <- argument_distribution[argu.id] + 1;
		}
	}
	
	output {
		
		display VisualNetwork type: opengl draw_env:false{
	    	species Individual aspect: basic;
		}
		display VisualIntention type: opengl draw_env:false{
			species Boundaries aspect: intention_overview;
	    	species Individual aspect: intention_overview;
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
			chart "decision states histogram" type: histogram{
				datalist decision_state_distribution.keys value: decision_state_distribution.keys collect decision_state_distribution[each] color:#blue;
			}
		}
		display arguments_chart {
			chart "occurence number of arguments histogram" type: histogram{
				datalist argument_distribution.keys value: argument_distribution.keys collect argument_distribution[each] color:#blue;
			}
		}
		display informed_chart {
			chart "% of not informed people according simulation cycles" type: xy series_label_position:none {
				data "not informed people" value: (Individual count !each.informed)/length(Individual.population) color:#black marker: false thickness:2.0;
			}
		}
		display interest_chart {
			chart "% of interested people according simulation cycles" type: xy series_label_position:none {
				data "interested people" value: (Individual count (each.interest="yes"))/(length(Individual.population)) color:#black marker: false thickness:2.0;
			}
		}
		display adoption_chart {
			chart "adoption (in % of pop.) according simulation cycles" type: xy series_label_position:none {
				data "adoption" value: (Individual count (each.decision_state="adoption" or each.decision_state="satisfied" or each.decision_state="unsatisfied"))/length(Individual.population) color:#black marker: false thickness:2.0;
			}
		}
	}
}
