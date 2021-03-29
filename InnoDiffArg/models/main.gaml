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
	bool attack_csv_has_header <- true;
	string attack_csv_namefile <- "attacks.csv";
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
	
	map<string,list<float>> weight_TPB <-["social_profile"::[0.23,0.65,0.12],
										  "marginal"::[0.60,0.23,0.17],
										  "extremist"::[1.0,0.0,0.0]];
										  
	float weight_attitude_nonadopters <- 0.229;
	float weight_subnorm_nonadopters <- 0.610;
	float weight_PBC_nonadopters <- 0.161;
	
	float weight_attitude_nadopters <- 0.321;
	float weight_subnorm_adopters <- 0.158;
	float weight_PBC_adopters <- 0.521;  

	graph<argument,unknown> global_argumentation_graph;
	
	list source_types <- [];
	list arguments_criteria <- [];
	
	//depth of all dialogues that occured in this step
	map<int,int> depth_all_dial <-[0::0,1::0,2::0,3::0];
	
	string type_explo <- "normal";//"stochasticity";	
	int nb_fake_news <- 0;
	int nb_attacks_fake_news <- 1;
	float mean_intention;
	float rate_adoption;
	float pol ;
	bool save_result_in_csv <- false;
	
	list<argument> A -> {global_argumentation_graph.vertices};
	
	init{
		create Boundaries;
		do readArg;
		do readAttacks;
		//do generateArgAndAttacks(40, 24, 3);
		if (nb_fake_news > 0) {
			do AddFakeNews;
		}
		do generatePopulation;
		do generateSocialNetwork(Individual.population,4,0.2);
	}
	
	reflex reset_dial_depth{
		depth_all_dial <-[0::0,1::0,2::0,3::0];
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

experiment test_fake_news repeat: 100 type: batch until: cycle = 3000 {
	parameter nb_fake_news var: nb_fake_news among: [100,50,10,5,0];
	parameter save_result_in_csv var: save_result_in_csv <- true;
	parameter type_explo var: type_explo <- "normal";
	
	reflex end_sim {
		write "num fake news: " + nb_fake_news + " mean intention: " + simulations mean_of each.mean_intention + " mean polarization: " + simulations mean_of each.pol + " mean_adoptions: " + simulations mean_of each.rate_adoption;
	}
}

experiment test_stochasticity repeat: 500 type: batch until: cycle = 3000 {
	parameter nb_fake_news var: nb_fake_news <- 0 among: [0];
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
		
		/*display VisualNetwork type: opengl draw_env:false{
	    	species Individual aspect: basic;
		}*/
		/*display VisualIntention type: opengl draw_env:false{
			species Boundaries aspect: intention_overview;
	    	species Individual aspect: intention_overview;
		}*/
		
		display intention_chart {
			chart "intention distribution according simulation cycles" type: xy series_label_position:none y_range:{-1,1}{
				datalist Individual collect each.name value: Individual collect each.intention color:#black marker: false thickness:2.0;
			}
		}
		/*display state_chart {
			chart "decision states histogram" type: histogram{
				datalist decision_state_distribution.keys value: decision_state_distribution.keys collect decision_state_distribution[each] color:#blue;
			}
		}*/
		display arguments_chart {
			chart "occurence number of arguments histogram" type: histogram{
				datalist argument_distribution.keys value: argument_distribution.keys collect argument_distribution[each] color:#blue;
			}
		}
		/*display dialogue_chart {
			chart "depth of all dialogue" type: xy {
				data "depth 1" value: depth_all_dial[1] color:#red marker: false thickness:2.0;
				data "depth 2" value: depth_all_dial[2] color:#green marker: false thickness:2.0;
				data "depth 3" value: depth_all_dial[3] color:#blue marker: false thickness:2.0;
			}
		}*/
		/*display informed_chart {
			chart "% of not informed people according simulation cycles" type: xy series_label_position:none {
				data "not informed people" value: (Individual count !each.informed)/length(Individual.population)*100 color:#black marker: false thickness:2.0;
			}
		}
		display interest_chart {
			chart "% of interested people according simulation cycles" type: xy series_label_position:none {
				data "interested people" value: (Individual count (each.interest="yes"))/(length(Individual.population))*100 color:#black marker: false thickness:2.0;
			}
		}
		display adoption_chart {
			chart "adoption (in % of pop.) according simulation cycles" type: xy series_label_position:none {
				data "adoption" value: (Individual count (each.decision_state="adoption" or each.decision_state="satisfied" or each.decision_state="unsatisfied"))/length(Individual.population)*100 color:#black marker: false thickness:2.0;
			}
		}*/
	}
}
