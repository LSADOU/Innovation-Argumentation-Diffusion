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
	string output_directory <- "../output/";
	bool arg_csv_has_header <- true;
	string arg_csv_namefile <- "MyChoice_argument.csv";
	bool attack_csv_has_header <- true;
	string attack_csv_namefile <- "attacks.csv";
	bool TPB_csv_has_header <- true;
	string TPB_csv_namefile <- "values_TPB.csv";
	int scale <- 10;
	
	int population_size <- 100;
	float social_impact_param <- 0.1;
	int nb_neighbors <- 4;
	int nb_relevents_args <- 4;
	int nb_max_known_arguments <- 7;
	int p <- 10;
	int q <- 15;
	
	float adoption_threshold <- 0.33;
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
	
	string type_explo;
	int nb_fake_news <- 0;
	int nb_strong_arg_added <- 0;
	int adding_cycle <- 50;
	string network_topology <- "scalefree" among:["smallworld","scalefree","regular","random"];
	string interaction_mode <- "hub_first" among:["hub_first","one by one","full"];
	map<Individual,float> interaction_distrib;
	float add_PBC <- 0.5;
	int nb_extremist <- 0;
	int nb_attacks_fake_news <- 1;
	bool read_arg_instead_of_gen <- false;
	float mean_intention;
	float rate_adoption;
	float pol ;
	bool save_result_in_csv <- false;
	
	
	list<argument> A -> {global_argumentation_graph.vertices};
	
	init{
		create Boundaries;
		if (read_arg_instead_of_gen){
			do readArg;
			do readAttacks;
		}else{
			do generateArgAndAttacks(40, 24, 3);
		}
		if nb_fake_news > 0 {
			write "adding "+nb_fake_news+" fake news.";
			do AddFakeNews;
		}
		if type_explo = "strong_arg"{
			loop times: nb_strong_arg_added{
				do addStrongConsArgument;
			}
		}
		do generatePopulation;
		switch network_topology{
			match "smallworld"{
				do generateSmallWorldSocialNetwork(Individual.population,nb_neighbors,0.2);
			}
			match "scalefree"{
				do generateScaleFreeSocialNetwork(Individual.population,1);
			}
			match "random"{
				do generateRandomSocialNetwork(Individual.population,2);
			}
			match "regular"{
				do generateRegularSocialNetwork(Individual.population,"Moore");
			}
		}
		if type_explo = "strong_arg_insertion"{
			do addStrongConsArgument();
		}
	}
	
	reflex choose_interaction{
		switch interaction_mode{
			match "hub_first"{
				//computing probability to put interact = true to each individual according their number of relatives
				if length(interaction_distrib)=0{
					int nb_relations <- sum(Individual collect length(each.relatives));
					loop indiv over: Individual{
						interaction_distrib[indiv] <- length(indiv.relatives) / nb_relations;
					}
				}else{
					ask rnd_choice(interaction_distrib){
						interact <- true;
					}
				}
			}
			match "one by one"{
				ask one_of(Individual){
					interact <- true;
				}
			}
			match "full"{
				ask Individual{
					interact <- true;
				}
			}
		}
	}
	reflex insert_strong_arg when: type_explo = "strong_arg_insertion" and adding_cycle = cycle {
		loop a over: strong_arg_added{
			ask one_of(Individual){
				do addArg(a);
				best_ext <- get_best_extension().key;
				do updateInformed;
				do updateAttitude;
				do updateIntentionValues;
				do updateInterest;
				do updateDecisionState;
			}
		}
	}
	
	reflex reset_dial_depth{
		depth_all_dial <-[0::0,1::0,2::0,3::0];
	}
	
	pair<int,int> getStrArgAttackedArg{
		int population_with_added_arg <- 0;
		int population_with_attacked_arg <- 0;
		loop indiv over: Individual{
			population_with_added_arg <- population_with_added_arg + length(indiv.known_arguments inter strong_arg_added);
			population_with_attacked_arg <- population_with_attacked_arg + length(indiv.known_arguments inter attacked_by_added_strong_arg);
		}
		return population_with_added_arg::population_with_attacked_arg;
	}
	
	
	reflex save_result when: every(50 #cycle) and save_result_in_csv{
		pol <- polarization();
		mean_intention <- Individual mean_of (each.intention);
		rate_adoption <- (Individual count (each.decision_state="adoption" or each.decision_state="satisfied" or each.decision_state="unsatisfied"))/length(Individual.population);
		string 	results <- "";
		switch(type_explo){
			match "strong_arg"{results <- ""+ int(self)+","+seed+","+nb_strong_arg_added+","+ cycle + ","+	pol+"," +mean_intention+","+rate_adoption; }
			match "fake_news"{results <- ""+ int(self)+","+seed+","+nb_fake_news+","+ cycle + ","+	pol+"," +mean_intention+","+rate_adoption; }
			match "extremist"{results <- ""+ int(self)+","+seed+","+nb_extremist+","+ cycle + ","+	pol+"," +mean_intention+","+rate_adoption; }
			match "stochasticity"{results <- ""+ int(self)+","+seed+","+ cycle+","+pol+","+mean_intention+","+rate_adoption; }
			match "acceptability"{
				float mean_intention_acceptabilty <- Individual mean_of (each.intention_acceptability);
				results <- ""+ int(self)+","+seed+","+cycle+","+pol+","+mean_intention+","+mean_intention_acceptabilty+","+rate_adoption;
			}
			match "strong_arg_insertion"{
				write "simulation "+self+" save cycle "+cycle+" - "+network_topology+"/"+nb_strong_arg_added+"/"+adding_cycle;
				pair<int,int> x <- getStrArgAttackedArg();
				results <- ""+ int(self)+","+seed+","+ cycle+","+network_topology+","+population_size+","+nb_strong_arg_added+","+adding_cycle+","+x.key+","+x.value+","+pol+","+mean_intention+","+rate_adoption; 
			}
		}
		save results to: output_directory+type_explo+"_results.csv" type:text rewrite: false;
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
	parameter type_explo var: type_explo <- "fake_news";
	
	init{
		string header_csv <- "id_exp,seed,nb_fake_news,step,polarisation,mean_intention,rate_adoption";
		save header_csv to: output_directory+"fake_news_results.csv" type:text rewrite: true;
		write "The file "+output_directory+"fake_news_results.csv is created/reset to store data from this experiment" color:#green;
	}
	
	reflex end_sim {
		write "END BATCH" color:#red;
	}
}

experiment test_strong_arg repeat: 100 type: batch until: cycle = 3000 {
	
	parameter strong_arg_added var: nb_strong_arg_added among: [0,1,2,5,10];
	parameter save_result_in_csv var: save_result_in_csv <- true;
	parameter type_explo var: type_explo <- "strong_arg";
	
	init{
		string header_csv <- "id_exp,seed,nb_strong_arg_added,step,polarisation,mean_intention,rate_adoption";
		save header_csv to: output_directory+"strong_arg_results.csv" type:text rewrite: true;
		write "The file "+output_directory+"strong_arg_results.csv is created/reset to store data from this experiment" color:#green;
	}
	
	reflex end_sim {
		write "END BATCH" color:#red;
	}
}

experiment test_extremist repeat: 100 type: batch until: cycle = 3000 {
	parameter nb_extremist var: nb_extremist <- 0 among: [0,1,5,10];
	parameter save_result_in_csv var: save_result_in_csv <- true;
	parameter type_explo var: type_explo <- "extremist";
	
	init{
		string header_csv <- "id_exp,seed,nb_extremists,step,polarisation,mean_intention,rate_adoption";
		save header_csv to: output_directory+"extremist_results.csv" type:text rewrite: true;
		write "The file "+output_directory+"extremist_results.csv is created/reset to store data from this experiment" color:#green;
	}
	
	reflex end_sim{
		write "END BATCH" color:#red;
	}
}

experiment test_stochasticity repeat: 500 type: batch until: cycle = 3000 {
	parameter save_result_in_csv var: save_result_in_csv <- true;
	parameter type_explo var: type_explo <- "stochasticity";
	
	init{
		string header_csv <- "id_exp,seed,step,polarisation,mean_intention,rate_adoption";	
		save header_csv to: output_directory+"stochasticity_results.csv" type:text rewrite: true;
		write "The file "+output_directory+"stochasticity_results.csv is created/reset to store data from this experiment" color:#green;
	}
	
	reflex end_sim {
		write "END BATCH" color:#red;
	}
}

experiment test_acceptability repeat: 100 type: batch until: cycle = 3000 {

	parameter save_result_in_csv var: save_result_in_csv <- true;
	parameter type_explo var: type_explo <- "acceptability";
	
	init{
		string header_csv <- "id_exp,seed,step,polarisation,mean_intention,mean_intention_acceptabilty,rate_adoption";	
		save header_csv to: output_directory+"acceptability_results.csv" type:text rewrite: true;
		write "The file "+output_directory+"acceptability_results.csv is created/reset to store data from this experiment" color:#green;
	}
	
	reflex end_sim {
		write "END BATCH" color:#red;
	}
}

experiment test_diffusion_strg_arg type: batch repeat: 50 until: cycle = 1000{
	
	parameter network_topology var: network_topology <- "regular" among:["regular","scalefree","smallworld","random"];
	parameter nb_strg_arg_added var: nb_strong_arg_added <- 1 among:[1,0];
	parameter adding_cycle var: adding_cycle <- 50 among:[50,500];
	
	init{
		/*gama.pref_parallel_simulations<-true;
		gama.pref_parallel_simulations_all<-true;
		gama.pref_parallel_threads<-24;*/
		save_result_in_csv <- true;
		type_explo <- "strong_arg_insertion";
		interaction_mode <- "full";
		string header_csv <- "id_exp,seed,step,network_topology,population_size,nb_strg_arg,adding_cycle,population_with_added_arg,population_with_attacked_arg,polarisation,mean_intention,rate_adoption";	
		save header_csv to: output_directory+"strong_arg_insertion_results.csv" type:text rewrite: true;
		write "The file "+output_directory+"strong_arg_insertion_results.csv is created/reset to store data from this experiment" color:#green;
	}
	
	reflex end_sim {
		write "END BATCH" color:#red;
	}
}

experiment diffusion_strg_arg type: gui {
		
	parameter type_explo var: type_explo <- "strong_arg_insertion";
	parameter nb_strg_arg_added var: nb_strong_arg_added <- 1;
	parameter adding_cycle var: adding_cycle <- 20;
	parameter network_topology var: network_topology <- "scalefree";
	parameter interaction_mode var:interaction_mode <- "hub_first";

	output {
		layout #split toolbars: false consoles: true navigator:false parameters: false;
		
		display VisualStrongArgDiffusion type: opengl draw_env:false{
	    	species Individual aspect: diffusion;
		}
		display VisualIntention type: opengl draw_env:false{
	    	species Individual aspect: basic;
		}
		display BoundariesIntention type: opengl draw_env:false{
			species Boundaries aspect: intention_overview;
	    	species Individual aspect: intention_overview_with_added_arg;
		}
	}
	
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
		/*display VisualIntention type: opengl draw_env:false{
			species Boundaries aspect: intention_overview;
	    	species Individual aspect: intention_overview;
		}*/
		
		display intention_chart {
			chart "intention distribution according simulation cycles" type: xy series_label_position:none y_range:{-1,1}{
				datalist Individual collect each.name value: Individual collect each.intention color:#black marker: false thickness:2.0;
			}
		}
		
		display degree_chart {
			chart "degree_distrib" type: histogram{
				map<int, int> degree_distrib <- [];
				loop k over: k_i.keys{
					if degree_distrib[k_i[k]] = nil{
						degree_distrib[k_i[k]] <- 1;
					}else{
						degree_distrib[k_i[k]] <- degree_distrib[k_i[k]] + 1;
					}
				}
				datalist degree_distrib.keys value: degree_distrib.keys collect degree_distrib[each];
			}
		}
		/*display state_chart {
			chart "decision states histogram" type: histogram{
				datalist decision_state_distribution.keys value: decision_state_distribution.keys collect decision_state_distribution[each] color:#blue;
			}
		}*/
		/*display arguments_chart {
			chart "occurence number of arguments histogram" type: histogram{
				datalist argument_distribution.keys value: argument_distribution.keys collect argument_distribution[each] color:#blue;
			}
		}*/
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
		}*/
		/*display adoption_chart {
			chart "adoption (in % of pop.) according simulation cycles" type: xy series_label_position:none {
				data "adoption" value: (Individual count (each.decision_state="adoption" or each.decision_state="satisfied" or each.decision_state="unsatisfied"))/length(Individual.population)*100 color:#black marker: false thickness:2.0;
			}
		}*/
	}
}
