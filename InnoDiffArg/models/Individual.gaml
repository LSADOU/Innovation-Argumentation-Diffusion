/**
* Name: Individual
* Based on the internal skeleton template. 
* Author: loic
* Tags: 
*/

model innodiffarg

import "main.gaml"

species Individual skills: [argumenting]{
	
	list<argument> known_arguments <- [];
	string decision_state <- "information request" among: ["information request", "not concerned", "no adoption", "pre adoption", "adoption", "satisfied", "unsatisfied"];
	string interest <- "maybe" among: ["no", "maybe", "yes"];
	bool informed <- false;
	bool satisfied <- false;
	map<string,float> criteria_importance <- [];
	map<string,float> source_confidence <- [];
	list<Individual> relatives <- [];
	Individual last_connexion;
	int cpt_satisfied <- 0;
	

	//*********TPB values***********
	
	float attitude <- 0.0 min: -1.0 max: 1.0;
	float attitude_weight <- 0.0;
	float attitude_uncertainty <- 0.01 min: 0.01 max: 2.0;
	
	float subjective_norm <- 0.0 min: -1.0 max: 1.0;
	float subjective_norm_weight <- 0.0;
	float subjective_norm_uncertainty <- 0.01 min: 0.01 max: 2.0;
	
	float perceived_behavioural_control <- rnd(-1.0,1.0) min: -1.0 max: 1.0;
	float perceived_behavioural_control_weight <- 0.0;
	
	float intention <- 0.0;
	float intention_uncertainty <- 0.0;
	
	//*******************************
	
	reflex interactWithRelative{
		last_connexion <- one_of(relatives);
		ask last_connexion { do influenced_by(myself); }
	}
	
	action influenced_by(Individual i){
		//Hij is the overlapping intention width between the current individual and i
		float Hij <- min([i.intention+i.intention_uncertainty, subjective_norm+subjective_norm_uncertainty]) - max([i.intention+i.intention_uncertainty, subjective_norm+subjective_norm_uncertainty]);
		subjective_norm <- subjective_norm + social_impact_param * (Hij/i.intention_uncertainty-1) * (i.intention-subjective_norm);
		subjective_norm_uncertainty <- subjective_norm + social_impact_param * (Hij/i.intention_uncertainty-1) * (i.intention_uncertainty-subjective_norm_uncertainty);

		switch(i.decision_state){
			match "information request"{ do addArg(one_of(i.known_arguments)); }
			match "unsatisfied"{ do addArg(one_of(i.known_arguments where (each.conclusion = "-"))); } // transmit negative argument
			match_one ["pre adoption", "adoption", "satisfied"]{
				//in those state the agent have a higher probability to transmit positive argument
				if flip(0.8){
					do addArg(one_of(i.known_arguments where (each.conclusion = "+")));
				}else{
					do addArg(one_of(i.known_arguments where (each.conclusion = "-")));
				}
			}
		}
		
		do updateInformed;
		do updateAttitude;
		do updateIntentionValues;
		do updateInterest;
		do updateDecisionState;
	}
	
	action updateInformed{
		informed <-  length(known_arguments) >= nb_relevents_args;
	}
	
	action updateAttitude {
		attitude <- getAttitudeFromArgs;
	}
	
	action updateIntentionValues{
		intention <- attitude*attitude_weight + subjective_norm*subjective_norm_weight + perceived_behavioural_control*perceived_behavioural_control_weight;
		intention_uncertainty <- (attitude_uncertainty*attitude_weight + subjective_norm_uncertainty*subjective_norm_weight)/(attitude_weight+subjective_norm_weight);
	}
	
	action updateInterest{
		interest <- "maybe";
		if(intention - intention_uncertainty > 0){interest <- "yes";}
		if(intention + intention_uncertainty < 0){interest <- "no";}
	}
	
	action updateDecisionState{
		
		switch decision_state{
			match "information request" {
				if (!informed and interest ="no") {decision_state <- "not concerned";}
				if (informed and interest ="no") {decision_state <- "no adoption";}
				if (informed and interest ="yes") {decision_state <- "pre adoption";}	
			}
			match "not concerned" {
				if (interest ="yes" or interest ="maybe") {decision_state <- "information request";}
			}
			match "pre adoption" {
				if (interest ="no" or interest ="maybe") {decision_state <- "no adoption";}
				if (interest ="yes" and intention >= adoption_threshold){
					decision_state <- "adoption";
				}
			}
			match "no adoption" {
				if (interest ="yes") {decision_state <- "pre adoption";}
			}
			match "adoption" {
				cpt_satisfied <- cpt_satisfied +1;
				//we consider that agent have more probability to be satisfied by innovation if it matches their attitude
				//attitude is in range [-1;1] we change it to a uniform distribution range [0;1] for being satisfied
				satisfied <- flip (attitude/2+0.50);
				if (satisfied and cpt_satisfied=q) {decision_state <- "satisfied";}
				if (!satisfied and cpt_satisfied=q) {decision_state <- "unsatisfied";}
			}
			match "satisfied" {}
			match "unsatisfied" {}
		}
	}
	
	
	
	float argStrength (argument arg){
		float strength <- 0.0;
		loop crit over: arg.criteria.keys{
			strength <- strength + criteria_importance[string(crit)] * float(arg.criteria[string(crit)]);
		}
		return strength;
	}
	
	action addArg(argument arg){
		if arg != nil and !(known_arguments contains arg){
			known_arguments << arg;
			if (length(known_arguments) >= nb_max_known_arguments){
				remove index:0 from:known_arguments;
			}	
		}
	}
	
	float getAttitudeFromArgs{
		list<list<argument>> preferedExtensions <- [];
		list<argument> attacked_by_offense <- [];
		argumentation_graph <- directed(graph(known_arguments));
		
		loop offense over: known_arguments{
			attacked_by_offense <- r[offense] where (known_arguments contains each);
			loop victim over: attacked_by_offense{
				if argStrength(offense) > argStrength(victim){
					argumentation_graph <- argumentation_graph add_edge(offense::victim);
				}
			}
		}
		
		preferedExtensions <- preferred_extensions();
		float max_value <- -2.0;
		float current_value;
		loop pe over: preferedExtensions{
			current_value <- sum(pe collect argStrength(each));
			max_value <-  current_value > max_value ? current_value : max_value ;
		}
		
		return max_value;
	}
	
	aspect basic{
		rgb c <- #white;
		c <- intention <0 ? rgb(255,255*(1+intention),255*(1+intention)) : rgb(255*(1-intention),255,255*(1-intention));
		draw circle(2) color: c border: #black;
		if (last_connexion != nil){
			draw line([location,last_connexion.location]) end_arrow: 1 color: #black;
		}
	}
	
}


