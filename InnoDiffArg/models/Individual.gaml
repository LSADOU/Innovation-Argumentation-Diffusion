/**
* Name: Individual
* Based on the internal skeleton template. 
* Author: loic
* Tags: 
*/

model innodiffarg

import "main.gaml"

species Individual skills: [argumenting]{
	
	int id <- 0;
	
	string decision_state <- "information request" among: ["information request", "not concerned", "no adoption", "pre adoption", "adoption", "satisfied", "unsatisfied"];
	string interest <- "maybe" among: ["no", "maybe", "yes"];
	bool informed <- false;
	list<Individual> relatives <- [];
	Individual last_connexion;
	bool satisfied;
	int cpt_satisfied <- 0;
	
	list<argument> known_arguments;
	
	

	//*********TPB values***********
	
	float attitude <- 0.0 min: -1.0 max: 1.0;
	float attitude_weight <- 0.0;
	float attitude_uncertainty <- 0.01 min: 0.01 max: 2.0;
	
	float subjective_norm <- 0.0 min: -1.0 max: 1.0;
	float subjective_norm_weight <- 0.0;
	float subjective_norm_uncertainty <- 0.01 min: 0.01 max: 2.0;
	
	float perceived_behavioural_control <- 0.0 min: -1.0 max: 1.0;
	float perceived_behavioural_control_weight <- 0.0;
	
	float intention <- 0.0 min: -1.0 max: 1.0;
	float intention_uncertainty <- 0.0;
	
	//*******************************
	
	reflex interactWithRelative{
		last_connexion <- one_of(relatives);
		ask last_connexion { do influenced_by(myself); }
	}
	
	action update_with_new_argument(Individual source, argument argt) {
		if (argt != nil) {
			argument r_arg <- first(known_arguments);
			source.known_arguments >> argt;
			source.known_arguments << argt;
			
			if (argt in known_arguments) {
				known_arguments >> argt;
				known_arguments << argt;
			} else {
				known_arguments << argt;
				bool added <- add_argument(argt, global_argumentation_graph);
			}	
		}
	}
	action influenced_by(Individual i){
		//Hij is the overlapping intention width between the current individual and i
		float Hij <- min([i.intention+i.intention_uncertainty, subjective_norm+subjective_norm_uncertainty]) - max([i.intention+i.intention_uncertainty, subjective_norm+subjective_norm_uncertainty]);
		subjective_norm <- subjective_norm + social_impact_param * (Hij/i.intention_uncertainty-1) * (i.intention-subjective_norm);
		subjective_norm_uncertainty <- subjective_norm + social_impact_param * (Hij/i.intention_uncertainty-1) * (i.intention_uncertainty-subjective_norm_uncertainty);

		switch(i.decision_state){
			match "information request"{ do update_with_new_argument(i,one_of(i.known_arguments)); }
			match "unsatisfied"{ do update_with_new_argument(i, one_of(i.known_arguments where (each.conclusion = "-"))); } // transmit negative argument
			match_one ["pre adoption", "adoption", "satisfied"]{
				//in those state the agent have a higher probability to transmit positive argument
				if flip(0.8){
					do update_with_new_argument(i,one_of(i.known_arguments where (each.conclusion = "+")));
				}else{
					do update_with_new_argument(i,one_of(i.known_arguments where (each.conclusion = "-")));
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
		attitude <- float(make_decision().value);
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
	
	
	
	
	aspect basic{
		rgb c <- #white;
		c <- intention <0 ? rgb(255,255*(1+intention),255*(1+intention)) : rgb(255*(1-intention),255,255*(1-intention));
		draw circle(2) color: c border: #black;
		if (last_connexion != nil){
			draw line([location,last_connexion.location]) end_arrow: 1 color: #black;
		}
	}
	
	aspect intention_overview{
		point center <- {intention*scale,id*2,0.0};
		point right <- {(intention+intention_uncertainty)*scale,id*2,0.0};
		point left <- {(intention-intention_uncertainty)*scale,id*2,0.0};
		draw circle(0.1*scale) at:center color: #black;
		draw line([center,right]) color: #black;
		draw line([center,left]) color: #black;
	}
	
}


