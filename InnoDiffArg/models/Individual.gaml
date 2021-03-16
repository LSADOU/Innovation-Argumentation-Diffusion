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
	
	//*********Debate variables**********
	// received attacks list [attack::attacked,...]
	list<pair<argument,argument>> D_in <- [];
	// attacks sent list [attack::attacked,...]
	list<pair<argument,argument>> D_out <- [];
	list<argument> best_ext <- [];
	argument debate_arg;
	
	//*********TPB values***********
	
	float attitude <- 0.0 min: -1.0 max: 1.0;
	float attitude_weight <- 0.0;
	float attitude_uncertainty <- 0.0 min: 0.0 max: 1.0;
	
	float subjective_norm <- 0.0 min: -1.0 max: 1.0;
	float subjective_norm_weight <- 0.0;
	float subjective_norm_uncertainty <- 0.0 min: 0.0 max: 1.0;
	
	float perceived_behavioural_control <- 0.0 min: -1.0 max: 1.0;
	float perceived_behavioural_control_weight <- 0.0;
	
	float intention <- 0.0 min: -1.0 max: 1.0;
	float intention_uncertainty <- 0.0;
	
	//*******************************
	
	reflex interactWithRelative{
		last_connexion <- one_of(relatives);
		do initiateDebate(last_connexion);
		//ask last_connexion { do influenced_by(myself); }
	}
	
	// action based on MS dialogue
	action initiateDebate (Individual opponent){
		//the initiator choose a debate central argument in one of its complete extensions, here we use the best extension
		debate_arg <- one_of(best_ext);
		ask opponent{
			do reactInitiateDebate(myself,myself.debate_arg);
		}
	}
	
	// action based on MS dialogue
	action reactInitiateDebate(Individual initiator, argument d_arg){
		debate_arg <- d_arg;
		if (best_ext contains debate_arg){
			// the current agent agrees with initiator because the debate argument is in the current agent best extension	
			do debateEnd(initiator,"OK");
		}else{
			//Hij is the overlapping intention width between the current individual and i
			float Hij <- min([initiator.intention+initiator.intention_uncertainty, intention+intention_uncertainty]) - max([initiator.intention-initiator.intention_uncertainty, intention-intention_uncertainty]);
			if (Hij > intention_uncertainty){
				//the current agent trusts the initiator so acquires the argument
				do addArg(debate_arg);
				do debateEnd(initiator,"OK");
			}else{
				//the current agent doesn't trust the initiator so it will send him an argument that attacks debate_arg	
				argument attacking_arg <- getAttackingArg(debate_arg);
				if (attacking_arg = nil){
					// there is no attack to the debate arg so the debate end here
					do debateEnd(initiator,"KO");
				}else{
					D_out << attacking_arg::debate_arg;
					ask initiator{
						do reactAttack(myself, attacking_arg, debate_arg);
					}
				}
			}
		}
	}

	// action based on MS dialogue
	action reactAttack(Individual opponent, argument attack_arg, argument attacked_arg){
		D_in << attack_arg::attacked_arg;
		if(known_arguments contains attack_arg){
			argument attacking_arg <- getAttackingArg(attack_arg);
			if (attacking_arg = nil){
				float Hij <- min([opponent.intention+opponent.intention_uncertainty, intention+intention_uncertainty]) - max([opponent.intention-opponent.intention_uncertainty, intention-intention_uncertainty]);
				if (Hij > intention_uncertainty){
					do addArg(debate_arg);
					do debateEnd(opponent,"OK");
				}else{
					do debateEnd(opponent,"KO");
				}
			}else{
				D_out << attacking_arg::attacked_arg;
				ask opponent{
					do reactAttack(myself, attacking_arg, attack_arg);
				}
			}
		}else{
			//Hij is the overlapping intention width between the current individual and i
			float Hij <- min([opponent.intention+opponent.intention_uncertainty, intention+intention_uncertainty]) - max([opponent.intention-opponent.intention_uncertainty, intention-intention_uncertainty]);
			if (Hij > intention_uncertainty){
				//the current agent trusts the opponent so acquires the argument
				do addArg(attack_arg);
				best_ext <- get_best_extension().key;
				if(best_ext contains attack_arg){
					do debateEnd(opponent,"OK");
				}else{
					do reactAttack(opponent, attack_arg, attacked_arg);
				}
			}else{
				//the current agent doesn't trust the opponent so rebut the attack
				D_out << attack_arg::attacked_arg;
				ask opponent{
					do reactRebut(myself, attack_arg, attacked_arg);
				}
			}
		}
	}
	
	// action based on MS dialogue
	action reactRebut(Individual opponent, argument attacking_arg, argument attacked_arg){
		//Hij is the overlapping intention width between the current individual and i
		float Hij <- min([opponent.intention+opponent.intention_uncertainty, intention+intention_uncertainty]) - max([opponent.intention-opponent.intention_uncertainty, intention-intention_uncertainty]);
		if (Hij > intention_uncertainty){
			//the current agent trusts the opponent so delete the argument and adds the debate argument (if not present)
			do remove_argument(attacking_arg);
			if not contains(known_arguments, debate_arg){
				do addArg(debate_arg);
			}
			best_ext <- get_best_extension().key;
			if(best_ext contains debate_arg){
				subjective_norm <- subjective_norm + social_impact_param *  (1 - opponent.intention_uncertainty) * (opponent.intention-subjective_norm );
				subjective_norm_uncertainty <- subjective_norm_uncertainty + social_impact_param * (opponent.intention_uncertainty-subjective_norm_uncertainty);
				do debateEnd(opponent,"OK");
			}else{
				//debate_arg still not in best extension then there is an argument attacking it
				argument attacking_arg <- getAttackingArg(debate_arg);
				if (attacking_arg = nil){
					write "error: argument "+ attacked_arg + " is in best extension but not defended by other argument." color: #red;
				}
				D_out << attacking_arg::attacked_arg;
				ask opponent{
					do reactAttack(myself, attacking_arg, debate_arg);
				}
			}
		}else{
			do debateEnd(opponent,"KO");
		}
	}
	
	action debateEnd(Individual contact, string ending){
		
		subjective_norm <- subjective_norm + social_impact_param *  (1 - contact.intention_uncertainty) * (contact.intention-subjective_norm );
		subjective_norm_uncertainty <- subjective_norm_uncertainty + social_impact_param * (contact.intention_uncertainty-subjective_norm_uncertainty);
		best_ext <- get_best_extension().key;
		D_in <- [];
		D_out <- [];
		debate_arg <- nil;
		do updateInformed;
		do updateAttitude;
		do updateIntentionValues;
		do updateInterest;
		do updateDecisionState;
		
		ask contact{
			subjective_norm <- subjective_norm + social_impact_param *  (1 - myself.intention_uncertainty) * (myself.intention-subjective_norm );
			subjective_norm_uncertainty <- subjective_norm_uncertainty + social_impact_param * (myself.intention_uncertainty-subjective_norm_uncertainty);
			best_ext <- get_best_extension().key;
			D_in <- [];
			D_out <- [];
			debate_arg <- nil;
			do updateInformed;
			do updateAttitude;
			do updateIntentionValues;
			do updateInterest;
			do updateDecisionState;
		}
	}
	
	//return an argument attacking argt argument 
	argument getAttackingArg(argument argt){
		argument attacking_arg;
		// first we look for an attacking arg in the best extension that is not already used in the debate
		list<argument> best_ext_diff_D_out <- best_ext where not contains(D_out,each::argt);
		attacking_arg <- one_of(best_ext_diff_D_out inter attacked_by[argt]);
		// if no arg was found before we look for other arguments presents in the known arguments that is not already used in the debate
		if(attacking_arg = nil){
			list<argument> known_arg_diff_D_out <- known_arguments where not contains(D_out,each::argt);
			attacking_arg <- one_of(known_arguments inter attacked_by[argt]);
		}
		if(attacking_arg = nil){
			//write "NO ATTACKING ARG FOUND" color:#red;
		}
		return attacking_arg;
	}
	
	action influenced_by(Individual i){
		
		subjective_norm <- subjective_norm + social_impact_param *  (1 - i.intention_uncertainty) * (i.intention-subjective_norm );
		subjective_norm_uncertainty <- subjective_norm_uncertainty + social_impact_param * (i.intention_uncertainty-subjective_norm_uncertainty);
		
		//Hij is the overlapping intention width between the current individual and i			
		float Hij <- min([i.intention+i.intention_uncertainty, intention+intention_uncertainty]) - max([i.intention-i.intention_uncertainty, intention-intention_uncertainty]);
		
		if (Hij > i.intention_uncertainty){
			switch(i.decision_state){
				match "information request"{ do update_with_new_argument(i,one_of(i.known_arguments)); }
				match "unsatisfied"{ do update_with_new_argument(i, one_of(i.known_arguments where (each.conclusion = "-"))); } // transmit negative argument
				match_one ["pre adoption", "adoption", "satisfied"]{
					//in those states the agent have a higher probability to transmit positive argument
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
	}
	
	action update_with_new_argument(Individual source, argument argt) {
		if (argt != nil) {
			source.known_arguments >> argt;
			source.known_arguments << argt;
			
			if (argt in known_arguments) {
				known_arguments >> argt;
				known_arguments << argt;
			} else {
				known_arguments << argt;
				do add_argument(argt, global_argumentation_graph);
				if (length(known_arguments) > nb_max_known_arguments) {
					argument r_arg <- first(known_arguments);
					do remove_argument(r_arg);
					known_arguments >>r_arg;
				}
			
			}	
		}
	}
	
	action addArg(argument argt){
		known_arguments << argt;
		do add_argument(argt, global_argumentation_graph);
		if (length(known_arguments) > nb_max_known_arguments) {
			argument r_arg <- first(known_arguments);
			do remove_argument(r_arg);
			known_arguments >>r_arg;
		}
	}
	
	action updateInformed{
		informed <-  length(known_arguments) >= nb_relevents_args;
	}
	
	action updateAttitude {
		attitude <- float(make_decision().value);
		//attitude_uncertainty is the mean value for known arguments sources confidences
		list<float> conf_list <- known_arguments collect source_type_confidence[each.source_type];
		attitude_uncertainty <- mean(conf_list);
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
				if (interest ="no") {decision_state <- "no adoption";}
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


