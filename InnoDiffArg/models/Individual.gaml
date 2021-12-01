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
	bool interact <- false;
	list<argument> known_arguments;
	
	//*********Debate variables**********
	// a map storing attacks already done during dialogue
	// D_out <- [attacked_arg :: [list_of_attacking_arg]]
	map<argument,list<argument>> D_out <- [];
	list<argument> best_ext <- [];
	argument debate_arg;
	int depth_dial <- 0;
	
	//*********TPB values***********
	
	float attitude <- 0.0 min: -1.0 max: 1.0;
	float attitude_from_acceptabilities <- 0.0 min: -1.0 max: 1.0;
	float attitude_acceptabilities <- 0.0 min: -1.0 max: 1.0;
	float attitude_weight <- 0.0;
	float attitude_uncertainty <- 0.0 min: 0.0 max: 1.0;
	
	float subjective_norm <- 0.0 min: -1.0 max: 1.0;
	float subjective_norm_weight <- 0.0;
	float subjective_norm_uncertainty <- 0.0 min: 0.0 max: 1.0;
	
	float perceived_behavioural_control <- 0.0 min: -1.0 max: 1.0;
	float perceived_behavioural_control_weight <- 0.0;
	
	float intention <- 0.0 min: -1.0 max: 1.0;
	float intention_acceptability <- 0.0 min: -1.0 max: 1.0;
	float intention_uncertainty <- 0.0;
	
	//*******************************
	
	reflex interactWithRelative{
		if interact{
			if length(relatives) = 0{
				write "KO";
			}
			last_connexion <- one_of(relatives);
			do initiateDebate(last_connexion);
			interact <- false;
		}else{
			last_connexion <- nil;
		}
	}
	
	// action based on MS dialogue
	action initiateDebate (Individual opponent){
		//the initiator choose a debate central argument in one of its complete extensions, here we use the best extension
		debate_arg <- last(known_arguments);
		ask opponent{
			do reactInitiateDebate(myself,myself.debate_arg);
		}
	}
	
	// action based on MS dialogue
	action reactInitiateDebate(Individual initiator, argument d_arg){
		depth_dial <- max ([1,depth_dial]);
		debate_arg <- d_arg;
		if (best_ext contains debate_arg){
			// the current agent agrees with initiator because the debate argument is in the current agent best extension	
			do debateEnd(initiator,"OK");
		}else{
			argument attacking_arg <- computeAttackingArg(debate_arg);
			if (attacking_arg = nil){
				//Hij is the overlapping intention width between the current individual and i
				float Hij <- min([initiator.intention+initiator.intention_uncertainty, intention+intention_uncertainty]) - max([initiator.intention-initiator.intention_uncertainty, intention-intention_uncertainty]);
				if Hij > intention_uncertainty {
					if not contains(known_arguments, debate_arg){
						//the current agent trusts the initiator and doesn't know the argument then it adds it to his graph
						do addArg(debate_arg);
					}
					do debateEnd(initiator,"OK");
				}else{
					//the current agent doesn't trust the initiator and there is no argument attacking debate_arg so the debate end here
					do debateEnd(initiator,"KO");
				}
			}else{
				do addAttackToDout(attacking_arg,debate_arg);
				ask initiator{
					do reactAttack(myself, attacking_arg, debate_arg);
				}
			}
		}
	}

	// action based on MS dialogue
	action reactAttack(Individual opponent, argument attack_arg, argument attacked_arg){
		depth_dial <- max ([2,depth_dial]);
		if(known_arguments contains attack_arg){
			argument attacking_arg <- computeAttackingArg(attack_arg);
			if (attacking_arg = nil){
				float Hij <- min([opponent.intention+opponent.intention_uncertainty, intention+intention_uncertainty]) - max([opponent.intention-opponent.intention_uncertainty, intention-intention_uncertainty]);
				if (Hij > intention_uncertainty){
					do debateEnd(opponent,"OK");
				}else{
					do debateEnd(opponent,"KO");
				}
			}else{
				do addAttackToDout(attacking_arg,attack_arg);
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
				ask opponent{
					do reactRebut(myself, attack_arg, attacked_arg);
				}
			}
		}
	}
	
	// action based on MS dialogue
	action reactRebut(Individual opponent, argument attacking_arg_r, argument attacked_arg){
		depth_dial <- max ([3,depth_dial]);
		//Hij is the overlapping intention width between the current individual and i
		float Hij <- min([opponent.intention+opponent.intention_uncertainty, intention+intention_uncertainty]) - max([opponent.intention-opponent.intention_uncertainty, intention-intention_uncertainty]);
		if (Hij > intention_uncertainty){
			//the current agent trusts the opponent so delete the argument and adds the debate argument (if not present)
			do removeArg(attacking_arg_r);
			if not contains(known_arguments, attacked_arg){
				do addArg(attacked_arg);
			}
			best_ext <- get_best_extension().key;
			if(best_ext contains attacked_arg){
				do debateEnd(opponent,"OK");
			}else{
				argument attacking_arg <- computeAttackingArg(attacked_arg);
				if (attacking_arg = nil){
					do debateEnd(opponent,"OK");
				}else{
					do addAttackToDout(attacking_arg,attacked_arg);
					ask opponent{
						do reactAttack(myself, attacking_arg, attacked_arg);
					}
				}
			}
		}else{
			do debateEnd(opponent,"KO");
		}
	}
	
	action debateEnd(Individual contact, string ending){
		depth_all_dial[depth_dial] <- depth_all_dial[depth_dial] +1;
		subjective_norm <- subjective_norm + social_impact_param *  (1 - contact.intention_uncertainty) * (contact.intention-subjective_norm );
		subjective_norm_uncertainty <- subjective_norm_uncertainty + social_impact_param * (contact.intention_uncertainty-subjective_norm_uncertainty);
		best_ext <- get_best_extension().key;
		D_out <- [];
		debate_arg <- nil;
		depth_dial <- 0;
		do updateInformed;
		do updateAttitude;
		do updateIntentionValues;
		do updateInterest;
		do updateDecisionState;
		
		ask contact{
			subjective_norm <- subjective_norm + social_impact_param *  (1 - myself.intention_uncertainty) * (myself.intention-subjective_norm );
			subjective_norm_uncertainty <- subjective_norm_uncertainty + social_impact_param * (myself.intention_uncertainty-subjective_norm_uncertainty);
			best_ext <- get_best_extension().key;
			D_out <- [];
			debate_arg <- nil;
			depth_dial <- 0;
			do updateInformed;
			do updateAttitude;
			do updateIntentionValues;
			do updateInterest;
			do updateDecisionState;
		}
	}
	
	//return an argument attacking the argt argument 
	argument computeAttackingArg(argument argt){
		// at first, looking for an attacking argument in the prefered extension
		list<argument> possible_attacking_args <- attacks[argt] inter best_ext;
		if D_out[argt] != nil {
			possible_attacking_args <- possible_attacking_args - D_out[argt];
		}
		if length(possible_attacking_args) > 0{
			argument attacking_arg <- one_of(possible_attacking_args);
			return attacking_arg;
		}
		// then, looking for an attacking argument in the known_arguments
		possible_attacking_args <- attacks[argt] inter known_arguments;
		if D_out[argt] != nil {
			possible_attacking_args <- possible_attacking_args - D_out[argt];
		}
		argument attacking_arg <- one_of(possible_attacking_args);
		return attacking_arg;
	}
	
	action addAttackToDout(argument attacking, argument attacked){
		if D_out.keys contains attacked{
			if not contains(D_out[attacked], attacking){
				D_out[attacked] << attacking;
			}
		}else{
			D_out[attacked] <- [attacking];
		}
	}
	
	
	action removeArg(argument argt){
		do remove_argument(argt);
		known_arguments >>argt;
	}
	
	action addArg(argument argt){
		known_arguments << argt;
		do add_argument(argt, global_argumentation_graph);
		best_ext <- get_best_extension().key;
		if (length(known_arguments) > nb_max_known_arguments) {
			if(length(best_ext) = length(known_arguments)){
				do removeArg(first(known_arguments - best_ext - [argt]));
			}else{
				do removeArg(first(known_arguments - best_ext - [argt]));
			}
		}
	}
	
	action updateInformed{
		informed <-  length(known_arguments) >= nb_relevents_args;
	}
	
	action updateAttitude {
		map<argument,float> acceptabilities <- get_arguments_acceptabilities();
		attitude_from_acceptabilities <- 0.0;
		int nb_known_arg <- length(known_arguments);
		loop argu over: acceptabilities.keys{
			attitude_from_acceptabilities <- argu.conclusion = "+" ? attitude_from_acceptabilities + acceptabilities[argu] / nb_known_arg : attitude_from_acceptabilities - acceptabilities[argu] / nb_known_arg ;
		}
		attitude <- float(make_decision().value);
		//attitude_uncertainty is the mean value for known arguments sources confidences
		list<float> conf_list <- known_arguments collect source_type_confidence[each.source_type];
		attitude_uncertainty <- 1-mean(conf_list);
	}
	
	action updateIntentionValues{
		intention <- attitude*attitude_weight + subjective_norm*subjective_norm_weight + perceived_behavioural_control*perceived_behavioural_control_weight;
		intention_acceptability <- attitude_acceptabilities*attitude_weight + subjective_norm*subjective_norm_weight + perceived_behavioural_control*perceived_behavioural_control_weight;
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
	
	aspect diffusion{
		
		if length(known_arguments inter strong_arg_added) > 0{
			if length(known_arguments inter attacked_by_added_strong_arg) > 0{
				draw arc(4,180,180) color: #red;
				draw arc(4,0,180) color: #green;
			}else{
				draw circle(2) color: #red border: #black;
			}
		}else{
			if length(known_arguments inter attacked_by_added_strong_arg) > 0{
				draw circle(2) color: #green border: #black;
			}else{
				draw circle(2) color: #white border: #black;
			}
		}
		if (last_connexion != nil){
			draw line([location,last_connexion.location]) end_arrow: 2 color: #black;
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
	
	aspect intention_overview_with_added_arg{
		point center <- {intention*scale,id*2,0.0};
		point right <- {(intention+intention_uncertainty)*scale,id*2,0.0};
		point left <- {(intention-intention_uncertainty)*scale,id*2,0.0};
		draw line([center,right]) color: #black;
		draw line([center,left]) color: #black;
		if length(known_arguments inter strong_arg_added) > 0{
			if length(known_arguments inter attacked_by_added_strong_arg) > 0{
				draw arc(0.1,180,180) at:center color: #red;
				draw arc(0.1,0,180) at:center color: #green;
			}else{
				draw circle(0.1*scale) at:center color: #red border: #black;
			}
		}else{
			if length(known_arguments inter attacked_by_added_strong_arg) > 0{
				draw circle(0.1*scale) at:center color: #green border: #black;
			}else{
				draw circle(0.1*scale) at:center color: #white border: #black;
			}
		}
	}
	
}


