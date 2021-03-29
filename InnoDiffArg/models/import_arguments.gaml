/**
* Name: importarguments
* Based on the internal skeleton template. 
* Author: loic
* Tags: 
*/

model innodiffarg

import "main.gaml"

global {
	
	file arg_csv <- csv_file(""+csv_directory+arg_csv_namefile, ",", string, false);
	file attack_csv <- csv_file(""+csv_directory+attack_csv_namefile, ",", string, attack_csv_has_header);
	matrix attack_data <- matrix (attack_csv);
	list<argument> pros_arg <- [];
	list<argument> cons_arg <- [];
	map<string, argument> argument_by_id <- [];
	map<argument,list<argument>> attacks <- [];
	map<argument,list<argument>> attacked_by <- [];
	
	action readArg {
		
		list<argument> loaded_arguments <- load_myChoice_arguments(arg_csv);
		//write ""+ length(loaded_arguments)+" argument(s) imported";
		global_argumentation_graph<- directed(graph([]));
		loop a over: loaded_arguments {
			add  node(a) to: global_argumentation_graph;
			argument_by_id[a.id] <- a;
			if a.conclusion = "+"{
				pros_arg << a;
			}else{
				cons_arg << a;
			}
			attacks[a]<- [];
			attacked_by[a]<-[];
		}
		
		source_types <- remove_duplicates(A collect each.source_type);
		arguments_criteria <- remove_duplicates(A accumulate each.criteria.keys);
		write "there are " + length(argument_by_id.keys) + " arguments, with "+length(pros_arg)+ " pros and "+length(cons_arg)+" cons arguments";
	}
	
	action computeAttacks {
		
		int nb_attack <- 0;
		
		loop arg1 over: A{
			loop arg2 over: A{
				if (arg1 != arg2) and  (arg1.conclusion != arg2.conclusion) and (arg1.criteria.keys[0] = arg2.criteria.keys[0]){
					attacks[arg1] << arg2;
					attacked_by[arg2] << arg1;
					add edge(arg1::arg2) to: global_argumentation_graph;
					nb_attack <- nb_attack + 1;
				}
			}
		}

		write ""+nb_attack+" attack(s) computed";
		do nb_conflict_attacks;
		
	}
	
	action readAttacks{
		
		int nb_attack <- 0;
		
		loop attack over: rows_list(attack_data){
			argument arg1 <- argument_by_id[attack[0]];
			argument arg2 <- argument_by_id[attack[1]];
			
			attacks[arg1] << arg2;
			attacked_by[arg2] << arg1;
			add edge(arg1::arg2) to: global_argumentation_graph;
			nb_attack <- nb_attack + 1;
		}
		
		write ""+nb_attack+" attack(s) computed";
		do nb_conflict_attacks;
	}
	
	action nb_conflict_attacks{
		
		int nb_sym <- 0;
		int nb_conflict <- 0;
		
		list<pair> sym_list <- [];
		list<pair> conflict_list <- [];
		
		loop offense over: attacks.keys{
			loop defense over: attacks[offense]{
				if attacks[defense] contains offense {
					nb_sym <- nb_sym +1;
					sym_list << offense::defense;
					if (offense.conclusion != defense.conclusion) and (offense.criteria.keys[0] = defense.criteria.keys[0]) and (offense.source_type = defense.source_type){
						nb_conflict <- nb_conflict +1;
						conflict_list << offense::defense;
					}
				}
			}
		}
		
		write "there are/is " + nb_sym + " symetric attacks";
		write sym_list;
		write "there are/is " + nb_conflict + " conflict attacks (symetric, different conclusion but same criteria)";
		write conflict_list;
	}
	
	action generateArgAndAttacks(int nb_arg, int nb_pro_arg, int mean_nb_attack){
		source_types <- ["source A","source B","source C","source D","source E"];
		arguments_criteria <- ["criterion A","criterion B","criterion C","criterion D","criterion E"];
		global_argumentation_graph <- directed(graph([]));
		
		loop id from: 1 to: nb_arg{
			argument argu <- argument(["id"::string(id), "conclusion"::length(pros_arg) < nb_pro_arg? "+":"-", "criteria"::[one_of(arguments_criteria)::1.0], "source_type"::one_of(source_types)]);
			add  node(argu) to: global_argumentation_graph;
			if argu.conclusion = "+"{
				pros_arg << argu;
			}else{
				cons_arg << argu;
			}
			argument_by_id[""+id] <- argu;
			attacks[argu]<- [];
			attacked_by[argu]<-[];
		}
		
		write "there are " + nb_arg + " arguments, with "+length(pros_arg)+ " pros and "+length(cons_arg)+" cons arguments";
		
		int nb_attack <- 0;
		
		loop arg1 over: A{
			loop times: max([gauss(mean_nb_attack, mean_nb_attack/2),0]){
				argument arg2 <- arg1.conclusion = "+" ? one_of(cons_arg where (not contains(attacks[arg1],each))): one_of(pros_arg where (not contains(attacks[arg1],each)));
				attacks[arg1] << arg2;
				attacked_by[arg2] << arg1;
				add edge(arg1::arg2) to: global_argumentation_graph;
				nb_attack <- nb_attack + 1;
			}
		}

		write ""+nb_attack+" attack(s) computed";
		do nb_conflict_attacks;
		
	}
	
	action AddFakeNews{
		loop i from: 1 to: nb_fake_news {
			argument a <- argument(["id":: "fake_news_" + i, "conclusion"::"-", "criteria"::[one_of(arguments_criteria)::1.0], "source_type"::one_of(source_types)]);
			add node(a) to: global_argumentation_graph;
			cons_arg << a;
			attacks[a]<- [];
			attacked_by[a]<-[];
			list<argument> args_to_attack <-  nb_attacks_fake_news among (pros_arg where (each.criteria[0] = a.criteria[0]));
			loop a2 over:args_to_attack{
				add edge(a::a2) to:global_argumentation_graph;
			}
		}
	}
	
}

