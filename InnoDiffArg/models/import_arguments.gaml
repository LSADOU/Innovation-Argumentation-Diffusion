/**
* Name: importarguments
* Based on the internal skeleton template. 
* Author: loic
* Tags: 
*/

model innodiffarg

import "main.gaml"

global {
	
	file arg_csv <- csv_file(""+csv_directory+arg_csv_namefile, ",", string, arg_csv_has_header);
	
	action readArg {
		
		list<argument> loaded_arguments <- load_myChoice_arguments(arg_csv);
		write ""+ length(loaded_arguments)+" argument(s) imported";
		global_argumentation_graph<- graph([]);
		loop a over: loaded_arguments {
			add  node(a) to: global_argumentation_graph;
		}
		source_types <- remove_duplicates(A collect each.source_type);
		arguments_criteria <- remove_duplicates(A accumulate each.criteria.keys);
	
		write "Sources are ";
		write source_types;
		write "criterias are ";
		write arguments_criteria;
		
	}
	
	action computeAttacks {
		
		int nb_attack <- 0;
		
		loop arg1 over: A{
			loop arg2 over: A{
				if (arg1 != arg2) and  (arg1.conclusion != arg2.conclusion) and (arg1.criteria.keys[0] = arg2.criteria.keys[0]){
					bool is_added <- add_attack(arg1,arg2, global_argumentation_graph);
					nb_attack <- nb_attack + 1;
				}
			}
		}

		write ""+nb_attack+" attack(s) computed";
		
	}
	
	
}

