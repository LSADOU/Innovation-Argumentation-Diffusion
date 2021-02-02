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
	matrix arg_data <- matrix(arg_csv);
	
	action readArg {
		
		int nb_arg <- 0;
		
		loop current_arg over: rows_list(arg_data){
			
			if !(source_types contains current_arg[16]){
				source_types << current_arg[16];
			}
			
			if !(arguments_criteria contains current_arg[4]){
				arguments_criteria << current_arg[4];
			}
			
			argument a <- argument(["id"::current_arg[0],
									"conclusion"::current_arg[3],
									"statement"::current_arg[11],
									"source_type"::current_arg[16], 
									"criteria"::[current_arg[4]::1.0]
									]);
			A << a;
			r[a] <- [];
			nb_arg <- nb_arg +1;
		}
		
		write ""+nb_arg+" argument(s) imported";
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
					/*write arg1.id + " : " + arg1.conclusion + " : " + arg1.criteria.keys[0];
					write arg2.id + " : " + arg2.conclusion + " : " + arg2.criteria.keys[0];
					write "***************";*/
					r[arg1] << arg2;
					nb_attack <- nb_attack + 1;
				}
			}
		}

		write ""+nb_attack+" attack(s) computed";
		
	}
	
	
}

