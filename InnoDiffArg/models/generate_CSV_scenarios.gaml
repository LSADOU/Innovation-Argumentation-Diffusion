/**
* Name: generateCSVstochasticty
* Based on the internal skeleton template. 
* Author: admin_ptaillandie
* Tags: 
*/

model generateCSVstochasticty

global {
	list<int> nb_fake_news <- [100,50,10,5,0];


	init {
		loop nb_fn over: nb_fake_news {
			csv_file results_csv_file <- csv_file("normal/results_normal_" + nb_fn + ".csv",",", string);
		
			matrix data <- matrix(results_csv_file);
			map<string,list<float>> res_mean_intention;
			map<string,list<float>> res_polarization;
			map<string,list<float>> res_taux_adoptants;
			list<int> index_mean_int <- [5];
			list<int> index_polarization <- [4];
			list<int> index_taux_adoptants <- [6];
			
			loop i from: 0 to: data.rows -1 {
				string id <- string(data[1,i]);
				float val_dead <- 0.0;
				loop j over: index_mean_int{
					val_dead <- val_dead + float(data[j,i]);
				}
				if not (id in res_mean_intention.keys) {
					res_mean_intention[id] <- [val_dead];
				} else {
					res_mean_intention[id] << val_dead;
				}
				//write sample(data[data.columns - 6]);
				float val_injured <- 0.0;
				loop j over: index_polarization{
					val_injured <- val_injured + float(data[j,i]);
				}
				if not (id in res_polarization.keys) {
					res_polarization[id] <- [val_injured];
				} else {
					res_polarization[id] << val_injured;
				}
	
				float val_building <- 0.0;
				loop j over: index_taux_adoptants{
					val_building <- val_building + float(data[j,i]);
				}
				if not (id in res_taux_adoptants.keys) {
					res_taux_adoptants[id] <- [val_building];
				} else {
					res_taux_adoptants[id] << val_building;
				}
			
			
			}
			int size_ref <- res_mean_intention.values max_of (length(each));
			
			string h <- "id";
			loop i from: 0 to: size_ref -1{
				h <- h + ",intention_" + i;
			}
			save h to: "generated/normal/result_normal_intention_" + nb_fn + ".csv" type: text ; 
			loop r over: res_mean_intention.keys {
				string tos <- r;
				if (length(res_mean_intention[r]) = size_ref) {
					loop i over: res_mean_intention[r] {
						tos <- tos + "," + i;
					}
					save tos to: "generated/normal/result_normal_intention_" + nb_fn + ".csv" type: text rewrite: false; 
				}
				
			}
			
			h <- "id";
			loop i from: 0 to: size_ref -1{
				h <- h + ",polarization_" + i;
			}
			save h to: "generated/normal/result_normal_polarization_" + nb_fn + ".csv" type: text ; 
			loop r over: res_polarization.keys {
				string tos <- r;
				if (length(res_polarization[r]) = size_ref) {
					
					loop i over: res_polarization[r] {
						tos <- tos + "," + i;
					}
					save tos to: "generated/normal/result_normal_polarization_" + nb_fn + ".csv" type: text rewrite: false; 
			
				}
			}
			
			
			
			h <- "id";
			loop i from: 0 to: size_ref -1{
				h <- h + ",adoptants_" + i;
			}
			save h to: "generated/normal/result_normal_adoptants_" + nb_fn + ".csv" type: text ; 
			loop r over: res_taux_adoptants.keys {
				string tos <- r;
				if (length(res_taux_adoptants[r]) = size_ref) {
					
					loop i over: res_taux_adoptants[r] {
						tos <- tos + "," + i;
					}
					save tos to: "generated/normal/result_normal_adoptants_" + nb_fn + ".csv" type: text rewrite: false; 
			
				}
			}
			
			
			h <- "id,polarization,mean_intention,num_adopters";
			save h to: "generated/normal/result_normal_" + nb_fn + ".csv" type: text ; 
			loop r over: res_polarization.keys {
				if (length(res_polarization[r]) = size_ref) {
					string tos <- r + "," + last(res_polarization[r])+ "," + last(res_mean_intention[r]) +"," + last(res_taux_adoptants[r]);
					save tos to: "generated/normal/result_normal_" + nb_fn + ".csv" type: text rewrite: false; 
				}
				
			}
			
			
		}
	
	}
	/** Insert the global definitions, variables and actions here */
}

experiment generateCSVscenario type: gui {
	/** Insert here the definition of the input and output of the model */
	output {
	}
}
