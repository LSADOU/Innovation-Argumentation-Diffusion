/**
* Name: generatexml
* Based on the internal empty template. 
* Author: admin_ptaillandie
* Tags: 
*/


model generatexml

global {
	int nb_replication <- 100;
	list<string> scenarios <- ["S1"] ;
	string type_explo <- "normal" among: ["normal", "stochasticity"];
	bool save_result_in_csv;
	list<int> nb_fake_news;
	
	
	init {
		string to_write <- "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>" + "\n";
			to_write <- to_write + "<Experiment_plan>" + "\n";
		loop num over: nb_fake_news {
			
			loop i from: 0 to: nb_replication -1 {
				to_write <- to_write + "<Simulation id=\"" + i + "\" sourcePath=\"./FloodMines/models/FloodCiotat2.gaml\" finalStep=\"3840\" experiment=\"go_flood\" seed=\"" + i +"\">";
				to_write <- to_write + "\n<Parameters>" + "\n";
				to_write <- to_write + "<Parameter name=\"type_explo\" type=\"STRING\" value=\""+ type_explo+"\" />\n";
				to_write <- to_write + "<Parameter name=\"nb_fake_news\" type=\"STRING\" value=\""+ num+"\" />\n";
				to_write <- to_write + "<Parameter name=\"save_result_in_csv\" type=\"STRING\" value=\""+ save_result_in_csv+"\" />\n";
				to_write <- to_write + "</Parameters>" + "\n";
				to_write <- to_write + "<Outputs>" + "\n";
				to_write <- to_write + "</Outputs>" + "\n";
				
				to_write <- to_write + "</Simulation>" + "\n";
					
			}
			
			
		}
		
		to_write <- to_write + "</Experiment_plan>" + "\n";
		save to_write to: "FloodMines_" +type_explo+".xml" type:text;
	}
}

experiment stochasticity_file  {
	action _init_ {
		create simulation with: [nb_replication::500, type_explo::"stochasticity",save_result_in_csv::true, nb_fake_news::[0] ];
	}	
}

experiment test_scenarios_file  {
	action _init_ {
		create simulation with: [nb_replication::100, type_explo::"normal",save_result_in_csv::true,nb_fake_news::[100,50,10,5,0] ];
	}	
}

/* Insert your model definition here */

