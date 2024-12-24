Qualtrics.SurveyEngine.addOnload(function()
{
	
$j(document).ready(function($j){	
	
	var couple = "${q://QID34/ChoiceGroup/SelectedChoices}"
	var income = "${q://QID30/ChoiceTextEntryValue}"; 
	if (couple == "Yes" || couple == "はい" || couple == "Oui" || couple == "Ja" || couple == "Si" || couple == "Да" || couple == "Sì" || couple == "Sí" || couple == "Tak" || couple == "نعم" || couple == "そうだ") { income = income/2; }
	var variant_sliders = "${e://Field/variant_sliders}";

	var non_losers, Nd_r, winners, Na_r, degree;
	winners = 60*10+1;
	non_losers = 1000-20*10-1;
	degree = 2+.1;
	if (variant_sliders == 1) {
		winners = 40*10+1;
		non_losers = 1000-10*10-1;
		degree = 7+.1;
	}
	
	
	$j.ajax({
		url:"https://wumarketing.eu.qualtrics.com/ControlPanel/File.php?F=F_yuKEs6pwxsAid44",
		success: function(params){
			
		const language = Qualtrics.SurveyEngine.getEmbeddedData("Q_Language"); 
			
		const rows = params.split('\n').map(row => row.split(';'));
        const headers = rows[0];
        const col = headers.indexOf(language);

        if (col === -1) {
            console.error("Column 'language' not found in the CSV file.");
            return;
        }

        // Iterate through the rows and create variables dynamically
        for (let i = 1; i < rows.length; i++) {
            const name = rows[i][0]; // Assuming the row name is in the first column
            const value = rows[i][col];
            window[name] = value; 
            console.log(`Variable ${name} created with value: ${value}`);
        }
			/*
		const rows = params.split("\n").map(row => row.split(";"));

		const headers = rows[0]; // First row as headers
		const countryIndex = headers.indexOf(country);

		if (countryIndex === -1) {
			console.error(`Column for country "${country}" not found.`);
			return;
		}

		const rowNames = rows.slice(1).map(row => row[0]); // First column as row names
		const values = rows.slice(1).map(row => row[countryIndex]); // Extract country-specific column values

		const now_10k = values[rowNames.indexOf("now_10k")];
		const now_40k = values[rowNames.indexOf("now_40k")];
		const now_60k = values[rowNames.indexOf("now_60k")];
		const now_100k = values[rowNames.indexOf("now_100k")];
		unit = values[rowNames.indexOf("unit")];
			*/
	$j.ajax({
		url:"https://wumarketing.eu.qualtrics.com/ControlPanel/File.php?F=F_TkO3EgHL6SYGfxJ",
		success: function(data){
			var actuel = charge(data);
			income = Math.min(income, actuel[1000]);
			correct_parameters(actuel);
			var graphe = new cfx.Chart();
			futur = courbe_reference(actuel);
			ajuste(actuel);
			trace(graphe, actuel, futur);
			
			var slider_transfert, slider_N_q;
			slider_N_q = new dhtmlXSlider({
				parent: "Slider_N_q",
				step: 1,
				min: 0,
				max: 100,
				value: [winners/10, non_losers/10],
				range: true,
				tooltip: true,
				size: 430,
				vertical: false,
				skin: "dhx_skyblue"
			});
			slider_transfert = new dhtmlXSlider({
					parent: "Slider_transfert",
					skin: "dhx_skyblue",
					min: 0,
					max: 10,
					step: 1,
					size: 430,
					tooltip: true,
					vertical: false,
					value: degree
				});    
			slider_N_q.attachEvent("onChange",function(pos,slider){
				non_losers = pos[1]*10;
				winners = pos[0]*10;
				degree = slider_transfert.getValue();
				correct_parameters(actuel);
				futur = courbe_reference(actuel);
				ajuste(actuel);
				maj(graphe, actuel, futur);
			});
			slider_transfert.attachEvent("onChange",function(pos,slider){
				degree = pos;
				winners = slider_N_q.getValue()[0]*10;
				non_losers = slider_N_q.getValue()[1]*10;
				correct_parameters(actuel);
				futur = courbe_reference(actuel);
				ajuste(actuel);
				maj(graphe, actuel, futur);
			});            
								
			function charge(data) {
				var tab=data.split('\n');
				var avant = new Array(1001);
				for (var i = 1; i<=1000; i++) {	avant[i] = parseInt(tab[i].split(';')[1]);	}
				avant[0] = parseInt(tab[1].split(';')[1]);
				return avant
			}
			
			function interpole(rev, avant, apres) {
				var e = 0
				while (avant[e]<rev & e < 1001) {     e++;    }
				if (e == 1001) { return apres[1000] }
				else if (e == 0) {return apres[0] }
				     else { return ((apres[e]-apres[e-1])*(rev-avant[e-1])/(avant[e]-avant[e-1])+apres[e-1]) }
			}
			
			function trace(graph, avant, apres) {
				var courbes = new Array(1001);
				for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/unit, avant: avant[i]/unit };	}
				graph.setGallery(cfx.Gallery.Lines);
				graph.setDataSource(courbes);	
				graph.getAxisY().setMax(100000/unit);
				graph.getSeries().getItem(0).setMarkerShape(cfx.MarkerShape.None);
				graph.getSeries().getItem(1).setMarkerShape(cfx.MarkerShape.None);
				graph.getSeries().getItem(1).setText(text_current.trim() + " (" + text_unit +")");
				graph.getSeries().getItem(0).setText(text_after.trim());
				graph.getLegendBox().setDock(cfx.DockArea.Top);
				titreX = new cfx.TitleDockable();
				// titreX.setText("Revenus après impôts et transferts des humains adultes, du plus pauvre au plus riche");
				titreX.setText(text_title.trim());
				titreX.setTextColor("#555555");
				graph.getAxisX().setTitle(titreX);
				graph.getAxisX().setStep(10000);
				graph.getAxisY().setStep(step_major);
				graph.getAxisX().setMinorStep(100);
				graph.getAxisY().setMinorStep(step_minor);
				graph.getAxisY().getGrids().getMinor().setStyle(cfx.DashStyle.Dot);
				graph.getAxisX().getGrids().getMinor().setVisible(true);
				graph.getAxisY().getGrids().getMinor().setVisible(true);
				var divHolder = document.getElementById("graphe");
				graph.create(divHolder);
			}
			
			function integrale(f, a, b) {
				if (b < a) { var sum = - integrale(f, b, a); }
				else {
					var sum = 0;
					for (i=Math.ceil(a); i<Math.floor(b); i++) { sum += f[i]; }
					sum += (Math.ceil(a) - a) * f[a] + (b - Math.floor(b)) * f[b];
				}
				return sum;
			}				
			
			function revenu(donnees, q) {
				return (Math.floor(q) == q) ? donnees[q] : (donnees[Math.floor(q)] * (Math.ceil(q)-q) + donnees[Math.ceil(q)] * (q - Math.floor(q)))
			}
							
			function maj(graph, avant, apres) {
				var courbes = new Array(1001);
				for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/unit, avant: avant[i]/unit };	}
				graph.setDataSource(courbes);		
				graph.getSeries().getItem(1).setText(text_current.trim() + " (" + text_unit +")");
				graph.getSeries().getItem(0).setText(text_after.trim());
			}
							
			function economisable(avant, cote) {
				var min_q = (cote == "g") ? 0 : non_losers;
				var max_q = (cote == "g") ? winners : 1000;
				var black = new Array(max_q - min_q + 1);
				for (i=min_q; i<max_q + 1; i++) { 
					black[i] = (cote =="g") ? Na_r - avant[i] : avant[i] - Nd_r;
				}
				return Math.max(0.000000001, integrale(black, min_q, max_q));
			}		
			
			function correct_parameters(avant) {
				Na_r = revenu(avant, winners);
				Nd_r = revenu(avant, non_losers);
				$j('#Slider_N_q_').html(text_benefit.trim() + " " + Math.round(winners/10) + "% &emsp;&emsp;&emsp; " + text_lose.trim() + " " + Math.round(100 - non_losers/10) + "%");
				// $j('#Slider_winners_').html("Proportion avantagée : " + winners/10 + "%");
				// $j('#Slider_non_losers_').html("Proportion désavantagée : " + (100 - non_losers/10) + "%");
				$j('#Slider_transfert_').html(text_degree.trim() + " " + Math.round(degree));
			}
		
			function courbe_reference(avant) {
				var apres = new Array(1001);
				for (i=0; i<winners; i++) { apres[i] = Math.max(avant[i], Na_r);	}
				for (i=winners; i<non_losers; i++) { apres[i] = avant[i];	}
				for (i=non_losers; i<1001; i++) { apres[i] = Math.min(avant[i], Nd_r); } 
				return apres;
			}
		
			function ajuste(avant) {
				var G = economisable(avant, "g");
				var R = economisable(avant, "d"); 
				var min_1 = (G <= R) ? 0 : non_losers;
				var max_1 = (G <= R) ? winners : 1001; 
				var min_2 = (G <= R) ? non_losers : 0;
				var max_2 = (G <= R) ? 1001 : winners; 
				rdb = 2*(Math.min(G,R)*(degree/10)+integrale(avant, 0, winners))/winners-Na_r
				var apres = new Array(1001);
				var affine = 1;
				for (i=0;i<winners;i++) {
					apres[i] = rdb + (i/winners)*(Na_r-rdb);
					if (apres[i]<avant[i]) { affine = 0; }
					if ((apres[i-1]-avant[i-1]) < (apres[i]-avant[i])) { affine = 0; 
					} // new
				}
				for (i=min_1; i<max_1; i++) { futur[i] -= (10-degree)/10 * (futur[i] - avant[i]); }
				for (i=min_2; i<max_2; i++) { futur[i] -= (1 - Math.min(G/R, R/G)) * (futur[i] - avant[i]); }
				for (i=min_2; i<max_2; i++) { futur[i] -= (10-degree)/10 * (futur[i] - avant[i]); }
				if (affine) { for (i=0;i<winners;i++) { futur[i] = apres[i]; }	}
				var diff = new Array(winners);
				for (i=0; i<winners; i++) { diff[i] = futur[i] - avant[i] ; }
				var econ = integrale(diff, 0, winners-1) + economisable(futur, "d");
				// $j('#out').html((integrale(avant,0,1000)-integrale(futur,0,1000))/integrale(avant,0,1000)+"  "+G/R+"  "+futur[0]/12);
				// $j('#out').html(Math.round(1000*(integrale(futur,0,winners)-integrale(avant,0,winners))/integrale(avant,0,1000))/10+"  "+Math.round(1000*(integrale(avant,non_losers,1000)-integrale(futur,non_losers,1000))/integrale(avant,0,1000))/10+"  "+Math.round(futur[0]/12));
				$j('#after_0').html(Math.round(interpole(0, avant, futur)).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#now_10k').html(Math.round(now_10k).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#now_40k').html(Math.round(now_40k).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#now_60k').html(Math.round(now_60k).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#now_100k').html(Math.round(now_100k).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#after_10k').html(Math.round(interpole(now_10k, avant, futur)).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#after_40k').html(Math.round(interpole(now_40k, avant, futur)).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#after_60k').html(Math.round(interpole(now_60k, avant, futur)).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#after_100k').html(Math.round(interpole(now_100k, avant, futur)).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#own').html(Math.round(income).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('#after_own').html(Math.round(interpole(income, avant, futur)).toString().replace(/\B(?=(\d{3})+(?!\d))/g, "&nbsp;"));
				$j('.text_unit').html(text_unit);
				// $j('#out').html(1000-non_losers);

				Qualtrics.SurveyEngine.setEmbeddedData("custom_redistr_winners", winners);
				Qualtrics.SurveyEngine.setEmbeddedData("custom_redistr_non_losers", 1000-non_losers);
				Qualtrics.SurveyEngine.setEmbeddedData("custom_redistr_degree", degree);
			}
			
		},
		error: function(){ alert('Data could not be loaded');}
	});
			},
		error: function(){ alert('Data could not be loaded');}
	});


});



});