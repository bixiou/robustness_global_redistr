	var couple = 0; //"${q://QID34/ChoiceGroup/SelectedChoices}"
	var income = 40000; //"${q://QID30/ChoiceTextEntryValue}"; 
	if (couple == "Yes" || couple == "はい" || couple == "Oui" || couple == "Ja" || couple == "Si" || couple == "Да" || couple == "Sì" || couple == "Sí" || couple == "Tak" || couple == "نعم" || couple == "そうだ") { income = income/2; }
	
	$j.ajax({
		url:"https://wumarketing.eu.qualtrics.com/ControlPanel/File.php?F=F_DdsIUHxEWs7YXfL", // in PPP 2024 $ 
		success: function(params){
			
		const language = "EN"; //Qualtrics.SurveyEngine.getEmbeddedData("Q_Language"); 
		
		const rows = params.split('\n').map(row => row.split('|'));
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

	$j.ajax({
		url:"interactive_graph/world_disposable_inc.csv", // https://wumarketing.eu.qualtrics.com/ControlPanel/File.php?F=F_TkO3EgHL6SYGfxJ
		success: function(data){
	$j.ajax({
		url:"interactive_graph/mean_custom_redistr/all_satisfied_touched.csv", // 
		success: function(data_apres){
			var graph_width = $j('#graphe').width();
			var graph_height = $j('#graphe').height();
			if (window.innerWidth < 500) {
				graph_height = graph_width*1.4;
				size_slider = window.innerWidth - 30;
				$j('#graphe').height(graph_height);
			}
			
			var actuel = charge(data);
			income = Math.min(income, actuel[1000]);
			//correct_parameters(actuel);
			var graphe = new cfx.Chart();
			futur = charge(data_apres);
			//ajuste(actuel);
			trace(graphe, actuel, futur);		

            var winners = 0; 
            var non_losers = 0;
            var degree= 0;
								
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
				for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i], avant: avant[i] };	}
				graph.setGallery(cfx.Gallery.Lines);
				graph.setDataSource(courbes);	
				graph.getAxisY().setMax(100000/(unit*period_custom));
				graph.getSeries().getItem(0).setMarkerShape(cfx.MarkerShape.None);
				graph.getSeries().getItem(1).setMarkerShape(cfx.MarkerShape.None);
				graph.getSeries().getItem(1).setText(text_current.trim() + " (" + text_unit +")");
				graph.getSeries().getItem(0).setText(text_after.trim());
				graph.getLegendBox().setDock(cfx.DockArea.Top);
				//graph.getLegendBox().setFont("32px Arial");
				titreX = new cfx.TitleDockable();
				// titreX.setText("Revenus après impôts et transferts des humains adultes, du plus pauvre au plus riche");
				titreX.setText(text_title_x.trim());
				//titreX.setTextColor("#555555");
				//titreX.setFont("60px Arial");
				graph.getAxisX().setTitle(titreX);
				titreY = new cfx.TitleDockable();
				titreY.setText(text_title_y.trim());
				//titreY.setTextColor("#555555");		
				graph.getAxisY().setTitle(titreY);		
				graph.getAxisX().setStep(10000);
				graph.getAxisX().setMinorStep(100);
				graph.getAxisX().getGrids().getMinor().setVisible(true);
				
				if (window.innerWidth < 500) { 
					titreX.setText(text_title_x.trim());	
					titreX.setDock(cfx.DockArea.Left);
					graph.getLegendBox().setPlotAreaOnly(false);
				}
				
				graph.getAxisY().setStep(step_major);
				graph.getAxisY().setMinorStep(step_minor);
				graph.getAxisY().getGrids().getMinor().setStyle(cfx.DashStyle.Dot);
				graph.getAxisY().getGrids().getMinor().setVisible(true);
				graph.getAxisY().getLabelsFormat().setCustomFormat("N");
				/*
				graph.getPlotAreaMargin().setTop(1);
				graph.getPlotAreaMargin().setBottom(1);
				graph.getPlotAreaMargin().setRight(1);
				graph.getPlotAreaMargin().setLeft(1);
				*/
				var divHolder = document.getElementById("graphe");
				graph.create(divHolder);
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
				$j('#couple').html(couple);
				$j('#income_exact').html(income_exact);
				$j('#satisfied').html(satisfied);
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
										
			function economisable(avant, cote) {
				var min_q = (cote == "g") ? 0 : non_losers;
				var max_q = (cote == "g") ? winners : 1000;
				var black = new Array(max_q - min_q + 1);
				for (i=min_q; i<max_q + 1; i++) { 
					black[i] = (cote =="g") ? Na_r - avant[i] : avant[i] - Nd_r;
				}
				return Math.max(0.000000001, integrale(black, min_q, max_q));
			}		
			
		},
		error: function(){ alert('Data could not be loaded');}
	});
			},
		error: function(){ alert('Data could not be loaded');}
	});
			},
		error: function(){ alert('Data could not be loaded');}
	});