$j(document).ready(function($j){	

	$j.ajax({
		//url:'https://az1.qualtrics.com/WRQualtricsControlPanel/File.php?F=F_bCxIEaQvjbauFKt',
		url:'world_thousandile.csv',
		success: function(data){
			var actuel = charge(data);
			
			var N_r;
			var rdb = 12*800;	
			var N_q = 800;		
			var X = 50.1;	
			var max=	100000000;						
			correct_parameters(actuel);
			var graphe = new cfx.Chart();
			var futur = courbe_reference(actuel, rdb, N_q, N_r, max);
			ajuste(actuel, futur, rdb, N_q, N_r, max);
			trace(graphe, actuel, futur);
			
			$j('#Slider_transfert_').html("Transfert : " + Math.round(10 - X/10));
			var ecart = diff(actuel, futur);
			var transfert = integrale(ecart, 0, N_q) / integrale(actuel,0,1000);
			var slider_rdb, slider_N_q, slider_transfert;
			slider_rdb = new dhtmlXSlider({
					parent: "Slider_rdb",
					skin: "dhx_skyblue",
					min: 0,
					max: 2000,
					step: 100,
					size: 430,
					tooltip: true,
					vertical: false,
					value: rdb/12
				});       
			slider_N_q = new dhtmlXSlider({
					parent: "Slider_N_q",
					skin: "dhx_skyblue",
					min: 0,
					max: 100,
					step: 1,
					size: 430,
					tooltip: true,
					vertical: false,
					value: N_q/10
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
					value: 10 - X/10
				});                
			slider_rdb.attachEvent("onChange",function(pos,slider){
				rdb = pos*12;
				N_q = slider_N_q.getValue()*10;
				X = 100 - slider_transfert.getValue() * 10;
				correct_parameters(actuel);
				futur = courbe_reference(actuel, rdb, N_q, N_r, max);
				ajuste(actuel, futur, rdb, N_q, N_r, max);
				maj(graphe, actuel, futur);
			});
			slider_N_q.attachEvent("onChange",function(pos,slider){
				N_q = pos*10;
				rdb = slider_rdb.getValue()*12;
				X = 100 - slider_transfert.getValue() * 10;
				correct_parameters(actuel);
				futur = courbe_reference(actuel, rdb, N_q, N_r, max);
				ajuste(actuel, futur, rdb, N_q, N_r, max);
				maj(graphe, actuel, futur);
			});
			slider_transfert.attachEvent("onChange",function(pos,slider){
				X = 100 - pos * 10;
				rdb = slider_rdb.getValue()*12;
				N_q = slider_N_q.getValue()*10;
				correct_parameters(actuel);
				futur = courbe_reference(actuel, rdb, N_q, N_r, max);
				ajuste(actuel, futur, rdb, N_q, N_r, max);
				maj(graphe, actuel, futur);
			});
			
			function charge(data) {
				var tab=data.split('\n');
				var avant = new Array(1001);
				for (var i = 1; i<=1000; i++) {	avant[i] = parseInt(tab[i].split(';')[1]);	}
				avant[0] = parseInt(tab[1].split(';')[1]);
				return avant
			}
		
			function revenu(donnees, q) {
				return (Math.floor(q) == q) ? donnees[q] : (donnees[Math.floor(q)] * (Math.ceil(q)-q) + donnees[Math.ceil(q)] * (q - Math.floor(q)))
			}
		
			function correct_parameters(avant) {
				N_r = revenu(avant, N_q);
				if (rdb > N_r) { 
					rdb = N_r;
				}
				if (N_r > max) { 
					N_r = (max + N_r) / 2; 
					max = (max + N_r) / 2; 
					while (revenu(avant, N_q) > N_r & 1 < N_q) { N_q -= 1; }
					N_r = revenu(avant, N_q);
					max = N_r;
				}
			}
		
			function courbe_reference(avant, rdb, N_q, N_r, max) {
		
				var apres = new Array(1001);
				apres[0] = Math.max(rdb, avant[0]);
				var spread = apres[0] - avant[0];
				for (i=1; i<N_q; i++) { apres[i] = Math.max(rdb, Math.min(avant[i] + spread, (i/ (N_q)) * (N_r - rdb) + rdb)); }
				var j = N_q-1;
				while (((N_r - apres[j])/(N_q - j) <= (apres[j] - apres[j-1])/1 + 0.000000001) & j > 0) { j--; }
				if (j == 0) { for (i=0; i < N_q; i++) { apres[i] = Math.max(rdb, avant[i]); } }
				else { for (i=j; i<N_q; i++) { apres[i] = Math.max(avant[i], ((i-j+1 )/ (N_q-j+1)) * (N_r - apres[j-1]) + apres[j-1]); } }
				for (i = 0; i < N_q; i++) { apres[i] = Math.max(avant[i], Math.min(apres[i], (i/ (N_q)) * (N_r - rdb) + rdb))}
				for (i=1; i<N_q; i++) { 
					apres[i] = Math.min(avant[i] + spread, apres[i]);
					spread = Math.min(spread, apres[i] - avant[i]);		}
				for (i=N_q; i<1001; i++) { apres[i] = Math.min(avant[i], max); }
				return apres;
			}

			function affiche(rdb, N_q) {		
				$j('#Slider_rdb_').html("Revenu de base : " + Math.round(rdb/12) + "€/mois");
				$j('#Slider_N_q_').html("Proportion avantagée : " + Math.round(N_q/10) + "%");
				$j('#Slider_transfert_').html("Transfert : " + Math.round(10 - X/10));
			}			
			
			function ajuste(avant, apres, rdb, N_q, N_r, max) {
				var D = deficit(avant, apres);
				if (D >= 0) {
					var G = economisable(avant, apres, "g", rdb, N_q, N_r, max);
					var R = economisable(avant, apres, "d", rdb, N_q, N_r, max); 
					if (D > G + R) { 
						N_q -= 30; 
						N_r = revenu(avant, N_q);
						G = economisable(avant, apres, "g", rdb, N_q, N_r, max);
						R = economisable(avant, apres, "d", rdb, N_q, N_r, max);
						futur = courbe_reference(avant, rdb, N_q, N_r, max);
					} 
					if (D > G + R) { // déficit trop grand
						var min_N = 0;
						while (revenu(avant, min_N) < rdb & min_N < N_q+1) { min_N += 1; }
							rdb = dichotomie(avant, apres, "g", rdb, N_q, N_r, max, min_N); 
							futur = courbe_reference(avant, rdb, N_q, N_r, max);
							D = deficit(avant, apres);
						// }
						G = economisable(avant, apres, "g", rdb, N_q, N_r, max);
						R = economisable(avant, apres, "d", rdb, N_q, N_r, max);
					} 
					var P = D / (G + R);
					if (X == 50.1) { 
						X = Math.min(99, 200* G / (G + R)); 
					}
					var P_G = Math.max((D-R)/G, Math.min(P*X*(G+R)/(100*G), D/G, 1));
					var P_R = (R != 0.000000001) * (D - P_G * G) / R;
					for (i=0; i<N_q; i++) { futur[i] -= (G != 0.000000001) * P_G * (futur[i] - Math.max(rdb, avant[i])); }
					futur = ajuste_droite(avant, futur, N_q, N_r, max, P_R, P_R * R);  			
				}
				else { // excédent
					var q = 2 * D / (N_r - rdb + 0.000000001)
					for (i=Math.max(0, Math.ceil(N_q + q)); i<N_q; i++) { apres[i] = N_r; }
					for (i=0; i<Math.min(1001, Math.ceil(N_q + q)); i++) { apres[i] = (N_r - rdb) * i / (N_q + q + 0.000000001) + rdb; }
				}
				affiche(rdb, N_q) ;
			}
		
			function trace(graph, avant, apres) {
				var courbes = new Array(1001);
				for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/12, avant: avant[i]/12 };	}
				graph.setGallery(cfx.Gallery.Lines);
				graph.setDataSource(courbes);	
				graph.getAxisY().setMax(8000);
				graph.getSeries().getItem(0).setMarkerShape(cfx.MarkerShape.None);
				graph.getSeries().getItem(1).setMarkerShape(cfx.MarkerShape.None);
				graph.getSeries().getItem(1).setColor("#FF0000");
				graph.getSeries().getItem(0).setColor("#00FF00");
				graph.getSeries().getItem(1).setText("revenus mensuels actuels    (en €)");
				graph.getSeries().getItem(0).setText("revenus mensuels après la réforme proposée");
				graph.getLegendBox().setDock(cfx.DockArea.Top);
				titreX = new cfx.TitleDockable();
				titreX.setText("Revenus après impôts et transferts des adultes français, du plus pauvre au plus riche");
				titreX.setTextColor("#555555");
				graph.getAxisX().setTitle(titreX);
				graph.getAxisX().setStep(10000);
				graph.getAxisY().setStep(1000);
				graph.getAxisX().setMinorStep(100);
				graph.getAxisY().setMinorStep(250);
				graph.getAxisY().getGrids().getMinor().setStyle(cfx.DashStyle.Dot);
				graph.getAxisX().getGrids().getMinor().setVisible(true);
				graph.getAxisY().getGrids().getMinor().setVisible(true);
				var divHolder = document.getElementById("graphe_reforme");
				graph.create(divHolder);
			}
		
			function maj(graph, avant, apres) {
				var courbes = new Array(1001);
				for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/12, avant: avant[i]/12 };	}
				graph.setDataSource(courbes);					
				graph.getSeries().getItem(1).setText("revenus mensuels actuels    (en €)");
				graph.getSeries().getItem(0).setText("revenus mensuels après la réforme proposée");
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
		
			function deficit(avant, apres) { return integrale(apres, 0, 1000) - integrale(avant, 0, 1000); }
		
			function diff(avant, apres) {
				var n = avant.length;
				var d = new Array(n);
				for (i=0; i<n; i++) { d[i] = apres[i] - avant[i]; }
				return d
			}
		
			function economisable(avant, apres, cote, rdb, N_q, N_r, max) {
				var min_q = (cote == "g") ? 0 : N_q;
				var max_q = (cote == "g") ? N_q : 1000;
				var black = new Array(max_q - min_q + 1);
				for (i=min_q; i<max_q + 1; i++) { 
					black[i] = (cote =="g") ? apres[i] - Math.max(rdb, avant[i]) : apres[i] - N_r;
				}
				return Math.max(0.000000001, integrale(black, min_q, max_q));
			}
		
			function ajuste_droite(avant, apres, N_q, N_r, max, P, D_R) {
				var Y = Math.max(1000, max - N_r);
				var max_q = N_q;
				var j = N_q;
				while ((avant[j] < max) & (j < 1000)) { max_q += 1; j+= 1; }
				var R_G = 0;
				for (i=N_q; i<max_q+1; i++) { R_G += apres[i] - N_r; }
				var z = 1000 - max_q;
				var result = apres;
				var x = D_R / (R_G + Y * z / 2);
				if (x <= 1 || z == 0) {
					for (i=N_q; i< max_q+1; i++) { result[i] -= P * (apres[i] - N_r); }
					for (i=max_q+1; i<1001; i++) { result[i] -= P * Y * (1000 - i) / z }
				}
				else {
					p = (2 * (D_R - R_G) - Y * z) / Y;
					p = p + (p == z) * 0.000000001
					for (i=N_q; i<Math.ceil(max_q+p); i++) { result[i] = N_r;}
					for (i=Math.ceil(max_q+p); i<1001; i++) { result[i] = N_r + (i - p - max_q) * Y / (z - p); }
				}
			return result;
			}
		
			function dichotomie(avant, apres, cote, rdb, N_q, N_r, max, min_N) {
				var precision = 100;
				var before = avant;
				var after = apres;
				var incr = 1 / 2;
				var rdb_d = (cote == "g") ? rdb * incr : rdb;
				var N_q_d = (cote == "d") ? N_q - (N_q - min_N) * incr : N_q;
				var cible = economisable(before, after, "d", rdb_d, Math.ceil(N_q_d), revenu(before, N_q_d), max) + economisable(before, after, "g", rdb_d, Math.ceil(N_q_d), revenu(before, N_q_d), max) - deficit(before, after);
				while (Math.abs(cible)>precision & incr > 0.00000000001) {
					incr/=2;
					(cible>0) ? res = incr : res = -incr ;
					rdb_d += (cote == "g") ? res * rdb : 0;
					N_q_d += (cote == "d") ? res * (N_q - min_N) : 0;
					after = courbe_reference(before, rdb_d, Math.ceil(N_q_d), N_r, max)
					cible = economisable(before, after, "d", rdb_d, Math.ceil(N_q_d), revenu(before, N_q_d), max) + economisable(before, after, "g", rdb_d, Math.ceil(N_q_d), revenu(before, N_q_d), max) - deficit(before, after); 
					if (incr < 0.00000000001) { 
						cible=0; 
						N_q_d = min_N;
						rdb_d = 0;
					}
				}
				res = (cote == "d") ? Math.floor(N_q_d) : rdb_d;
				return res;
			}

		},
		error: function(){ alert('Les données n\'ont pas pu être chargées');}
	});

});

