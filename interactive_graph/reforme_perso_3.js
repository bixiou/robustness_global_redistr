// /!\ In 2024 I have changed "ajuste" (which makes it almost like "ajuste2") to correct the following pb of the old algorithm: when advantage (Na_q) is high, lowest income may receive less than low-middle income and situation gets worse when transfer increases (!) e.g. Na_q=75, Nd_q=10, Transfer=5-6, for world_thousandile (post_transfer_inc_mean). The pb was due to redistribution being too often affine. In ajuste2 it is never affine.

$j(document).ready(function($j){	
	
	var Nd_q, Nd_r, Na_q, Na_r, max;
	Nd_q = 95*10;
	Na_q = 33*10;
	max = 100000*12;
	var X = 0;
	var aid = 0 * 201938;
	
	$j.ajax({
		url:'../data/world_disposable_inc.csv',
		success: function(data){  										
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
				Na_r = revenu(avant, Na_q);
				Nd_r = revenu(avant, Nd_q);
				if (Nd_r > max) { 
					Nd_r = (max + Nd_r) / 2; 
					max = (max + Nd_r) / 2; 
					while (revenu(avant, Nd_q) > Nd_r & 1 < Nd_q) { Nd_q -= 1; }
					Nd_r = revenu(avant, Nd_q);
					max = Nd_r;
				}
				$j('#Slider_N_q_').html("Proportion avantagée : " + Na_q/10 + "% &emsp;&emsp;&emsp; Proportion désavantagée : " + (100 - Nd_q/10) + "%");
				// $j('#Slider_Na_q_').html("Proportion avantagée : " + Na_q/10 + "%");
				// $j('#Slider_Nd_q_').html("Proportion désavantagée : " + (100 - Nd_q/10) + "%");
				$j('#Slider_transfert_').html("Transfert : " + Math.round(10 - X));
			}

			var actuel = charge(data);
			correct_parameters(actuel);
			var graphe = new cfx.Chart();
			var graphe_top = new cfx.Chart();

			$j.ajax({
				url:'../data/world_pretax_inc.csv', // world_post_transfer_inc world_pretax_inc
				success: function(data){
					futur = courbe_reference(actuel, max);

					var other = charge(data);
					ajuste(actuel, max);
					trace(graphe, actuel, futur, other);

					var top = 3;
					trace_top(graphe_top, actuel, futur, other, top);
					
					var slider_transfert, slider_N_q;
					slider_N_q = new dhtmlXSlider({
						parent: "Slider_N_q",
						step: 1,
						min: 0,
						max: 100,
						value: [Na_q/10, Nd_q/10],
						range: true,
						tooltip: true,
						size: 430,
						vertical: false,
						skin: "dhx_skyblue"
					});
					// slider_transfert = new dhtmlxSlider("Slider_transfert", {
					slider_transfert = new dhtmlXSlider({
							parent: "Slider_transfert",
							skin: "dhx_skyblue",
							min: 0,
							max: 10,
							step: 1,
							size: 430,
							tooltip: true,
							vertical: false,
							value: 10 - X
						});    
					slider_N_q.attachEvent("onChange",function(pos,slider){
						Nd_q = pos[1]*10;
						Na_q = pos[0]*10;
						X = 10 - slider_transfert.getValue();
						correct_parameters(actuel);
						futur = courbe_reference(actuel, max);
						ajuste(actuel, max);
						maj(graphe, actuel, futur, other);
						maj_top(graphe_top, actuel, futur, other, top);
					});
					slider_transfert.attachEvent("onChange",function(pos,slider){
						X = 10 - pos;
						// Na_q = slider_Na_q.getValue()*10;
						// Nd_q = 1000 - slider_Nd_q.getValue()*10;
						Na_q = slider_N_q.getValue()[0]*10;
						Nd_q = slider_N_q.getValue()[1]*10;
						correct_parameters(actuel);
						futur = courbe_reference(actuel, max);
						ajuste(actuel, max);
						maj(graphe, actuel, futur, other);
						maj_top(graphe_top, actuel, futur, other, top);
					});       
					
					function interpole(rev, avant, apres, rdb) {
						var e = 0
						while (avant[e]<rev & e < 1001) {     e++;    }
						if (e == 1001) { return max }
						else if (e == 0) { return ((apres[0]-rdb)*rev/avant[0] + rdb) }
							else { return ((apres[e]-apres[e-1])*(rev-avant[e-1])/(avant[e]-avant[e-1])+apres[e-1]) }
					}
					
					function trace(graph, avant, apres, other) {
						var courbes = new Array(1001);
						for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/12, avant: avant[i]/12, other: other[i]/12 };	}
						graph.setGallery(cfx.Gallery.Lines);
						graph.setDataSource(courbes);	
						graph.getAxisY().setMax(8000);
						graph.getSeries().getItem(0).setMarkerShape(cfx.MarkerShape.None);
						graph.getSeries().getItem(1).setMarkerShape(cfx.MarkerShape.None);
						graph.getSeries().getItem(2).setMarkerShape(cfx.MarkerShape.None);
						graph.getSeries().getItem(1).setText("current monthly income");
						graph.getSeries().getItem(0).setText("monthly income after your custom reform");
						graph.getSeries().getItem(2).setText("monthly income after the proposed reform");
						// graph.getSeries().getItem(1).setText("revenus mensuels selon la taxation optimale otheritarienne    (en €)");
						// graph.getSeries().getItem(2).setText("revenus mensuels actuels");
						// graph.getSeries().getItem(0).setText("revenus mensuels selon la réforme proposée moyenne");
						graph.getLegendBox().setDock(cfx.DockArea.Top);
						titreX = new cfx.TitleDockable();
						titreX.setText("Income after taxes and transfers of French adults, from the poorest to the richest");
						// titreX.setText("Revenus disponibles des adultes français, du plus pauvre au plus riche");
						titreX.setTextColor("#555555");
						graph.getAxisX().setTitle(titreX);
						graph.getAxisX().setStep(10000);
						graph.getAxisY().setStep(1000);
						graph.getAxisX().setMinorStep(100);
						graph.getAxisY().setMinorStep(250);
						graph.getAxisY().getGrids().getMinor().setStyle(cfx.DashStyle.Dot);
						graph.getAxisX().getGrids().getMinor().setVisible(true);
						graph.getAxisY().getGrids().getMinor().setVisible(true);
						//graph.getLegendBox().setMarginX(30);
						//graph.setBackColor('#FFFFFF');
						var divHolder = document.getElementById("graphe");
						// var divHolder = document.getElementById("graphe");
						graph.create(divHolder);
					}
		

					function trace_top(graph, avant, apres, other, top) {
						var courbes = new Array(10*top+1);
						for (i=0; i<10*top+1; i++) { courbes[i] = { apres: apres[i+1000-10*top]/12, avant: avant[i+1000-10*top]/12, other: other[i+1000-10*top]/12 };	}
						graph.setGallery(cfx.Gallery.Lines);
						graph.setDataSource(courbes);	
						graph.getAxisY().setMax(30000);
						graph.getSeries().getItem(0).setMarkerShape(cfx.MarkerShape.None);
						graph.getSeries().getItem(1).setMarkerShape(cfx.MarkerShape.None);
						graph.getSeries().getItem(2).setMarkerShape(cfx.MarkerShape.None);
						graph.getSeries().getItem(1).setText("current monthly income");
						graph.getSeries().getItem(0).setText("monthly income after your custom reform");
						graph.getSeries().getItem(2).setText("monthly income after the proposed reform");
						// graph.getSeries().getItem(1).setText("revenus mensuels selon la taxation optimale otheritarienne    (en €)");
						// graph.getSeries().getItem(2).setText("revenus mensuels actuels");
						// graph.getSeries().getItem(0).setText("revenus mensuels selon la réforme proposée moyenne");
						graph.getLegendBox().setDock(cfx.DockArea.Top);
						titreX = new cfx.TitleDockable();
						titreX.setText("Income after taxes and transfers of French adults, from the poorest to the richest");
						// titreX.setText("Revenus disponibles des adultes français, du plus pauvre au plus riche");
						titreX.setTextColor("#555555");
						graph.getAxisX().setTitle(titreX);
						graph.getAxisX().setStep(10000);
						graph.getAxisY().setStep(5000);
						graph.getAxisX().setMinorStep(top);
						graph.getAxisY().setMinorStep(2500);
						graph.getAxisY().getGrids().getMinor().setStyle(cfx.DashStyle.Dot);
						graph.getAxisX().getGrids().getMinor().setVisible(true);
						graph.getAxisY().getGrids().getMinor().setVisible(true);
						//graph.getLegendBox().setMarginX(30);
						//graph.setBackColor('#FFFFFF');
						var divHolder = document.getElementById("graphe_top");
						// var divHolder = document.getElementById("graphe");
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
									
					function maj(graph, avant, apres, other) {
						var courbes = new Array(1001);
						for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/12, avant: avant[i]/12, other: other[i]/12 };	}
						graph.setDataSource(courbes);					
						graph.getSeries().getItem(1).setText("revenus mensuels actuels    (en €)");
						graph.getSeries().getItem(0).setText("revenus mensuels après la réforme proposée");
					}
									
					function maj_top(graph, avant, apres, other, top) {
						var courbes = new Array(10*top+1);
						for (i=0; i<10*top+1; i++) { courbes[i] = { apres: apres[i+1000-10*top]/12, avant: avant[i+1000-10*top]/12, other: other[i+1000-10*top]/12 };	}
						graph.setDataSource(courbes);					
						graph.getSeries().getItem(1).setText("revenus mensuels actuels    (en €)");
						graph.getSeries().getItem(0).setText("revenus mensuels après la réforme proposée");
					}
									
					function economisable(avant, cote) {
						var min_q = (cote == "g") ? 0 : Nd_q;
						var max_q = (cote == "g") ? Na_q : 1000;
						var black = new Array(max_q - min_q + 1);
						for (i=min_q; i<max_q + 1; i++) { 
							black[i] = (cote =="g") ? Na_r - avant[i] : avant[i] - Nd_r;
						}
						return Math.max(0.000000001, integrale(black, min_q, max_q));
					}		
				
					function courbe_reference(avant, max) {
						var apres = new Array(1001);
						for (i=0; i<Na_q; i++) { apres[i] = Math.max(avant[i], Na_r);	}
						for (i=Na_q; i<Nd_q; i++) { apres[i] = avant[i];	}
						for (i=Nd_q; i<1001; i++) { apres[i] = Math.min(avant[i], Nd_r, max); } 
						return apres;
					}
				
					function ajuste2(avant, max) {
						// more redistributive
						var G = economisable(avant, "g");
						var R = economisable(avant, "d");
						var min_1 = (G <= R) ? 0 : Nd_q;
						var max_1 = (G <= R) ? Na_q : 1001; 
						var min_2 = (G <= R) ? Nd_q : 0;
						var max_2 = (G <= R) ? 1001 : Na_q; 
						// for (i=0; i<Na_q; i++) { futur[i] -= aid * (futur[i] - avant[i]) / (G+R) ; } //* 1000/(Na_q*(R+G)*(1000-Nd_q+Na_q)); }
						// for (i=Nd_q; i<1001; i++) { futur[i] -= aid * (avant[i] - futur[i]) / (G+R) ; } // * 1000/(Na_q*(R+G)*(1000-Nd_q+Na_q)); }
						// G = G - G/(R+G) * aid; 
						// R = R - R/(R+G) * aid;
						for (i=min_1; i<max_1; i++) { futur[i] -= X/10 * (futur[i] - avant[i]); }
						for (i=min_2; i<max_2; i++) { futur[i] -= (1 - Math.min(G/R, R/G)) * (futur[i] - avant[i]); }
						for (i=min_2; i<max_2; i++) { futur[i] -= X/10 * (futur[i] - avant[i]); }
						var diff = new Array(Na_q);
						for (i=0; i<Na_q; i++) { diff[i] = futur[i] - avant[i] ; }
						var econ = integrale(diff, 0, Na_q-1) + economisable(futur, "d");
						for (i=0; i<Na_q; i++) { futur[i] -= Math.min(aid, econ) * (futur[i] - avant[i]) / econ ; }
						for (i=Nd_q; i<1001; i++) { futur[i] -= Math.min(aid, econ) * (futur[i] - futur[Nd_q]) / econ ; }
						//$j('#out').html((integrale(avant,0,1000)-integrale(futur,0,1000))/integrale(avant,0,1000)+"  "+G/R+"  "+futur[0]/12);
					}
				
					function ajuste(avant, max) {
						var G = economisable(avant, "g");
						var R = economisable(avant, "d"); 
						var min_1 = (G <= R) ? 0 : Nd_q;
						var max_1 = (G <= R) ? Na_q : 1001; 
						var min_2 = (G <= R) ? Nd_q : 0;
						var max_2 = (G <= R) ? 1001 : Na_q; 
						rdb = 2*(Math.min(G,R)*(1-X/10)+integrale(avant, 0, Na_q))/Na_q-Na_r
						var apres = new Array(1001);
						var affine = 1;
						for (i=0;i<Na_q;i++) {
							apres[i] = rdb + (i/Na_q)*(Na_r-rdb);
							if (apres[i]<avant[i]) { affine = 0; }
							if ((apres[i-1]-avant[i-1]) < (apres[i]-avant[i])) { affine = 0; 
							} // new
						}
						for (i=min_1; i<max_1; i++) { futur[i] -= X/10 * (futur[i] - avant[i]); }
						for (i=min_2; i<max_2; i++) { futur[i] -= (1 - Math.min(G/R, R/G)) * (futur[i] - avant[i]); }
						for (i=min_2; i<max_2; i++) { futur[i] -= X/10 * (futur[i] - avant[i]); }
						if (affine) { for (i=0;i<Na_q;i++) { futur[i] = apres[i]; }	}
						var diff = new Array(Na_q);
						for (i=0; i<Na_q; i++) { diff[i] = futur[i] - avant[i] ; }
						var econ = integrale(diff, 0, Na_q-1) + economisable(futur, "d");
						for (i=0; i<Na_q; i++) { futur[i] -= Math.min(aid, econ) * (futur[i] - avant[i]) / econ ; }
						for (i=Nd_q; i<1001; i++) { futur[i] -= Math.min(aid, econ) * (futur[i] - futur[Nd_q]) / econ ; }
						// $j('#out').html((integrale(avant,0,1000)-integrale(futur,0,1000))/integrale(avant,0,1000)+"  "+G/R+"  "+futur[0]/12);
						$j('#out').html(Math.round(1000*(integrale(futur,0,Na_q)-integrale(avant,0,Na_q))/integrale(avant,0,1000))/10+"  "+Math.round(1000*(integrale(avant,Nd_q,1000)-integrale(futur,Nd_q,1000))/integrale(avant,0,1000))/10+"  "+Math.round(futur[0]/12));
						// Qualtrics.SurveyEngine.setEmbeddedData("advantage", Na_q);
						// Qualtrics.SurveyEngine.setEmbeddedData("disadvantage", Nd_q);
						// Qualtrics.SurveyEngine.setEmbeddedData("transfer", X);
					}
											
				},
				error: function(){ alert('Les données reforme n\'ont pas pu être chargées');}
			});
			

		},
		error: function(){ alert('Les données n\'ont pas pu être chargées');}
	});

});

