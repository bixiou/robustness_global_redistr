$j(document).ready(function($j){	
			
	var Nd_q, Nd_r, Na_q, Na_r, max;
	Nd_q = 90*10;
	Na_q = 50*10;
	max = 100000*12;
	var X = 1.43;
	var aid = 0 * 201938;
	
	$j.ajax({
		// url:'https://az1.qualtrics.com/WRQualtricsControlPanel/File.php?F=F_6Lu6NsffRRM4EEB',
		url:'../data/world_thousandile.csv',
		success: function(data){
			var actuel = charge(data);
			correct_parameters(actuel);
			var graphe = new cfx.Chart();
			futur = courbe_reference(actuel, max);
			ajuste(actuel, max);
			trace(graphe, actuel, futur);
			
			var rev = 24000; //12*parseInt("${q://QID50/ChoiceTextEntryValue}");
			var nb_pers = 1; //parseInt("${q://QID52/ChoiceTextEntryValue}");
			var revenu_conjoint = 0; // 12*parseInt("${q://QID67/ChoiceTextEntryValue}");
			
			function irpp(rev, marie,nb_pers) {
				var quotient = 1;
				if (nb_pers == 2) { quotient = 2; }
				if (nb_pers == 3) { quotient = 2.5; }
				if (nb_pers == 4) { quotient = 3; }
				if (nb_pers > 4) { quotient = Math.min(6, nb_pers - 1); }
				var income = 0.9334 * rev / quotient // (1 + (0.029 * 1.28))*0.9 : passage au brut (+28% en moyenne), CSG+CRDS non déductibles (2,90%), puis abattement de 10%
				var ir = 0;
				if (income > 12676*12) {
					ir = ir + (income - 12676*12) * 0.45
				}
				if (income > 5986*12) {
					ir = ir + (Math.min(income, 12676*12) - 5986*12) * 0.41
				}
				if (income > 2233*12) {
					ir = ir + (Math.min(income, 5986*12) - 2233*12) * 0.3
				}
				if (income > 808*12) {
					ir = ir + (Math.min(income, 2233*12) - 808*12) * 0.14
				}
				
				ir = quotient * ir;
				var decote=0;
				if (marie==1 & ir<2560) {
					decote = 1920 - 0.75 * ir;
				}
				if (marie==0 & ir<1553) {
					decote = 1165 - 0.75 * ir;
				}
				return (ir-decote);
			}
			
			var rev_tot = rev;
			var marie = 0;
			// if ("${q://QID67/ChoiceTextEntryValue}" != "") {
			if (false) {
				rev_tot = rev + revenu_conjoint; 
				marie = 1;
			}
			nb_pers = Math.max(1, nb_pers);
			
			var revdisp = Math.round((rev / rev_tot) * (rev_tot -  irpp(rev_tot,marie,nb_pers))/12);
			
			function change_revenu() {
				var rdb = futur[0];
				var rev_futur =  Math.round(interpole(12*revdisp, actuel, futur, rdb)/12);
				
				// if ("${q://QID67/ChoiceTextEntryValue}" != "") {
				if (false) {
					var revdisp_tot =  Math.round((rev_tot -  irpp(rev_tot,marie,nb_pers))/12);
					var revdisp_conjoint = Math.round((1 - rev / rev_tot) * (rev_tot -  irpp(rev_tot,marie,nb_pers))/12);
					var revdisp_tot_futur = Math.round(interpole(12*revdisp_conjoint, actuel, futur, rdb)/12 +  interpole(12*rev_futur, actuel, futur, rdb)/12);
					$j('#rev_actuel').html(revdisp + "€/mois, et celui de votre ménage à " + revdisp_tot);
					$j('#rev_futur').html(rev_futur + "€/mois, tandis que celui de votre ménage passerait à " + revdisp_tot_futur);	
				}
				else {						
					$j('#rev_actuel').html(revdisp);
					$j('#rev_futur').html(rev_futur);		
				}						
			}
			
			change_revenu();
			
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
				maj(graphe, actuel, futur);
				change_revenu();
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
				maj(graphe, actuel, futur);
				change_revenu();
			});            
								
			function charge(data) {
				var tab=data.split('\n');
				var avant = new Array(1001);
				for (var i = 1; i<=1000; i++) {	avant[i] = parseInt(tab[i].split(';')[1]);	}
				avant[0] = parseInt(tab[1].split(';')[1]);
				return avant
			}
			
			function interpole(rev, avant, apres, rdb) {
				var e = 0
				while (avant[e]<rev & e < 1001) {     e++;    }
				if (e == 1001) { return max }
				else if (e == 0) { return ((apres[0]-rdb)*rev/avant[0] + rdb) }
					else { return ((apres[e]-apres[e-1])*(rev-avant[e-1])/(avant[e]-avant[e-1])+apres[e-1]) }
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
				for (i=0; i<1001; i++) { courbes[i] = { apres: apres[i]/12, avant: avant[i]/12 };	}
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
				//$j('#out').html((integrale(avant,0,1000)-integrale(futur,0,1000))/integrale(avant,0,1000)+"  "+G/R+"  "+futur[0]/12);
			}
									
		},
		error: function(){ alert('Les données n\'ont pas pu être chargées');}
	});

});

