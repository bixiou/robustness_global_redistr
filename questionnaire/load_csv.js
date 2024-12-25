Qualtrics.SurveyEngine.addOnload(function()
{
	// Loads CSV and set variables from the column corresponding to Q_Language, both in JS and HTML
	$j(document).ready(function($j){	
		$j.ajax({
			url:"https://wumarketing.eu.qualtrics.com/ControlPanel/File.php?F=F_yuKEs6pwxsAid44",
			success: function(csv){
				
			const language = Qualtrics.SurveyEngine.getEmbeddedData("Q_Language"); 
				
			const rows = csv.split('\n').map(row => row.split(';'));
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
				$j("#" + name).html(value);
				console.log(`Variable ${name} created with value: ${value}`);
			}

				},
			error: function(){ alert('Data could not be loaded');}
		});
	});
});