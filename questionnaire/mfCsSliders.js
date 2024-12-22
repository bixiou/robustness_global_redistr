// Documentation on https://qualtricswiki.tgibbons.com/doku.php?id=mfcssliders

// Use a slider question (not constant sum), with mobile friendly activated. 
// Add custom validation that first item should equal $e{ [mustTotalAmt] - q://QID1/ChoiceNumericEntryValue/2 - ... - q://QID1/ChoiceNumericEntryValue/[n] }

// Add the following script to the Qualtrisc survey slider question
// Qualtrics.SurveyEngine.addOnload(function() {

//     jQuery(this.getQuestionContainer()).find(".ChoiceStructure").prepend("<div style='text-align:right;font-weight:bold;padding-bottom:10px;line-height:1.1'>"+
 
//               "<span class='totCheck' style='display:none;color:green'>&#10004; </span>Total"+
 
//               ": <span class='totWrap'><span class='total'>0</span>%</span></span>");
 
//         mfCsSliders(this);
 
//  });

// Add the following script to Qualtrics survey header

function mfCsSliders(t,e){
    var n={ mustTotalFlag:true,
            mustTotalAmt: 100, // customize
            prefix:"",
            suffix:"%",
            widths:3,
            labelStyle:{"font-weight":"bold"}
        };
    $j.extend(true,n,e);
    var i=$j("#"+t.questionId);
    var s=i.find("input.ResultsInput");
    i.find(".ChoiceStructure").append("<div style='text-align:right;font-weight:bold;padding-top:10px;line-height:1.1'>"+"<span class='totCheck' style='display:none;color:green'>&#10004; </span>Total"+": <span class='totWrap'>"+n.prefix+"<span class='total'>0</span>"+n.suffix+"</span></span>");
    var a=i.find("span.total");
    var l=i.find("span.totWrap");
    var r=i.find("span.totCheck");
    i.find("div.slider-container").each(function(t){
        var e=$j(this);
        e.find("div.statement").after("<div style='float:right;min-width:"+n.widths+"em;text-align:right'>"+""+n.prefix+"<span class='resultdiv'></span>"+n.suffix+"</div>");var i=e.find("input.ResultsInput");if(isNaN(parseInt(i.val())))i.val(0);
        e.find(".resultdiv").append(i);i.attr("type","text");i.prop("readonly",true);
        var s=n.widths-n.prefix.length-n.suffix.length;
        i.css({width:s+"em","text-align":"right",border:"none",padding:"0px"})
    });
    i.find("ul.numbers li").each(function(){
        var t=$j(this);
        if(!$j.isEmptyObject(n.labelStyle)) t.css(n.labelStyle);
        t.html(n.prefix+t.text().trim()+n.suffix+" ")
    });
    sumIt();
    s.change(function(){sumIt()});

    function sumIt(){
        var t=0;
        s.each(function(){
            if(this.value.length>0)t+=parseInt(this.value)
        });
        a.html(t);
        if(n.mustTotalFlag){
            if(t>n.mustTotalAmt){
                r.hide();
                l.css("color","red")
            } else if(t==n.mustTotalAmt){
                r.show();
                l.css("color","green")
            } else {
                r.hide();
                l.css("color","orange")
            }
        }
    }
}

