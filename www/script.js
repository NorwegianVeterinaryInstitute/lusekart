$(document).ready(function(){

    $("#gotoblock").hide();
    $("#km").show();
    $('input[type=radio][name=sokeradio]').change(function() {
        if (this.value == 'find') {
            $("#gotoblock").hide();
            $("#findblock").show();
				    $("#km").show();
        }
        else if (this.value == 'goto') {
            $("#gotoblock").show();
            $("#findblock").hide();
				    $("#km").hide();
        }
        else if (this.value == 'smi') {
            $("#gotoblock").hide();
            $("#findblock").hide();
				    $("#km").hide();
        }
    });

    $("#velghistoriske").hide();
    $('input[type=radio][name=velgradio]').change(function() {
        if ((this.value == '1uke') || (this.value == '2uker')) {
            $("#velghistoriske").hide();
        }
        else if (this.value == 'hist') {
            $("#velghistoriske").show();
        }
    });


});