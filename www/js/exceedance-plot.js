// Logic required to customize variations of the exceedance plot of panel
// Exceedance. This is located in tag <legend> of <fieldset>. There is only
// one such tag in the application.

$(document).ready(function() {
    $("legend.toggle-personnalisation").click(function() {
        $fieldset = $(this).closest("fieldset");
        $icon = $fieldset.find(".glyphicon");
        $fbody = $fieldset.find(".fieldset-body");
        $fbody.toggleClass("show");

        var show = $fbody.hasClass("show");

        if (show) {
            $fbody.show("blind");
        } else {
            $fbody.hide("blind");
        }

        $icon.removeClass("glyphicon-plus glyphicon-minus");
        $icon.addClass("glyphicon-" + (show ? "minus" : "plus"));
    });
});

Shiny.addCustomMessageHandler("handler1", function(message) {
    $fig = $("#fracDepVariantes");
    $fig.removeClass("plot-2 plot-1");
    $fig.addClass("plot-"+ message);
});
