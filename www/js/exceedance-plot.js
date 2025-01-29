// Execute the following callback function once when the DOM is loaded.
$(document).ready(function() {
    // Register a callback function for click events on the exceedance plot's
    // legend (object #exceedance-plot-customize-legend). Each click triggers
    // the event which execute the callback function. It either shows or hides
    // the inputs of the parent fieldset (object #exceedance-plot-customize).
    $("#exceedance-plot-customize-legend").click(function() {
        // Get parent fieldset.
        let fieldset = $("#exceedance-plot-customize");

        // Get span containing the icons within fieldset.
        // Since there is only one, searching by class is safe.
        let icon = fieldset.find(".glyphicon");

        // Get div containing all the inputs.
        // Since there is only one, searching by class is safe.
        let inputs = fieldset.find(".fieldset-body");

        // Invert the visibility state of the inputs.
        // If they are shown, remove class show. Otherwise, add it.
        inputs.toggleClass("show");

        // Reset state of legend's icon.
        icon.removeClass("glyphicon-plus glyphicon-minus");

        // Set state by setting the appropriate class name
        // and adding it to the class attribute of $icon.
        // This shows a (+) or (-) icon in the UI.
        icon.addClass(`glyphicon-${inputs.hasClass("show") ? "minus" : "plus"}`);
    });
});

// Get how many plots must be visible (message can be equal to 1 or 2)
// and update the class attribute of HTML entity #fracDepVariantes based
// it by setting either classes plot-1, or plot-2.
Shiny.addCustomMessageHandler("exceedance-plot-handler", message => {
    // Get object.
    let obj = $("#fracDepVariantes");

    // Reset state.
    obj.removeClass("plot-2 plot-1");

    // Set state by setting the appropriate class name
    // and adding it to the class attribute of $fig.
    obj.addClass(`plot-${message}`);
});
