// Define minimal custom Shiny handlers that update specific components
// of the application. Handlers are used whenever it is not possible to
// use shiny::*Output() functions in the R ui object. Values (messages)
// are considered valid (and checked by the server before).

// Update the window's title (what is displayed in a browser's tab).
Shiny.addCustomMessageHandler(
  "update_window_title",
  (x) => (document.title = x)
);

// Update the value of attribute "lang" of <html> element (the web page).
Shiny.addCustomMessageHandler(
  "update_page_lang",
  (x) => (document.documentElement.lang = x)
);

// Update the value of attribute named "attr" of element identified by "id".
Shiny.addCustomMessageHandler("update_attribute", ({ id, attr, value }) => {
  document.getElementById(id).setAttribute(attr, value);
});
