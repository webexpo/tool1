// Define minimal custom Shiny handlers that update specific components
// of the application. Handlers are used whenever it is not possible to
// use shiny::*Output() functions in the R ui object. Values (messages)
// are considered valid (and checked by the server before).

// Update the window's title (what is displayed in a browser's tab).
Shiny.addCustomMessageHandler("update_window_title", (title) => {
  document.title = title;
  console.log("[INFO] Updating window's title.");
});

// Update the value of attribute "lang" of <html> element (the web page).
Shiny.addCustomMessageHandler("update_page_lang", (lang) => {
  document.documentElement.lang = lang;
  console.log(`[INFO] Updating attribute lang of root element to ${lang}.`);
});
