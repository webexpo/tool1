var def = function(x) { return window[x] !== undefined };
var doGoogAnalysis = def('gaId');
var hostname = document.location.hostname;
var runningOffline = /^[0-9.]+$/.test(hostname) || hostname == 'localhost';
var incrOfflineCounter =  runningOffline && def('gatmId');

if ( doGoogAnalysis ) {
  if ( incrOfflineCounter ) {
    console.log("OFFLINE analytics:", gatmId);
    (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer',gatmId);
  } else if ( !runningOffline ) {
    console.log("ONLINE analytics:", gaId);
    $.getScript("https://www.googletagmanager.com/gtag/js?id=" + gaId, function() {
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      var dte = new Date();
      gtag('js', dte);
      gtag('config', gaId); 
      console.log("GA", gaId, dte);
    });
  }
}