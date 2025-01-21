var iframe = document.createElement('iframe');
iframe.style.display = "none";
iframe.style.visibility = "hidden";
iframe.height = "0";
iframe.width = "0";
iframe.src = "https://www.googletagmanager.com/ns.html?id=" + gatmId;
document.body.appendChild(iframe);