import "./main.css";
import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

app.ports.updateIframe.subscribe(function([iframeId, url]) {
  addIframeWithSrc(iframeId, url)
})

function addIframeWithSrc(parentElementId, srcUrl) {
    //cgpt-4
    // Select the target element by its ID
    var parentElement = document.getElementById(parentElementId);

    // Check if an iframe already exists and remove it
    var existingIframe = parentElement.querySelector('iframe');
    if (existingIframe) {
        parentElement.removeChild(existingIframe);
    }

    // Create a new iframe element
    var iframe = document.createElement('iframe');

    // Set the iframe's width to 100% and the src attribute
    iframe.style.width = '100%';
    iframe.style.height = '30em';
    iframe.src = srcUrl;

    // Append the iframe to the target element
    parentElement.appendChild(iframe);
}
