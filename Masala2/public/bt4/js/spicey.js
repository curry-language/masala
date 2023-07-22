/* Auxiliaries for activating elements */

function toggleDisplayBlock(elem) {
  var x = document.getElementById(elem);
  if (x.style.display === "none") {
    x.style.display = "block";
  } else {
    x.style.display = "none";
  }
} 

function setDisplayInlineBlock(elem) {
  var x = document.getElementById(elem);
  x.style.display = "inline-block";
} 

function setDisplayBlock(elem) {
  var x = document.getElementById(elem);
  x.style.display = "block";
} 
