wrap=function(tag){
	var elems=document.getElementsByTagName(tag);
	for(var i=0; i<elems.length;i++){
		var el=elems[i];
		var par=el.parentNode;
		var link=document.createElement("a");
		if(par.tagName=="A")
			continue;
		link.href="#top";
		par.insertBefore(link,el);
		link.appendChild(el);
	}
}
var target=document.getElementsByTagName("h1");
target=target[0];
var par=target.parentNode;
var link=document.createElement("a");
link.name="top";
par.insertBefore(link,target);
link.appendChild(target);
wrap("h1");
wrap("h2");
wrap("h3");




function captionizeImages() {
  if (!document.getElementsByTagName) return false;
  if (!document.createElement) return false;
  var images = document.getElementsByTagName("img");
  if (images.length < 1) return false; 
  for (var i=0; i<images.length; i++) {
    if (images[i].className.indexOf("captioned") != -1) {
      var title = images[i].getAttribute("title");
      var divCaption = document.createElement("div");
      divCaption.className = "caption";
      var divCaption_text = document.createTextNode(title);
      divCaption.appendChild(divCaption_text);
      var divContainer = document.createElement("div");
      divContainer.className="imgcontainer";
      images[i].parentNode.insertBefore(divContainer,images[i]);
      divContainer.appendChild(images[i]);
      insertAfter(divCaption,images[i]);
    }
  }
}
