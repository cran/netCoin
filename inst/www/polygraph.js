window.onload = function(){
  var body = document.querySelector('body'),
      vp = viewport(),
      width = vp.width/2,
      height = vp.height;

  for(var i=0; i<2; i++){
    var iframe = document.createElement("iframe");

    iframe.setAttribute("src","multiGraph/index.html?"+i);
    iframe.setAttribute("width",width);
    iframe.setAttribute("height",height);
    iframe.setAttribute("frameborder",0);
    iframe.setAttribute("marginwidth",0);
    iframe.setAttribute("marginheight",0);
    iframe.style.display = "block";
    iframe.style.position = "absolute";
    iframe.style.top = "0px";
    iframe.style.left = i*width+"px";

    body.appendChild(iframe);
  }
}

function viewport(){
  var e = window,
      a = 'inner';
  if ( !( 'innerWidth' in window ) ){
    a = 'client';
    e = document.documentElement || document.body;
  }
  return { width : e[a+'Width'] , height : e[a+'Height'] }
}
