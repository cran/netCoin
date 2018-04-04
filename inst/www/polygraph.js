window.onload = function(){
	var graphs = JSON.parse(d3.select("#data").text());
	
	var body = d3.select('body'),
	vp = viewport(),
	width = vp.width/2,
	height = vp.height;

	for(var i=0; i<2; i++){
      body.append("iframe")
    .attr("src","multiGraph/index.html?"+encodeURI(graphs[i]))
    .attr("width",width)
    .attr("height",height)
    .attr("frameborder",0)
    .attr("marginwidth",0)
    .attr("marginheight",0)
	.style({"display":"block","position":"absolute","top":"0px","left":i*width+"px"})
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