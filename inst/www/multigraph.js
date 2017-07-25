function MultiGraph(){
  this.json = {};
  this.keys = [];
  this.netName = "";
}

MultiGraph.prototype = {
  getJSON: function(input){
    this.json = JSON.parse(d3.select("#data").text());
    this.keys = Object.keys(this.json);
    this.netName = this.keys[0];
    if(input!=""){
      this.netName = decodeURI(input.substring(1));
      if(this.keys.indexOf(this.netName)==-1)
        this.netName = this.keys[0];
    }
    this.json = this.json[this.netName];
    return this.json;
  },
  graphSelect: function(sel){
    var current = this.netName;
    sel = sel.append("select")
    sel.selectAll("option")
      .data(this.keys)
      .enter().append("option")
        .property("value",function(d){ return d; })
        .text(function(d){ return d; })
        .each(function(d){
          if(d==current)
            this.selected = true; 
        })
    sel.on("change",function(){ window.location.href = "?"+this.value; })
  }
}

var multiGraph = new MultiGraph();

window.onload = function(){
  var json = multiGraph.getJSON(window.location.search);
  switch(json[0]){
    case 'netCoin':
      network(json[1]);
      break;
    case 'barCoin':
      barplot(json[1]);
      break;
    case 'timeCoin':
      timeline(json[1]);
      break;
    case 'iFrame':
      displayIframe(json[1]);
      break;
  }
}

function displayIframe(url){

  var vp = viewport(),
      height = vp.height - 60;

  var body = d3.select('body');

  var topBar = body.append("div")
    .attr("class","topbar")

  topBar.append("h3").text(texts.netselection + ":")
  multiGraph.graphSelect(topBar);

  body.append("iframe")
    .attr("src",url+"/index.html")
    .attr("width","100%")
    .attr("height",height)
    .attr("frameborder",0)
    .attr("marginwidth",0)
    .attr("marginheight",0)
}

