function MultiGraph(input){
  this.json = JSON.parse(d3.select("#data").text());
  this.items = this.json.items;
  this.types = this.json.types;
  this.current = 0;
  if(input!=""){
    this.current = parseInt(input.substring(1));
    if(isNaN(this.current) || this.current>=this.items.length)
      this.current = 0;
  }
  this.json = this.json.data[this.current];
}

MultiGraph.prototype = {
  graphSelect: function(sel){
    var current = this.current;
    sel = sel.append("select")
    sel.selectAll("option")
      .data(this.items)
      .enter().append("option")
        .property("value",function(d,i){ return i; })
        .text(function(d){ return d; })
        .each(function(d,i){
          if(i==current)
            this.selected = true; 
        })
    sel.on("change",function(){ window.location.href = "?"+this.value; })
  },
  getJSON: function(){
    return this.json;
  },
  getType: function(){
    return this.types[this.current];
  },
  getItem: function(){
    return this.items[this.current];
  }
}

var multiGraph = true;

window.onload = function(){
  multiGraph = new MultiGraph(window.location.search);
  var json = multiGraph.getJSON();
  switch(multiGraph.getType()){
    case 'netCoin':
      network(json);
      break;
    case 'barCoin':
      barplot(json);
      break;
    case 'timeCoin':
      timeline(json);
      break;
    case 'iFrame':
      displayIframe(json);
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

