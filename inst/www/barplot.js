function barplot(json){

  var vp = viewport(),
      margin = {top: 80, right: 40, bottom: 80, left: 160},
      width = vp.width - 20 - margin.left - margin.right,
      height = vp.height - 55 - margin.top - margin.bottom;

  var options = json.options,
      nodes = json.nodes,
      links = json.links;

  var x = d3.scale.linear()
      .range([0, width]);

  var y = d3.scale.ordinal()

  var xAxis = d3.svg.axis()
      .scale(x)
      .orient("bottom");

  var yAxis = d3.svg.axis()
      .scale(y)
      .orient("left");

  if(options.label)
    yAxis.tickFormat(function(d){ return nodes.filter(function(p){ return d==p[options.name]; })[0][options.label]; })
  else
    options.label = options.name;

  var main = "coincidences",
      textLegend = ["coincidences","incidences"];
  if(options.expected){
    main = "concoincidences";
    textLegend = ["coincidences","ltexpected","gtexpected"];
    if(options.line)
      textLegend.push("confidence");
  }

  var maxIncidence = d3.max(nodes, function(d){ return d[options.incidences]; }),
      subject = nodes.filter(function(d){ return d[options.incidences]==maxIncidence; });

  subject = subject[0][options.name];

  if(options.expected)
    x.domain([0,d3.max(links,function(d){
      return Math.max(d[options.coincidences],d[options.expected]);
    })]).nice()
  else
    x.domain([0,maxIncidence]).nice()

  var body = d3.select("body");

  // top bar
  var topBar = body.append("div")
    .attr("class","topbar")

  iconButton(topBar,"pdf",pdfIcon_b64,"PDF export",svg2pdf);
  iconButton(topBar,"svg",svgIcon_b64,"SVG export",svgDownload);

  // multigraph
  if(typeof multiGraph != 'undefined'){
    topBar.append("h3").text(texts.netselection + ":")
    multiGraph.graphSelect(topBar);
  }

  // subjects
  topBar.append("h3").text(texts.subjectselect + ":")

  topBar.append("select")
    .on("change",function(){
      subject = this.value;
      displayGraph();
    })
    .selectAll("option")
        .data(nodes.map(function(d){
          return [d[options.name],d[options.label]];
        }).sort(function(a,b){
          return a[1] < b[1] ? -1 : a[1] > b[1] ? 1 : a[1] >= b[1] ? 0 : NaN;
        }))
      .enter().append("option")
        .property("value",function(d){ return d[0]; })
        .text(function(d){ return d[1]; })
        .property("selected",function(d){ return d[0]==subject?true:null; })

  // node filter
  topFilter(topBar,nodes,options.name,displayGraph);

  // graph
  displayGraph();

  function displayGraph(filter){
    //subject is global

    var data = links.filter(function(d){
      if(d.source == subject || d.target == subject){
        d.object = (d.source == subject ? d.target : d.source);
        return true;
      }
    });

    if(filter)
      data = data.filter(function(d){ return filter.indexOf(d.object)!=-1; });

    data.forEach(function(d){
      d.a = d[options.coincidences];
      if(options.expected){
        if(d[options.expected]<d.a)
          d.c = d[options.expected];
        if(d[options.expected]>d.a)
          d.b = d[options.expected];
      }else{
        d.b = nodes.filter(function(p){ return d.object==p[options.name]; })[0][options.incidences];
      }
      if(options.text)
        d.t = nodes.filter(function(p){ return d.object==p[options.name]; })[0][options.text];
    })

    data.sort(function(a,b){
      var ab = a.b?a.b:a.c?a.c:a.a,
          bb = b.b?b.b:b.c?b.c:b.a;
      return b.a < a.a ? -1 : b.a > a.a ? 1 : bb < ab ? -1 : bb > ab ? 1 : 0;
    });

    if(height/data.length < 13)
      height = data.length*13;

    y.rangeBands([0, height],.3,.6)
     .domain(data.map(function(d){ return d.object; }));

    body.select("svg.plot").remove();

    var svg = body.append("svg")
      .attr("class","plot")
      .attr("xmlns","http://www.w3.org/2000/svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)

    svg.append("style").text("text { font: 10px sans-serif; }"+
      ".main { font-size: 20px; }"+
      ".bar, .legend rect { stroke: #000; stroke-width: .4px; }"+
      "rect.a { fill: #677BB2; }"+
      "rect.b { fill: #FFB8A7; }"+
      "rect.c { fill: #AECC83; }"+
      ".axis path, .axis line { fill: none; stroke: #000; shape-rendering: crispEdges; }"+
      ".y.axis path, .y.axis line { display: none; }"+
      ".line { stroke-dasharray: 2, 2; stroke: #333; }");

    svg.append("text")
        .attr("class","main")
        .attr("x",margin.left)
        .attr("y",margin.top/2)
        .text(texts[main] + " " + texts.ofsomeone + " " + nodes.filter(function(p){ return subject==p[options.name]; })[0][options.label] + " " + texts.withsomeone + "...")

    var legend = svg.append("g")
        .attr("class","legend")
        .attr("transform","translate("+(margin.left)+","+margin.top/1.1+")")

    legend.selectAll("text")
          .data(textLegend)
        .enter().append("text")
          .text(function(d){ return texts[d]; })
          .attr("x",function(d,i){ return i*110 + 20; })

    legend.selectAll("rect")
          .data(textLegend)
        .enter().append("rect")
          .attr("class",function(d,i){ return ["a","b","c","d"][i]; })
          .attr("width",16)
          .attr("height",8)
          .attr("y",-7)
          .attr("x",function(d,i){ return i*110; })

    if(options.minor){
      body.append("p")
        .attr("class","minor")
        .style({position:"absolute",left:margin.left+"px",top:(margin.top+height+margin.bottom)+"px"})
        .html(options.minor)
    }

    if(options.line){
      var confRect = legend.select("rect.d");
      legend.insert("line","rect.d")
        .attr("class", "line")
        .attr("x1",+confRect.attr("x")+8)
        .attr("x2",+confRect.attr("x")+8)
        .attr("y1",-8)
        .attr("y2",2);
      confRect.remove();
    }

    svg = svg.append("g")
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);

    svg.append("g")
        .attr("class", "y axis")
        .call(yAxis);

    displayBar("b");
    displayBar("a");
    displayBar("c");

    svg.selectAll(".line")
        .data(data.filter(function(d){ return d[options.line] > 0; }))
      .enter().append("line")
        .attr("class", "line")
        .attr("x1",function(d) { return x(d[options.line]); })
        .attr("x2",function(d) { return x(d[options.line]); })
        .attr("y1",function(d) { return y(d.object); })
        .attr("y2",function(d) { return y(d.object)+y.rangeBand(); });

    function displayBar(type){

      var bar = svg.selectAll(".bar."+type)
        .data(data.filter(function(d){ return d[type]; }))
      .enter().append("rect")
        .attr("class", "bar "+type)
        .attr("x", 0)
        .attr("width", 0)
        .attr("y", function(d) { return y(d.object); })
        .attr("height", y.rangeBand());

      if(options.text)
        tooltip(bar,"t");
      else
        bar.append("title")
          .text(function(d) { return "(" + d.object + ", " + formatter(d[type]) + ")"; });

      bar.transition().duration(1000)
        .attr("width", function(d) { return x(d[type]); });
    }
  }

  function svgDownload(){
    var svg = d3.select("svg.plot");
    var svgString = new XMLSerializer().serializeToString(svg.node());
    var blob = new Blob([svgString], {type: 'image/svg+xml;charset=utf-8'});
    fileDownload(blob, d3.select("head>title").text()+'.svg');
  }

function svg2pdf(){

  var tWidth = width + margin.left + margin.right,
      tHeight = height + margin.top + margin.bottom;

  var doc = new jsPDF(tWidth>tHeight?"l":"p","pt",[tWidth, tHeight]);

  doc.setTextColor(0);
  doc.setDrawColor(0);
  doc.setLineWidth(1);

  d3.selectAll("svg>text").each(function(){
    var self = d3.select(this),
        x = margin.left,
        y = self.attr("y"),
        txt = self.text(),
        fontsize = parseInt(self.style("font-size"));
    doc.setFontSize(fontsize);
    doc.text(x,y,txt);
  })

  doc.setFontSize(10);

  d3.selectAll(".legend").each(function(){
    var self = d3.select(this),
        coors = d3.transform(self.attr("transform")).translate;
    self.selectAll("text").each(function(){
      var self = d3.select(this),
          x = +self.attr("x") + coors[0],
          txt = self.text();
      doc.text(x,coors[1],txt);
    })
    self.selectAll("rect").each(function(){
      var self = d3.select(this),
        x = +self.attr("x") + coors[0],
        y = coors[1] - 8,
        w = +self.attr("width"),
        h = +self.attr("height"),
        color = d3.rgb(self.style("fill"));
      doc.setFillColor(color.r,color.g,color.b);
      doc.rect(x, y, w, h, 'FD');
    })
  })

  d3.selectAll(".bar").each(function(){
    var self = d3.select(this),
        x = +self.attr("x") + margin.left,
        y = +self.attr("y") + margin.top,
        w = +self.attr("width"),
        h = +self.attr("height"),
        color = d3.rgb(self.style("fill"));
    doc.setFillColor(color.r,color.g,color.b);
    doc.rect(x, y, w, h, 'FD');
  });

  d3.selectAll(".line").each(function(){
    var self = d3.select(this),
        margin = d3.transform(d3.select(this.parentNode).attr("transform")).translate;
        x = +self.attr("x1") + margin[0],
        y = +self.attr("y1") + margin[1],
        x2 = +self.attr("x2") + margin[0],
        y2 = +self.attr("y2") + margin[1],
        color = d3.rgb(self.style("stroke"));
    doc.setDrawColor(color.r,color.g,color.b);
    for(var i = y; i<y2; i += 4)
      doc.line(x,i,x2,i+2)
  });

  d3.selectAll(".y.axis .tick text").each(function(){
    var self = d3.select(this),
        y = d3.transform(d3.select(this.parentNode).attr("transform")).translate[1] + margin.top,
        txt = self.text(),
        txtWidth = doc.getStringUnitWidth(txt) * 10,
        x = margin.left - txtWidth;
    doc.text(x-6, y+3, txt);
  });

  doc.line(margin.left,margin.top+height,margin.left+width,margin.top+height)

  d3.selectAll(".x.axis .tick text").each(function(){
    var self = d3.select(this),
        x = d3.transform(d3.select(this.parentNode).attr("transform")).translate[0] + margin.left,
        y = height + margin.top,
        txt = self.text();
    doc.line(x,y,x,y+6);
    doc.text(x-3, y+16, txt);
  });

  d3.selectAll("p.minor").each(function(){
    var self = d3.select(this),
        x = margin.left,
        y = height + margin.top + margin.bottom/2,
        txt = self.text();
    doc.text(x, y, txt);
  })

  doc.save(d3.select("head>title").text()+".pdf");
}

} // barplot function end

if(typeof multiGraph == 'undefined'){
  window.onload = function(){
    barplot(JSON.parse(d3.select("#data").text()));
  };
}
