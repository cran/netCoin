function timeline(json){

  var nodes = json.nodes,
      options = json.options;

  var ctrlKey = false;

  var body = d3.select("body");

  body
    .on("keydown", keyflip)
    .on("keyup", keyflip)

  if(options.cex)
    body.style("font-size", 10*options.cex + "px")
  else
    options.cex = 1;

  // split multivariables
  nodes.forEach(function(d){
      for(var p in d) {
        if(p!=options.name){
          if(typeof d[p] == "string" && d[p].indexOf("|")!=-1){
            d[p] = d[p].split("|").map(function(d){ return isNaN(parseInt(d)) ? d : +d; });
          }
        }
      }
  });

  // top bar
  var topBar = body.append("div")
        .attr("class","topbar")

  iconButton(topBar,"pdf",pdfIcon_b64,"PDF export",svg2pdf);
  iconButton(topBar,"svg",svgIcon_b64,"SVG export",svgDownload);

  // multigraph
  if(typeof multiGraph != 'undefined'){
      topBar.append("h3").text(texts.netselection + ":")
      multiGraph.graphSelect(topBar);
  }else
      topBar.append("select").style("visibility","hidden");

  // node filter
  var topFilterInst = topFilter()
    .data(nodes)
    .attr(options.name)
    .displayGraph(displayGraph);
  topBar.call(topFilterInst);

  if(options.main)
    body.append("div")
        .attr("class","main")
        .html(options.main)

  //styles
  d3.select("head")
      .append("style")
      .text("text { font-family: sans-serif; font-size: "+body.style("font-size")+"; } "+
    ".laneLines {  shape-rendering: crispEdges; }"+
    ".mini text { font-size:  90%; }"+
    ".mini .miniItem { fill-opacity: .7; stroke-width: 6;  }"+
    ".brush .selection { fill: dodgerblue; }"+
    ".axis path, .axis line { fill: none; stroke: #000; shape-rendering: crispEdges; }"+
    ".main text { font-size:  120%; }"+
    ".main .miniItem { stroke-width: 6; }")

  displayGraph();

  if(options.note)
    body.append("div")
        .attr("class","note")
        .html(options.note)

  function displayGraph(filter){

    var plot = body.select("div.plot")

    if(plot.empty())
      plot = body.append("div")
               .attr("class","plot")
    else
      plot.selectAll("*").remove();

    var currentYear = new Date().getFullYear(),
        getEnd = function(y){
          return y === null ? currentYear : y;
        };

    var data = filter ? nodes.filter(function(d){ return filter.indexOf(d[options.name])!=-1; }) : nodes;

    var lanes = options.group?d3.set(data.map(function(d){ return d[options.group]; })).values().sort():[""],
      laneLength = lanes.length,
      items = data.map(function(d){
        d.lane = options.group?lanes.indexOf(d[options.group]):0;
        return d;
      }),
      timeBegin = d3.min(items,function(d){ return d[options.start]; }),
      timeEnd = d3.max(items,function(d){ return getEnd(d[options.end]); });

    if(!options.text){
      items.forEach(function(d){
        d["text"] = d[options.name]+" </br>"+d[options.start]+((d[options.end]===null)?"":" - "+d[options.end]);
      })
      options.text = "text";
    }

    //sizes
    var vp = viewport(),
      m = [20, 15, 20*options.cex, 120*options.cex], //top right bottom left
      w = vp.width - 30 - m[1] - m[3],
      miniHeight = laneLength * (12*options.cex) + 50,
      mainHeight = 10;

    //scales
    var color = d3.scaleOrdinal().range(categoryColors.slice(0,10));
    if(laneLength>10)
      color = d3.scaleOrdinal().range(categoryColors);
    color.domain(d3.range(0,laneLength));

    var x = d3.scaleLinear()
      .domain([timeBegin, timeEnd])
      .range([0, w]);
    var x1 = d3.scaleLinear()
      .range([0, w]);
    var y2 = d3.scaleLinear()
      .domain([0, laneLength])
      .range([0, miniHeight]);

    //mini
    var mini = plot
      .append("svg")
      .attr("width", w + m[1] + m[3])
      .attr("height", miniHeight + m[0] + m[2]*2);

    mini = mini.append("g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")")
      .attr("class", "mini");

    //mini lanes and texts
    mini.append("g").selectAll(".laneLines")
      .data(lanes)
      .enter().append("line")
      .attr("class", "laneLines")
      .attr("x1", 0)
      .attr("y1", function(d,i) {return y2(i);})
      .attr("x2", w)
      .attr("y2", function(d,i) {return y2(i);})
      .attr("stroke", "lightgray");

    var selectedGroups = d3.set();
    var laneText = mini.append("g").selectAll(".laneText")
      .data(lanes)
      .enter().append("text")
      .text(function(d) {return d;})
      .attr("x", -m[1])
      .attr("y", function(d, i) {return y2(i + .5);})
      .attr("dy", ".5ex")
      .attr("text-anchor", "end")
      .attr("class", "laneText")
      .on("click",function(group){
        if(ctrlKey){
          selectedGroups[selectedGroups.has(group)?"remove":"add"](group);
        }else{
          selectedGroups.clear();
          selectedGroups.add(group);
        }
        laneText.each(function(g){
          d3.select(this).style("font-weight",selectedGroups.has(g)?"bold":null);
        })
      })
      .on("dblclick",function(group){
        selectedGroups.add(group);
        var filter = nodes.filter(function(d){ return selectedGroups.has(d[options.group]); })
                       .map(function(d){ return d[options.name]; });
        displayGraph(filter);
      });

    //mini axis
    var xAxis = d3.axisBottom(x).tickFormat(formatter);

    mini.append("g")
      .attr("class", "x axis")
      .attr("transform", "translate(0," + y2(laneLength) + ")")
      .call(xAxis);

    var mainAxis = mini.append("g")
      .attr("class", "x1 axis")
      .attr("transform", "translate(0," + (miniHeight + m[0] + m[2] - 1) + ")");

    //mini item rects
    mini.append("g").selectAll(".miniItem")
      .data(items)
      .enter().append("rect")
      .attr("class", function(d){ return "miniItem"; })
      .attr("fill", function(d) { return color(d.lane); })
      .attr("x", function(d){ return x(d[options.start]); })
      .attr("y", function(d){ return y2(d.lane + .5) - 5; })
      .attr("width", function(d){ return x(timeBegin + getEnd(d[options.end]) - d[options.start]); })
      .attr("height", 10);
    
    //main
    var svgLanes = plot.selectAll("svg.lane")
      .data(lanes)
      .enter().append("svg")
      .attr("class","lane")
      .attr("width", w + m[1] + m[3])
      .attr("height", mainHeight + m[0] + m[2]);
    
    svgLanes.append("defs").append("clipPath")
      .attr("id", function(d,i){ return "clip"+i; })
      .append("rect")
      .attr("width", w)
      .attr("height", mainHeight);

    var main = svgLanes.append("g")
      .attr("transform", "translate(" + m[3] + "," + m[0] + ")")
      .attr("width", w)
      .attr("height", mainHeight)
      .attr("class", "main");

    //main lanes and texts
    main.append("line")
      .attr("class", "laneLines")
      .attr("x1", 0)
      .attr("y1", 0)
      .attr("x2", w)
      .attr("y2", 0)
      .attr("stroke", "lightgray");

    main.append("text")
      .attr("class", "laneText")
      .text(String)
      .attr("x", 0)
      .attr("y", 4 + 12*options.cex)

    main.append("g")
      .attr("clip-path", function(d,i){ return "url(#clip"+i+")"; });

    //brush
    var brush = d3.brushX()
      .extent([[0,0],[w,miniHeight - 1]])
      .on("brush", display);

    mini.append("g")
      .attr("class", "x brush")
      .call(brush)

    var yearGuideTop = 53 + (options.main ? parseInt(body.select("div.main").style("height")) : 0) + parseInt(body.select("div.plot>svg:first-child").style("height")),
        yearGuide = plot.append("div")
      .attr("class","year-guide")
      .style("position","absolute")
      .style("top",yearGuideTop+"px")
      .style("left",((w/2)+m[3])+"px")
      .style("width",0)
      .style("height",0)
      .style("border-left","dashed 1px #000")
      .style("z-index",-1);

    var pYear = body.append("p")
      .attr("class","year")
      .style("background-color","#fff")
      .style("position","fixed")
      .style("top",0)
      .style("margin-left","-16px")
      .style("padding","2px 0")
      .style("border-radius","0 0 5px 5px")
      .style("width","32px")
      .style("text-align","center")
      .style("display","none");

    body.on("mousemove",function(){
      var coords = d3.mouse(body.node());
      if(coords[1]>yearGuideTop && coords[0]>m[3] && coords[0]<(m[3]+w)){
        yearGuide.style("left",coords[0]+"px");
        pYear.style("left",coords[0]+"px");
        var year = parseInt(x1.invert(coords[0]-m[3]));
        pYear.text(year);
      }
    })

    window.onscroll = function(){
      if(window.pageYOffset>yearGuideTop)
        pYear.style("display",null)
      else
        pYear.style("display","none")
    }

    mini.select(".brush").call(brush.move,x.range());

    function display() {
      var rects, rectsEnter, rectsUpdate, lines, self, height,
      extent = d3.event.selection.map(x.invert, x),
      minExtent = extent[0],
      maxExtent = extent[1],
      visItems = items.filter(function(d) {return d[options.start] < maxExtent && getEnd(d[options.end]) > minExtent;});

      x1.domain([minExtent, maxExtent]);

      if(minExtent < maxExtent){
        var x1Axis = d3.axisTop(x1).tickFormat(formatter);
        mainAxis.call(x1Axis);
      }else{
        mainAxis.selectAll("*").remove();
      }

      //update main item rects
      svgLanes.each(function(d,i){
        self = d3.select(this);

        var laneData = options.group ? visItems.filter(function(p){ return p[options.group] == d; }) : visItems;

        if(!laneData.length){
          self.style("display","none");
        }else{
          self.style("display",null);

          rects = self.select("g[clip-path]").selectAll("g")
            .data(laneData, function(d) { return d[options.name]; })
      
          rectsEnter = rects.enter()
              .append("g")
            .attr("class", "miniItem")
            .attr("fill", color(i))
          rectsEnter.append("rect")
            .attr("height", 10)
          rectsEnter.append("text")
            .attr("y", 10 + 12*options.cex)

          tooltip(rectsEnter,options.text);

          rects.exit().remove();

          rectsUpdate = rectsEnter.merge(rects);

          rectsUpdate.select("rect")
            .attr("x", function(d) { return x1(d[options.start]);} )
            .attr("width", function(d) { return x1(getEnd(d[options.end])) - x1(d[options.start]);} );
          rectsUpdate.select("text").each(function(d){
            var self = d3.select(this),
                x = x1(d[options.start]),
                name = d[options.name];
            if(x<0){
                x = 0;
                name = "← " + name;
            }
            self.attr("x", x).text(name)
            if((x+this.getBBox().width)>w)
              self.attr("x",w).attr("text-anchor","end");
            else
              self.attr("text-anchor",null);
          });

          lines = [-Infinity];
          rectsUpdate.attr("transform",function(){
            var selfCoords = this.getBBox(),
            i = 0;
            while(lines[i]>=selfCoords.x){
              i++;
              if(typeof lines[i] == 'undefined')
                lines.push(-Infinity);
            }
            lines[i] = selfCoords.x+selfCoords.width;
            return "translate(0,"+((30+i*30)*options.cex)+")";
          });

          height =  Math.ceil(this.getBBox().height);
          self.attr("height", height + m[0] + m[2]);
          self.select("defs #clip"+i+" rect").attr("height", height);
        }
      });

      var guideHeight = (parseInt(plot.style("height")) - parseInt(plot.select(".plot>svg:first-child").style("height"))) + "px";
      yearGuide.style("height",guideHeight);

    }
  }

  function keyflip() {
    ctrlKey = d3.event.ctrlKey | d3.event.metaKey;
  }

function svgDownload(){
  var svgs = d3.selectAll(".plot>svg").filter(function(){ return d3.select(this).style("display")!="none"; }),
      tWidth = d3.select(".plot>svg").attr("width"),
      tHeight = 0,
      styles = d3.select("head>style").text(),
      svgString = "";

  svgs.each(function(){
    svgString = svgString + '<g transform="translate(0,' + tHeight + ')">' + this.innerHTML + '</g>';
    tHeight = tHeight + parseInt(d3.select(this).attr("height"));
  });

  svgString = '<svg xmlns="http://www.w3.org/2000/svg" width="' + tWidth + '" height="' + tHeight + '"><style>' + styles + '</style>' + svgString + '</svg>';

  var blob = new Blob([svgString], {type: 'image/svg+xml;charset=utf-8'});
  fileDownload(blob, d3.select("head>title").text()+'.svg');
}

function svg2pdf(){
  var svgs = d3.selectAll(".plot>svg").filter(function(){ return d3.select(this).style("display")!="none"; }),
      tWidth = d3.select(".plot>svg").attr("width"),
      tHeight = 0,
      width = +d3.select("#clip0>rect").attr("width"),
      heights = [tHeight],
      margin = getTranslation(d3.select(".plot>svg>g").attr("transform"));

  svgs.each(function(){
    var h = parseInt(d3.select(this).attr("height"));
    tHeight = tHeight + h;
    heights.push(tHeight);
  });

  if(!d3.select("div.main").empty()){
    margin[1] = margin[1] + 30;
    tHeight = tHeight + 30;
  }

  if(!d3.select("div.note").empty()){
    tHeight = tHeight + 40;
  }

  var doc = new jsPDF({
    orientation: tWidth>tHeight?"l":"p",
    unit: 'pt',
    format: [tWidth, tHeight]
  });

  doc.setTextColor(0);
  doc.setLineWidth(1);

  d3.select("div.main").each(function(){
      var self = d3.select(this),
          txt = self.text(),
          fontsize = parseInt(self.style("font-size")),
          txtWidth = doc.getStringUnitWidth(txt) * fontsize,
          x = tWidth/2 - txtWidth/2,
          y = fontsize + 10;
      doc.setFontType("bold");
      doc.setFontSize(fontsize);
      doc.text(x, y, txt);
  })

  doc.setFontType("normal")

  d3.select("div.note").each(function(){
      var self = d3.select(this),
          txt = self.text(),
          fontsize = parseInt(self.style("font-size")),
          x = margin[0],
          y = tHeight - fontsize + 10;
      doc.setFontSize(fontsize);
      doc.text(x, y, txt);
  })

  svgs.each(function(d,i){
    var svg = d3.select(this),
        svgY = heights[i];

    doc.setDrawColor(211);
    svg.selectAll(".laneLines").each(function(){
      var self = d3.select(this),
          x = +self.attr("x1") + margin[0],
          y = +self.attr("y1") + margin[1] + svgY,
          x2 = +self.attr("x2") + margin[0];
      doc.line(x,y,x2,y);
    })
    svg.selectAll(".laneText").each(function(){
      var self = d3.select(this),
          y = +self.attr("y") + margin[1] + svgY,
          txt = self.text(),
          fontsize = parseInt(self.style("font-size")),
          txtWidth = self.attr("text-anchor")=="end" ? doc.getStringUnitWidth(txt) * fontsize : 0,
          x = +self.attr("x") + margin[0] - txtWidth;
      doc.setFontSize(fontsize);
      doc.setTextColor(0);
      doc.text(x, y+3, txt);
    })
    if(!i){
      var drawAxis = function(name){
        if(!svg.select("."+name+".axis>.domain").empty()){
          var axisY = getTranslation(svg.select("."+name+".axis").attr("transform"))[1],
              y = axisY + margin[1] + svgY;
          doc.setDrawColor(0);
          doc.setFontSize(9);
          doc.line(margin[0],y,margin[0]+width,y)
          svg.selectAll("."+name+".axis .tick text").each(function(){
            var self = d3.select(this),
                x = getTranslation(d3.select(this.parentNode).attr("transform"))[0] + margin[0],
                txt = self.text(),
                txtWidth = doc.getStringUnitWidth(txt) * 9;
            doc.line(x, y, x, y + ((name=="x") ? 6 : -6));
            x = x - txtWidth/2;
            doc.text(x, y + ((name=="x")? 16 : -10) , txt);
          });
        }
      }
      drawAxis("x");
      drawAxis("x1");
      svg.selectAll(".miniItem").each(function(){
        var self = d3.select(this),
            x = +self.attr("x") + margin[0],
            y = +self.attr("y") + margin[1] + svgY,
            w = +self.attr("width"),
            h = +self.attr("height"),
            color = d3.rgb(self.style("fill"));
        doc.setFillColor(color.r,color.g,color.b);
        doc.rect(x, y, w, h, 'F');
      })
      svg.selectAll(".x.brush rect.extent").each(function(){
        var self = d3.select(this),
            x = +self.attr("x") + margin[0],
            y = +self.attr("y") + margin[1] + svgY,
            w = +self.attr("width"),
            h = +self.attr("height");
        doc.setDrawColor(128);
        if(w!=0)
          doc.rect(x, y, w, h, 'D');
      })
    }else{
      svg.selectAll(".miniItem").each(function(){
        var self = d3.select(this),
            y = getTranslation(self.attr("transform"))[1] + margin[1] + svgY,
            color = d3.rgb(self.style("fill")),
            selfRect = self.select("rect"),
            x = +selfRect.attr("x") + margin[0],
            w = +selfRect.attr("width"),
            h = +selfRect.attr("height"),
            selfText = self.select("text"),
            txt = selfText.text().replace("← ","<- ");
        doc.setFillColor(color.r,color.g,color.b);
        doc.setTextColor(color.r,color.g,color.b);
        if(x<margin[0])
          x = margin[0];
        doc.rect(x, y, w, h, 'F');
        if(self.select("text").attr("text-anchor")=="end"){
          var fontsize = parseInt(selfText.style("font-size")),
          txtWidth = doc.getStringUnitWidth(txt) * fontsize;
          x = margin[0] + width - txtWidth;
        }
        doc.text(x, y+22, txt);
      })
    }
  })
  doc.setFillColor(255);
  doc.rect(tWidth-15,0,15,tHeight,'F');

  doc.save(d3.select("head>title").text()+".pdf");
}

} // timeline function end

if(typeof multiGraph == 'undefined'){
  window.onload = function(){
    timeline(JSON.parse(d3.select("#data").text()));
  };
}
