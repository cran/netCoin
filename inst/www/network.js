function network(Graph){

  var docSize = viewport(),
      width = docSize.width,
      height = docSize.height,
      oldWidth = 0,
      oldHeight = 0,
      images = false,
      images64 = false,
      heatmap = false,
      heatmapTriangle = false,
      tooltipFixed = false,
      egoNet = false,
      transform = d3.zoomIdentity,
      backupNodes = false,
      frameControls = false,
      repulsionSlider,
      distanceSlider,
      frameSlider,
      timeSlider,
      zoomSlider,
      options;

  var defaultColor = "#1f77b4", // nodes and areas default color
      defaultLinkColor = "#999", // links default color
      defaultShape = "Circle", // node shape by default
      symbolTypes = ["Circle","Square","Diamond","Triangle","Cross","Star","Wye"], // list of available shapes
      nodeSizeRange = [0.5,4], // node size range
      nodeLabelSizeRange = [8,20], // node label size range
      linkWeightRange = [200,40], // link weight range (link distance)
      linkWidthRange = [1,5], // link width range
      zoomRange = [0.1, 10], // zoom range
      chargeRange = [0,-1000], // charge range
      linkDistanceRange = [0,500], // link distance range
      timeRange = [5000,500], // speed range for dynamic net
      axisExtension = 50, // pixels to increase the axes size
      sliderWidth = 200, // width of the sliders
      infoPanelLeft = docSize.width * (2/3), // information panel left position
      findNodeRadius = 16, // radius in which to find a node in the canvas
      noShowFields = ["Source","Target","x","y","source","target","vx","vy","fx","fy","id","selected","nodeSize","noShow","childNodes","parentNode","_frame_"]; // not to show in sidebar controllers or tables

  var simulation = d3.forceSimulation()
      .force("link", d3.forceLink())
      .force("charge", d3.forceManyBody())
      .on("end", forceEnd)
      .stop();

  var body = d3.select("body");

  checkGraphData();

  // main title
  if(options.main){
    body.append("div")
      .attr("class", "main")
      .html(typeof options.main == "string" ? options.main : options.main[0]);
  }

  // panel
  var panel = body.append("div")
      .attr("class", "panel");

  if(options.main)
    panel.style("top",(body.select("div.main").node().offsetHeight+8) + "px")

  var plot = panel.append("div")
      .attr("class", "plot")
      .style("position","relative")

  if(options.note){
    var divNote = plot.append("div")
      .attr("class", "note")
      .html(typeof options.note == "string" ? options.note : "");
  }

  plot.style("width",width+"px")

  displayArrows();
  displayBottomPanel();
  displaySidebar();

  if(options.helpOn)
    displayWindow().html(options.help);

  function checkGraphData(){

    options = Graph.options;
    delete Graph.options;

    simulation.force("link").id(function(d) { return d[options.nodeName]; });

    if(options.background){
      var s = new Option().style;
      s.background = options.background;
      if(s.background != ""){
        body.style("background",s.background);
        s.color = options.background;
        if(s.color!="")
          options.background = s.color;
        else
          delete options.background;
      }else
        delete options.background;
    }

    if(options.frames){
      if(options.frames.length>1 && Graph.linknames.indexOf("_frame_")!=-1){

        var speed = 50;
        if(options.hasOwnProperty("speed"))
          speed = options.speed;
        speed = speed/100 * (timeRange[1]-timeRange[0]) + timeRange[0];

        var frame = 0;
        if(options.frame && options.frame<options.frames.length)
          frame = options.frame;

        frameControls = {
          "play": true,
          "frame": frame,
          "frames": options.frames,
          "frameInterval": null,
          "time": speed,
          "loop": false
        };

        ["zoom","repulsion","distance"].forEach(function(d){
          if(options.hasOwnProperty(d) && Array.isArray(options[d])){
            frameControls[d] = options[d];
            options[d] = options[d][0];
          }
        });
      }
      delete options.frames;
    }

    var nodes = [],
        len = 0;

    len = Graph.nodes[0].length;
    for(var i = 0; i<len; i++){
      var node = {};
      Graph.nodenames.forEach(function(d,j){
        node[d] = Graph.nodes[j][i];
      })
      node.degree = 0;
      splitMultiVariable(node);
      nodes.push(node);
    }
    Graph.nodenames.push("degree");

    Graph.nodes = nodes;

    if(frameControls){
      Graph.nodes.forEach(function(node){
        if(node.hasOwnProperty("fx") && !Array.isArray(node.fx))
          node.fx = frameControls.frames.map(function(){ return node.fx; });
        if(node.hasOwnProperty("fy") && !Array.isArray(node.fy))
          node.fy = frameControls.frames.map(function(){ return node.fy; });
      })
      backupNodes = JSON.parse(JSON.stringify(Graph.nodes));
    }

    if(Graph.links){
      var links = [];

      len = Graph.links[0].length;
      for(var i = 0; i<len; i++){
        var link = {};
        Graph.linknames.forEach(function(d,j){
          link[d] = Graph.links[j][i];
        })
        splitMultiVariable(link);
        link.source = Graph.nodes[link.Source];
        link.Source = link.source[options.nodeName];
        link.target = Graph.nodes[link.Target];
        link.Target = link.target[options.nodeName];
        links.push(link);
      }

      Graph.links = links;
    }else{
      Graph.links = [];
      Graph.linknames = [];
    }

    loadTree();

    ["nodeText","nodeInfo"].forEach(function(d){
      if(options[d])
        noShowFields.push(options[d]);
    });

    if(options.defaultColor)
      defaultColor = options.defaultColor;

    options.colorScalenodeColor = "RdWhGn"; // default linear scale for nodes
    options.colorScalelinkColor = "RdBkGn"; // default linear scale for links

    if(options.nodeBipolar){
      switch(defaultColor) {
        case "black":
        case "#000":
        case "#000000":
          options.colorScalenodeColor = "RdWhBk";
          break;
        case "#2ca02c":
          options.colorScalenodeColor = "RdWhGn";
          break;
        default:
          colorScales['custom1'] = ["#d62728","#ffffff",defaultColor];
          colorScales['custom2'] = [defaultColor,"#ffffff","#d62728"];
          options.colorScalenodeColor = "custom1";
      }
    }else{
      switch(defaultColor) {
        case "black":
        case "#000":
        case "#000000":
          options.colorScalenodeColor = "WhBk";
          break;
        case "#1f77b4":
          options.colorScalenodeColor = "WhBu";
          break;
        case "#2ca02c":
          options.colorScalenodeColor = "WhGn";
          break;
        case "#d62728":
          options.colorScalenodeColor = "WhRd";
          break;
        default:
          var custom = d3.scaleLinear()
            .domain([1,3])
            .range(["#ffffff",defaultColor])
          colorScales['custom1'] = [defaultColor,custom(2),"#ffffff"];
          colorScales['custom2'] = ["#ffffff",custom(2),defaultColor];
          options.colorScalenodeColor = "custom2";
      }
    }

    if(options.cex)
      body.style("font-size", 10*options.cex + "px")
    else
      options.cex = 1;

    if(!options.hasOwnProperty("zoom"))
      options.zoom = 1;

    if(!options.hasOwnProperty("repulsion"))
      options.repulsion = 25;

    if(!options.hasOwnProperty("distance"))
      options.distance = 10;

    if(options.imageNames){
      if(!Array.isArray(options.imageNames))
        options.imageNames = [options.imageNames];
    }else
      options.imageNames = [];
    if(options.imageItems){
      if(!Array.isArray(options.imageItems))
        options.imageItems = [options.imageItems];
      options.nodeShape = options.imageNames[0];
      images = {};
      Graph.nodes.forEach(function(node){
        options.imageItems.forEach(function(col){
          var img = new Image();
          img.src = node[col];
          images[node[col]] = img;
        })
      })
    }else
      options.imageItems = [];

    options.imageItems.forEach(function(d){
      noShowFields.push(d);
    })

    options.showSidebar = showControls(1);
    options.showButtons2 = showControls(2);
    options.showTables = showControls(3);
    options.showButtons = showControls(4);
    options.showExport = showControls(5);

    if(Array.isArray(options.axesLabels)){
      if(options.axesLabels.length>4)
        options.axesLabels.length = 4;
    }else{
      if(options.axesLabels)
        options.axesLabels = [options.axesLabels];
      else
        options.axesLabels = [];
    }

    if(!Array.isArray(options.degreeFilter)){
      if(options.degreeFilter)
        options.degreeFilter = [options.degreeFilter,Infinity];
    }

    if(options.mode && (options.mode=="h" || options.mode[0]=="h")){
      heatmap = true;
    }

    function splitMultiVariable(d){
      for(var p in d) {
        if(p!=options.nodeName){
          if(typeof d[p] == "string" && d[p].indexOf("|")!=-1){
            var aux = d[p].split("|");
            if(!frameControls || aux.length==frameControls.frames.length)
              d[p] = aux.map(function(d){ return isNaN(parseInt(d)) ? d : +d; });
          }
        }
      }
    }

    function showControls(n){
      if(options.hasOwnProperty("controls")){
        if(options.controls===0)
          return false;
        if(options.controls==-n)
          return false;
        if(options.controls==n)
          return true;
        if(Array.isArray(options.controls)){
          if(options.controls.indexOf(-n)!=-1)
            return false;
          if(options.controls.indexOf(n)!=-1)
            return true;
        }
      }
      return null;
    }
  }

  function loadFrameData(frame){
    for(var i=0; i<Graph.nodes.length; i++){
      Graph.nodenames.forEach(function(col){
        if(Array.isArray(backupNodes[i][col]))
          Graph.nodes[i][col] = backupNodes[i][col][frame];
      })
    }
    loadTree(frameControls.frame);
  }

  function loadTree(frame){
      if(!Graph.tree)
        return;
      if(frameControls && frame===undefined)
        return;

      Graph.nodes.forEach(function(node){
        node.childNodes = [];
        node.parentNode = false;
      })
      var len = Graph.tree[0].length;
      for(var i = 0; i<len; i++){
        if(frame===undefined || frame==Graph.tree[2][i]){
          var source = Graph.tree[0][i],
              target = Graph.tree[1][i];
          Graph.nodes[source].childNodes.push(Graph.nodes[target]);
          Graph.nodes[target].parentNode = Graph.nodes[source];
        }
      }
  }

function displayArrows(){

  var buttonsArrow = visArrow()
    .item("showButtons")
    .top("10px")
    .left("0px")
    .title(texts.showhidebuttons)
    .callback(function(){ plot.call(drawSVG); })

  panel.call(buttonsArrow);

  var sidebarArrow = visArrow()
    .item("showSidebar")
    .top((10+(20*options.cex))+"px")
    .left("0px")
    .title(texts.showhidesidebar)
    .callback(displaySidebar)

  panel.call(sidebarArrow);

  var tablesArrow = visArrow()
    .item("showTables")
    .vertical(true)
    .bottom("0px")
    .left("0px")
    .title(texts.showhidetables)
    .callback(displayBottomPanel)

  panel.call(tablesArrow);

  var buttons2Arrow = visArrow()
    .item("showButtons2")
    .vertical(true)
    .bottom("0px")
    .left("24px")
    .title(texts.showhidebuttons)
    .callback(displayBottomPanel)

  panel.call(buttons2Arrow);
}

function displayBottomPanel(){

  panel.select("div.panel-dragbar").remove();
  panel.select("div.tables").remove();

  height = computeHeight();
  plot.style("height",height+"px");

  if(!plot.selectAll("svg, canvas").empty())
    plot.call(drawSVG);

  if(options.showButtons2 || options.showTables){

    // panel dragbar
    var dragbar = panel.append("div")
      .attr("class","panel-dragbar")
      .style("cursor","row-resize")
      .style("height","5px")
      .style("width","100%");

    var dragOffset;

    dragbar.call(d3.drag()
      .on("start", function() {
        body.style("cursor","row-resize");
        plot.select("canvas").remove();
        plot.select("svg").remove();
        dragOffset = d3.mouse(body.node())[1]-height;
      })
      .on("drag", function() {
        var value = d3.mouse(body.node())[1];
        if(value > 200){
          height = value-dragOffset;
          plot.style("height",height+"px");
        }
      })
      .on("end", function() {
        body.style("cursor",null);
        plot.call(drawSVG);
      })
    );

    // tables
    var tables = panel.append("div")
      .attr("class", "tables")

  if(options.showTables){
    var tablesoffset = 18+12*options.cex*1.2;
    dragbar.style("margin-bottom",tablesoffset+"px");
    tables.style("min-height","150px");
    tables.style("margin-top",-tablesoffset+"px")
    tables.append("div")
      .attr("class","switchNodeLink")
      .selectAll("div")
        .data(["nodes","links"])
        .enter().append("div")
          .style("top","-"+ tablesoffset +"px")
          .on("click",function(d){
              tables.selectAll("div.switchNodeLink > div")
                .style("background",null)
                .style("border-bottom-color",null)
              d3.select(this)
                .style("background","#f5f5f5")
                .style("border-bottom-color","#f5f5f5")
              tables.selectAll("div.nodes,div.links").style("display","none")
              tables.select("div."+d).style("display",null)
          })
          .append("h3")
            .text(function(d){ return texts[d]; })
    tables.select("div.switchNodeLink > div")
      .style("background","#f5f5f5")
      .style("border-bottom-color","#f5f5f5")
  }

  if(options.showExport!==false){
    if(options.help){
      iconButton(tables,"help",infoIcon_b64,"info",function(){
        var win = displayWindow();
        win.html(options.help);
      });
    }

    if(options.showTables)
      iconButton(tables,"xlsx",xlsxIcon_b64,texts.downloadtable,tables2xlsx);

    iconButton(tables,"pdf",pdfIcon_b64,texts.pdfexport,function(){
embedImages(svg2pdf); });
  }

  if(frameControls){
    var divFrameCtrl = tables.append("div")
      .attr("class", "divFrameCtrl")

    var buttonBackColor = "#888";

    divFrameCtrl.append("select")
      .attr("class","selectFrame")
      .on("change",function(){
        frameControls.play = false;
        clickThis();
        handleFrames(+(this.value));
      })
      .selectAll("option")
        .data(frameControls.frames)
      .enter().append("option")
        .property("value",function(d,i){ return i; })
        .text(function(d,i){
          if(Array.isArray(options.main))
            return options.main[i];
          else
            return d;
        })

    divFrameCtrl.append("button") // prev
      .html(getSVG(['M0,0L2,0L2,8L0,8Z','M8,0L8,8L2,4Z']))
      .on("click",function(){
        var val = frameControls.frame-1;
        if(val < 0)
          val = frameControls.frames.length+val;
        frameControls.play = false;
        clickThis();
        handleFrames(val);
      })
    divFrameCtrl.append("button") // loop
      .html(getSVG(['m5.8204 4.576c0 1.015-0.8054 1.8204-1.8204 1.8204s-1.8204-0.8054-1.8204-1.8204 0.8054-1.8204 1.8204-1.8204v-1.6036c-1.8815 0-3.424 1.5425-3.424 3.424s1.5425 3.424 3.424 3.424 3.424-1.5425 3.424-3.424z','m4 0v4l2.5-2z']))
      .on("click",function(){
        frameControls.loop = !frameControls.loop;
        d3.select(this).style("background-color",frameControls.loop?buttonBackColor:null)
          .selectAll("path").style("fill",frameControls.loop?"#f5f5f5":null);
      })
    var stopRecord = function(){
          frameControls.recorder.stop();
          frameControls.recorder.save(Math.round((new Date()).getTime() / 1000)+'record.webm');
          delete frameControls.recorder;
          divFrameCtrl.select("button.rec").style("background-color",null)
            .select("path").style("fill",null);
    }
    divFrameCtrl.append("button") // rec
      .attr("class","rec")
      .html(getSVG(['m8 4a4 4 0 0 1 -4 4 4 4 0 0 1 -4 -4 4 4 0 0 1 4 -4 4 4 0 0 1 4 4z']))
      .on("click",function(){
        if(heatmap){
          displayWindow(texts.alertrecordheatmap);
        }else{
          if(frameControls.recorder){
            stopRecord();
          }else{
            frameControls.recorder = new CanvasRecorder(d3.select("div.plot > canvas").node());
            simulation.restart();
            if(frameControls.recorder.start && frameControls.recorder.start()){
              d3.select(this).style("background-color",buttonBackColor)
                .select("path").style("fill","Red");
            }else{
              delete frameControls.recorder;
            }
          }
        }
      }).style("background-color", frameControls.recorder ? buttonBackColor : null)
      .select("path").style("fill", frameControls.recorder ? "#d62728" : null);
    divFrameCtrl.append("button") // stop
      .html(getSVG(['M0,0L8,0L8,8L0,8Z']))
      .on("click",function(){
        frameControls.play = false;
        clickThis();
        handleFrames(0);
        if(frameControls.recorder){
          stopRecord();
        }
      })
    divFrameCtrl.append("button") // pause
      .attr("class","pause")
      .html(getSVG(['M1,0L3,0L3,8L1,8Z','M5,0L7,0L7,8L5,8Z']))
      .on("click",function(){
        frameControls.play = false;
        clickThis();
        clearInterval(frameControls.frameInterval);
      })
    divFrameCtrl.append("button") // play
      .attr("class","play")
      .html(getSVG(['M1,0L1,8L7,4Z']))
      .on("click",function(){
        frameControls.play = true;
        clickThis(true);
        handleFrames(frameControls.frame+1);
      })
    divFrameCtrl.append("button") // next
      .html(getSVG(['M0,0L0,8L6,4Z','M8,0L6,0L6,8L8,8Z']))
      .on("click",function(){
        frameControls.play = false;
        clickThis();
        handleFrames(frameControls.frame+1);
      })

    clickThis(frameControls.play);

    function clickThis(play){
      divFrameCtrl.selectAll("button.pause, button.play")
        .style("background-color",null)
        .selectAll("path").style("fill",null);
      if(play)
        divFrameCtrl.select("button.play")
          .style("background-color",buttonBackColor)
          .selectAll("path").style("fill","LawnGreen");
      else
        divFrameCtrl.select("button.pause")
          .style("background-color",buttonBackColor)
          .selectAll("path").style("fill","#f5f5f5");
    }

    function getSVG(d){
      var path = '';
      d.forEach(function(dd){
        path += '<path d="'+dd+'"></path>';
      })
      return '<svg xmlns="http://www.w3.org/2000/svg" width="8" height="8">'+path+'</svg>';
    }
  }

  if(options.showButtons2){
    var buttonsSelect = tables.append("div")
          .attr("class","selectButton")

    buttonsSelect.append("span").text(texts.select+": ");
    buttonsSelect.append("input")
      .attr("type", "text")
      .attr("placeholder","search...")
      .on("keyup",function(){
        var txt = d3.select(this).property("value");
        if(txt.length>1){
          txt = new RegExp(txt,'i');
          Graph.nodes.forEach(function(node){
            node.selected = false;
            if(checkSelectable(node)){
              var i = 0;
              while(!node.selected && i<Graph.nodenames.length){
                if(String(node[Graph.nodenames[i++]]).match(txt))
                  node.selected = true;
              }
            }
          });
          if(!heatmap)
            simulation.restart();
          showTables();
        }
      })
    buttonsSelect.append("span").text(" ");

    var selectButton = function(txt,clk){
          buttonsSelect.append("div")
            .text(txt)
            .on("click",clk)
        }

    selectButton(texts.selectall,selectAllNodes);
    selectButton(texts.tableselection,selectNodesFromTable);
    selectButton(texts.selectneighbors,addNeighbors);
    selectButton(texts.isolateselection,filterSelection);
    selectButton(texts.egoNet,displayEgoNet);
    if(Graph.tree)
      selectButton(texts.expandcollapse,treeAction);
    selectButton(texts.resetfilter,deleteNoShow);
  }

    if(options.showTables){
      if(options.scenarios)
        tables.append("h3").text(texts.scenarios + ": " + options.scenarios);
      tables.append("div").attr("class","nodes");
      tables.append("div").attr("class","links").style("display","none");
      showTables();
    }
  }
}

function displaySidebar(){
  var sidebar = body.select("div.sidebar"),
      sidebarOffset = 190 * Math.sqrt(options.cex),
      dragbar = body.select("body>div.dragbar");

  if(sidebar.empty()){

    sidebar = body.append("div")
      .attr("class", "sidebar")
      .style("width", sidebarOffset-15 + "px")

    if(typeof multiGraph != 'undefined'){
      var multiSel = sidebar.append("div")
        .attr("class","subSidebar multigraph");
      multiSel.append("h3").text(texts.netselection+":");
      multiGraph.graphSelect(multiSel);
    }

  // dragbar
  if(dragbar.empty()){
    dragbar = body.append("div")
      .attr("class","dragbar")
      .style("width","5px")
      .style("cursor","col-resize")
      .style("position","absolute")
      .style("top","0px")
      .style("left",(sidebarOffset-5) + "px")
      .style("z-index",1);

    dragbar.call(d3.drag()
      .on("start", function() {
        body.style("cursor","col-resize");
        if(options.showSidebar){
          plot.select("canvas").remove();
          plot.select("svg").remove();
        }
      })
      .on("drag", function() {
        var value = d3.mouse(body.node())[0];
        if(value > 177 && value < 400){
          dragbar.style("left", value + "px")
          sidebar.style("width", (value-10) + "px")
          if(options.showSidebar){
            panel.style("left", (value+5) + "px")
            width = docSize.width - 25 - value;
            plot.style("width",width+"px");
          }
        }
      })
      .on("end", function() {
        body.style("cursor",null);
        d3.selectAll(".sidebar select.attrSel").each(function(){ 
          d3.select(this).dispatch("change");
        });
        if(options.showSidebar)
          plot.call(drawSVG);
      })
    );
  }

  }else{
    sidebar.selectAll("div.sidebar>div:not(.multigraph)").remove();
  }

  if(options.showSidebar){

    sidebarOffset = parseInt(sidebar.style("width"))+15;
    panel.style("left", sidebarOffset + "px");

  var divControl, applyFuncObject;
    
// sidebar nodes
  var sideNodes = sidebar.append("div")
    .attr("class","subSidebar nodes")

  sideNodes.append("h3").text(texts.nodes);

  var visData = heatmap?["Label","Color","Shape","Legend","OrderA","OrderD"]:["Label","Size","LabelSize","Color","Shape","Legend","Group"];
  divControl = sideNodes.append("div")
      .attr("class", "nodeAuto")
  addController(divControl, Graph.nodes, false, visData);

  sideNodes.call(filterSwitch);

  applyFuncObject = {};
  applyFuncObject[texts.filter] = applyFilter;
  applyFuncObject[texts.select] = applySelection;
  applyFuncObject[texts.egoNet] = function(query,data){
        applySelection(query,data);
        switchEgoNet();
    };

  divControl = sideNodes.append("div")
      .attr("class","nodeSelect filter");
  addController(divControl, Graph.nodes, applyFuncObject);

// sidebar links
  var sideLinks = sidebar.append("div")
      .attr("class", "subSidebar links")

  sideLinks.append("h3").text(texts.links);

  visData = heatmap?["Intensity","Color","Text"]:["Width","Weight","Color","Text"];
  divControl = sideLinks.append("div")
      .attr("class", "linkAuto");
  addController(divControl, Graph.links, false,visData);

  sideLinks.call(filterSwitch);

  applyFuncObject = {};
  applyFuncObject[texts.filter] = applyFilter;
  divControl = sideLinks.append("div")
      .attr("class","linkSelect filter");
  addController(divControl, Graph.links, applyFuncObject);

// sidebar simple/advanced button
  var advanced = false;
  sidebar.append("div")
    .style("text-align","right")  
    .append("a")
      .text(texts.simpleadvanced)
      .on("click", function(){
        d3.event.preventDefault();
        if(advanced)
          sidebar.selectAll(".advanced").style("display","none")
        else
          sidebar.selectAll(".advanced").style("display",null)
        advanced = !advanced;
      })

  }else{
    sidebarOffset = 0;
    panel.style("left", null);
  }
  dragbar.style("height",(8 + parseInt(sidebar.style("height"))) + "px");
  width = docSize.width - sidebarOffset - 20;
  plot.style("width",width+"px");
  plot.call(drawSVG);
  applyDegreeFilter();

  function filterSwitch(sel){
    sel.append("h4")
      .on("click", function(){
        var div = d3.select(this.parentNode).select(".filter"),
            show = div.style("display")!="block";
        d3.select(this).html(texts.filter + (show?" &#9652;":" &#9662;"))
        div.style("display",show?"block":null);
        if(show)
          div.select("select.attrSel").dispatch("change");
      })
      .html(texts.filter+" &#9662;");
  }
}

function visArrow(){
    var item,
        vertical = false,
        top = null,
        left = null,
        bottom = null,
        title = null,
        callback = false,
        arrows = ["&#9666;","&#9656;"];

    function visArrow(panel){
      if(options[item]!==false){
        plot.append("div")
          .attr("class","showhideArrow")
          .style("top",top)
          .style("left",left)
          .style("bottom",bottom)
          .attr("title",title)
          .call(changeArrow)
          .on("click",function(){
            options[item] = !options[item];
            d3.select(this).call(changeArrow);
            if(callback)
              callback();
          })
      }
    }

    function changeArrow(div){
        div.html(function(){ return options[item] ? arrows[0] : arrows[1]});
    }

    visArrow.item = function(x) {
      if (!arguments.length) return item;
      item = x;
      return visArrow;
    };

    visArrow.vertical = function(x) {
      if (!arguments.length) return vertical;
      vertical = x;
      arrows = vertical?["&#9662;","&#9652;"]:["&#9666;","&#9656;"];
      return visArrow;
    };

    visArrow.top = function(x) {
      if (!arguments.length) return top;
      top = x;
      return visArrow;
    };

    visArrow.left = function(x) {
      if (!arguments.length) return left;
      left = x;
      return visArrow;
    };

    visArrow.bottom = function(x) {
      if (!arguments.length) return bottom;
      bottom = x;
      return visArrow;
    };

    visArrow.title = function(x) {
      if (!arguments.length) return title;
      title = x;
      return visArrow;
    };

    visArrow.callback = function(x) {
      if (!arguments.length) return callback;
      callback = x;
      return visArrow;
    };

    return visArrow;
}

function addController(contr, data, applyFunc, visData){
  var typeData = contr.attr("class").slice(0, 4),
      attrData = Graph[typeData+"names"].filter(function(d){ return noShowFields.indexOf(d)==-1; });

if(visData){

  var selectVisual = function(sel){
    var sels = sel.selectAll("visSel")
        .data(visData)
      .enter().append("div")
        .attr("class","visSel")
        .property("value",String)
    sels.append("span")
          .text(function(d){ return texts[d]; });
    sels.append("select")
    .on("change", function(){
      var visual = this.parentNode.value,
          attr = this.value,
          applyFunc = function(pickerProperty){ applyAuto(typeData,visual,attr,pickerProperty); }
      if(visual=="Color"||visual=="Group"){
        if(dataType(data,attr) == "number"){
          displayPicker(attr,applyFunc);
          return;
        }
      }
      applyFunc(false);
    })
    .selectAll("option")
        .data(attrData)
      .enter().append("option")
        .property("selected",function(d){
          var visual = this.parentNode.parentNode.value;
          if(options[typeData+visual]==d)
            return true;
          else
            return null;
        })
        .property("value",String)
        .text(String)
    sel.append("div").attr("class","clear")
  }

  var displayPicker = function(attr,callback){
    var picker = displayWindow(),
        scaleKeys = d3.keys(colorScales),
        fontsize = parseInt(picker.style("font-size"));
    picker.append("h2").text(texts.selectacolorscale+"\""+attr+"\"");
    picker
              .append("ul")
              .attr("class","picker")
              .selectAll("li")
              .data(scaleKeys)
                .enter().append("li")
                .property("val",String)
                .on("click",function(){
                  callback(this.val);
                  d3.select(picker.node().parentNode).remove();
                })
                .append("canvas")
                  .attr("id", function(d){ return "canvas"+d; })
                  .attr("width",fontsize*6)
                  .attr("height",fontsize)
                  .text(String)
    scaleKeys.forEach(function(d){
      var c = document.getElementById("canvas"+d);
      var ctx = c.getContext("2d");

      // Create gradient
      var grd = ctx.createLinearGradient(0,0,fontsize*6,0);
      grd.addColorStop(0,colorScales[d][0]);
      grd.addColorStop(0.5,colorScales[d][1]);
      grd.addColorStop(1,colorScales[d][2]);

      // Fill with gradient
      ctx.fillStyle = grd;
      ctx.fillRect(0,0,fontsize*6,fontsize);
    });
  }

  attrData.unshift("-"+texts.none+"-");
  contr.call(selectVisual);

}else{

  var attrSelect = contr.append("select")
      .attr("class","attrSel");

  attrSelect.selectAll("option")
    .data(attrData)
    .enter().append("option")
      .property("value",String)
      .text(String)

    var selectedValues = {};

    var changeQuery = function(opA,opB){
      var key = attrSelect.property("value"),
          query = new Query(txtQuery.property("value")),
          b = opB == "=";
      selectedValues[key].forEach(function(d,i){
        if(typeof d == 'number'){
          if(b)
            opB = i==0?">":"<";
          else
            opB = i==0?"<":">";
        }
        query.add2Query(key,d,opA,opB);
      });

      txtQuery.property("value",query.query);
    }

    var changeAttrSel = function(val){
      valSelector.html("")
      var type = dataType(data,val);
      if(type == 'number'){
        var extent = d3.extent(data, function(d){ return d[val]; }),
            baseWidth = parseInt(valSelector.style("width"));
        if(!selectedValues[val])
          selectedValues[val] = extent;
        brushSlider(valSelector,extent,selectedValues[val],function(s){ selectedValues[val] = s; },baseWidth);
      }else{
        var dat = data.map(function(d){ return d[val]; });
        if(type != 'string')
          dat = dat.reduce(function(a,b) { return b ? a.concat(b) : a; }, []);
        valSelector.style("height",null);
        valSelector.append("select")
          .attr("multiple","multiple")
          .on("blur", function() { loadSelValues(val); })
          .selectAll("option")
        .data(d3.set(dat).values().sort())
          .enter().append("option")
          .property("value",function(d){ return d.replace(/\'/g, "\\'"); })
          .text(stripTags)
          .each(function(d){ if(selectedValues[val] && selectedValues[val].indexOf(d)!=-1)this.selected = true; })
      }
    }

    var loadSelValues = function(val){
      selectedValues[val] = [];
      valSelector.selectAll("option").each(function(){
        if(this.selected)
          selectedValues[val].push(this.value);
      })
      if(selectedValues[val].length == 0)
        delete selectedValues[val];
    }

  attrSelect.on("change", function() { 
    changeAttrSel(this.value);
  })

  var valSelector = contr.append("div")
      .attr("class","valSel")
      .style("margin-bottom",0);

  var advanced = contr.append("div")
        .attr("class","advanced")
        .style("display","none");

  advanced.append("button")
      .text("And")
      .on("click", function(){ changeQuery("and","="); });

  advanced.append("button")
      .text("Or")
      .on("click", function(){ changeQuery("or","="); });

  advanced.append("button")
      .text("Nand")
      .on("click", function(){ changeQuery("and","not"); });

  advanced.append("button")
      .text("Nor")
      .on("click", function(){ changeQuery("or","not"); });

  var txtQuery = advanced.append("textarea")
    .attr("name","query")
    .property("value","")
    .on("focus", function(){ txtQuery.style("height", "160px") })
    .on("blur", function(){ txtQuery.style("height", null) })

  contr.append("button")
      .text(texts.clear)
      .on("click", function(){
        selectedValues = {};
        changeAttrSel(attrSelect.property("value"));
        txtQuery.property("value","");
      });

  changeAttrSel(attrSelect.property("value"));

  if(typeof applyFunc == 'object'){
    var prepareQuery = function(){
      var query = null;
      if(txtQuery){
        if(advanced.style("display")=="none"){
          query = selectedValues2str(selectedValues,data);
        }else{
          query = new Query(txtQuery.property("value"));
          query = query.toJS();
        }
      }
      return query;
    }
    for(var i in applyFunc){
      contr.append("button")
        .text(i)
        .on("click", function(){ 
          applyFunc[this.textContent](prepareQuery(),data); 
        });
    }
  }
}
} // end of addController
  
function applyDegreeFilter(){
  if(options.degreeFilter){
    var query = "d.degree >= " + options.degreeFilter[0] + " && d.degree <= " + options.degreeFilter[1];
    applyFilter(query,Graph.nodes);
  }
}

function applyFilter(query,data){
  data.forEach(function(d){ 
      if(eval(query)){
        delete d.noShow;
      }else{
        d.noShow = true;
        delete d.selected;
      }
  });
  drawNet();
}

function applySelection(query,data){
  data.forEach(function(d){
    if(eval(query))
      d.selected = true;
    else
      delete d.selected;
  });
  if(!heatmap)
    simulation.restart();
  showTables();
}

function applyAuto(item, visual, attr, pickerProperty){
    if(pickerProperty==false || attr=="-"+texts.none+"-")
        delete options["colorScale"+item+visual];
    else
        options["colorScale"+item+visual] = pickerProperty;
    if(attr=="-"+texts.none+"-")
        delete options[item+visual];
    else
        options[item+visual] = attr;
    if(item+visual == "nodeOrderA")
        delete options.nodeOrderD;
    if(item+visual == "nodeOrderD")
        delete options.nodeOrderA;
    if((item+visual == "nodeColor" && !options["colorScalenodeColor"]) || item+visual == "nodeShape")
        options.showLegend = true;
    drawNet();
}

function Query(q){
  this.query = q,
  this.ops = [" && "," || "," == "," != ","d['","']"],
  this.rpl = [" and "," or "," \\= "," not ","\\[","\\]"];
}

Query.prototype = {
  toJS: function() {
    var js = this.query;
    for(var i = 0;i<this.ops.length;i++)
      js = js.replace(new RegExp(this.rpl[i],"g"),this.ops[i]);
    return js!=""?js:"true";
  },
  add2Query: function(key,val,opA,opB) {
    var str = ((this.query != "")?" " + opA + " ":"")+"[" + key + "] " + opB + " '" + val + "'";
    this.query = this.query+str;
  }
}

function drawSVG(sel){

  sel.select("canvas").remove();
  sel.select("svg").remove();

  var zoom = d3.zoom()
    .scaleExtent(zoomRange)
/*    .filter(function(){
      return !d3.event.button && (d3.event.ctrlKey || d3.event.metaKey) && !d3.event.shiftKey;
    }) */
    .on("end",function(){
      d3.select(this).style("cursor","grab");
    }) 
    .on("zoom", zoomed)

  adaptLayout();

  simulation
      .force("x", d3.forceX().strength(0.1))
      .force("y", d3.forceY().strength(0.1))

  body
    .on("keydown.viewbrush", keyflip)
    .on("keyup.viewbrush", keyflip)
  
  var svg = sel.insert("svg",":first-child")
      .attr("xmlns","http://www.w3.org/2000/svg")
      .attr("width", width)
      .attr("height", height)
    .style("position","absolute")
    .style("top",0)
    .style("left",0)

  var canvas = sel.insert("canvas",":first-child")
    .attr("width", width)
    .attr("height", height)

    if(options.nodeText){
      body.append("div")
          .attr("class","tooltip")
    }

    svg.append("style")
     .text("text { font-family: sans-serif; font-size: "+body.style("font-size")+"; } "+
".scale text { font-size: 120%; fill: #444; } "+
".scale text:first-child { font-size: 160%; text-anchor: middle; } "+
".legend text.title { text-anchor: end; font-size: 160%; } "+
".axisLabel { stroke-width: 0.5px; font-size: 100%; fill: #999; } "+
".label { font-size: 100%; fill: #444; } "+
"line.axis { stroke: #aaa; }"+
"g.heatmap path.cluster { stroke: #666; fill: none; }"+
".cellText { font-weight: bold; fill: #fff; }");

  var defs = svg.append("defs");
  d3.keys(colorScales).forEach(function(d){ addGradient(defs,d,colorScales[d]); });

  defs.append("clipPath")
    .attr("id","heatmapClip")
    .append("rect")
      .attr("x",-height)
      .attr("y",-height/2)
      .attr("width",height*2)
      .attr("height",height)

  svg.append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", width)
    .attr("height", height)
    .attr("pointer-events","all")
    .style("fill","none")
    .on("click",clickNet)
    .on("dblclick",dblClickNet)
    .on("mousemove",hoverNet)
    .on("mousedown.grabbing",function(){
      d3.select(this).style("cursor","grabbing");
    })
    .on("mouseup.grabbing",function(){
      d3.select(this).style("cursor","grab");
    })
    .call(d3.drag()
          .subject(dragsubject)
          .on("start", dragstarted)
          .on("drag", dragged)
          .on("end", dragended))
    .call(zoom)
    .on("dblclick.zoom",null)

  var net = svg.append("g")
    .attr("class","net")

  var brush = d3.brush()
      .filter(function(){
        return !d3.event.button && d3.event.shiftKey;
      })
      .extent( [ [0,0], [width,height] ] )
        .on("start", function() {
          brushg.selectAll('.selection').style("display",null);
        })
        .on("end", function() {
          d3.selectAll(".legend > g > text").style("stroke",function(){
            delete this.parentNode.selected;
            return null;
          });
          var extent = d3.event.selection;
          if(extent){
            extent[0][0] = transform.invertX(extent[0][0]);
            extent[0][1] = transform.invertY(extent[0][1]);
            extent[1][0] = transform.invertX(extent[1][0]);
            extent[1][1] = transform.invertY(extent[1][1]);
            Graph.nodes.forEach(function(node) {
              node.selected = checkSelectable(node) && (node.selected ^ (extent[0][0] <= node.x && node.x < extent[1][0] && extent[0][1] <= node.y && node.y < extent[1][1]));
            });
          }
          simulation.restart();
          showTables();
          brushg.selectAll('.selection').style("display","none");
        });

  var brushg = svg.append("g")
    .attr("class","brush")
    .call(brush)

  brushg.selectAll('.handle').remove();

  brushg.style("display","none");

  svg.append("g").attr("class","scale")
    .attr("transform", "translate("+(width-320)+",20)");

  var size = Math.min(width,height);
  chargeRange = [0,-(size*2)];
  linkDistanceRange = [0,size*3/4];

  if(!options.hasOwnProperty("charge"))
      options.charge = chargeRange[1] * (options.repulsion/100);
  if(!options.hasOwnProperty("linkDistance"))
      options.linkDistance = linkDistanceRange[1] * (options.distance/100);
  if(!options.hasOwnProperty("zoomScale"))
      resetZoom();
  net.attr("transform", transform);

  zoomSlider = displaySlider()
      .domain(zoomRange)
      .text("Zoom")
      .prop('zoomScale')
      .callback(function(value){
          options.zoomScale = value;
          transform.k = options.zoomScale;
          net.attr("transform", transform);
          if(!heatmap)
            simulation.restart();
      });

  if(frameControls){
      frameSlider = displaySlider()
      .domain([0,frameControls.frames.length-1])
      .domain2([1,frameControls.frames.length])
      .rounded(true)
      .text("Frame")
      .prop('frames')
      .callback(frameStep);

      timeSlider = displaySlider()
      .domain(timeRange)
      .domain2([0,100])
      .text(texts.speed)
      .prop('time')
      .callback(function(value){
        frameControls.time = value;
        if(frameControls.play)
          handleFrames(frameControls.frame);
      });
  }

  var left = !options.showSidebar && !options.main && typeof multiGraph != 'undefined' && !d3.select(".sidebar").empty() ? parseInt(d3.select(".sidebar").style("width"))+15 : 0;

  sel.selectAll(".showhideArrow").filter(function(d,i){ return i<2; }).style("left",left+"px");

  if(options.showButtons){
    var buttons = svg.append("g")
        .attr("class", "buttons")
        .attr("transform", "translate(30,20)")

    var sliders = buttons.append("g")
        .attr("class","sliders")
        .attr("transform",left ? "translate("+left+",0)":null)

    var countY = 8;

    distanceSlider = displaySlider()
      .y(countY*options.cex)
      .domain(linkDistanceRange)
      .domain2([0,100])
      .text(texts.distance)
      .prop('linkDistance')
      .callback(function(value){
        options['linkDistance'] = value;
        update_forces();
      })
    sliders.call(distanceSlider);
    distanceSlider.move(options['linkDistance']);

    countY += 18;

    repulsionSlider = displaySlider()
      .y(countY*options.cex)
      .domain(chargeRange)
      .domain2([0,100])
      .text(texts.repulsion)
      .prop('charge')
      .callback(function(value){
        options['charge'] = value;
        update_forces();
      })
    sliders.call(repulsionSlider);
    repulsionSlider.move(options['charge']);

    countY += 18;

    zoomSlider.y(countY*options.cex)
    sliders.call(zoomSlider);

    countY += 18;

    if(frameControls){
      frameSlider.y(countY*options.cex)
      sliders.call(frameSlider);
      frameSlider.move(frameControls.frame);

      countY += 18;

      timeSlider.y(countY*options.cex)
      sliders.call(timeSlider);
      timeSlider.move(frameControls.time);

      countY += 18;
    }

    countY += 8;

    loadSVGbuttons(countY);
  }

  zoomSlider.update(options.zoomScale);

  if(frameControls)
    handleFrames(frameControls.frame);
  else
    drawNet();

  function zoomed() {
      transform = d3.event.transform;
      options.zoomScale = transform.k;
      zoomSlider.update(options.zoomScale);
      zoomSlider.brushedValue(true);
  }

  function keyflip() {
    if(d3.event.shiftKey){
      if(!heatmap){
        brushg.style("display",null);
      }
    }else{
      brushg.style("display","none");
    }
  }

  function loadSVGbuttons(count){
  var dat = [],
      datStopResume = {txt: texts.stopresume, callback: stopResumeNet},
      datScale = {txt: texts.resetzoom, callback: function(){
        resetZoom();
        zoomSlider.update(options.zoomScale);
        zoomSlider.brushedValue(false);
      }, gap: 5},
      datDirectional = {txt: texts.directional, callback: function(){
        options.showArrows = !options.showArrows;
        if(heatmap)
          drawNet();
        else
          simulation.restart();
      }},
      datLegend = {txt: texts.showhidelegend, callback: function(){
        options.showLegend = !options.showLegend;
        clickHide(d3.selectAll(".scale"), options.showLegend);
      }},
      datAxes = {txt: texts.showhideaxes, callback: function(){
        options.showAxes = !options.showAxes;
        clickHide(d3.selectAll(".net .axis"), options.showAxes);
        clickHide(d3.selectAll(".net .axisLabel"), options.showAxes);
      }},
      datMode = {txt: texts.netheatmap, callback: function(){
        heatmap = !heatmap;
        resetZoom();
        displaySidebar();
      }, gap: 5},
      datReset = {txt: texts.reset, callback: function(){ location.reload(); }},
      datPyramid = {txt : texts.trianglesquare, callback: function(){
        heatmapTriangle = !heatmapTriangle;
        drawNet();
      }};

  if(heatmap)
    dat = [datPyramid];
  else
    dat = [datStopResume];

  dat.push(datScale);
  dat.push(datDirectional);
  dat.push(datLegend);
  if(!heatmap) dat.push(datAxes);

  if(Array.isArray(options.mode))
    dat.push(datMode);
  else
    datReset.gap = 5;

  dat.push(datReset);
  
  var gButton = buttons.selectAll(".button")
        .data(dat, function(d){ return d.txt; })

  gButton.exit().remove();

  gButton.enter().append("g")
    .attr("class","button")
    .each(function(d,i){

  d3.select(this).append("rect")
    .attr("x",0)
    .attr("y",5*(options.cex-1))
    .attr("rx",2)
    .attr("ry",2)
    .attr("width",30)
    .attr("height",10)
    .on("click",function(){
      d.callback();
    });

  d3.select(this).append("text")
      .attr("x",35)
    .attr("y",9*options.cex)
    .text(d.txt);

    })
    .merge(gButton)
    .attr("transform",function(d){
      if(d.gap)
        count += d.gap;
      var val = "translate(0,"+(count*options.cex)+")";
      count += 15;
      return val;
    })

    // zoom in
    var zoomin = buttons.append("g")
      .attr("class","zoombutton")
      .attr("transform","translate("+(width-55)+","+(height-70)+")")
      .on("click",function(){
        transform.k = transform.k + 0.1;
        if(transform.k>zoomRange[1]){
          transform.k = zoomRange[1];
          return;
        }
        options.zoomScale = transform.k
        zoomSlider.update(options.zoomScale);
        zoomSlider.brushedValue(true);
      })

    zoomin.append("rect")
      .attr("x",0)
      .attr("y",0)
      .attr("rx",2)
      .attr("ry",2)
      .attr("width",20)
      .attr("height",20)
    zoomin.append("rect")
      .attr("x",5)
      .attr("y",8)
      .attr("width",10)
      .attr("height",4)
      .style("stroke","none")
      .style("fill","#666")
    zoomin.append("rect")
      .attr("x",8)
      .attr("y",5)
      .attr("width",4)
      .attr("height",10)
      .style("stroke","none")
      .style("fill","#666")

    // zoom out
    var zoomout = buttons.append("g")
      .attr("class","zoombutton")
      .attr("transform","translate("+(width-55)+","+(height-45)+")")
      .on("click",function(){
        transform.k = transform.k - 0.1;
        if(transform.k<zoomRange[0]){
          transform.k = zoomRange[0];
          return;
        }
        options.zoomScale = transform.k
        zoomSlider.update(options.zoomScale);
        zoomSlider.brushedValue(true);
      })

    zoomout.append("rect")
      .attr("x",0)
      .attr("y",0)
      .attr("rx",2)
      .attr("ry",2)
      .attr("width",20)
      .attr("height",20)
    zoomout.append("rect")
      .attr("x",5)
      .attr("y",8)
      .attr("width",10)
      .attr("height",4)
      .style("stroke","none")
      .style("fill","#666")
  }

  function displaySlider(){
    var scale,
        brush,
        slider,
        bubble,
        y = 0,
        domain = [1,0],
        domain2 = false,
        scale2 = false,
        text = "",
        prop = "",
        callback = null,
        rounded = false,
        brushedValue = false;

    function displaySlider(sliders){
      scale = d3.scaleLinear()
        .clamp(true)
        .domain(domain)
        .range([0, sliderWidth])

      scale2 = d3.scaleLinear()
        .clamp(true)
        .domain(domain2 ? domain2 : domain)
        .range([0, sliderWidth])

      brush = d3.brushX().extent([[-6,0], [sliderWidth + 6,12]]);

      sliders = sliders.append("g")
            .attr("class","slider "+prop)

      var x = 0;

      sliders.append("text")
        .attr("x", x + sliderWidth + 10)
        .attr("y", y + 3*options.cex)
        .text(text);

      slider = sliders.append("g")
        .attr("transform", "translate("+ x +","+ y +")")
        .attr("class", "x axis brushSlider")
        .call(d3.axisBottom(scale)
          .tickSize(0)
          .ticks(0))
        .append("g")
          .attr("class", "slider")
          .attr("transform", "translate(0,-5)")
          .call(brush)
          .call(function(g){
            g.select(".overlay")
             .datum({type:"selection"})
             .on("mousedown touchstart", beforebrushstarted);
          });

      slider.selectAll('.handle').remove();
      slider.selectAll('rect.selection')
      .attr("fill",null)
      .attr("fill-opacity",null)
      .attr("stroke",null)
      .attr("shape-rendering",null)
      .attr("rx",6)
      .attr("ry",6)
      .on("mouseover",function(){ bubble.style("visibility","visible"); })
      .on("mouseleave",function(){ bubble.style("visibility","hidden"); })

      bubble = slider.append("text")
        .attr("text-anchor","start")
        .style("visibility","hidden");

      brush.on("brush", brushed)
           .on("start",function(){
             slider.selectAll('rect.selection').on("mouseleave",null);
             bubble.style("visibility","visible");
           })
           .on("end",function(){
             bubble.style("visibility","hidden");
             slider.selectAll('rect.selection').on("mouseleave",function(){ bubble.style("visibility","hidden"); });
           })
    }

    function innerBrushed(brValue,value) {
      var renderedValue = scale2.invert(brValue);
      renderedValue = rounded ? Math.round(renderedValue) : formatter(renderedValue);
      bubble.text(renderedValue);
      if(rounded){
        value = Math.round(value);
        var tomove = scale2(renderedValue);
        slider.call(brush.move,[tomove-6,tomove+6]);
        bubble.attr("x",tomove+7);
      }else
        bubble.attr("x",brValue+7);
      return value;
    }

    function beforebrushstarted() {
      var dx = 12,
          cx = d3.mouse(this)[0],
          x0 = cx - dx / 2,
          x1 = cx + dx / 2;
      slider.call(brush.move, x1 > sliderWidth ? [sliderWidth - dx, sliderWidth] 
            : 0 < 0 ? [0, 0 + dx] 
            : [x0, x1]);
      displaySlider.update(scale.invert(cx));
    }

    function brushed() {
      if (!d3.event.sourceEvent || d3.event.sourceEvent.type!="mousemove") return;
      brushedValue = d3.mean(d3.event.selection);
      callback(innerBrushed(brushedValue,scale.invert(brushedValue)));
    }

    displaySlider.move = function(value) {
      if(slider){
        slider.call(brush.move,[scale(value)-6,scale(value)+6]);
        value = innerBrushed(scale(value),value);
      }
    }

    displaySlider.update = function(value) {
      displaySlider.move(value);
      callback(value);
    }

    displaySlider.y = function(x) {
      if (!arguments.length) return y;
      y = x;
      return displaySlider;
    };

    displaySlider.domain = function(x) {
      if (!arguments.length) return domain;
      domain = x;
      return displaySlider;
    };

    displaySlider.domain2 = function(x) {
      if (!arguments.length) return domain2;
      domain2 = x;
      return displaySlider;
    };

    displaySlider.text = function(x) {
      if (!arguments.length) return text;
      text = x;
      return displaySlider;
    };

    displaySlider.prop = function(x) {
      if (!arguments.length) return prop;
      prop = x;
      return displaySlider;
    };

    displaySlider.callback = function(x) {
      if (!arguments.length) return callback;
      callback = x;
      return displaySlider;
    };

    displaySlider.rounded = function(x) {
      if (!arguments.length) return rounded;
      rounded = x;
      return displaySlider;
    };

    displaySlider.brushedValue = function(x) {
      if (!arguments.length) return brushedValue;
      brushedValue = x;
      return displaySlider;
    };

    return displaySlider;
  }
}

function update_forces(){
      if(!options.stopped){
        simulation.force("charge")
          .strength(options.charge)
        if(!options.linkWeight)
          simulation.force("link")
            .distance(options.linkDistance)

        simulation.alpha(frameControls?0.1:1).restart();
      }else
        simulation.restart();
}

function handleFrames(value){
      clearInterval(frameControls.frameInterval);
      frameSlider.update(checkLoop(value));

      if(frameControls.play){
        frameControls.frameInterval = setInterval(function(){
          frameSlider.update(checkLoop(frameControls.frame+1));
        }, frameControls.time);
      }

      function checkLoop(value){
        if(value>=frameControls.frames.length)
          return 0; 
        return value;
      }
}

function frameStep(value){
        frameControls.frame = value;

        var selectFrame = d3.select(".divFrameCtrl .selectFrame");
        if(!selectFrame.empty())
          selectFrame.node().selectedIndex = frameControls.frame;
        loadFrameData(frameControls.frame);

        if(Array.isArray(options.main))
          body.select("div.main").html(options.main[frameControls.frame]);
        if(Array.isArray(options.note))
          body.select("div.note").html(options.note[frameControls.frame]);

        Graph.nodes.forEach(function(node){
          node._hideFrame = true;
        });

        Graph.links.forEach(function(link){
          link._hideFrame = link['_frame_']!=frameControls.frame;
          if(!link._hideFrame){
            delete link.source._hideFrame;
            delete link.target._hideFrame;
            delete link._hideFrame;
          }
        });

        drawNet();

        if(frameControls.hasOwnProperty("zoom")){
          options.zoom = frameControls.zoom[frameControls.frame];
          if(zoomSlider.brushedValue()===false){
            resetZoom();
            zoomSlider.update(options.zoomScale);
            zoomSlider.brushedValue(false);
          }
        }
        if(frameControls.hasOwnProperty("repulsion")){
          options.repulsion = frameControls.repulsion[frameControls.frame];
          if(repulsionSlider.brushedValue()===false){
            options.charge = chargeRange[1] * (options.repulsion/100);
            repulsionSlider.update(options.charge);
            repulsionSlider.brushedValue(false);
          }
        }
        if(frameControls.hasOwnProperty("distance")){
          options.distance = frameControls.distance[frameControls.frame];
          if(distanceSlider.brushedValue()===false){
            options.linkDistance = linkDistanceRange[1] * (options.distance/100);
            distanceSlider.update(options.linkDistance);
            distanceSlider.brushedValue(false);
          }
        }

        if(frameControls.frame==frameControls.frames.length-1 && !frameControls.loop)
          d3.select(".divFrameCtrl .pause").dispatch("click");
}

function drawNet(){
  d3.selectAll(".slider.charge, .slider.linkDistance")
    .style("opacity",heatmap||options.stopped?0:1);
  
  var svg = d3.select(".plot svg g.net");

  var ctx = d3.select(".plot canvas").node().getContext("2d");

  var gScale = d3.select(".plot svg g.scale");
  gScale.selectAll("*").remove();

  if(Graph.tree){
    var hideChildren = function(d){
      d.childNodes.forEach(function(d){
        d.noShow = true;
        d.selected = false;
        hideChildren(d);
      });
    }
    Graph.nodes.forEach(function(d){
      if(checkSelectable(d) && d.childNodes.length)
        hideChildren(d);
    });
  }

  var nodes = Graph.nodes.filter(function(node){
    node.degree = 0;
    return checkSelectable(node);
  });

  var links = Graph.links.filter(checkSelectableLink);

  for(var i=1; i<links.length; i++){
    for(var j = i-1; j>=0; j--){
      if((links[i].Source == links[j].Source && links[i].Target == links[j].Target)||(links[i].Source == links[j].Target && links[i].Target == links[j].Source)){
        if(!links[j].linkNum)
          links[j].linkNum = 1;
        links[i].linkNum = links[j].linkNum + 1;
        break;
      }
    }
  }

  links.forEach(function(link){
    link.source.degree = +link.source.degree+1;
    link.target.degree = +link.target.degree+1;
  })

  var imgidx = options.imageNames.indexOf(options.nodeShape);
  options.imageItem = imgidx!=-1 ? options.imageItems[imgidx] : false;

  // compute colors
  var colorNodesScale = setColorScale(options.nodeColor=="degree" ? nodes : Graph.nodes,'node',"nodeColor"),
      colorGroupsScale = setColorScale(options.nodeGroup=="degree" ? nodes : Graph.nodes,'node',"nodeGroup"),
      colorLinksScale = setColorScale(Graph.links,'link',"linkColor"),
  colorNodes = colorNodesScale?function(d){ return colorNodesScale(d[options.nodeColor]); }:defaultColor,
  colorGroups = colorGroupsScale?(function(d){ return colorGroupsScale(d[options.nodeGroup]); }):defaultColor,
  colorLinks = colorLinksScale?(function(d){ return colorLinksScale(d[options.linkColor]); }):defaultLinkColor;

  // compute link attributes
  if(heatmap){
    var getLinkIntensity = getNumAttr(Graph.links,'linkIntensity',[0.1,1],1);
  }else{
    var getLinkDistance = getNumAttr(Graph.links,'linkWeight',linkWeightRange,options.linkDistance),
        getLinkWidth = getNumAttr(links,'linkWidth',linkWidthRange,1);

    // compute node size
    var getNodeSize = getNumAttr(nodes,'nodeSize',nodeSizeRange,options.imageItem?3:1),
        getNodeLabelSize = getNumAttr(nodes,'nodeLabelSize',nodeLabelSizeRange,10*options.cex);
    nodes.forEach(function(node){
      node.nodeSize = getNodeSize(node) * 4.514;
      node.nodeLabelSize = getNodeLabelSize(node);
    });
  }

  // compute shapes
  var getShape = function() { return d3["symbol"+defaultShape]; };
  if(options.nodeShape){
    var symbolList = d3.scaleOrdinal()
         .range(symbolTypes)
         .domain(d3.map(Graph.nodes, function(d) { return d[options.nodeShape]; }).keys());

    getShape = function(d) { return d3["symbol"+symbolList(d[options.nodeShape])]; }
  }

  svg.attr("clip-path", heatmap && heatmapTriangle?"url(#heatmapClip)":null);
  if(heatmap){ // draw heatmap

    ctx.clearRect(0, 0, width, height);
    svg.selectAll("*").remove();
    svg = svg.append("g")
               .attr("class","heatmap")

    var n = nodes.length,
        x = d3.scaleBand().range([0, n*20]),
        matrix = [];

    var size = Math.min(width, height),
        side = x.range()[1],
        k = 14/30 + n/150,
        scale = size * (k < 0.8 ? k : 0.8) / side,
        scaledSide = side*scale;

    svg.attr("transform", heatmapTriangle ?
      "translate(" + -(width - (Math.sqrt(scaledSide*scaledSide*2))) / 4 + "," + height/2 + ")scale(" + scale + ")rotate(-45)" :
      "translate(" + (-scaledSide/2) + "," + (height/2 - scaledSide) + ")scale(" + scale + ")");

    nodes.forEach(function(node, i) {
      node.index = i;
      matrix[i] = d3.range(n).map(function(j) { return {x: j, y: i}; });
    });

    links.forEach(function(link) {
      var val = options.linkIntensity?link[options.linkIntensity]:1,
          valColor = options.linkColor?link[options.linkColor]:1,
          loadMatrix = function(i,j){
            matrix[i][j][options.linkIntensity] = val;
            matrix[i][j].color = valColor;
            if(options.linkText)
              matrix[i][j].txt = link[options.linkText];
          }
      if(options.linkBipolar)
        val = Math.abs(val);
      loadMatrix(link.source.index,link.target.index);
      if(!options.showArrows || heatmapTriangle)
        loadMatrix(link.target.index,link.source.index);
    });

  if(options.nodeOrderA || options.nodeOrderD){
    if(options.nodeOrderA)
      options.nodeOrder = options.nodeOrderA;
    else
      options.nodeOrder = options.nodeOrderD;
    x.domain(d3.range(n).sort(function(a, b) {
          a = nodes[a][options.nodeOrder];
          b = nodes[b][options.nodeOrder]
          if(options.nodeOrderD)
            b = [a, a = b][0];
          return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
        }));
  }else
    x.domain(d3.range(n));

  svg.append("rect")
      .style("fill","#eee")
      .attr("width", side)
      .attr("height", side);

  var row = svg.selectAll(".row")
      .data(matrix)
    .enter().append("g")
      .attr("class", "row")
      .attr("transform", function(d, i) { return "translate(0," + x(i) + ")"; })
      .each(rowFunc);

  row.append("line")
      .attr("x2", side)
    .style("stroke","#fff");

  appendText(row,true);
      
  var column = svg.selectAll(".column")
      .data(matrix)
    .enter().append("g")
      .attr("class", "column")
      .attr("transform", function(d, i) { return "translate(" + x(i) + ")rotate(-90)"; });

  column.append("line")
      .attr("x1", -side)
    .style("stroke","#fff");

  appendText(column,false);

  if(options.nodeOrder){
    var clusters = nodes.map(function(d){ return d[options.nodeOrder]; }).sort(function(a, b) { 
          if(options.nodeOrderD)
            b = [a, a = b][0];
          return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
        }),
        step = NaN,
        lines = {};
    for(var i=0; i<clusters.length; i++){
      if(step!=String(clusters[i])){
        step = String(clusters[i]);
        lines[step] = [i,i];
      }else
        lines[step][1] = i;
    }
    svg.selectAll(".cluster")
        .data(d3.values(lines))
      .enter().append("path")
        .attr("class","cluster")
        .attr("d",function(d){
            var x1 = d[0]*x.bandwidth() + x.bandwidth()/2,
                x2 = d[1]*x.bandwidth() + x.bandwidth()/2;
            return "M"+x1+",-6L"+x1+",-12L"+x2+",-12L"+x2+",-6";
        })

    delete options.nodeOrder;
  }

  function appendText(sel,row){
    if(options.nodeLabel){
    sel.append("text")
      .attr("class","label")
      .attr("x", row? (heatmapTriangle? side + 6 : -6) : ((heatmapTriangle?-1:1) * (options.nodeOrder?18:6)))
      .attr("y", (!row && heatmapTriangle? -1 : 1) * (x.bandwidth() / 2))
      .attr("text-anchor", row ^ heatmapTriangle? "end" : "start")
      .attr("transform", !row && heatmapTriangle? "rotate(180)" : null)
      .attr("dy", ".32em")
      .style("font-size",(x.bandwidth()-2)+"px")
      .style("fill",colorNodesScale? function(d, i) {
        var col = d3.rgb(colorNodesScale(nodes[i][options.nodeColor]));
        if(col.r==255 && col.g==255 && col.b==255)
          col = col.darker(1);
        return col;
      } : null)
      .style("opacity",0)
      .text(function(d, i) { return (!row ? nodes[i][options.nodeLabel] : nodes[i][options.nodeName]); })
      .on("click",function(d,i){
        var name = nodes[i][options.nodeName];
        nodes.forEach(function(p){
            if(d3.event.ctrlKey || d3.event.metaKey)
              p.selected = p.selected ^ name == p[options.nodeName];
            else
              p.selected = name == p[options.nodeName];
        });
        showTables();
      })
      .on("dblclick", row || !options.linkIntensity ? function(){
        d3.event.stopPropagation();
        switchEgoNet();
      } : function(d,i){
        d3.event.stopPropagation();
        var order = d3.transpose(matrix)[i].sort(function(a,b){
              var aIntensity = a[options.linkIntensity]?a[options.linkIntensity]:0,
                  bIntensity = b[options.linkIntensity]?b[options.linkIntensity]:0;
              if(bIntensity==aIntensity){
                if(a.y==i) return -1;
                if(b.y==i) return 1;
                return a.y-b.y;
              }else
                return bIntensity-aIntensity;
            }).map(function(p){ return p.y; })
        x.domain(order);

        var t = svg.transition().duration(2500);

        svg.selectAll("path.cluster").remove();

        t.selectAll(".column .label").attr("x",((heatmapTriangle?-1:1) * 6))

        t.selectAll(".row")
          .delay(function(d, i) { return x(i) * 4; })
          .attr("transform", function(d, i) { return "translate(0," + x(i) + ")"; })
         .selectAll(".cell")
          .delay(function(d) { return x(d.x) * 4; })
          .attr("x", function(d) { return x(d.x); });

        t.selectAll(".row .cellText")
          .delay(function(d) { return x(d.x) * 4; })
          .attr("x", function(d) { return x(d.x) + x.bandwidth()/2; });

        t.selectAll(".column")
          .delay(function(d, i) { return x(i) * 4; })
          .attr("transform", function(d, i) { return "translate(" + x(i) + ")rotate(-90)"; });
      })
      .on("mouseover", function(){
        d3.select(this).style("font-weight","bold");
      })
      .on("mouseout", mouseout)
      .transition()
      .duration(500)
      .style("opacity",1)
    }
  }

  function rowFunc(row) {
    d3.select(this).selectAll(".cell")
        .data(row.filter(function(d) { return d[options.linkIntensity]; }))
      .enter().append("rect")
        .attr("class", "cell")
        .attr("x", function(d) { return x(d.x); })
        .attr("width", x.bandwidth())
        .attr("height", x.bandwidth())
        .style("fill-opacity", getLinkIntensity)
        .style("fill", colorLinksScale?function(d) { return colorLinksScale(d.color); }:defaultColor)
        .on("mouseover", mouseover)
        .on("mouseout", mouseout)
        .on("click",click)
        .on("dblclick",dblclick)
    if(options.linkText){
      d3.select(this).selectAll(".cellText")
        .data(row.filter(function(d) { return d[options.linkIntensity]; }))
      .enter().append("text")
        .attr("class", "cellText")
        .attr("x", function(d) { return x(d.x) + x.bandwidth()/2; })
        .attr("y", x.bandwidth()/2)
        .attr("dy", ".32em")
        .attr("text-anchor", "middle")
        .style("font-size", x.bandwidth()*2/5 + "px")
        .on("mouseover", mouseover)
        .text(function(d){
            if(!isNaN(+d.txt))
              return d.txt.toFixed(1);
            return d.txt;
        })
    }
  }

  function mouseover(p) {
    d3.selectAll(".row .label").style("font-weight", function(d, i) {
      if(i == p.y) return "bold"; else return null;
    });
    d3.selectAll(".column .label").style("font-weight", function(d, i) {
      if(i == p.x) return "bold"; else return null;
    });
  }

  function mouseout() {
    d3.selectAll(".label").style("font-weight",null);
  }
  
  function click(p){
    links.forEach(function(l){ checkNeighbors(l,p); });
    nodes.forEach(function(n){
      n.selected = !n.noShow && n.__neighbor;
      delete n.__neighbor;
    });
    showTables();
  }

  function dblclick(p){
    d3.event.stopPropagation();
    links.forEach(function(l){ checkNeighbors(l,p); });
    nodes.forEach(function(n){
      n.selected = (checkSelectable(n) && (n.index == p.x || n.index == p.y))? true : false;
      delete n.__neighbor;
    });
    switchEgoNet();
    drawNet();
  }

    function checkNeighbors(l,p){
      if(!l.noShow && !l._hideFrame && (((!options.showArrows || heatmapTriangle) && (l.target.index==p.x || l.target.index==p.y)) || (l.source.index==p.x || l.source.index==p.y))){
        l.source.__neighbor = l.target.__neighbor = true;
      }
    }

  }else{ // draw network

    svg.select("g.heatmap").remove();

    //hide sliders
    d3.select(".slider.charge").style("display",nodes.length<2?"none":null)
    d3.select(".slider.linkDistance").style("display",!links.length || options.linkWeight?"none":null)

    simulation.nodes(nodes)

    simulation.force("link")
      .links(links)
      .distance(getLinkDistance);

    simulation.on("tick", tick);

    update_forces();

    //axes
    var axes = svg.selectAll(".axis")
      .data([[0,1],[1,0]])
    .enter().append("line")
      .attr("class","axis")
      .attr("pointer-events","none")
      .style("opacity",0)

    var axesLabelsAnchors = ["start","middle","end","middle"],
      axesLabels = svg.selectAll(".axisLabel")
        .data(options.axesLabels)
    .enter().append("text")
      .attr("class","axisLabel")
      .attr("pointer-events","none")
      .style("opacity",0)
      .text(String)
      .attr("text-anchor",function(d,i){
        return axesLabelsAnchors[i];
      })

    //groups
    var groups = getGroups(nodes);
  }

  // display legends
  var dat;
  if(options.nodeLegend){
    dat = nodes.map(function(d){ return d[options.nodeLegend]; });
    if(dataType(nodes,options.nodeLegend) == 'object')
      dat = dat.reduce(function(a,b) { return a.concat(b); }, []);
    dat = d3.set(dat).values();
    displayLegend(gScale,options.nodeLegend,"#000000",defaultShape,dat.sort(sortAsc));
  }

  if(!options.imageItem){
    if(options.nodeColor && !options.colorScalenodeColor){
      dat = d3.map(nodes.filter(function(d){ return d[options.nodeColor]!==null; }), function(d){ return d[options.nodeColor]; }).keys();
      displayLegend(gScale,options.nodeColor,colorNodesScale,defaultShape,dat.sort(sortAsc));
    }

    if(options.nodeShape){
      dat = d3.map(nodes, function(d){ return d[options.nodeShape]; }).keys();
      displayLegend(gScale,options.nodeShape,"#000000",symbolList,dat.sort(sortAsc));
    }
  }

  if(!heatmap && options.imageItem){
    if(options.imageItems && options.imageNames){
      dat = nodes.map(function(d){ return [d[options.imageNames[options.imageItems.indexOf(options.imageItem)]],d[options.imageItem]]; })
      dat.sort(function(a,b){
          return sortAsc(a[0],b[0]);
      })
      var dat2 = d3.map(dat, function(d){ return d[0]; }).keys()
      dat = d3.map(dat, function(d){ return d[1]; }).keys()
      if(dat2.length!=dat.length)
          dat2 = false;
      displayLegend(gScale,options.imageItem,'image',dat2,dat);
    }
  }

  showTables();

  //render network
  function tick() {
    ctx.save();
    ctx.clearRect(0, 0, width, height);

    if(frameControls.recorder){
      ctx.fillStyle = options.background ? options.background : "#ffffff";
      ctx.fillRect(0, 0, width, height);
      var text = frameControls.frames[frameControls.frame];
      if(options.main && Array.isArray(options.main))
        text = options.main[frameControls.frame];
      ctx.font = 10*options.cex+"px sans-serif";
      ctx.textAlign = "right";
      ctx.fillStyle = "#333";
      ctx.fillText(text,width-10,height-10);
    }

    ctx.translate(transform.x, transform.y);
    ctx.scale(transform.k, transform.k);

    // draw areas
    ctx.lineJoin = "round";
    if(options.nodeGroup){
      ctx.globalAlpha = 0.2;
      ctx.lineWidth = 3;
      groups.forEach(function(group){
        ctx.strokeStyle = colorGroups(group);
        ctx.fillStyle = d3.rgb(ctx.strokeStyle).brighter(0.6);
        var points = getArea(group,nodes);
        ctx.beginPath();
        ctx.rect(points[0],points[1],points[2],points[3]);
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      })
    }

    // draw links
    ctx.globalAlpha = 0.6;
    links.forEach(function(link) {
      var points = getLinkCoords(link);
      if(!points)
        return;
      ctx.beginPath();
      ctx.lineWidth = getLinkWidth(link);
      ctx.strokeStyle = link._selected? "#F00" : (colorLinksScale ? colorLinks(link) : defaultLinkColor);
      ctx.moveTo(points[0][0], points[0][1]);
      if(link.linkNum)
        ctx.quadraticCurveTo(points[2][0], points[2][1], points[1][0], points[1][1]);
      else
        ctx.lineTo(points[1][0], points[1][1]);
      if(options.showArrows){
        var arrow;
        if(link.linkNum)
          arrow = getArrow(points[2][0], points[2][1], points[1][0], points[1][1], ctx.lineWidth);
        else
          arrow = getArrow(points[0][0], points[0][1], points[1][0], points[1][1], ctx.lineWidth);
        ctx.lineTo(arrow[2][0], arrow[2][1]);
        ctx.lineTo(arrow[0][0], arrow[0][1]);
        ctx.lineTo(points[1][0], points[1][1]);
      }
      ctx.stroke();
    });

    ctx.lineJoin = "miter";
    ctx.globalAlpha = 1;

    if(options.linkText){
      ctx.fillStyle = "#999";
      ctx.beginPath();
      ctx.font = 10*options.cex+"px sans-serif";
      links.forEach(function(link) {
        var coords = getLinkTextCoords(link);

        ctx.textAlign = "left";
        if(link.linkNum && link.linkNum%2==0)
          ctx.textAlign = "right";

        ctx.fillText(formatter(link[options.linkText]), coords[0], coords[1]);
      });
      ctx.fill();
    }

    // draw nodes
    nodes.forEach(function(node) {
      ctx.lineWidth = node.selected || node._selected ? 2 : 1;
      var strokeStyle = node._selected ? "#F00" : (node.selected ? "#FF0" : false);
      ctx.strokeStyle = strokeStyle;
      ctx.translate(node.x, node.y);
      if(options.imageItem){
        var img = images[node[options.imageItem]],
            imgHeight = img.height*2/img.width;
        try{ ctx.drawImage(img, -node.nodeSize, -(imgHeight/2)*node.nodeSize, node.nodeSize * 2, node.nodeSize * imgHeight); }catch(e){}
        if(strokeStyle){
          ctx.beginPath();
          ctx.arc(0, 0, node.nodeSize, 0, 2 * Math.PI);
          ctx.closePath();
          ctx.stroke();
        }
      }else{
        ctx.fillStyle = colorNodesScale?colorNodes(node):defaultColor;
        if(!strokeStyle)
          ctx.strokeStyle = d3.rgb(ctx.fillStyle).darker(1);
        ctx.beginPath();
        d3.symbol().type(getShape(node)).size(node.nodeSize * node.nodeSize * Math.PI).context(ctx)();
        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      }
      ctx.translate(-node.x, -node.y);
    });

    // write labels
    if(options.nodeLabel){
      ctx.textAlign = "left";
      ctx.fillStyle = "#444";
      ctx.beginPath();
      ctx.font = 10*options.cex+"px sans-serif";
      nodes.forEach(function(node) {
        if(options.nodeLabelSize){
          if(node[options.nodeLabelSize]<0)
            return;
          ctx.font = node.nodeLabelSize+"px sans-serif";
        }
        ctx.fillText(node[options.nodeLabel], node.x + node.nodeSize + 4, node.y + 4);
      });
      ctx.fill();
    }
    ctx.restore();
  }

  // generate pdf
  svg2pdf = function(){

    var doc = new jsPDF({
      orientation: (width>height)?"l":"p",
      unit: 'pt',
      format: [width,height]
    });

    doc.polygon = pdfPolygon;

    var translate = [transform.x,transform.y],
        scale = transform.k;

    doc.setLineWidth(scale);

    if(heatmap){ // heatmap display
      doc.setDrawColor(255);
      doc.setFillColor(238,238,238);
      var size = parseInt(d3.select("g.heatmap>rect").attr("width")) * scale,
          dim = parseInt(d3.select("g.heatmap rect.cell").attr("width")) * scale;
      for(i=0;i<size/dim;i++){
        for(j=0;j<size/dim;j++){
          doc.rect(((i*dim)+translate[0]),((j*dim)+translate[1]), dim, dim, 'FD');
        }
      }

      d3.selectAll("g.heatmap rect.cell").each(function(){
        var self = d3.select(this),
            x = (+self.attr("x")*scale) + translate[0],
            y = (getTranslation(d3.select(this.parentNode).attr("transform"))[1]*scale) + translate[1],
            o = self.style("fill-opacity"),
            color = d3.rgb(self.style("fill"));
        color = applyOpacity(color,o,{r:238,g:238,b:238});
        doc.setFillColor(color.r,color.g,color.b);
        doc.rect(x, y, dim, dim, 'FD');
      });

      doc.setTextColor(64);
      d3.selectAll("g.heatmap .row text").each(function(){
        var self = d3.select(this),
            y = (getTranslation(d3.select(this.parentNode).attr("transform"))[1]*scale) + translate[1],
            txt = self.text(),
            x = translate[0],
            fontSize = parseInt(self.style("font-size"))*scale;
        doc.setFontSize(fontSize);
        doc.text(x-6, y+fontSize, txt, { align: "right" });
      });
      d3.selectAll("g.heatmap .column text").each(function(){
        var self = d3.select(this),
            x = (getTranslation(d3.select(this.parentNode).attr("transform"))[0]*scale) + translate[0],
            txt = self.text(),
            y = translate[1],
            fontSize = parseInt(self.style("font-size"))*scale;
        doc.setFontSize(fontSize);
        doc.text(x+fontSize, y-6, txt, null, 90);
      });

    }else{ // network display
      var areas = [];
      groups.forEach(function(group){
        var d = {},
            color = colorGroups(group),
            points = getArea(group,nodes);
        d.colorf = applyOpacity(d3.rgb(color).brighter(0.6),0.2,{r:255,g:255,b:255});
        d.colord = applyOpacity(d3.rgb(color),0.2,{r:255,g:255,b:255});
        d.x = (points[0]*scale)+translate[0];
        d.y = (points[1]*scale)+translate[1];
        d.width = points[2]*scale;
        d.height = points[3]*scale;
        areas.push(d);
      });
      areas.sort(function(a,b){
        var areaA = a.width * a.height,
            areaB = b.width * b.height;
        if (areaA < areaB) {
          return 1;
        }
        if (areaA > areaB) {
          return -1;
        }
        return 0;
      });
      areas.forEach(function(d){
        doc.setFillColor(d.colorf.r,d.colorf.g,d.colorf.b);
        doc.setDrawColor(d.colord.r,d.colord.g,d.colord.b);
        doc.roundedRect(d.x,d.y,d.width,d.height,10,10,"FD");
      });

      links.forEach(function(link){
        var color = applyOpacity(d3.rgb(colorLinksScale ? colorLinks(link) : defaultLinkColor),0.6,{r:255,g:255,b:255}),
            w = getLinkWidth(link)*scale,
            points = getLinkCoords(link);

        if(!points)
          return;

        var x1 = (points[0][0]*scale)+translate[0],
            y1 = (points[0][1]*scale)+translate[1],
            x2 = (points[1][0]*scale)+translate[0],
            y2 = (points[1][1]*scale)+translate[1];

        doc.setDrawColor(color.r,color.g,color.b);
        doc.setLineWidth(w);
        if(link.linkNum){
          var cpx = (points[2][0]*scale)+translate[0]-x1,
              cpy = (points[2][1]*scale)+translate[1]-y1;
          doc.lines([[cpx,cpy,cpx,cpy,x2-x1,y2-y1]],x1,y1);
        }else
          doc.line(x1, y1, x2, y2);

        if(options.showArrows){
          var arrow;
          if(link.linkNum)
            arrow = getArrow(cpx+x1, cpy+y1, x2, y2, w);
          else
            arrow = getArrow(x1, y1, x2, y2, w);
          doc.line(x2, y2, arrow[2][0], arrow[2][1]);
          doc.line(arrow[2][0], arrow[2][1], arrow[0][0], arrow[0][1]);
          doc.line(arrow[0][0], arrow[0][1], x2, y2);
        }
      });

      if(options.linkText){
        doc.setFontSize(10*options.cex*scale);
        doc.setTextColor("#999");
        links.forEach(function(link){
          var coords = getLinkTextCoords(link),
              x = (coords[0]*scale)+translate[0],
              y = (coords[1]*scale)+translate[1],
              t = String(formatter(link[options.linkText])),
              tAlign = "left";
          if(link.linkNum && link.linkNum%2==0)
            tAlign = "right";
          doc.text(x, y, t, { align: tAlign });
        });
      }

      nodes.forEach(function(node){
        var color = d3.rgb(colorNodesScale?colorNodes(node):defaultColor),
            sColor = d3.rgb(node.selected ? "#FF0" : d3.rgb(color).darker(1)),
            size = node.nodeSize*scale,
            x = (node.x*scale)+translate[0],
            y = (node.y*scale)+translate[1];
        doc.setLineWidth((node.selected ? 2 : 1)*scale);
        doc.setDrawColor(sColor.r,sColor.g,sColor.b);
        doc.setFillColor(color.r,color.g,color.b);
        if(options.imageItem){
          var imgSrc = node[options.imageItem];
          if(images64[imgSrc]){
            var imgHeight = images[imgSrc].height*2/images[imgSrc].width;
            doc.addImage(images64[imgSrc], 'PNG', x-size, y-(imgHeight/2)*size, 2*size, imgHeight*size);
            if(node.selected){
              doc.circle(x, y, size);
            }
          }
        }else{
          var points = d3.symbol().type(getShape(node))();
          doc.polygon(points, x, y, [size/4.514,size/4.514], 'FD');
        }
      });

      if(options.nodeLabel){
        doc.setFontSize(10*options.cex*scale);
        doc.setTextColor("#444");
        nodes.forEach(function(node){
          var x = ((node.x + node.nodeSize + 8)*scale)+translate[0],
              y = ((node.y + 4)*scale)+translate[1],
              txt = String(node[options.nodeLabel]);
          doc.text(x, y, txt);
        });
      }
    }

    doc.setTextColor("#333");

    d3.selectAll("div.main").each(function(){
      doc.setFontSize(parseInt(d3.select(this).style("font-size")));
      doc.setFontType("bold");
      doc.text(12, 28, this.textContent);
    })

    doc.setFontType("normal");

    d3.selectAll("div.note").each(function(){
      doc.setFontSize(parseInt(d3.select(this).style("font-size")));
      doc.text(12, height-12, this.textContent);
    })

    if(d3.select(".scale").style("opacity")!=0){
      // scale
      if(!d3.select(".scale>rect").empty()) {
          var colors = colorScales[d3.select(".scale rect").attr("fill").replace(/(url\()|(\))/g, "").replace("#","")];
          var canvas = document.createElement("canvas");
          canvas.width = 300;
          canvas.height = 10;
          var ctx = canvas.getContext("2d");
          var grd = ctx.createLinearGradient(0,0,300,0);
          grd.addColorStop(0,colors[0]);
          grd.addColorStop(0.5,colors[1]);
          grd.addColorStop(1,colors[2]);
          ctx.fillStyle = grd;
          ctx.fillRect(0,0,300,10);
          var uri = canvas.toDataURL();
          doc.addImage(uri, 'PNG', (width-320), 40, 300, 10);
          d3.selectAll(".scale>text").each(function(){
            var self = d3.select(this),
                x = +(self.attr("x"))+(width-320),
                y = +(self.attr("y"))+20,
                t = self.text();

            doc.setFontSize(parseInt(self.style("font-size")));
            doc.text(x, y, t);
          });
      }

      // legend
      if(!d3.select(".scale .legend").empty()) {
        d3.selectAll(".scale .legend").each(function(){
          var sel = d3.select(this),
              y = getTranslation(sel.attr("transform"))[1]+10,
              fontSize = parseInt(sel.select(".title").style("font-size")),
              t = sel.select(".title").text(),
              txtWidth = doc.getStringUnitWidth(t) * fontSize;
          doc.setFontSize(fontSize);
          doc.text(width-txtWidth-60, y+10, t)
          fontSize = parseInt(sel.selectAll("g>text").style("font-size"));
          doc.setFontSize(fontSize);
          sel.selectAll("g").each(function(d,i){
            var el = d3.select(this),
                gy = getTranslation(el.attr("transform"))[1],
                x = width-60,
                t = el.select("text").text(),
                txtWidth = doc.getStringUnitWidth(t) * fontSize;
            if(el.select("image").empty()){
              var color = d3.rgb(el.select("path").style("fill")),
                  d = el.select("path").attr("d");
              doc.setFillColor(color.r,color.g,color.b);
              doc.polygon(d,x,y+gy,[1,1],"F");
            }else{
              var imgSrc = el.select("image").attr("href");
              if(images64[imgSrc]){
                var imgHeight = images[imgSrc].height*10/images[imgSrc].width;
                doc.addImage(images64[imgSrc], 'PNG', x, y+gy-4, 10, imgHeight);
              }
            }
            doc.text(x-txtWidth-10, y+gy+4, t);
          });
        })
      }
    }

    // axes
    doc.setLineWidth(scale);
    doc.setDrawColor(170, 170, 170);
    doc.setTextColor(170, 170, 170);
    d3.selectAll(".net .axis").each(function(){
      var self = d3.select(this);
      if(self.style("opacity")=="1"){
        var x1 = (+self.attr("x1")*scale)+translate[0],
            y1 = (+self.attr("y1")*scale)+translate[1],
            x2 = (+self.attr("x2")*scale)+translate[0],
            y2 = (+self.attr("y2")*scale)+translate[1];

        doc.line(x1, y1, x2, y2);
      }
    })
    d3.selectAll(".net .axisLabel").each(function(){
      var self = d3.select(this);
      if(self.style("opacity")=="1"){
        var x = (+self.attr("x")*scale)+translate[0],
            y = (+self.attr("y")*scale)+translate[1],
            anchors = {"start":"left","middle":"center","end":"right"},
            tAlign = anchors[self.attr("text-anchor")];

        doc.text(x, y, self.text(), { align: tAlign });
      }
    })

    doc.save(d3.select("head>title").text()+".pdf");
  } // end pdf function
}

function findNode(){
  return simulation.find(transform.invertX(d3.mouse(d3.event.target)[0]), transform.invertY(d3.mouse(d3.event.target)[1]), findNodeRadius);
}

function clickNet(){
  var node = findNode(),
      tip = body.select("div.tooltip");
  if(node){
    if(d3.event.ctrlKey || d3.event.metaKey){
      node.selected = !node.selected;
    }else{
      Graph.nodes.forEach(function(n){
        n.selected = false;
      });
      node.selected = true;
    }
    if(!tip.empty()){
      tooltipFixed = true;
      showTooltip(tip,node);
    }
  }else{
    Graph.nodes.forEach(function(n){
      n.selected = false;
    });
    if(!tip.empty()){
      tooltipFixed = false;
      tip.style("display","none");
    }
  }
  simulation.restart();
  showTables();
  displayInfoPanel(node,options.nodeInfo);
}

function dblClickNet(){
  var node = findNode();
  if(node){
    if(Graph.tree && (d3.event.ctrlKey || d3.event.metaKey)){
      node.selected = true;
      treeAction();
    }else
      switchEgoNet();
  }else{
    deleteNoShow();
    applyDegreeFilter();
  }
}

function showTooltip(tip,node){
    if(!tip.empty() && (tip.style("display")=="none" || tooltipFixed)){
        if(node[options.nodeText]){
          tip.style("display","block").html(node[options.nodeText])
             .style("top",(d3.event.y+20)+"px")
             .style("left",(d3.event.x+20)+"px")
        }else
          tip.style("display","none")
    }
}

function hoverNet(){
  var node = findNode(),
      tip = body.select("div.tooltip");
  if(node){
    d3.select(this).style("cursor","pointer");
    if(!tip.empty() && !tooltipFixed)
      showTooltip(tip,node);
  }else{
    if(!tip.empty() && !tooltipFixed)
      tip.style("display","none");
    d3.select(this).style("cursor","grab");
  }
}

function dragsubject() {
  var node = simulation.find(transform.invertX(d3.event.x), transform.invertY(d3.event.y), findNodeRadius);
  if(node){
    node.x = transform.applyX(node.x);
    node.y = transform.applyY(node.y);
  }
  return node;
}

function dragstarted(d) {
  if (!d3.event.active) simulation.alphaTarget(0.3).restart();
  var node = d3.event.subject;
  if(typeof node.fx == 'number' || typeof node.fy == 'number')
    node.fixed = true;
  node.fx = transform.invertX(node.x);
  node.fy = transform.invertY(node.y);
}

function dragged(d) {
  d3.event.subject.fx = transform.invertX(d3.event.x);
  d3.event.subject.fy = transform.invertY(d3.event.y);
}

function dragended(d) {
  if (!d3.event.active) simulation.alphaTarget(0);
  var node = d3.event.subject;
  if (!options.stopped && !node.fixed) {
    node.fx = null;
    node.fy = null;
  }
  if(node.fixed)
    delete node.fixed;

  d3.select(this).style("cursor","grab");
}

function getGroups(nodes){
    var groups = [];
    if(options.nodeGroup){
      groups = d3.map(nodes.filter(function(node){ return node[options.nodeGroup] !== null; }), function(node){ return node[options.nodeGroup]; }).keys()
      .map(function(g){
        var group = {};
        group[options.nodeGroup] = g;
        group.xExt = [0,0];
        group.yExt = [0,0];
        return group;
      });
    }
    return groups;
}

function getArea(group,nodes){
    var points = nodes.filter(function(node){ return node[options.nodeGroup]==group[options.nodeGroup]; }).map(function(node){ return [node.x,node.y]; }),
        xExt = d3.extent(points,function(point){ return point[0];}),
        yExt = d3.extent(points,function(point){ return point[1];});
    return [xExt[0]-3,yExt[0]-3,xExt[1]-xExt[0]+6,yExt[1]-yExt[0]+6];
}

function getLinkCoords(link){
      var sx = link.source.x,
          sy = link.source.y,
          tx = link.target.x,
          ty = link.target.y,
          offSetX,
          offSetY;

      if(sx==tx && sy==ty)
        return 0;

      if(options.showArrows || link.linkNum){
        var dx = tx - sx,
            dy = ty - sy;

        var dr = Math.sqrt((dx * dx) + (dy * dy));

        if(options.showArrows){
          var radius = (link.target.nodeSize),
              offsetX = (dx * radius) / dr,
              offsetY = (dy * radius) / dr;

          tx = tx - offsetX;
          ty = ty - offsetY;
        }

        if(link.linkNum){
          var offset = (Math.ceil(link.linkNum/2))*10;

          var midpoint_x = (sx + tx) / 2,
              midpoint_y = (sy + ty) / 2;

          var offSetX = offset*(dy/dr),
              offSetY = offset*(dx/dr);

          if(link.linkNum%2 ^ (link.Target<link.Source)){
            offSetX = midpoint_x + offSetX;
            offSetY = midpoint_y - offSetY;
          }else{
            offSetX = midpoint_x - offSetX;
            offSetY = midpoint_y + offSetY;
          }
        }
      }

      return [[sx,sy],[tx,ty],[offSetX,offSetY]];
}

function getArrow(x1,y1,x2,y2,w){
    var w = w ? Math.log(w+1)/Math.log(10) : 1;
    var dx = x2-x1,
        dy = y2-y1,
        dr = Math.sqrt(dx*dx+dy*dy),
        tx = dx/dr*8,
        ty = dy/dr*8,
        x3 = x2-(ty/2*w)-tx,
        y3 = y2+(tx/2*w)-ty,
        x4 = x2+(ty/2*w)-tx,
        y4 = y2-(tx/2*w)-ty;
    return [[x4,y4],[x2,y2],[x3,y3]];
}

function getLinkTextCoords(link){
        var x = ((link.target.x)+(link.source.x))/2,
            y = ((link.target.y)+(link.source.y))/2;

        if(link.linkNum){
          x = x + (link.linkNum%2==0? -1 : 1) * (Math.ceil(link.linkNum/2)*6);
          y = y + (Math.floor(link.linkNum/2)%2==0? -1 : 1) * ((6*Math.ceil(link.linkNum/2))-3);
        }

        return [x,y];
}

function forceEnd(){
  //update axes
    var nodes = Graph.nodes.filter(checkSelectable);

    if(nodes.length>1){
      var extX = d3.extent(nodes, function(d){ return d.x; }),
          extY = d3.extent(nodes, function(d){ return d.y; });
      var size = Math.max((extX[1]-extX[0]),(extY[1]-extY[0]));
      size = size + axisExtension;
    }else{
      var size = Math.min(width,height)-axisExtension;
    }



    d3.selectAll(".net .axis")
        .attr("x1",function(d){ return -(d[0]*size/2); })
        .attr("y1",function(d){ return -(d[1]*size/2); })
        .attr("x2",function(d){ return (d[0]*size/2); })
        .attr("y2",function(d){ return (d[1]*size/2); })
        .style("opacity",+options.showAxes)

    d3.selectAll(".net .axisLabel")
      .style("opacity",+options.showAxes)
      .attr("x",function(d,i){
        switch(i){
          case 1:
            return 0;
          case 2:
            return (-size-4)/2;
          case 3:
            return 0;
          default:
            return size/2;
        }
      })
      .attr("y",function(d,i){
        switch(i){
          case 1:
            return (-size-4)/2;
          case 2:
            return 4*options.cex;
          case 3:
            return size/2 + 8*options.cex;
          default:
            return 4*options.cex;
        }
      })
}

function setColorScale(data,item,itemAttr){
    if(options[itemAttr]){
      var scale;
      if(options["colorScale"+itemAttr] && dataType(data,options[itemAttr]) == "number"){
        var colorDomain = d3.extent(data.filter(function(d){ return d !== null; }), function(d) { return d[options[itemAttr]]; }),
            nameScale = options["colorScale"+itemAttr];
        if(options[item+"Bipolar"]){
          var absmax = Math.max(Math.abs(colorDomain[0]),Math.abs(colorDomain[1]));
          colorDomain = [-absmax,+absmax];
        }
        if(!options.imageItem && ((itemAttr=="nodeColor" && !heatmap) || (itemAttr=="linkColor" && heatmap)))
          displayScale(colorDomain, "url(#"+nameScale+")", options[itemAttr]);
        scale = d3.scaleLinear().range(colorScales[nameScale])
          .domain([colorDomain[0],d3.mean(colorDomain),colorDomain[1]]);
      }else{
        options["colorScale"+itemAttr] = null;
        scale = d3.scaleOrdinal().range(categoryColors)
          .domain(d3.map(data.filter(function(d){ return d[options[itemAttr]] !== null; }), function(d){ return d[options[itemAttr]]; }).keys());
      }
      return function(d){
               return (d === null)? (item == "node"? "#ffffff" : "#000000") : scale(d);
             };
    }else{
      return false;
    }
  }

function getNumAttr(data,itemAttr,range,def){
    if(options[itemAttr]){
      if(data.length){
        if(dataType(data,options[itemAttr]) == "number"){
          var item = itemAttr.slice(0,4),
              attrDomain;
          if(options[item+"Bipolar"])
            attrDomain = [0,d3.max(data, function(d) { return Math.abs(d[options[itemAttr]]); })];
          else
            attrDomain = d3.extent(data, function(d) { return +d[options[itemAttr]]; });
          if(attrDomain[0]!=attrDomain[1]){
            var scaleAttr = d3.scaleLinear()
              .range(range)
              .domain(attrDomain);

            return function(d){
              if(d[options[itemAttr]] === null && (itemAttr == 'linkWidth' || itemAttr == 'linkIntensity'))
                return 0;
              if(options[item+"Bipolar"]){
                return scaleAttr(Math.abs(d[options[itemAttr]]));
              }else{
                if(d[options[itemAttr]] === null && attrDomain[0]<0)
                  return range[0];
                return scaleAttr(+d[options[itemAttr]]);
              }
            }
          }
        }
      }
    }
    return function(){ return def; }
}

function addGradient(defs,id, stops){
  var offset = 100/(stops.length-1);
  var gradient = defs.append("linearGradient")
    .attr("id",id)
    .attr("x1","0%")
    .attr("y1","0%")
    .attr("x2","100%")
    .attr("y2","0%");

  stops.forEach(function(d, i){
    gradient
    .append("stop")
    .attr("offset",(offset*i)+"%")
    .style("stop-color",d);
  });
}

function displayInfoPanel(d,i){
  if(i && d[i]){
    d3.select("div.infopanel").remove();
    var div = body.append("div")
          .attr("class","infopanel"),
        infoHeight = d3.select("div.tables").node().getBoundingClientRect().top
      - parseInt(div.style("top"))
      - parseInt(div.style("border-top-width"))
      - parseInt(div.style("border-bottom-width"))
      - parseInt(div.style("padding-top"))
      - parseInt(div.style("padding-bottom"));
    div.style("left",infoPanelLeft+"px");
    div.style("height",infoHeight+"px");
    div.append("div")
      .attr("class","drag")
      .call(d3.drag()
        .on("drag", function() {
          infoPanelLeft = d3.mouse(body.node())[0]-parseInt(div.style("border-left-width"));
          div.style("left",infoPanelLeft+"px");
        })
      )
    div.append("div")
          .attr("class","close-button")
          .html("&#x2716;")
          .on("click", function(){ div.remove() });
    div.append("div").html(d[i]);
  }
}

function selectAllNodes(){
  if(Graph.nodes.filter(function(d){ return !d.selected && checkSelectable(d); }).length)
    Graph.nodes.forEach(function(d){
      if(checkSelectable(d))
        d.selected = true;
    });
  else
    Graph.nodes.forEach(function(d){
      delete d.selected;
    });
  if(!heatmap)
    simulation.restart();
  showTables();
}

function filterSelection(){
  if(!Graph.nodes.filter(function(d){ return d.selected; }).length){
    displayWindow(texts.alertnonodes);
  }else{
    Graph.nodes.forEach(function(d){
      if(!d.selected)
        d.noShow = true;
    });
    drawNet();
  }
}

function deleteNoShow(){
  egoNet = false;
  Graph.nodes.forEach(function(d){
    delete d.noShow;
    delete d._neighbor;
  });
  drawNet();
}

function addNeighbors(){
  if(!Graph.nodes.filter(function(d){ return d.selected; }).length){
    displayWindow(texts.alertnonodes);
  }else{
    Graph.links.forEach(function(d){
      if(checkSelectableLink(d))
        if(d.source.selected || d.target.selected)
          d.source.__neighbor = d.target.__neighbor = true;
    });
    Graph.nodes.forEach(function(d){
      d.selected = checkSelectable(d) && d.__neighbor;
      delete d.__neighbor;
    });
    if(!heatmap)
      simulation.restart();
    showTables();
  }
}

function switchEgoNet(){
    egoNet = true;
    Graph.nodes.forEach(function(d){
      delete d._neighbor;
    });
    Graph.links.forEach(function(d){
      if(!d.noShow && !d._hideFrame)
        if(d.source.selected || d.target.selected)
          d.target._neighbor = d.source._neighbor = true;
    });
    drawNet();
}

function displayEgoNet(){
  if(!Graph.nodes.filter(function(d){ return d.selected; }).length)
    displayWindow(texts.alertnonodes);
  else
    switchEgoNet();
}

function checkSelectable(node){
  var selectable = !node.noShow && !node._hideFrame;
  if(egoNet)
    selectable = selectable && node._neighbor;
  return selectable;
}

function checkSelectableLink(link){
  var selectable = !link.noShow && !link.target.noShow && !link.source.noShow && !link._hideFrame;
  if(egoNet)
    selectable = selectable && ((link.source.select || link.source._neighbor) && (link.target.select || link.target._neighbor));
  return selectable;
}

function treeAction(){
  if(!Graph.nodes.filter(function(d){ return d.selected; }).length)
    Graph.nodes.forEach(function(d){
      if(checkSelectable(d))
        d.selected = true;
    });
  Graph.nodes.forEach(function(d){
    if(!d.selected)
      return;
    if(d.childNodes.length){
      d.childNodes.forEach(function(c){ delete c.noShow; });
      d.noShow = true;
      delete d.selected;
    }else{
      var return2roots = function(d){
        if(d.parentNode){
          return2roots(d.parentNode);
        }else
          delete d.noShow;
        };
      return2roots(d);
    }
  });
  drawNet();
}

function stopResumeNet(){
  options.stopped = !options.stopped;
  if(!options.stopped){
    Graph.nodes.forEach(function(node){
        delete node.fx;
        delete node.fy;
    });
    if(backupNodes){
      backupNodes.forEach(function(node){
        delete node.fx;
        delete node.fy;
      });
    }
    d3.selectAll(".slider.charge, .slider.linkDistance")
      .style("opacity",1);
    update_forces();
  }else{
    Graph.nodes.forEach(function(node){
        node.fx = node.x ? node.x : 0;
        node.fy = node.y ? node.y : 0;
    });
    d3.selectAll(".slider.charge, .slider.linkDistance")
      .style("opacity",0);
    simulation.stop();
  }
}

function clickHide(items, show) {
    items.transition()
      .duration(500)
      .style("opacity", +show);
}

function displayLegend(scale, key, color, shape, dat){
  var y = scale.node().getBBox().height;
  if(y!=0)
    y = y + 40*options.cex;
  else
    y = y + 20*options.cex;

  if(dat.length > (height-y)/(20*options.cex) - 1.1)
    return 0;

  var txt = key;
  if(color=="image" && shape)
    txt = options.imageNames[options.imageItems.indexOf(key)];

  var legend = scale.append("g")
    .attr("class","legend")
    .attr("transform", "translate(290,"+y+")")

  legend.append("text")
      .attr("class","title")
      .attr("x", 0)
      .attr("y", 5)
      .style("cursor","pointer")
      .text(txt)

  var g = legend.selectAll("g")
      .data(dat)
    .enter().append("g")
      .attr("transform", function(d, i){ return "translate(0," + (22+i*20)*options.cex + ")"; });

  function legendSelected(){
        var selecteds = d3.selectAll(".legend > g").filter(function(){ return this.selected; })
        Graph.nodes.forEach(function(d){
           d.selected = false;
           if(checkSelectable(d)){
             selecteds.each(function(p){
               if((p=="null" && d[this.selected]===null) || (p=="0" && d[this.selected]===0) || (d[this.selected] && (d[this.selected]==p || (typeof d[this.selected] == 'object' && d[this.selected].indexOf(p)!=-1))))
                 d.selected = true;
             });
           }
        });
  }

  if(color == "image"){
    g.append("image")
        .attr("xlink:href", String)
        .attr("x",-6)
        .attr("y",-8)
        .attr("width",16)
        .attr("height",16)
        .style("cursor","pointer")
        .on("click",function(){
          clickLegend(this,switchEgoNet);
        });
  }else{
    g.append("path")
      .attr("d", d3.symbol().type(typeof shape=="function" ? function(d){ return d3["symbol"+shape(d)]; } : d3["symbol"+shape]))
      .style("fill", color)
      .style("cursor","pointer")
      .on("click",function(){
        clickLegend(this,switchEgoNet);
      });
  }

  g.append("text")
      .attr("x", -10)
      .attr("y", 4*options.cex)
      .style("text-anchor", "end")
      .style("cursor","pointer")
      .on("click",function(){
        clickLegend(this,filterSelection);
      })
      .text((color == "image") ? (shape? function(d,i){ return shape[i]; } : getImageName) : stripTags)

  scale.transition()
    .duration(500)
    .style("opacity",+options.showLegend);

  function clickLegend(self,callback){
      if(self.parentNode.selected && !(d3.event.ctrlKey || d3.event.metaKey)){
          callback();
      }else{
        if(d3.event.ctrlKey || d3.event.metaKey){
          if(self.parentNode.selected)
            delete self.parentNode.selected;
          else
            self.parentNode.selected = key;
        }else{
          d3.selectAll(".legend > g").property("selected",null)
          self.parentNode.selected = key;
        }
        d3.selectAll(".legend > g > text").style("font-weight",function(){
            return this.parentNode.selected? "bold" : null;
        });
        legendSelected();
        if(!heatmap)
          simulation.restart();
        showTables();
      }
  }
}

function getImageName(path){
  var name = path.split("/");
  name = name.pop().split(".");
  name.pop();
  return name.join(".");
}

function stripTags(txt){
  return txt.replace(/(<([^>]+)>)/ig,"");
}

function displayScale(domain, fill, title){
    var scale = d3.select(".plot svg .scale");

    scale.style("opacity",0);

    scale.append("text")
    .attr("x",150)
    .attr("y",10)
    .text(title);

    scale.append("rect")
    .attr("x",0)
    .attr("y",20)
    .attr("height",10)
    .attr("width",300)
    .attr("rx",2)
    .attr("fill", fill);
    scale.append("text")
    .attr("x",0)
    .attr("y",12*options.cex + 32)
    .text(formatter(domain[0]));
    scale.append("text")
    .attr("x",300)
    .attr("y",12*options.cex + 32)
    .attr("text-anchor", "end")
    .text(formatter(domain[domain.length-1]));
    scale.transition()
    .duration(500)
    .style("opacity",+options.showLegend);
}

function showTables() {
  var noShow = noShowFields.slice((options.showCoordinates && !heatmap) ? 4 : 2);

  var tableWrapper = function(dat, name, columns){
    var currentData;
    if(name=="nodes")
      currentData = simulation.nodes();
    else
      currentData = simulation.force("link").links();
    var table = d3.select("div.tables div."+name),
        last = -1,
    drawTable = function(d){
      var tr = table.append("tr")
        .datum(d.index)
        .classed("selected",function(dd){
          return currentData[dd]._selected;
        });
      columns.forEach(function(col){
          var txt = d[col],
              textAlign = null;
          if(txt == null)
            txt = "";
          if(typeof txt == 'object'){
            if(frameControls)
              txt = txt[frameControls.frame];
            else
              txt = txt.join("; ");
          }
          if(typeof txt == 'number'){
            txt = formatter(txt);
            textAlign = "right";
          }
          tr.append("td").html(txt)
              .style("text-align",textAlign)
              .on("mousedown",function(){ d3.event.preventDefault(); });
      });
      tr.on("click",function(origin,j){
          if(d3.event.shiftKey && last!=-1)
            var selecteds = d3.range(Math.min(last,this.rowIndex),Math.max(last,this.rowIndex)+1);
          table.selectAll("tr").classed("selected", function(d,i){
            var selected = d3.select(this).classed("selected");
            if(selecteds){
              if(d3.event.ctrlKey || d3.event.metaKey)
                selected = selected || selecteds.indexOf(i)!=-1;
              else
                selected = selecteds.indexOf(i)!=-1;
            }else{
              if(d3.event.ctrlKey || d3.event.metaKey)
                selected = selected ^ d == origin;
              else
                selected = d == origin;
            }
            if(!heatmap){
              currentData[d]._selected = selected;                
            }
            return selected;
          })
          if(!heatmap)
            simulation.restart();

          if(d3.select(this).classed("selected"))
            last = this.rowIndex;
          else
            last = -1;
        });
    },

    drawHeader = function() {
        var thead = table.append("thead"),
            tbody = table.append("tbody"),
            desc = columns.map(function(){ return false; });
        columns.forEach(function(d,i){
          var sort1 = function(a,b){
                var rv = [1,-1];
                if(a[d]==null) return rv[0];
                if(b[d]==null) return rv[1];
                a = a[d][options.nodeName]?a[d][options.nodeName]:a[d];
                b = b[d][options.nodeName]?b[d][options.nodeName]:b[d];
                if(typeof a == "number" && typeof b == "number"){
                  if(!desc[i])
                    rv = rv.reverse();
                }else{
                  if(desc[i])
                    rv = rv.reverse();
                }
                if (a > b) {
                  return rv[0];
                }
                if (a < b) {
                  return rv[1];
                }
                return 0;
              };
          thead.append("th")
            .attr("class","sorting")
            .text(d)
            .on("click",function(){
              tbody.html("");
              dat.sort(sort1);
              var desci = desc[i];
              desc = columns.map(function(){ return false; });
              thead.selectAll("th").attr("class","sorting");
              desc[i] = !desci;
              d3.select(this).attr("class",desci ? "sorting_desc" : "sorting_asc");
              dat.forEach(drawTable);
            });
        });
        return tbody;
    }

    table.html("");
    table.append("div")
      .attr("class","title")
      .html("<span>"+texts[name+"attributes"] + "</span> ("+dat.length+" "+texts.outof+" "+(frameControls ? Graph[name].filter(function(d){ return !d._hideFrame; }).length : Graph[name].length)+")");
    table = table.append("div");
    if(dat.length==0){
      table.style("cursor",null);
      table.on('mousedown.drag', null);
      table.text(texts.noitemsselected);
    }else{
      table = table.append("table");
      table.on("mousedown", function(){ d3.event.stopPropagation(); })
      table = drawHeader();
      dat.forEach(drawTable);
      table.each(function(){
        var twidth = this.parentNode.offsetWidth;
        d3.select(this.parentNode.parentNode)
            .style("width",(twidth+8)+"px")
            .style("cursor","col-resize")
            .call(d3.drag()
              .on("drag",function(){
                var coorx = d3.mouse(this)[0],
                    self = d3.select(this),
                    selfTable = self.select("table");
                selfTable.style("width",(coorx-8)+"px");
                if(selfTable.node().offsetWidth > (coorx-8))
                  selfTable.style("width",selfTable.node().offsetWidth+"px");
                else
                  self.style("width",coorx+"px");
              })
            )
      })
    }
  },

  cleanData = function(d){
    var dReturn = {}, key;
    for(key in d)
      if(noShow.indexOf(key)==-1)
        dReturn[key] = d[key];
    return dReturn;
  },

  filterNoShow = function(d){
    return noShow.indexOf(d)==-1;
  };

  var nodesData = Graph.nodes.filter(function(d){
        delete d._selected;
        return checkSelectable(d) && d.selected;
      }).map(cleanData),
      linksData = Graph.links.filter(function(d){
        delete d._selected;
        return checkSelectableLink(d) && (d.source.selected && d.target.selected);
      }).map(cleanData),
      nodeColumns = Graph.nodenames.filter(filterNoShow),
      linkColumns = Graph.linknames.filter(filterNoShow);

  if(options.showCoordinates && !heatmap){
    var size = Math.min(width,height),

    x = d3.scaleLinear()
    .range([-width/size,+width/size])
    .domain([0,width]),

    y = d3.scaleLinear()
    .range([height/size,-height/size])
    .domain([0,height]);

    nodeColumns = d3.merge([nodeColumns,["x","y"]]);

    nodesData.forEach(function(d){
      d["x"] = x(d["x"]).toFixed(2);
      d["y"] = y(d["y"]).toFixed(2);
    });
  }

  tableWrapper(nodesData,"nodes",nodeColumns);
  tableWrapper(linksData,"links",linkColumns);
}

function tables2xlsx(){
      var nodes = [],
          links = [],
          tableNodes = d3.select(".tables .nodes table"),
          tableLinks = d3.select(".tables .links table"),
          loadData = function(table){
            var items = [];
            items.push([]);
            table.selectAll("th").each(function(){
              items[0].push(d3.select(this).text());
            })
            table.selectAll("tr").each(function(){
              var row = [];
              d3.select(this).selectAll("td").each(function(){
                var dat = d3.select(this).text();
                if(d3.select(this).style("text-align")=="right") dat = +dat;
                row.push(dat);
              })
              items.push(row);
            })
            return items;
          }
      if(!tableNodes.empty()){
        nodes = loadData(tableNodes);
      }
      if(!tableLinks.empty()){
        links = loadData(tableLinks);
      }
      if(nodes.length == 0 && links.length == 0)
        displayWindow(texts.noitemsselected);
      else
        downloadExcel({nodes: nodes, links: links}, d3.select("head>title").text());
}

function selectNodesFromTable(){
        var names = [],
            index = 0,
            trSelected;
        if(d3.select("div.tables div.nodes").style("display")=="block"){
          trSelected = d3.selectAll("div.nodes table tr.selected");
          if(!trSelected.empty()){
            d3.selectAll("div.nodes table th").each(function(d,i){
              if(this.textContent == options.nodeName)
                index = i+1;
            })
            trSelected
              .each(function(){
                names.push(d3.select(this).select("td:nth-child("+index+")").text());
              })
              .classed("selected",false);
          }else{
            displayWindow(texts.alertnonodestable);
          }
        }else{
          trSelected = d3.selectAll("div.links table tr.selected");
          if(!trSelected.empty()){
            trSelected
              .each(function(){
                for(var i=1; i<=2; i++){
                  index = d3.select(this).select("td:nth-child("+i+")").text();
                  if(names.indexOf(index)==-1)
                    names.push(index);
                }
              })
              .classed("selected",false);
          }else{
            displayWindow(texts.alertnonodestable);
          }
        }
        if(!trSelected.empty()){
          Graph.nodes.forEach(function(d){
            d.selected = names.indexOf(d[options.nodeName]) != -1;
          });
          if(!heatmap)
            simulation.restart();
          showTables();
        }
}

function adaptLayout(){
  var anyFixed = false,
      nodes = backupNodes ? backupNodes : Graph.nodes;

  for(var i=0; i<nodes.length; i++){
      if(nodes[i].hasOwnProperty("fx") || nodes[i].hasOwnProperty("fy")){
        anyFixed = true;
        break;
      }
  }
  if(anyFixed){
    var size = Math.min(width,height),
        xdim, ydim, xrange, yrange,
        centerDim = function(dim){
          if(dim[0]==dim[1]){
            dim[0] = dim[0] - 1;
            dim[1] = dim[1] + 1;
          }
          return dim;
        }

    if(oldWidth && oldHeight){
      var oldSize = Math.min(oldWidth,oldHeight);
      xdim = [(oldWidth-oldSize)/2,(oldWidth-oldSize)/2+oldSize];
      ydim = [(oldHeight-oldSize)/2,(oldHeight-oldSize)/2+oldSize];
      xrange = [(width-size)/2,(width-size)/2+size];
      yrange = [(height-size)/2,(height-size)/2+size];
    }else{
      if(options.hasOwnProperty("limits") && options.limits.length==4){
        xdim = [options.limits[0],options.limits[2]];
        ydim = [options.limits[1],options.limits[3]];
      }else{
        if(backupNodes){
          xdim = centerDim(d3.extent(d3.merge(nodes.map(function(d){ return d.fx }))));
          ydim = centerDim(d3.extent(d3.merge(nodes.map(function(d){ return d.fy }))));
        }else{
          xdim = centerDim(d3.extent(nodes,function(d){ return d.fx }));
          ydim = centerDim(d3.extent(nodes,function(d){ return d.fy }));
        }
      }
      size = size/1.2;
      xrange = [-size/2,-size/2 +size];
      yrange = [-size/2 +size,-size/2];
    }

    oldWidth = width;
    oldHeight = height;

    var x = d3.scaleLinear()
      .range(xrange)
      .domain(xdim);

    var y = d3.scaleLinear()
      .range(yrange)
      .domain(ydim);


    options.stopped = true;
    if(backupNodes){
      backupNodes.forEach(function(d){
        if(d.hasOwnProperty("fx"))
          d.fx = d.fx.map(function(e){ return(x(e)); });
        else
          options.stopped = false;
        if(d.hasOwnProperty("fy"))
          d.fy = d.fy.map(function(e){ return(y(e)); });
        else
          options.stopped = false;
      });
    }else{
      Graph.nodes.forEach(function(d){
        if(typeof d.fx == 'number')
          d.fx = x(d.fx);
        else
          options.stopped = false;
        if(typeof d.fy == 'number')
          d.fy = y(d.fy);
        else
          options.stopped = false;
      });
    }
  }
}

function embedImages(callback){

  var docSize = viewport(),
      loading = body.append("div")
      .attr("class","loading")
      .style("width",docSize.width+"px")
      .style("height",docSize.height+"px");

  loading.append("p")
    .text(texts.loading)

  if(options.imageItem){
    if(options.imageItems && !images64){

      images64 = {};

      var imgLinks = d3.set(d3.merge(options.imageItems.map(function(d){ return Graph.nodes.map(function(dd){ return dd[d]; }); }))).values();

      var loadImage = function(i){
        var imgSrc = imgLinks[i++];
        var img = images[imgSrc];
        var canvas = document.createElement("canvas");
        canvas.width = img.width;
        canvas.height = img.height;
        var ctx = canvas.getContext("2d");
        ctx.drawImage(img, 0, 0);
        try{
            images64[imgSrc] = canvas.toDataURL();
        }catch(e){
            console.log(e);
        }finally{
            if(i<imgLinks.length){
              loadImage(i);
            }else{
              callback();
              loading.remove();
            }
        }
      }

      loadImage(0);
      return 0;
    }
  }
  callback();
  loading.remove();
}

function svg2pdf(){
    displayWindow("The network is not loaded yet!");
}

function resetZoom(){
  transform.k = options.zoomScale = options.zoom;
  transform.x = width/2;
  transform.y = height/2;
}

function computeHeight(){
  var h = docSize.height - 2;
  if(options.main)
      h = h-parseInt(body.select("div.panel").style("top"));
  if(options.showTables){
      h = h - 170;
  }else if(options.showButtons2){
      h = h - (40 + 12*options.cex);
  }  
  return h;
}

window.onresize = function(){
  docSize = viewport();
  width = docSize.width;

  width = width - parseInt(panel.style("left")) - 20;
  height = computeHeight();

  plot.style("width",width+"px");
  plot.style("height",height+"px");
  plot.call(drawSVG);
}

} // network function end

if(typeof multiGraph == 'undefined'){
  window.onload = function(){
    network(JSON.parse(d3.select("#data").text()));
  };
}
