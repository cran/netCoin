function network(Graph){

  var docSize = viewport(),
      width = docSize.width - 210,
      height = docSize.height - 160,
      ctrlKey = false,
      images = false,
      options;

  var force = d3.layout.force()
      .on("tick", tick);

  force.drag().on("dragstart", function(d) {
    d3.event.sourceEvent.preventDefault();
    d3.event.sourceEvent.stopPropagation();
  });

  body = d3.select("body");

  options = Graph.options;
  delete Graph.options;

  Graph.links.forEach(function(d){
    d.source = Graph.nodes[d.source];
    d.target = Graph.nodes[d.target];
  });

  if(options.nodeItem){
    delete options.nodeColor;
    delete options.nodeShape;
  }

  options.charge = -200;
  options.linkDistance = 80;

  Graph.nodes.forEach(function(d){
    for (var p in d) {
      if(typeof d[p] == "string" && d[p].indexOf("|")!=-1)
        Graph.nodes.forEach(function(dd){ dd[p] = dd[p].split("|"); })
    }
  })

  if(options.main){
    body.append("div")
      .attr("class", "main")
      .html(options.main);
  }

// sidebar
  var sidebar = body.append("div")
      .attr("class", "sidebar");

// sidebar multi graph selection
  if(typeof multiGraph != 'undefined'){
    var multiSel = sidebar.append("div")
      .attr("class","subSidebar");
    multiSel.append("h2").text(texts.netselection);
    multiGraph.graphSelect(multiSel);
  }


// sidebar visualization
  var sideVisual = sidebar.append("div")
    .attr("class","subSidebar")

  sideVisual.append("h2").text(texts.visualization);

  sideVisual.append("h3").text(texts.nodes);

  sideVisual.append("div")
      .attr("class", "nodeAuto")
      .append("h4").text(texts.general);

  sideVisual.append("div")
      .attr("class", "nodeEdit")
      .append("h4").text(texts.particular);

  sideVisual.append("h3").text(texts.links);

  sideVisual.append("div")
      .attr("class", "linkAuto");

// sidebar selection
  var sideSelect = sidebar.append("div")
      .attr("class", "subSidebar")
      
  sideSelect.append("h2").text(texts.selection);

  sideSelect.append("h3").text(texts.nodes);

  sideSelect.append("div")
      .attr("class","nodeSelect");

  sideSelect.append("h3").text(texts.links);

  sideSelect.append("div")
      .attr("class","linkSelect");


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

// panel
  var panel = body.append("div")
      .attr("class", "panel");

  if(options.main)
    panel.style("top","38px")

  var plot = panel.append("div")
      .attr("class", "plot")
      .style({"width":width+"px","height":height+"px"});

  if(options.minor){
    panel.append("div")
      .attr("class", "minor")
      .html(options.minor);
  }

// tables
  var tables = panel.append("div")
      .attr("class", "tables")

  if(options.help){
    iconButton(tables,"help",infoIcon_b64,"info",function(){
      var win = displayWindow();
      win.html(options.help);
    });
  }

  iconButton(tables,"xlsx",xlsxIcon_b64,texts.downloadtable,tables2xlsx);

  if(!(options.nodeItem && options.nodeItem == "pie"))
    iconButton(tables,"pdf",pdfIcon_b64,texts.pdfexport,function(){ embedImages(svg2pdf); });

  iconButton(tables,"svg",svgIcon_b64,texts.svgexport,function(){ embedImages(svgDownload); });

  var buttonsSelect = tables.append("div")
                        .attr("class","selectButton")
  var selectButton = function(txt,clk){
        buttonsSelect.append("div")
          .text(txt)
          .on("click",clk)
      }

  selectButton(texts.selectall,selectAllNodes);
  selectButton(texts.tableselection,selectNodesFromTable);
  selectButton(texts.selectneighbors,selectNeighbors);
  selectButton(texts.isolateselection,isolateNodes);
  selectButton(texts.resetfilter,deleteNoShow);

  tables.append("h2")
    .attr("class","nodes")
    .text(texts.nodeattributes)
  tables.append("div").attr("class","nodes")
    .text(texts.noitemsselected);
  tables.append("h2")
    .attr("class","links")
    .text(texts.linkattributes)
  tables.append("div").attr("class","links")
    .text(texts.noitemsselected);

// dragbar

  var dragbar = body.append("div")
      .attr("class","dragbar")
      .style({"width":"5px","height":docSize.height+"px","cursor":"col-resize","position":"absolute","top":"0px","left":"185px","z-index":1});

  dragbar.call(d3.behavior.drag()
    .on("dragstart", function() {
      body.style("cursor","col-resize");
      d3.selectAll(".sidebar svg").style("visibility","hidden");
      plot.select("svg").remove();
    })
    .on("drag", function() {
      value = d3.mouse(body.node())[0];
      if(value > 177 && value < 400){
        dragbar.style("left", value + "px")
        sidebar.style("width", (value-10) + "px")
        panel.style("left", (value+5) + "px")
        width = docSize.width - 25 - value;
        plot.style("width",width+"px");
      }
    })
    .on("dragend", function() {
      body.style("cursor",null);
      d3.selectAll(".sidebar select.attrSel").each(function(){ 
        this.__onchange();
      });
      d3.selectAll(".sidebar svg").style("visibility",null);
      plot.call(drawSVG);
      drawNet();
    })
  );

// draw SVG in panel
  plot.call(drawSVG);

// draw Net in SVG
  drawNet();

// add controllers in sidebar
  var applyFuncObject = {};
  applyFuncObject[texts.filter] = applyFilter;
  applyFuncObject[texts.select] = applySelection;
  addController("nodeSelect", Graph.nodes, true, applyFuncObject);
  applyFuncObject = {};
  applyFuncObject[texts.filter] = applyFilter;
  addController("linkSelect", Graph.links, true, applyFuncObject);

  addController("nodeAuto", Graph.nodes, false, applyAuto("node"), options.nodeItem?["Label","Group","Size"]:["Label","Group","Size","Color","Shape"]);

  addController("nodeEdit", Graph.nodes, true, applyEditNode, options.nodeItem?["Size","Label"]:["Size","Color","Shape","Label"]);

  addController("linkAuto", Graph.links, false, applyAuto("link"),["Width","Weight","Color","Text"]);

// change node d3 weight attribute name
  d3.selectAll('.sidebar [class^="node"] .attrSel option[value=weight]').text(texts.degree);


function addController(name, data, selection, applyFunc, visData){
  var contr = d3.select("div."+name),
      attrData = d3.keys(data[0]),
      noShow = ["target","source","index","px","py","x","y","fixed","id","nodeSize","noShow"],
      loadPicker;

  var i = 0,
      index = -1;
  for(var i = 0; i < noShow.length; i++){
    index = attrData.indexOf(noShow[i]);
    if(index > -1)
      attrData.splice(index,1);
  }

  if(!selection){
    attrData.unshift("-"+texts.none+"-");
  }

  if(visData)
    contr.call(selectVisual(visData));

  var attrSelect = contr.append("select")
      .attr("class","attrSel");

  attrSelect.selectAll("option")
    .data(attrData)
    .enter().append("option")
      .property("value",function(d){ return d; })
      .text(function(d){ return d; })

  if(selection){
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
      if(typeof data[0][val] == 'number'){
        var extent = d3.extent(data, function(d){ return d[val]; }),
            baseWidth = parseInt(valSelector.style("width"))+16;
        brushSlider(valSelector,extent,selectedValues[val],function(s){ selectedValues[val] = s; },baseWidth);
      }else{
        var dat = data.map(function(d){ return d[val]; });
        if(typeof dat[0] !== 'string')
          dat = dat.reduce(function(a,b) { return a.concat(b); }, []);
        valSelector.append("select")
          .attr("multiple","multiple")
          .on("blur", function() { loadSelValues(val); })
          .selectAll("option")
        .data(d3.set(dat).values().sort())
          .enter().append("option")
          .property("value",function(d){ return d; })
          .text(function(d){ return d; })
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

  loadPicker = function(){ displayPickerEdit(contr); }
}else{
  loadPicker = function(){ displayPicker(contr,data); }
  attrSelect
     .on("change",loadPicker)
     .selectAll("option")
       .on("click",loadPicker)
}
  contr.select(".visSel")
     .on("change",loadPicker)
     .selectAll("option")
       .on("click",loadPicker)

  var objFunc = {};
  if(typeof applyFunc == 'function')
    objFunc[texts.apply] = applyFunc;
  else
    objFunc = applyFunc
  for(i in objFunc)
    contr.append("button")
      .text(i)
      .on("click", function(){ 
        objFunc[this.textContent](prepareQuery(),data); 
      });

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
  showTables();
}

function applySelection(query,data){
  data.forEach(function(d){
    if(eval(query))
      d.selected = true;
    else
      delete d.selected;
  });
  drawNet();
  showTables();
}

function selectVisual(dat){
  return function(sel){
      sel.append("select")
          .attr("class","visSel")
        .selectAll("option")
        .data(dat)
          .enter().append("option")
          .property("value",String)
          .text(function(d){ return texts[d]; });

      sel.property("picker","disabled");
    }
};

function displayPicker(sel,dat){
  if(d3.event && d3.select("div.window").empty()){
    var val = sel.select(".visSel").property("value"),
        attrVal = sel.select(".attrSel").property("value");
        if((val=="Color"||val=="Group") && typeof dat[0][attrVal] == "number"){
          var picker = displayWindow(),
              scaleKeys = d3.keys(colorScales);
	  picker
              .append("ul")
              .attr("class","picker")
              .selectAll("li")
              .data(scaleKeys)
                .enter().append("li")
                .property("val",String)
                .on("click",function(){
                  sel.property("picker", this.val)
                })
                .append("img")
                  .attr("alt", String)
                  .attr("src", function(d){ return colorScales[d].png; })
                  .attr("width",60)
                  .attr("height",10)
        }else{
          sel.property("picker","disabled");
        }
  }
}

function applyAuto(item){
    return function(){
    var attr = d3.select("."+item+"Auto .attrSel").property("value"),
        visual = d3.select("."+item+"Auto .visSel").property("value"),
        color = d3.select("."+item+"Auto").property("picker");
      if(color!='disabled'){
        if(color == "" || attr=="-"+texts.none+"-")
          delete options["colorScale"+item+visual];
        else
          options["colorScale"+item+visual] = color;
      }
      if(attr=="-"+texts.none+"-")
        delete options[item+visual];
      else
        options[item+visual] = attr;
      drawNet();
    }
}

function displayPickerEdit(sel){
  if(d3.select("div.window").empty()){
    var picker = displayWindow(),
        val = sel.select(".visSel").property("value"),
        isColor = function(d){ return (typeof d == 'string' && d.indexOf("#")==0) ? d : null; },
        keys = [];

    switch(val){
        case "Color":
            keys = d3.scale.category20().range();
          break;
        case "Size":
            keys = d3.range(0.5,4.5,0.5);
          break;
        case "Shape":
            keys =  d3.svg.symbolTypes;
          break;
        case "Label":
            keys = ["Show","Hide"];
          break;
     }

    picker
      .append("ul")
      .attr("class","picker")
      .selectAll("li")
      .data(keys)
      .enter().append("li")
        .property("val",String)
        .on("click",function(){
          sel.property("picker", this.val)
        })
        .style("background",isColor)
        .style("color",isColor)
        .text(function(d){ return texts[d]?texts[d]:d; });
  }
}

function applyEditNode(query){
    var visual = d3.select(".nodeEdit .visSel").property("value"),
        val = d3.select(".nodeEdit").property("picker"),
        nodes = d3.selectAll(".node").filter(function(d){ return eval(query); });
    if(val!="disabled"){
      switch(visual){
        case "Color":
            nodes.selectAll("path").transition().duration(500).attr("fill",val);
          break;
        case "Size":
            if(options.directional){
              nodes.each(function(d){ d.nodeSize = val; });
              tick();
            }
            nodes.selectAll('[transform^="scale"]').transition().duration(500).attr("transform","scale("+val+","+val+")");
            nodes.selectAll("text").transition().duration(500).attr("x",10+(val-1)*4);
          break;
        case "Shape":
            nodes.selectAll("path").attr("d",d3.svg.symbol().type(val));
          break;
        case "Label":
            var items = nodes.selectAll("text");
            if(val == "Hide")
              items.transition().duration(500).style("opacity",0);
            else
              items.transition().duration(500).style("opacity",1);
          break;
      }
    }
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
  var shiftKey = false,
    saved = {};

  var zoom = d3.behavior.zoom()
    .scaleExtent([0.5, 10])
    .on("zoom", zoomed);

  if(options.stopped)
    adaptLayout();

  force.size([width, height]);

  body
    .on("keydown", keyflip)
    .on("keyup", keyflip)

  var svg = sel.append("svg")
      .attr("xmlns","http://www.w3.org/2000/svg")
      .attr("width", width)
      .attr("height", height);

    svg.append("style")
     .text("text { font-family: sans-serif; } "+
".scale text { font-size: 12px; fill: #444; } "+
".link { opacity: .6; } "+
".linkText { stroke-width: 0.5px; font-size: 7px; fill: #999; } "+
".node > path { stroke: #000; vector-effect: non-scaling-stroke; } "+
".node text { stroke-width: 0.5px; font-size: 8px; fill: #444; } "+
".area { opacity: 0.2; stroke-width:3; } "+
".node.selected path { stroke: yellow; stroke-width: 2; } ");

  var defs = svg.append("defs");
  d3.keys(colorScales).forEach(function(d){ addGradient(defs,d,colorScales[d].colors); });

  defs.selectAll("marker")
    .data(["end"])
  .enter().append("marker")
    .attr("id", String)
    .attr("viewBox", "0 -5 10 10")
    .attr("markerUnits", "userSpaceOnUse")
    .attr("refX", 0)
    .attr("refY", 0)
    .attr("markerWidth", 8)
    .attr("markerHeight", 8)
    .attr("orient", "auto")
  .append("path")
    .attr("d", "M0,-5L10,0L0,5")
    .style({"fill":"#999"});

  svg.call(zoom);

  svg.append("rect")
    .attr("width", width)
    .attr("height", height)
    .style("fill", "none")
    .style({"pointer-events":"all"});

  var net = svg.append("g")
    .attr("class","net")

  var brush = svg.append("g")
    .attr("class","brush net")
    .call(d3.svg.brush()
        .x(d3.scale.identity().domain([0, width]))
        .y(d3.scale.identity().domain([0, height]))
        .on("brush", function() {
          var extent = d3.event.target.extent();
          extent[0][0] = 1/saved.scale * (extent[0][0] - saved.translate[0]);
          extent[0][1] = 1/saved.scale * (extent[0][1] - saved.translate[1]);
          extent[1][0] = 1/saved.scale * (extent[1][0] - saved.translate[0]);
          extent[1][1] = 1/saved.scale * (extent[1][1] - saved.translate[1]);
          d3.selectAll(".node").classed("selected", function(d) {
            return d.selected = extent[0][0] <= d.x && d.x < extent[1][0]
                && extent[0][1] <= d.y && d.y < extent[1][1];
          });
        })
        .on("brushend", function() {
          d3.event.target.clear();
          d3.select(this).call(d3.event.target);
          showTables();
        }))

  brush.style("display","none");

  svg.append("g").attr("class","scale")
    .attr("transform", "translate("+(width-320)+",20)");

  var buttons = svg.append("g")
      .attr("class", "buttons")
      .attr("transform", "translate(20,20)")

  var count = 0,
      nextCount = function(){ return count = count + 15; };
  addButton(buttons,count,texts.stopresume,stopResumeNet);
  count = count+5;
  addButton(buttons,nextCount(),texts.directional,switchDirectional);
  addButton(buttons,nextCount(),texts.showhidelegend,function(){ clickHide(d3.selectAll(".scale")) });
  addButton(buttons,nextCount(),texts.showhidelabels,function(){ clickHide(d3.selectAll(".node text")) });
  count = count+5;
  addButton(buttons,nextCount(),texts.reset,function(){ location.reload(); });

  var sliders = buttons.append("g")
      .attr("class","sliders")
      .attr("transform", "translate(150,0)")
      .style("opacity",options.stopped?0:null);

  displaySlider(sliders, 5, [0, -500], texts.repulsion, 'charge');

  displaySlider(sliders, 20, [0, 200], texts.distance, 'linkDistance');

  function zoomed() {
    if(!shiftKey){
      net.attr("transform", "translate(" + zoom.translate() + ")scale(" + zoom.scale() + ")");
    }
  }

  function keyflip() {
    ctrlKey = d3.event.ctrlKey;
    shiftKey = d3.event.shiftKey;
    if(shiftKey){
      if(!saved.scale){
        saved.scale = zoom.scale();
        saved.translate = zoom.translate();
        brush.style("display",null);
      }
    }else{
      if(saved.scale){
        brush.style("display","none");
        zoom.scale(saved.scale)
        zoom.translate(saved.translate);
        saved = {};
      }
    }
  }
}

function drawNet(){

  var svg = d3.select(".plot svg g.net");

  var gScale = d3.select(".plot svg .scale");
  gScale.selectAll("*").remove();

  var nodes = Graph.nodes.filter(function(d){ return !d.noShow; }),
    links = Graph.links.filter(function(d){ return !d.noShow && !d.target.noShow && !d.source.noShow; });

  var colorNodesScale = setColorScale(nodes,"nodeColor",true),
      colorGroupsScale = setColorScale(nodes,"nodeGroup",false),
      colorLinksScale = setColorScale(links,"linkColor",false),
  colorNodes = colorNodesScale?function(d){ return colorNodesScale(d[options.nodeColor]); }:"#1f77b4",
  colorGroups = colorGroupsScale?(function(d){ return colorGroupsScale(d[options.nodeGroup]); }):"#1f77b4",
  colorLinks = colorLinksScale?(function(d){ return colorLinksScale(d[options.linkColor]); }):"#999";

  var getNodeSize;
  if(options.nodeSize && typeof nodes[0][options.nodeSize] == "number"){
    var nodeSizeDomain = d3.extent(nodes, function(d) { return d[options.nodeSize]; });

    var nodeSize = d3.scale.linear()
    .domain(nodeSizeDomain)
    .range([0.5,4]);

    getNodeSize = function(d){ d.nodeSize = nodeSize(d[options.nodeSize]); }
  }else{
    if(options.nodeItem)
      getNodeSize = function(d){ d.nodeSize = 3; }
    else
      getNodeSize = function(d){ d.nodeSize = 1; }
  }

  var getLinkAttr = function(linkAttr,range,def){
        if(options[linkAttr] && links.length && typeof links[0][options[linkAttr]] == "number"){
          var attrDomain = d3.extent(links, function(d) { return d[options[linkAttr]]; });

          var scaleAttr = d3.scale.linear()
            .range(range)
            .domain(attrDomain);

          return function(d) { return scaleAttr(d[options[linkAttr]]); }
        }else{
          return def;
        }
  },
  getLinkDistance = getLinkAttr('linkWeight',[200,40],80),
  getLinkWidth = getLinkAttr('linkWidth',[1,5],1);

  var getShape = function() { return "circle"; }
  if(options.nodeShape){
    var symbolList = d3.scale.ordinal()
       .range(d3.svg.symbolTypes);

    getShape = function(d) { return symbolList(d[options.nodeShape]); }
  }

  //hide link distance slider for link weight representation
  d3.select(".slider.linkDistance").style("display",options.linkWeight?"none":null)

  //update force
  force
      .nodes(nodes)
      .links(links)
      .linkDistance(getLinkDistance)
      .start();

  // nodes
  var node = svg.selectAll(".node")
      .data(nodes, function(d) { return d[options.nodeName]; });

  //exit nodes
  node.exit().remove();

  //enter nodes
  var nodeEnter = node.enter().append("g")
	.attr("class", "node")
        .on("click", function(d) {
          if(ctrlKey)
            d3.select(this).classed("selected", function(p) { return p.selected = !d.selected; });
          else
            node.classed("selected", function(p) { return p.selected = d[options.nodeName] == p[options.nodeName]; });
          showTables();
        })
        .call(force.drag);

  var nodeItem;
  if(!options.nodeItem){
    nodeItem = nodeEnter.append("path")
  } else {
    if(options.nodeItem == 'image'){
      nodeItem = nodeEnter.append("image")
        .attr("xlink:href", function(d){ return d.image; })
        .attr("x",-5)
        .attr("y",-5)
        .attr("width",10)
        .attr("height",10);
    }
    if(options.nodeItem == 'pie'){
      nodeItem = nodeEnter.append("g");
      nodeItem.call(addPie);
    }
  }
  nodeItem
    .style("cursor","pointer")

  nodeEnter.append("text")
      .attr("y", 4);

  if(options.nodeText){
    tooltip(nodeEnter,options.nodeText);
  }

  //update nodes
  node.each(getNodeSize);

  node.selectAll(".node>path")
      .attr("d", d3.svg.symbol().type(getShape))
      .attr("fill", colorNodes);

  node.classed("selected", function(d) { return d.selected; });

  node.selectAll(".node > :first-child").attr("transform", function(d) { return "scale("+d.nodeSize+")"; });

  node.selectAll("text")
      .attr("x", function(d) { return 10+(d.nodeSize-1)*4; })
      .text(function(d) { return options.nodeLabel?d[options.nodeLabel]:d[options.nodeName]; });

  // links
  var link = svg.selectAll(".link")
        .data(links, function(d) { return d.source[options.nodeName]+" "+d.target[options.nodeName]; });

  //exit links
  link.exit().remove();

  //enter link
  link.enter().insert("path",".node")
      .attr("class", "link");

  //update links
  link.attr("stroke-width", getLinkWidth)
      .attr("stroke", colorLinks)
      .attr("marker-end", (options.directional)?"url(#end)":null);

  // link text
  var linkText = svg.selectAll(".linkText")
        .data(options.linkText?links:[], function(d) { return d.source[options.nodeName]+" "+d.target[options.nodeName]; })

  //exit link text
  linkText.exit().remove();

  //enter  link text
  linkText.enter().insert("text",".node")
      .attr("class", "linkText")

  //update link text
  linkText.text(function(d) { 
    var txt = d[options.linkText];
    return typeof txt == 'number'?formatter(txt):txt;
  });

  var groups = [];
  if(options.nodeGroup){
    groups = d3.set(nodes.map(function(d){return d[options.nodeGroup];})).values();
    groups = groups.map(function(d){
      var dd = {};
      dd[options.nodeGroup] = d;
      dd.xExt = [0,0];
      dd.yExt = [0,0];
      return dd;
    });
  }

  // areas
  var area = svg.selectAll(".area")
        .data(groups)

  //exit areas
  area.exit().remove();

  //enter areas
  area.enter().insert("rect",".link")
    .attr("class", "area")
    .attr("rx", 10)

  //update areas
  area.style("stroke",colorGroups)
    .style("fill",function(d) { return d3.rgb(colorGroups(d)).brighter(0.6); });

  if(options.nodeColor && !options.colorScalenodeColor)
    displayLegend(gScale,options.nodeColor,colorNodesScale,"square",colorNodesScale.domain().sort());

  if(options.nodeShape)
    displayLegend(gScale,options.nodeShape,"#000000",symbolList,symbolList.domain().sort());

  if(options.nodeItem){
    if(options.nodeItem == 'image'){
      var imagesData = d3.set(node.data().map(function(d){ return d.image; })).values().sort();
      if(imagesData.length < (height-30)/20)
        displayLegend(gScale,options.imageLegend,'image','image',imagesData);
    }
    if(options.nodeItem == 'pie')
      displayLegend(gScale,"pie",d3.scale.category10(),"square",options.pieLegend);
  }
}

function tick() {
  if(options.directional){
    d3.selectAll(".link").attr("d", function(d) {
            // Total difference in x and y from source to target
            diffX = d.target.x - d.source.x;
            diffY = d.target.y - d.source.y;

            // Length of path from center of source node to center of target node
            pathLength = Math.sqrt((diffX * diffX) + (diffY * diffY));

            // x and y distances from center to outside edge of target node
            offsetX = (diffX * (d.target.nodeSize*5+8)) / pathLength;
            offsetY = (diffY * (d.target.nodeSize*5+8)) / pathLength;

            return "M" + d.source.x + "," + d.source.y + "L" + (d.target.x - offsetX) + "," + (d.target.y - offsetY);
        });
  }else{
    d3.selectAll(".link").attr("d", function(d) {
            return "M" + d.source.x + "," + d.source.y + "L" + d.target.x + "," + d.target.y;
        });
  }

    d3.selectAll(".linkText").attr("x", function(d) { return ((d.target.x)+(d.source.x))/2; })
        .attr("y", function(d) { return ((d.target.y)+(d.source.y))/2; });

    d3.selectAll(".node").attr("transform", function(d){  return "translate(" + d.x + "," + d.y + ")"; });

    if(options.nodeGroup){
      d3.selectAll(".area").each(function(dd){
          var points = d3.selectAll(".node").filter(function(d){ return d[options.nodeGroup]==dd[options.nodeGroup]; }).data();
          dd.xExt = d3.extent(points,function(d){ return d.x;});
          dd.yExt = d3.extent(points,function(d){ return d.y;});
      })
        .attr("x", function(d){ return d.xExt[0]-3 })
        .attr("y", function(d){ return d.yExt[0]-3 })
        .attr("width", function(d){ return d.xExt[1]-d.xExt[0]+6 })
        .attr("height", function(d){ return d.yExt[1]-d.yExt[0]+6 });
    }
}

function setColorScale(data,name,drawScale){
    if(options[name]){
      var scale;
      if(options["colorScale"+name] && typeof data[0][options[name]] == "number"){
        var colorDomain = d3.extent(data, function(d) { return d[options[name]]; }),
            nameScale = options["colorScale"+name];
        if(drawScale)
          displayScale(colorDomain, "url(#"+nameScale+")");
        scale = d3.scale.linear().range(colorScales[nameScale].colors).domain([colorDomain[0],d3.mean(colorDomain),colorDomain[1]]);
      }else{
        scale = d3.scale.category20();
      }
      return scale;
    }else{
      return false;
    }
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
  })
}

function addPie(nodeItem){
  var color = d3.scale.category10();

  var arc = d3.svg.arc()
    .outerRadius(5)
    .innerRadius(0);

  var pie = d3.layout.pie()
    .sort(null);

  nodeItem.each(function(d){
    var g = d3.select(this).selectAll(".arc")
      .data(pie(JSON.parse(d.pie)))
    .enter().append("path")
      .attr("class", "arc")
      .attr("d", arc)
      .style("fill", function(d,i) { return color(i); })
      .style("stroke-width",1)
  });
}

function addButton(sel,y,txt,callback) {
    sel.append("rect")
	.attr("x",0)
	.attr("y",y)
	.attr("rx",2)
	.attr("ry",2)
	.attr("width",30)
	.attr("height",10)
	.on("click", callback);
    if(txt){
	sel.append("text")
	.attr("x",35)
	.attr("y",y+8)
	.text(txt);
    }
}

function switchDirectional(){
  options.directional = !options.directional;
  drawNet(); 
}

function selectAllNodes(){
  var action;
  if(d3.selectAll(".node.selected").empty())
    action = true;
  else
    action = false;
  d3.selectAll(".node").classed("selected",function(d){ return d.selected = action; })
  showTables();
}

function isolateNodes(){
  if(d3.selectAll(".node.selected").empty()){
    displayWindow(texts.alertnonodes);
  }else{
    d3.selectAll(".node:not(.selected)").each(function(d){
      d.noShow = true;
      delete d.selected;
    });
    drawNet();
    showTables();
  }
}

function deleteNoShow(){
  Graph.nodes.forEach(function(d){
    delete d.noShow;
  });
  drawNet();
}

function selectNeighbors(){
  if(d3.selectAll(".node.selected").empty()){
    displayWindow(texts.alertnonodes);
  }else{
    d3.selectAll(".link").each(function(d){
      if(d.source.selected&&!d.target.selected)
        d.target.neighbor = true;
      if(d.target.selected&&!d.source.selected)
        d.source.neighbor = true;
    });
    d3.selectAll(".node").classed("selected",function(d){
      d.selected = d.selected || d.neighbor;
      delete d.neighbor;
      return d.selected;
    });
    showTables();
  }
}

function stopResumeNet(){
  d3.selectAll(".node").each(function(d){
    if(options.stopped)
      delete d.fixed;
    else
      d.fixed = 1;
  });
  if(options.stopped){
    d3.select(".sliders").transition()
	.duration(500)
        .style("opacity",1);
    force.resume();
  }else{
    d3.select(".sliders").transition()
	.duration(500)
        .style("opacity",0);
  }
  options.stopped = !options.stopped;
}

function clickHide(items) {
    if(items.style("opacity")!=0){
      items.transition()
	.duration(500)
	.style("opacity",0);}
    else
      items.transition()
	.duration(500)
	.style("opacity",1);
}

function displayLegend(scale, txt, color, shape, dat){

  var y = scale.node().getBBox().height;
  if(y!=0)
    y = y + 20;

  var key = (color == "image" && shape == "image")? "image" : txt;

  var legend = scale.append("g")
    .attr("class","legend")
    .attr("transform", "translate(290,"+y+")")

  legend.append("text")
      .attr("class","title")
      .attr("x", -6)
      .attr("y", 5)
      .style({"text-anchor":"end","font-size":"16px"})
      .text(txt);

  var g = legend.selectAll("g")
      .data(dat)
    .enter().append("g")
      .attr("transform", function(d, i){ return "translate(0," + (24+i*20) + ")"; })
      .on("click", function(d){
        d3.selectAll(".node").classed("selected", function(p){
          if(ctrlKey)
            p.selected = p.selected ^ d == p[key];
          else
            p.selected = d == p[key];
          return p.selected;
        })
        showTables();
      })

  if(key == "image")
    g.append("image")
        .attr("xlink:href", String)
        .attr("x",-6)
        .attr("y",-8)
        .attr("width",16)
        .attr("height",16);
  else
    g.append("path")
      .attr("d", d3.svg.symbol().type(shape))
      .style("fill", color);

  g.append("text")
      .attr("x", -10)
      .attr("y", 4)
      .style("text-anchor", "end")
      .text((key == "image") ? function(d){
        var name = d.split("/");
        name = name.pop().split(".");
        name.pop();
        return name.join(".");
      } : String)

  scale.transition()
	.duration(500)
	.style("opacity",1);
}

function displayScale(domain, fill){
    var scale = d3.select(".plot svg .scale");

    scale.style("opacity",0);

    scale.append("rect")
	.attr("x",0)
	.attr("y",0)
	.attr("height",10)
	.attr("width",300)
	.attr("rx",2)
	.attr("fill", fill);
    scale.append("text")
	.attr("x",0)
	.attr("y",25)
	.text(formatter(domain[0]));
    scale.append("text")
	.attr("x",300)
	.attr("y",25)
	.attr("text-anchor", "end")
	.text(formatter(domain[domain.length-1]));
    scale.transition()
	.duration(500)
	.style("opacity",1);
}

function displaySlider(sliders, y, domain, txt, name){
	var scale = d3.scale.linear()
	    .domain(domain)
	    .range([0, 200])
	    .clamp(true);

	var brush = d3.svg.brush()
	    .x(scale)
	    .extent([0, 0])
	    .on("brush", brushed);

        sliders = sliders.append("g")
            .attr("class","slider "+name)

	sliders.append("text")
	    .attr("x", 210)
	    .attr("y", y+3)
	    .text(txt);

	var slider = sliders.append("g")
	    .attr("transform", "translate(0,"+ y +")")
	    .attr("class", "x axis brushSlider")
	    .call(d3.svg.axis()
	      .scale(scale)
	      .orient("bottom")
	      .tickSize(0)
              .ticks(0))
	    .append("g")
	      .attr("class", "slider")
	      .call(brush);

        slider.select(".background")
          .attr({"height":10,"y":-5})
          .style("cursor","pointer");

	var handle = slider.append("circle")
	    .attr("class", "handle")
	    .attr("r", 6);

	slider
	    .call(brush.extent([options[name],options[name]]))
	    .call(brush.event);

	slider.selectAll(".extent, .resize").remove();

	function brushed() {
	  var value = brush.extent()[0];

	  if (d3.event.sourceEvent) {
            if("stopPropagation" in d3.event.sourceEvent){
              d3.event.sourceEvent.stopPropagation();
	      value = scale.invert(d3.mouse(this)[0]);
	      brush.extent([value, value]);
            }
	  }

	  handle.attr("cx", scale(value));
          options[name] = value;
          force[name](value);
          force.start();
	}
}

function showTables() {
  var noShow = ["px","py","fixed","index","id","selected","nodeSize","noShow","image"],

  tableWrapper = function(dat, name){
    var table = d3.select("div.tables div."+name),
    drawTable = function(d){
      var tr = table.append("tr"),
        currentItem;
      if(d[options.nodeName])
        currentItem = d3.selectAll(".node.selected>path").filter(function(p){ return p[options.nodeName] == d[options.nodeName]; });
      else
        currentItem = d3.selectAll(".link").filter(function(p){ return (p.source[options.nodeName] == d.source[options.nodeName]) && (p.target[options.nodeName] == d.target[options.nodeName]); });
      d3.entries(d).forEach(function(dd){
          var txt = dd.value,
              textAlign = null;
          if(txt == null)
            txt = "";
          if(txt[options.nodeName])
            txt = txt[options.nodeName];
          if(typeof txt == 'object')
            txt = txt.join(", ");
          if(typeof txt == 'number'){
            txt = formatter(txt);
            textAlign = "right";
          }
          tr.append("td").text(txt)
              .style("text-align",textAlign)
              .on("mousedown",function(){ d3.event.preventDefault(); });
      });
      tr.on("mouseover",function(){ currentItem.style("stroke","red"); })
        .on("mouseout",function(){ currentItem.style("stroke",null); })
        .on("click",function(){
          var origin = this;
          table.selectAll("tr").classed("selected", function(){
            var selected = d3.select(this).classed("selected");
            if(ctrlKey)
              selected = selected ^ this === origin;
            else
              selected = this === origin;
            return selected;
          })
        });
    },

    drawHeader = function(items) {
        var thead = table.append("thead"),
            tbody = table.append("tbody"),
            desc = false;
        items.forEach(function(d){
          var sort1 = function(a,b){
              var rv = [1,-1];
              a = a[d]==null?Infinity:a[d][options.nodeName]?a[d][options.nodeName]:a[d];
              b = b[d]==null?Infinity:b[d][options.nodeName]?b[d][options.nodeName]:b[d];
              if(typeof a == "number" && typeof b == "number"){
                if(!desc)
                  rv = rv.reverse();
              }else{
                if(desc)
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
            .text(d)
            .on("click",function(){
              tbody.html("");
              dat.sort(sort1);
              desc = !desc;
              dat.forEach(drawTable);
            });
        })
        return tbody;
    }

    table.html("");
    if(dat.length==0){
      table.style("cursor",null);
      table.on('mousedown.drag', null);
      table.text(texts.noitemsselected);
    }else{
      table = table.append("table");
      table.on("mousedown", function(){ d3.event.stopPropagation(); })
      table = drawHeader(d3.keys(dat[0]));
      dat.forEach(drawTable);
      table.each(function(){
        var twidth = this.parentNode.offsetWidth;
        d3.select(this.parentNode.parentNode)
            .style({"width":(twidth+8)+"px","cursor":"col-resize"})
            .call(d3.behavior.drag()
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
  }

  var nodesData = d3.selectAll(".node.selected").data().map(cleanData),
      linksData = Graph.links.filter(function(d){ return d.source.selected && d.target.selected; }).map(cleanData);
  tableWrapper(nodesData,"nodes");
  tableWrapper(linksData,"links");

  d3.selectAll(".tables .nodes th")
    .filter(function(){ return this.textContent == "weight"; })
      .text(texts.degree);
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
                row.push(d3.select(this).text());
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
            trSelected = d3.selectAll("div.nodes table tr.selected");
        if(trSelected.empty()){
          displayWindow(texts.alertnonodestable);
        }else{
          d3.selectAll("div.nodes table th").each(function(d,i){
            if(this.textContent == options.nodeName)
              index = i+1;
          })
          trSelected
            .each(function(){
              names.push(d3.select(this).select("td:nth-child("+index+")").text());
            })
            .classed("selected",false);
          d3.selectAll(".node").classed("selected",function(d){
            d.selected = names.indexOf(d[options.nodeName]) != -1
            return d.selected;
          });
          showTables();
        }
}

function adaptLayout(){
  var size = Math.min(width,height),
      xdim = d3.extent(Graph.nodes,function(d){ return d.x }),
      ydim = d3.extent(Graph.nodes,function(d){ return d.y });
  size = size/1.2;

  var x = d3.scale.linear()
    .range([(width-size)/2,(width-size)/2+size])
    .domain(xdim);

  var y = d3.scale.linear()
    .range([(height-size)/2,(height-size)/2+size])
    .domain(ydim);

  Graph.nodes.forEach(function(d){
    d.x = x(d.x);
    delete d.px;
    d.y = y(d.y);
    delete d.py;
  });
}

function embedImages(callback){

  var docSize = viewport(),
      loading = body.append("div")
      .style({"position":"fixed","top":0,"left":0,"width":docSize.width+"px","height":docSize.height+"px","background":"rgba(0,0,0,0.8)","z-index":10});

  loading.append("p")
    .style({"color":"#fff","font-size":"20px","font-weight":"bold","margin":"20px"})
    .text(texts.loading)

  if(options.nodeItem && options.nodeItem == 'image' && !images){

    images = {};

    var imgLinks = d3.set(Graph.nodes.map(function(d){ return d.image; })).values();

    var loadImage = function(i){
      var imgSrc = imgLinks[i++];
      var img = new Image();
      img.onload = function() {
          var canvas = document.createElement("canvas");
          canvas.width = this.width;
          canvas.height = this.height;
          var ctx = canvas.getContext("2d");
          ctx.drawImage(this, 0, 0);
          try{
            var uri = canvas.toDataURL();
            images[imgSrc] = [uri,[canvas.width,canvas.height]];
          }finally{
            if(i<imgLinks.length){
              loadImage(i);
            }else{
              callback();
              loading.remove();
            }
          }
      }
      img.src = imgSrc;
    }

    loadImage(0);

  }else{
    callback();
    loading.remove();
  }
}

function svgDownload(){

  var svg = d3.select(".panel>.plot>svg"),
      title = d3.select("head>title").text()+'.svg';
  svg.selectAll(".buttons, .brush").remove();
  var svgString = new XMLSerializer().serializeToString(svg.node());
  svg.remove();
  if(images){
    var zip = new JSZip();
    zip.file(title, svgString);
    var dir = zip.folder("images");
    d3.entries(images).forEach(function(d){
      dir.file(d.key.split("/").pop(),d.value[0].substring(22),{base64: true});
    });
    zip.generateAsync({type:"blob"})
    .then(function(content) {
        fileDownload(content, (title.split(".")[0])+".zip");
    });
  }else{
    var svgBlob = new Blob([svgString], {type: 'image/svg+xml;charset=utf-8'});
    fileDownload(svgBlob, title);
  }
  drawSVG(d3.select(".panel>.plot"));
  drawNet();
}

function svg2pdf(){

  var doc = new jsPDF((width>height)?"l":"p","pt",[width,height]);

  doc.polygon = pdfPolygon;

  doc.setTextColor(68);

  d3.selectAll("div.main").each(function(){
    doc.setFontSize(24);
    doc.setFontType("bold");
    doc.text(12, 28, this.textContent);
  })

  doc.setFontType("normal");

  d3.selectAll("div.minor").each(function(){
    doc.setFontSize(18);
    doc.text(12, height-12, this.textContent);
  })

  doc.setFontSize(12);
  if(d3.select(".scale").style("opacity")!=0){
    if(!d3.select(".scale>rect").empty()) {
      var idGrad = d3.select(".scale rect").attr("fill").replace(/(url\()|(\))/g, "");
      doc.addImage(colorScales[idGrad.replace("#","")].png, 'PNG', (width-320), 20, 300, 10);
      d3.selectAll(".scale>text").each(function(){
        var x = parseInt(d3.select(this).attr("x"))+(width-320),
	    y = parseInt(d3.select(this).attr("y"))+20,
	    t = d3.select(this).text();
	doc.text(x, y, t);
      });
    }

    if(!d3.select(".scale .legend").empty()) {
      d3.selectAll(".scale .legend").each(function(){
        var sel = d3.select(this),
        y = d3.transform(sel.attr("transform")).translate[1]+10;
        doc.setFontSize(16);
        doc.text(width-100, y+10, sel.select(".title").text())
        doc.setFontSize(12);
        sel.selectAll("g").each(function(d,i){
          var el = d3.select(this),
              gy = d3.transform(el.attr("transform")).translate[1],
              x = width-60,
              t = el.select("text").text(),
              txtWidth = doc.getStringUnitWidth(t) * 12;
          if(el.select("image").empty()){
            var color = d3.rgb(el.select("path").style("fill")),
                d = el.select("path").attr("d");
            doc.setFillColor(color.r,color.g,color.b);
            doc.polygon(d,x,y+gy,[1,1],"F");
          }else{
            var imgSrc = el.select("image").attr("href");
            if(images[imgSrc]){
              var imageSize = images[imgSrc][1],
                  imgHeight = imageSize[1]*10/imageSize[0];
              doc.addImage(images[imgSrc][0], 'PNG', x, y+gy-4, 10, imgHeight);
            }
          }
          doc.text(x-txtWidth-10, y+gy+4, t);
        });
      })
    }
  }

  var transform = d3.select(".net").attr("transform"),
      translate = [0,0],
      scale = 1;

  if(transform){
    transform = d3.transform(transform);
    translate = transform.translate;
    scale = transform.scale[0];
  }

  doc.setLineWidth(scale);

  var areas = [];
  d3.selectAll(".area").each(function(){
    var d = {};
      d.colorf = applyOpacity(d3.rgb(d3.select(this).style("fill")),0.2,{r:255,g:255,b:255});
      d.colord = applyOpacity(d3.rgb(d3.select(this).style("stroke")),0.2,{r:255,g:255,b:255});
      d.x = (parseFloat(d3.select(this).attr("x"))*scale)+translate[0];
      d.y = (parseFloat(d3.select(this).attr("y"))*scale)+translate[1];
      d.width = parseFloat(d3.select(this).attr("width"))*scale;
      d.height = parseFloat(d3.select(this).attr("height"))*scale;
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

  d3.selectAll(".link").each(function(){
    var self = d3.select(this),
        color = applyOpacity(d3.rgb(self.attr("stroke")),0.6,{r:255,g:255,b:255}),
        w = self.attr("stroke-width")*scale;
    doc.setDrawColor(color.r,color.g,color.b);
    doc.setLineWidth(w);
    var points = self.attr("d");
    points = points.replace("M","");
    points = points.split("L");
    points[0] = points[0].split(/[,| ]/).filter(function(d){ return d.length>0; });
    points[1] = points[1].split(/[,| ]/).filter(function(d){ return d.length>0; });

    var  x1 = (parseFloat(points[0][0])*scale)+translate[0],
         x2 = (parseFloat(points[1][0])*scale)+translate[0],
         y1 = (parseFloat(points[0][1])*scale)+translate[1],
         y2 = (parseFloat(points[1][1])*scale)+translate[1];

    doc.line(x1, y1, x2, y2);

    if(options.directional){
      doc.setFillColor(color.r,color.g,color.b);
      var hipotenuse = Math.sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)),
        x3 = ((x2-x1)/hipotenuse*8)+x2,
        y3 = ((y2-y1)/hipotenuse*8)+y2,
        x4 = x2-((y3-y2)/2),
        y4 = y2+((x3-x2)/2),
        x5 = x2+((y3-y2)/2),
        y5 = y2-((x3-x2)/2);
      doc.triangle(x5,y5,x3,y3,x4,y4,"F")
    }
  });

  doc.setFontSize(8*scale);
  doc.setTextColor(143);
  d3.selectAll(".linkText").each(function(){
    var x = (parseFloat(d3.select(this).attr("x"))*scale)+translate[0],
        y = (parseFloat(d3.select(this).attr("y"))*scale)+translate[1],
        t = d3.select(this).text();
    doc.text(x, y, t);
  });

  var show = d3.selectAll(".node text").style("opacity")!=0;

  doc.setTextColor(64);
  d3.selectAll(".node").each(function(){
    var item = d3.select(this.childNodes[0]),
        color = d3.rgb(item.attr("fill")),
        sColor = d3.rgb(item.style("stroke")),
        size = d3.transform(item.attr("transform")),
        size = size.scale[0]*scale,
        position = d3.transform(d3.select(this).attr("transform")),
        x = (parseFloat(position.translate[0])*scale)+translate[0],
        y = (parseFloat(position.translate[1])*scale)+translate[1];
    if(d3.select(this).select('image').empty()){
      doc.setLineWidth(parseInt(item.style("stroke-width")));
      doc.setDrawColor(sColor.r,sColor.g,sColor.b);
      doc.setFillColor(color.r,color.g,color.b);
      var points = item.attr("d");
      doc.polygon(points, x, y, [size,size], 'FD');
    }else{
      var imgSrc = item.attr("href");
      if(images[imgSrc]){
        var imageSize = images[imgSrc][1],
            imgHeight = imageSize[1]*10/imageSize[0];
        doc.addImage(images[imgSrc][0], 'PNG', x-5*size, y-(imgHeight/2)*size, 10*size, imgHeight*size);
      }
    }
    if(show)
      doc.text((x+6*size),(y+3), d3.select(this.childNodes[1]).text());
  });

  doc.save(d3.select("head>title").text()+".pdf");
}

} // network function end

if(typeof multiGraph == 'undefined'){
  window.onload = function(){
    network(JSON.parse(d3.select("#data").text()));
  };
}
