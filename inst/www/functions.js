function fileDownload(blob,name){
  if(window.navigator.msSaveBlob){
    window.navigator.msSaveBlob(blob, name);
  }else{
    var url = window.URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.style.display = 'none';
    a.href = url;
    a.download = name;
    document.body.appendChild(a);
    a.click();
    setTimeout(function(){
      document.body.removeChild(a);
      window.URL.revokeObjectURL(url);
    }, 100);
  }
}

function pdfPolygon(path, x, y, scale, style){
    if(path.indexOf("A")!=-1){
      this.circle(x, y, 4.514*scale[0], style);
    }else{
      var closed = path.indexOf("Z")!=-1,
          points = [];
      path = path.replace(/M|Z/g,"").split(/[Lhv]/); 
      for(var i = 0; i<path.length; i++){
        var p = path[i].split(/[,| ]/).filter(function(d){ return d.length>0; }),
        pLen = p.length;
        if(pLen==1){
          points.push(points[points.length-1].map(function(d,j){ return i%2!=0 ^ j ? d+parseInt(p[0]) : d; }));
        }
        if(pLen==2){
          points.push([+p[0],+p[1]]);
        }
        if(pLen>2){
          for(var j = 0; j<pLen; j=j+2){
            points.push([+p[j],+p[j+1]]);
          }
        }
      }

      var acc = [],
        x1 = points[0][0],
        y1 = points[0][1],
        cx = x1,
        cy = y1;
      for(var i=1; i<points.length; i++) {
          var point = points[i],
            dx = point[0]-cx,
            dy = point[1]-cy;
          acc.push([dx, dy]);
          cx += dx;
          cy += dy;
      }
      this.lines(acc, x+(x1*scale[0]), y+(y1*scale[1]), scale, style, closed);
    }
}

function applyOpacity(rgb,alpha,old){
  var blending = function(newC,old){
    return alpha * newC + (1 - alpha) * old;
  }
  return {r: blending(rgb.r,old.r), g: blending(rgb.g,old.g), b: blending(rgb.b,old.b)};
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

function formatter(d){
  if(typeof d == 'number'){
    var dabs = Math.abs(d);
    if(dabs>0 && dabs<1e-2)
      d = d.toExponential(2);
    else
      d = (d % 1 === 0)?d:d.toFixed(2);
  }
  return d;
}

function escapeHtml(unsafe) {
    return unsafe
         .replace(/&/g, "&amp;")
         .replace(/</g, "&lt;")
         .replace(/>/g, "&gt;")
         .replace(/"/g, "&quot;")
         .replace(/'/g, "&#039;");
}

function downloadExcel(data,name){
  var sheets = ["void"],
      contentTypes = [],
      workbook = [],
      workbookRels = [],
      sheetXML = function(dat){
        var xml = [];
        dat.forEach(function(d){
          xml.push('<row>');
          d.forEach(function(dd){
            if(typeof dd == 'number')
              xml.push('<c t="n"><v>'+dd+'</v></c>');
            else
              xml.push('<c t="inlineStr"><is><t>'+escapeHtml(dd)+'</t></is></c>');
          });
          xml.push('</row>');
        });
        return '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006" mc:Ignorable="x14ac" xmlns:x14ac="http://schemas.microsoft.com/office/spreadsheetml/2009/9/ac"><sheetData>'+xml.join('')+'</sheetData></worksheet>';
      }

  for(var d in data)
    sheets.push(d);

  var zip = new JSZip(),
      rels = zip.folder("_rels"),
      xl = zip.folder("xl"),
      xlrels = xl.folder("_rels"),
      xlworksheets = xl.folder("worksheets");

  rels.file(".rels", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/></Relationships>');

  for(var i = 1; i < sheets.length; i++){
    contentTypes.push('<Override PartName="/xl/worksheets/sheet'+i+'.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>');
    workbook.push('<sheet name="'+sheets[i]+'" sheetId="'+i+'" r:id="rId'+i+'"/>');
    workbookRels.push('<Relationship Id="rId'+i+'" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet'+i+'.xml"/>');
    xlworksheets.file("sheet"+i+".xml", sheetXML(data[sheets[i]]));
  }

  zip.file("[Content_Types].xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Default Extension="xml" ContentType="application/xml"/><Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/><Default Extension="jpeg" ContentType="image/jpeg"/><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'+contentTypes.join('')+'</Types>');

  xl.file("workbook.xml", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><fileVersion appName="xl" lastEdited="5" lowestEdited="5" rupBuild="24816"/><workbookPr showInkAnnotation="0" autoCompressPictures="0"/><bookViews><workbookView xWindow="0" yWindow="0" windowWidth="25600" windowHeight="19020" tabRatio="500"/></bookViews><sheets>'+workbook.join('')+'</sheets></workbook>');

  xlrels.file("workbook.xml.rels", '<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'+workbookRels.join('')+'</Relationships>');

  zip.generateAsync({type:"blob", mimeType:"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"})
  .then(function(content) {
      fileDownload(content, name + '.xlsx');
  });
}

var infoIcon_b64 = "PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIGhlaWdodD0iMTQiIHdpZHRoPSIxNCI+PGNpcmNsZSBzdHlsZT0iZmlsbDogcmdiKDI1NSwgMjU1LCAyNTUpOyBzdHJva2U6IHJnYigwLCAwLCAwKTsiIHI9IjYuNSIgY3k9IjciIGN4PSI3Ij48L2NpcmNsZT48Y2lyY2xlIHN0eWxlPSJmaWxsOiByZ2IoMCwgMCwgMCk7IiByPSIxLjQiIGN5PSI0IiBjeD0iNyI+PC9jaXJjbGU+PHBhdGggc3R5bGU9ImZpbGw6IHJnYigwLCAwLCAwKTsiIGQ9Ik0gNSA2IEwgNSA3IEwgNiA3IEwgNiA5LjU5NzY1NjIgTCA1IDkuNTk3NjU2MiBMIDUgMTEgTCA2IDExIEwgOCAxMSBMIDkgMTEgTCA5IDkuNTk3NjU2MiBMIDggOS41OTc2NTYyIEwgOCA2IEwgNSA2IHogIj48L3BhdGg+PC9zdmc+Cg==";

var xlsxIcon_b64 = "PHN2ZyB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgaGVpZ2h0PSIxNCIgd2lkdGg9IjE0IiB2ZXJzaW9uPSIxLjEiIHhtbG5zOmNjPSJodHRwOi8vY3JlYXRpdmVjb21tb25zLm9yZy9ucyMiIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgdmlld0JveD0iMCAwIDE0IDE0Ij4KPGcgdHJhbnNmb3JtPSJ0cmFuc2xhdGUoMCAtMTAzOC40KSI+CjxnPgo8cmVjdCBoZWlnaHQ9IjEwLjQ3MiIgc3Ryb2tlPSIjMjA3MjQ1IiBzdHJva2Utd2lkdGg9Ii41MDIwMSIgZmlsbD0iI2ZmZiIgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIuNTM2OTYiIHdpZHRoPSI3Ljg2NDYiIHk9IjEwNDAiIHg9IjUuODc4OCIvPgo8ZyBmaWxsPSIjMjA3MjQ1Ij4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0MS4yIiB4PSIxMC4xNjUiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0Mi45IiB4PSIxMC4xNjUiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0NC43IiB4PSIxMC4xNjUiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0Ni40IiB4PSIxMC4xNjUiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0OC4yIiB4PSIxMC4xNjUiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0MS4yIiB4PSI3LjI0NzgiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0Mi45IiB4PSI3LjI0NzgiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0NC43IiB4PSI3LjI0NzgiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0Ni40IiB4PSI3LjI0NzgiLz4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIwIiBoZWlnaHQ9IjEuMDYwNyIgd2lkdGg9IjIuMjA5NyIgeT0iMTA0OC4yIiB4PSI3LjI0NzgiLz4KPHBhdGggc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIGQ9Im0wIDEwMzkuNyA4LjIzMDEtMS4zN3YxNGwtOC4yMzAxLTEuNHoiLz4KPC9nPgo8L2c+CjxnIGZpbGw9IiNmZmYiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEuMzI1OCAuMDYyNSAtMzM5LjcyKSI+CjxwYXRoIGQ9Im00LjQwNiAxMDQ0LjZsMS4zNzUzIDIuMDU2OC0xLjA3MjUtMC4wNjEtMC44OTAzLTEuMzU2LTAuODQ1NjYgMS4yNTc4LTAuOTQxNTYtMC4wNTMgMS4yMTg3LTEuODU0NC0xLjE3My0xLjgwMDggMC45NDE0MS0wLjAzNSAwLjgwMDE0IDEuMjAxMSAwLjgzMDQzLTEuMjYyNiAxLjA3NzUtMC4wNDFzLTEuMzIwNSAxLjk0ODItMS4zMjA1IDEuOTQ4MiIgZmlsbD0iI2ZmZiIvPgo8L2c+CjwvZz4KPC9zdmc+Cg==";

var svgIcon_b64 = "PHN2ZyB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgaGVpZ2h0PSIxNCIgd2lkdGg9IjE0IiB2ZXJzaW9uPSIxLjEiIHhtbG5zOmNjPSJodHRwOi8vY3JlYXRpdmVjb21tb25zLm9yZy9ucyMiIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyIgdmlld0JveD0iMCAwIDE0IDE0Ij4KPHJlY3Qgc3R5bGU9ImNvbG9yLXJlbmRlcmluZzphdXRvO2NvbG9yOiMwMDAwMDA7aXNvbGF0aW9uOmF1dG87bWl4LWJsZW5kLW1vZGU6bm9ybWFsO3NoYXBlLXJlbmRlcmluZzphdXRvO3NvbGlkLWNvbG9yOiMwMDAwMDA7aW1hZ2UtcmVuZGVyaW5nOmF1dG8iIHJ5PSIyLjYzNDciIGhlaWdodD0iMTMuNTE3IiB3aWR0aD0iMTMuNTE3IiBzdHJva2U9IiNkZWE4NTMiIHk9Ii4yNDEzOCIgeD0iLjI0MTM4IiBzdHJva2Utd2lkdGg9Ii40ODI3NiIgZmlsbD0iI2ZjZjNkYiIvPgo8ZyB0cmFuc2Zvcm09Im1hdHJpeCguNzY1NzQgLjY0MzE1IC0uNjQzMTUgLjc2NTc0IDMuNjI1OSAuMDEwNCkiPgo8cGF0aCBvcGFjaXR5PSIuOTkiIHN0eWxlPSJjb2xvci1yZW5kZXJpbmc6YXV0bztjb2xvcjojMDAwMDAwO2lzb2xhdGlvbjphdXRvO21peC1ibGVuZC1tb2RlOm5vcm1hbDtzaGFwZS1yZW5kZXJpbmc6YXV0bztzb2xpZC1jb2xvcjojMDAwMDAwO2ltYWdlLXJlbmRlcmluZzphdXRvIiBkPSJtMi4yMjQ4IDYuMDQwMmMwLTIuNjUzOCAyLjE1MTMtNC44MDUxIDQuODA1MS00LjgwNTEgMi42NTM4IDNlLTcgNC44MDUxIDIuMTUxMyA0LjgwNTEgNC44MDUxIiBzdHJva2U9IiNhMDYyMDAiIHN0cm9rZS13aWR0aD0iLjciIGZpbGw9IiNmZmQ1NmYiLz4KPHBhdGggb3BhY2l0eT0iLjk4IiBkPSJtMS4zMDUzIDEuMjM1MWgxMS40NDkiIHN0cm9rZT0iI2EwNjIwMCIgc3Ryb2tlLXdpZHRoPSIuNyIgZmlsbD0ibm9uZSIvPgo8cmVjdCBzdHlsZT0iY29sb3ItcmVuZGVyaW5nOmF1dG87Y29sb3I6IzAwMDAwMDtpc29sYXRpb246YXV0bzttaXgtYmxlbmQtbW9kZTpub3JtYWw7c2hhcGUtcmVuZGVyaW5nOmF1dG87c29saWQtY29sb3I6IzAwMDAwMDtpbWFnZS1yZW5kZXJpbmc6YXV0byIgdHJhbnNmb3JtPSJyb3RhdGUoLTQ1KSIgaGVpZ2h0PSIxLjU5MTgiIHdpZHRoPSIxLjU5MTgiIHN0cm9rZT0iIzY5NGMwZiIgeT0iNS4wNDgzIiB4PSIzLjMwMTYiIHN0cm9rZS13aWR0aD0iLjUiIGZpbGw9Im5vbmUiLz4KPC9nPgo8L3N2Zz4K";

var pdfIcon_b64 = "PHN2ZyB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgaGVpZ2h0PSIxNCIgd2lkdGg9IjE0IiB2ZXJzaW9uPSIxLjEiIHhtbG5zOmNjPSJodHRwOi8vY3JlYXRpdmVjb21tb25zLm9yZy9ucyMiIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB2aWV3Qm94PSIwIDAgMTQgMTQiIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyI+CjxkZWZzPgo8Y2xpcFBhdGggaWQ9ImNsaXBQYXRoNDE4OSIgY2xpcFBhdGhVbml0cz0idXNlclNwYWNlT25Vc2UiPgo8cGF0aCBzdHlsZT0iY29sb3ItcmVuZGVyaW5nOmF1dG87Y29sb3I6IzAwMDAwMDtpc29sYXRpb246YXV0bzttaXgtYmxlbmQtbW9kZTpub3JtYWw7c2hhcGUtcmVuZGVyaW5nOmF1dG87c29saWQtY29sb3I6IzAwMDAwMDtpbWFnZS1yZW5kZXJpbmc6YXV0byIgZD0ibTkuMDM4NiAwLjAwMDNoLTYuMjMyNWMtMC4zMzQ4IDAtMC42MDU4IDAuMjY4OS0wLjYwNTggMC42MDM4djEyLjc5MmMwIDAuMzM0OCAwLjI3MTAxIDAuNjAzOCAwLjYwNTg0IDAuNjAzOGg4LjM4NzdjMC4zMzQ4MyAwIDAuNjA1ODQtMC4yNjkgMC42MDU4NC0wLjYwMzh2LTEwLjYzNWMtMC45MjEtMC45MTk4LTEuODQxOS0xLjg0MDQtMi43NjE3LTIuNzYwOXoiIGZpbGw9IiNkMzFlMWUiLz4KPC9jbGlwUGF0aD4KPGxpbmVhckdyYWRpZW50IGlkPSJsaW5lYXJHcmFkaWVudDQxNjkiIHkyPSItLjM1NTkzIiBncmFkaWVudFVuaXRzPSJ1c2VyU3BhY2VPblVzZSIgeDI9IjUuNTExNSIgeTE9IjE0Ljc3MSIgeDE9IjkuMDgxNyI+CjxzdG9wIHN0b3AtY29sb3I9IiM5NzE2MTYiIG9mZnNldD0iMCIvPgo8c3RvcCBzdG9wLWNvbG9yPSIjZDUxYjFiIiBvZmZzZXQ9Ii40NDkxNCIvPgo8c3RvcCBzdG9wLWNvbG9yPSIjZTg1YzVjIiBvZmZzZXQ9IjEiLz4KPC9saW5lYXJHcmFkaWVudD4KPC9kZWZzPgo8cGF0aCBzdHlsZT0iY29sb3ItcmVuZGVyaW5nOmF1dG87Y29sb3I6IzAwMDAwMDtpc29sYXRpb246YXV0bzttaXgtYmxlbmQtbW9kZTpub3JtYWw7c2hhcGUtcmVuZGVyaW5nOmF1dG87c29saWQtY29sb3I6IzAwMDAwMDtpbWFnZS1yZW5kZXJpbmc6YXV0byIgZD0ibTkuMDM4NiAwLjAwMDI5OThoLTYuMjMyNWMtMC4zMzQ4IDAtMC42MDU4IDAuMjY4OS0wLjYwNTggMC42MDM4djEyLjc5MmMwIDAuMzM0OCAwLjI3MTAxIDAuNjAzOCAwLjYwNTg0IDAuNjAzOGg4LjM4NzdjMC4zMzQ4MyAwIDAuNjA1ODQtMC4yNjkgMC42MDU4NC0wLjYwMzh2LTEwLjYzNWMtMC45MjEtMC45MTk5LTEuODQxNi0xLjg0MDUtMi43NjE0LTIuNzYxeiIgZmlsbD0idXJsKCNsaW5lYXJHcmFkaWVudDQxNjkpIi8+CjxwYXRoIHN0eWxlPSJjb2xvci1yZW5kZXJpbmc6YXV0bztjb2xvcjojMDAwMDAwO2lzb2xhdGlvbjphdXRvO21peC1ibGVuZC1tb2RlOm5vcm1hbDtzaGFwZS1yZW5kZXJpbmc6YXV0bztzb2xpZC1jb2xvcjojMDAwMDAwO2ltYWdlLXJlbmRlcmluZzphdXRvIiBkPSJtMTEuNjM5IDIuOTEzMy0yLjc2MTItMi43NjExdjIuMTU3M2MwIDAuMzM0OSAwLjI3MTAxIDAuNjAzOCAwLjYwNTg0IDAuNjAzOGgyLjE1NTJ6IiBmaWxsPSIjOWUxNjE2Ii8+CjxwYXRoIGZpbGw9IiNmZmYiIGQ9Im0xLjk1OTMgMTMuNTc5YzAuNzg1NC0wLjM2OSAxLjc1NTQtMS41IDIuOTUyNS0zLjQ0NSAwLjE3NTgtMC4yODUzIDEuMTUyMS0wLjYyNDEgMi45NTM2LTEuMDI0OGwxLjA3NjEtMC4yMzkzIDAuNTc2NyAwLjQyNWMxLjgyNzggMS4zNDcxIDMuOTYzOCAxLjY1MzEgNC4zOTE4IDAuNjI4NCAwLjEyNS0wLjI5OTQgMC4xMjMtMC4zNTU3LTAuMDI2LTAuNjU0Ny0wLjIxMi0wLjQyNy0wLjQ5Ni0wLjYyMDMtMS4xNTYtMC43ODk3LTAuNjI4LTAuMTYwNy0yLjE3Mi0wLjE4MjUtMy4wMzI4LTAuMDQyOGwtMC41ODA5IDAuMDkzOC0wLjM1NTItMC4zNDQyYy0wLjM5LTAuMzc4MS0xLjEzNzQtMS4zNTIyLTEuNTk0OC0yLjA3OWwtMC4yOTI5LTAuNDY1NCAwLjI2NC0wLjk0MDNjMC4zMzE0LTEuMTggMC41MTM2LTIuNDQzNiAwLjQzMTQtMi45OTA0LTAuMTU4NC0xLjA1MjgtMC43OTY3LTEuNjYyNC0xLjUxNTEtMS40NDcyLTAuNDIwOCAwLjEyNjA1LTAuNTgzNSAwLjM0NzIyLTAuNzE4MyAwLjk3NjItMC4xMzU3IDAuNjMyNSAwLjAwNzEgMS42MDc4IDAuNDEyMSAyLjgxODVsMC4zMTcgMC45NDc0LTAuMzI3NCAwLjkxMDJjLTAuNDIxNyAxLjE3MjctMS4xNTEgMi44NzQ4LTEuNTA0NiAzLjUxMTctMC4yNzE3IDAuNDg5My0wLjMwMTYgMC41MTEyLTEuNTEyNSAxLjEwNjYtMS4zNjk0IDAuNjczNDktMi4yNzI0IDEuMzQxNC0yLjU1MDQgMS44ODY0LTAuMjUyMTIgMC40OTQyLTAuMjI3NjkgMC42OTU3NCAwLjEyNzM3IDEuMDUwOCAwLjI3MDEzIDAuMjcwMTMgMC4zNjU1MiAwLjMwODI0IDAuNzcxNTkgMC4zMDgyNCAwLjI4OTU2IDAgMC42MjQ0OS0wLjA3NTU3IDAuODkzLTAuMjAxNTR6bS0xLjQ2MDItMC4zMjdjLTAuNTAzOTMtMC41NTY4MyAwLjMxMzM3LTEuNDIxOCAyLjI3MzMtMi40MDU4IDAuNDYwMjMtMC4yMzEwNiAwLjg1OTA2LTAuNDIwMTMgMC44ODYyNi0wLjQyMDEzIDAuMDg3MjQyIDAtMS4wMDcgMS41NzA2LTEuNDE2MSAyLjAzMjctMC40NDc3OCAwLjUwNTc0LTEuMTA4OCAwLjk1OS0xLjM5ODUgMC45NTktMC4xMDcxNiAwLTAuMjYyMzYtMC4wNzQ1OS0wLjM0NDktMC4xNjU3OXptMTAuODctMy4yOTY4Yy0wLjUzMi0wLjE4ODQtMS4zMzgtMC42Mjk5LTEuNzY4NS0wLjk2OTdsLTAuMjIyMy0wLjE3NTMgMC40NDg5LTAuMDc2NGMwLjI0NzAyLTAuMDQxOTM5IDAuOTE4MjctMC4wNzQ2ODcgMS40OTE3LTAuMDcyNTYxIDAuODA4NTIgMC4wMDI2MyAxLjEwODQgMC4wMzg5MjMgMS4zMzU5IDAuMTYwMyAwLjQ5ODk1IDAuMjY2MjggMC41MTQzMyAwLjk0MjI1IDAuMDI3MTcgMS4xOTQyLTAuMzUzODMgMC4xODI5OC0wLjY2NDg2IDAuMTY4NjMtMS4zMTI1LTAuMDYwNTE1em0tNS43NTgzLTEuMTY4MWMwLjE5NTctMC4zODkyIDAuNTIzNC0xLjE0ODcgMC43MjgzLTEuNjg3OCAwLjIwNDgtMC41MzkxIDAuMzg0Ni0wLjk5MjUgMC4zOTk0LTEuMDA3NnMwLjEyMDkgMC4xMjc3IDAuMjM1NyAwLjMxNzNjMC4zMTIxIDAuNTE1NCAwLjk3MzEgMS4zODQ4IDEuMzY4NSAxLjhsMC4zNSAwLjM2NzUtMC45MzA2IDAuMjEyNWMtMC41MTE5IDAuMTE2OS0xLjI4NTMgMC4zMjM1LTEuNzE4OSAwLjQ1OTFsLTAuNzg4MiAwLjI0NjZ6bTAuMzc1OC00Ljk2ODFjLTAuMzIwOS0wLjk5NjMtMC40MzQ0LTEuOTc4NS0wLjI5ODctMi41ODQ5IDAuMTE2My0wLjUxOTU0IDAuMjczMi0wLjY5MDUzIDAuNjMzOC0wLjY5MDUzIDAuNTE4OTkgMCAwLjYxODA0IDEuMTMyNyAwLjIzNjY2IDIuNzA3LTAuMTI1MSAwLjUxNjQtMC4yNTk2IDAuOTk2NC0wLjI5ODkgMS4wNjY2LTAuMDQ4IDAuMDg1Ni0wLjEzNzctMC4wNzgyLTAuMjcyOS0wLjQ5ODF6IiBjbGlwLXBhdGg9InVybCgjY2xpcFBhdGg0MTg5KSIvPgo8L3N2Zz4K";

function iconButton(sel,alt,src,title,job){
    sel.append("img")
      .attr("class","icon")
      .attr("alt", alt)
      .attr("width", 14)
      .attr("height", 14)
      .attr("src", "data:image/svg+xml;base64,"+src)
      .attr("title", title)
      .on("click", job);
}

function displayWindow(txt){
  var docSize = viewport(),
      bg = d3.select("body").append("div")
        .style("position","fixed")
        .style("top",0)
        .style("left",0)
        .style("width",docSize.width+"px")
        .style("height",docSize.height+"px")
        .style("background","rgba(0,0,0,0.8)")
        .style("z-index",10);

  var boxStyle = {"background-color":"#fff","margin":"0 auto","margin-top":(docSize.height/5)+"px","padding":"30px","width":(docSize.width/2)+"px"};

  if(txt){
    boxStyle["font-size"] = "16px";
    boxStyle["text-align"] = "center";
  }else{
    boxStyle["height"] = (docSize.height/2)+"px";
    boxStyle["overflow-y"] = "auto";
  }

  var win = bg.append("div")
    .attr("class","window")
    .on("click",function(){ d3.event.stopPropagation(); })

  for(var s in boxStyle)
    win.style(s,boxStyle[s]);

  bg.append("div")
    .attr("class","close-button")
    .style("position","absolute")
    .style("top",(12+docSize.height/5)+"px")
    .style("right",(docSize.width/4)+"px")
    .html("&#x2716;")
    .on("click", function(){ bg.remove() });

  if(txt)
    win.text(txt);
  else
    return win;
}

function brushSlider(sel,domain,current,callback,baseWidth){
  var cex = parseInt(sel.style("font-size"))/10,
      margin = {top: 21 + 15*cex, right: 17, bottom: 0, left: 17},
      width = baseWidth - margin.left - margin.right,
      height = 21;

  if(!current)
    current = domain;

  var x = d3.scaleLinear()
      .range([0, width])
      .domain(domain)
      .clamp(true);

  sel.style("height", height+margin.top+margin.bottom + "px");
  
  var slider = sel.append("div")
    .attr("class", "slider")
    .style("width", width + "px")
    //.style("height", height + "px")
    .style("position", "relative")
    .style("top", margin.top+"px")
    .style("left", margin.left+"px");
    
  var sliderTray = slider.append("div")
    .attr("class", "slider-tray");
    
  var sliderExtent = slider.append("span")
    .attr("class","slider-extent")
    .style("width",(x(current[1])-x(current[0]))+"px")
    .style("left",x(current[0])+"px")

  slider.append("span")
    .attr("class","slider-min")
    .style("left", -5*cex+"px")
    .style("top", -20*cex + "px")
    .text(formatter(domain[0]))

  slider.append("span")
    .attr("class","slider-max")
    .style("left", width-5*cex+"px")
    .style("top", -20*cex + "px")
    .text(formatter(domain[1]))

  var sliderHandle = slider.selectAll(".slider-handle")
    .data(current)
      .enter().append("div")
    .attr("class", "slider-handle")
    .style("position","absolute")
    .style("top", "3px")
    .style("left", function(d){ return x(d)+"px"; });
    
  sliderHandle.append("div")
    .attr("class", "slider-handle-icon")

  sliderHandle.append("span")
    .attr("class","slider-text")
    .style("top", -22*cex + "px")
    .style("left", -4*cex+"px")
    .text(function(d){ return formatter(d); })

  sliderHandle.each(function(d, i){
    var self = d3.select(this);
    self.call(d3.drag()
    .on("drag", function() {
      var value = x.invert(d3.mouse(sliderTray.node())[0]);
      self.style("left", x(value) + "px");
      self.select(".slider-text").text(formatter(value));
      current[i] = value;
      var extent = d3.extent(current);
      sliderExtent.style("width", (x(extent[1])-x(extent[0]))+"px")
        .style("left",x(extent[0])+"px");
      callback(extent);
    }));
  })
}

function dataType(data,key){
  var type = d3.map(data.filter(function(d){ return d[key] !== null; }),function(d){ return typeof d[key]; }).keys();
  if(type.length == 1)
    return type[0];
  else
    return 'undefined';    
}

function selectedValues2str(selectedValues,data){
  var query = "(true";
  d3.keys(selectedValues).forEach(function(k){
    query = query + ") && (false";
    if(typeof selectedValues[k][0] == 'number' && selectedValues[k].length == 2){
      query = query + " || ((d['" + k + "'] >= " + selectedValues[k][0] + ") && (d['" + k + "'] <= " + selectedValues[k][1] + "))";
    }else{
      var type = dataType(data,k);
      if(type == 'string')
        selectedValues[k].forEach(function(p){
          query = query + " || (d['" + k + "'] == " + (p=='null' ? p : "'" + p + "'") + ")";
        })
      if(type == 'object')
        selectedValues[k].forEach(function(p){
          query = query + " || (d['" + k + "'] && d['" + k + "'].indexOf('" + p + "')!=-1)";
        })
    }
  })
  query = query + ")";
  return query;
}

function topFilter(){

  var data,
      attr,
      displayGraph,
      selectedValues = {},
      selFilter;

  function topFilter(topBar){

    topBar.append("h3").text(texts.filter + ":")

    var changeAttrSel = function(val){
      if(d3.select("body>div>div.window").empty()){

        var panel = displayWindow(),
            vp = viewport();

        panel.append("h3").text(val).style("margin-bottom","10px")

        var type = dataType(data.filter(function(d){ return d[val] !== null; }),val);
        if(type == 'number'){
          var extent = d3.extent(data, function(d){ return d[val]; }),
              tempValues;
          brushSlider(panel.append("div"),extent,selectedValues[val],function(s){ tempValues = s; },vp.width/3);
        }else{
          var dat = data.map(function(d){ return d[val]; });
          if(type != 'string')
            dat = dat.reduce(function(a,b) { return b ? a.concat(b) : a; }, []);
          dat = d3.set(dat).values().sort();

          var valSelector = panel.append("select")
            .attr("multiple","multiple")
            .attr("size",dat.length)
            .style("width",vp.width/3+"px");

          valSelector.selectAll("option").data(dat)
            .enter().append("option")
              .property("value",String)
              .property("selected",function(d){ return (selectedValues[val] && selectedValues[val].indexOf(d)!=-1); })
              .text(String);
        }

        panel.append("button")
          .text(texts.apply)
          .style("position","fixed")
          .style("left",((2*vp.width/3)-80)+"px")
          .style("top",(2*vp.height/3)+"px")
          .on("click",function(){
            selectedValues = {};
            add2filter();
            applyfilter();
          })

        panel.append("button")
          .text(texts.add)
          .style("position","fixed")
          .style("left",(2*vp.width/3)+"px")
          .style("top",(2*vp.height/3)+"px")
          .on("click",add2filter)
      }

      function add2filter(){
            selectedValues[val] = [];
            if(typeof tempValues != 'undefined'){
              selectedValues[val] = tempValues;
            }else{
              valSelector.selectAll("option").each(function(){
                if(this.selected)
                  selectedValues[val].push(this.value);
              })
            }
            if(selectedValues[val].length == 0)
              delete selectedValues[val];
            d3.select(panel.node().parentNode).remove();
      }
    }

    selFilter = topBar.append("select")
      .on("change",function(){ changeAttrSel(this.value); })

    var options = d3.keys(data[0]).sort();
    options.unshift("-"+texts.none+"-");
    selFilter.selectAll("option")
        .data(options)
      .enter().append("option")
        .property("disabled",function(d,i){ return !i; })
        .property("value",String)
        .text(String)

    topBar.append("button")
      .text(texts.apply)
      .on("click",applyfilter)

    topBar.append("button")
      .text(texts.removefilter)
      .on("click",removeFilter)
  }

  function applyfilter(){
      var query = selectedValues2str(selectedValues,data);
      var names = data.filter(function(d){ return eval(query); }).map(function(d){ return d[attr]; });
      displayGraph(names);
  }

  function removeFilter(){
      selFilter.node().selectedIndex = 0;
      selectedValues = {};
      displayGraph(false);
  }

  topFilter.removeFilter = function(){
    removeFilter();
  }

  topFilter.data = function(x) {
    if (!arguments.length) return data;
    data = x;
    return topFilter;
  };

  topFilter.attr = function(x) {
    if (!arguments.length) return attr;
    attr = x;
    return topFilter;
  };

  topFilter.displayGraph = function(x) {
    if (!arguments.length) return displayGraph;
    displayGraph = x;
    return topFilter;
  };

  return topFilter;
}

function tooltip(sel,text){
    var body = d3.select("body"),
        tip = body.select("div.tooltip");

    if(tip.empty())
      tip = body.append("div")
          .attr("class","tooltip")

    sel
      .on("mouseenter", function(d){
        if(d[text])
          tip.style("display","block").html(d[text]);
      })
      .on("mousemove", function(){
        var coor = [0, 0];
        coor = d3.mouse(body.node());
        tip.style("top",(coor[1]+20)+"px")
           .style("left",(coor[0]+20)+"px")
      })
      .on("mouseleave", function(){
        tip.style("display","none").html("")
      })
}

function sortAsc(a,b){
  if(!isNaN(+a) && !isNaN(+b)){
    a = +a;
    b = +b;
  }
  return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
}

function getTranslation(transform) {
  var g = document.createElementNS("http://www.w3.org/2000/svg", "g");
  
  g.setAttributeNS(null, "transform", transform);
 
  var matrix = g.transform.baseVal.consolidate().matrix;
  
  return [matrix.e, matrix.f];
}

function CanvasRecorder(canvas, video_bits_per_sec) {
    this.start = startRecording;
    this.stop = stopRecording;
    this.save = download;

    var recordedBlobs = [];
    var supportedType = null;
    var mediaRecorder = null;

    try {
      var stream = canvas.captureStream();
    } catch (e) {
      alert("canvas.captureStream() is not supported by this browser.");
    }
    if (typeof stream == undefined || !stream) {
      this.start = null;
      return;
    }

    function startRecording() {
        if(typeof MediaRecorder == "undefined"){
          alert('MediaRecorder is not supported by this browser.');
          return false;
        }

        var types = [
            "video/webm;codecs=vp9",
            "video/webm;codecs=vp8",
            "video/webm;codecs=daala",
            "video/webm;codecs=h264",
            "video/webm",
            "video/vp8",
            "video/mpeg"
        ];

        for (var i in types) {
            if (MediaRecorder.isTypeSupported(types[i])) {
                supportedType = types[i];
                break;
            }
        }
        if (supportedType == null) {
            alert("No supported type found for MediaRecorder");
            return false;
        }
        var options = { 
            mimeType: supportedType,
            videoBitsPerSecond: video_bits_per_sec || 2500000 // 2.5Mbps
        };

        recordedBlobs = [];
        try {
            mediaRecorder = new MediaRecorder(stream, options);
        } catch (e) {
            alert('MediaRecorder is not supported by this browser.');
            console.error('Exception while creating MediaRecorder:', e);
            return false;
        }

        mediaRecorder.ondataavailable = handleDataAvailable;
        mediaRecorder.start(100); // collect 100ms of data blobs
        return true;
    }

    function handleDataAvailable(event) {
        if (event.data && event.data.size > 0) {
            recordedBlobs.push(event.data);
        }
    }

    function stopRecording() {
        mediaRecorder.stop();
    }

    function download(file_name) {
        var name = file_name || 'recording.webm';
        var blob = new Blob(recordedBlobs, { type: supportedType });
        fileDownload(blob,name);
    }
}
