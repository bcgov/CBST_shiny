##Kiri Daust
##Javascript for tileserver functionality
##Some of this code is adapted from Bruno Tremblay

bgc_tileserver <- "https://tileserver.thebeczone.ca/data/BC_BGC/{z}/{x}/{y}.pbf"
bgc_tilelayer <- "BECMap"
subzones_colours_ref <- fread("input_data/WNA_v12_HexCols.csv")##colours for each BGC

plugins <- {list(vgplugin =
                   htmltools::htmlDependency(
                     name = "leaflet.vectorgrid",
                     version = "1.3.0",
                     src = "htmlwidgets",
                     script = "lfx-vgrid-prod.js"
                   ),
                 sliderplugin = htmltools::htmlDependency(
                   name = "leaflet.slider",
                   version = "1.0.0",
                   stylesheet = "lfx-slider.css",
                   src = "htmlwidgets",
                   script = "lfx-slider.js"
                 )
)
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

##this is the function with the main javascript
add_bgc_map <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$BGC, "':'", subzones_colours_ref$Col,"'", collapse = ","), "}"), '

      L.bec_layer_opacity2 = 0.25

      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {//plot tiles
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: L.bec_layer_opacity2
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
              return f.properties[id];
          }
        }

      };

      var subzLayer = L.vectorGrid.protobuf(
        "', bgc_tileserver, '",
        vectorTileOptions("bec_subz", "', bgc_tilelayer, '", true,
                          "tilePane", subzoneColors, "BGC", "BGC")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "BEC");

      //highlight on click
      var styleHL = {
            weight: 1.5,
            color: "#fc036f",
            fillColor: "#FFFB00",
            fillOpacity: 1,
            fill: true
          };
      var selectHighlight = ["SBSdk","SBSmc2"];
      subzLayer.on("click", function(e){
        console.log("click");
        selectHighlight.forEach((ID,i) => {
          subzLayer.resetFeatureStyle(ID);
        });
        Shiny.setInputValue("becselect_click",e.layer.properties.BGC);//send bgc name to shiny
        var properties = e.layer.properties
  			  highlight = properties.BGC
          subzLayer.setFeatureStyle(properties.BGC, styleHL);
      });

      Shiny.addCustomMessageHandler("highlightBEC",function(BECSelect){//recieve message from shiny, highlight
        console.log(BECSelect);
        if(!Array.isArray(BECSelect)){
          BECSelect = [BECSelect];
        }
        if(selectHighlight){
          selectHighlight.forEach((ID,i) => {
            subzLayer.resetFeatureStyle(ID);
          });
          selectHighlight = BECSelect;
          BECSelect.forEach((ID,i) => {
            subzLayer.setFeatureStyle(ID, styleHL);
          });
          Shiny.setInputValue("becselect_click",BECSelect);
        }
      });

      var cols;
      var bgc;
      var opa;
      Shiny.addCustomMessageHandler("colour_bec",function(col_map){//colour BGCs and change opacity
        //console.log(col_map);
        cols = col_map.Col;
        bgc = col_map.BGC;
        opa = col_map.Opacity;
        selectHighlight.forEach((ID,i) => {
          subzLayer.resetFeatureStyle(ID);
        });
        selectHighlight = bgc;
        bgc.forEach((ID,i) => {
         var styleHL = {
            weight: 0.5,
            color: cols[i],
            fillColor: cols[i],
            fillOpacity: opa[i],
            fill: true
          };
          subzLayer.setFeatureStyle(ID, styleHL);
        });

      });


		  var clearHighlight = function() {
		    //console.log(selectHighlight);
		    selectHighlight.forEach((ID,i) => {
          subzLayer.resetFeatureStyle(ID);
        });

		  }

      Shiny.addCustomMessageHandler("clearBEC",function(x){//clear everythin
          selectHighlight.forEach((ID,i) => {
            subzLayer.resetFeatureStyle(ID);
          });
      });

      subzLayer.bindTooltip(function(e) {
        return e.properties.BGC
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();

      updateOpacity = function(value) {
        L.bec_layer_opacity2 = parseFloat(value);
      }

      var opacityslider2 = L.control.slider(updateOpacity, {
        id:"opacity_slider2",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="opacity.svg" />\',
        max:1,
        step:0.01,
        syncSlider:true,
        size:"250px",
        title: "Adjust BGC Opacity",
        // Starting opacity value for bec map layers
        value:0.25,
        showValue:true
      })
      opacityslider2.addTo(this);
    }'
  ))
  map
}
