
// polyfill indexOf for IE8
if (!Array.prototype.indexOf) {
  Array.prototype.indexOf = function(elt /*, from*/) {
    var len = this.length >>> 0;

    var from = Number(arguments[1]) || 0;
    from = (from < 0)
         ? Math.ceil(from)
         : Math.floor(from);
    if (from < 0)
      from += len;

    for (; from < len; from++) {
      if (from in this &&
          this[from] === elt)
        return from;
    }
    return -1;
  };
}

HTMLWidgets.widget({

  name: "dygraphs",

  type: "output",

  initialize: function(el, width, height) { 
    
    // add qt style if we are running under Qt
    if (window.navigator.userAgent.indexOf(" Qt/") > 0)
      el.className += " qt";
    
    return {};
  },

  resize: function(el, width, height, instance) {
    if (instance.dygraph)
      instance.dygraph.resize();
  },

  renderValue: function(el, x, instance) {
    
    // reference to this for closures
    var thiz = this;
    
    // get dygraph attrs and populate file field
    var attrs = x.attrs;
    attrs.file = x.data;
    
    // convert non-arrays to arrays
    for (var index = 0; index < attrs.file.length; index++) {
      if (!$.isArray(attrs.file[index]))
        attrs.file[index] = [].concat(attrs.file[index]);
    }
        
    // resolve "auto" legend behavior
    if (x.attrs.legend == "auto") {
      if (x.data.length <= 2)
        x.attrs.legend = "onmouseover";
      else
        x.attrs.legend = "always";
    }
    
    if (x.format == "date") {
      
      // set appropriated function in case of fixed tz
      if ((attrs.axes.x.axisLabelFormatter === undefined) && x.fixedtz)
        attrs.axes.x.axisLabelFormatter = this.xAxisLabelFormatterFixedTZ(x.tzone);
        
      if ((attrs.axes.x.valueFormatter === undefined) && x.fixedtz)
        attrs.axes.x.valueFormatter = this.xValueFormatterFixedTZ(x.scale, x.tzone);
  
      if ((attrs.axes.x.ticker === undefined) && x.fixedtz)
        attrs.axes.x.ticker = this.customDateTickerFixedTZ(x.tzone);
    
      // provide an automatic x value formatter if none is already specified
      if ((attrs.axes.x.valueFormatter === undefined) && (x.fixedtz != true))
        attrs.axes.x.valueFormatter = this.xValueFormatter(x.scale);
      
      // convert time to js time
      attrs.file[0] = attrs.file[0].map(function(value) {
        return thiz.normalizeDateValue(x.scale, value, x.fixedtz);
      });
      if (attrs.dateWindow != null) {
        attrs.dateWindow = attrs.dateWindow.map(function(value) {
          var date = thiz.normalizeDateValue(x.scale, value, x.fixedtz);
          return date.getTime();
        });
      }
    }
    
    
    // transpose array
    attrs.file = HTMLWidgets.transposeArray2D(attrs.file);
    
    // add drawCallback for group
    if (x.group != null)
      this.addGroupDrawCallback(x);  
      
    // add shading and event callback if necessary
    this.addShadingCallback(x);
    this.addEventCallback(x);
    this.addZoomCallback(x, instance);
    
    // disable y-axis touch events on mobile phones
    if (attrs.mobileDisableYTouch !== false && this.isMobilePhone()) {
      // create default interaction model if necessary
      if (!attrs.interactionModel)
        attrs.interactionModel = Dygraph.Interaction.defaultModel;
      // disable y touch direction
      attrs.interactionModel.touchstart = function(event, dygraph, context) {
        Dygraph.defaultInteractionModel.touchstart(event, dygraph, context);
        context.touchDirections = { x: true, y: false };
      };
    }

    // if there is no existing instance perform one-time initialization
    if (!instance.dygraph) {
      
      // subscribe to custom shown event (fired by ioslides to trigger
      // shiny reactivity but we can use it as well). this is necessary
      // because if a dygraph starts out as display:none it has height
      // and width == 0 and this doesn't change when it becomes visible
      $(el).closest('slide').on('shown', function() {
        if (instance.dygraph)
          instance.dygraph.resize();  
      });
      
      // do the same for reveal.js
      $(el).closest('section.slide').on('shown', function() {
        if (instance.dygraph)
          instance.dygraph.resize();  
      });
      
      // redraw on R Markdown {.tabset} tab visibility changed
      var tab = $(el).closest('div.tab-pane');
      if (tab !== null) {
        var tabID = tab.attr('id');
        var tabAnchor = $('a[data-toggle="tab"][href="#' + tabID + '"]');
        if (tabAnchor !== null) {
          tabAnchor.on('shown.bs.tab', function() {
            if (instance.dygraph)
              instance.dygraph.resize();  
          });
        }
      }
      // add default font for viewer mode
      if (this.queryVar("viewer_pane") === "1")
        document.body.style.fontFamily = "Arial, sans-serif";

      // inject css if necessary
      if (x.css != null) {
        var style = document.createElement('style');
        style.type = 'text/css';
        if (style.styleSheet) 
          style.styleSheet.cssText = x.css;
        else 
          style.appendChild(document.createTextNode(x.css));
        document.getElementsByTagName("head")[0].appendChild(style);
      }
      
    } else {
      
        // retain the userDateWindow if requested
        if (instance.dygraph.userDateWindow != null
            && attrs.retainDateWindow == true) {
          attrs.dateWindow = instance.dygraph.xAxisRange();
        }
            
        // remove it from groups if it's there
        if (x.group != null && this.groups[x.group] != null) {
          var index = this.groups[x.group].indexOf(instance.dygraph);
          if (index != -1)
            this.groups[x.group].splice(index, 1);
        }
        
        // destroy the existing dygraph 
        instance.dygraph.destroy();
        instance.dygraph = null;
    }
    
    // add shiny input for date window
    if (HTMLWidgets.shinyMode)
      this.addDateWindowShinyInput(el.id, x);
    
    // create the instance and add it to it's group (if any)
    instance.dygraph = new Dygraph(el, attrs.file, attrs);
    instance.dygraph.userDateWindow = attrs.dateWindow;
    if (x.group != null)
      this.groups[x.group].push(instance.dygraph);
    
    // set annotations
    if (x.annotations != null) {
      instance.dygraph.ready(function() {
        if (x.format == "date") {
          x.annotations.map(function(annotation) {
            var date = thiz.normalizeDateValue(x.scale, annotation.x, x.fixedtz);
            annotation.x = date.getTime();
          });
        }
        instance.dygraph.setAnnotations(x.annotations);
      }); 
    }
      
  },
  
  customDateTickerFixedTZ : function(tz){
    return function(t,e,a,i,r) {   
      var a=Dygraph.pickDateTickGranularity(t,e,a,i);
      if(a >= 0){
        
        var n=i("axisLabelFormatter"),
        o=i("labelsUTC"),
        s=o?Dygraph.DateAccessorsUTC:Dygraph.DateAccessorsLocal;
        l=Dygraph.TICK_PLACEMENT[a].datefield;
        h=Dygraph.TICK_PLACEMENT[a].step;
        p=Dygraph.TICK_PLACEMENT[a].spacing;
        
        var y = [];
        var d = moment(t);
        d.tz(tz); 
        d.millisecond(0);
      
        if(l > Dygraph.DATEFIELD_M){
          var x;
          if (l === Dygraph.DATEFIELD_SS) {  // seconds 
            x = d.second();         
            d.second(x - x % h);     
          } else if(l === Dygraph.DATEFIELD_MM){
            d.second(0)
            x = d.minute();
            d.minute(x - x % h);
          } else if(l === Dygraph.DATEFIELD_HH){
            d.second(0);
            d.minute(0);
            x = d.hour();
            d.hour(x - x % h);
          } else if(l === Dygraph.DATEFIELD_D){
            d.second(0);
            d.minute(0);
            d.hour(0);
            if (h == 7) {  // one week
                d.startOf('week');
            }
          }
          
          v = d.valueOf();
          _=moment(v).tz(tz);
        
          // For spacings coarser than two-hourly, we want to ignore daylight
          // savings transitions to get consistent ticks. For finer-grained ticks,
          // it's essential to show the DST transition in all its messiness.
          var start_offset_min = moment(v).tz(tz).zone();
          var check_dst = (p >= Dygraph.TICK_PLACEMENT[Dygraph.TWO_HOURLY].spacing);
          
	        if(a<=Dygraph.HOURLY){
		        for(t>v&&(v+=p,_=moment(v).tz(tz));e>=v;){
			        y.push({v:v,label:n(_,a,i,r)});
			        v+=p;
			        _=moment(v).tz(tz);
		        }
	        }else{
            for(t>v&&(v+=p,_=moment(v).tz(tz));e>=v;){  
            
              // This ensures that we stay on the same hourly "rhythm" across
              // daylight savings transitions. Without this, the ticks could get off
              // by an hour. See tests/daylight-savings.html or issue 147.
              if (check_dst && _.zone() != start_offset_min) {
                var delta_min = _.zone() - start_offset_min;
                v += delta_min * 60 * 1000;
                _= moment(v).tz(tz);
                start_offset_min = _.zone();

                // Check whether we've backed into the previous timezone again.
                // This can happen during a "spring forward" transition. In this case,
                // it's best to skip this tick altogether (we may be shooting for a
                // non-existent time like the 2AM that's skipped) and go to the next
                // one.
                if (moment(v + p).tz(tz).zone() != start_offset_min) {
                  v += p;
                  _= moment(v).tz(tz);
                  start_offset_min = _.zone();
                }
              }
            
              (a>=Dygraph.DAILY||_.get('hour')%h===0)&&y.push({v:v,label:n(_,a,i,r)});
			        v+=p;
			        _=moment(v).tz(tz);
		        }
	        }
	      }else{
          var start_year = moment(t).tz(tz).year();
          var end_year   = moment(e).tz(tz).year();
          var start_month = moment(t).tz(tz).month();
          
          if(l === Dygraph.DATEFIELD_M){
            var step_month = h;
            for (var ii = start_year; ii <= end_year; ii++) {
              for (var j = 0; j < 12;) {
                var dt = moment(new Date(ii, j, 1)).tz(tz); 
                // fix some tz bug
                dt.year(ii);
                dt.month(j);
                dt.date(1);
                dt.hour(0);
                v = dt.valueOf();
                y.push({v:v,label:n(moment(v).tz(tz),a,i,r)});
                j+=step_month;
              }
            }
          }else{
            var step_year = h;
            for (var ii = start_year; ii <= end_year;) {
              var dt = moment(new Date(ii, 1, 1)).tz(tz); 
              // fix some tz bug
              dt.year(ii);
              dt.month(j);
              dt.date(1);
              dt.hour(0);
              v = dt.valueOf();
              y.push({v:v,label:n(moment(v).tz(tz),a,i,r)});
              ii+=step_year;
            }
          }
	      }
	      return y;
	    }else{
       return []; 
	    }
    };
  },

  xAxisLabelFormatterFixedTZ : function(tz){
  
    return function dateAxisFormatter(date, granularity){
      var mmnt = moment(date).tz(tz);
      if (granularity >= Dygraph.DECADAL){
        return mmnt.format('YYYY');
      }else{
        if(granularity >= Dygraph.MONTHLY){
          return mmnt.format('MMM YYYY');
        }else{
          var frac = mmnt.hour() * 3600 + mmnt.minute() * 60 + mmnt.second() + mmnt.millisecond();
            if (frac === 0 || granularity >= Dygraph.DAILY) {
              return mmnt.format('DD MMM');
            } else {
             if (mmnt.second()) {
               return mmnt.format('HH:mm:ss');
             } else {
               return mmnt.format('HH:mm');
             }
            }
         } 
                        
       }         
   }
  },
         
  xValueFormatterFixedTZ: function(scale, tz) {
                   
    return function(millis) {
      var mmnt = moment(millis).tz(tz);
        if (scale == "yearly")
          return mmnt.format('YYYY') + ' (' + mmnt.zoneAbbr() + ')';
        else if (scale == "monthly" || scale == "quarterly")
          return mmnt.format('MMM, YYYY')+ ' (' + mmnt.zoneAbbr() + ')';
        else if (scale == "daily" || scale == "weekly")
          return mmnt.format('MMM, DD, YYYY')+ ' (' + mmnt.zoneAbbr() + ')';
        else
          return mmnt.format('dddd, MMMM DD, YYYY HH:mm:ss')+ ' (' + mmnt.zoneAbbr() + ')';
    }
  },
  
  xValueFormatter: function(scale) {
    
    var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
                      
    return function(millis) {
      var date = new Date(millis);
        if (scale == "yearly")
          return date.getFullYear();
        else if (scale == "monthly" || scale == "quarterly")
          return monthNames[date.getMonth()] + ', ' + date.getFullYear(); 
        else if (scale == "daily" || scale == "weekly")
          return monthNames[date.getMonth()] + ', ' + 
                           date.getDate() + ', ' + 
                           date.getFullYear();
        else
          return date.toLocaleString();
    }
  },
  
  addZoomCallback: function(x, instance) {
    
    // alias this
    var thiz = this;
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing zoomCallback
    var prevZoomCallback = attrs["zoomCallback"];
    
    attrs.zoomCallback = function(minDate, maxDate, yRanges) {
      
      // call existing
      if (prevZoomCallback)
        prevZoomCallback(minDate, maxDate, yRanges);
        
      // record user date window (or lack thereof)
      var me = instance.dygraph;
      if (me.xAxisExtremes()[0] != minDate ||
          me.xAxisExtremes()[1] != maxDate) {
         me.userDateWindow = [minDate, maxDate];
      } else {
         me.userDateWindow = null;
      }
      
      // record in group if necessary
      if (x.group != null && thiz.groups[x.group] != null) {
        var group = thiz.groups[x.group];
        for(var i = 0; i<group.length; i++)
          group[i].userDateWindow = me.userDateWindow;
      }
    };
  },
  
  
  groups: {},
  
  addGroupDrawCallback: function(x) {
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing drawCallback
    var prevDrawCallback = attrs["drawCallback"];
    
    this.groups[x.group] = this.groups[x.group] || [];
    var group = this.groups[x.group];
    var blockRedraw = false;
    attrs.drawCallback = function(me, initial) {
      
      // call existing
      if (prevDrawCallback)
        prevDrawCallback(me, initial);
      
      // sync peers in group
      if (blockRedraw || initial) return;
      blockRedraw = true;
      var range = me.xAxisRange();
      for (var j = 0; j < group.length; j++) {
        if (group[j] == me) continue;
        // update group range only if it's different (prevents
        // infinite recursion in updateOptions)
        var peerRange = group[j].xAxisRange();
        if (peerRange[0] != range[0] || peerRange[1] != range[1]) {
          group[j].updateOptions({
            dateWindow: range
          });
        }
      }
      blockRedraw = false;
    };
  },
  
  addShadingCallback: function(x) {
    
    // bail if no shadings
    if (x.shadings.length == 0)
      return;
    
    // alias this
    var thiz = this;
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing underlayCallback
    var prevUnderlayCallback = attrs["underlayCallback"];
    
    // install callback
    attrs.underlayCallback = function(canvas, area, g) {
      
      // call existing
      if (prevUnderlayCallback)
        prevUnderlayCallback(canvas, area, g);
        
      for (var i = 0; i < x.shadings.length; i++) {
        var shading = x.shadings[i];
        canvas.save();
        canvas.fillStyle = shading.color;
        if (shading.axis == "x") {
          var x1 = shading.from;
          var x2 = shading.to;
          if (x.format == "date") {
            x1 = thiz.normalizeDateValue(x.scale, x1, x.fixedtz).getTime();
            x2 = thiz.normalizeDateValue(x.scale, x2, x.fixedtz).getTime();
          }
          var left = g.toDomXCoord(x1);
          var right = g.toDomXCoord(x2);
          
          canvas.fillRect(left, area.y, right - left, area.h);
        } else if (shading.axis == "y") {
          var bottom = g.toDomYCoord(shading.from);
          var top = g.toDomYCoord(shading.to);

          canvas.fillRect(area.x, bottom, area.w, top - bottom);
        }
        canvas.restore();
      }
    };
  },
  
  addEventCallback: function(x) {
    
    // bail if no evets
    if (x.events.length == 0)
      return;
    
    // alias this
    var thiz = this;
    
    // get attrs
    var attrs = x.attrs;
    
    // check for an existing underlayCallback
    var prevUnderlayCallback = attrs["underlayCallback"];
    
    // install callback
    attrs.underlayCallback = function(canvas, area, g) {
      
      // call existing
      if (prevUnderlayCallback)
        prevUnderlayCallback(canvas, area, g);
        
      for (var i = 0; i < x.events.length; i++) {
        
        // get event and x-coordinate
        var event = x.events[i];
        
        // draw line
        canvas.save();
        canvas.strokeStyle = event.color;
        if (event.axis == "x") {
          var xPos;
          if (jQuery.isNumeric(event.pos)) {
            xPos = g.toDomXCoord(event.pos);
          } else {
            xPos = thiz.normalizeDateValue(x.scale, event.pos, x.fixedtz).getTime();
            xPos = g.toDomXCoord(xPos);
          }
          
          // draw line
          thiz.dashedLine(canvas, 
                          xPos, 
                          area.y, 
                          xPos, 
                          area.y + area.h,
                          event.strokePattern);
        } else if (event.axis == "y") {
          yPos = g.toDomYCoord(event.pos);
          
          thiz.dashedLine(canvas, 
                          area.x, 
                          yPos, 
                          area.x + area.w, 
                          yPos,
                          event.strokePattern);
        }
        canvas.restore();
        
        // draw label
        if (event.label != null) {
          canvas.save();
          thiz.setFontSize(canvas, 12);
          var size = canvas.measureText(event.label);
          if (event.axis == "x") {
            var tx = xPos - 4;
            var ty;
            if (event.labelLoc == "top")
              ty = area.y + size.width + 10;
            else
              ty = area.y + area.h - 10;
            canvas.translate(tx, ty);
            canvas.rotate(3 * Math.PI / 2);
            canvas.translate(-tx,-ty);
          } else if (event.axis == "y") {
            var ty = yPos - 4;
            var tx;
            if (event.labelLoc == "right")
              tx = area.x + area.w - size.width - 10;
            else
              tx = area.x + 10;
          }
          canvas.fillText(event.label, tx, ty);
          canvas.restore();
        }
      }
    };
  },
  
  addDateWindowShinyInput: function(id, x) {
      
    // check for an existing drawCallback
    var prevDrawCallback = x.attrs["drawCallback"];
    
    // install the callback
    x.attrs.drawCallback = function(me, initial) {
      
      // call existing
      if (prevDrawCallback)
        prevDrawCallback(me, initial);
        
      // fire input change
      var range = me.xAxisRange();
      var dateWindow = [new Date(range[0]), new Date(range[1])];
      Shiny.onInputChange(id + "_date_window", dateWindow); 
    };
  },
  
  // Add dashed line support to canvas rendering context
  // See: http://stackoverflow.com/questions/4576724/dotted-stroke-in-canvas
  dashedLine: function(canvas, x, y, x2, y2, dashArray) {
    canvas.beginPath();
    if (!dashArray) dashArray=[10,5];
    if (dashLength==0) dashLength = 0.001; // Hack for Safari
    var dashCount = dashArray.length;
    canvas.moveTo(x, y);
    var dx = (x2-x), dy = (y2-y);
    var slope = dx ? dy/dx : 1e15;
    var distRemaining = Math.sqrt( dx*dx + dy*dy );
    var dashIndex=0, draw=true;
    while (distRemaining>=0.1){
      var dashLength = dashArray[dashIndex++%dashCount];
      if (dashLength > distRemaining) dashLength = distRemaining;
      var xStep = Math.sqrt( dashLength*dashLength / (1 + slope*slope) );
      if (dx<0) xStep = -xStep;
      x += xStep
      y += slope*xStep;
      canvas[draw ? 'lineTo' : 'moveTo'](x,y);
      distRemaining -= dashLength;
      draw = !draw;
    }
    canvas.stroke();
  },
  
  setFontSize: function(canvas, size) {
    var cFont = canvas.font;
    var parts = cFont.split(' ');
    if (parts.length === 2)
      canvas.font = size + 'px ' + parts[1];
    else if (parts.length === 3)
      canvas.font = parts[0] + ' ' + size + 'px ' + parts[2];
  },
  
  // Returns the value of a GET variable
  queryVar: function(name) {
    return decodeURI(window.location.search.replace(
      new RegExp("^(?:.*[&\\?]" +
                 encodeURI(name).replace(/[\.\+\*]/g, "\\$&") +
                 "(?:\\=([^&]*))?)?.*$", "i"),
      "$1"));
  },
  
  // We deal exclusively in UTC dates within R, however dygraphs deals 
  // exclusively in the local time zone. Therefore, in order to plot date
  // labels that make sense to the user when we are dealing with days,
  // months or years we need to convert the UTC date value to a local time
  // value that "looks like" the equivilant UTC value. To do this we add the
  // timezone offset to the UTC date.
  // Don't use in case of fixedtz
  normalizeDateValue: function(scale, value, fixedtz) {
    var date = new Date(value); 
    if (scale != "minute" && scale != "hourly" && scale != "seconds" && !fixedtz) {
      var localAsUTC = date.getTime() + (date.getTimezoneOffset() * 60000);
      date = new Date(localAsUTC);
    }
    return date;
  },
  
  // safely detect rendering on a mobile phone
  isMobilePhone: function() {
    try
    {
      return ! window.matchMedia("only screen and (min-width: 768px)").matches;
    }
    catch(e) {
      return false;
    }
  }
  
});

