/* imgsrcset - Img srcset polyfill for resolution responsive images. Authors & copyright (c) 2012: WebLinc, David Knight. */

// Imgsrcset
(function(win) {
    'use strict';

    var _viewport       = win.document.documentElement,
        _srcsetID       = 0,
        _srcsets        = [],
        _eventPrefix    = '',
        _addEvent       = win.addEventListener || (_eventPrefix = 'on') && win.attachEvent,
        _removeEvent    = win.removeEventListener || win.detachEvent,
        _srcExpr        = /[^\s]+/g,
        _digitExpr      = /[0-9\.]+/g,
        _timer          = 0,

        /*
            _matches
        */
        _matches    = function(srcset) {
            var srcList     = (srcset.indexOf(',') !== -1 && srcset.split(',')) || [srcset],
                srcIndex    = srcList.length - 1,
                srcLength   = srcIndex,

                list        = null,
                listIndex   = 0,

                src         = '',
                media       = '';

            if (srcIndex < 0) {
                return;
            }

            do {
                var list        = srcList[srcLength - srcIndex].match(_srcExpr) || [],
                    listIndex   = list.length;

                while (listIndex--) {
                    var item    = list[listIndex],
                        feature = 0,
                        digits  = 0;

                    if (listIndex > 0) {
                        feature =   (item.indexOf('w') !== -1 && (win.innerWidth || _viewport.clientWidth)) || 
                                    (item.indexOf('h') !== -1 && (win.innerHeight ||  _viewport.clientHeight)) || 
                                    (item.indexOf('x') !== -1 && (win.devicePixelRatio || 1));

                        digits  = Number(item.match(_digitExpr));

                        if (feature && digits && digits > feature) {
                            break;
                        }
                    } else {
                        src     = item;
                        media   = srcList[srcIndex];
                    }
                }
            } while (srcIndex--);

            return (src && media && {src: src, media: media}) || false;
        },

        /*
            watch
        */
        _watch      = function(evt) {
            clearTimeout(_timer);

            _timer = setTimeout(function() {
                var srcset          = null,
                    srcsetIndex     = _srcsetID - 1,
                    srcsetLength    = srcsetIndex,
                    match           = false;

                do {
                    srcset = _srcsets[srcsetLength - srcsetIndex];

                    // If img element does not have a parent, remove array index to prevent caching
                    if (!srcset.element.parentNode) {
                        _srcsetID--;
                        srcset.splice(srcsetIndex, 1);
                        continue;
                    }

                    match = _matches(srcset.media);

                    if (match && (srcset.matches !== match.media)) {
                        srcset.matches = match.media;

                        srcset.element.src = match.src;
                    } else if (!match) {
                        srcset.matches = false;
                        srcset.src && (srcset.element.src = srcset.src);
                    }
                } while(srcsetIndex--);
            }, 10);
        },

        /*
            init
        */
        _init        = function() {
            _removeEvent(_eventPrefix + 'load', _init);

            win.Imgsrcset.parse();
            _watch();

            // Processes '_srcsets' array and determines which source to use
            // '_watch' will clear out any images from the array that do not have parents, which should eliminate element caching
            _addEvent(_eventPrefix + 'resize', _watch);
            _addEvent(_eventPrefix + 'orientationchange', _watch);
        };

    /*
        imgsrcset
    */
    win.Imgsrcset = {
        /*
            parse
            
            Called on '_init' and can also be called if new images are added/removed
         */
        parse: function() {
            _srcsets = [];

            var imgs        = win.document.getElementsByTagName('img') || [],
                imgIndex    = imgs.length - 1,
                imgLength   = imgIndex,
                img         = null,
                srcset      = '';

            do {
                img     = imgs[imgLength - imgIndex];
                srcset  = img.getAttribute('srcset') || '';

                if (!srcset) {
                    continue;
                }

                _srcsetID = _srcsets.push({
                    element     : img,
                    media       : srcset,
                    matches     : false,
                    src         : img.getAttribute('src') || ''
                });
            } while(imgIndex--);
        }
    };

    // Set up listeners
    _addEvent(_eventPrefix + 'load', _init);
})(window);
