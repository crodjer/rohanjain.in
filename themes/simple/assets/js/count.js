// GoatCounter: https://www.goatcounter.com
// This file is released under the ISC license: https://opensource.org/licenses/ISC
;(function() {
  'use strict';

  window.goatcounter = window.goatcounter || {}

  // Get current path.
  var get_path = function() {
    var loc = location,
      c = document.querySelector('link[rel="canonical"][href]')
    if (c) {  // May be relative or point to different domain.
      var a = document.createElement('a')
      a.href = c.href
      if (a.hostname.replace(/^www\./, '') === location.hostname.replace(/^www\./, ''))
        loc = a
    }
    return (loc.pathname + loc.search) || '/'
  }

  // See if this looks like a bot.
  var is_bot = function() {
    var w = window, d = document
    if (w.callPhantom || w._phantom || w.phantom) return 150
    if (w.__nightmare) return 151
    if (d.__selenium_unwrapped || d.__webdriver_evaluate || d.__driver_evaluate) return 152
    if (navigator.webdriver) return 153
    return 0
  }

  // Filter some requests that we (probably) don't want to count.
  var filter = function() {
    if ('visibilityState' in document && document.visibilityState === 'prerender')
      return 'visibilityState'
    if (location.hostname.match(/(localhost$|^127\.|^10\.|^172\.(1[6-9]|2[0-9]|3[0-1])\.|^192\.168\.|^0\.0\.0\.0$)/))
      return 'localhost'
    if (location.protocol === 'file:')
      return 'localfile'
    if (localStorage && localStorage.getItem('skipgc') === 't')
      return 'disabled with #toggle-goatcounter'
    return false
  }

  // Count a hit.
  window.goatcounter.count = function() {
    var f = filter()
    if (f) return

    var s = document.querySelector('script[data-goatcounter]')
    var endpoint = (s && s.dataset.goatcounter) ? s.dataset.goatcounter : ''
    if (!endpoint) return

    var data = {
      p: get_path(),
      r: document.referrer,
      t: document.title,
      e: false,
      s: window.screen.width,
      b: is_bot(),
      rnd: Math.random().toString(36).substr(2, 5),
    }

    var p = []
    var enc = encodeURIComponent
    for (var k in data)
      if (data[k] !== '' && data[k] !== null && data[k] !== undefined)
        p.push(enc(k) + '=' + enc(data[k]))
    var url = endpoint + '?' + p.join('&')

    if (!navigator.sendBeacon(url)) {
      var img = document.createElement('img')
      img.src = url
      img.style.position = 'absolute'
      img.style.bottom = '0px'
      img.style.width = '1px'
      img.style.height = '1px'
      img.loading = 'eager'
      img.setAttribute('alt', '')
      img.setAttribute('aria-hidden', 'true')
      document.body.appendChild(img)
    }
  }

  // Helper to toggle tracking
  if (location.hash === '#toggle-goatcounter') {
    if (localStorage.getItem('skipgc') === 't') {
      localStorage.removeItem('skipgc')
      alert('GoatCounter tracking is now ENABLED in this browser.')
    } else {
      localStorage.setItem('skipgc', 't')
      alert('GoatCounter tracking is now DISABLED in this browser until ' + location + ' is loaded again.')
    }
  }

  // Init
  var on_load = function(f) {
    if (document.body === null)
      document.addEventListener('DOMContentLoaded', function() { f() }, false)
    else
      f()
  }

  on_load(function() {
    if (!('visibilityState' in document) || document.visibilityState === 'visible')
      goatcounter.count()
    else {
      var f = function(e) {
        if (document.visibilityState !== 'visible') return
        document.removeEventListener('visibilitychange', f)
        goatcounter.count()
      }
      document.addEventListener('visibilitychange', f)
    }
  })
})();