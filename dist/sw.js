var serviceWorkerOption = {"assets":["./b03987fe265be148b304.js","././b03987fe265be148b304.css"]};
      !function(t){function e(o){if(n[o])return n[o].exports;var r=n[o]={exports:{},id:o,loaded:!1};return t[o].call(r.exports,r,r.exports,e),r.loaded=!0,r.exports}var n={};return e.m=t,e.c=n,e.p="",e(0)}([function(t,e){"use strict";function n(t){if(Array.isArray(t)){for(var e=0,n=Array(t.length);e<t.length;e++)n[e]=t[e];return n}return Array.from(t)}function o(t){var e="https://maps.googleapis.com"===t.origin,n="/maps/api/place/textsearch/json"===t.pathname,o="/maps/api/place/details/json"===t.pathname;return e&&(n||o)}function r(t){var e="https://maps.googleapis.com"===t.origin,n="/maps/api/place/photo"===t.pathname;return e&&n}function s(t,e){return console.log("fetchAndPutInto: "+t),caches.open(t).then(function(n){return n.match(e.url).then(function(o){return o?(console.log(t+" hit!"),o):fetch(e).then(function(t){return n.put(e.url,t.clone()),t})})})}var a="restaurant-reviewer-static-v1",i="restaurant-reviewer-imgs",c="restaurant-reviewer-json",u=[a,i,c],l=serviceWorkerOption,f=l.assets;console.log(f);var p=[].concat(n(f),["/","https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext","https://fonts.gstatic.com/s/roboto/v15/RxZJdnzeo3R5zSexge8UUZBw1xU1rKptJj_0jans920.woff2","https://fonts.gstatic.com/s/materialicons/v19/2fcrYFNaTjcS6g4U3t-Y5UEw0lE80llgEseQY3FEmqw.woff2","https://fonts.gstatic.com/s/roboto/v15/oMMgfZMQthOryQo9n22dcuvvDin1pK8aKteLpeZ5c0A.woff2","https://fonts.googleapis.com/icon?family=Material+Icons","https://code.getmdl.io/1.2.1/material.indigo-orange.min.css"]).map(function(t){return console.log(t),new URL(t,location).toString()});self.addEventListener("install",function(t){return t.waitUntil(caches.open(a).then(function(t){return t.addAll(p)}))}),self.addEventListener("activate",function(t){return t.waitUntil(caches.keys().then(function(t){return Promise.all(t.filter(function(t){return t.startsWith("restaurant-reviewer-")&&!u.includes(t)}).map(function(t){return caches.delete(t)}))}))}),self.addEventListener("fetch",function(t){var e,n=new URL(t.request.url);e=o(n)?s(c,t.request):r(n)?s(i,t.request):caches.match(t.request).then(function(e){return e||fetch(t.request)}),t.respondWith(e)})}]);