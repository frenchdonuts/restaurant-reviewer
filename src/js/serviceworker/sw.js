var staticCacheName = 'restaurant-reviewer-static-v1'
var contentImgsCache = 'restaurant-reviewer-imgs'
var jsonCache = 'restaurant-reviewer-json'
var allCaches = [
    staticCacheName,
    contentImgsCache,
    jsonCache
]

const {
    assets,
} = serviceWorkerOption

console.log(assets)
let assetsToCache = [
    ...assets,
    '/',
    'https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext',
    'https://fonts.gstatic.com/s/roboto/v15/RxZJdnzeo3R5zSexge8UUZBw1xU1rKptJj_0jans920.woff2',
    'https://fonts.gstatic.com/s/materialicons/v19/2fcrYFNaTjcS6g4U3t-Y5UEw0lE80llgEseQY3FEmqw.woff2',
    'https://fonts.gstatic.com/s/roboto/v15/oMMgfZMQthOryQo9n22dcuvvDin1pK8aKteLpeZ5c0A.woff2',
    'https://fonts.googleapis.com/icon?family=Material+Icons',
    'https://code.getmdl.io/1.2.1/material.indigo-orange.min.css'
].map((path) => {
    console.log(path)
    return new URL(path, location).toString();
})


self.addEventListener('install', event =>
    event.waitUntil(
        caches.open(staticCacheName).then(cache =>
            cache.addAll(assetsToCache)
        )
    )
)

self.addEventListener('activate', event =>
    event.waitUntil(
        caches.keys().then(cacheNames =>
            Promise.all(
                cacheNames.filter(cacheName =>
                    cacheName.startsWith('restaurant-reviewer-') &&
                    !allCaches.includes(cacheName)
                ).map(cacheName =>
                    caches.delete(cacheName)
                )
            )
        )
    )
);

self.addEventListener('fetch', event => {
    //
    var requestUrl = new URL(event.request.url)
    var responsePromise

    if (isGooglePlacesQuery(requestUrl)) {
        //
        responsePromise = fetchAndPutInto(jsonCache, event.request)
    } else if (isImageFetch(requestUrl)) {
        //
        responsePromise = fetchAndPutInto(contentImgsCache,
            event.request)
    } else {
        responsePromise = caches.match(event.request).then(
            cachedResponse => {
                console.log("Cache hit!")
                return cachedResponse || fetch(event.request)
            })
    }

    event.respondWith(responsePromise)
})

function isGooglePlacesQuery(url) {
    // TODO
    return false
}

function isImageFetch(url) {
    // TODO
    return false
}

function fetchAndPutInto(cacheName, request) {
    return caches.open(cacheName)
        .then(cache => cache.match(request.url))
        .then(response => {
            if (response) return response

            return fetch(request).then(networkResponse => {
                cache.put(request.url, networkResponse.clone())
                return networkResponse
            })
        })
}
