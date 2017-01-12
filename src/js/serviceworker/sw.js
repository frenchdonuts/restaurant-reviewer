var staticCacheName = 'restaurant-reviewer-static-v1'

const {
    assets,
} = serviceWorkerOption

let assetsToCache = [
    ...assets,
    './',
].map((path) => {
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
                    cacheName != staticCacheName
                ).map(cacheName =>
                    caches.delete(cacheName)
                )
            )
        )
    )
);

self.addEventListener('fetch', event => {
    //
    event.respondWith(
        caches.match(event.request).then(cachedResponse =>
            cachedResponse || fetch(event.request))
    )
})
