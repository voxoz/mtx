Metrics Protocol
================

MTX supports front-end API for tracking Historgram, Meter,
Counter, Gauge, Timing keys. Metrics protocols support various API backends:

* DataDog
* New Relic
* Librato

NewRelic and Librato will come later, sticked with Etsy statsd.

Usage
-----

```erlang
    mtx:decrement("test.decrement", 1, 0.5).
    mtx:increment("test.increment", 1, 0.5).
    mtx:gauge("test.gauge", 333, 1.0).
    mtx:timing("test.timing", 5, 0.5).
```

Credits
-------

* Maxim Sokhatsky
* Vladimir Kirillov

OM A HUM
