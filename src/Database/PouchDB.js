"use strict";

exports._on = function (ee) {
    return function (e) {
        return function (eh) {
            return function () {
                ee.on(e, function (a) { eh(a)(); });
            }}}}

exports._cancel = function (r) {
    return function() {
        r.cancel();
    }
}
