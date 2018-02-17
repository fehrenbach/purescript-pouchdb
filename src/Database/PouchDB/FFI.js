"use strict";

function callback (e, r) {
    return function (err, response) {
        if (err)
            e(err);
        else
            r(response);
    }
}

// Many PouchDB operations cannot be cancelled
function nonCanceller() {
    return function (cancelError, cancelerError, cancelerSuccess) {
        cancelerSuccess();
    };
}

exports.pouchDB = function (options) {
    return function () {
        return new PouchDB(options);
    }}

exports.destroy = function (db) {
    return function (options) {
        return function (e, r) {
            db.destroy(options, callback(e, r));
            return nonCanceller();
        }}}

exports.put = function (db) {
    return function (doc) {
        return function (options) {
            return function (e, r) {
                db.put(doc, options, callback(e, r));
                return nonCanceller();
            }}}}

exports.post = function (db) {
    return function (doc) {
        return function (options) {
            return function (e, r) {
                db.post(doc, options, callback(e, r));
                return nonCanceller();
            }}}}

exports.get = function (db) {
    return function (docId) {
        return function (options) {
            return function (e, r) {
                db.get(docId, options, callback(e, r));
                return nonCanceller();
            }}}}

exports.remove = function (db) {
    return function (doc) {
        return function (options) {
            return function (e, r) {
                db.remove(doc, options, callback(e, r));
                return nonCanceller();
            }}}}

exports.bulkDocs = function (db) {
    return function (docs) {
        return function (options) {
            return function (e, r) {
                db.bulkDocs(docs, options, callback(e, r));
                return nonCanceller();
            }}}}

exports.allDocs = function (db) {
    return function (options) {
        return function (e, r) {
            db.allDocs(options, callback(e, r));
            return nonCanceller();
        }}}

exports.changes = function (db) {
    return function (options) {
        return function () {
            return db.changes(options);
        }}}

exports.replicate = function (source) {
    return function (target) {
        return function (options) {
            return function () {
                return PouchDB.replicate(source, target, options);
            }}}}

exports.replicateTo = function (source) {
    return function (target) {
        return function (options) {
            return function (e, r) {
                var rep = source.replicate.to(target, options, callback(e, r));
                return function (cancelError, cancelerError, cancelerSuccess) {
                    // not sure we *can* cancel non-live replications, PouchDB docs are unclear
                    rep.cancel();
                    cancelerSuccess();
                }}}}}

exports.sync = function (source) {
    return function (target) {
        return function (options) {
            return function () {
                return PouchDB.sync(source, target, options);
            }}}}

exports.putAttachment = function (db) {
    return function (docId) {
        return function (attachmentId) {
            return function (rev) {
                return function (attachment) {
                    return function (type) {
                        return function (e, r) {
                            db.putAttachment(docId, attachmentId, rev, attachment, type, callback(e, r));
                            return nonCanceller();
                        }}}}}}}

exports.getAttachment = function (db) {
    return function (docId) {
        return function (attachmentId) {
            return function (options) {
                return function (e, r) {
                    db.getAttachment(docId, attachmentId, options, callback(e, r));
                    return nonCanceller();
                }}}}}

exports.removeAttachment = function (db) {
    return function (docId) {
        return function (attachmentId) {
            return function (rev) {
                return function (e, r) {
                    db.removeAttachment(docId, attachmentId, rev, callback(e, r));
                    return nonCanceller();
                }}}}}

exports.query = function (db) {
    return function (fun) {
        return function (options) {
            return function (e, r) {
                db.query(fun, options, callback(e, r));
                return nonCanceller();
            }}}}

exports.viewCleanup = function (db) {
    return function (e, r) {
        db.viewCleanup(callback(e, r));
        return nonCanceller();
    }}

exports.info = function (db) {
    return function (e, r) {
        db.info(callback(e, r));
        return nonCanceller();
    }}

exports.compact = function (db) {
    return function (options) {
        return function (e, r) {
            db.compact(options, callback(e, r));
            return nonCanceller();
        }}}

exports.revsDiff = function (db) {
    return function (diff) {
        return function (e, r) {
            db.revsDiff(diff, callback(e, r));
            return nonCanceller();
        }}}

exports.bulkGet = function (db) {
    return function (options) {
        return function (e, r) {
            db.bulkGet(options, callback(e, r));
            return nonCanceller();
        }}}

exports.close = function (db) {
    return function (callback) {
        return function () {
            db.close(function () {
                callback();
            });
        }}}
