"use strict";

const _pouchdb = require('pouchdb');

function callback (e, r) {
    return function (err, response) {
        if (err)
            e(err)();
        else
            r(response)();
    }
}

exports.pouchDB = function (name) {
    return function (options) {
        return new _pouchdb(name, options);
    }
}

exports.destroy = function (db) {
    return function (options) {
        return function (e) {
            return function (r) {
                return function () {
                    db.destroy(options, callback(e, r));
                }}}}}

exports.put = function (db) {
    return function (doc) {
        return function (options) {
            return function (e) {
                return function (r) {
                    return function () {
                        db.put(doc, options, callback(e, r));
                    }}}}}}

exports.get = function (db) {
    return function (docId) {
        return function (options) {
            return function (e) {
                return function (r) {
                    db.get(docId, options, callback(e, r));
                }}}}}

exports.remove = function (db) {
    return function (doc) {
        return function (options) {
            return function (e) {
                return function (r) {
                    return function () {
                        db.remove(doc, options, callback(e, r));
                    }}}}}}

exports.bulkDocs = function (db) {
    return function (docs) {
        return function (options) {
            return function (e) {
                return function (r) {
                    return function () {
                        db.bulkDocs(docs, options, callback(e, r));
                    }}}}}}

exports.allDocs = function (db) {
    return function (options) {
        return function (e) {
            return function (r) {
                return function () {
                    db.allDocs(options, callback(e, r));
                }}}}}

exports.changes = function (db) {
    return function (options) {
        return function () {
            return db.changes(options);
        }}}

exports.replicate = function (source) {
    return function (target) {
        return function (options) {
            return function () {
                return _pouchdb.replicate(source, target, options);
            }}}}

exports.sync = function (source) {
    return function (target) {
        return function (options) {
            return function () {
                return _pouchdb.sync(source, target, options);
            }}}}

exports.putAttachment = function (db) {
    return function (docId) {
        return function (attachmentId) {
            return function (rev) {
                return function (attachment) {
                    return function (type) {
                        return function (e) {
                            return function (r) {
                                return function () {
                                    db.putAttachment(docId, attachmentId, rev, attachment, type, callback(e, r));
                                }}}}}}}}}

exports.getAttachment = function (db) {
    return function (docId) {
        return function (attachmentId) {
            return function (options) {
                return function (e) {
                    return function (r) {
                        return function () {
                            db.getAttachment(docId, attachmentId, options, callback(e, r));
                        }}}}}}}

exports.removeAttachment = function (db) {
    return function (docId) {
        return function (attachmentId) {
            return function (rev) {
                return function (e) {
                    return function (r) {
                        return function () {
                            db.removeAttachment(docId, attachmentId, rev, callback(e, r));
                        }}}}}}}

exports.query = function (db) {
    return function (fun) {
        return function (options) {
            return function (e) {
                return function (r) {
                    return function () {
                        db.query(fun, options, callback(e, r));
                    }}}}}}

exports.viewCleanup = function (db) {
    return function (e) {
        return function (r) {
            return function () {
                db.viewCleanup(callback(e, r));
            }}}}

exports.info = function (db) {
    return function (e) {
        return function (r) {
            return function () {
                db.info(callback(e, r));
            }}}}

exports.compact = function (db) {
    return function (options) {
        return function (e) {
            return function (r) {
                return function () {
                    db.compact(options, callback(e, r));
                }}}}}

exports.revsDiff = function (db) {
    return function (diff) {
        return function (e) {
            return function (r) {
                return function () {
                    db.revsDiff(diff, callback(e, r));
                }}}}}

exports.bulkGet = function (db) {
    return function (options) {
        return function (e) {
            return function (r) {
                return function () {
                    db.bulkGet(options, callback(e, r));
                }}}}}

exports.close = function (db) {
    return function (callback) {
        return function () {
            db.close(function () {
                callback();
            });
        }}}
