"use strict";

const _pouchdb = require('pouchdb');

exports.pouchDB = function (name) {
    return new _pouchdb(name)
}

exports.f_info = function (db) {
    return function (k_err) {
        return function (k_res) {
            return function () {
                db.info(function (err, res) {
                    if (err)
                        k_err(err)();
                    else
                        k_res(res)();
                })}}}}

exports.f_put = function (db) {
    return function (doc) {
        return function (k_err) {
            return function (k_res) {
                return function () {
                    db.put(doc, function (err, res) {
                        if (err)
                            k_err(err)();
                        else
                            k_res(res)();
                    })}}}}}

exports.f_get = function (db) {
    return function (id) {
        return function (k_err) {
            return function (k_res) {
                return function () {
                    db.get(id, function (err, res) {
                        if (err)
                            k_err(err)();
                        else
                            k_res(res)();
                    })}}}}}

exports.f_remove = function (db) {
    return function (doc) {
        return function (k_err) {
            return function (k_res) {
                return function () {
                    db.remove(doc, function (err, res) {
                        if (err)
                            k_err(err)();
                        else
                            k_res(res)();
                    })}}}}}
