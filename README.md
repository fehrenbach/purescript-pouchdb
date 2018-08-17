# purescript-pouchdb
[![Build Status](https://travis-ci.org/fehrenbach/purescript-pouchdb.svg?branch=master)](https://travis-ci.org/fehrenbach/purescript-pouchdb)
<a href="https://pursuit.purescript.org/packages/purescript-pouchdb">
  <img src="https://pursuit.purescript.org/packages/purescript-pouchdb/badge"
       alt="purescript-pouchdb on Pursuit">
  </img>
</a>

## Installation

Install this library itself like any other PureScript library using bower, psc-package, or any other way.

To actually use it, you need to have a *global* `PouchDB` object present!

For a web application that runs in a browser, the simplest way is to just add a script tag for PouchDB, like this:
```html
<script src="//cdn.jsdelivr.net/npm/pouchdb@7.0.0/dist/pouchdb.min.js"></script>
```

For a Node.js application, install the `pouchdb` npm package, then make sure you set `global.PouchDB`.
For example, the tests are run like this:
```bash
node -e 'global.PouchDB = require("pouchdb"); require("./output/Test.Main").main();'
```

It should be possible to use the same technique to make this work with browserify, webpack, etc.

See also the [PouchDB installation documentation](https://pouchdb.com/download.html).

## The Id and Rev newtypes and document newtypes

PouchDB is a schemaless document database.
Nevertheless, in practice people use mostly use a limited set of different types of documents.
This library embraces fixed, declared document types.

Declare a document type like this:
```PureScript
newtype Actor = Actor { _id :: Id Actor,
                        _rev :: Rev Actor,
                        name :: String }

derive instance newtypeActor :: Newtype Actor _
derive newtype instance writeForeignActor :: WriteForeign Actor
derive newtype instance readForeignActor :: ReadForeign Actor
```
The above defines a newtype `Actor` around a record with a `name` field, and two special fields `_id` and `_rev`.
We derive the `Newtype` instance, which gives us a generic way to wrap and unwrap to go from a correctly typed record to `Actor` and back.
We also derive reading and writing as JSON using the simple-json library.

This is slightly cumbersome, but in my experience it's worth it.

The reason this library encourages the use of newtypes are the `Id` and `Rev` types.
Plain PouchDB uses strings for document keys (and revisions).
`Id` and `Rev` are newtypes of `String` with a phantom type variable.
The type variable says what type of document the `Id` refers to.
For example, given an `Id Actor`, the document retrieval function `getDoc` returns an `Actor`.
The other great use is better modeling of references between documents.
Consider this movie record newtype:
```PureScript
newtype Movie = Movie { _id :: Id Movie,
                        _rev :: Rev Movie,
                        title :: String,
                        year :: Int,
                        actors :: Array (Id Actor),
                        sequel :: Maybe (Id Movie)}

derive instance newtypeMovie :: Newtype Movie _
derive newtype instance writeForeignMovie :: WriteForeign Movie
derive newtype instance readForeignMovie :: ReadForeign Movie
```
Without the phantom types we would have lots of plain ids or even strings around and we would have to try much harder to use the right one in any given context.

**Note:** The phantom type argument to `Id` and `Rev` is a lie!
If your database is inconsistent you might still have problems.
If an id is just missing from the database, you won't be able to retrieve it.
If an id is referring to a document of a different type or otherwise does not follow the schema you might not be able to deserialize it.
If you stay in nice and cosy PureScript land you will be fine.
If you venture into JavaScript land: be careful!
