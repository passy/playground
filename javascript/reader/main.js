'use strict';

var Reader = function (fn) {
    this.f = fn;
};

Reader.ask = function () {
    return new Reader(_.identity);
};

Reader.asks = function (fn) {
    return new Reader(fn);
};

Reader.prototype.run = function (ctx) {
    return this.f(ctx);
};

// We use the name “unit” here since “return”
// is a keyword in JavaScript.
Reader.prototype.unit = function (fn) {
    return new Reader(_.constant(fn));
};

// `bind` isn’t a keyword but also has a different
// meaning in JS, so we use `flatMap` instead. Scala
// peeps, rejoice!
Reader.prototype.flatMap = function (k) {
    return new Reader(function (r) {
        return k.call(this, this.run(r)).run(r);
    }.bind(this));
};

var greet = function (name) {
    return Reader.ask().flatMap(function (ctx) {
        return this.unit(ctx + ", " + name);
    });
};

var example0 = function () {
    console.log(greet("Tom").run("Hi"));
};

example0();

var end = function (str) {
    var isHello = _.partial(_.isEqual, "Hello");
    return Reader.asks(isHello).flatMap(function (isH) {
        return this.unit(str + (isH ? "!" : "."));
    });
};

var example1 = function () {
    console.log(greet("James").flatMap(end).run("Hello"));
};

example1();