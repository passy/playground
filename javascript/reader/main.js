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

Reader.prototype.unit = function (fn) {
    return new Reader(_.constant(fn));
};

Reader.prototype.flatMap = function (k) {
    return new Reader(function (r) {
        return k.call(this, this.run(r)).run(r);
    }.bind(this));
};

var computation0 = function (name) {
    return Reader.ask().flatMap(function (ctx) {
        return this.unit(ctx + ", " + name);
    });
}

var example = function () {
    console.log(computation0("Tom").run("Hi"));
};

example();