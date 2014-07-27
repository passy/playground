var Reader = function (fn) {
    this.f = fn;
};

Reader.prototype.run = function (ctx) {
    return this.f(ctx);
};

Reader.prototype.map = function (fn) {
    return new Reader(function (ctx) {
        return fn(this.run(ctx));
    }.bind(this));
};

Reader.prototype.flatMap = function (fn) {
    return new Reader(function (ctx) {
        return fn(this.run(ctx)).run(ctx);
    }.bind(this));
};

var simple = new Reader(function () {
    return this.ask();
});

var computation = new Reader(function (name) {
    this.ask().bind(function (greeting) {
        return this.unit(greeting + ", " + name);
    });
});

var example = function () {
    console.log(computation("Tom").runReader("Hello"));
};

example();