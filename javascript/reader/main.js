var DOMWriter = function (id, content, config) {
    config.render(content);

    return "Hullo";
}.reader();

var main = function () {
    var config = {
        renderer: function (el, c) { el.innerHTML = c; }
    };
    DOMWriter("#main", "Hello, World").map(function (r) { console.log(r); }).run(config)
};

main();