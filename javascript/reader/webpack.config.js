'use strict';

module.exports = {
    entry: './entry.js',
    cache: true,
    module: {
        loaders: [{
            test: /\.js$/, loader: 'sweetjs?modules[]=./macros.sjs'
        }]
    },
    output: {
        path: __dirname,
        filename: 'bundle.js'
    }
};
