'use strict';

var reader = require('./reader');

reader.example0();
reader.example1();

var square = λ(_1 * _1);
console.log(square(4));
