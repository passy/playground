'use strict';
/** @flow */

const mergesort = (arr) => {
  const n = arr.length;
  if (n <= 1) return arr;

  const a = mergesort(arr.slice(0, Math.floor(n / 2)));
  const b = mergesort(arr.slice(Math.floor(n / 2), n));

  return merge(a, b);
};

const merge = (as, bs) => {
  const res = [];
  while (as.length > 0 || bs.length > 0) {
    let a = as[0];
    let b = bs[0];

    if (a === undefined) {
      res.push(bs.shift());
    } else if (b === undefined || a <= b) {
      res.push(as.shift());
    } else if (b < a) {
      res.push(bs.shift());
    }
  }

  return res;
};

const main = () => {
  const r = mergesort([5, 2, 6, 3, 1, 7, 0, 10, 12, 8, 5]);
  console.log(r);
};

main();
