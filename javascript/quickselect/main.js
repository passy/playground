'use strict';
/** @flow */

function getRandomIntInclusive(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const swap = (arr, a, b) => {
  const tmp = arr[a];
  arr[a] = arr[b];
  arr[b] = tmp;
  console.log('swap', a, b);
};

const partition = (arr, lo, hi) => {
  let i = getRandomIntInclusive(lo, hi);
  const pivot = arr[i];

  swap(arr, i, hi);
  for (let j = i = lo; j < hi; j += 1) {
    if (arr[j] < pivot) {
      swap(arr, i++, j);
    }
  }
  swap(arr, i, hi);
  return i;
};

const arr = [5, 3, 10, 7, 9, 2, 1];
const p = partition(arr, 0, arr.length - 1);

console.log('p =', p);
console.log('arr =', arr);
