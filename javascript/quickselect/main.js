'use strict';
/** @flow */

function getRandomIntInclusive(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

const swap = (arr, a, b) => {
  const tmp = arr[a];
  arr[a] = arr[b];
  arr[b] = tmp;
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

const quicksort = (arr, lo, hi) => {
  if (lo < hi) {
    const p = partition(arr, lo, hi);
    quicksort(arr, lo, p);
    quicksort(arr, p + 1, hi);
  }
};

const rselect = (arr, order) => {
  const go = (lo, hi) => {
    if (hi <= lo) return arr[lo];
    const p = partition(arr, lo, hi);
    if (p === order) return arr[order];
    if (p > order) return go(lo, p - 1);
    if (p < order) return go(p + 1, hi);
  };

  return go(0, arr.length - 1, order);
};

const arr = [5, 3, 10, 7, 9, 2, 1];
const o = rselect(arr, 5);

console.log('o =', o);
console.log('arr =', arr);
