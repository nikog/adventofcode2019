// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Curry = require("bs-platform/lib/js/curry.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");

function readInput(param) {
  return Fs.readFileSync(__dirname + "/input", "utf8");
}

function solution(solveFn) {
  return Fs.readFileSync(__dirname + "/input", "utf8").split("\n").map((function (value) {
                  return Curry._1(solveFn, Caml_format.caml_int_of_string(value));
                })).reduce((function (acc, n) {
                return acc + n | 0;
              }), 0);
}

function solve(input) {
  return (input / 3 | 0) - 2 | 0;
}

var Part1 = {
  solve: solve
};

function solve$1(fuel) {
  var fuel$1 = solve(fuel);
  var match = fuel$1 > 0;
  if (match) {
    return fuel$1 + solve$1(fuel$1) | 0;
  } else {
    return 0;
  }
}

var Part2 = {
  solve: solve$1
};

console.log(solution(solve));

console.log(solution(solve$1));

exports.readInput = readInput;
exports.solution = solution;
exports.Part1 = Part1;
exports.Part2 = Part2;
/*  Not a pure module */
