// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var $$Array = require("bs-platform/lib/js/array.js");
var $$String = require("bs-platform/lib/js/string.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Day02$Adventofcode2019 = require("./day02.bs.js");

function readInput(param) {
  return Fs.readFileSync(__dirname + "/input", "utf8");
}

function solutionPart1(noun, verb) {
  var __x = Fs.readFileSync(__dirname + "/input", "utf8");
  return Caml_array.caml_array_get(Day02$Adventofcode2019.Part1.solve($$Array.mapi((function (index, value) {
                        if (index !== 1) {
                          if (index !== 2) {
                            return value;
                          } else {
                            return verb;
                          }
                        } else {
                          return noun;
                        }
                      }), $$Array.map(Caml_format.caml_int_of_string, $$Array.of_list($$String.split_on_char(/* "," */44, __x))))), 0);
}

console.log(solutionPart1(12, 2));

function solutionPart2(param) {
  var nouns = Belt_Array.range(0, 99);
  var verbs = Belt_Array.range(0, 99);
  var __x = Belt_Array.concatMany($$Array.map((function (noun) {
              return $$Array.map((function (verb) {
                            var match = solutionPart1(noun, verb);
                            if (match !== 19690720) {
                              return ;
                            } else {
                              return Caml_int32.imul(100, noun) + verb | 0;
                            }
                          }), verbs);
            }), nouns));
  var res = Belt_Array.keepMap(__x, (function (x) {
          return x;
        }));
  return Caml_array.caml_array_get(res, 0);
}

console.log(solutionPart2(/* () */0));

exports.readInput = readInput;
exports.solutionPart1 = solutionPart1;
exports.solutionPart2 = solutionPart2;
/*  Not a pure module */
