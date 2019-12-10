// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Pervasives = require("bs-platform/lib/js/pervasives.js");
var Belt_SetString = require("bs-platform/lib/js/belt_SetString.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function getValues(string) {
  var split = string.split(")");
  return /* tuple */[
          Caml_array.caml_array_get(split, 0),
          Caml_array.caml_array_get(split, 1)
        ];
}

function findBodyOf(satellite, map) {
  return List.find((function (item) {
                var match = getValues(item);
                return satellite === match[1];
              }), map);
}

function getOrbit(map, satellite, toBody) {
  var nextOrbit;
  try {
    nextOrbit = findBodyOf(satellite, map);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return /* tuple */[
              1,
              /* :: */[
                satellite,
                /* [] */0
              ]
            ];
    } else {
      throw exn;
    }
  }
  var match = getValues(nextOrbit);
  var nextBody = match[0];
  var match$1 = nextBody === toBody;
  if (match$1) {
    return /* tuple */[
            2,
            /* :: */[
              satellite,
              /* [] */0
            ]
          ];
  } else {
    var match$2 = getOrbit(map, nextBody, toBody);
    return /* tuple */[
            1 + match$2[0] | 0,
            List.append(/* :: */[
                  satellite,
                  /* [] */0
                ], match$2[1])
          ];
  }
}

function distanceTo(map, fromSatellite, toBody) {
  return List.fold_left((function (param, value) {
                var accPath = param[1];
                var accDistance = param[0];
                var match = getValues(value);
                if (match[1] === fromSatellite) {
                  var match$1 = getOrbit(map, match[0], toBody);
                  return /* tuple */[
                          accDistance + match$1[0] | 0,
                          List.append(match$1[1], accPath)
                        ];
                } else {
                  return /* tuple */[
                          accDistance,
                          accPath
                        ];
                }
              }), /* tuple */[
              0,
              /* [] */0
            ], map);
}

function make(orbitMap) {
  return List.fold_left((function (acc, value) {
                var match = getValues(value);
                var match$1 = getOrbit(orbitMap, match[0], "COM");
                return acc + match$1[0] | 0;
              }), 0, orbitMap);
}

var Part1 = {
  make: make
};

function make$1(orbitMap) {
  var match = distanceTo(orbitMap, "YOU", "COM");
  var match$1 = distanceTo(orbitMap, "SAN", "COM");
  var youSet = Belt_SetString.fromArray($$Array.of_list(match[1]));
  var sanSet = Belt_SetString.fromArray($$Array.of_list(match$1[1]));
  var commonBodies = Belt_SetString.toArray(Belt_SetString.intersect(youSet, sanSet));
  var totalDist = $$Array.fold_left((function (acc, i) {
          var match = distanceTo(orbitMap, "YOU", i);
          var match$1 = distanceTo(orbitMap, "SAN", i);
          var totalDist = match[0] + match$1[0] | 0;
          var match$2 = totalDist < acc;
          if (match$2) {
            return totalDist;
          } else {
            return acc;
          }
        }), Pervasives.max_int, commonBodies);
  return totalDist - 2 | 0;
}

var Part2 = {
  make: make$1
};

exports.getValues = getValues;
exports.findBodyOf = findBodyOf;
exports.getOrbit = getOrbit;
exports.distanceTo = distanceTo;
exports.Part1 = Part1;
exports.Part2 = Part2;
/* No side effect */
