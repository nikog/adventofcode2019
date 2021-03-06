// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var $$Array = require("bs-platform/lib/js/array.js");
var Int64 = require("bs-platform/lib/js/int64.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_int64 = require("bs-platform/lib/js/caml_int64.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var Invalid_instruction = Caml_exceptions.create("Day09-Adventofcode2019.Invalid_instruction");

var Out_of_bounds = Caml_exceptions.create("Day09-Adventofcode2019.Out_of_bounds");

var Waiting_for_instruction = Caml_exceptions.create("Day09-Adventofcode2019.Waiting_for_instruction");

function parseOpcode(instruction) {
  var opcode = instruction % 100;
  var modes = (instruction - opcode | 0) / 100 | 0;
  return /* tuple */[
          opcode,
          Belt_Array.map(Belt_Array.reverse(String(modes).split("")), (function (code) {
                  var match = Caml_format.caml_int_of_string(code);
                  switch (match) {
                    case 0 :
                        return /* Position */0;
                    case 1 :
                        return /* Immediate */1;
                    case 2 :
                        return /* Relative */2;
                    default:
                      return /* Position */0;
                  }
                }))
        ];
}

function getIndex(instructions, modes, pointer, relativeBase, i) {
  var match = Belt_Array.get(modes, i - 1 | 0);
  if (match !== undefined) {
    switch (match) {
      case /* Position */0 :
          return Caml_int64.to_int32(Caml_array.caml_array_get(instructions, pointer + i | 0));
      case /* Immediate */1 :
          return pointer + i | 0;
      case /* Relative */2 :
          return relativeBase + Caml_int64.to_int32(Caml_array.caml_array_get(instructions, pointer + i | 0)) | 0;
      
    }
  } else {
    return Caml_int64.to_int32(Caml_array.caml_array_get(instructions, pointer + i | 0));
  }
}

function loop(instructions, _pointer, _input, _output, _relativeBase) {
  while(true) {
    var relativeBase = _relativeBase;
    var output = _output;
    var input = _input;
    var pointer = _pointer;
    var withinBounds = pointer <= (instructions.length - 1 | 0);
    var match = parseOpcode(Caml_int64.to_int32(Caml_array.caml_array_get(instructions, pointer)));
    var modes = match[1];
    var opcode = match[0];
    var basicGetIndex = (function(pointer,relativeBase,modes){
    return function basicGetIndex(param) {
      return getIndex(instructions, modes, pointer, relativeBase, param);
    }
    }(pointer,relativeBase,modes));
    if (withinBounds) {
      if (opcode >= 10) {
        if (opcode !== 99) {
          throw [
                Invalid_instruction,
                "Invalid instruction " + String(opcode)
              ];
        }
        return /* tuple */[
                instructions,
                output
              ];
      } else if (opcode > 0) {
        switch (opcode - 1 | 0) {
          case 0 :
              var p1 = Caml_array.caml_array_get(instructions, basicGetIndex(1));
              var p2 = Caml_array.caml_array_get(instructions, basicGetIndex(2));
              var pos = basicGetIndex(3);
              Caml_array.caml_array_set(instructions, pos, Caml_int64.add(p1, p2));
              _pointer = pointer + 4 | 0;
              continue ;
          case 1 :
              var p1$1 = Caml_array.caml_array_get(instructions, basicGetIndex(1));
              var p2$1 = Caml_array.caml_array_get(instructions, basicGetIndex(2));
              var pos$1 = basicGetIndex(3);
              Caml_array.caml_array_set(instructions, pos$1, Caml_int64.mul(p1$1, p2$1));
              _pointer = pointer + 4 | 0;
              continue ;
          case 2 :
              var pos$2 = basicGetIndex(1);
              if (input.length === 0) {
                throw [
                      Waiting_for_instruction,
                      pointer,
                      relativeBase,
                      instructions,
                      output
                    ];
              }
              Caml_array.caml_array_set(instructions, pos$2, Caml_array.caml_array_get(input, 0));
              _input = $$Array.sub(input, 1, input.length - 1 | 0);
              _pointer = pointer + 2 | 0;
              continue ;
          case 3 :
              var pos$3 = basicGetIndex(1);
              _output = $$Array.append(output, /* array */[Caml_array.caml_array_get(instructions, pos$3)]);
              _pointer = pointer + 2 | 0;
              continue ;
          case 4 :
              var jump = Caml_int64.to_int32(Caml_array.caml_array_get(instructions, basicGetIndex(1))) > 0;
              var newPointer = jump ? Caml_int64.to_int32(Caml_array.caml_array_get(instructions, basicGetIndex(2))) : pointer + 3 | 0;
              _pointer = newPointer;
              continue ;
          case 5 :
              var jump$1 = Caml_int64.to_int32(Caml_array.caml_array_get(instructions, basicGetIndex(1))) === 0;
              var newPointer$1 = jump$1 ? Caml_int64.to_int32(Caml_array.caml_array_get(instructions, basicGetIndex(2))) : pointer + 3 | 0;
              _pointer = newPointer$1;
              continue ;
          case 6 :
              var p1$2 = Caml_array.caml_array_get(instructions, basicGetIndex(1));
              var p2$2 = Caml_array.caml_array_get(instructions, basicGetIndex(2));
              var pos$4 = basicGetIndex(3);
              var match$1 = Int64.compare(p1$2, p2$2) < 0;
              Caml_array.caml_array_set(instructions, pos$4, Caml_int64.of_int32(match$1 ? 1 : 0));
              _pointer = pointer + 4 | 0;
              continue ;
          case 7 :
              var p1$3 = Caml_array.caml_array_get(instructions, basicGetIndex(1));
              var p2$3 = Caml_array.caml_array_get(instructions, basicGetIndex(2));
              var pos$5 = basicGetIndex(3);
              var match$2 = Int64.compare(p1$3, p2$3) === 0;
              Caml_array.caml_array_set(instructions, pos$5, Caml_int64.of_int32(match$2 ? 1 : 0));
              _pointer = pointer + 4 | 0;
              continue ;
          case 8 :
              var p1$4 = Caml_int64.to_int32(Caml_array.caml_array_get(instructions, basicGetIndex(1)));
              _relativeBase = relativeBase + p1$4 | 0;
              _pointer = pointer + 2 | 0;
              continue ;
          
        }
      } else {
        throw [
              Invalid_instruction,
              "Invalid instruction " + String(opcode)
            ];
      }
    } else {
      throw [
            Out_of_bounds,
            "Out of bounds at " + String(pointer)
          ];
    }
  };
}

function make($staropt$star, $staropt$star$1, input, instructions) {
  var pointer = $staropt$star !== undefined ? $staropt$star : 0;
  var relativeBase = $staropt$star$1 !== undefined ? $staropt$star$1 : 0;
  var match = pointer === 0;
  var instructionsWithMemory;
  if (match) {
    var __x = $$Array.map(Caml_int64.of_int32, instructions);
    instructionsWithMemory = $$Array.append(__x, Caml_array.caml_make_vect(Caml_int32.imul(10, instructions.length), /* int64 */{
              hi: 0,
              lo: 0
            }));
  } else {
    instructionsWithMemory = $$Array.map(Caml_int64.of_int32, instructions);
  }
  var match$1 = loop(instructionsWithMemory, pointer, $$Array.map(Caml_int64.of_int32, input), /* array */[], relativeBase);
  return /* tuple */[
          $$Array.map(Int64.to_string, match$1[0]),
          $$Array.map(Int64.to_string, match$1[1])
        ];
}

var Part1 = {
  loop: loop,
  make: make
};

exports.Invalid_instruction = Invalid_instruction;
exports.Out_of_bounds = Out_of_bounds;
exports.Waiting_for_instruction = Waiting_for_instruction;
exports.parseOpcode = parseOpcode;
exports.getIndex = getIndex;
exports.Part1 = Part1;
/* No side effect */
