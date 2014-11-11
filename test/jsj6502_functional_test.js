#!/usr/bin/env node

var assert = require('assert');
var buffer = require('buffer');
var fs = require('fs');
var Jsj6502 = require('../src/Jsj6502.js').Jsj6502;

assert.ok(Jsj6502);
var cpu = new Jsj6502();

var data = fs.readFileSync('../third_party/6502_65C02_functional_tests/bin_files/6502_functional_test.bin');
assert.equal(65536, data.length);
for (var i = 0; i < data.length; ++i)
    cpu.s8(i, data[i]);

cpu.reset();
cpu.PC[0]=0x400;
cpu.halt = true;
for (;;) {
    cpu.run();
    if (cpu.PC[0] == 0x400)
        break;
}
