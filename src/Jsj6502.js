(function () {
/**
 * Converts a number to a hex in string with the prefix 0x.
 * @param {number} n - A number to convert.
 * @param {number} [l=2] - Output width.
 */
var ToHex = function (n, l) {
    var size = l || 2;
    return ('00000000' + n.toString(16)).substr(-size);
};

/**
 * Starts performance counting.
 * @param {string} name - An entry name.
 */
var CheckIn = function (name) {
    console.time(name);
};

/**
 * Finishes and shows performance conting result.
 * @param {string} name - An entry name that should match with one for CheckIn.
 */
var CheckOut = function (name) {
    console.timeEnd(name);
};

/**
 * Checks an assert condition.
 * @param {boolean} condition - A condition to check.
 * @param {string} [log] - An error message to show on condition check failures.
 */
var Assert = function (condition, log) {
    if (!condition) {
        console.error(log);
        throw console.assert(condition);
    }
};

/**
 * Constructs Jsj650 instance.
 * @constructor
 * @classdesc A fast 6502 CPU emulator using JIT compilation.
 */
var Jsj6502 = function () {
    // 6502 native registers.
    this.A = new Uint8Array(1);
    this.Y = new Uint8Array(1);
    this.X = new Uint8Array(1);
    this.PC = new Uint16Array(1);
    this.S = new Uint8Array(1);
    this.SR = new Uint8Array(1);
    this.T = new Uint8Array(2);  // temporary register for emulation
    this.TMP = 0;

    // Variables representing each bit of 6502 processor status register.
    this.N = 0;  // 7: Negative
    this.V = 0;  // 6: Overflow

    this.B = 1;  // 4: BRK command (It will be reset on handling IRQ interrupt)
    this.D = 0;  // 3: Decimal mode
    this.I = 0;  // 2: IRQ disable
    this.Z = 0;  // 1: Zero
    this.C = 0;  // 0: Carry

    // Memory.
    this.memory = new Uint8Array(0x10000);
    this.writable = new Array(0x100);
    this.io = new Array(0x100);
    for (var i = 0; i < 0x100; ++i) {
        this.writable[i] = true;
        this.io[i] = undefined;
    }
    // Creates memory alias for optimized stack access.
    this.stack = this.memory.subarray(0x100, 0x200);

    // Compiled trace cache.
    // this.cache[<address>] : {
    //     <trace start address>: <compiled code>,
    //     ...
    // }
    this.cache = new Array(0x10000);

    this.halt = false;

    this.logCache = false;
    this.logDecode = false;
    this.logJit = false;
    this.logDump = false;
    this.logMemory = false;
    this.enableCache = true;
    this.enableConditionOptimization = true;
    this.enableInfiniteLoopDetector = true;
    this.enableStepCompile = false;

    this.reset();
};

// Vector addresses.
Jsj6502.VECTOR_IRQ = 0xfffe;
Jsj6502.VECTOR_BRK = 0xfffe;
Jsj6502.VECTOR_RES = 0xfffc;
Jsj6502.VECTOR_NMI = 0xfffa;

Jsj6502.OpDescriptions = [
    // 0x00 - 0x0f
    'BRK', 'ORA - (Indirect, X)', '', '', '', 'ORA - Zero Page',
    'ASL - Zero Page', '', 'PHP', 'ORA - Immediate', 'ASL - Accumulator', '',
    '', 'ORA - Absolute', 'ASL - Absolute', '',
    // 0x10 - 0x1f
    'BPL', 'ORA - (Indirect), Y', '', '', '', 'ORA - Zero Page, X',
    'ASL - Zero Page, X', '', 'CLC', 'ORA - Absolute, Y', '', '', '',
    'ORA - Absolute, X', 'ASL - Absolute, X', '',
    // 0x20 - 0x2f
    'JSR', 'AND - (Indirect, X)', '', '', 'BIT - Zero Page', 'AND - Zero Page',
    'ROL - Zero Page', '', 'PLP', 'AND - Immediate', 'ROL - Accumulator', '',
    'BIT - Absolute', 'AND - Absolute', 'ROL - Absolute', '',
    // 0x30 - 0x3f
    'BMI', 'AND - (Indirect), Y', '', '', '', 'AND - Zero Page, X',
    'ROL - Zero Page, X', '', 'SEC', 'AND - Absolute, Y', '', '', '',
    'AND - Absolute, X', 'ROL - Absolute, X', '',
    // 0x40 - 0x4f
    'RTI', 'EOR - (Indirect, X)', '', '', '', 'EOR - Zero Page',
    'LSR - Zero Page', '', 'PHA', 'EOR - Immediate', 'LSR - Accumulator', '',
    'JMP - Absolute', 'EOR - Absolute', 'LSR - Absolute', '',
    // 0x50 - 0x5f
    'BVC', 'EOR - (Indirect), Y', '', '', '', 'EOR - Zero Page, X',
    'LSR - Zero Page, X', '', 'CLI', 'EOR - Absolute, Y', '', '', '',
    'EOR - Absolute, X', 'LSR - Absolute, X', '',
    // 0x60 - 0x6f
    'RTS', 'ADB - (Indirect, X)', '', '', '', 'ADC - Zero Page',
    'ROR - Zero Page', '', 'PLA', 'ADC - Immediate', 'ROR - Accumulator', '',
    'JMP - Indirect', 'ADC - Absolute', 'ROR - Absolute', '',
    // 0x70 - 0x7f
    'BVS', 'ADC - (Indirect), Y', '', '', '', 'ADC - Zero Page, X',
    'ROR - Zero Page, X', '', 'SEI', 'ADC - Absolute, Y', '', '', '',
    'ADC - Absolute, X', 'ROR - Absolute, X', '',
    // 0x80 - 0x8f
    '', 'STA - (Indirect, X)', '', '', 'STY - Zero Page', 'STA - Zero Page',
    'STX - Zero Page', '', 'DEY', '', 'TXA', '', 'STY - Absolute',
    'STA - Absolute', 'STX - Absolute', '',
    // 0x90 - 0x9f
    'BCC', 'STA - (Indirect), Y', '', '', 'STY - Zero Page, X',
    'STA - Zero Page X', 'STA - Zero Page, Y', '', 'TYA', 'STA - Absolute, Y',
    'TXS', '', '', 'STA - Absolute, X', '', '',
    // 0xa0 - 0xaf
    'LDY - Immediate', 'LDA - (Indirect, X)', 'LDX - Immediate', '',
    'LDY - Zero Page', 'LDA - Zero Page', 'LDX - Zero Page', '', 'TAY',
    'LDA - Immediate', 'TAX', '', 'LDY - Absolute', 'LDA - Absolute',
    'LDX - Absolute', '',
    // 0xb0 - 0xbf
    'BCS', 'LDA - (Indirect), Y', '', '', 'LDY - Zero Page, X',
    'LDA - Zero Page, X', 'LDX - Zero Page, Y', '', 'CLV', 'LDA - Absolute, Y',
    'TSX', '', 'LDY - Absolute, X', 'LDA - Absolute, X', 'LDX - Absolute, Y',
    '',
    // 0xc0 - 0xcf
    'CPY - Immediate', 'CMP - (Indirect, X)', '', '', 'CPY - Zero Page',
    'CMP - Zero Page', 'DEC - Zero Page', '', 'INY', 'CMP - Immediate', 'DEX',
    '', 'CPY - Absolute', 'CMP - Absolute', 'DEC - Absolute', '',
    // 0xd0 - 0xdf
    'BNE', 'CMP - (Indirect), Y', '', '', '', 'CMP - Zero Page, X',
    'DEC - Zero Page, X', '', 'CLD', 'CMP - Absolute, Y', '', '', '',
    'CMP - Absolute, X', 'DEC - Absolute, X', '',
    // 0xe0 - 0xef
    'CPX - Immediate', 'SBC - (Indirect, X)', '', '', 'CPX - Zero Page',
    'SBC - Zero Page', 'INC - Zero Page', '', 'INX', 'SBC - Immediate', 'NOP',
    '', 'CPX - Absolute', 'SBC - Absolute', 'INC - Absolute', '',
    // 0xf0 - 0xff
    'BEQ', 'SBC - (Indirect), Y', '', '', '', 'SBC - Zero Page, X',
    'INC - Zero Page, X', '', 'SED', 'SBC - Absolulte, Y', '', '', '',
    'SBC - Absolute, X', 'INC - Absolute, X', ''
];
Assert(Jsj6502.OpDescriptions.length == 256);

/**
 * Resets all CPU internal states.
 */
Jsj6502.prototype.reset = function () {
    this.A[0] = 0;
    this.Y[0] = 0;
    this.X[0] = 0;
    this.PC[0] = this.l16(Jsj6502.VECTOR_RES);
    this.S[0] = 0xff;
    this.SR[0] = 0;
    this.N = 0;
    this.V = 0;
    this.B = 1;
    this.D = 0;
    this.I = 0;
    this.Z = 0;
    this.C = 0;
    this.halt = false;
    if (this.logDump)
        this.dump(this.PC[0]);

    for (var i = 0; i < 0x10000; ++i)
        this.cache[i] = undefined;
};

/**
 * Shows register dump.
 * @param {Uint16} pc - A program counter.
 * @param {boolean} full - A flag to dump internal registers.
 */
Jsj6502.prototype.dump = function (pc, full) {
    var ext = ' : ' + Jsj6502.OpDescriptions[this.memory[pc]];
    this.log('*** dump *** PC=$' + ToHex(pc, 4) +
             ' A=$' + ToHex(this.A[0]) +
             ' X=$' + ToHex(this.X[0]) +
             ' Y=$' + ToHex(this.Y[0]) +
             ' SP=$' + ToHex(this.S[0]) +
             ' NPC=$' + ToHex(this.PC[0], 4) +
             ' NV-B_DIZC=' + this.N + this.V + '-' + this.B + '_' +
                             this.D + this.I + this.Z + this.C + ext);
    if (!full)
        return;
    this.log('************ T[0]=$' + ToHex(this.T[0]) + ' T[1]=$' + ToHex(this.T[1]));
};

/**
 * Runs.
 */
Jsj6502.prototype.run = function () {
    do {
        var cache = this.cache[this.PC[0]];
        var code = undefined;
        if (cache && cache[this.PC[0]]) {
            code = cache[this.PC[0]];
            if (this.logCache) {
                this.log('cache hit: pc=$' + ToHex(this.PC[0], 4));
                Assert(code.valid);
            }
        }
        if (!code) {
            code = this._compile();
            if (!this.disableCache) {
                for (var pc = code.start; pc <= code.end; ++pc) {
                    if (!this.cache[pc])
                        this.cache[pc] = {};
                    this.cache[pc][code.start] = code;
                }
            }
        }
        code.trace(this);
    } while (!this.halt);
};

/**
 * Writes a log.
 */
Jsj6502.prototype.log = console.log.bind(console);

/**
 * Loads 16-bit values from the main memory.
 * @param {Uint16} address - A load address.
 * @return {Uint8} 16-bit value read from the address.
 */
Jsj6502.prototype.l16 = function (address) {
    return (this.l8(address + 1) << 8) | this.l8(address);
};

/**
 * Loads 8-bit values from the main memory.
 * @param {Uint16} address - A load address.
 * @return {Uint8} 8-bit value read from the address.
 */
Jsj6502.prototype.l8 = function (address) {
    if (this.logMemory) {
        this.log('*** load $' + ToHex(address, 4) + ' => $' +
                 ToHex(this.memory[address]));
    }
    var bank = address >> 8;
    if (this.io[bank])
        return this.io[bank](address);
    return this.memory[address];
};

/**
 * Loads 16-bit values from the main memory in zero page.
 * @param {Uint8} address - A load address in zero page.
 * @return {Uint16} 16-bit value read from the address.
 */
Jsj6502.prototype.lz16 = function (address) {
    return (this.l8((address + 1) & 0xff) << 8) | this.l8(address & 0xff);
};

/**
 * Storess 8-bit values to the main memory.
 * @param {Uint16} address - A load address.
 * @param {Uint8} value - A 8-bit value to store.
 */
Jsj6502.prototype.s8 = function (address, value) {
    if (this.logMemory)
        this.log('*** store $' + ToHex(address, 4) + ' <= $' + ToHex(value));
    // TODO: Should check if this operation invalidate current running trace.
    var bank = address >> 8;
    if (!this.writable[bank])
        return;
    if (this.io[bank])
        return this.io[bank](address, value);
    if (this.cache[address]) {
        if (this.logCache) {
            this.log('translation cache invalidation: $' + ToHex(address, 4));
            CheckIn('invalidation');
        }
        var traces = this.cache[address];
        for (var key in traces) {
            var trace = traces[key];
            trace.valid = false;
            for (var pc = trace.start; pc <= trace.end; ++pc)
                delete this.cache[pc][trace.start];
        }
        this.cache[address] = undefined;
        if (this.logCache)
            CheckOut('invalidation');
    }
    this.memory[address] = value;
};

/**
 * Pops 16-bit values from the stack.
 * @return {Uint16} value - A 16-bit value.
 */
Jsj6502.prototype.pop16 = function (value) {
    ++this.S[0];
    var l = this.stack[this.S[0]];
    ++this.S[0];
    var h = this.stack[this.S[0]];
    var data = h << 8 | l;
    if (this.logMemory)
        this.log('*** load $' + ToHex(0xff + this.S[0], 4) + ' => $' + ToHex(data, 4));
    return data;
};

/**
 * Pops 8-bit values from the stack.
 * @return {Uint8} value - A 8-bit value.
 */
Jsj6502.prototype.pop8 = function (value) {
    ++this.S[0];
    if (this.logMemory)
        this.log('*** load $' + ToHex(0x100 + this.S[0], 4) + ' => $' + ToHex(this.stack[this.S[0]]));
    return this.stack[this.S[0]];
};

/**
 * Pushes 16-bit values to the stack.
 * @param {Uint16} value - A 16-bit value to store.
 */
Jsj6502.prototype.push16 = function (value) {
    if (this.logMemory)
        this.log('*** store $' + ToHex(0x100 + this.S[0], 4) + ' <= $' + ToHex(value, 4));
    // Do not handle cache invalidation under assumption that PC does not jump
    // into the stack.
    // Note: --this.S[0] seems not work correctly against ArrayBuffer.
    this.stack[this.S[0]] = value >> 8;
    this.S[0]--;
    this.stack[this.S[0]] = value;
    this.S[0]--;
};

/**
 * Pushes 8-bit values to the stack.
 * @param {Uint8} value - A 8-bit value to store.
 */
Jsj6502.prototype.push8 = function (value) {
    if (this.logMemory)
        this.log('*** store $' + ToHex(0x100 + this.S[0], 4) + ' <= $' + ToHex(value));
    // Do not handle cache invalidation under assumption that PC does not jump
    // into the stack.
    this.stack[this.S[0]] = value;
    this.S[0]--;
};

/**
 * Updates condition flags..
 * @param {Uint8} value - A 8-bit value to store.
 */
Jsj6502.prototype.updateFlags = function () {
    this.N = this.SR[0] >> 7;
    this.V = (this.SR[0] >> 6) & 1;
    // B flag can not be modified by any software operation.
    // this.B = (this.SR[0] >> 4) & 1;
    this.D = (this.SR[0] >> 3) & 1;
    this.I = (this.SR[0] >> 2) & 1;
    this.Z = (this.SR[0] >> 1) & 1;
    this.C = this.SR[0] & 1;
};

/**
 * Updates processor status register.
 * @param {Uint8} value - A 8-bit value to store.
 */
Jsj6502.prototype.updatePSR = function () {
    this.SR[0] = (this.N << 7) | (this.V << 6) | (1 << 5) | (this.B << 4) |
                 (this.D << 3) | (this.I << 2) | (this.Z << 1) | this.C;
    return this.SR[0];
};

/**
 * Compiles a basic block.
 * @return {object} An code object.
 */
Jsj6502.prototype._compile = function () {
    if (this.logJit) {
        this.log('binary translation: pc=$' + ToHex(this.PC[0], 4));
        CheckIn('compile');
    }
    // Binary code generation.
    var code = {
        start: this.PC[0],
        end: 0,
        valid: true,
        trace: undefined
    };
    var pc = this.PC[0];
    var asm = [];
    for (;;) {
        // {
        //   size: {number} instruction size in bytes
        //   code: {Array<string>} compiled code
        //   in: {object} dictionary to know input set of conditions
        //   out: {object} dictionary to know output set of conditions
        //   quit: {boolean} indicates an end of basic block
        // }
        var data = this._decode(pc);
        if (this.enableStepCompile && !data.quit)
            data.code.push('t.PC[0]=' + (pc + data.size));
        if (this.logDecode) {
            this.log('decode: pc=$' + ToHex(pc, 4) + ', ' +
                     Jsj6502.OpDescriptions[this.l8(pc)] + ', op=' +
                     ToHex(this.l8(pc)) + ', code=' + data.code.join(';'));
        }
        if (this.logDump || this.logJit)
            data.pc = pc;
        asm.push(data);
        if (data.quit || this.enableStepCompile) {
            code.end = pc + data.size - 1;
            break;
        }
        pc += data.size;
    }

    // Optimization.
    var optCode = [];
    var asmLength = asm.length;

    // 1. Eliminate unused condition calculations.
    for (var i = 0; i < asmLength; ++i) {
        if (!asm[i].out)
            continue;
        var flags = Object.keys(asm[i].out);
        for (var n = 0; n < flags.length; ++n) {
            var flag = flags[n];
            for (var j = i + 1; j < asmLength; ++j) {
                if (asm[j].in && asm[j].in[flag])
                    break;
                if (asm[j].out && asm[j].out[flag]) {
                    if (this.enableConditionOptimization) {
                        if (this.logJit) {
                            this.log('eliminating condition at $' +
                                     ToHex(asm[i].pc, 4) + ': ' +
                                     asm[i].out[flag]);
                        }
                        asm[i].out[flag] = null;
                    }
                    break;
                }
            }
            if (asm[i].out[flag]) {
                if (this.logJit) {
                    this.log('condition at $' + ToHex(asm[i].pc, 4) +
                             ': ' + asm[i].out[flag]);
                }
                asm[i].code.push(asm[i].out[flag]);
            }
        }
    }
    for (var i = 0; i < asmLength; ++i) {
        if (this.logDump)
            asm[i].code.push('t.dump(' + asm[i].pc + ')');
        optCode.push(asm[i].code.join(';'));
    }

    var trace = optCode.join(';');
    code.trace = new Function('t', trace);
    if (this.logJit) {
        CheckOut('compile');
        this.log('trace: ' + trace);
    }
    return code;
};

/**
 * Decodes an instruction.
 * @param {Uint8} pc - Program counter to fetch the next instruction.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decode = function (pc) {
    var inst = this.l8(pc);
    switch (inst) {
        case 0x00:  // BRK
            return this._decodeBRK(pc);
        case 0x01:  // ORA - (Indirect, X)
            return this._decodeLogicalOp('|',
                                         this._fromIndexedIndirect(pc + 1));
        case 0x05:  // ORA - Zero Page
            return this._decodeLogicalOp('|', this._fromZero(pc + 1));
        case 0x06:  // ASL - Zero Page
            return this._decodeASL(this._fromZero(pc + 1),
                                   this._toZero(pc + 1));
        case 0x08:  // PHP
            return this._decodePush('t.updatePSR()', true);
        case 0x09:  // ORA - Immediate
            return this._decodeLogicalOp('|', this._fromImm8(pc + 1));
        case 0x0a:  // ASL - Accumulator
            return this._decodeASL('t.A[0]', ['t.A[0]=', ''], 1);
        case 0x0d:  // ORA - Absolute
            return this._decodeLogicalOp('|', this._fromAbs8(pc + 1), 3);
        case 0x0e:  // ASL - Absolute
            return this._decodeASL(this._fromAbs8(pc + 1),
                                   this._toAbs(pc + 1), 3);

        case 0x10:  // BPL
            return this._decodeB(pc, 't.N==0', { 'n': true });
        case 0x11:  // ORA - (Indirect), Y
            return this._decodeLogicalOp('|', this._fromIndirectIndex(pc + 1));
        case 0x15:  // ORA - Zero Page, X
            return this._decodeLogicalOp('|',
                                         this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0x16:  // ASL - Zero Page, X
            return this._decodeASL(this._fromZeroIndex(pc + 1, 't.X[0]'),
                                   this._toZeroIndex(pc + 1, 't.X[0]'));

        case 0x18:  // CLC
            return this._decodeUpdateFlag({c: 't.C=0'});
        case 0x19:  // ORA - Absolute, Y
            return this._decodeLogicalOp(
                    '|', this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'), 3);
        case 0x1d:  // ORA - Absolute, X
            return this._decodeLogicalOp(
                    '|', this._fromAbsoluteIndexed(pc + 1, 't.X[0]'), 3);
        case 0x1e:  // ASL - Absolute, X
            return this._decodeASL(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   this._toAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);

        case 0x20:  // JSR
            return this._decodeJSR(pc);
        case 0x21:  // AND - (Indirect, X)
            return this._decodeLogicalOp('&', this._fromIndexedIndirect(pc + 1),
                                         2);
        case 0x24:  // BIT - Zero Page
            return this._decodeBIT(this._fromZero(pc + 1));
        case 0x25:  // AND - Zero Page
            return this._decodeLogicalOp('&', this._fromZero(pc + 1));
        case 0x26:  // ROL - Zero Page
            return this._decodeROL(this._fromZero(pc + 1),
                                   this._toZero(pc + 1));
        case 0x28:  // PLP
            return this._decodePull(['t.SR[0]=', ';t.updateFlags()'], true);
        case 0x29:  // AND - Immediate
            return this._decodeLogicalOp('&', this._fromImm8(pc + 1));
        case 0x2a:  // ROL - Accumulator
            return this._decodeROL('t.A[0]', ['t.A[0]=', ''], 1);
        case 0x2c:  // BIT - Absolute
            return this._decodeBIT(this._fromAbs8(pc + 1), 3);
        case 0x2d:  // AND - Absolute
            return this._decodeLogicalOp('&', this._fromAbs8(pc + 1), 3);
        case 0x2e:  // ROL - Absolute
            return this._decodeROL(this._fromAbs8(pc + 1),
                                   this._toAbs(pc + 1), 3);

        case 0x30:  // BMI
            return this._decodeB(pc, 't.N', { 'n': true });
        case 0x31:  // AND - (Indirect), Y
            return this._decodeLogicalOp('&', this._fromIndirectIndex(pc + 1));
        case 0x35:  // AND - Zero Page, X
            return this._decodeLogicalOp('&',
                                         this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0x36:  // ROL - Zero Page, X
            return this._decodeROL(this._fromZeroIndex(pc + 1, 't.X[0]'),
                                   this._toZeroIndex(pc + 1, 't.X[0]'));
        case 0x38:  // SEC
            return this._decodeUpdateFlag({c: 't.C=1'});
        case 0x39:  // AND - Absolute, Y
            return this._decodeLogicalOp(
                    '&', this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'), 3);
        case 0x3d:  // AND - Absolute, X
            return this._decodeLogicalOp(
                    '&', this._fromAbsoluteIndexed(pc + 1, 't.X[0]'), 3);
        case 0x3e:  // ROL - Absolute, X
            return this._decodeROL(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   this._toAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);

        case 0x40:  // RTI
            return this._decodeRTI();
        case 0x41:  // EOR - (Indirect), X
            return this._decodeLogicalOp('^',
                                         this._fromIndexedIndirect(pc + 1));
        case 0x45:  // EOR - Zero Page
            return this._decodeLogicalOp('^', this._fromZero(pc + 1));
        case 0x46:  // LSR - Zero Page
            return this._decodeLSR(this._fromZero(pc + 1),
                                   this._toZero(pc + 1));
        case 0x48:  // PHA
            return this._decodePush('t.A[0]');
        case 0x49:  // EOR - Immediate
            return this._decodeLogicalOp('^', this._fromImm8(pc + 1));
        case 0x4a:  // LSR - Accumulator
            return this._decodeLSR('t.A[0]', ['t.A[0]=', ''], 1);
        case 0x4c:  // JMP - Absolute [absolute address is immediate value]
            return this._decodeJMP(pc, this._fromImm16(pc + 1));
        case 0x4d:  // EOR - Absolute
            return this._decodeLogicalOp('^', this._fromAbs8(pc + 1), 3);
        case 0x4e:  // LSR - Absolute
            return this._decodeLSR(this._fromAbs8(pc + 1),
                                   this._toAbs(pc + 1), 3);

        case 0x50:  // BVC
            return this._decodeB(pc, 't.V==0', { 'v': true });
        case 0x51:  // EOR - (Indirect), Y
            return this._decodeLogicalOp('^', this._fromIndirectIndex(pc + 1));
        case 0x55:  // EOR - Zero Page, X
            return this._decodeLogicalOp(
                    '^', this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0x56:  // LSR - Zero Page, X
            return this._decodeLSR(this._fromZeroIndex(pc + 1, 't.X[0]'),
                                   this._toZeroIndex(pc + 1, 't.X[0]'));

        case 0x58:  // CLI
            return this._decodeUpdateFlag({i: 't.I=0'});
        case 0x59:  // EOR - Absolute, Y
            return this._decodeLogicalOp(
                    '^', this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'), 3);
        case 0x5d:  // EOR - Absolute, X
            return this._decodeLogicalOp(
                    '^', this._fromAbsoluteIndexed(pc + 1, 't.X[0]'), 3);
        case 0x5e:  // LSR - Absolute, X
            return this._decodeLSR(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   this._toAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);

        case 0x60:  // RTS
            return this._decodeRTS();
        case 0x61:  // ADC - (Indirect, X)
            return this._decodeADC(this._fromIndexedIndirect(pc + 1));
        case 0x65:  // ADC - Zero Page
            return this._decodeADC(this._fromZero(pc + 1));
        case 0x66:  // ROR - Zero Page
            return this._decodeROR(this._fromZero(pc + 1),
                                   this._toZero(pc + 1));
        case 0x68:  // PLA
            return this._decodePull(['t.A[0]=', '']);
        case 0x69:  // ADC - Immediate
            return this._decodeADC(this._fromImm8(pc + 1));
        case 0x6a:  // ROR - Accumulator
            return this._decodeROR('t.A[0]', ['t.A[0]=', ''], 1);
        case 0x6c:  // JMP - Indirect [indirect address is absolute value]
            return this._decodeJMP(pc, this._fromAbs16(pc + 1));
        case 0x6d:  // ADC - Absolute
            return this._decodeADC(this._fromAbs8(pc + 1), 3);
        case 0x6e:  // ROR - Absolute
            return this._decodeROR(this._fromAbs8(pc + 1),
                                   this._toAbs(pc + 1), 3);

        case 0x70:  // BVS
            return this._decodeB(pc, 't.V', { 'v': true });
        case 0x71:  // ADC - (Indirect), Y
            return this._decodeADC(this._fromIndirectIndex(pc + 1));
        case 0x75:  // ADC - Zero Page, X
            return this._decodeADC(this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0x76:  // ROR - Zero Page, X
            return this._decodeROR(this._fromZeroIndex(pc + 1, 't.X[0]'),
                                   this._toZeroIndex(pc + 1, 't.X[0]'));
        case 0x78:  // SEI
            return this._decodeUpdateFlag({i: 't.I=1'});
        case 0x79:  // ADC - Absolute, Y
            return this._decodeADC(this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'),
                                   3);
        case 0x7d:  // ADC - Absolute, X
            return this._decodeADC(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);
        case 0x7e:  // ROR - Absolute, X
            return this._decodeROR(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   this._toAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);

        case 0x81:  // STA - (Indirect, X)
            return this._decodeST('A', this._toIndexedIndirect(pc + 1));
        case 0x84:  // STY - Zero Page
            return this._decodeST('Y', this._toZero(pc + 1));
        case 0x85:  // STA - Zero Page
            return this._decodeST('A', this._toZero(pc + 1));
        case 0x86:  // STX - Zero Page
            return this._decodeST('X', this._toZero(pc + 1));
        case 0x88:  // DEY
            return this._decodeDEC('t.Y[0]', ['t.Y[0]=', ''], 1);
        case 0x8a:  // TXA
            return this._decodeT('X', 'A');
        case 0x8c:  // STY - Absolute
            return this._decodeST('Y', this._toAbs(pc + 1), 3);
        case 0x8d:  // STA - Absolute
            return this._decodeST('A', this._toAbs(pc + 1), 3);
        case 0x8e:  // STX - Absolute
            return this._decodeST('X', this._toAbs(pc + 1), 3);

        case 0x90:  // BCC
            return this._decodeB(pc, 't.C==0', { 'c': true });
        case 0x91:  // STA - (Indirect), Y
            return this._decodeST('A', this._toIndirectIndex(pc + 1));
        case 0x94:  // STY - Zero Page, X
            return this._decodeST('Y', this._toZeroIndex(pc + 1, 't.X[0]'));
        case 0x95:  // STA - Zero Page, X
            return this._decodeST('A', this._toZeroIndex(pc + 1, 't.X[0]'));
        case 0x96:  // STX - Zero Page, Y
            return this._decodeST('X', this._toZeroIndex(pc + 1, 't.Y[0]'));
        case 0x98:  // TYA
            return this._decodeT('Y', 'A');
        case 0x99:  // STA - Absolute, Y
            return this._decodeST('A',
                                  this._toAbsoluteIndexed(pc + 1, 't.Y[0]'), 3);
        case 0x9a:  // TXS
            return this._decodeT('X', 'S', true);
        case 0x9d:  // STA - Absolute, X
            return this._decodeST('A',
                                  this._toAbsoluteIndexed(pc + 1, 't.X[0]'), 3);

        case 0xa0:  // LDY - Immediate
            return this._decodeLD('Y', this._fromImm8(pc + 1));
        case 0xa1:  // LDA - (Indirect, X)
            return this._decodeLD('A', this._fromIndexedIndirect(pc + 1));
        case 0xa2:  // LDX - Immediate
            return this._decodeLD('X', this._fromImm8(pc + 1));
        case 0xa4:  // LDY - Zero Page
            return this._decodeLD('Y', this._fromZero(pc + 1));
        case 0xa5:  // LDA - Zero Page
            return this._decodeLD('A', this._fromZero(pc + 1));
        case 0xa6:  // LDX - Zero Page
            return this._decodeLD('X', this._fromZero(pc + 1));
        case 0xa8:  // TAY
            return this._decodeT('A', 'Y');
        case 0xa9:  // LDA - Immediate
            return this._decodeLD('A', this._fromImm8(pc + 1));
        case 0xaa:  // TAX
            return this._decodeT('A', 'X');
        case 0xac:  // LDY - Absolute
            return this._decodeLD('Y', this._fromAbs8(pc + 1), 3);
        case 0xad:  // LDA - Absolute
            return this._decodeLD('A', this._fromAbs8(pc + 1), 3);
        case 0xae:  // LDX - Absolute
            return this._decodeLD('X', this._fromAbs8(pc + 1), 3);

        case 0xb0:  // BCS
            return this._decodeB(pc, 't.C', { 'c': true });
        case 0xb1:  // LDA - (Indirect), Y
            return this._decodeLD('A', this._fromIndirectIndex(pc + 1));
        case 0xb4:  // LDY - Zero Page, X
            return this._decodeLD('Y', this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0xb5:  // LDA - Zero Page, X
            return this._decodeLD('A', this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0xb6:  // LDX - Zero Page, Y
            return this._decodeLD('X', this._fromZeroIndex(pc + 1, 't.Y[0]'));
        case 0xb8:  // CLV
            return this._decodeUpdateFlag({v: 't.V=0'});
        case 0xb9:  // LDA - Absolute, Y
            return this._decodeLD('A',
                                  this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'),
                                  3);
        case 0xba:  // TSX
            return this._decodeT('S', 'X');
        case 0xbc:  // LDY - Absolute, X
            return this._decodeLD('Y',
                                  this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                  3);
        case 0xbd:  // LDA - Absolute, X
            return this._decodeLD('A',
                                  this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                  3);
        case 0xbe:  // LDX - Absolute, Y
            return this._decodeLD('X',
                                  this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'),
                                  3);

        case 0xc0:  // CPY - Immediate
            return this._decodeCP('Y', this._fromImm8(pc+1));
        case 0xc1:  // CMP - (Indirect, X)
            return this._decodeCP('A', this._fromIndexedIndirect(pc + 1));
        case 0xc4:  // CPY - Zero Page
            return this._decodeCP('Y', this._fromZero(pc + 1));
        case 0xc5:  // CMP - Zero Page
            return this._decodeCP('A', this._fromZero(pc + 1));
        case 0xc6:  // DEC - Zero Page
            return this._decodeDEC(this._fromZero(pc + 1),
                                   this._toZero(pc + 1));
        case 0xc8:  // INY
            return this._decodeINC('t.Y[0]', ['t.Y[0]=', ''], 1);
        case 0xc9:  // CMP - Immediate
            return this._decodeCP('A', this._fromImm8(pc+1));
        case 0xca:  // DEX
            return this._decodeDEC('t.X[0]', ['t.X[0]=', ''], 1);
        case 0xcc:  // CPY - Absolute
            return this._decodeCP('Y', this._fromAbs8(pc+1), 3);
        case 0xcd:  // CMP - Absolute
            return this._decodeCP('A', this._fromAbs8(pc+1), 3);
        case 0xce:  // DEC - Absolute
            return this._decodeDEC(this._fromAbs8(pc + 1),
                                   this._toAbs(pc + 1), 3);

        case 0xd0:  // BNE
            return this._decodeB(pc, 't.Z==0', { 'z': true });
        case 0xd1:  // CMP - (Indirect), Y
            return this._decodeCP('A', this._fromIndirectIndex(pc + 1));
        case 0xd5:  // CMP - Zero Page, X
            return this._decodeCP('A', this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0xd6:  // DEC - Zero Page, X
            return this._decodeDEC(this._fromZeroIndex(pc + 1, 't.X[0]'),
                                   this._toZeroIndex(pc + 1, 't.X[0]'));
        case 0xd8:  // CLD
            return this._decodeUpdateFlag({d: 't.D=0'});
        case 0xd9:  // CMP - Absolute, Y
            return this._decodeCP('A',
                                  this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'),
                                  3);
        case 0xdd:  // CMP - Absolute, X
            return this._decodeCP('A',
                                  this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                  3);
        case 0xde:  // DEC - Absolute, X
            return this._decodeDEC(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   this._toAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);

        case 0xe0:  // CPX - Immediate
            return this._decodeCP('X', this._fromImm8(pc+1));
        case 0xe1:  // SBC - (Indirect, X)
            return this._decodeSBC(this._fromIndexedIndirect(pc + 1));
        case 0xe4:  // CPX - Zero Page
            return this._decodeCP('X', this._fromZero(pc + 1));
        case 0xe5:  // SBC - Zero Page
            return this._decodeSBC(this._fromZero(pc + 1));
        case 0xe6:  // INC - Zero Page
            return this._decodeINC(this._fromZero(pc + 1),
                                   this._toZero(pc + 1), 2);
        case 0xe8:  // INX
            return this._decodeINC('t.X[0]', ['t.X[0]=', ''], 1);
        case 0xe9:  // SBC - Immediate
            return this._decodeSBC(this._fromImm8(pc+1));
        case 0xea:  // NOP
            return { code: [], size: 1 };
        case 0xec:  // CPX - Absolute
            return this._decodeCP('X', this._fromAbs8(pc+1), 3);
        case 0xed:  // SBC - Absolute
            return this._decodeSBC(this._fromAbs8(pc + 1), 3);
        case 0xee:  // INC - Absolute
            return this._decodeINC(this._fromAbs8(pc + 1),
                                   this._toAbs(pc + 1), 3);

        case 0xf0:  // BEQ
            return this._decodeB(pc, 't.Z', { 'z': true });
        case 0xf1:  // SBC - (Indirect), Y
            return this._decodeSBC(this._fromIndirectIndex(pc + 1));
        case 0xf5:  // SBC - Zero Page, X
            return this._decodeSBC(this._fromZeroIndex(pc + 1, 't.X[0]'));
        case 0xf6:  // INC - Zero Page, X
            return this._decodeINC(this._fromZeroIndex(pc + 1, 't.X[0]'),
                                   this._toZeroIndex(pc + 1, 't.X[0]'))
        case 0xf8:  // SED
            return this._decodeUpdateFlag({d: 't.D=1'});
        case 0xf9:  // SBC - Absolute, Y
            return this._decodeSBC(this._fromAbsoluteIndexed(pc + 1, 't.Y[0]'),
                                   3);
        case 0xfd:  // SBC - Absolute, X
            return this._decodeSBC(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);
        case 0xfe:  // INC - Absolute, X
            return this._decodeINC(this._fromAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   this._toAbsoluteIndexed(pc + 1, 't.X[0]'),
                                   3);

        default:    // NOP
            return { code: [] };
    }
    Assert(false, 'unknown instruction: $' + ToHex(inst));
};

/**
 * Decode an absolute 16-bit value for load.
 * @param {Uint16} addr - Address from where a value is loaded.
 * @return {string} - A decoded code to get the absolute value.
 */
Jsj6502.prototype._fromAbs16 = function (addr) {
    if (!this.writable[addr >> 8])
        return 't.l16(' + this.l16(addr) + ')';
    return 't.l16(t.l16(' + addr + '))';
};

/**
 * Decode an absolute 8-bit value for load.
 * @param {Uint16} addr - Address from where a value is loaded.
 * @return {string} - A decoded code to get the absolute value.
 */
Jsj6502.prototype._fromAbs8 = function (addr) {
    if (!this.writable[addr >> 8])
        return 't.l8(' + this.l16(addr) + ')';
    return 't.l8(t.l16(' + addr + '))';
};

/**
 * Decode an absolute indexed 8-bit value for load.
 * @param {Uint16} addr - Address from where a value is loaded.
 * @param {string} index - An offset in the page.
 * @return {string} - A decoded code to get the absolute value.
 */
Jsj6502.prototype._fromAbsoluteIndexed = function (addr, index) {
    if (!this.writable[addr >> 8])
        return 't.l8(' + this.l16(addr) + '+' + index + ')';
    return 't.l8(t.l16(' + addr + ')+' + index + ')';
};

/**
 * Decode an immediate 16-bit value for load.
 * @param {Uint16} addr - Address from where a value is decoded.
 * @return {string} - A decoded code to get the immediate value.
 */
Jsj6502.prototype._fromImm16 = function (addr) {
    if (!this.writable[addr >> 8])
        return '' + this.l16(addr);
    return 't.l16(' + addr + ')';
};

/**
 * Decode an immediate 8-bit value for load.
 * @param {Uint16} addr - Address from where a value is decoded.
 * @return {string} - A decoded code to get the immediate value.
 */
Jsj6502.prototype._fromImm8 = function (addr) {
    if (!this.writable[addr >> 8])
        return '' + this.l8(addr);
    return 't.l8(' + addr + ')';
};

/**
 * Decode an indexed indirect value for load.
 * @param {Uint16} addr - Address from where a zero page base address is loaded.
 * @return {string} - A decoded code to get the immediate value.
 */
Jsj6502.prototype._fromIndexedIndirect = function (addr) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    return 't.l8(t.lz16(' + base + '+t.X[0]))';
};

/**
 * Decode an indirect indexed value for load.
 * @param {Uint16} addr - Address in zdro page to load base address.
 * @return {string} - A decoded code to get the immediate value.
 */
Jsj6502.prototype._fromIndirectIndex = function (addr) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    return 't.l8(t.lz16(' + base + ')+t.Y[0])';
};

/**
 * Decode a zero page addressing value for load.
 * @param {Uint16} addr - Address from where a zero page address is loaded.
 * @return {string} - A decoded code to get the zero page addressing value.
 */
Jsj6502.prototype._fromZero = function (addr) {
    if (!this.writable[addr >> 8])
        return 't.l8(' + this.l8(addr) + ')';
    return 't.l8(t.l8(' + addr + '))';
};

/**
 * Decode an indexed zero page addressing value for load.
 * @param {Uint16} addr - Address from where a zero page address is loaded.
 * @return {string} - A decoded code to get the zero page addressing value.
 */
Jsj6502.prototype._fromZeroIndex = function (addr, offset) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    var index = '(' + base + '+' + offset + ')&0xff';
    return 't.l8(' + index + ')';
};

/**
 * Decode an absolute value for store.
 * @param {Uint16} addr - Address from where a value is decoded.
 * @return {Array<string>[2]} - A decoded code to set a value to the memory.
 */
Jsj6502.prototype._toAbs = function (addr) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l16(addr);
    else
        base = 't.l16(' + addr + ')';
    return ['t.s8(' + base + ',', ')'];
};

/**
 * Decode an absolute indexed 8-bit value for store.
 * @param {Uint16} addr - Base address from where a value is stored.
 * @param {string} index - An offset in the page relative to the base address.
 * @return {Array<string>[2]} - A decoded code to set a value to the memory.
 */
Jsj6502.prototype._toAbsoluteIndexed = function (addr, index) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l16(addr);
    else
        base = 't.l16(' + addr + ')';
    return ['t.s8(' + base + '+' + index + ',', ')'];
};

/**
 * Decode an indexed indirect value for store.
 * @param {Uint16} addr - Address in zdro page to load base address.
 * @return {Array<string>[2]} - A decoded code to set a value to the memory.
 */
Jsj6502.prototype._toIndexedIndirect = function (addr) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    return ['t.s8(t.lz16(' + base + '+t.X[0]),', ')'];
};

/**
 * Decode an indirect indexed value for store.
 * @param {Uint16} addr - Address in zdro page to load base address.
 * @return {Array<string>[2]} - A decoded code to set a value to the memory.
 */
Jsj6502.prototype._toIndirectIndex = function (addr) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    return ['t.s8(t.lz16(' + base + ')+t.Y[0],', ')'];
};

/**
 * Decode a zero page addressing value for store.
 * @param {Uint16} addr - Address from where a zero page address is loaded.
 * @return {Array<string>[2]} - A decoded code to set a value to the zero page.
 */
Jsj6502.prototype._toZero = function (addr) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    return ['t.s8(' + base + ',', ')'];
};

/**
 * Decode an indexed zero page addressing value for store.
 * @param {Uint16} addr - Address from where a zero page address is loaded.
 * @param {string} offset - Offset in the zero page
 * @return {Array<string>[2]} - A decoded code to set a value to the zero page.
 */
Jsj6502.prototype._toZeroIndex = function (addr, offset) {
    var base;
    if (!this.writable[addr >> 8])
        base = this.l8(addr);
    else
        base = 't.l8(' + addr + ')';
    var index = '(' + base + '+' + offset + ')&0xff';
    return ['t.s8(' + index + ',', ')'];
};

/**
 * Performes an ADC decimal operation.
 * @param {number} a - number.
 * @param {number} b - number.
 * @return {number} result.
 */
Jsj6502.prototype._adc = function (a, b) {
    var l = (a & 0x0f) + (b & 0x0f) + this.C;
    var h = (a >> 4) + (b >> 4);
    if (l > 9) {
        l += 6;
        h++;
    }
    if (h > 9)
        h += 6;
    return (h << 4) | (l & 0x0f);
};

/**
 * Decodes an ADC instruction.
 * @param {string} data - Data to set.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeADC = function (data, size) {
    return {
        code: [
            't.T[0]=t.A[0]',
            't.T[1]=' + data,
            't.TMP=t.D?t._adc(t.T[0],t.T[1]):(t.T[0]+t.T[1]+t.C)',
            't.A[0]=t.TMP'
        ],
        in: {
            c: true,
            d: true
        },
        out: {
            n: 't.N=t.A[0]>>7',
            z: 't.Z=t.A[0]?0:1',
            c: 't.C=(t.TMP&0xff00)?1:0',
            v: 't.V=(~(t.T[0]^t.T[1])&(t.T[0]^t.A[0]))>>7'
        },
        size: size || 2
    };
};

/**
 * Decodes a ASL instruction.
 * @param {string} data - String representing input data.
 * @param {Array<string>[2]} store - Code prolog and epilog to store a data.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeASL = function (data, store, size) {
    return {
        code: [
            't.T[0]=' + data,
            't.T[1]=t.T[0]<<1',
            store[0] + 't.T[1]' + store[1]
        ],
        out: {
            n: 't.N=t.T[1]>>7',
            z: 't.Z=t.T[1]?0:1',
            c: 't.C=t.T[0]>>7'
        },
        size: size || 2
    };
};

/**
 * Test A register's bits with a data.
 * @param {string} data - Data to test.
 * @param {number} [size=2] - Instruction total length.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeBIT = function (data, size) {
    return {
        code: [
            't.T[0]=' + data,
            't.T[1]=t.A[0]&t.T[0]'
        ],
        out: {
            n: 't.N=t.T[0]>>7',
            z: 't.Z=t.T[1]?0:1',
            v: 't.V=(t.T[0]>>6)&1'
        },
        size: size || 2
    };
};

/**
 * Decodes a B instruction.
 * @param {Uint16} pc - Program counter to fetch the next instruction.
 * @param {string} c - A condition flag.
 * @param {object} inc - An object representing input conditions.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeB = function (pc, c, inc) {
    var offset = this.l8(pc + 1);
    if (offset > 127)
        offset -= 256;
    var code = [
        't.PC[0]=(' + c + ')?' + (pc + 2 + offset) + ':' + (pc + 2)
    ];
    if (this.enableInfiniteLoopDetector && offset == -2)
        code.push('console.assert(!(' + c + '), \'infinite loop bxx at $' +
                  ToHex(pc, 4) + '\')');
    return {
        code: code,
        in: inc,
        size: 2,
        quit: true
    };
};

/**
 * Decodes a BRK instruction.
 * @param {Uint16} pc - Program counter to fetch the next instruction.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeBRK = function (pc) {
    return {
        code: [
            't.push16(' + (pc + 2) + ')',
            't.push8(t.updatePSR())',
            't.PC[0]=t.l16(' + Jsj6502.VECTOR_BRK + ')'
        ],
        in: {
            n: true,
            v: true,
            d: true,
            i: true,
            z: true,
            c: true
        },
        out: {
            i: 't.I=1',
        },
        size: 1,
        quit: true
    };
};

/**
 * Decodes a CP instruction.
 * @param {string} reg - A register to compare.
 * @param {string} data - Data to push.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeCP = function (reg, data, size) {
    return {
        code: [
            't.T[0]=' + data
        ],
        out: {
            n: 't.N=((t.' + reg + '[0]-t.T[0])>>7)&1',
            z: 't.Z=(t.' + reg + '[0]!=t.T[0])?0:1',
            c: 't.C=(t.T[0]>t.' + reg + '[0])?0:1'
        },
        size: size || 2
    };
};

/**
 * Decodes a DE instruction.
 * @param {string} data - A input data to increase.
 * @param {Array<string>[2]} code - An array of code for prolog and epilog.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeDEC = function (data, code, size) {
    return {
        code: [
            't.T[0]=' + data + '-1',
            code[0] + 't.T[0]' + code[1]
        ],
        out: {
            n: 't.N=t.T[0]>>7',
            z: 't.Z=t.T[0]?0:1'
        },
        size: size || 2
    };
};

/**
 * Decodes a IN instruction.
 * @param {string} data - A input data to increase.
 * @param {Array<string>[2]} code - An array of code for prolog and epilog.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeINC = function (data, code, size) {
    return {
        code: [
            't.T[0]=' + data + '+1',
            code[0] + 't.T[0]' + code[1]
        ],
        out: {
            n: 't.N=t.T[0]>>7',
            z: 't.Z=t.T[0]?0:1'
        },
        size: size || 2
    };
};

/**
 * Decodes a JMP instruction.
 * @param {Uint16} pc - Program counter.
 * @param {string} addr - Address to jump.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeJMP = function (pc, addr) {
    var code = [
        't.PC[0]=' + addr
    ];
    if (this.enableInfiniteLoopDetector)
        code.push('console.assert(t.PC[0]!=' + pc + ', \'infinite loop jmp at $' +
                  ToHex(pc, 4) + '\')');
    return {
        code: code,
        size: 3,
        quit: true
    };
};

/**
 * Decodes a JSR instruction.
 * @param {Uint16} pc - Program counter to fetch the next instruction.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeJSR = function (pc) {
    return {
        code: [
            't.push16(' + (pc + 2) + ')',
            't.PC[0]=t.l16(' + (pc + 1) + ')'
        ],
        size: 3,
        quit: true
    };
};

/**
 * Decodes a LD instruction.
 * @param {string} reg - Register name to load.
 * @param {string} data - Data to set.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeLD = function (reg, data, size) {
    return {
        code: [
            't.' + reg + '[0]=' + data
        ],
        out: {
            n: 't.N=t.' + reg + '[0]>>7',  // TODO: Check the spec
            z: 't.Z=t.' + reg + '[0]?0:1'
        },
        size: size || 2
    };
};

/**
 * Decodes a LSR instruction.
 * @param {string} data - String representing input data.
 * @param {Array<string>[2]} store - Code prolog and epilog to store a data.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeLSR = function (data, store, size) {
    return {
        code: [
            't.T[0]=' + data,
            't.T[1]=t.T[0]>>1',
            store[0] + 't.T[1]' + store[1]
        ],
        out: {
            n: 't.N=0',
            z: 't.Z=t.T[1]?0:1',
            c: 't.C=t.T[0]&1'
        },
        size: size || 2
    };
};

/**
 * Decode an Logical operation instruction.
 * @param {string} op - Operation to adopt.
 * @param {string} data - Data to set.
 * @param {number} [size=2] - Instruction total length.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeLogicalOp = function (op, data, size) {
    return {
        code: [
            't.A[0]=t.A[0]' + op + data,
        ],
        out: {
            n: 't.N=t.A[0]>>7',
            z: 't.Z=t.A[0]?0:1'
        },
        size: size || 2
    };
};

/**
 * Decodes a PHA instruction.
 * @param {string} data - Data to push.
 * @param {boolean} update - Update condition flags before pushing.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodePush = function (data, update) {
    var inFlags = {
        n: true,
        v: true,
        d: true,
        i: true,
        z: true,
        c: true
    };
    return {
        code: [
            't.push8(' + data + ')'
        ],
        in: update ? inFlags : {},
        size: 1
    };
};

/**
 * Decodes a PLA instruction.
 * @param {Array<string>[2]} code - An array of code for prolog and epilog.
 * @param {boolean} noupdate - Do not update condition flags.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodePull = function (code, noupdate) {
    var out = {
        n: 't.N=t.T[0]>>7',
        z: 't.Z=t.T[0]?0:1'
    };
    return {
        code: [
            't.T[0]=t.pop8()',
            code[0] + 't.T[0]' + code[1]
        ],
        out: noupdate ? {} : out,
        size: 1
    };
};

/**
 * Decodes a ROL instruction.
 * @param {string} data - String representing input data.
 * @param {Array<string>[2]} store - Code prolog and epilog to store a data.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeROL = function (data, store, size) {
    return {
        code: [
            't.T[0]=' + data,
            't.T[1]=(t.T[0]<<1)+t.C',
            store[0] + 't.T[1]' + store[1]
        ],
        out: {
            n: 't.N=t.T[1]>>7',
            z: 't.Z=t.T[1]?0:1',
            c: 't.C=t.T[0]>>7'
        },
        size: size || 2
    };
};

/**
 * Decodes a ROR instruction.
 * @param {string} data - String representing input data.
 * @param {Array<string>[2]} store - Code prolog and epilog to store a data.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeROR = function (data, store, size) {
    return {
        code: [
            't.T[0]=' + data,
            't.T[1]=(t.T[0]>>1)+(t.C<<7)',
            store[0] + 't.T[1]' + store[1]
        ],
        out: {
            n: 't.N=t.T[1]>>7',
            z: 't.Z=t.T[1]?0:1',
            c: 't.C=t.T[0]&1'
        },
        size: size || 2
    };
};

/**
 * Decodes a RTI instruction.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeRTI = function () {
    return {
        code: [
            't.SR[0]=t.pop8()',
            't.updateFlags()',
            't.PC[0]=t.pop16()'
        ],
        out: {
            n: true,
            v: true,
            d: true,
            i: true,
            z: true,
            c: true
        },
        size: 1,
        quit: true
    };
};

/**
 * Decodes a RTS instruction.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeRTS = function () {
    return {
        code: [
            't.PC[0]=t.pop16()+1'
        ],
        size: 1,
        quit: true
    };
};

/**
 * Performes an SBC decimal operation.
 * @param {number} a - number.
 * @param {number} b - number.
 * @return {number} result.
 */
Jsj6502.prototype._sbc = function (a, b) {
    var l = ((a & 0x0f) - (b & 0x0f) -1 + this.C) & 0x1f;
    var h = (a >> 4) - (b >> 4)
    if (l > 9) {
        l -= 6;
        h--;
    }
    h = h & 0x1f;
    if (h > 9)
        h -= 6;
    return (h << 4) | l;
};

/**
 * Decodes a SBC instruction.
 * @param {string} data - A input data to increase.
 * @param {number} [size=2] - code size.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeSBC = function (data, size) {
    return {
        code: [
            't.T[0]=t.A[0]',
            't.T[1]=' + data,
            't.TMP=t.D?this._sbc(t.T[0],t.T[1]):(t.T[0]-t.T[1]-1+t.C)',
            'if(t.D)t.TMP=0',
            't.A[0]=t.TMP'
        ],
        in: {
            c: true,
            d: true
        },
        out: {
            n: 't.N=t.A[0]>>7',
            z: 't.Z=t.A[0]?0:1',
            c: 't.C=(t.TMP&0xff00)?0:1',
            v: 't.V=((t.T[0]^t.T[1])&(t.T[0]^t.A[0])&0x80)?1:0'
        },
        size: size || 2
    };
};

/**
 * Decodes a ST instruction.
 * @param {string} reg - Register name to store.
 * @param {Array<string>[2]} store - Code prolog and epilog to store a data.
 * @param {number} [size=2] - Instruction total length.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeST = function (reg, store, size) {
    return {
        code: [
            store[0] + 't.' + reg + '[0]' + store[1]
        ],
        size: size || 2
    };
};

/**
 * Decodes a T instruction.
 * @param {string} reg - Register name to store.
 * @param {Array<string>[2]} store - Code prolog and epilog to store a data.
 * @param {boolean} txs - A flag to indicate TXS instruction.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeT = function (from, to, txs) {
    var transfer = [
        't.' + to + '[0]=t.' + from + '[0]'
    ];
    var out = txs ? {} : {
        n: 't.N=t.' + to + '[0]>>7',
        z: 't.Z=t.' + to + '[0]?0:1'
    };
    return {
        code: transfer,
        out: out,
        size: 1
    };
};

/**
 * Decodes a flag update instruction.
 * @param {object} out - An output condition update dictionary.
 * @return {object} - A decoded instruction data.
 */
Jsj6502.prototype._decodeUpdateFlag = function (out) {
    return {
        code: [
        ],
        out: out,
        size: 1
    };
};

if (this.process)
    exports.Jsj6502 = Jsj6502;
else
    this.Jsj6502 = Jsj6502;
})();