#pragma once
/*#
    # w65c02.h

    W65C02 CPU emulator.

    NOTE: this file is code-generated from w65c02.template.h and w65c02_gen.py
    in the 'codegen' directory.

    Do this:
    ~~~C
    #define CHIPS_IMPL
    ~~~
    before you include this file in *one* C or C++ file to create the
    implementation.

    Optionally provide the following macros with your own implementation
    ~~~C
    CHIPS_ASSERT(c)
    ~~~

    ## Emulated Pins

    ***********************************
    *           +-----------+         *
    *   IRQ --->|           |---> A0  *
    *   NMI --->|           |...      *
    *    RDY--->|           |---> A15 *
    *    RES--->|           |         *
    *    RW <---|           |         *
    *  SYNC <---|           |         *
    *    VP <---|           |<--> D0  *
    *           |           |...      *
    *           |           |<--> D7  *
    *           |           |         *
    *           +-----------+         *
    ***********************************

    If the RDY pin is active (1) the CPU will loop until the pin goes inactive.

    ## Overview

    w65c02.h implements a cycle-stepped w65c02 CPU emulator, meaning
    that the emulation state can be ticked forward in clock cycles instead
    of full instructions.

    To initialize the emulator, fill out a w65c02_desc_t structure with
    initialization parameters and then call w65c02_init().

        ~~~C
        typedef struct {
            bool bcd_disabled;          // set to true if BCD mode is disabled
         } w65c02_desc_t;
         ~~~

    At the end of w65c02_init(), the CPU emulation will be at the start of
    RESET state, and the first 7 ticks will execute the reset sequence
    (loading the reset vector at address 0xFFFC and continuing execution
    there.

    w65c02_init() will return a 64-bit pin mask which must be the input argument
    to the first call of w65c02_tick().

    To execute instructions, call w65c02_tick() in a loop. w65c02_tick() takes
    a 64-bit pin mask as input, executes one clock tick, and returns
    a modified pin mask.

    After executing one tick, the pin mask must be inspected, a memory read
    or write operation must be performed, and the modified pin mask must be
    used for the next call to w65c02_tick(). This 64-bit pin mask is how
    the CPU emulation communicates with the outside world.

    The simplest-possible execution loop would look like this:

        ~~~C
        // setup 64 kBytes of memory
        uint8_t mem[1<<16] = { ... };
        // initialize the CPU
        w65c02_t cpu;
        uint64_t pins = w65c02_init(&cpu, &(w65c02_desc_t){...});
        while (...) {
            // run the CPU emulation for one tick
            pins = w65c02_tick(&cpu, pins);
            // extract 16-bit address from pin mask
            const uint16_t addr = W65C02_GET_ADDR(pins);
            // perform memory access
            if (pins & W65C02_RW) {
                // a memory read
                W65C02_SET_DATA(pins, mem[addr]);
            }
            else {
                // a memory write
                mem[addr] = W65C02_GET_DATA(pins);
            }
        }
        ~~~

    To start a reset sequence, set the W65C02_RES bit in the pin mask and
    continue calling the w65c02_tick() function. At the start of the next
    cycle, the CPU will initiate the 7-tick reset sequence. You do NOT
    need to clear the W65C02_RES bit, this will be cleared when the reset
    sequence starts.

    To request an interrupt, set the W65C02_IRQ or W65C02_NMI bits in the pin
    mask and continue calling the tick function. The interrupt sequence
    will be initiated at the end of the current or next instruction
    (depending on the exact cycle the interrupt pin has been set).

    Unlike the W65C02_RES pin, you are also responsible for clearing the
    interrupt pins (typically, the interrupt lines are cleared by the chip
    which requested the interrupt once the CPU reads a chip's interrupt
    status register to check which chip requested the interrupt).

    To find out whether a new instruction is about to start, check if the
    W65C02_SYNC pin is set.

    To "goto" a random address at any time, a 'prefetch' like this is
    necessary (this basically simulates a normal instruction fetch from
    address 'next_pc'). This is usually only needed in "trap code" which
    intercepts operating system calls, executes some native code to emulate
    the operating system call, and then continue execution somewhere else:

        ~~~C
        pins = W65C02_SYNC;
        W65C02_SET_ADDR(pins, next_pc);
        W65C02_SET_DATA(pins, mem[next_pc]);
        w65c02_set_pc(next_pc);
        ~~~~

    ## Functions
    ~~~C
    uint64_t w65c02_init(w65c02_t* cpu, const w65c02_desc_t* desc)
    ~~~
        Initialize a w65c02_t instance, the desc structure provides
        initialization attributes:
            ~~~C
            typedef struct {
                bool bcd_disabled;              // set to true if BCD mode is disabled
            } w65c02_desc_t;
            ~~~
    ~~~C
    uint64_t w65c02_tick(w65c02_t* cpu, uint64_t pins)
    ~~~
        Tick the CPU for one clock cycle. The 'pins' argument and return value
        is the current state of the CPU pins used to communicate with the
        outside world (see the Overview section above for details).
    ~~~C
    void w65c02_set_x(w65c02_t* cpu, uint8_t val)
    void w65c02_set_xx(w65c02_t* cpu, uint16_t val)
    uint8_t w65c02_x(w65c02_t* cpu)
    uint16_t w65c02_xx(w65c02_t* cpu)
    ~~~
        Set and get 6502 registers and flags.


    ## zlib/libpng license
    Original MOS 6502 simulation code copyright (c) 2018 Andre Weissflog
    W65C02 modifications copyright (c) 2021 Mathias Bergvall

    This software is provided 'as-is', without any express or implied warranty.
    In no event will the authors be held liable for any damages arising from the
    use of this software.
    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:
        1. The origin of this software must not be misrepresented; you must not
        claim that you wrote the original software. If you use this software in a
        product, an acknowledgment in the product documentation would be
        appreciated but is not required.
        2. Altered source versions must be plainly marked as such, and must not
        be misrepresented as being the original software.
        3. This notice may not be removed or altered from any source
        distribution.
#*/
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* address bus pins */
#define W65C02_A0    (1ULL<<0)
#define W65C02_A1    (1ULL<<1)
#define W65C02_A2    (1ULL<<2)
#define W65C02_A3    (1ULL<<3)
#define W65C02_A4    (1ULL<<4)
#define W65C02_A5    (1ULL<<5)
#define W65C02_A6    (1ULL<<6)
#define W65C02_A7    (1ULL<<7)
#define W65C02_A8    (1ULL<<8)
#define W65C02_A9    (1ULL<<9)
#define W65C02_A10   (1ULL<<10)
#define W65C02_A11   (1ULL<<11)
#define W65C02_A12   (1ULL<<12)
#define W65C02_A13   (1ULL<<13)
#define W65C02_A14   (1ULL<<14)
#define W65C02_A15   (1ULL<<15)

/* data bus pins */
#define W65C02_D0    (1ULL<<16)
#define W65C02_D1    (1ULL<<17)
#define W65C02_D2    (1ULL<<18)
#define W65C02_D3    (1ULL<<19)
#define W65C02_D4    (1ULL<<20)
#define W65C02_D5    (1ULL<<21)
#define W65C02_D6    (1ULL<<22)
#define W65C02_D7    (1ULL<<23)

/* control pins */
#define W65C02_RW    (1ULL<<24)      /* out: memory read or write access */
#define W65C02_SYNC  (1ULL<<25)      /* out: start of a new instruction */
#define W65C02_IRQ   (1ULL<<26)      /* in: maskable interrupt requested */
#define W65C02_NMI   (1ULL<<27)      /* in: non-maskable interrupt requested */
#define W65C02_RDY   (1ULL<<28)      /* in: freeze execution at next read cycle */
#define W65C02_RES   (1ULL<<30)      /* request RESET */
#define W65C02_VP    (1ULL<<31)      /* out: vector pull */

/* bit mask for all CPU pins (up to bit pos 40) */
#define W65C02_PIN_MASK ((1ULL<<31)-1)

/* status indicator flags */
#define W65C02_CF    (1<<0)  /* carry */
#define W65C02_ZF    (1<<1)  /* zero */
#define W65C02_IF    (1<<2)  /* IRQ disable */
#define W65C02_DF    (1<<3)  /* decimal mode */
#define W65C02_BF    (1<<4)  /* BRK command */
#define W65C02_XF    (1<<5)  /* unused */
#define W65C02_VF    (1<<6)  /* overflow */
#define W65C02_NF    (1<<7)  /* negative */

/* internal BRK state flags */
#define W65C02_BRK_IRQ   (1<<0)  /* IRQ was triggered */
#define W65C02_BRK_NMI   (1<<1)  /* NMI was triggered */
#define W65C02_BRK_RESET (1<<2)  /* RES was triggered */

/* the desc structure provided to w65c02_init() */
typedef struct {
    bool bcd_disabled;              /* set to true if BCD mode is disabled */
} w65c02_desc_t;

/* CPU state */
typedef struct {
    uint16_t IR;        /* internal instruction register */
    uint16_t PC;        /* internal program counter register */
    uint16_t AD;        /* ADL/ADH internal register */
    uint8_t A,X,Y,S,P;  /* regular registers */
    uint64_t PINS;      /* last stored pin state (do NOT modify) */
    uint16_t irq_pip;
    uint16_t nmi_pip;
    uint8_t brk_flags;  /* W65C02_BRK_* */
    uint8_t bcd_enabled;
    uint8_t waiting;
    uint8_t stopped;
} w65c02_t;

/* initialize a new w65c02 instance and return initial pin mask */
uint64_t w65c02_init(w65c02_t* cpu, const w65c02_desc_t* desc);
/* execute one tick */
uint64_t w65c02_tick(w65c02_t* cpu, uint64_t pins);

/* register access functions */
void w65c02_set_a(w65c02_t* cpu, uint8_t v);
void w65c02_set_x(w65c02_t* cpu, uint8_t v);
void w65c02_set_y(w65c02_t* cpu, uint8_t v);
void w65c02_set_s(w65c02_t* cpu, uint8_t v);
void w65c02_set_p(w65c02_t* cpu, uint8_t v);
void w65c02_set_pc(w65c02_t* cpu, uint16_t v);
uint8_t w65c02_a(w65c02_t* cpu);
uint8_t w65c02_x(w65c02_t* cpu);
uint8_t w65c02_y(w65c02_t* cpu);
uint8_t w65c02_s(w65c02_t* cpu);
uint8_t w65c02_p(w65c02_t* cpu);
uint16_t w65c02_pc(w65c02_t* cpu);

/* extract 16-bit address bus from 64-bit pins */
#define W65C02_GET_ADDR(p) ((uint16_t)(p&0xFFFFULL))
/* merge 16-bit address bus value into 64-bit pins */
#define W65C02_SET_ADDR(p,a) {p=((p&~0xFFFFULL)|((a)&0xFFFFULL));}
/* extract 8-bit data bus from 64-bit pins */
#define W65C02_GET_DATA(p) ((uint8_t)((p&0xFF0000ULL)>>16))
/* merge 8-bit data bus value into 64-bit pins */
#define W65C02_SET_DATA(p,d) {p=(((p)&~0xFF0000ULL)|(((d)<<16)&0xFF0000ULL));}
/* copy data bus value from other pin mask */
#define W65C02_COPY_DATA(p0,p1) (((p0)&~0xFF0000ULL)|((p1)&0xFF0000ULL))
/* return a pin mask with control-pins, address and data bus */
#define W65C02_MAKE_PINS(ctrl, addr, data) ((ctrl)|(((data)<<16)&0xFF0000ULL)|((addr)&0xFFFFULL))
/* set the port bits on the 64-bit pin mask */

#ifdef __cplusplus
} /* extern "C" */
#endif

/*-- IMPLEMENTATION ----------------------------------------------------------*/
#ifdef CHIPS_IMPL
#include <string.h>
#ifndef CHIPS_ASSERT
    #include <assert.h>
    #define CHIPS_ASSERT(c) assert(c)
#endif

/* register access functions */
void w65c02_set_a(w65c02_t* cpu, uint8_t v) { cpu->A = v; }
void w65c02_set_x(w65c02_t* cpu, uint8_t v) { cpu->X = v; }
void w65c02_set_y(w65c02_t* cpu, uint8_t v) { cpu->Y = v; }
void w65c02_set_s(w65c02_t* cpu, uint8_t v) { cpu->S = v; }
void w65c02_set_p(w65c02_t* cpu, uint8_t v) { cpu->P = v; }
void w65c02_set_pc(w65c02_t* cpu, uint16_t v) { cpu->PC = v; }
uint8_t w65c02_a(w65c02_t* cpu) { return cpu->A; }
uint8_t w65c02_x(w65c02_t* cpu) { return cpu->X; }
uint8_t w65c02_y(w65c02_t* cpu) { return cpu->Y; }
uint8_t w65c02_s(w65c02_t* cpu) { return cpu->S; }
uint8_t w65c02_p(w65c02_t* cpu) { return cpu->P; }
uint16_t w65c02_pc(w65c02_t* cpu) { return cpu->PC; }

/* helper macros and functions for code-generated instruction decoder */
#define _W65C02_NZ(p,v) ((p&~(W65C02_NF|W65C02_ZF))|((v&0xFF)?(v&W65C02_NF):W65C02_ZF))

static inline void _w65c02_adc(w65c02_t* cpu, uint8_t val) {
    if (cpu->bcd_enabled && (cpu->P & W65C02_DF)) {
        /* decimal mode (credit goes to MAME) */
        uint8_t c = cpu->P & W65C02_CF ? 1 : 0;
        cpu->P &= ~(W65C02_NF|W65C02_VF|W65C02_ZF|W65C02_CF);
        uint8_t al = (cpu->A & 0x0F) + (val & 0x0F) + c;
        if (al > 9) {
            al += 6;
        }
        uint8_t ah = (cpu->A >> 4) + (val >> 4) + (al > 0x0F);
        if (0 == (uint8_t)(cpu->A + val + c)) {
            cpu->P |= W65C02_ZF;
        }
        else if (ah & 0x08) {
            cpu->P |= W65C02_NF;
        }
        if (~(cpu->A^val) & (cpu->A^(ah<<4)) & 0x80) {
            cpu->P |= W65C02_VF;
        }
        if (ah > 9) {
            ah += 6;
        }
        if (ah > 15) {
            cpu->P |= W65C02_CF;
        }
        cpu->A = (ah<<4) | (al & 0x0F);
    }
    else {
        /* default mode */
        uint16_t sum = cpu->A + val + (cpu->P & W65C02_CF ? 1:0);
        cpu->P &= ~(W65C02_VF|W65C02_CF);
        cpu->P = _W65C02_NZ(cpu->P,sum);
        if (~(cpu->A^val) & (cpu->A^sum) & 0x80) {
            cpu->P |= W65C02_VF;
        }
        if (sum & 0xFF00) {
            cpu->P |= W65C02_CF;
        }
        cpu->A = sum & 0xFF;
    }
}

static inline void _w65c02_sbc(w65c02_t* cpu, uint8_t val) {
    if (cpu->bcd_enabled && (cpu->P & W65C02_DF)) {
        /* decimal mode (credit goes to MAME) */
        uint8_t c = cpu->P & W65C02_CF ? 0 : 1;
        cpu->P &= ~(W65C02_NF|W65C02_VF|W65C02_ZF|W65C02_CF);
        uint16_t diff = cpu->A - val - c;
        uint8_t al = (cpu->A & 0x0F) - (val & 0x0F) - c;
        if ((int8_t)al < 0) {
            al -= 6;
        }
        uint8_t ah = (cpu->A>>4) - (val>>4) - ((int8_t)al < 0);
        if (0 == (uint8_t)diff) {
            cpu->P |= W65C02_ZF;
        }
        else if (diff & 0x80) {
            cpu->P |= W65C02_NF;
        }
        if ((cpu->A^val) & (cpu->A^diff) & 0x80) {
            cpu->P |= W65C02_VF;
        }
        if (!(diff & 0xFF00)) {
            cpu->P |= W65C02_CF;
        }
        if (ah & 0x80) {
            ah -= 6;
        }
        cpu->A = (ah<<4) | (al & 0x0F);
    }
    else {
        /* default mode */
        uint16_t diff = cpu->A - val - (cpu->P & W65C02_CF ? 0 : 1);
        cpu->P &= ~(W65C02_VF|W65C02_CF);
        cpu->P = _W65C02_NZ(cpu->P, (uint8_t)diff);
        if ((cpu->A^val) & (cpu->A^diff) & 0x80) {
            cpu->P |= W65C02_VF;
        }
        if (!(diff & 0xFF00)) {
            cpu->P |= W65C02_CF;
        }
        cpu->A = diff & 0xFF;
    }
}

static inline void _w65c02_cmp(w65c02_t* cpu, uint8_t r, uint8_t v) {
    uint16_t t = r - v;
    cpu->P = (_W65C02_NZ(cpu->P, (uint8_t)t) & ~W65C02_CF) | ((t & 0xFF00) ? 0:W65C02_CF);
}

static inline uint8_t _w65c02_trb(w65c02_t* cpu, uint8_t v) {
    uint8_t t = cpu->A & v;
    cpu->P = cpu->P&~(W65C02_ZF);
    if (t == 0) {
        cpu->P |= W65C02_ZF;
    }
    return v & (~cpu->A);
}

static inline uint8_t _w65c02_tsb(w65c02_t* cpu, uint8_t v) {
    uint8_t t = cpu->A & v;
    cpu->P = cpu->P&~(W65C02_ZF);
    if (t == 0) {
        cpu->P |= W65C02_ZF;
    }
    return v | cpu->A;
}

static inline uint8_t _w65c02_asl(w65c02_t* cpu, uint8_t v) {
    cpu->P = (_W65C02_NZ(cpu->P, v<<1) & ~W65C02_CF) | ((v & 0x80) ? W65C02_CF:0);
    return v<<1;
}

static inline uint8_t _w65c02_lsr(w65c02_t* cpu, uint8_t v) {
    cpu->P = (_W65C02_NZ(cpu->P, v>>1) & ~W65C02_CF) | ((v & 0x01) ? W65C02_CF:0);
    return v>>1;
}

static inline uint8_t _w65c02_rol(w65c02_t* cpu, uint8_t v) {
    bool carry = cpu->P & W65C02_CF;
    cpu->P &= ~(W65C02_NF|W65C02_ZF|W65C02_CF);
    if (v & 0x80) {
        cpu->P |= W65C02_CF;
    }
    v <<= 1;
    if (carry) {
        v |= 1;
    }
    cpu->P = _W65C02_NZ(cpu->P, v);
    return v;
}

static inline uint8_t _w65c02_ror(w65c02_t* cpu, uint8_t v) {
    bool carry = cpu->P & W65C02_CF;
    cpu->P &= ~(W65C02_NF|W65C02_ZF|W65C02_CF);
    if (v & 1) {
        cpu->P |= W65C02_CF;
    }
    v >>= 1;
    if (carry) {
        v |= 0x80;
    }
    cpu->P = _W65C02_NZ(cpu->P, v);
    return v;
}

static inline void _w65c02_bit(w65c02_t* cpu, uint8_t v) {
    uint8_t t = cpu->A & v;
    cpu->P &= ~(W65C02_NF|W65C02_VF|W65C02_ZF);
    if (!t) {
        cpu->P |= W65C02_ZF;
    }
    cpu->P |= v & (W65C02_NF|W65C02_VF);
}

static inline void _w65c02_bit_imm(w65c02_t* cpu, uint8_t v) {
    uint8_t t = cpu->A & v;
    cpu->P &= ~(W65C02_ZF);
    if (!t) {
        cpu->P |= W65C02_ZF;
    }
}
#undef _W65C02_NZ

uint64_t w65c02_init(w65c02_t* c, const w65c02_desc_t* desc) {
    CHIPS_ASSERT(c && desc);
    memset(c, 0, sizeof(*c));
    c->P = W65C02_ZF;
    c->bcd_enabled = !desc->bcd_disabled;
    c->PINS = W65C02_RW | W65C02_SYNC | W65C02_RES;
    c->waiting = 0;
    c->stopped = 0;
    return c->PINS;
}

/* set 16-bit address in 64-bit pin mask */
#define _SA(addr) pins=(pins&~0xFFFF)|((addr)&0xFFFFULL)
/* extract 16-bit addess from pin mask */
#define _GA() ((uint16_t)(pins&0xFFFFULL))
/* set 16-bit address and 8-bit data in 64-bit pin mask */
#define _SAD(addr,data) pins=(pins&~0xFFFFFF)|((((data)&0xFF)<<16)&0xFF0000ULL)|((addr)&0xFFFFULL)
/* fetch next opcode byte */
#define _FETCH() _SA(c->PC);_ON(W65C02_SYNC);
/* set 8-bit data in 64-bit pin mask */
#define _SD(data) pins=((pins&~0xFF0000ULL)|(((data&0xFF)<<16)&0xFF0000ULL))
/* extract 8-bit data from 64-bit pin mask */
#define _GD() ((uint8_t)((pins&0xFF0000ULL)>>16))
/* enable control pins */
#define _ON(m) pins|=(m)
/* disable control pins */
#define _OFF(m) pins&=~(m)
/* a memory read tick */
#define _RD() _ON(W65C02_RW);
/* a memory write tick */
#define _WR() _OFF(W65C02_RW);
/* set N and Z flags depending on value */
#define _NZ(v) c->P=((c->P&~(W65C02_NF|W65C02_ZF))|((v&0xFF)?(v&W65C02_NF):W65C02_ZF))

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable:4244)   /* conversion from 'uint16_t' to 'uint8_t', possible loss of data */
#endif

uint64_t w65c02_tick(w65c02_t* c, uint64_t pins) {
    if (c->stopped && ((pins & W65C02_RES) == 0)) {
        return pins;
    }
    c->stopped = 0;
    if (pins & W65C02_RES) {
        c->PINS = W65C02_RW | W65C02_SYNC | W65C02_RES;
        c->waiting = 0;
        pins = c->PINS;
    }
    if (pins & (W65C02_SYNC|W65C02_IRQ|W65C02_NMI|W65C02_RDY|W65C02_RES)) {
        // interrupt detection also works in RDY phases, but only NMI is "sticky"

        // NMI is edge-triggered
        if (0 != ((pins & (pins ^ c->PINS)) & W65C02_NMI)) {
            c->nmi_pip |= 1;
        }
        // IRQ test is level triggered
        if ((pins & W65C02_IRQ) && (0 == (c->P & W65C02_IF))) {
            c->irq_pip |= 1;
        }

        if (c->waiting && (pins & (W65C02_IRQ | W65C02_NMI))) {
            pins &= ~W65C02_RDY;
            c->PINS = pins;
            c->irq_pip <<= 1;
            c->waiting = 0;
            return pins;
        }

        // RDY pin is checked all cycles
        if ((pins & W65C02_RDY) == W65C02_RDY) {
            c->PINS = pins;
            c->irq_pip <<= 1;
            return pins;
        }
        if (pins & W65C02_SYNC) {
            // load new instruction into 'instruction register' and restart tick counter
            c->IR = _GD()<<3;
            _OFF(W65C02_SYNC);

            // check IRQ, NMI and RES state
            //  - IRQ is level-triggered and must be active in the full cycle
            //    before SYNC
            //  - NMI is edge-triggered, and the change must have happened in
            //    any cycle before SYNC
            //  - RES behaves slightly different than on a real 6502, we go
            //    into RES state as soon as the pin goes active, from there
            //    on, behaviour is 'standard'
            if (0 != (c->irq_pip & 4)) {
                c->brk_flags |= W65C02_BRK_IRQ;
            }
            if (0 != (c->nmi_pip & 0xFFFC)) {
                c->brk_flags |= W65C02_BRK_NMI;
            }
            if (0 != (pins & W65C02_RES)) {
                c->brk_flags |= W65C02_BRK_RESET;
            }
            c->irq_pip &= 3;
            c->nmi_pip &= 3;

            // if interrupt or reset was requested, force a BRK instruction
            if (c->brk_flags) {
                c->IR = 0;
                c->P &= ~W65C02_BF;
                pins &= ~W65C02_RES;
            }
            else {
                c->PC++;
            }
        }
    }
    // reads are default, writes are special
    _RD();
    switch (c->IR++) {
$decode_block
    }
    c->PINS = pins;
    c->irq_pip <<= 1;
    c->nmi_pip <<= 1;
    return pins;
}
#if defined(_MSC_VER)
#pragma warning(pop)
#endif

#undef _SA
#undef _SAD
#undef _FETCH
#undef _SD
#undef _GD
#undef _ON
#undef _OFF
#undef _RD
#undef _WR
#undef _NZ
#endif /* CHIPS_IMPL */
