# -------------------------------------------------------------------------------
#   w65c02_decoder.py
#   Generate instruction decoder for w65c02.h emulator.
# -------------------------------------------------------------------------------
import sys
from string import Template

InpPath = 'w65c02.template.h'
OutPath = '../chips/w65c02.h'

# flag bits
CF = (1 << 0)
ZF = (1 << 1)
IF = (1 << 2)
DF = (1 << 3)
BF = (1 << 4)
XF = (1 << 5)
VF = (1 << 6)
NF = (1 << 7)


def flag_name(f):
    if f == CF:
        return 'C'
    elif f == ZF:
        return 'Z'
    elif f == IF:
        return 'I'
    elif f == DF:
        return 'D'
    elif f == BF:
        return 'B'
    elif f == XF:
        return 'X'
    elif f == VF:
        return 'V'
    elif f == NF:
        return 'N'


def branch_name(m, v):
    if m == NF:
        return 'BPL' if v == 0 else 'BMI'
    elif m == VF:
        return 'BVC' if v == 0 else 'BVS'
    elif m == CF:
        return 'BCC' if v == 0 else 'BCS'
    elif m == ZF:
        return 'BNE' if v == 0 else 'BEQ'


# addressing mode constants
A____ = 0  # no addressing mode
A_IMM = 1  # immediate
A_ZER = 2  # zero-page
A_ZPX = 3  # zp,X
A_ZPY = 4  # zp,Y
A_ABS = 5  # abs
A_ABX = 6  # abs,X
A_ABY = 7  # abs,Y
A_IDX = 8  # (zp,X)
A_IDY = 9  # (zp),Y
A_IDZ = 10  # (zp)
A_JMP = 11  # special JMP abs
A_JSR = 12  # special JSR abs
A_ZPR = 13  # zp, rel
A_IGN = 14  # ignore operands, used for NOPs
A_IAX = 15  # (a, X)
A_AX2 = 16  # abs,X (no page crossing optimization)
A_AY2 = 17  # abs,Y (no page crossing optimization)

# addressing mode strings
addr_mode_str = ['', '#', 'zp', 'zp,X', 'zp,Y', 'abs', 'abs,X', 'abs,Y', '(zp,X)', '(zp),Y', '(zp)', '', '', '', '',
                 '(a,X)', 'abs,X', 'abs,Y']

# memory access modes
M___ = 0  # no memory access
M_R_ = 1  # read access
M__W = 2  # write access
M_RW = 3  # read-modify-write

# addressing-modes and memory accesses for each instruction
ops = [
    # cc = 00
    [
        [[A____, M___], [A_JSR, M_R_], [A____, M_R_], [A____, M_R_],
         [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_]],
        [[A_ZER, M_RW], [A_ZER, M_R_], [A_ZER, M_R_], [A_ZER, M__W],
         [A_ZER, M__W], [A_ZER, M_R_], [A_ZER, M_R_], [A_ZER, M_R_]],
        [[A____, M__W], [A____, M___], [A____, M__W], [A____, M___],
         [A____, M___], [A____, M___], [A____, M___], [A____, M___]],
        [[A_ABS, M_RW], [A_ABS, M_R_], [A_JMP, M_R_], [A_JMP, M_R_],
         [A_ABS, M__W], [A_ABS, M_R_], [A_ABS, M_R_], [A_ABS, M_R_]],
        [[A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_],
         [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_]],
        [[A_ZER, M_RW], [A_ZPX, M_R_], [A_ZPX, M_R_], [A_ZPX, M__W],
         [A_ZPX, M__W], [A_ZPX, M_R_], [A_ZPX, M_R_], [A_ZPX, M_R_]],
        [[A____, M___], [A____, M___], [A____, M___], [A____, M___],
         [A____, M___], [A____, M___], [A____, M___], [A____, M___]],
        [[A_ABS, M_RW], [A_ABX, M_R_], [A_IGN, M_R_], [A_IAX, M_R_],
         [A_ABS, M__W], [A_ABX, M_R_], [A_ABS, M_R_], [A_ABS, M_R_]]
    ],
    # cc = 01
    [
        [[A_IDX, M_R_], [A_IDX, M_R_], [A_IDX, M_R_], [A_IDX, M_R_],
         [A_IDX, M__W], [A_IDX, M_R_], [A_IDX, M_R_], [A_IDX, M_R_]],
        [[A_ZER, M_R_], [A_ZER, M_R_], [A_ZER, M_R_], [A_ZER, M_R_],
         [A_ZER, M__W], [A_ZER, M_R_], [A_ZER, M_R_], [A_ZER, M_R_]],
        [[A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_],
         [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_], [A_IMM, M_R_]],
        [[A_ABS, M_R_], [A_ABS, M_R_], [A_ABS, M_R_], [A_ABS, M_R_],
         [A_ABS, M__W], [A_ABS, M_R_], [A_ABS, M_R_], [A_ABS, M_R_]],
        [[A_IDY, M_R_], [A_IDY, M_R_], [A_IDY, M_R_], [A_IDY, M_R_],
         [A_IDY, M__W], [A_IDY, M_R_], [A_IDY, M_R_], [A_IDY, M_R_]],
        [[A_ZPX, M_R_], [A_ZPX, M_R_], [A_ZPX, M_R_], [A_ZPX, M_R_],
         [A_ZPX, M__W], [A_ZPX, M_R_], [A_ZPX, M_R_], [A_ZPX, M_R_]],
        [[A_ABY, M_R_], [A_ABY, M_R_], [A_ABY, M_R_], [A_ABY, M_R_],
         [A_AY2, M__W], [A_ABY, M_R_], [A_ABY, M_R_], [A_ABY, M_R_]],
        [[A_ABX, M_R_], [A_ABX, M_R_], [A_ABX, M_R_], [A_ABX, M_R_],
         [A_AX2, M__W], [A_ABX, M_R_], [A_ABX, M_R_], [A_ABX, M_R_]]
    ],
    # cc = 02
    [
        [[A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_],
         [A_IGN, M_R_], [A_IMM, M_R_], [A_IGN, M_R_], [A_IGN, M_R_]],
        [[A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW],
         [A_ZER, M__W], [A_ZER, M_R_], [A_ZER, M_RW], [A_ZER, M_RW]],
        [[A____, M___], [A____, M___], [A____, M___], [A____, M___],
         [A____, M___], [A____, M___], [A____, M___], [A____, M___]],
        [[A_ABS, M_RW], [A_ABS, M_RW], [A_ABS, M_RW], [A_ABS, M_RW],
         [A_ABS, M__W], [A_ABS, M_R_], [A_ABS, M_RW], [A_ABS, M_RW]],
        [[A_IDZ, M_R_], [A_IDZ, M_R_], [A_IDZ, M_R_], [A_IDZ, M_R_],
         [A_IDZ, M__W], [A_IDZ, M_R_], [A_IDZ, M_R_], [A_IDZ, M_R_]],
        [[A_ZPX, M_RW], [A_ZPX, M_RW], [A_ZPX, M_RW], [A_ZPX, M_RW],
         [A_ZPY, M__W], [A_ZPY, M_R_], [A_ZPX, M_RW], [A_ZPX, M_RW]],
        [[A____, M___], [A____, M___], [A____, M__W], [A____, M_R_],
         [A____, M___], [A____, M___], [A____, M__W], [A____, M_R_]],
        [[A_ABX, M_RW], [A_ABX, M_RW], [A_ABX, M_RW], [A_ABX, M_RW],
         [A_AX2, M__W], [A_ABY, M_R_], [A_AX2, M_RW], [A_AX2, M_RW]]
    ],
    # cc = 03
    [
        [[A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_],
         [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_]],
        [[A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW],
         [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW]],
        [[A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_],
         [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_]],
        [[A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_],
         [A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_]],
        [[A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_],
         [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_]],
        [[A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW],
         [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW], [A_ZER, M_RW]],
        [[A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_],
         [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_], [A_IGN, M_R_]],
        [[A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_],
         [A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_], [A_ZPR, M_R_]]
    ]
]


class opcode:
    def __init__(self, op):
        self.code = op
        self.cmt = None
        self.i = 0
        self.src = [None] * 8

    def t(self, src):
        self.src[self.i] = src
        self.i += 1

    def ta(self, src):
        self.src[self.i - 1] += src


# -------------------------------------------------------------------------------
#   output a src line
#
out_lines = ''


def l(s):
    global out_lines
    out_lines += s + '\n'


# -------------------------------------------------------------------------------
def write_op(op):
    if not op.cmt:
        op.cmt = '???'
    l('    /* {} */'.format(op.cmt if op.cmt else '???'))
    for t in range(0, 8):
        if t < op.i:
            l('        case (0x{:02X}<<3)|{}: {}break;'.format(op.code, t, op.src[t]))
        else:
            l('        case (0x{:02X}<<3)|{}: assert(false);break;'.format(op.code, t))


# -------------------------------------------------------------------------------
def cmt(o, cmd):
    cc = o.code & 3
    bbb = (o.code >> 2) & 7
    aaa = (o.code >> 5) & 7
    addr_mode = ops[cc][bbb][aaa][0]
    o.cmt = cmd
    if addr_mode != '':
        o.cmt += ' ' + addr_mode_str[addr_mode]


# -------------------------------------------------------------------------------
def enc_addr(op, addr_mode, mem_access):
    if addr_mode == A____:
        # no addressing, this still puts the PC on the address bus without
        # incrementing the PC
        op.t('_SA(c->PC);')
    elif addr_mode == A_IMM:
        # immediate mode
        op.t('_SA(c->PC++);')
    elif addr_mode == A_ZER:
        # zero page
        op.t('_SA(c->PC++);')
        op.t('_SA(_GD());')
    elif addr_mode == A_ZPX:
        # zero page + X
        op.t('_SA(c->PC++);')
        op.t('c->AD=_GD();')
        op.t('_SA((c->AD+c->X)&0x00FF);')
    elif addr_mode == A_ZPY:
        # zero page + Y
        op.t('_SA(c->PC++);')
        op.t('c->AD=_GD();')
        op.t('_SA((c->AD+c->Y)&0x00FF);')
    elif addr_mode == A_ABS:
        # absolute
        op.t('_SA(c->PC++);')
        op.t('_SA(c->PC++);c->AD=_GD();')
        op.t('_SA((_GD()<<8)|c->AD);')
    elif addr_mode == A_ABX:
        # absolute + X
        op.t('_SA(c->PC++);')
        op.t('_SA(c->PC);c->AD=_GD(); c->AD+=c->X;')
        op.t('c->AD+=_GD()<<8;if(((c->AD>>8)&0xff)==_GD()){_SA(c->AD);c->PC++;c->IR++;}else{_SA(c->PC);}')
        op.t('c->PC++;_SA(c->AD);')
    elif addr_mode == A_AX2:
        # absolute + X (no page crossing optimization)
        op.t('_SA(c->PC++);')
        op.t('_SA(c->PC);c->AD=_GD(); c->AD+=c->X;')
        op.t('c->AD+=_GD()<<8;if(((c->AD>>8)&0xff)==_GD()){_SA(c->AD);}else{_SA(c->PC);}')
        op.t('c->PC++;_SA(c->AD);')
    elif addr_mode == A_ABY:
        # absolute + Y
        # same page-boundary-crossed special case as absolute+X
        op.t('_SA(c->PC++);')
        op.t('_SA(c->PC);c->AD=_GD(); c->AD+=c->Y;')
        op.t('c->AD+=_GD()<<8;if(((c->AD>>8)&0xff)==_GD()){ _SA(c->AD);c->PC++;c->IR++;}else{_SA(c->PC);}')
        op.t('c->PC++;_SA(c->AD);')
    elif addr_mode == A_AY2:
        # absolute + Y (no page crossing optimization)
        op.t('_SA(c->PC++);')
        op.t('_SA(c->PC);c->AD=_GD(); c->AD+=c->Y;')
        op.t('c->AD+=_GD()<<8;if(((c->AD>>8)&0xff)==_GD()){ _SA(c->AD);}else{_SA(c->PC);}')
        op.t('c->PC++;_SA(c->AD);')
    elif addr_mode == A_IDX:
        # (zp,X)
        op.t('_SA(c->PC++);')
        op.t('c->AD=_GD();')
        op.t('c->AD=(c->AD+c->X)&0xFF;_SA(c->AD);')
        op.t('_SA((c->AD+1)&0xFF);c->AD=_GD();')
        op.t('_SA((_GD()<<8)|c->AD);')
    elif addr_mode == A_IDY:
        # (zp),Y
        # same page-boundary-crossed special case as absolute+X
        op.t('_SA(c->PC++);')
        op.t('c->AD=_GD();_SA(c->AD);')
        op.t('_SA((c->AD+1)&0xFF);c->AD=_GD()+c->Y;')
        op.t('c->AD+=_GD()<<8;')
        if mem_access == M_R_:
            op.ta('if(((c->AD>>8)&0xff)==_GD()){ _SA(c->AD);c->IR++;}')
        op.t('_SA(c->AD);')
    elif addr_mode == A_IDZ:
        # (zp)
        op.t('_SA(c->PC++);')
        op.t('c->AD=_GD();_SA(c->AD);')
        op.t('_SA((c->AD+1)&0xFF);c->AD=_GD();')
        op.t('c->AD|=_GD()<<8;_SA(c->AD);')
    elif addr_mode == A_JMP:
        # jmp is completely handled in instruction decoding
        pass
    elif addr_mode == A_JSR:
        # jsr is completely handled in instruction decoding
        pass
    elif addr_mode == A_ZPR:
        # zp,rel is completely handled in instruction decoding
        pass
    elif addr_mode == A_IGN:
        #  handled in instruction decoding
        pass
    elif addr_mode == A_IAX:
        # (a,x) is completely handled in instruction decoding
        pass
    else:
        # invalid instruction
        pass


# -------------------------------------------------------------------------------
def i_brk(o):
    cmt(o, 'BRK')
    o.t('if(0==(c->brk_flags&(W65C02_BRK_IRQ|W65C02_BRK_NMI))){c->PC++;}'
        '_SAD(0x0100|c->S--,c->PC>>8);if(0==(c->brk_flags&W65C02_BRK_RESET)){_WR();}')
    o.t('_SAD(0x0100|c->S--,c->PC);if(0==(c->brk_flags&W65C02_BRK_RESET)){_WR();}')
    o.t('_SAD(0x0100|c->S--,c->P|W65C02_XF);'
        'if(c->brk_flags&W65C02_BRK_RESET){c->AD=0xFFFC;}'
        'else{_WR();if(c->brk_flags&W65C02_BRK_NMI){c->AD=0xFFFA;}else{c->AD=0xFFFE;}}')
    o.t('_SA(c->AD++);c->P|=(W65C02_IF|W65C02_BF);c->P&=~W65C02_DF;_ON(W65C02_VP);')
    o.t('_SA(c->AD);c->AD=_GD();')
    o.t('c->PC=(_GD()<<8)|c->AD;c->brk_flags=0;_OFF(W65C02_VP);')


# -------------------------------------------------------------------------------
def i_nop(o, length, cycles):
    cmt(o, 'NOP')
    for _ in range(length - 1):
        o.t('_SA(c->PC++);')
    for _ in range(cycles - length + 1):
        o.t('')


def i_nop5c(o):
    cmt(o, 'NOP')
    o.t('_SA(c->PC++);')
    o.t('_SA(c->PC++);c->AD=0xff00|_GD();')
    o.t('_SA(c->AD);')
    o.t('_SA(0xffff);')
    o.t(';')
    o.t(';')
    o.t(';')
    o.t(';')


# -------------------------------------------------------------------------------
def i_lda(o):
    cmt(o, 'LDA')
    o.t('c->A=_GD();_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_ldx(o):
    cmt(o, 'LDX')
    o.t('c->X=_GD();_NZ(c->X);')


# -------------------------------------------------------------------------------
def i_ldy(o):
    cmt(o, 'LDY')
    o.t('c->Y=_GD();_NZ(c->Y);')


# -------------------------------------------------------------------------------
def i_sta(o):
    cmt(o, 'STA')
    o.ta('_SD(c->A);_WR();')


# -------------------------------------------------------------------------------
def i_stx(o):
    cmt(o, 'STX')
    o.ta('_SD(c->X);_WR();')


# -------------------------------------------------------------------------------
def i_sty(o):
    cmt(o, 'STY')
    o.ta('_SD(c->Y);_WR();')


# -------------------------------------------------------------------------------
def i_stz(o):
    cmt(o, 'STZ')
    o.ta('_SD(0);_WR();')


# -------------------------------------------------------------------------------
def i_tax(o):
    cmt(o, 'TAX')
    o.t('c->X=c->A;_NZ(c->X);')


# -------------------------------------------------------------------------------
def i_tay(o):
    cmt(o, 'TAY')
    o.t('c->Y=c->A;_NZ(c->Y);')


# -------------------------------------------------------------------------------
def i_txa(o):
    cmt(o, 'TXA')
    o.t('c->A=c->X;_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_tya(o):
    cmt(o, 'TYA')
    o.t('c->A=c->Y;_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_txs(o):
    cmt(o, 'TXS')
    o.t('c->S=c->X;')


# -------------------------------------------------------------------------------
def i_tsx(o):
    cmt(o, 'TSX')
    o.t('c->X=c->S;_NZ(c->X);')


# -------------------------------------------------------------------------------
def i_php(o):
    cmt(o, 'PHP')
    o.t('_SAD(0x0100|c->S--,c->P|W65C02_XF);_WR();')


# -------------------------------------------------------------------------------
def i_plp(o):
    cmt(o, 'PLP')
    o.t('_SA(0x0100|c->S++);')  # read junk byte from current SP
    o.t('_SA(0x0100|c->S);')  # read actual byte
    o.t('c->P=(_GD()|W65C02_BF)&~W65C02_XF;');


# -------------------------------------------------------------------------------
def i_pha(o):
    cmt(o, 'PHA')
    o.t('_SAD(0x0100|c->S--,c->A);_WR();')


# -------------------------------------------------------------------------------
def i_pla(o):
    cmt(o, 'PLA')
    o.t('_SA(0x0100|c->S++);')  # read junk byte from current SP
    o.t('_SA(0x0100|c->S);')  # read actual byte
    o.t('c->A=_GD();_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_phx(o):
    cmt(o, 'PHX')
    o.t('_SAD(0x0100|c->S--,c->X);_WR();')


# -------------------------------------------------------------------------------
def i_plx(o):
    cmt(o, 'PLX')
    o.t('_SA(0x0100|c->S++);')  # read junk byte from current SP
    o.t('_SA(0x0100|c->S);')  # read actual byte
    o.t('c->X=_GD();_NZ(c->X);')


# -------------------------------------------------------------------------------
def i_phy(o):
    cmt(o, 'PHY')
    o.t('_SAD(0x0100|c->S--,c->Y);_WR();')


# -------------------------------------------------------------------------------
def i_ply(o):
    cmt(o, 'PLY')
    o.t('_SA(0x0100|c->S++);')  # read junk byte from current SP
    o.t('_SA(0x0100|c->S);')  # read actual byte
    o.t('c->Y=_GD();_NZ(c->Y);')


# -------------------------------------------------------------------------------
def i_se(o, f):
    cmt(o, 'SE' + flag_name(f))
    o.t('c->P|=' + hex(f) + ';')


# -------------------------------------------------------------------------------
def i_cl(o, f):
    cmt(o, 'CL' + flag_name(f))
    o.t('c->P&=~' + hex(f) + ';')


# -------------------------------------------------------------------------------
def i_br(o, m, v):
    cmt(o, branch_name(m, v))
    # if branch not taken?
    o.t('_SA(c->PC);c->AD=c->PC+(int8_t)_GD();if((c->P&' + hex(m) + ')!=' + hex(v) + '){_FETCH();};')
    # branch taken: shortcut if page not crossed
    o.t('if((c->AD&0xFF00)==(c->PC&0xFF00)){c->PC=c->AD;_FETCH();};')
    # page crossed extra cycle:
    o.t('c->PC=c->AD;')


# -------------------------------------------------------------------------------
def i_bra(o):
    cmt(o, "BRA")
    o.t('_SA(c->PC);c->AD=c->PC+(int8_t)_GD();')
    # branch taken: shortcut if page not crossed
    o.t('if((c->AD&0xFF00)==(c->PC&0xFF00)){c->PC=c->AD;_FETCH();};')
    # page crossed extra cycle:
    o.t('c->PC=c->AD;')


# -------------------------------------------------------------------------------
def i_bb(o, bit_no, set_reset):
    bit_str = str(bit_no)
    if set_reset == 0:
        cmt(o, "BBR" + bit_str)
        mask = '0'
    else:
        cmt(o, "BBS" + bit_str)
        mask = '(1<<' + bit_str + ')'
    bit_mask = '(1<<' + bit_str + ')'
    o.t('_SA(c->PC++);')
    o.t('_SA(_GD());')  # read zp
    o.t('c->AD=_GD();')
    o.t('_SA(c->PC++);')  # read r
    o.t('if((c->AD&' + bit_mask + ')!=' + mask + '){_FETCH();}else{c->AD=c->PC+(int8_t)_GD();_SA(c->PC++);};')
    o.t('if((c->PC&0xff00)==(c->AD&0xff00)){c->PC=c->AD;_FETCH();};')
    o.t('c->PC=c->AD;')


# -------------------------------------------------------------------------------
def i_jmp(o):
    cmt(o, 'JMP')
    o.t('_SA(c->PC++);')
    o.t('_SA(c->PC++);c->AD=_GD();')
    o.t('c->PC=(_GD()<<8)|c->AD;')


# -------------------------------------------------------------------------------
def i_jmpi(o):
    cmt(o, 'JMP (a)')
    o.t('_SA(c->PC++);')
    o.t('_SA(c->PC++);c->AD=_GD();')
    o.t('c->AD|=_GD()<<8;')
    o.t('_SA(c->AD);c->AD+=1;')
    o.t('_SA(c->AD);c->PC=_GD();')
    o.t('c->PC|=_GD()<<8;')


# -------------------------------------------------------------------------------
def i_jmpix(o):
    cmt(o, 'JMP')
    o.t('_SA(c->PC++);')
    o.t('_SA(c->PC++);c->AD=_GD()+c->X;')
    o.t('c->AD=c->AD+(_GD()<<8);')
    o.t('_SA(c->AD);c->AD+=1;')
    o.t('_SA(c->AD);c->PC=_GD();')
    o.t('c->PC|=_GD()<<8;')


# -------------------------------------------------------------------------------
def i_jsr(o):
    cmt(o, 'JSR')
    # read low byte of target address
    o.t('_SA(c->PC++);')
    # put SP on addr bus, next cycle is a junk read
    o.t('_SA(0x0100|c->S);c->AD=_GD();')
    # write PC high byte to stack
    o.t('_SAD(0x0100|c->S--,c->PC>>8);_WR();')
    # write PC low byte to stack
    o.t('_SAD(0x0100|c->S--,c->PC);_WR();')
    # load target address high byte
    o.t('_SA(c->PC);')
    # load PC and done
    o.t('c->PC=(_GD()<<8)|c->AD;')


# -------------------------------------------------------------------------------
def i_rts(o):
    cmt(o, 'RTS')
    # put SP on stack and do a junk read
    o.t('_SA(0x0100|c->S++);')
    # load return address low byte from stack
    o.t('_SA(0x0100|c->S++);')
    # load return address high byte from stack
    o.t('_SA(0x0100|c->S);c->AD=_GD();')
    # put return address in PC, this is one byte before next op, do junk read from PC
    o.t('c->PC=(_GD()<<8)|c->AD;_SA(c->PC++);')
    # next tick is opcode fetch
    o.t('')


# -------------------------------------------------------------------------------
def i_rti(o):
    cmt(o, 'RTI')
    # put SP on stack and do a junk read
    o.t('_SA(0x0100|c->S++);')
    # load processor status flag from stack
    o.t('_SA(0x0100|c->S++);')
    # load return address low byte from stack
    o.t('_SA(0x0100|c->S++);c->P=(_GD()|W65C02_BF)&~W65C02_XF;')
    # load return address high byte from stack
    o.t('_SA(0x0100|c->S);c->AD=_GD();')
    # update PC (which is already placed on the right return-to instruction)
    o.t('c->PC=(_GD()<<8)|c->AD;')


# -------------------------------------------------------------------------------
def i_ora(o):
    cmt(o, 'ORA')
    o.t('c->A|=_GD();_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_and(o):
    cmt(o, 'AND')
    o.t('c->A&=_GD();_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_eor(o):
    cmt(o, 'EOR')
    o.t('c->A^=_GD();_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_adc(o):
    cmt(o, 'ADC')
    o.t('_w65c02_adc(c,_GD());if((c->P&W65C02_DF)==0){_FETCH();}else{_SA(c->PC);};')
    o.t('_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_sbc(o):
    cmt(o, 'SBC')
    o.t('_w65c02_sbc(c,_GD());if((c->P&W65C02_DF)==0){_FETCH();}else{_SA(c->PC);};')
    o.t('_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_cmp(o):
    cmt(o, 'CMP')
    o.t('_w65c02_cmp(c, c->A, _GD());')


# -------------------------------------------------------------------------------
def i_cpx(o):
    cmt(o, 'CPX')
    o.t('_w65c02_cmp(c, c->X, _GD());')


# -------------------------------------------------------------------------------
def i_cpy(o):
    cmt(o, 'CPY')
    o.t('_w65c02_cmp(c, c->Y, _GD());')


# -------------------------------------------------------------------------------
def i_trb(o):
    cmt(o, 'TRB')
    o.t('c->AD=_w65c02_trb(c, _GD());')
    o.t('_SD(c->AD&0xFF);_WR();')


# -------------------------------------------------------------------------------
def i_tsb(o):
    cmt(o, 'TSB')
    o.t('c->AD=_w65c02_tsb(c, _GD());')
    o.t('_SD(c->AD&0xFF);_WR();')


# -------------------------------------------------------------------------------
def i_dec(o):
    cmt(o, 'DEC')
    o.t('c->AD=_GD();')
    o.t('c->AD--;_NZ(c->AD);_SD(c->AD);_WR();')


# -------------------------------------------------------------------------------
def i_inc(o):
    cmt(o, 'INC')
    o.t('c->AD=_GD();')
    o.t('c->AD++;_NZ(c->AD);_SD(c->AD);_WR();')


# -------------------------------------------------------------------------------
def i_dex(o):
    cmt(o, 'DEX')
    o.t('c->X--;_NZ(c->X);')


# -------------------------------------------------------------------------------
def i_dey(o):
    cmt(o, 'DEY')
    o.t('c->Y--;_NZ(c->Y);')


# -------------------------------------------------------------------------------
def i_dea(o):
    cmt(o, 'DEA')
    o.t('c->A--;_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_inx(o):
    cmt(o, 'INX')
    o.t('c->X++;_NZ(c->X);')


# -------------------------------------------------------------------------------
def i_iny(o):
    cmt(o, 'INY')
    o.t('c->Y++;_NZ(c->Y);')


# -------------------------------------------------------------------------------
def i_ina(o):
    cmt(o, 'INA')
    o.t('c->A++;_NZ(c->A);')


# -------------------------------------------------------------------------------
def i_rmb(o, b):
    bit_str = str(b)
    cmt(o, 'RMB' + bit_str)
    o.t('c->AD=_GD()&(~(1<<' + bit_str + '));')
    o.t('_SD(c->AD);_WR();')


# -------------------------------------------------------------------------------
def i_smb(o, b):
    bit_str = str(b)
    cmt(o, 'SMB' + bit_str)
    o.t('c->AD=_GD()|(1<<' + bit_str + ');')
    o.t('_SD(c->AD);_WR();')


# -------------------------------------------------------------------------------
def i_asl(o):
    cmt(o, 'ASL')
    o.t('c->AD=_GD();')
    o.t('_SD(_w65c02_asl(c,c->AD));_WR();')


# -------------------------------------------------------------------------------
def i_asla(o):
    cmt(o, 'ASLA')
    o.t('c->A=_w65c02_asl(c,c->A);')


# -------------------------------------------------------------------------------
def i_lsr(o):
    cmt(o, 'LSR')
    o.t('c->AD=_GD();')
    o.t('_SD(_w65c02_lsr(c,c->AD));_WR();')


# -------------------------------------------------------------------------------
def i_lsra(o):
    cmt(o, 'LSRA')
    o.t('c->A=_w65c02_lsr(c,c->A);')


# -------------------------------------------------------------------------------
def i_rol(o):
    cmt(o, 'ROL')
    o.t('c->AD=_GD();')
    o.t('_SD(_w65c02_rol(c,c->AD));_WR();')


# -------------------------------------------------------------------------------
def i_rola(o):
    cmt(o, 'ROLA')
    o.t('c->A=_w65c02_rol(c,c->A);')


# -------------------------------------------------------------------------------
def i_ror(o):
    cmt(o, 'ROR')
    o.t('c->AD=_GD();')
    o.t('_SD(_w65c02_ror(c,c->AD));_WR();')


# -------------------------------------------------------------------------------
def i_rora(o):
    cmt(o, 'RORA')
    o.t('c->A=_w65c02_ror(c,c->A);')


# -------------------------------------------------------------------------------
def i_bit(o):
    cmt(o, 'BIT')
    o.t('_w65c02_bit(c,_GD());')


def i_bit_imm(o):
    cmt(o, 'BIT')
    o.t('_w65c02_bit_imm(c,_GD());')


# -------------------------------------------------------------------------------
def i_wai(o):
    cmt(o, 'WAI')
    o.t('_SA(c->PC);if((pins&(W65C02_IRQ|W65C02_NMI))==0){c->waiting=1;_ON(W65C02_RDY);}')
    o.t('')
    o.t('')


# -------------------------------------------------------------------------------
def i_stp(o):
    cmt(o, 'STP')
    o.t('_SA(c->PC);c->stopped=1;')
    o.t('')


# -------------------------------------------------------------------------------
def enc_op(op):
    o = opcode(op)

    # decode the opcode byte
    cc = op & 3
    bbb = (op >> 2) & 7
    aaa = (op >> 5) & 7
    addr_mode = ops[cc][bbb][aaa][0]
    mem_access = ops[cc][bbb][aaa][1]
    # addressing mode
    enc_addr(o, addr_mode, mem_access)
    # actual instruction
    if cc == 0:
        if aaa == 0:
            if bbb == 0:
                i_brk(o)
            elif bbb in [1, 3]:
                i_tsb(o)
            elif bbb == 2:
                i_php(o)
            elif bbb == 4:
                i_br(o, NF, 0)  # BPL
            elif bbb in [5, 7]:
                i_trb(o)
            elif bbb == 6:
                i_cl(o, CF)
        elif aaa == 1:
            if bbb == 0:
                i_jsr(o)
            elif bbb == 2:
                i_plp(o)
            elif bbb == 4:
                i_br(o, NF, NF)  # BMI
            elif bbb == 6:
                i_se(o, CF)
            else:
                i_bit(o)
        elif aaa == 2:
            if bbb == 0:
                i_rti(o)
            elif bbb == 1:
                i_nop(o, 1, 1)
            elif bbb == 2:
                i_pha(o)
            elif bbb == 3:
                i_jmp(o)
            elif bbb == 4:
                i_br(o, VF, 0)  # BVC
            elif bbb == 5:
                i_nop(o, 1, 1)
            elif bbb == 6:
                i_cl(o, IF)
            elif bbb == 7:
                i_nop5c(o)
        elif aaa == 3:
            if bbb == 0:
                i_rts(o)
            elif bbb in [1, 5]:
                i_stz(o)
            elif bbb == 2:
                i_pla(o)
            elif bbb == 3:
                i_jmpi(o)
            elif bbb == 4:
                i_br(o, VF, VF)  # BVS
            elif bbb == 6:
                i_se(o, IF)
            elif bbb == 7:
                i_jmpix(o)
        elif aaa == 4:
            if bbb == 0:
                i_bra(o)
            elif bbb in [1, 3, 5]:
                i_sty(o)
            elif bbb == 2:
                i_dey(o)
            elif bbb == 4:
                i_br(o, CF, 0)  # BCC
            elif bbb == 6:
                i_tya(o)
            elif bbb == 7:
                i_stz(o)
        elif aaa == 5:
            if bbb == 2:
                i_tay(o)
            elif bbb == 4:
                i_br(o, CF, CF)  # BCS
            elif bbb == 6:
                i_cl(o, VF)
            else:
                i_ldy(o)
        elif aaa == 6:
            if bbb == 2:
                i_iny(o)
            elif bbb == 4:
                i_br(o, ZF, 0)  # BNE
            elif bbb == 5:
                i_nop(o, 1, 1)
            elif bbb == 6:
                i_cl(o, DF)
            elif bbb == 7:
                i_nop(o, 1, 1)
            else:
                i_cpy(o)
        elif aaa == 7:
            if bbb == 2:
                i_inx(o)
            elif bbb == 4:
                i_br(o, ZF, ZF)  # BEQ
            elif bbb == 5:
                i_nop(o, 1, 1)
            elif bbb == 6:
                i_se(o, DF)
            elif bbb == 7:
                i_nop(o, 1, 1)
            else:
                i_cpx(o)
    elif cc == 1:
        if aaa == 0:
            i_ora(o)
        elif aaa == 1:
            i_and(o)
        elif aaa == 2:
            i_eor(o)
        elif aaa == 3:
            i_adc(o)
        elif aaa == 4:
            if bbb == 2:
                i_bit_imm(o)
            else:
                i_sta(o)
        elif aaa == 5:
            i_lda(o)
        elif aaa == 6:
            i_cmp(o)
        elif aaa == 7:
            i_sbc(o)
    elif cc == 2:
        if aaa == 0:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_asla(o)
            elif bbb == 4:
                i_ora(o)
            elif bbb == 6:
                i_ina(o)
            else:
                i_asl(o)
        elif aaa == 1:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_rola(o)
            elif bbb == 4:
                i_and(o)
            elif bbb == 6:
                i_dea(o)
            else:
                i_rol(o)
        elif aaa == 2:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_lsra(o)
            elif bbb == 4:
                i_eor(o)
            elif bbb == 6:
                i_phy(o)
            else:
                i_lsr(o)
        elif aaa == 3:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_rora(o)
            elif bbb == 4:
                i_adc(o)
            elif bbb == 6:
                i_ply(o)
            else:
                i_ror(o)
        elif aaa == 4:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_txa(o)
            elif bbb == 4:
                i_sta(o)
            elif bbb == 6:
                i_txs(o)
            elif bbb == 7:
                i_stz(o)
            else:
                i_stx(o)
        elif aaa == 5:
            if bbb == 2:
                i_tax(o)
            elif bbb == 4:
                i_lda(o)
            elif bbb == 6:
                i_tsx(o)
            else:
                i_ldx(o)
        elif aaa == 6:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_dex(o)
            elif bbb == 4:
                i_cmp(o)
            elif bbb == 6:
                i_phx(o)
            else:
                i_dec(o)
        elif aaa == 7:
            if bbb == 0:
                i_nop(o, 2, 2)
            elif bbb == 2:
                i_nop(o, 1, 1)
            elif bbb == 4:
                i_sbc(o)
            elif bbb == 6:
                i_plx(o)
            else:
                i_inc(o)
    elif cc == 3:
        if aaa == 0:
            if bbb == 1:
                i_rmb(o, 0)
            elif bbb == 3:
                i_bb(o, 0, 0)
            elif bbb == 5:
                i_rmb(o, 1)
            elif bbb == 7:
                i_bb(o, 1, 0)
            else:
                i_nop(o, 1, 1)
        elif aaa == 1:
            if bbb == 1:
                i_rmb(o, 2)
            elif bbb == 3:
                i_bb(o, 2, 0)
            elif bbb == 5:
                i_rmb(o, 3)
            elif bbb == 7:
                i_bb(o, 3, 0)
            else:
                i_nop(o, 1, 1)
        elif aaa == 2:
            if bbb == 1:
                i_rmb(o, 4)
            elif bbb == 3:
                i_bb(o, 4, 0)
            elif bbb == 5:
                i_rmb(o, 5)
            elif bbb == 7:
                i_bb(o, 5, 0)
            else:
                i_nop(o, 1, 1)
        elif aaa == 3:
            if bbb == 1:
                i_rmb(o, 6)
            elif bbb == 3:
                i_bb(o, 6, 0)
            elif bbb == 5:
                i_rmb(o, 7)
            elif bbb == 7:
                i_bb(o, 7, 0)
            else:
                i_nop(o, 1, 1)
        elif aaa == 4:
            if bbb == 1:
                i_smb(o, 0)
            elif bbb == 3:
                i_bb(o, 0, 1)
            elif bbb == 5:
                i_smb(o, 1)
            elif bbb == 7:
                i_bb(o, 1, 1)
            else:
                i_nop(o, 1, 1)
        elif aaa == 5:
            if bbb == 1:
                i_smb(o, 2)
            elif bbb == 3:
                i_bb(o, 2, 1)
            elif bbb == 5:
                i_smb(o, 3)
            elif bbb == 7:
                i_bb(o, 3, 1)
            else:
                i_nop(o, 1, 1)
        elif aaa == 6:
            if bbb == 1:
                i_smb(o, 4)
            elif bbb == 2:
                i_wai(o)
            elif bbb == 3:
                i_bb(o, 4, 1)
            elif bbb == 5:
                i_smb(o, 5)
            elif bbb == 6:
                i_stp(o)
            elif bbb == 7:
                i_bb(o, 5, 1)
            else:
                i_nop(o, 1, 1)
        elif aaa == 7:
            if bbb == 1:
                i_smb(o, 6)
            elif bbb == 3:
                i_bb(o, 6, 1)
            elif bbb == 5:
                i_smb(o, 7)
            elif bbb == 7:
                i_bb(o, 7, 1)
            else:
                i_nop(o, 1, 1)

    # fetch next opcode byte
    if mem_access in [M_R_, M___]:
        o.ta('_FETCH();')
    else:
        o.t('_FETCH();')
    return o


# -------------------------------------------------------------------------------
#   execution starts here
#
for op in range(0, 256):
    write_op(enc_op(op))

with open(InpPath, 'r') as inf:
    templ = Template(inf.read())
    c_src = templ.safe_substitute(decode_block=out_lines)
    with open(OutPath, 'w') as outf:
        outf.write(c_src)
