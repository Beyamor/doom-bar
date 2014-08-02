(ns bar.gameboy
  (:require [bar.ops :refer [no-op]])
  (:require-macros [bar.gameboy.ops-translators :refer [LD INC DEC ADD JR]]))

(def NOP  bar.ops/no-op)
(def RLCA bar.ops/rlca)
(def RLA bar.ops/rla)
(def RRCA bar.ops/rrca)
(def RRA bar.ops/rra)
(def STOP [0 (fn [& _] (throw "STOP not implemented"))])
(def DAA bar.ops/daa)
(def CPL bar.ops/ones-complement)
(def SCF bar.ops/set-carry-flag)
(def CCF bar.ops/complement-carry-flag)

(def ops
  [; 0x0
   NOP
   (LD BC, d16)
   (LD (BC), A)
   (INC BC)
   (DEC B)
   (LD B, d8)
   RLCA
   (LD (a16), SP)
   (ADD HL, BC)
   (LD A, (BC))
   (DEC BC)
   (INC C)
   (DEC C)
   RRCA

   ;0x1
   STOP
   (LD DE, d16)
   (LD (DE), A)
   (INC DE)
   (INC D)
   (DEC D)
   (LD D, d8)
   RLA
   (JR r8)
   (ADD HL, DE)
   (DEC DE)
   (INC E)
   (DEC E)
   (LD E, d8)
   RRA
   
   ;0x2
   (JR NZ, r8)
   (LD HL, d16)
   (LD (HL+), A)
   (INC HL)
   (INC H)
   (DEC H)
   (LD H, d8)
   DAA
   (JR Z, r8)
   (ADD HL, SP)
   (LD A, (HL+))
   (DEC HL)
   (INC L)
   (DEC L)
   (LD L, d8)
   CPL

   ;0x3
   (JR NC, r8)
   (LD SP, d16)
   (INC SP)
   (INC (HL))
   (DEC (HL))
   (LD (HL), d8)
   SCF
   (JR C, r8)
   (ADD HL, SP)
   (LD A, (HL-))
   (DEC SP)
   (INC A)
   (DEC A)
   (LD A, d8)
   CCF])

(def bios
  [0x31 0xFE 0xFF 0xAF 0x21 0xFF 0x9F 0x32 0xCB 0x7C 0x20 0xFB 0x21 0x26 0xFF 0x0E
   0x11 0x3E 0x80 0x32 0xE2 0x0C 0x3E 0xF3 0xE2 0x32 0x3E 0x77 0x77 0x3E 0xFC 0xE0
   0x47 0x11 0x04 0x01 0x21 0x10 0x80 0x1A 0xCD 0x95 0x00 0xCD 0x96 0x00 0x13 0x7B
   0xFE 0x34 0x20 0xF3 0x11 0xD8 0x00 0x06 0x08 0x1A 0x13 0x22 0x23 0x05 0x20 0xF9
   0x3E 0x19 0xEA 0x10 0x99 0x21 0x2F 0x99 0x0E 0x0C 0x3D 0x28 0x08 0x32 0x0D 0x20
   0xF9 0x2E 0x0F 0x18 0xF3 0x67 0x3E 0x64 0x57 0xE0 0x42 0x3E 0x91 0xE0 0x40 0x04
   0x1E 0x02 0x0E 0x0C 0xF0 0x44 0xFE 0x90 0x20 0xFA 0x0D 0x20 0xF7 0x1D 0x20 0xF2
   0x0E 0x13 0x24 0x7C 0x1E 0x83 0xFE 0x62 0x28 0x06 0x1E 0xC1 0xFE 0x64 0x20 0x06
   0x7B 0xE2 0x0C 0x3E 0x87 0xF2 0xF0 0x42 0x90 0xE0 0x42 0x15 0x20 0xD2 0x05 0x20
   0x4F 0x16 0x20 0x18 0xCB 0x4F 0x06 0x04 0xC5 0xCB 0x11 0x17 0xC1 0xCB 0x11 0x17
   0x05 0x20 0xF5 0x22 0x23 0x22 0x23 0xC9 0xCE 0xED 0x66 0x66 0xCC 0x0D 0x00 0x0B
   0x03 0x73 0x00 0x83 0x00 0x0C 0x00 0x0D 0x00 0x08 0x11 0x1F 0x88 0x89 0x00 0x0E
   0xDC 0xCC 0x6E 0xE6 0xDD 0xDD 0xD9 0x99 0xBB 0xBB 0x67 0x63 0x6E 0x0E 0xEC 0xCC
   0xDD 0xDC 0x99 0x9F 0xBB 0xB9 0x33 0x3E 0x3c 0x42 0xB9 0xA5 0xB9 0xA5 0x42 0x4C
   0x21 0x04 0x01 0x11 0xA8 0x00 0x1A 0x13 0xBE 0x20 0xFE 0x23 0x7D 0xFE 0x34 0x20
   0xF5 0x06 0x19 0x78 0x86 0x23 0x05 0x20 0xFB 0x86 0x20 0xFE 0x3E 0x01 0xE0 0x50])
