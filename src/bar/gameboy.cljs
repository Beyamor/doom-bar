(ns bar.gameboy
  (:require [bar.ops :as ops]))

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

(def ADDr-a (ops/addr :a))
(def ADDr-b (ops/addr :b))
(def ADDr-c (ops/addr :c))
(def ADDr-d (ops/addr :d))
(def ADDr-e (ops/addr :e))
(def ADDr-h (ops/addr :h))
(def ADDr-l (ops/addr :l))

(comment
(def op-map
  [; 00
   NOP       LDBCnn    LDBCmA    INCBC
   INCr-b    DECr-b    LDrn-b    RLCA
   LDmmSP    ADDHLBC   LDABCm    DECBC
   INCr-c    DECr-c    LDrn-c    RRCA

   ; 10
   DJNZn     LDDEnn    LDDEmA    INCDE
   INCr-d    DECr-d    LDrn-d    RLA
   JRn       ADDHLDE   LDADEm    DECDE
   INCr-e    DECr-e    LDrn-e    RRA

   ; 20
   JRNZn     LDHLnn    LDHLIA    INCHL
   INCr-h    DECr-h    LDrn-h    DAA
   JRZn      ADDHLHL   LDAHLI    DECHL
   INCr-l    DECr-l    LDrn-l    CPL

   ; 30
   JRNCn     LDSPnn    LDHLDA    INCSP
   INCHLm    DECHLm    LDHLmn    SCF
   JRCn      ADDHLSP   LDAHLD    DECSP
   INCr-a    DECr-a    LDrn-a    CCF

   ; 40
   LDrr-bb   LDrr-bc   LDrr-bd   LDrr-be
   LDrr-bh   LDrr-bl   LDrHLm-b  LDrr-ba
   LDrr-cb   LDrr-cc   LDrr-cd   LDrr-ce
   LDrr-ch   LDrr-cl   LDrHLm-c  LDrr-ca

   ; 50
   LDrr-db   LDrr-dc   LDrr-dd   LDrr-de
   LDrr-dh   LDrr-dl   LDrHLm-d  LDrr-da
   LDrr-eb   LDrr-ec   LDrr-ed   LDrr-ee
   LDrr-eh   LDrr-el   LDrHLm-e  LDrr-ea

   ; 60
   LDrr-hb   LDrr-hc   LDrr-hd   LDrr-he
   LDrr-hh   LDrr-hl   LDrHLm-h  LDrr-ha
   LDrr-lb   LDrr-lc   LDrr-ld   LDrr-le
   LDrr-lh   LDrr-ll   LDrHLm-l  LDrr-la

   ; 70
   LDHLmr-b  LDHLmr-c  LDHLmr-d  LDHLmr-e
   LDHLmr-h  LDHLmr-l  HALT      LDHLmr-a
   LDrr-ab   LDrr-ac   LDrr-ad   LDrr-ae
   LDrr-ah   LDrr-al   LDrHLm-a  LDrr-aa

   ; 80
   ADDr-b    ADDr-c    ADDr-d    ADDr-e
   ADDr-h    ADDr-l    ADDHL     ADDr-a
   ADCr-b    ADCr-c    ADCr-d    ADCr-e
   ADCr-h    ADCr-l    ADCHL     ADCr-a

   ; 90
   SUBr-b    SUBr-c    SUBr-d    SUBr-e
   SUBr-h    SUBr-l    SUBHL     SUBr-a
   SBCr-b    SBCr-c    SBCr-d    SBCr-e
   SBCr-h    SBCr-l    SBCHL     SBCr-a

   ; A0
   ANDr-b    ANDr-c    ANDr-d    ANDr-e
   ANDr-h    ANDr-l    ANDHL     ANDr-a
   XORr-b    XORr-c    XORr-d    XORr-e
   XORr-h    XORr-l    XORHL     XORr-a

   ; B0
   ORr-b     ORr-c     ORr-d     ORr-e
   ORr-h     ORr-l     ORHL      ORr-a
   CPr-b     CPr-c     CPr-d     CPr-e
   CPr-h     CPr-l     CPHL      CPr-a

   ; C0
   RETNZ     POPBC     JPNZnn    JPnn
   CALLNZnn  PUSHBC    ADDn      RST00
   RETZ      RET       JPZnn     MAPcb
   CALLZnn   CALLnn    ADCn      RST08

   ; D0
   RETNC     POPDE     JPNCnn    XX
   CALLNCnn  PUSHDE    SUBn      RST10
   RETC      RETI      JPCnn     XX
   CALLCnn   XX        SBCn      RST18

   ; E0
   LDIOnA    POPHL     LDIOCA    XX
   XX        PUSHHL    ANDn      RST20
   ADDSPn    JPHL      LDmmA     XX
   XX        XX        XORn      RST28

   ; F0
   LDAIOn    POPAF     LDAIOC    DI
   XX        PUSHAF    ORn       RST30
   LDHLSPn   XX        LDAmm     EI
   XX        XX        CPn       RST38])

(def cb-op-map
  [; CB00
   RLCr-b   RLCr-c   RLCr-d   RLCr-e
   RLCr-h   RLCr-l   RLCHL    RLCr-a
   RRCr-b   RRCr-c   RRCr-d   RRCr-e
   RRCr-h   RRCr-l   RRCHL    RRCr-a

   ; CB10
   RLr-b    RLr-c    RLr-d    RLr-e
   RLr-h    RLr-l    RLHL     RLr-a
   RRr-b    RRr-c    RRr-d    RRr-e
   RRr-h    RRr-l    RRHL     RRr-a

   ; CB20
   SLAr-b   SLAr-c   SLAr-d   SLAr-e
   SLAr-h   SLAr-l   XX       SLAr-a
   SRAr-b   SRAr-c   SRAr-d   SRAr-e
   SRAr-h   SRAr-l   XX       SRAr-a

   ; CB30
   SWAPr-b  SWAPr-c  SWAPr-d  SWAPr-e
   SWAPr-h  SWAPr-l  XX       SWAPr-a
   SRLr-b   SRLr-c   SRLr-d   SRLr-e
   SRLr-h   SRLr-l   XX       SRLr-a

   ; CB40
   BIT0b    BIT0c    BIT0d    BIT0e
   BIT0h    BIT0l    BIT0m    BIT0a
   BIT1b    BIT1c    BIT1d    BIT1e
   BIT1h    BIT1l    BIT1m    BIT1a

   ; CB50
   BIT2b    BIT2c    BIT2d    BIT2e
   BIT2h    BIT2l    BIT2m    BIT2a
   BIT3b    BIT3c    BIT3d    BIT3e
   BIT3h    BIT3l    BIT3m    BIT3a

   ; CB60
   BIT4b    BIT4c    BIT4d    BIT4e
   BIT4h    BIT4l    BIT4m    BIT4a
   BIT5b    BIT5c    BIT5d    BIT5e
   BIT5h    BIT5l    BIT5m    BIT5a

   ; CB70
   BIT6b    BIT6c    BIT6d    BIT6e
   BIT6h    BIT6l    BIT6m    BIT6a
   BIT7b    BIT7c    BIT7d    BIT7e
   BIT7h    BIT7l    BIT7m    BIT7a

   ; CB80
   RES0b    RES0c    RES0d    RES0e
   RES0h    RES0l    RES0m    RES0a
   RES1b    RES1c    RES1d    RES1e
   RES1h    RES1l    RES1m    RES1a

   ; CB90
   RES2b    RES2c    RES2d    RES2e
   RES2h    RES2l    RES2m    RES2a
   RES3b    RES3c    RES3d    RES3e
   RES3h    RES3l    RES3m    RES3a

   ; CBA0
   RES4b    RES4c    RES4d    RES4e
   RES4h    RES4l    RES4m    RES4a
   RES5b    RES5c    RES5d    RES5e
   RES5h    RES5l    RES5m    RES5a

   ; CBB0
   RES6b    RES6c    RES6d    RES6e
   RES6h    RES6l    RES6m    RES6a
   RES7b    RES7c    RES7d    RES7e
   RES7h    RES7l    RES7m    RES7a

   ; CBC0
   SET0b    SET0c    SET0d    SET0e
   SET0h    SET0l    SET0m    SET0a
   SET1b    SET1c    SET1d    SET1e
   SET1h    SET1l    SET1m    SET1a

   ; CBD0
   SET2b    SET2c    SET2d    SET2e
   SET2h    SET2l    SET2m    SET2a
   SET3b    SET3c    SET3d    SET3e
   SET3h    SET3l    SET3m    SET3a

   ; CBE0
   SET4b    SET4c    SET4d    SET4e
   SET4h    SET4l    SET4m    SET4a
   SET5b    SET5c    SET5d    SET5e
   SET5h    SET5l    SET5m    SET5a

   ; CBF0
   SET6b    SET6c    SET6d    SET6e
   SET6h    SET6l    SET6m    SET6a
   SET7b    SET7c    SET7d    SET7e
   SET7h    SET7l    SET7m    SET7a])
)
