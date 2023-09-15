;;
F000: 89 					;CHECKSUM BYTE
F001: 28       					;"(C)1983 WILLIAMS ELECTRONICS"
F002: 43       
F003: 29 31    
F005: 39      
F006: 38     
F007: 33       
F008: 20 57   
F00A: 49       
F00B: 4C       
F00C: 4C       
F00D: 49       
F00E: 41       
F00F: 4D       
F010: 53       
F011: 20 45    
F013: 4C       
F014: 45       
F015: 43       
F016: 54       
F017: 52      
F018: 4F       
F019: 4E       
F01A: 49       
F01B: 43       
F01C: 53      
;;
F01D: 0F       sei  				;SET INTERRUPT MASK
F01E: 8E 00 7F lds  #$007F			;INITIALIZE STACK POINTER
F021: CE 04 00 ldx  #$0400			;INDEX TO PIA
F024: 6F 01    clr  (x+$01)			;ACCESS DDRA
F026: 6F 03    clr  (x+$03)			;ACCESS DDRB
F028: 86 FF    lda  #$FF			;PA0-PA7
F02A: A7 00    sta  (x+$00)			;SET SIDE A AS OUTPUTS
F02C: 6F 02    clr  (x+$02)			;;(in Mystic Marathon, the opcodes are flipped...A702,6F00
F02E: 86 37    lda  #$37			;CB2 LOW, IRQ ALLOWED
F030: A7 03    sta  (x+$03)			;PROGRAM B CONTROL REG
F032: 86 3C    lda  #$3C			;CA2 SET INIT HIGH, NO IRQS
F034: A7 01    sta  (x+$01)			;PROGRAM A SIDE
F036: 97 05    sta  $05				;;START RANDOM GENERATOR?
F038: 4F       clra 				;;(looks like we're clearing flags)
F039: 97 03    sta  $03
F03B: 97 00    sta  $00
F03D: 97 01    sta  $01
F03F: 97 02    sta  $02
F041: 97 04    sta  $04
F043: 0E       cli  				;CLEAR INTERRUPTS
F044: 20 FE    bra  $F044			;WAIT FOR INTERRUPT
F046: CE 00 01 ldx  #$0001
F049: DF 11    stx  $11
F04B: CE 03 80 ldx  #$0380
F04E: DF 13    stx  $13
F050: 7F 04 00 clr  $0400
F053: DE 11    ldx  $11
F055: 08       inx  
F056: DF 11    stx  $11
F058: 09       dex  
F059: 26 FD    bne  $F058
F05B: 73 04 00 com  $0400
F05E: DE 13    ldx  $13
F060: 09       dex  
F061: 26 FD    bne  $F060
F063: 20 EB    bra  $F050
F065: 7F 04 02 clr  $0402
F068: CE F0 AC ldx  #$F0AC
F06B: DF 13    stx  $13
F06D: DE 13    ldx  $13
F06F: A6 00    lda  (x+$00)
F071: 27 33    beq  $F0A6
F073: E6 01    ldb  (x+$01)
F075: C4 F0    andb #$F0
F077: D7 12    stb  $12
F079: E6 01    ldb  (x+$01)
F07B: 08       inx  
F07C: 08       inx  
F07D: DF 13    stx  $13
F07F: 97 11    sta  $11
F081: C4 0F    andb #$0F
F083: 96 12    lda  $12
F085: B7 04 00 sta  $0400
F088: 96 11    lda  $11
F08A: CE 00 05 ldx  #$0005
F08D: 09       dex  
F08E: 26 FD    bne  $F08D
F090: 4A       deca 
F091: 26 F7    bne  $F08A
F093: 7F 04 00 clr  $0400
F096: 96 11    lda  $11
F098: CE 00 05 ldx  #$0005
F09B: 09       dex  
F09C: 26 FD    bne  $F09B
F09E: 4A       deca 
F09F: 26 F7    bne  $F098
F0A1: 5A       decb 
F0A2: 26 DF    bne  $F083
F0A4: 20 C7    bra  $F06D
F0A6: 86 80    lda  #$80
F0A8: B7 04 02 sta  $0402
F0AB: 39       rts  
F0AC: 01       nop  
F0AD: FC       illegal
F0AE: 02       illegal
F0AF: FC       illegal
F0B0: 03       illegal
F0B1: F8 04 F8 eorb $04F8
F0B4: 06       tap  
F0B5: F8 08 F4 eorb $08F4
F0B8: 0C       clc  
F0B9: F4 10 F4 andb $10F4
F0BC: 20 F2    bra  $F0B0
F0BE: 40       nega 
F0BF: F1 60 F1 cmpb $60F1
F0C2: 80 F1    suba #$F1
F0C4: A0 F1    suba (x+$F1)
F0C6: C0 F1    subb #$F1
F0C8: 00       illegal
F0C9: 00       illegal
*
*VARI LOADER
*
F0CA: 16       tab  
F0CB: 48       asla 
F0CC: 48       asla 
F0CD: 48       asla 
F0CE: 1B       aba  
F0CF: CE 00 11 ldx  #$0011			;;(LOCRAM)
F0D2: DF 0D    stx  $0D
F0D4: CE F6 23 ldx  #$F623
F0D7: BD F5 C3 jsr  $F5C3
F0DA: C6 09    ldb  #$09
F0DC: 7E F3 A8 jmp  $F3A8
*
*VARIABLE DUTY CYCLE SQUARE WAVE ROUTINE
*
F0DF: 96 19    lda  $19
F0E1: B7 04 00 sta  $0400
F0E4: 96 11    lda  $11
F0E6: 97 1A    sta  $1A
F0E8: 96 12    lda  $12
F0EA: 97 1B    sta  $1B
F0EC: DE 16    ldx  $16
F0EE: 96 1A    lda  $1A
F0F0: 73 04 00 com  $0400
F0F3: 09       dex  
F0F4: 27 10    beq  $F106
F0F6: 4A       deca 
F0F7: 26 FA    bne  $F0F3
F0F9: 73 04 00 com  $0400
F0FC: 96 1B    lda  $1B
F0FE: 09       dex  
F0FF: 27 05    beq  $F106
F101: 4A       deca 
F102: 26 FA    bne  $F0FE
F104: 20 E8    bra  $F0EE
F106: B6 04 00 lda  $0400
F109: 2B 01    bmi  $F10C
F10B: 43       coma 
F10C: 8B 00    adda #$00
F10E: B7 04 00 sta  $0400
F111: 96 1A    lda  $1A
F113: 9B 13    adda $13
F115: 97 1A    sta  $1A
F117: 96 1B    lda  $1B
F119: 9B 14    adda $14
F11B: 97 1B    sta  $1B
F11D: 91 15    cmpa $15
F11F: 26 CB    bne  $F0EC
F121: 96 18    lda  $18
F123: 27 06    beq  $F12B
F125: 9B 11    adda $11
F127: 97 11    sta  $11
F129: 26 B9    bne  $F0E4
;;VARX
F12B: 39       rts  
*
*LAUNCH
*
;; Variation of LAUNCH?
;;
F12C: 86 FF    lda  #$FF
F12E: 97 18    sta  $18
F130: 86 D0    lda  #$D0			;;(originally #$60)
F132: C6 0E    ldb  #$0E			;;(originally #$FF)
F134: 20 10    bra  $F146
*
*LIGHTNING
*
F136: 86 01    lda  #$01
F138: 97 18    sta  $18
F13A: C6 03    ldb  #$03
F13C: 20 08    bra  $F146
F13E: 86 FF    lda  #$FF
F140: 97 18    sta  $18
F142: 86 D0    lda  #$D0
F144: C6 15    ldb  #$15
*
*LIGHTNING+APPEAR NOISE ROUTINE
*
F146: 97 17    sta  $17
F148: 86 FF    lda  #$FF
F14A: B7 04 00 sta  $0400
F14D: D7 13    stb  $13
F14F: D6 13    ldb  $13
F151: 96 06    lda  $06
F153: 44       lsra 
F154: 44       lsra 
F155: 44       lsra 
F156: 98 06    eora $06
F158: 44       lsra 
F159: 76 00 05 ror  $0005			;;(HI)
F15C: 76 00 06 ror  $0006			;;(LO)
F15F: 24 03    bcc  $F164
F161: 73 04 00 com  $0400
;;LITE2
F164: 96 17    lda  $17
;;LITE3
F166: 4A       deca 
F167: 26 FD    bne  $F166
F169: 5A       decb 
F16A: 26 E5    bne  $F151
F16C: 96 17    lda  $17
F16E: 9B 18    adda $18
F170: 97 17    sta  $17
F172: 26 DB    bne  $F14F
F174: 39       rts  
*
*TURBO
*
F175: 86 A0    lda  #$A0			;;(new)
F177: 20 02    bra  $F17B			;;(new)
*
*TURBO
*
F179: 86 20    lda  #$20
F17B: 97 13    sta  $13
F17D: 97 16    sta  $16
F17F: 86 00    lda  #$00			;;(originally #$1)
F181: CE 00 01 ldx  #$0001
F184: C6 FF    ldb  #$FF
F186: 20 0D    bra  $F195
*
*TURBO
*
F188: 86 20    lda  #$20
F18A: 97 13    sta  $13
F18C: 97 16    sta  $16
F18E: 86 01    lda  #$01
F190: CE 00 01 ldx  #$0001
F193: C6 C8    ldb  #$C8			;;(originally #$FF)
*
*WHITE NOISE ROUTINE
*X=INIT PERIOD, ACCB=INIT AMP, ACCA DECAY RATE
*CYCNT=CYCLE COUNT, NFFLG= FREQ DECAY FLAG
*
F195: 97 11    sta  $11
F197: DF 14    stx  $14
F199: D7 12    stb  $12
F19B: D6 13    ldb  $13
F19D: 96 06    lda  $06
F19F: 44       lsra 
F1A0: 44       lsra 
F1A1: 44       lsra 
F1A2: 98 06    eora $06
F1A4: 44       lsra 
F1A5: 76 00 05 ror  $0005
F1A8: 76 00 06 ror  $0006
F1AB: 86 00    lda  #$00
F1AD: 24 02    bcc  $F1B1
F1AF: 96 12    lda  $12
F1B1: B7 04 00 sta  $0400
F1B4: DE 14    ldx  $14
F1B6: 09       dex  
F1B7: 26 FD    bne  $F1B6
F1B9: 5A       decb 
F1BA: 26 E1    bne  $F19D
F1BC: D6 12    ldb  $12
F1BE: D0 11    subb $11
F1C0: 27 09    beq  $F1CB
F1C2: DE 14    ldx  $14
F1C4: 08       inx  
F1C5: 96 16    lda  $16
F1C7: 27 D0    beq  $F199
F1C9: 20 CC    bra  $F197
F1CB: 39       rts  
F1CC: C6 01    ldb  #$01
F1CE: D7 00    stb  $00
F1D0: CE F1 F5 ldx  #$F1F5
F1D3: 20 3B    bra  $F210
F1D5: CE F1 FB ldx  #$F1FB
F1D8: 20 36    bra  $F210
F1DA: CE F2 01 ldx  #$F201
F1DD: 20 31    bra  $F210
F1DF: CE F1 E9 ldx  #$F1E9
F1E2: 20 2C    bra  $F210
F1E4: CE F1 EF ldx  #$F1EF
F1E7: 20 27    bra  $F210
F1E9: 00       illegal
F1EA: 00       illegal
F1EB: 01       nop  
F1EC: C0 00    subb #$00
F1EE: 80 01    suba #$01
F1F0: 00       illegal
F1F1: 01       nop  
F1F2: FF 0C 00 stx  $0C00
F1F5: 00       illegal
F1F6: 00       illegal
F1F7: 00       illegal
F1F8: 01       nop  
F1F9: 00       illegal
F1FA: 00       illegal
F1FB: 00       illegal
F1FC: 00       illegal
F1FD: 00       illegal
F1FE: 05       illegal
F1FF: 00       illegal
F200: 00       illegal
F201: 01       nop  
F202: 00       illegal
F203: 01       nop  
F204: FF 03 E8 stx  $03E8
F207: 01       nop  
F208: 01       nop  
F209: 01       nop  
F20A: 40       nega 
F20B: 10       sba  
F20C: 00       illegal
F20D: CE F2 07 ldx  #$F207
F210: A6 00    lda  (x+$00)
F212: 97 18    sta  $18
F214: A6 01    lda  (x+$01)
F216: 97 14    sta  $14
F218: A6 02    lda  (x+$02)
F21A: E6 03    ldb  (x+$03)
F21C: EE 04    ldx  (x+$04)
F21E: 97 17    sta  $17
F220: D7 11    stb  $11
F222: DF 15    stx  $15
F224: 7F 00 13 clr  $0013
F227: DE 15    ldx  $15
F229: B6 04 00 lda  $0400
F22C: 16       tab  
F22D: 54       lsrb 
F22E: 54       lsrb 
F22F: 54       lsrb 
F230: D8 06    eorb $06
F232: 54       lsrb 
F233: 76 00 05 ror  $0005
F236: 76 00 06 ror  $0006
F239: D6 11    ldb  $11
F23B: 7D 00 18 tst  $0018
F23E: 27 04    beq  $F244
F240: D4 05    andb $05
F242: DB 14    addb $14
F244: D7 12    stb  $12
F246: D6 13    ldb  $13
F248: 91 06    cmpa $06
F24A: 22 12    bhi  $F25E
F24C: 09       dex  
F24D: 27 26    beq  $F275
F24F: B7 04 00 sta  $0400
F252: DB 13    addb $13
F254: 99 12    adca $12
F256: 25 16    bcs  $F26E
F258: 91 06    cmpa $06
F25A: 23 F0    bls  $F24C
F25C: 20 10    bra  $F26E
F25E: 09       dex  
F25F: 27 14    beq  $F275
F261: B7 04 00 sta  $0400
F264: D0 13    subb $13
F266: 92 12    sbca $12
F268: 25 04    bcs  $F26E
F26A: 91 06    cmpa $06
F26C: 22 F0    bhi  $F25E
F26E: 96 06    lda  $06
F270: B7 04 00 sta  $0400
F273: 20 B7    bra  $F22C
F275: D6 17    ldb  $17
F277: 27 B3    beq  $F22C
F279: 96 11    lda  $11
F27B: D6 13    ldb  $13
F27D: 44       lsra 
F27E: 56       rorb 
F27F: 44       lsra 
F280: 56       rorb 
F281: 44       lsra 
F282: 56       rorb 
F283: 43       coma 
F284: 50       negb 
F285: 82 FF    sbca #$FF
F287: DB 13    addb $13
F289: 99 11    adca $11
F28B: D7 13    stb  $13
F28D: 97 11    sta  $11
F28F: 26 96    bne  $F227
F291: C1 07    cmpb #$07
F293: 26 92    bne  $F227
F295: 39       rts  
F296: 86 F6    lda  #$F6
F298: 97 0D    sta  $0D
F29A: CE 00 64 ldx  #$0064
F29D: DF 09    stx  $09
F29F: DB 0A    addb $0A
F2A1: 96 0F    lda  $0F
F2A3: 99 09    adca $09
F2A5: 97 0F    sta  $0F
F2A7: DE 09    ldx  $09
F2A9: 25 04    bcs  $F2AF
F2AB: 20 00    bra  $F2AD
F2AD: 20 03    bra  $F2B2
F2AF: 08       inx  
F2B0: 27 11    beq  $F2C3
F2B2: DF 09    stx  $09
F2B4: 84 0F    anda #$0F
F2B6: 8B 62    adda #$62
F2B8: 97 0E    sta  $0E
F2BA: DE 0D    ldx  $0D
F2BC: A6 00    lda  (x+$00)
F2BE: B7 04 00 sta  $0400
F2C1: 20 DC    bra  $F29F
F2C3: 39       rts  
F2C4: 4F       clra 
F2C5: B7 04 00 sta  $0400
F2C8: 97 0F    sta  $0F
F2CA: 4F       clra 
F2CB: 91 0F    cmpa $0F
F2CD: 26 03    bne  $F2D2
F2CF: 73 04 00 com  $0400
F2D2: C6 12    ldb  #$12
F2D4: 5A       decb 
F2D5: 26 FD    bne  $F2D4
F2D7: 4C       inca 
F2D8: 2A F1    bpl  $F2CB
F2DA: 73 04 00 com  $0400
F2DD: 7C 00 0F inc  $000F
F2E0: 2A E8    bpl  $F2CA
F2E2: 39       rts  
F2E3: CE 00 11 ldx  #$0011
F2E6: 6F 00    clr  (x+$00)
F2E8: 08       inx  
F2E9: 8C 00 19 cmpx #$0019
F2EC: 26 F8    bne  $F2E6
F2EE: 86 40    lda  #$40
F2F0: 97 11    sta  $11
F2F2: CE 00 11 ldx  #$0011
F2F5: 86 80    lda  #$80
F2F7: 97 0F    sta  $0F
F2F9: 5F       clrb 
F2FA: A6 01    lda  (x+$01)
F2FC: AB 00    adda (x+$00)
F2FE: A7 01    sta  (x+$01)
F300: 2A 02    bpl  $F304
F302: DB 0F    addb $0F
F304: 74 00 0F lsr  $000F
F307: 08       inx  
F308: 08       inx  
F309: 8C 00 19 cmpx #$0019
F30C: 26 EC    bne  $F2FA
F30E: F7 04 00 stb  $0400
F311: 7C 00 10 inc  $0010
F314: 26 DC    bne  $F2F2
F316: CE 00 11 ldx  #$0011
F319: 5F       clrb 
F31A: A6 00    lda  (x+$00)
F31C: 27 0B    beq  $F329
F31E: 81 37    cmpa #$37
F320: 26 04    bne  $F326
F322: C6 41    ldb  #$41
F324: E7 02    stb  (x+$02)
F326: 6A 00    dec  (x+$00)
F328: 5C       incb 
F329: 08       inx  
F32A: 08       inx  
F32B: 8C 00 19 cmpx #$0019
F32E: 26 EA    bne  $F31A
F330: 5D       tstb 
F331: 26 BF    bne  $F2F2
F333: 39       rts  
F334: 86 80    lda  #$80
F336: 97 1B    sta  $1B
F338: 86 F3    lda  #$F3
F33A: 97 19    sta  $19
F33C: 86 80    lda  #$80
F33E: 97 0F    sta  $0F
F340: 86 12    lda  #$12
F342: 4A       deca 
F343: 26 FD    bne  $F342
F345: 96 18    lda  $18
F347: 9B 1B    adda $1B
F349: 97 18    sta  $18
F34B: 44       lsra 
F34C: 44       lsra 
F34D: 44       lsra 
F34E: 8B 68    adda #$68
F350: 97 1A    sta  $1A
F352: DE 19    ldx  $19
F354: A6 00    lda  (x+$00)
F356: B7 04 00 sta  $0400
F359: 7A 00 0F dec  $000F
F35C: 26 E2    bne  $F340
F35E: 7A 00 1B dec  $001B
F361: 96 1B    lda  $1B
F363: 81 20    cmpa #$20
F365: 26 D5    bne  $F33C
F367: 39       rts  
F368: 80 8C    suba #$8C
F36A: 98 A5    eora $A5
F36C: B0 BC C6 suba $BCC6
F36F: D0 DA    subb $DA
F371: E2 EA    sbcb (x+$EA)
F373: F0 F5 FA subb $F5FA
F376: FD       illegal
F377: FE FF FE ldx  $FFFE
F37A: FD       illegal
F37B: FA F5 F0 orb  $F5F0
F37E: EA E2    orb  (x+$E2)
F380: DA D0    orb  $D0
F382: C6 BC    ldb  #$BC
F384: B0 A5 98 suba $A598
F387: 8C 80 73 cmpx #$8073
F38A: 67 5A    asr  (x+$5A)
F38C: 4F       clra 
F38D: 43       coma 
F38E: 39       rts  
F38F: 2F 25    ble  $F3B6
F391: 1D       illegal
F392: 15       illegal
F393: 0F       sei  
F394: 0A       clv  
F395: 05       illegal
F396: 02       illegal
F397: 01       nop  
F398: 00       illegal
F399: 01       nop  
F39A: 02       illegal
F39B: 05       illegal
F39C: 0A       clv  
F39D: 0F       sei  
F39E: 15       illegal
F39F: 1D       illegal
F3A0: 25 2F    bcs  $F3D1
F3A2: 39       rts  
F3A3: 43       coma 
F3A4: 4F       clra 
F3A5: 5A       decb 
F3A6: 67 73    asr  (x+$73)
;;TRANS
F3A8: 36       psha 
F3A9: A6 00    lda  (x+$00)
F3AB: DF 0B    stx  $0B
F3AD: DE 0D    ldx  $0D
F3AF: A7 00    sta  (x+$00)
F3B1: 08       inx  
F3B2: DF 0D    stx  $0D
F3B4: DE 0B    ldx  $0B
F3B6: 08       inx  
F3B7: 5A       decb 
F3B8: 26 EF    bne  $F3A9
F3BA: 32       pula 
F3BB: 39       rts  
F3BC: 4F       clra 
F3BD: 97 00    sta  $00
F3BF: 97 01    sta  $01
F3C1: 97 04    sta  $04
F3C3: 39       rts  
F3C4: 7F 00 00 clr  $0000
F3C7: 96 01    lda  $01
F3C9: 84 7F    anda #$7F
F3CB: 81 7F    cmpa #$7F
F3CD: 26 01    bne  $F3D0
F3CF: 4F       clra 
F3D0: 4C       inca 
F3D1: 97 01    sta  $01
F3D3: 39       rts  
F3D4: 86 0E    lda  #$0E
F3D6: BD F4 1F jsr  $F41F
F3D9: 96 01    lda  $01
F3DB: 43       coma 
F3DC: BD F4 D7 jsr  $F4D7
F3DF: 7C 00 15 inc  $0015
F3E2: BD F4 D9 jsr  $F4D9
F3E5: 20 F8    bra  $F3DF
F3E7: 86 03    lda  #$03
F3E9: BD F0 CA jsr  $F0CA
F3EC: D6 02    ldb  $02
F3EE: C1 1F    cmpb #$1F
F3F0: 26 01    bne  $F3F3
F3F2: 5F       clrb 
F3F3: 5C       incb 
F3F4: D7 02    stb  $02
F3F6: 86 20    lda  #$20
F3F8: 10       sba  
F3F9: 5F       clrb 
F3FA: 81 14    cmpa #$14
F3FC: 23 05    bls  $F403
F3FE: CB 0E    addb #$0E
F400: 4A       deca 
F401: 20 F7    bra  $F3FA
F403: CB 05    addb #$05
F405: 4A       deca 
F406: 26 FB    bne  $F403
F408: D7 11    stb  $11
F40A: BD F0 DF jsr  $F0DF
F40D: 20 FB    bra  $F40A
F40F: 96 03    lda  $03
F411: 26 09    bne  $F41C
F413: 7C 00 03 inc  $0003
F416: 86 0D    lda  #$0D
F418: 8D 05    bsr  $F41F
F41A: 20 69    bra  $F485
F41C: 7E F4 CC jmp  $F4CC
*
*
*GWAVE LOADER
F41F: 16       tab  
F420: 58       aslb 
F421: 1B       aba  
F422: 1B       aba  
F423: 1B       aba  
F424: CE F7 BF ldx  #$F7BF
F427: BD F5 C3 jsr  $F5C3
F42A: A6 00    lda  (x+$00)
F42C: 16       tab  
F42D: 84 0F    anda #$0F
F42F: 97 12    sta  $12
F431: 54       lsrb 
F432: 54       lsrb 
F433: 54       lsrb 
F434: 54       lsrb 
F435: D7 11    stb  $11
F437: A6 01    lda  (x+$01)
F439: 16       tab  
F43A: 54       lsrb 
F43B: 54       lsrb 
F43C: 54       lsrb 
F43D: 54       lsrb 
F43E: D7 13    stb  $13
F440: 84 0F    anda #$0F
F442: 97 0F    sta  $0F
F444: DF 09    stx  $09
F446: CE F6 72 ldx  #$F672
F449: 7A 00 0F dec  $000F
F44C: 2B 08    bmi  $F456
F44E: A6 00    lda  (x+$00)
F450: 4C       inca 
F451: BD F5 C3 jsr  $F5C3
F454: 20 F3    bra  $F449
F456: DF 16    stx  $16
F458: BD F5 13 jsr  $F513
F45B: DE 09    ldx  $09
F45D: A6 02    lda  (x+$02)
F45F: 97 18    sta  $18
F461: BD F5 25 jsr  $F525
F464: DE 09    ldx  $09
F466: A6 03    lda  (x+$03)
F468: 97 14    sta  $14
F46A: A6 04    lda  (x+$04)
F46C: 97 15    sta  $15
F46E: A6 05    lda  (x+$05)
F470: 16       tab  
F471: A6 06    lda  (x+$06)
F473: CE F8 7C ldx  #$F87C
F476: BD F5 C3 jsr  $F5C3
F479: 17       tba  
F47A: DF 19    stx  $19
F47C: 7F 00 21 clr  $0021
F47F: BD F5 C3 jsr  $F5C3
F482: DF 1B    stx  $1B
F484: 39       rts  
F485: 96 11    lda  $11
F487: 97 20    sta  $20
F489: DE 19    ldx  $19
F48B: DF 0B    stx  $0B
F48D: DE 0B    ldx  $0B
F48F: A6 00    lda  (x+$00)
F491: 9B 21    adda $21
F493: 97 1F    sta  $1F
F495: 9C 1B    cmpx $1B
F497: 27 26    beq  $F4BF
F499: D6 12    ldb  $12
F49B: 08       inx  
F49C: DF 0B    stx  $0B
F49E: CE 00 22 ldx  #$0022
F4A1: 96 1F    lda  $1F
F4A3: 4A       deca 
F4A4: 26 FD    bne  $F4A3
F4A6: A6 00    lda  (x+$00)
F4A8: B7 04 00 sta  $0400
F4AB: 08       inx  
F4AC: 9C 1D    cmpx $1D
F4AE: 26 F1    bne  $F4A1
F4B0: 5A       decb 
F4B1: 27 DA    beq  $F48D
F4B3: 08       inx  
F4B4: 09       dex  
F4B5: 08       inx  
F4B6: 09       dex  
F4B7: 08       inx  
F4B8: 09       dex  
F4B9: 08       inx  
F4BA: 09       dex  
F4BB: 01       nop  
F4BC: 01       nop  
F4BD: 20 DF    bra  $F49E
F4BF: 96 13    lda  $13
F4C1: 8D 62    bsr  $F525
F4C3: 7A 00 20 dec  $0020
F4C6: 26 C1    bne  $F489
F4C8: 96 03    lda  $03
F4CA: 26 46    bne  $F512
F4CC: 96 14    lda  $14
F4CE: 27 42    beq  $F512
F4D0: 7A 00 15 dec  $0015
F4D3: 27 3D    beq  $F512
F4D5: 9B 21    adda $21
F4D7: 97 21    sta  $21
F4D9: DE 19    ldx  $19
F4DB: 5F       clrb 
F4DC: 96 21    lda  $21
F4DE: 7D 00 14 tst  $0014
F4E1: 2B 06    bmi  $F4E9
F4E3: AB 00    adda (x+$00)
F4E5: 25 08    bcs  $F4EF
F4E7: 20 0B    bra  $F4F4
F4E9: AB 00    adda (x+$00)
F4EB: 27 02    beq  $F4EF
F4ED: 25 05    bcs  $F4F4
F4EF: 5D       tstb 
F4F0: 27 08    beq  $F4FA
F4F2: 20 0F    bra  $F503
F4F4: 5D       tstb 
F4F5: 26 03    bne  $F4FA
F4F7: DF 19    stx  $19
F4F9: 5C       incb 
F4FA: 08       inx  
F4FB: 9C 1B    cmpx $1B
F4FD: 26 DD    bne  $F4DC
F4FF: 5D       tstb 
F500: 26 01    bne  $F503
F502: 39       rts  
F503: DF 1B    stx  $1B
F505: 96 13    lda  $13
F507: 27 06    beq  $F50F
F509: 8D 08    bsr  $F513
F50B: 96 18    lda  $18
F50D: 8D 16    bsr  $F525
F50F: 7E F4 85 jmp  $F485
F512: 39       rts  
F513: CE 00 22 ldx  #$0022
F516: DF 0D    stx  $0D
F518: DE 16    ldx  $16
F51A: E6 00    ldb  (x+$00)
F51C: 08       inx  
F51D: BD F3 A8 jsr  $F3A8
F520: DE 0D    ldx  $0D
F522: DF 1D    stx  $1D
F524: 39       rts  
F525: 4D       tsta 
F526: 27 2B    beq  $F553
F528: DE 16    ldx  $16
F52A: DF 0B    stx  $0B
F52C: CE 00 22 ldx  #$0022
F52F: 97 10    sta  $10
F531: DF 0D    stx  $0D
F533: DE 0B    ldx  $0B
F535: D6 10    ldb  $10
F537: D7 0F    stb  $0F
F539: E6 01    ldb  (x+$01)
F53B: 54       lsrb 
F53C: 54       lsrb 
F53D: 54       lsrb 
F53E: 54       lsrb 
F53F: 08       inx  
F540: DF 0B    stx  $0B
F542: DE 0D    ldx  $0D
F544: A6 00    lda  (x+$00)
F546: 10       sba  
F547: 7A 00 0F dec  $000F
F54A: 26 FA    bne  $F546
F54C: A7 00    sta  (x+$00)
F54E: 08       inx  
F54F: 9C 1D    cmpx $1D
F551: 26 DE    bne  $F531
F553: 39       rts  
*
* INTERRUPT PROCESSING
*
F554: 8E 00 7F lds  #$007F
F557: B6 04 02 lda  $0402
F55A: 0E       cli  
F55B: 43       coma 
F55C: 84 3F    anda #$3F
F55E: 5F       clrb 
F55F: 81 0E    cmpa #$0E
F561: 27 02    beq  $F565
F563: D7 02    stb  $02
F565: 81 12    cmpa #$12
F567: 27 02    beq  $F56B
F569: D7 03    stb  $03
F56B: 4D       tsta 
F56C: 27 42    beq  $F5B0
F56E: 4A       deca 
F56F: 81 1F    cmpa #$1F
F571: 2D 17    blt  $F58A
F573: 81 2A    cmpa #$2A
F575: 22 04    bhi  $F57B
F577: 80 10    suba #$10
F579: 20 13    bra  $F58E
F57B: 81 31    cmpa #$31
F57D: 22 07    bhi  $F586
F57F: 80 2B    suba #$2B
F581: BD FD 8F jsr  $FD8F			;;WALSH SYNTH
F584: 20 2A    bra  $F5B0
F586: 80 23    suba #$23
F588: 20 12    bra  $F59C
F58A: 81 0C    cmpa #$0C
F58C: 22 08    bhi  $F596
F58E: BD F4 1F jsr  $F41F
F591: BD F4 85 jsr  $F485
F594: 20 1A    bra  $F5B0
F596: 81 1B    cmpa #$1B
F598: 22 0E    bhi  $F5A8
F59A: 80 0D    suba #$0D
F59C: 48       asla 
F59D: CE F5 EB ldx  #$F5EB
F5A0: 8D 21    bsr  $F5C3
F5A2: EE 00    ldx  (x+$00)
F5A4: AD 00    jsr  (x+$00)
F5A6: 20 08    bra  $F5B0
F5A8: 80 1C    suba #$1C
F5AA: BD F0 CA jsr  $F0CA
F5AD: BD F0 DF jsr  $F0DF
F5B0: 96 00    lda  $00
F5B2: 9A 01    ora  $01
F5B4: 27 FE    beq  $F5B4
F5B6: 4F       clra 
F5B7: 97 03    sta  $03
F5B9: 96 00    lda  $00
F5BB: 27 03    beq  $F5C0
F5BD: 7E F1 CC jmp  $F1CC
F5C0: 7E F3 D4 jmp  $F3D4
;;ADDX
F5C3: DF 0B    stx  $0B
F5C5: 9B 0C    adda $0C
F5C7: 97 0C    sta  $0C
F5C9: 24 03    bcc  $F5CE
F5CB: 7C 00 0B inc  $000B
F5CE: DE 0B    ldx  $0B
F5D0: 39       rts  
;;NMI
F5D1: 0F       sei  
F5D2: 8E 00 7F lds  #$007F
F5D5: CE FF FF ldx  #$FFFF
F5D8: 5F       clrb 
;;NMI1
F5D9: EB 00    addb (x+$00)
F5DB: 09       dex  
F5DC: 8C F0 00 cmpx #$F000
F5DF: 26 F8    bne  $F5D9
F5E1: E1 00    cmpb (x+$00)
F5E3: 27 01    beq  $F5E6
F5E5: 3E       wai  
;;NMI2
F5E6: BD F2 0D jsr  $F20D
F5E9: 20 E6    bra  $F5D1
F5EB: F3       illegal
F5EC: E7 F1    stb  (x+$F1)
F5EE: CC       illegal
F5EF: F3       illegal
F5F0: C4 F1    andb #$F1
F5F2: 36       psha 
F5F3: F4 0F F3 andb $0FF3
F5F6: BC F1 79 cmpx $F179
F5F9: F1 3E F1 cmpb $3EF1
F5FC: D5 F1    bitb $F1
F5FE: DA F2    orb  $F2
F600: 96 F2    lda  $F2
F602: C4 F2    andb #$F2
F604: E3       illegal
F605: F1 75 FE cmpb $75FE
F608: 8A FF    ora  #$FF
F60A: 3B       rti  
F60B: FF 6A FF stx  $6AFF
F60E: 54       lsrb 
F60F: FF 80 FE stx  $80FE
F612: FF F1 88 stx  $F188
F615: F0 46 F1 subb $46F1
F618: 2C F0    bge  $F60A
F61A: 65       illegal
F61B: F1 DF F3 cmpb $DFF3
F61E: 34       des  
F61F: F2 0D F1 sbcb $0DF1
F622: E4 
;;VVECT
F623: 40    andb (x+$40)
F624: 01       nop  
F625: 00       illegal
F626: 10       sba  
F627: E1 00    cmpb (x+$00)
F629: 80 FF    suba #$FF
F62B: FF 28 01 stx  $2801
F62E: 00       illegal
F62F: 08       inx  
F630: 81 02    cmpa #$02
F632: 00       illegal
F633: FF FF 28 stx  $FF28
F636: 81 00    cmpa #$00
F638: FC       illegal
F639: 01       nop  
F63A: 02       illegal
F63B: 00       illegal
F63C: FC       illegal
F63D: FF FF 01 stx  $FF01
F640: 00       illegal
F641: 18       illegal
F642: 41       illegal
F643: 04       illegal
F644: 80 00    suba #$00
F646: FF 00 FF stx  $00FF
F649: 08       inx  
F64A: FF 68 04 stx  $6804
F64D: 80 00    suba #$00
F64F: FF 28 81 stx  $2881
F652: 00       illegal
F653: FC       illegal
F654: 01       nop  
F655: 02       illegal
F656: 00       illegal
F657: FC       illegal
F658: FF 60 01 stx  $6001
F65B: 57       asrb 
F65C: 08       inx  
F65D: E1 02    cmpb (x+$02)
F65F: 00       illegal
F660: FE 80 8C ldx  $808C
F663: 5B       illegal
F664: B6 40 BF lda  $40BF
F667: 49       rola 
F668: A4 73    anda (x+$73)
F66A: 73 A4 49 com  $A449
F66D: BF 40 B6 sts  $40B6
F670: 5B       illegal
F671: 8C
;;
;;GWVTAB 
;;
;;(WAVEFORM COUNT = $0D)
;;
;; GS2
F672: 08
F673: 7F D9 FF D9 7F 24 00 24 
;;
;; GSSQR2
F67B: 08
F67C: 00 40 80 00 FF 00 80 40  
;;
;; GS1
F684: 10       
F685: 7F B0 D9 F5
F689: FF F5 D9 B0
F68D: 7F 4E 24 09 
F691: 00 09 24 4E
;;
;; GS12
F695: 10 
F696: 7F C5 EC E7 
F69A: BF 8D 6D 6A 
F69E: 7F 94 92 71
F6A2: 40 17 12 39
;;
;; GSQ22
F6A6: 10 
F6A7: FF FF FF FF 
F6AB: 00 00 00 00
F6AF: FF FF FF FF
F6B3: 00 00 00 00
;;
;; GS72
F6B7: 48
F6B8: 8A 95    ora  #$95
F6BA: A0 AB    suba (x+$AB)
F6BC: B5 BF C8 bita $BFC8
F6BF: D1 DA    cmpb $DA
F6C1: E1 E8    cmpb (x+$E8)
F6C3: EE F3    ldx  (x+$F3)
F6C5: F7 FB FD stb  $FBFD
F6C8: FE FF FE ldx  $FFFE
F6CB: FD       illegal
F6CC: FB F7 F3 addb $F7F3
F6CF: EE E8    ldx  (x+$E8)
F6D1: E1 DA    cmpb (x+$DA)
F6D3: D1 C8    cmpb $C8
F6D5: BF B5 AB sts  $B5AB
F6D8: A0 95    suba (x+$95)
F6DA: 8A 7F    ora  #$7F
F6DC: 75       illegal
F6DD: 6A 5F    dec  (x+$5F)
F6DF: 54       lsrb 
F6E0: 4A       deca 
F6E1: 40       nega 
F6E2: 37       pshb 
F6E3: 2E 25    bgt  $F70A
F6E5: 1E       illegal
F6E6: 17       tba  
F6E7: 11       cba  
F6E8: 0C       clc  
F6E9: 08       inx  
F6EA: 04       illegal
F6EB: 02       illegal
F6EC: 01       nop  
F6ED: 00       illegal
F6EE: 01       nop  
F6EF: 02       illegal
F6F0: 04       illegal
F6F1: 08       inx  
F6F2: 0C       clc  
F6F3: 11       cba  
F6F4: 17       tba  
F6F5: 1E       illegal
F6F6: 25 2E    bcs  $F726
F6F8: 37       pshb 
F6F9: 40       nega 
F6FA: 4A       deca 
F6FB: 54       lsrb 
F6FC: 5F       clrb 
F6FD: 6A 75 7F 
;;
;; GS1.7
F700: 10
F701: 59 7B 98 AC
F705: B3 AC 98 7B
F709: 59 37 19 06
F70D: 00 06 19 37
;;
;; GSQ2
F711: 08  
F712: FF FF FF FF 00 00 00 00
;;
;; GS1234
F71A: 10
F71B: 76 FF B8 D0 
F71F: 9D E6 6A 82 
F723: 76 EA 81 86 
F727: 4E 9C 32 63
;;
;; MW1
F72B: 10
F72C: 00 F4 00 E8
F730: 00 DC 00 E2
F734: 00 DC 00 E8
F738: 00 F4 00 00
;;
;; HBPAT2
F73C: 48       asla 
F73D: 45       illegal
F73E: 4B       illegal
F73F: 50       negb 
F740: 56       rorb 
F741: 5B       illegal
F742: 60 64    neg  (x+$64)
F744: 69 6D    rol  (x+$6D)
F746: 71       illegal
F747: 74 77 7A lsr  $777A
F74A: 7C 7E 7F inc  $7E7F
F74D: 7F 80 7F clr  $807F
F750: 7F 7E 7C clr  $7E7C
F753: 7A 77 74 dec  $7774
F756: 71       illegal
F757: 6D 69    tst  (x+$69)
F759: 64 60    lsr  (x+$60)
F75B: 5B       illegal
F75C: 56       rorb 
F75D: 50       negb 
F75E: 4B       illegal
F75F: 45       illegal
F760: 40       nega 
F761: 3B       rti  
F762: 35       txs  
F763: 30       tsx  
F764: 2A 25    bpl  $F78B
F766: 20 1C    bra  $F784
F768: 17       tba  
F769: 13       illegal
F76A: 0F       sei  
F76B: 0C       clc  
F76C: 09       dex  
F76D: 06       tap  
F76E: 04       illegal
F76F: 02       illegal
F770: 01       nop  
F771: 01       nop  
F772: 00       illegal
F773: 01       nop  
F774: 01       nop  
F775: 02       illegal
F776: 04       illegal
F777: 06       tap  
F778: 09       dex  
F779: 0C       clc  
F77A: 0F       sei  
F77B: 13       illegal
F77C: 17       tba  
F77D: 1C       illegal
F77E: 20 25    bra  $F7A5
F780: 2A 30    bpl  $F7B2
F782: 35       txs  
F783: 3B       rti  
F784: 40       nega 
;;
;; Sinistar Waveform #6
F785: 0C   
F786: 00 50 
F788: 60 B0
F78A: 20 20
F78C: F0 90 80
F78F: C0 50 70
;;
;; Sinistar Waveform #7
F792: 07
F793: 40 09 35 0C 29 0F 20
;;
;; NEW WAVEFORM (sounds like Inferno stuff, very buzzy bass)
F79A: 24
F79B: 7F B0 D6 E8 E3 C9 A3 7B 5E
F7A4: 54 5E 7B A3 C9 E3 E8 D6 B0
F7AB: 7F 4C 26 14 19 33 5A 81 9E
F7B6: A8 9E 81 5A 33 19 14 26 4C
;;
;;SVTAB
F7BF: 81 24 00 00 00 16 31
F7C6: 12 05 1A FF 00 27 6D
F7CD: 11 05 11 01 0F 01 47
F7D4: 11 31 00 01 00 0D 1B
F7DB: F1 18 00 01 01 08 FB
F7E2: 41 45 00 00 00 0F 5B
F7E9: 21 35 11 FF 00 0D 1B
F7F0: 46 59 00 00 00 08 85
F7F7: 31 11 00 01 00 03 6A
F7FE: 63 25 00 03 0A 03 6A
F805: 43 0D 00 04 02 0D 1B			;;(uses new waveform)
F80C: 08 4C 0B 40 01 02 F3
F813: 1F 12 00 FF 10 04 69
F81A: F1 11 00 FF 00 0D 00
F821: 12 06 00 FF 01 09 28
F828: 14 17 00 00 00 0E 0D
F82F: F4 11 00 00 00 0E 0D
F836: 21 30 00 01 00 0D 1B
F83D: 11 0B 06 02 20 03 EF
F844: F4 18 00 00 00 12 B3
F84B: 52 32 12 00 00 10 DF
F852: 1F 14 02 00 00 05 F6
F859: 21 30 00 FF 00 1B 0D
F860: F1 19 00 00 00 0E A4 
F867: 31 19 00 01 00 03 6A 
F86E: 41 02 D0 00 00 27 6D
F875: 03 15 11 FF 00 0D 1B
;;
;;GFRTAB
F87C: A0 98    suba (x+$98)
F87E: 90 88    suba $88
F880: 80 78    suba #$78
F882: 70 68 60 neg  $6860
F885: 58       aslb 
F886: 50       negb 
F887: 44       lsra 
F888: 40       nega 
F889: 01       nop  
F88A: 01       nop  
F88B: 02       illegal
F88C: 02       illegal
F88D: 04       illegal
F88E: 04       illegal
F88F: 08       inx  
F890: 08       inx  
F891: 10       sba  
F892: 10       sba  
F893: 30       tsx  
F894: 60 C0    neg  (x+$C0)
F896: E0 01    subb (x+$01)
F898: 01       nop  
F899: 02       illegal
F89A: 02       illegal
F89B: 03       illegal
F89C: 04       illegal
F89D: 05       illegal
F89E: 06       tap  
F89F: 07       tpa  
F8A0: 08       inx  
F8A1: 09       dex  
F8A2: 0A       clv  
F8A3: 0C       clc  
F8A4: 80 7C    suba #$7C
F8A6: 78 74 70 asl  $7470
F8A9: 74 78 7C lsr  $787C
F8AC: 80 01    suba #$01
F8AE: 01       nop  
F8AF: 02       illegal
F8B0: 02       illegal
F8B1: 04       illegal
F8B2: 04       illegal
F8B3: 08       inx  
F8B4: 08       inx  
F8B5: 10       sba  
F8B6: 20 28    bra  $F8E0
F8B8: 30       tsx  
F8B9: 38       illegal
F8BA: 40       nega 
F8BB: 48       asla 
F8BC: 50       negb 
F8BD: 60 70    neg  (x+$70)
F8BF: 80 A0    suba #$A0
F8C1: B0 C0 08 suba $C008
F8C4: 40       nega 
F8C5: 08       inx  
F8C6: 40       nega 
F8C7: 08       inx  
F8C8: 40       nega 
F8C9: 08       inx  
F8CA: 40       nega 
F8CB: 08       inx  
F8CC: 40       nega 
F8CD: 08       inx  
F8CE: 40       nega 
F8CF: 08       inx  
F8D0: 40       nega 
F8D1: 08       inx  
F8D2: 40       nega 
F8D3: 08       inx  
F8D4: 40       nega 
F8D5: 08       inx  
F8D6: 40       nega 
F8D7: 01       nop  
F8D8: 02       illegal
F8D9: 04       illegal
F8DA: 08       inx  
F8DB: 09       dex  
F8DC: 0A       clv  
F8DD: 0B       sev  
F8DE: 0C       clc  
F8DF: 0E       cli  
F8E0: 0F       sei  
F8E1: 10       sba  
F8E2: 12       illegal
F8E3: 14       illegal
F8E4: 16       tab  
F8E5: 40       nega 
F8E6: 10       sba  
F8E7: 08       inx  
F8E8: 01       nop  
F8E9: 01       nop  
F8EA: 01       nop  
F8EB: 01       nop  
F8EC: 01       nop  
F8ED: 02       illegal
F8EE: 02       illegal
F8EF: 03       illegal
F8F0: 03       illegal
F8F1: 04       illegal
F8F2: 04       illegal
F8F3: 05       illegal
F8F4: 06       tap  
F8F5: 08       inx  
F8F6: 0A       clv  
F8F7: 0C       clc  
F8F8: 10       sba  
F8F9: 14       illegal
F8FA: 18       illegal
F8FB: 20 30    bra  $F92D
F8FD: 40       nega 
F8FE: 50       negb 
F8FF: 40       nega 
F900: 30       tsx  
F901: 20 10    bra  $F913
F903: 0C       clc  
F904: 0A       clv  
F905: 08       inx  
F906: 07       tpa  
F907: 06       tap  
F908: 05       illegal
F909: 04       illegal
F90A: 03       illegal
F90B: 02       illegal
F90C: 02       illegal
F90D: 01       nop  
F90E: 01       nop  
F90F: 01       nop  
F910: 07       tpa  
F911: 08       inx  
F912: 09       dex  
F913: 0A       clv  
F914: 0C       clc  
F915: 08       inx  
F916: 17       tba  
F917: 18       illegal
F918: 19       daa  
F919: 1A       illegal
F91A: 1B       aba  
F91B: 1C       illegal
F91C: 00       illegal
F91D: 00       illegal
F91E: 00       illegal
F91F: 00       illegal
F920: 08       inx  
F921: 80 10    suba #$10
F923: 78 18 70 asl  $1870
F926: 20 60    bra  $F988
F928: 28 58    bvc  $F982
F92A: 30       tsx  
F92B: 50       negb 
F92C: 40       nega 
F92D: 48       asla 
F92E: 00       illegal
F92F: 01       nop  
F930: 08       inx  
F931: 10       sba  
F932: 01       nop  
F933: 08       inx  
F934: 10       sba  
F935: 01       nop  
F936: 08       inx  
F937: 10       sba  
F938: 01       nop  
F939: 08       inx  
F93A: 10       sba  
F93B: 01       nop  
F93C: 08       inx  
F93D: 10       sba  
F93E: 01       nop  
F93F: 08       inx  
F940: 10       sba  
F941: 00       illegal
F942: 10       sba  
F943: 20 40    bra  $F985
F945: 10       sba  
F946: 20 40    bra  $F988
F948: 10       sba  
F949: 20 40    bra  $F98B
F94B: 10       sba  
F94C: 20 40    bra  $F98E
F94E: 10       sba  
F94F: 20 40    bra  $F991
F951: 10       sba  
F952: 20 40    bra  $F994
F954: 10       sba  
F955: 20 40    bra  $F997
F957: 10       sba  
F958: 20 40    bra  $F99A
F95A: 00       illegal
F95B: 01       nop  
F95C: 02       illegal
F95D: 02       illegal
F95E: 03       illegal
F95F: 03       illegal
F960: 03       illegal
F961: 06       tap  
F962: 06       tap  
F963: 06       tap  
F964: 06       tap  
F965: 0F       sei  
F966: 1F       illegal
F967: 36       psha 
F968: 55       illegal
F969: 74 91 01 lsr  $9101
F96C: 02       illegal
F96D: 03       illegal
F96E: 04       illegal
F96F: 26 03    bne  $F974
F971: 04       illegal
F972: 05       illegal
F973: 05       illegal
F974: 05       illegal
F975: 05       illegal
F976: 05       illegal
F977: FF 01 00 stx  $0100
F97A: 18       illegal
F97B: 41       illegal
F97C: 04       illegal
F97D: 80 00    suba #$00
;;
;; WALSH FUNCTION SOUND MACHINE
;;
F97F: C0 0D    subb #$0D
F981: 37       pshb 
F982: BD 00 2C jsr  $002C
F985: 33       pulb 
F986: C1 14    cmpb #$14
F988: 22 F5    bhi  $F97F
F98A: 01       nop  
F98B: 96 24    lda  $24
F98D: 9B 21    adda $21
F98F: 97 24    sta  $24
F991: C9 F6    adcb #$F6
F993: 5A       decb 
F994: 2A FD    bpl  $F993
F996: 96 28    lda  $28
F998: 4C       inca 
F999: 84 0F    anda #$0F
F99B: 8A 10    ora  #$10
F99D: 97 28    sta  $28
F99F: DE 27    ldx  $27
F9A1: E6 00    ldb  (x+$00)
F9A3: F7 04 00 stb  $0400
F9A6: 84 0F    anda #$0F
F9A8: 39       rts  
F9A9: 4F       clra 
F9AA: CE 00 10 ldx  #$0010
F9AD: C6 61    ldb  #$61
F9AF: A7 00    sta  (x+$00)
F9B1: 08       inx  
F9B2: 5A       decb 
F9B3: 26 FA    bne  $F9AF
F9B5: C6 5F    ldb  #$5F
F9B7: D7 26    stb  $26
F9B9: C6 37    ldb  #$37
F9BB: D7 30    stb  $30
F9BD: C6 7E    ldb  #$7E
F9BF: D7 2C    stb  $2C
F9C1: CE FB 94 ldx  #$FB94
F9C4: DF 2D    stx  $2D
F9C6: D6 0C    ldb  $0C
F9C8: D7 23    stb  $23
F9CA: C0 03    subb #$03
F9CC: BD F9 86 jsr  $F986
F9CF: 08       inx  
F9D0: D6 23    ldb  $23
F9D2: C0 02    subb #$02
F9D4: BD F9 7F jsr  $F97F
F9D7: 26 F7    bne  $F9D0
F9D9: D6 20    ldb  $20
F9DB: 96 21    lda  $21
F9DD: 9B 0D    adda $0D
F9DF: D9 0C    adcb $0C
F9E1: 97 0D    sta  $0D
F9E3: D7 0C    stb  $0C
F9E5: DB 22    addb $22
F9E7: 86 19    lda  #$19
F9E9: 11       cba  
F9EA: 24 01    bcc  $F9ED
F9EC: 81 16    cmpa #$16
F9EE: D7 23    stb  $23
F9F0: 01       nop  
F9F1: C0 09    subb #$09
F9F3: BD F9 86 jsr  $F986
F9F6: 96 2F    lda  $2F
F9F8: 16       tab  
F9F9: 48       asla 
F9FA: C9 00    adcb #$00
F9FC: D7 2F    stb  $2F
F9FE: D6 23    ldb  $23
FA00: C0 05    subb #$05
FA02: 96 25    lda  $25
FA04: 2A 06    bpl  $FA0C
FA06: 7C 00 25 inc  $0025
FA09: 01       nop  
FA0A: 20 BE    bra  $F9CA
FA0C: 5A       decb 
FA0D: BD F9 86 jsr  $F986
FA10: DE 0A    ldx  $0A
FA12: A6 00    lda  (x+$00)
FA14: 2A 12    bpl  $FA28
FA16: 81 80    cmpa #$80
FA18: 27 5F    beq  $FA79
FA1A: 4C       inca 
FA1B: 97 25    sta  $25
FA1D: 08       inx  
FA1E: FF 00 0A stx  $000A
FA21: D6 23    ldb  $23
FA23: C0 06    subb #$06
FA25: 7E F9 CA jmp  $F9CA
FA28: 08       inx  
FA29: E6 00    ldb  (x+$00)
FA2B: 37       pshb 
FA2C: 08       inx  
FA2D: DF 0A    stx  $0A
FA2F: 97 29    sta  $29
FA31: 84 70    anda #$70
FA33: 44       lsra 
FA34: 44       lsra 
FA35: 44       lsra 
FA36: 5F       clrb 
FA37: 8B 0D    adda #$0D
FA39: C9 FB    adcb #$FB
FA3B: 97 2B    sta  $2B
FA3D: D7 2A    stb  $2A
FA3F: D6 23    ldb  $23
FA41: D6 23    ldb  $23
FA43: C0 0D    subb #$0D
FA45: BD F9 86 jsr  $F986
FA48: 5F       clrb 
FA49: DE 2A    ldx  $2A
FA4B: EE 00    ldx  (x+$00)
FA4D: 6E 00    jmp  (x+$00)
FA4F: 96 29    lda  $29
FA51: 47       asra 
FA52: C2 00    sbcb #$00
FA54: D4 0C    andb $0C
FA56: 32       pula 
FA57: 10       sba  
FA58: 9B 0C    adda $0C
FA5A: 97 0C    sta  $0C
FA5C: 08       inx  
FA5D: D6 23    ldb  $23
FA5F: C0 0A    subb #$0A
FA61: 7E F9 CC jmp  $F9CC
FA64: 96 29    lda  $29
FA66: 47       asra 
FA67: C2 00    sbcb #$00
FA69: D4 22    andb $22
FA6B: 32       pula 
FA6C: 10       sba  
FA6D: 9B 22    adda $22
FA6F: 97 22    sta  $22
FA71: 20 EA    bra  $FA5D
FA73: 32       pula 
FA74: DE 0A    ldx  $0A
FA76: 09       dex  
FA77: 6E 00    jmp  (x+$00)
FA79: 96 26    lda  $26
FA7B: 81 5F    cmpa #$5F
FA7D: 2B 01    bmi  $FA80
FA7F: 39       rts  
FA80: D6 23    ldb  $23
FA82: C0 07    subb #$07
FA84: BD F9 86 jsr  $F986
FA87: DE 25    ldx  $25
FA89: 6A 02    dec  (x+$02)
FA8B: 2B 12    bmi  $FA9F
FA8D: EE 00    ldx  (x+$00)
FA8F: A6 00    lda  (x+$00)
FA91: 36       psha 
FA92: 08       inx  
FA93: DF 0A    stx  $0A
FA95: F6 00 23 ldb  $0023
FA98: C0 09    subb #$09
FA9A: BD F9 86 jsr  $F986
FA9D: 20 55    bra  $FAF4
FA9F: EE 00    ldx  (x+$00)
FAA1: 08       inx  
FAA2: DF 0A    stx  $0A
FAA4: 96 26    lda  $26
FAA6: 8B 03    adda #$03
FAA8: 97 26    sta  $26
FAAA: D6 23    ldb  $23
FAAC: C0 07    subb #$07
FAAE: 01       nop  
FAAF: 7E F9 CA jmp  $F9CA
FAB2: 08       inx  
FAB3: 20 04    bra  $FAB9
FAB5: D7 20    stb  $20
FAB7: D7 21    stb  $21
FAB9: D6 29    ldb  $29
FABB: C4 0F    andb #$0F
FABD: CB F8    addb #$F8
FABF: C8 F8    eorb #$F8
FAC1: 32       pula 
FAC2: 9B 21    adda $21
FAC4: D9 20    adcb $20
FAC6: 97 21    sta  $21
FAC8: D7 20    stb  $20
FACA: F6 00 23 ldb  $0023
FACD: C0 09    subb #$09
FACF: 7E F9 CA jmp  $F9CA
FAD2: 96 26    lda  $26
FAD4: 80 03    suba #$03
FAD6: 97 26    sta  $26
FAD8: DE 25    ldx  $25
FADA: 96 0B    lda  $0B
FADC: D6 0A    ldb  $0A
FADE: 8B FF    adda #$FF
FAE0: C9 FF    adcb #$FF
FAE2: E7 00    stb  (x+$00)
FAE4: A7 01    sta  (x+$01)
FAE6: D6 29    ldb  $29
FAE8: C4 0F    andb #$0F
FAEA: E7 02    stb  (x+$02)
FAEC: D6 23    ldb  $23
FAEE: C0 0C    subb #$0C
FAF0: BD F9 86 jsr  $F986
FAF3: 08       inx  
FAF4: 08       inx  
FAF5: 08       inx  
FAF6: 5F       clrb 
FAF7: 01       nop  
FAF8: 32       pula 
FAF9: 47       asra 
FAFA: 49       rola 
FAFB: C2 00    sbcb #$00
FAFD: 9B 0B    adda $0B
FAFF: D9 0A    adcb $0A
FB01: 97 0B    sta  $0B
FB03: F7 00 0A stb  $000A
FB06: D6 23    ldb  $23
FB08: C0 07    subb #$07
FB0A: 7E F9 CA jmp  $F9CA
FB0D: FA 4F FA orb  $4FFA
FB10: 64 FA    lsr  (x+$FA)
FB12: B5 FA B2 bita $FAB2
FB15: FA 4F FA orb  $4FFA
FB18: 73 FA D2 com  $FAD2
FB1B: FA F8 FC orb  $F8FC
FB1E: 91 FD    cmpa $FD
FB20: 1A       illegal
FB21: FB C9 FC addb $C9FC
FB24: C2 FB    sbcb #$FB
FB26: 44       lsra 
FB27: FC       illegal
FB28: D3       illegal
FB29: FB 6F FC addb $6FFC
FB2C: 06       tap  
FB2D: DE 2F    ldx  $2F
FB2F: EE 03    ldx  (x+$03)
FB31: 08       inx  
FB32: DF 08    stx  $08
FB34: BD FC 00 jsr  $FC00
FB37: 08       inx  
FB38: 39       rts  
FB39: EE 00    ldx  (x+$00)
FB3B: DF 08    stx  $08
FB3D: CE FC 06 ldx  #$FC06
FB40: DF 2D    stx  $2D
FB42: 01       nop  
FB43: 39       rts  
FB44: 96 30    lda  $30
FB46: 81 37    cmpa #$37
FB48: 23 12    bls  $FB5C
FB4A: DE 2F    ldx  $2F
FB4C: 6A 02    dec  (x+$02)
FB4E: 2A E9    bpl  $FB39
FB50: 80 03    suba #$03
FB52: 97 30    sta  $30
FB54: CE FB 2D ldx  #$FB2D
FB57: DF 2D    stx  $2D
FB59: 6D 00    tst  (x+$00)
FB5B: 39       rts  
FB5C: CE FB 64 ldx  #$FB64
FB5F: DF 2D    stx  $2D
FB61: 01       nop  
FB62: 20 05    bra  $FB69
FB64: 08       inx  
FB65: 08       inx  
FB66: 01       nop  
FB67: 8D 05    bsr  $FB6E
FB69: 8D 03    bsr  $FB6E
FB6B: 6D 00    tst  (x+$00)
FB6D: 01       nop  
FB6E: 39       rts  
FB6F: DE 2F    ldx  $2F
FB71: 96 08    lda  $08
FB73: A7 03    sta  (x+$03)
FB75: 96 09    lda  $09
FB77: A7 04    sta  (x+$04)
FB79: 96 39    lda  $39
FB7B: 84 0F    anda #$0F
FB7D: A7 05    sta  (x+$05)
FB7F: 08       inx  
FB80: CE FB 86 ldx  #$FB86
FB83: DF 2D    stx  $2D
FB85: 39       rts  
FB86: 96 30    lda  $30
FB88: 8B 03    adda #$03
FB8A: 97 30    sta  $30
FB8C: CE FC 06 ldx  #$FC06
FB8F: DF 2D    stx  $2D
FB91: 01       nop  
FB92: 20 D5    bra  $FB69
FB94: 7D 00 2F tst  $002F
FB97: 26 CE    bne  $FB67
FB99: DE 08    ldx  $08
FB9B: A6 00    lda  (x+$00)
FB9D: 08       inx  
FB9E: DF 08    stx  $08
FBA0: 97 39    sta  $39
FBA2: 2A 05    bpl  $FBA9
FBA4: 97 2F    sta  $2F
FBA6: A6 00    lda  (x+$00)
FBA8: 39       rts  
FBA9: CE FB B0 ldx  #$FBB0
FBAC: FF 00 2D stx  $002D
FBAF: 39       rts  
FBB0: 5F       clrb 
FBB1: 96 39    lda  $39
FBB3: 84 70    anda #$70
FBB5: 44       lsra 
FBB6: 44       lsra 
FBB7: 44       lsra 
FBB8: 8B 1D    adda #$1D
FBBA: C9 FB    adcb #$FB
FBBC: D7 37    stb  $37
FBBE: 97 38    sta  $38
FBC0: DE 37    ldx  $37
FBC2: EE 00    ldx  (x+$00)
FBC4: DF 2D    stx  $2D
FBC6: DF 2D    stx  $2D
FBC8: 39       rts  
FBC9: 96 39    lda  $39
FBCB: 84 0F    anda #$0F
FBCD: 4C       inca 
FBCE: 4C       inca 
FBCF: 97 2F    sta  $2F
FBD1: 20 1D    bra  $FBF0
FBD3: 7C 00 32 inc  $0032
FBD6: DE 31    ldx  $31
FBD8: 8C 00 68 cmpx #$0068
FBDB: 27 13    beq  $FBF0
FBDD: A6 00    lda  (x+$00)
FBDF: CE FC 1A ldx  #$FC1A
FBE2: 97 35    sta  $35
FBE4: 27 03    beq  $FBE9
FBE6: 7E FB EC jmp  $FBEC
FBE9: CE FB D3 ldx  #$FBD3
FBEC: DF 2D    stx  $2D
FBEE: 08       inx  
FBEF: 39       rts  
FBF0: 86 5E    lda  #$5E
FBF2: B7 00 32 sta  $0032
FBF5: CE FB D3 ldx  #$FBD3
FBF8: 7A 00 2F dec  $002F
FBFB: 27 03    beq  $FC00
FBFD: 7E FC 03 jmp  $FC03
FC00: CE FB 94 ldx  #$FB94
FC03: DF 2D    stx  $2D
FC05: 39       rts  
FC06: DE 08    ldx  $08
FC08: 5F       clrb 
FC09: A6 00    lda  (x+$00)
FC0B: 4C       inca 
FC0C: 47       asra 
FC0D: 49       rola 
FC0E: C2 00    sbcb #$00
FC10: 9B 09    adda $09
FC12: D9 08    adcb $08
FC14: 97 09    sta  $09
FC16: D7 08    stb  $08
FC18: 20 E6    bra  $FC00
FC1A: 96 32    lda  $32
FC1C: 80 5F    suba #$5F
FC1E: 48       asla 
FC1F: 5F       clrb 
FC20: 9B 0F    adda $0F
FC22: D9 0E    adcb $0E
FC24: D7 37    stb  $37
FC26: 97 38    sta  $38
FC28: 86 80    lda  #$80
FC2A: 97 36    sta  $36
FC2C: CE FC 37 ldx  #$FC37
FC2F: DF 2D    stx  $2D
FC31: CE 00 10 ldx  #$0010
FC34: DF 33    stx  $33
FC36: 39       rts  
FC37: DE 37    ldx  $37
FC39: EE 00    ldx  (x+$00)
FC3B: DF 37    stx  $37
FC3D: CE FC 4C ldx  #$FC4C
FC40: DF 2D    stx  $2D
FC42: DE 31    ldx  $31
FC44: A6 09    lda  (x+$09)
FC46: 9B 35    adda $35
FC48: A7 09    sta  (x+$09)
FC4A: 08       inx  
FC4B: 39       rts  
FC4C: 96 36    lda  $36
FC4E: 27 1D    beq  $FC6D
FC50: 74 00 36 lsr  $0036
FC53: DE 33    ldx  $33
FC55: E6 00    ldb  (x+$00)
FC57: 94 37    anda $37
FC59: 26 09    bne  $FC64
FC5B: FB 00 35 addb $0035
FC5E: E7 00    stb  (x+$00)
FC60: 7C 00 34 inc  $0034
FC63: 39       rts  
FC64: F0 00 35 subb $0035
FC67: E7 00    stb  (x+$00)
FC69: 7C 00 34 inc  $0034
FC6C: 39       rts  
FC6D: D6 34    ldb  $34
FC6F: C1 20    cmpb #$20
FC71: 27 0B    beq  $FC7E
FC73: D6 38    ldb  $38
FC75: D7 37    stb  $37
FC77: C6 80    ldb  #$80
FC79: F7 00 36 stb  $0036
FC7C: 20 0F    bra  $FC8D
FC7E: CE FB 94 ldx  #$FB94
FC81: D6 2F    ldb  $2F
FC83: 26 03    bne  $FC88
FC85: 7E FC 8B jmp  $FC8B
FC88: CE FB D3 ldx  #$FBD3
FC8B: DF 2D    stx  $2D
FC8D: 6D 00    tst  (x+$00)
FC8F: 08       inx  
FC90: 39       rts  
FC91: 96 39    lda  $39
FC93: 84 07    anda #$07
FC95: 8B 60    adda #$60
FC97: 97 32    sta  $32
FC99: DE 08    ldx  $08
FC9B: A6 00    lda  (x+$00)
FC9D: 08       inx  
FC9E: DF 08    stx  $08
FCA0: 97 35    sta  $35
FCA2: CE FC A9 ldx  #$FCA9
FCA5: DF 2D    stx  $2D
FCA7: 08       inx  
FCA8: 39       rts  
FCA9: DE 31    ldx  $31
FCAB: 5F       clrb 
FCAC: 96 39    lda  $39
FCAE: 8B F8    adda #$F8
FCB0: C2 00    sbcb #$00
FCB2: E4 09    andb (x+$09)
FCB4: 50       negb 
FCB5: DB 35    addb $35
FCB7: D7 35    stb  $35
FCB9: CE FC 1A ldx  #$FC1A
FCBC: DF 2D    stx  $2D
FCBE: 08       inx  
FCBF: 08       inx  
FCC0: 01       nop  
FCC1: 39       rts  
FCC2: D6 39    ldb  $39
FCC4: 54       lsrb 
FCC5: C4 07    andb #$07
FCC7: CA 60    orb  #$60
FCC9: D7 32    stb  $32
FCCB: C6 FF    ldb  #$FF
FCCD: C9 00    adcb #$00
FCCF: C9 00    adcb #$00
FCD1: 20 E4    bra  $FCB7
FCD3: 96 39    lda  $39
FCD5: 47       asra 
FCD6: 25 13    bcs  $FCEB
FCD8: CE 00 00 ldx  #$0000
FCDB: DF 60    stx  $60
FCDD: DF 62    stx  $62
FCDF: DF 64    stx  $64
FCE1: DF 66    stx  $66
FCE3: 08       inx  
FCE4: CE FB 94 ldx  #$FB94
FCE7: FF 00 2D stx  $002D
FCEA: 39       rts  
FCEB: 85 02    bita #$02
FCED: 26 0C    bne  $FCFB
FCEF: C6 5F    ldb  #$5F
FCF1: D7 32    stb  $32
FCF3: CE FD 00 ldx  #$FD00
FCF6: DF 2D    stx  $2D
FCF8: 7E FB 6B jmp  $FB6B
FCFB: FE 00 08 ldx  $0008
FCFE: 20 F6    bra  $FCF6
FD00: 5F       clrb 
FD01: 96 39    lda  $39
FD03: 8B AE    adda #$AE
FD05: C2 00    sbcb #$00
FD07: D4 68    andb $68
FD09: DE 08    ldx  $08
FD0B: A6 00    lda  (x+$00)
FD0D: 08       inx  
FD0E: DF 08    stx  $08
FD10: 10       sba  
FD11: 97 35    sta  $35
FD13: CE FC 1A ldx  #$FC1A
FD16: FF 00 2D stx  $002D
FD19: 39       rts  
FD1A: C6 60    ldb  #$60
FD1C: D7 32    stb  $32
FD1E: DE 08    ldx  $08
FD20: E6 00    ldb  (x+$00)
FD22: D7 37    stb  $37
FD24: 08       inx  
FD25: DF 08    stx  $08
FD27: D6 39    ldb  $39
FD29: 54       lsrb 
FD2A: 24 18    bcc  $FD44
FD2C: CE FD 5E ldx  #$FD5E
FD2F: DF 2D    stx  $2D
FD31: 39       rts  
FD32: 5F       clrb 
FD33: 96 38    lda  $38
FD35: 47       asra 
FD36: C2 00    sbcb #$00
FD38: DE 31    ldx  $31
FD3A: E4 00    andb (x+$00)
FD3C: 1B       aba  
FD3D: A7 00    sta  (x+$00)
FD3F: 7C 00 32 inc  $0032
FD42: A6 00    lda  (x+$00)
FD44: CE FD 4A ldx  #$FD4A
FD47: DF 2D    stx  $2D
FD49: 39       rts  
FD4A: 78 00 37 asl  $0037
FD4D: 25 13    bcs  $FD62
FD4F: 27 06    beq  $FD57
FD51: 7C 00 32 inc  $0032
FD54: 7E FB 69 jmp  $FB69
FD57: BD FC 00 jsr  $FC00
FD5A: 6D 00    tst  (x+$00)
FD5C: 01       nop  
FD5D: 39       rts  
FD5E: 7A 00 32 dec  $0032
FD61: 08       inx  
FD62: A6 00    lda  (x+$00)
FD64: DE 08    ldx  $08
FD66: A6 00    lda  (x+$00)
FD68: 08       inx  
FD69: DF 08    stx  $08
FD6B: 97 38    sta  $38
FD6D: CE FD 32 ldx  #$FD32
FD70: DF 2D    stx  $2D
FD72: 39       rts
;;
;;WALSHT 
FD73: FE 26 FE 6A 
FD77: FD C7 FE 58
FD7B: FD C7 FE 70
FD7F: FE 01 FE 70 
FD83: FE 0A FE 58
FD87: FE 0A FE 5C 
FD8B: FE 0A FE 70
;;WALSH
FD8F: 5F       clrb
FD90: D7 0D    stb  $0D
FD92: 48       asla 
FD93: 48       asla 
FD94: 8B 73    adda #$73
FD96: C9 FD    adcb #$FD
FD98: D7 0A    stb  $0A
FD9A: 97 0B    sta  $0B
FD9C: DE 0A    ldx  $0A
FD9E: EE 00    ldx  (x+$00)
FDA0: DF 08    stx  $08
FDA2: DE 0A    ldx  $0A
FDA4: EE 02    ldx  (x+$02)
FDA6: E6 00    ldb  (x+$00)
FDA8: D7 0C    stb  $0C
FDAA: 08       inx  
FDAB: DF 0A    stx  $0A
FDAD: CE FD B5 ldx  #$FDB5
FDB0: DF 0E    stx  $0E
FDB2: 7E F9 A9 jmp  $F9A9
;;
;;ODDTBL
FDB5: 00       illegal
FDB6: 00       illegal
FDB7: 55       illegal
FDB8: 55       illegal
FDB9: AA 55    ora  (x+$55)
FDBB: 5A       decb 
FDBC: 5A       decb 
FDBD: 96 69    lda  $69
FDBF: 66 66    ror  (x+$66)
FDC1: CC       illegal
FDC2: 33       pulb 
FDC3: 3C       illegal
FDC4: 3C       illegal
FDC5: 0F       sei  
FDC6: F0 
;;
;; WAVE PROGRAMS
FDC7: 53 80
FDC9: 10       sba  
FDCA: 8B 08    adda #$08
FDCC: 06       tap  
FDCD: 04       illegal
FDCE: 02       illegal
FDCF: 69 28    rol  (x+$28)
FDD1: 10       sba  
FDD2: 80 F8    suba #$F8
FDD4: 62       illegal
FDD5: 23 10    bls  $FDE7
FDD7: 88 00    eora #$00
FDD9: FA 63 1D orb  $631D
FDDC: 10       sba  
FDDD: 88 FC    eora #$FC
FDDF: 00       illegal
FDE0: 63 17    com  (x+$17)
FDE2: 10       sba  
FDE3: 0B       sev  
FDE4: FC       illegal
FDE5: FC       illegal
FDE6: FE 69 10 ldx  $6910
FDE9: 10       sba  
FDEA: 8A 00    ora  #$00
FDEC: FE FA 66 ldx  $FA66
FDEF: 09       dex  
FDF0: 3C       illegal
FDF1: 07       tpa  
FDF2: FC       illegal
FDF3: 70 2D FF neg  $2DFF
FDF6: FF FF FF stx  $FFFF
FDF9: FF FF FF stx  $FFFF
FDFC: FF FF FF stx  $FFFF
FDFF: 20 40    bra  $FE41
FE01: 10       sba  
FE02: 0F       sei  
FE03: 02       illegal
FE04: FE 02 FE ldx  $02FE
FE07: 2F 70    ble  $FE79
FE09: FD       illegal
FE0A: 0F       sei  
FE0B: 28 10    bvc  $FE1D
FE0D: 74 02 02 lsr  $0202
FE10: 02       illegal
FE11: 02       illegal
FE12: 2F 10    ble  $FE24
FE14: 24 FE    bcc  $FE14
FE16: FE 2F 10 ldx  $2F10
FE19: 50       negb 
FE1A: FE FE 2F ldx  $FE2F
FE1D: 10       sba  
FE1E: 24 02    bcc  $FE22
FE20: 02       illegal
FE21: 2F 55    ble  $FE78
FE23: 7E F5 B0 jmp  $F5B0
FE26: 0F       sei  
FE27: 08       inx  
FE28: 6F 10    clr  (x+$10)
FE2A: 6F 1D    clr  (x+$1D)
FE2C: 0B       sev  
FE2D: 28 0D    bvc  $FE3C
FE2F: 00       illegal
FE30: 0C       clc  
FE31: 28 0B    bvc  $FE3E
FE33: 00       illegal
FE34: 0D       sec  
FE35: 28 0C    bvc  $FE43
FE37: 00       illegal
FE38: 70 F2 6F neg  $F26F
FE3B: 00       illegal
FE3C: 0B       sev  
FE3D: 05       illegal
FE3E: 0D       sec  
FE3F: 00       illegal
FE40: 0C       clc  
FE41: 05       illegal
FE42: 0B       sev  
FE43: 00       illegal
FE44: 0D       sec  
FE45: 05       illegal
FE46: 0C       clc  
FE47: 00       illegal
FE48: 40       nega 
FE49: 6F 00    clr  (x+$00)
FE4B: 0B       sev  
FE4C: 0A       clv  
FE4D: 0D       sec  
FE4E: 00       illegal
FE4F: 0C       clc  
FE50: 0A       clv  
FE51: 0B       sev  
FE52: 00       illegal
FE53: 0D       sec  
FE54: 0A       clv  
FE55: 0C       clc  
FE56: 00       illegal
FE57: 40       nega 
FE58: 3C       illegal
FE59: 81 70    cmpa #$70
FE5B: FD       illegal
FE5C: 2A 21    bpl  $FE7F
FE5E: 00       illegal
FE5F: 01       nop  
FE60: 2A F0    bpl  $FE52
FE62: 01       nop  
FE63: 1F       illegal
FE64: EB 01    addb (x+$01)
FE66: 19       daa  
FE67: F5 70 F3 bitb $70F3
FE6A: 19       daa  
FE6B: 20 28    bra  $FE95
FE6D: 81 70    cmpa #$70
FE6F: FD       illegal
FE70: 37       pshb 
FE71: 2F 00    ble  $FE73
FE73: 01       nop  
FE74: 37       pshb 
FE75: F5 01 2D bitb $012D
FE78: F5 01 46 bitb $0146
FE7B: FA 70 F5 orb  $70F5
FE7E: F6 F9 FC ldb  $F9FC
FE81: FF 3F C1 stx  $3FC1
FE84: 3F       swi  
FE85: C1 FF    cmpb #$FF
FE87: FF FF FF stx  $FFFF
FE8A: CE FE 7E ldx  #$FE7E
FE8D: C6 15    ldb  #$15
FE8F: 4F       clra 
FE90: 97 0D    sta  $0D
FE92: D7 0E    stb  $0E
FE94: C6 0C    ldb  #$0C
FE96: BD F3 A8 jsr  $F3A8
FE99: 7C 00 04 inc  $0004
FE9C: D6 04    ldb  $04
FE9E: 96 15    lda  $15
FEA0: 9B 1D    adda $1D
FEA2: 97 15    sta  $15
FEA4: 96 16    lda  $16
FEA6: 9B 1E    adda $1E
FEA8: 97 16    sta  $16
FEAA: 96 17    lda  $17
FEAC: 9B 1F    adda $1F
FEAE: 97 17    sta  $17
FEB0: 96 18    lda  $18
FEB2: 9B 20    adda $20
FEB4: 97 18    sta  $18
FEB6: 5A       decb 
FEB7: 26 E5    bne  $FE9E
FEB9: DE 15    ldx  $15
FEBB: DF 11    stx  $11
FEBD: DE 17    ldx  $17
FEBF: DF 13    stx  $13
FEC1: F7 04 00 stb  $0400
FEC4: 7A 00 11 dec  $0011
FEC7: 26 09    bne  $FED2
FEC9: 96 15    lda  $15
FECB: 97 11    sta  $11
FECD: 70 00 19 neg  $0019
FED0: DB 19    addb $19
FED2: 7A 00 12 dec  $0012
FED5: 26 09    bne  $FEE0
FED7: 96 16    lda  $16
FED9: 97 12    sta  $12
FEDB: 70 00 1A neg  $001A
FEDE: DB 1A    addb $1A
FEE0: 7A 00 13 dec  $0013
FEE3: 26 09    bne  $FEEE
FEE5: 96 17    lda  $17
FEE7: 97 13    sta  $13
FEE9: 70 00 1B neg  $001B
FEEC: DB 1B    addb $1B
FEEE: 7A 00 14 dec  $0014
FEF1: 26 CE    bne  $FEC1
FEF3: 96 18    lda  $18
FEF5: 97 14    sta  $14
FEF7: 70 00 1C neg  $001C
FEFA: DB 1C    addb $1C
FEFC: 7E FE C1 jmp  $FEC1
FEFF: 86 A5    lda  #$A5
FF01: 97 11    sta  $11
FF03: 86 85    lda  #$85
FF05: 97 12    sta  $12
FF07: 86 01    lda  #$01
FF09: 97 13    sta  $13
FF0B: 86 80    lda  #$80
FF0D: 97 1D    sta  $1D
FF0F: 86 FF    lda  #$FF
FF11: 97 14    sta  $14
FF13: CE 04 00 ldx  #$0400
FF16: DF 15    stx  $15
FF18: 96 11    lda  $11
FF1A: 48       asla 
FF1B: 24 02    bcc  $FF1F
FF1D: 98 12    eora $12
FF1F: B7 04 00 sta  $0400
FF22: D6 13    ldb  $13
FF24: 09       dex  
FF25: 27 05    beq  $FF2C
FF27: 5A       decb 
FF28: 26 FA    bne  $FF24
FF2A: 20 EE    bra  $FF1A
FF2C: D6 13    ldb  $13
FF2E: D0 14    subb $14
FF30: D7 13    stb  $13
FF32: D1 1D    cmpb $1D
FF34: 27 04    beq  $FF3A
FF36: DE 15    ldx  $15
FF38: 20 E0    bra  $FF1A
FF3A: 39       rts  
FF3B: 86 A5    lda  #$A5
FF3D: 97 11    sta  $11
FF3F: 86 85    lda  #$85
FF41: 97 12    sta  $12
FF43: 86 03    lda  #$03
FF45: 97 13    sta  $13
FF47: 86 80    lda  #$80
FF49: 97 1D    sta  $1D
FF4B: 86 FF    lda  #$FF
FF4D: 97 14    sta  $14
FF4F: CE 03 00 ldx  #$0300
FF52: 20 C2    bra  $FF16
FF54: 86 01    lda  #$01
FF56: 97 19    sta  $19
FF58: 86 01    lda  #$01
FF5A: 97 1C    sta  $1C
FF5C: 86 6C    lda  #$6C
FF5E: 97 12    sta  $12
FF60: 86 FF    lda  #$FF
FF62: 97 1B    sta  $1B
FF64: 86 01    lda  #$01
FF66: C6 6C    ldb  #$6C
FF68: 20 2A    bra  $FF94
FF6A: 86 02    lda  #$02
FF6C: 97 19    sta  $19
FF6E: 86 1D    lda  #$1D
FF70: 97 12    sta  $12
FF72: 86 01    lda  #$01
FF74: 97 1C    sta  $1C
FF76: 86 C8    lda  #$C8
FF78: 97 1B    sta  $1B
FF7A: 86 01    lda  #$01
FF7C: C6 6C    ldb  #$6C
FF7E: 20 14    bra  $FF94
FF80: 86 01    lda  #$01
FF82: 97 19    sta  $19
FF84: 86 1D    lda  #$1D
FF86: 97 12    sta  $12
FF88: 86 01    lda  #$01
FF8A: 97 1C    sta  $1C
FF8C: 86 FF    lda  #$FF
FF8E: 97 1B    sta  $1B
FF90: 86 01    lda  #$01
FF92: C6 6C    ldb  #$6C
FF94: 97 18    sta  $18
FF96: 7F 04 00 clr  $0400
FF99: 96 12    lda  $12
FF9B: D7 1A    stb  $1A
FF9D: D6 1A    ldb  $1A
FF9F: D7 17    stb  $17
FFA1: 48       asla 
FFA2: 24 0A    bcc  $FFAE
FFA4: F6 04 00 ldb  $0400
FFA7: D8 1B    eorb $1B
FFA9: F7 04 00 stb  $0400
FFAC: 98 12    eora $12
FFAE: D6 18    ldb  $18
FFB0: 5A       decb 
FFB1: 26 FD    bne  $FFB0
FFB3: 7A 00 17 dec  $0017
FFB6: 26 E9    bne  $FFA1
FFB8: 7F 04 00 clr  $0400
FFBB: D6 1B    ldb  $1B
FFBD: D0 1C    subb $1C
FFBF: D7 1B    stb  $1B
FFC1: 27 08    beq  $FFCB
FFC3: D6 18    ldb  $18
FFC5: DB 19    addb $19
FFC7: D7 18    stb  $18
FFC9: 26 D2    bne  $FF9D
FFCB: 39       rts  
FFCC: 00       illegal
FFCD: 00       illegal
FFCE: 00       illegal
FFCF: 00       illegal
FFD0: 00       illegal
FFD1: 00       illegal
FFD2: 00       illegal
FFD3: 00       illegal
FFD4: 00       illegal
FFD5: 00       illegal
FFD6: 00       illegal
FFD7: 00       illegal
FFD8: 00       illegal
FFD9: 00       illegal
FFDA: 00       illegal
FFDB: 00       illegal
FFDC: 00       illegal
FFDD: 00       illegal
FFDE: 00       illegal
FFDF: 00       illegal
FFE0: 00       illegal
FFE1: 00       illegal
FFE2: 00       illegal
FFE3: 00       illegal
FFE4: 00       illegal
FFE5: 00       illegal
FFE6: 00       illegal
FFE7: 00       illegal
FFE8: 00       illegal
FFE9: 00       illegal
FFEA: 00       illegal
FFEB: 00       illegal
FFEC: 00       illegal
FFED: 00       illegal
FFEE: 00       illegal
FFEF: 00       illegal
FFF0: 00       illegal
FFF1: 00       illegal
FFF2: 00       illegal
FFF3: 00       illegal
FFF4: 00       illegal
FFF5: 00       illegal
FFF6: 00       illegal
FFF7: 00       illegal
FFF8: F5 54 F0 bitb $54F0
FFFB: 1D       illegal
FFFC: F5 D1 F0 bitb $D1F0
FFFF: 1D       illegal
