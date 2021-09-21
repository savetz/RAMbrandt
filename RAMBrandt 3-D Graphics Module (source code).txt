════════════════════════════════════════════════════════════════   SCREEN 005
( MENU SELECTOR )                0 VARIABLE VALUE               
 0 VARIABLE CURX                 0 VARIABLE CURY                
 0 VARIABLE ADDV                 : POS. CURX @ 657 ! ;          
   CURY @ 656 C! ;                                              
 : NOMAR 0 82 C! 39 83 C! ;      : VISCUR NOT 752 C! ;          
 : SETVAL ( mod# ... name )        <BUILDS , DOES> POS. ."    " 
   POS. VALUE @ ADDV @ + SWAP @    MOD DUP VALUE ! . ;          
                                                             -->
                                 : SAVMSC 88 @ 2 0 DO DUP I     
   128 * + 1 I + 0 R/W LOOP DROP  ;                             
 : GETMSC ( addr ... )             2 0 DO DUP I 128 * + 1 I + 1 
   R/W LOOP ;                                                   
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 009
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
( MODULE TESTING )               29000 CONSTANT MODSTART        
: GETMOD <BUILDS   C,   ,         DOES> ->0    0 MODSTART !     
       DUP 1+ @ MODSTART SWAP   1 R/W 0  NOT SWAP C@ MODSTART C@
  = AND NOT                       IF      BUZZ BUZZ             
  ELSE MODSTART 1+ @ MODSTART     3 + @ 0 DO MODSTART I 128 *   
  + OVER I + 1 R/W LOOP  DROP    MODSTART 5 + @EX THEN 0->   ;  
 320 5 GETMOD GLO                                               
                                                                
════════════════════════════════════════════════════════════════   SCREEN 010
( MULTIPLE CFA VAR             )                                
: TO                              -FIND 0= 0 ?ERROR DROP        
          STATE @                 IF ,                          
  ELSE EXECUTE                    ENDIF ; IMMEDIATE             
                                : AT                            
  -FIND 0= 0 ?ERROR DROP                2+ STATE @              
  IF ,                            ELSE EXECUTE                  
  ENDIF ; IMMEDIATE               ( corrected )              -->
( Quan:  patch for CREATE      )                                
DCX                                                             
: (PTCH)              ( system )  SWAP >R R = 251 R = 249 R> =  
  OR OR ;                                                       
: PTCH                ( system )  IF [ ' (PTCH) CFA ] LITERAL   
  ELSE [ ' = CFA ] LITERAL        ENDIF                         
  [ ' CREATE 63 + ] LITERAL ! ;                                 
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 011
( MULTIPLE CFA                 )                                
HEX ASSEMBLER                   LABEL <CFA> DEX, DEX, W LDA,    
 0 ,X STA, W 1+ LDA, 1 ,X STA,   PLA, W STA, PLA, W 1+ STA,     
 W LDA, EQ IF, W 1+ DEC, THEN,   W DEC, D44     JMP,            
 FORTH  DCX                                                     
                                : CFA: <BUILDS 32 C, <CFA> ,    
  SMUDGE !CSP ] DOES> , ;                                       
                                                             -->
( VAR -HEADERLESS              )( BY SETTING AFL = 1 LEAVES  )  
( ADDRESS INSTEAD OF VALUE   )   LABEL AFL 0 C,                 
 CFA: Q@ 6 + AFL C@ NOT IF @          THEN ;                    
                                 CFA: Q! 4 + ! ;                
                                 CFA: QAT 2+ ;                  
: VAR ON PTCH LABEL  -2 ALLOT     Q@ Q! QAT 0 , OFF PTCH ;      
( Only headerless words here )                                  
DCX                                                          -->
════════════════════════════════════════════════════════════════   SCREEN 012
( MORE REDEFINITIONS )                                          
 : : 0 AFL C! [COMPILE] : ;      IMMEDIATE                      
                                 : CODE 1 AFL C! [COMPILE] CODE 
 ; IMMEDIATE                                                    
 : C; 0 AFL C! [COMPILE] C; ;     IMMEDIATE                     
                                                                
                                                                
                                 DCX 26 LOAD                    
( CONSTANTS FOR PAINT 10  )         ON HEADLESS                 
 HEX                              500 CONSTANT SCHI             
 5C0  CONSTANT SCLO              B22B CONSTANT FLSTK            
 BE00 CONSTANT OFFMSK            BEA0 CONSTANT ONMSK            
 BF40 CONSTANT YOFF              DCX                            
  ' SCHI FENCE !                                                
                                                                
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 013
 ( CONSTANTS PAINT 10 )          LABEL COLTAB 16 ALLOT          
                                   -->                          
                                 LABEL SCHI 192 ALLOT           
 LABEL SCLO 192 ALLOT            LABEL FLSTK 2048 ALLOT         
 LABEL YOFF 192 ALLOT            LABEL ONMSK 160 ALLOT          
 LABEL OFFMSK 160 ALLOT                                         
                                                                
                                                             -->
( CONSTANTS FOR P10           )  LABEL BRACT 4 ALLOT            
 LABEL OLDCOL 10 ALLOT           VAR ?MSK  VAR GMODE            
 : MDTAB <BUILDS DOES> GMODE       + C@ ;                       
 VAR XOLD VAR YOLD VAR BRVT      VAR BRHZ                       
 : BRINIT BRACT 4 ERASE ;        VAR PBUFPT                     
 VAR OLDST                       HEX                            
 9150 TO PBUFPT                                                 
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 014
( CONSTANTS FOR PAINT         ) MDTAB NCLRS ( 9 C, FOR MD 5 )   
4 C, 10 C, 9 C, 10 C, 4 C,      MDTAB SCLN 60 C, C0 C, C0 C,    
C0 C, C0 C,                     MDTAB GR? ( 17 C, )             
17 C, 9 C, A C, B C, 18 C,      MDTAB XEND 9F C, 4F C, 4F C,    
4F C, 9F C, ( 4F C, )                                        -->
                                                                
                                                                
                                                                
( CONSTANTS FOR PAINT          ) VAR MCOL1      VAR MCOL2       
 VAR YMX                         VAR XND        VAR YND         
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 015
( RAMBRANDT WORDS )            ) DCX                            
: ?KEY BEGIN 764 C@ 255 = WHILE    REPEAT 255 764 C! ;          
: CLICK  53279 8 OVER C! 7 OVER   C! 8 SWAP C! ;                
: BUZZ 192 0 DO CLICK LOOP ;    : 28* 40 * ;                    
: PAUSE 540 C! BEGIN 540 C@ 0=    UNTIL ;                       
: SPAUSE 10 PAUSE ;             : LPAUSE 25 PAUSE ;             
: MYCOL NOOP  ; : ZIPFG NOOP ;   : CF, [COMPILE] ' CFA , ;      
                                                             -->
( CONSTANTS                    ) HEX                            
VAR PXS  50 TO PXS              VAR PYS  C0 TO PYS              
VAR CL1 11 TO CL1                LABEL CLR4 0 C, 55 C, AA C,    
 FF C,                           LABEL CLR16 0 C, 11 C, 22 C,   
 33 C, 44 C, 55 C, 66 C, 77 C,   88 C, 99 C, AA C, BB C, CC C,  
 DD C, EE C, FF C,               VAR RW                         
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 016
( CONSTANTS                   )  VAR PXB VAR CLRS               
 LABEL MS4 C0 C, 30 C, C C, 3 C, LABEL MS2 F0 C, 0F C,          
 MDTAB CLO A C, 4 C, 5 C, 4 C,   A C, ( 5 C, )                  
 MDTAB PRT 8F C, 47 C, 47 C, 47  C, 8F C, (  4B C, )            
 MDTAB HMSK FF C, F0 C, FF C,    0F C, FF C, ( FF C, )          
 : 764 2FC ;                 -->                                
                                                                
                                                                
( MORE CONSTANTS )                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 017
( GR 7PLUS                     )CODE 7PLUS                ( -- )
  A9 C, 07 C, 85 C, 57 C, AD C,   30 C, 02 C, 85 C, N  C, AD C, 
  31 C, 02 C, 85 C, N 1+ C, HERE  B1 C, N  C, 29 C, FC C, C9 C, 
  40 C, F0 C, 14 C, B1 C, N C,    85 C, N 2+ C, 29 C,           
  0F C, C9 C, 0F C, D0 C, 06 C,   C6 C, N 2+ C, A5 C, N 2+ C,   
  91 C, N  C, C8 C, 4C C, ,       4C C, NEXT ,  C;              
CODE LO-HI ( wd -- lo hi )      1 ,X LDA, 1 ,X STY, PUSH0A JMP, 
 C; DCX                           : 128 128 ;                -->
( HISPEED GRAPHICS TABLES )     BASE @ HEX                      
                                                                
                                 F0         CONSTANT OLDX       
 OLDX 1 +   CONSTANT OLDY                                       
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 018
( ZERO PAGE EQUATES )                                           
 OLDX A +   CONSTANT XMAX        OLDX B +   CONSTANT XMIN       
 OLDX C +   CONSTANT YMAX        OLDX D +   CONSTANT YMIN       
 OLDX 7 +   CONSTANT COLR        OLDX 8 +   CONSTANT SCRN       
                                                             -->
                                                                
                                                                
                                                                
( LOCATE )                        1 AFL C!                      
 CODE BDS?   OLDX LDX, OLDY      LDY, FF # LDA, XMIN CPX, IFCC, 
 RTS, THEN, XMAX CPX, IFCS, RTS, THEN, YMIN CPY, IFCC, RTS,     
 THEN, YMAX CPY, IFCS, RTS,      THEN, 0 # LDA, RTS, C;         
                                 : STVB NOOP ;                  
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 019
       DCX                      : GRF GR? GR. ( GMODE 5 = IF )  
 ( 80 26F C! THEN ) GMODE 4 = IF  7PLUS THEN ; DCX              
 : ->0 88 @ PBUFPT 7680 CMOVE      0 GR. ;                      
                                 : 0-> GRF PBUFPT 88 @ 7680     
   CMOVE ;                                                      
                                                                
                                                                
                                                             -->
VAR BKOFF                       : DFSET ;                       
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 020
( GRAPHICS )                     HEX  0 AFL C!                  
: XTAB A0 0 DO I PXB /MOD YOFF I  + C! ?MSK + C@ DUP ONMSK I +  
  C! FF XOR OFFMSK I + C! LOOP ;                                
: .LN ( # blk  ... ) OVER 3 .R    SPACE SWAP  4 /MOD ROT + FLSTK
  SWAP 1 R/W BL * FLSTK + BL      -TRAILING TYPE ;              
                                                                
                                                                
                                                             -->
( GRAPHICS )                     : GRX ( A0 6A C! MUST INCLUDE )
           DUP TO GMODE GR? GR.    NCLRS 5 > IF 2 10 ELSE 4 DUP 
   THEN TO CLRS DUP TO PXB 4 =     IF MS4 CLR4 ELSE MS2 CLR16   
   THEN COLTAB 10 CMOVE TO ?MSK    XEND TO XND                  
   4 = IF 7PLUS THEN C0 0 DO       I 28 * 58 @ + LO-HI I SCHI + 
   C! I SCLO + C! LOOP XTAB        SCLN 1- TO YND               
   YND 1+ TO YMX                   ; DCX                        
       19 LOAD                   DCX   HERE .                   
════════════════════════════════════════════════════════════════   SCREEN 021
( TESTER )                        OFF HEADLESS                  
  HEX                            : TESTER  90 6A C!  GRX        
   9150 TO PBUFPT                  BEGIN 764 C@ 3D = IF FF 764  
 C! GLO THEN  30 PAUSE             ?TERMINAL UNTIL ;            
   DCX                           HERE .                         
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 022
( MAIN MOD )                      HERE DUP .   MODSTART DP !    
 5 C, 320 ,  4 , ' NOOP CFA ,    5 ' GLO 2 + C!                 
 320 ' GLO 3 + !                ON HEADLESS                     
 LABEL APL0                      29400 CONSTANT MODST           
 : MOD? FLSTK 128 + ;                                           
 : CHOICE                          ." Directory (D)" CR         
   ." Load - pick # " CR          ;                             
                                                             -->
( module loader )                : MDDIR                        
   CR ." Modules:" CR             MOD? C@ -DUP IF 0 DO CR       
        I 324 .LN LOOP CR CR      CHOICE ELSE BUZZ THEN ;       
                                 : RUNMOD ( # ... )             
   2* MOD? 1+ + @                  MODST SWAP 1 R/W MODST 1+ @  
   MODST 3 + @ 0 DO MODST I 128    * + OVER I + 1 R/W LOOP DROP 
   MODST 5 + @EX ;                                              
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 023
( MODULE LOADER )               : PICKMOD MOD? 329 1 R/W        
  CLS CR CHOICE BEGIN 0 764 C@   255 < IF  KEY DUP 68 = IF      
  DROP MDDIR ELSE 48 - DUP 0<     NOT OVER MOD? C@ < AND        
  IF SWAP DROP 1 SWAP                RUNMOD                     
  ELSE DROP                       THEN  THEN                    
  THEN ?TERMINAL OR UNTIL ;                                     
                                                                
                             -->                                
( PINCH OFF MODULES )                                           
  ' PICKMOD CFA MODSTART 5 + !     HERE U.                      
   DP !                                                         
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 024
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                DCX                             
 HERE                             MODST DP !                    
 5 C, 30  , 50 , ' NOOP CFA ,                                   
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 025
( ZERO PAGE EQUATES )                                           
HEX                              F2 CONSTANT MLPCND             
 F2 CONSTANT RADCND              F3 CONSTANT MLPLER             
 F4 CONSTANT PROD                DC CONSTANT DVDND              
 DA CONSTANT DVSOR               DA CONSTANT TEMP               
 DE CONSTANT RMNDR                                              
 LABEL ROOT 0 ,                                                 
                                                             -->
( CONSTANTS )                    HEX                            
 LABEL RAM 8F80 ,                : *+ RAM @ CONSTANT RAM +! ;   
                                                                
                             -->                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 026
( MODULE # 4 3D OBJECTS )  HEX   2 *+ XCENT     2 *+ YCENT      
 2 *+ RO        2 *+ RI          2 *+ CLIPL     2 *+ CLIPR      
 2 *+ CLIPU     2 *+ CLIPD       1 *+ YREL      2 *+ YSHD       
 2 *+ ZREL      2 *+ ZWX         2 *+ RADIUS    2 *+ TONE       
 2 *+ TNTMP     1 *+ HEMI        1 *+ BAKLIT    1 *+ HVFLAG     
 2 *+ TEM2      1 *+ CNTX        1 *+ CNTY      1 *+ MAXX       
 1 *+ HLEN      2 *+ RS          1 *+ RT        1 *+ RC         
 2 *+ XSHD      1 *+ XREL        BAKLIT .                    -->
( MORE RAM STUFF )               1 *+ PLTFLG                    
 2 *+ XPLT                       1 *+ YPLT                      
 2 *+ VALUE                      1 *+ HTORRN                    
 1 *+ NOSCAL                     2 *+ XSQR      1 *+ XMAXX      
 HLEN CONSTANT R0                PROD CONSTANT SQR              
 DVDND CONSTANT QUOT             MLPCND CONSTANT ARG            
 0 VARIABLE CHCLUP               0 VARIABLE PTPLT2              
 0 VARIABLE RHEMI                                            -->
════════════════════════════════════════════════════════════════   SCREEN 027
( PLOTTING STUFF )               VAR ?PLT       HEX             
 8DC0 CONSTANT THRESH            LABEL COLTB2 10 ALLOT          
 0 VARIABLE XPL2                 LABEL HORZ 4 ALLOT             
 LABEL VERT 4 ALLOT              VAR YCR VAR XCR                
 VAR TCX VAR TCY                 VAR OBJ#                       
 8D00 CONSTANT TXTWND            8CD0 CONSTANT SHPMEN           
 8DA0 CONSTANT SHDMEN            8DB2 CONSTANT LITMEN           
                                                             -->
( CONSTANTS )                    CODE VBOFF XSAVE STX,          
 62 # LDY, E4 # LDX, 7 # LDA,    E45C JSR, XSAVE LDX, NEXT JMP, 
 C;                                                             
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 028
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 029
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 030
( MULT SQUARE )                  HEX                            
 CODE MULT 0 # LDA, 8 # LDX,      BEGIN, MLPLER LSR, IFCS,      
  CLC, MLPCND ADC, THEN, ROR.A,   PROD ROR, DEX,                
  EQ UNTIL, PROD 1+ STA, RTS, C;                                
 CODE SQUARE MLPCND LDA, IFMI,   SEC, 0 # LDA, MLPCND SBC,      
 MLPCND STA, THEN, MLPLER STA,   ' MULT JMP, C;                 
                                                                
                                                             -->
( DIVIDE )                      CODE DIVIDE 0 # LDA, RMNDR STA, 
 RMNDR 1+ STA, 10 # LDX,         BEGIN, DVDND ROL, DVDND 1+ ROL,
  RMNDR ROL, RMNDR 1+ ROL, SEC,   RMNDR LDA, DVSOR SBC, TAY,    
  RMNDR 1+ LDA, DVSOR 1+ SBC,     IFCS, RMNDR STY, RMNDR 1+ STA,
 THEN, DEX, EQ UNTIL, DVDND ROL, DVDND 1+ ROL, RMNDR ASL, RMNDR 
 1+ ROL, IFCC, SEC, DVSOR LDA,   RMNDR SBC, DVSOR 1+ LDA, RMNDR 
 1+ SBC, IFCS, RTS, THEN, THEN,  DVDND INC, IFEQ, DVDND 1+ INC, 
 THEN, RTS, C;                                               -->
════════════════════════════════════════════════════════════════   SCREEN 031
( SQUARE ROOT )                  CODE SQRT 8 # LDX, 0 # LDA,    
 ROOT STA, ROOT 1+ STA, TEMP     STA, TEMP 1+ STA,              
 BEGIN, ROOT ASL, ROOT 1+ ROL,   ROOT INC, IFEQ, ROOT 1+ INC,   
 THEN, RADCND ASL, RADCND 1+     ROL, TEMP ROL, TEMP 1+ ROL,    
 RADCND ASL, RADCND 1+ ROL, TEMP ROL, TEMP 1+ ROL, SEC, TEMP    
 LDA, ROOT SBC, TAY, TEMP 1+     LDA, ROOT 1+ SBC, 90 C, 12 C,  
 TEMP 1+ STA, TEMP STY, ROOT     INC, IFEQ, ROOT 1+ INC, THEN,  
 DEX, EQ UNTIL, HERE 14 + JMP,                               -->
( SQUARE ROOT CONTINUED )        SEC, ROOT LDA, 1 # SBC, ROOT   
 STA, IFCC, ROOT 1+ DEC, THEN,   DEX, D0 C, B1 C, ROOT 1+ ROR,  
 ROOT ROR, RTS, C;                                              
                                ( THESE HAVE BEEN TESTED     )  
                                                                
                                                                
                             -->                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 032
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 033
( PLOTTING STUFF )                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                             -->                                
                                                                
( PLOTTING )         HEX         CODE GETCOL                    
 HTORRN LDA,                     IFEQ, ( dither ) OLDX LDA, 7 # 
 AND, N STA, OLDY LDA, 7 # AND,  ASL.A, ASL.A, ASL.A, N ORA,    
 TAX, MCOL1 LDY, THRESH ,X LDA,  VALUE CMP, IFMI, MCOL2 LDY,    
 THEN, COLTB2 ,Y LDA, COLR STA,  RTS, THEN,                     
 IFMI, ( GTIA ) VALUE LDA,       LSR.A, LSR.A, TAY, COLTB2 ,Y   
 LDA, COLR STA, RTS, THEN,       ( random ) MCOL1 LDY, D20A LDA,
 LSR.A, LSR.A, VALUE CMP,                                    -->
════════════════════════════════════════════════════════════════   SCREEN 034
( PLOTTING  )                    IFMI, MCOL2 LDY, THEN,         
 COLTB2 ,Y LDA, COLR STA, RTS,   C;                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
( QPLT ) HEX                     CODE QPLT ' BDS? JSR, FF # CMP,
 IFEQ, RTS, THEN, CLC,          SCLO ,Y LDA, BKOFF ADC, SCRN    
 STA, SCHI ,Y LDA, BKOFF 1+ ADC, SCRN 1+ STA, YOFF ,X LDY,      
 ?PLT LDA, IFMI, ( xor ) COLR     LDA, ONMSK ,X AND, SCRN )Y    
 EOR, SCRN )Y STA, RTS, THEN,    SCRN )Y LDA, OFFMSK ,X AND,    
 SCRN )Y STA, COLR LDA, ONMSK ,X AND, SCRN )Y ORA,              
 SCRN )Y STA, RTS, C;                                           
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 035
( PLTSHD ) DCX                   CODE PLTSHD YPLT LDY, INY,     
 MLPLER STY, 204 # LDA, MLPCND   STA, ' MULT JSR, YPLT STA, SEC,
 191 # LDA, YPLT SBC, YPLT STA,  GMODE LDY, IFEQ, YPLT LSR,     
 THEN, YPLT LDA, OLDY STA,       XPLT LDA, XPL2 STA, XPLT 1+    
 LDA, XPL2 1+ STA,               GMODE LDA, 3 # AND, IFNE,      
       CLC, XPL2 1+ LSR, XPL2    ROR, THEN,                     
                                                             -->
                                                                
( PLOT TESTER )                               CLC,     XPL2 1+  
 LSR, XPL2 ROR,                  XPL2 LDA,  OLDX STA, ' GETCOL  
 JSR, ' QPLT JMP, C;                                            
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 036
( INITIALIZE )                  DCX                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 037
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
( LDS ) DCX                     : LDS ( ADR SEC # ... )         
  O+S DO DUP I 1 R/W 128 +        LOOP DROP ;                   
                                 VAR DTH?                       
                                                                
: GTDTH FLSTK DTH? 644 + 1 R/W    FLSTK 128 + SHPMEN 816 CMOVE  
  FLSTK      THRESH 64 CMOVE           ;                        
                             -->                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 038
( DLI TABLES )                                                  
 HEX                             LABEL GPR?                     
 0 C, 40 C, 80 C, C0 C, 0 C,     LABEL DLS?                     
 2 C, 4 C, 4 C, 4 C, 6 C,                                       
 LABEL NEWDL ASSEMBLER           PHA, TXA, PHA, TYA, PHA,       
 0 # LDA, A2 # LDX, AC # LDY,    D40A STA, D01B STA,            
      D017 STY, D018 STX, PLA,   TAY, PLA, TAX, PLA, RTI,       
 FORTH                                                       -->
( TURN ON GR )                  : INIDSP 0 22F C! 2 PAUSE       
                  8C20 DUP GMODE  DLS? + C@ 2 LDS 230 !         
  TXTWND  8 2 LDS                 GMODE GPR? + C@ 26F C!        
  NEWDL 200 ! C0 D40E C!          22 22F C!                     
        BRINIT DFSET               GMODE IF  2000 ELSE  10F0    
   THEN TO BKOFF 0 TO OLDST ;                                   
( THIS WORKS !! )               ( -1000 -1F10 --- 2000 10F0 )   
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 039
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 040
( GETZ )  HEX                    CODE GETZ RADIUS LDA, ARG STA, 
 ' SQUARE JSR, TNTMP 1+ STA,     SQR LDA, TNTMP STA, XSHD LDA,  
 ARG STA, ' SQUARE JSR, SEC,     TNTMP LDA, SQR SBC, TNTMP STA, 
 TNTMP 1+ LDA, SQR 1+ SBC, TNTMP 1+ STA, YSHD LDA, ARG STA,     
 ' SQUARE JSR, SEC, TNTMP LDA,   SQR SBC, RADCND STA, TNTMP 1+  
 LDA, SQR 1+ SBC, RADCND 1+ STA, IFPL, ' SQRT JSR, ROOT ASL,    
 ROOT 1+ ROL, RTS, THEN,         0 # LDA, ROOT STA, ROOT 1+ STA,
 RTS, C;                                                     -->
( SDIV GETVAL )                  CODE SDIV 0 # LDA, DVSOR 1+    
 STA, ' DIVIDE JMP, C;                                          
 CODE GETVAL TONE 1+ BIT, IFMI,  BAKLIT LDA, IFEQ, VALUE STA,   
 RTS, THEN, SEC, 0 # LDA, TONE   SBC, TONE STA, THEN, TONE LDA, 
 MLPCND STA, 1A # LDA, MLPLER    STA, ' MULT JSR, DVDND 1+ STA, 
 PROD LDA, DVDND STA, RADIUS     LDA, DVSOR STA, ' SDIV JSR,    
 QUOT LDA, VALUE STA, RTS, C;                                   
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 041
( PTPLOT -- MAIN PLOTTER )      CODE PTPLOT HVFLAG BIT,         
 IFMI, XREL LDA, PHA, PHA, YREL  LDA, XREL STA, PLA, YREL STA,  
 XSHD LDA, PHA, PHA, YSHD LDA,   XSHD STA, PLA, YSHD STA, XSHD  
 1+ LDA, PHA, PHA, YSHD 1+ LDA,  XSHD 1+ STA, PLA, YSHD 1+ STA, 
 THEN, ' GETZ JSR,               HERE PTPLT2 !                  
 1 # LDA, HEMI STA, SEC, CLIPL   LDA, XREL CMP, IFCS, SEC,      
 ROOT LDA, XSHD SBC, ZWX STA,    ROOT 1+ LDA, XSHD 1+ SBC, ZWX  
 1+ STA, SEC, XCENT LDA, XREL                                -->
( PTPLOT )                       SBC, XPLT STA, XCENT 1+ LDA,   
 0 # SBC, XPLT 1+ STA,           HERE CHCLUP !                  
 SEC, CLIPU LDA, YREL CMP,       IFCS, CLC, ZWX LDA, YSHD ADC,  
 TONE STA, ZWX 1+ LDA, YSHD 1+   ADC, TONE 1+ STA, ' GETVAL JSR,
 CLC, YCENT LDA, YREL ADC, YPLT  STA, ' PLTSHD JSR,  THEN, SEC, 
 CLIPD LDA, YREL CMP, IFCS, SEC, ZWX LDA, YSHD SBC, TONE STA,   
 ZWX 1+ LDA, YSHD 1+ SBC, TONE   1+ STA, ' GETVAL JSR, SEC,     
 YCENT LDA, YREL SBC, YPLT STA,                              -->
════════════════════════════════════════════════════════════════   SCREEN 042
( PTPLOT )                       ' PLTSHD JSR, THEN, THEN,      
 HEMI LDA, IFNE, HEMI DEC, SEC,  CLIPR LDA, XREL CMP, IFCS,     
 CLC, ROOT LDA, XSHD ADC, ZWX    STA, ROOT 1+ LDA, XSHD 1+ ADC, 
 ZWX 1+ STA, CLC, XCENT LDA,     XREL ADC, XPLT STA, XCENT 1+   
 LDA, 0 # ADC, XPLT 1+ STA,      CHCLUP @ JMP, THEN,            
 THEN, HVFLAG BIT, IFMI, XSHD 1+ LDA, YSHD 1+ STA, PLA, XSHD 1+ 
 STA, XSHD LDA, YSHD STA, PLA,   XSHD STA, XREL LDA, YREL STA,  
 PLA, XREL STA, THEN, RTS, C;                                -->
( TPARM )                                                       
 CODE TPARM                      RO LDA, SEC, RI SBC, LSR.A,    
 RT STA, RADIUS STA, CLC, RI     ADC, RC STA, RT LDA, ARG STA,  
 ' SQUARE JSR, SQR LDA, RS STA,  SQR 1+ LDA, RS 1+ STA, 0 # LDA,
 CNTX STA, RTS, C;                                              
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 043
( SPHERE )                       CODE SPHER                     
 RADIUS LDA, ARG STA, ' SQUARE   JSR, SQR ASL, SQR 1+ ROL, SQR  
 LDA, RADCND STA, SQR 1+ LDA,    RADCND 1+ STA, ' SQRT JSR,     
 ROOT 1+ LSR, ROOT ROR, ROOT     LDA, XMAXX STA, 0 # LDA, CNTX  
 STA, XSHD 1+ STA, YSHD 1+ STA,  RADIUS LDA, ARG STA, ' SQUARE  
 JSR, TEM2 1+ STA, SQR LDA,      TEM2 STA,                      
                                                                
                                                             -->
( SPHERE CONTINUED )             BEGIN, CNTX LDA, CNTY STA, ARG 
 STA, XREL STA, XSHD STA,        ' SQUARE JSR, SEC, TEM2 LDA,   
 SQR SBC, RADCND STA, TEM2 1+    LDA, SQR 1+ SBC, RADCND 1+ STA,
 ' SQRT JSR, ROOT LDA, MAXX STA,  BEGIN, CNTY LDA, YREL STA,    
  YSHD STA, 0 # LDA, HVFLAG STA,  ' PTPLOT JSR, 80 # LDA,       
  HVFLAG STA, ' PTPLOT JSR,       CNTY LDA, MAXX CMP,           
  NE WHILE, CNTY INC,             REPEAT, CNTX LDA, XMAXX CMP,  
 NE WHILE, CNTX INC, REPEAT,     RTS, C;                     -->
════════════════════════════════════════════════════════════════   SCREEN 044
( CYLNDR )                                                      
 CODE CYLNDR                     0 # LDA, XSHD STA, XSHD 1+ STA,
 RADIUS LDA, YREL STA,           BEGIN, HLEN LDA, XREL STA, YREL
 LDA, YSHD STA, BEGIN, ' PTPLOT   JSR, XREL DEC, MI UNTIL,      
  YREL DEC, MI UNTIL, RTS, C;                                   
                                                                
                                                                
                                                             -->
( EDGTOR  )            HEX       CODE EDGTR                     
 ' TPARM JSR, 0 # LDA,  XSHD     STA, XSHD 1+ STA,              
 BEGIN, CNTX LDA, XREL STA, XSHD  STA, ARG STA, ' SQUARE JSR,   
 SEC, RS LDA, SQR SBC, RADCND    STA, RS 1+ LDA, SQR 1+ SBC,    
 RADCND 1+ STA, ' SQRT JSR,      ROOT LDA, R0 STA, CLC, RC ADC, 
 MAXX STA, 0 # LDA, CNTY STA,    BEGIN, CNTY LDA, YREL STA,     
 MLPLER STA, R0 LDA, MLPCND STA, ' MULT JSR, DVDND 1+ STA, PROD 
LDA, DVDND STA, MAXX LDA, DVSOR                              -->
════════════════════════════════════════════════════════════════   SCREEN 045
( EDGTOR )                       STA, ' SDIV JSR, QUOT LDA,     
 YSHD STA, ' PTPLOT JSR,         CNTY LDA, MAXX CMP,            
 NE WHILE, CNTY INC,             REPEAT, CNTX LDA, RT CMP,      
 NE WHILE, CNTX INC,             REPEAT, RTS, C;                
                                                                
                                                                
                                                                
                                                             -->
( TOROID TOP VIEW )                                             
 CODE TOROID ' TPARM JSR, RO     LDA, ARG STA, ' SQUARE JSR,    
 SQR ASL, SQR 1+ ROL, SQR LDA,   RADCND STA, SQR 1+ LDA,        
 RADCND 1+ STA, ' SQRT JSR,      ROOT 1+ LSR, ROOT ROR, ROOT    
 LDA, XMAXX STA, BEGIN, CNTX     LDA,   XREL STA, ARG STA,      
 ' SQUARE JSR, XSQR 1+ STA,      SQR LDA, XSQR STA, RO LDA,     
 ARG STA, ' SQUARE JSR, SEC,    SQR LDA, XSQR SBC, RADCND STA,  
 SQR 1+ LDA, XSQR 1+ SBC,                                    -->
════════════════════════════════════════════════════════════════   SCREEN 046
( TOROID )                       RADCND 1+ STA, ' SQRT JSR,     
 ROOT LDA, MAXX STA, SEC,        RI LDA, CNTX SBC,              
 IFCS, RI LDA, ARG STA, ' SQUARE JSR, SEC, SQR LDA, XSQR SBC,   
 RADCND STA, SQR 1+ LDA, XSQR 1+ SBC, RADCND 1+ STA, ' SQRT JSR,
 ROOT LDA, CNTY STA,             ELSE, CNTX LDA, CNTY STA,      
 THEN,                           BEGIN, CNTY LDA, YREL STA, ARG 
 STA, ' SQUARE JSR, CLC, SQR     LDA, XSQR ADC, RADCND STA,     
 SQR 1+ LDA, XSQR 1+ ADC,                                    -->
( TOROID )                       RADCND 1+ STA, ' SQRT JSR,     
 ROOT LDA, R0 STA, DVSOR STA,    CNTX LDA, MLPLER STA, RC LDA,  
 MLPCND STA, ' MULT JSR,         DVDND 1+ STA, PROD LDA,        
 DVDND STA, ' SDIV JSR, SEC,     CNTX LDA, QUOT SBC, XSHD STA,  
 0 # LDA, QUOT 1+ SBC, XSHD 1+   STA, CNTY LDA, MLPLER STA, RC  
 LDA, MLPCND STA, ' MULT JSR,    DVDND 1+ STA, PROD LDA,        
 DVDND STA, R0 LDA, DVSOR STA,   ' SDIV JSR, SEC, CNTY LDA,     
 QUOT SBC, YSHD STA, 0 # LDA,                                -->
════════════════════════════════════════════════════════════════   SCREEN 047
( TOROID )                       HVFLAG STA, QUOT 1+ SBC, YSHD  
 1+ STA, ' PTPLOT JSR,  80 #     LDA, HVFLAG STA,               
 ' PTPLOT JSR, CNTY LDA, MAXX    CMP,                           
NE WHILE, CNTY INC,              REPEAT, CNTX LDA, XMAXX CMP,   
 NE WHILE, CNTX INC,             REPEAT, RTS, C;                
                                                                
                                                                
                                                             -->
( SPOOL )                       CODE SPUUL ' TPARM  JSR,        
 BEGIN, CNTX LDA, XREL STA,      ARG STA, SEC, 0 # LDA, CNTX    
 SBC, XSHD STA, 0 # LDA, 0 #     SBC, XSHD 1+ STA, ' SQUARE     
 JSR, SEC, RS LDA, SQR SBC,      RADCND STA, RS 1+ LDA, SQR 1+  
 SBC, RADCND 1+ STA, ' SQRT JSR, SEC, RC LDA, ROOT SBC, MAXX    
 STA, 0 # LDA, CNTY STA,         BEGIN, CNTY LDA, YREL STA,     
 MLPLER STA, RC LDA, MLPCND STA,  ' MULT JSR, DVDND 1+ STA,     
 PROD LDA, DVDND STA, MAXX LDA,                              -->
════════════════════════════════════════════════════════════════   SCREEN 048
( SPOOL )                        DVSOR STA, ' SDIV JSR, QUOT    
 LDA, SEC, CNTY SBC, YSHD STA,   QUOT 1+ LDA, 0 # SBC, YSHD 1+  
 STA, ' PTPLOT JSR, CNTY LDA,    MAXX CMP,                      
 NE WHILE, CNTY INC,             REPEAT, CNTX LDA, RT CMP,      
 NE WHILE, CNTX INC,             REPEAT, RTS, C;                
                                                                
                                                                
                                                             -->
( DEFAULTS   )                    DCX  -->                      
 CODE DSPHERE                    XSAVE STX, ' SPHER  JSR,       
 XSAVE LDX, NXT, C;                                             
 CODE DCYLNDR                    XSAVE STX, ' CYLNDR JSR,       
 XSAVE LDX, NXT, C;                                             
 CODE DEDGTOR                    XSAVE STX, ' EDGTR JSR,        
 XSAVE LDX, NXT, C;                                             
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 049
( DEFAULTS )                     -->                            
 CODE DTORUS                     XSAVE STX, ' TOROID JSR,       
 XSAVE LDX, NXT, C;                                             
 CODE DSPOOL                     XSAVE STX, ' SPUUL  JSR,       
 XSAVE LDX, NXT, C;                                             
                                                                
                                                                
                                                             -->
( SET UP PARAMS FOR CURSORS )   DCX                             
 LABEL GTPRMS 20 ALLOT                                          
                                                                
                                                                
                             -->                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 050
( PARAMS HORZ ) ASSEMBLER       HERE GTPRMS !                   
CLC, RO LDA, RI ADC, ROR.A, HORZ STA, HORZ 1+ STA, RTS,         
HERE GTPRMS 2 + !                RO LDA, HORZ STA, HORZ 1+ STA, 
 RTS,                           HERE GTPRMS 4 + !               
 RI LDA, HORZ 2 + STA, HORZ 3 +  STA, RTS,                      
HERE GTPRMS 6 + !                SEC, RO LDA, RI SBC, LSR.A,    
 HORZ STA, HORZ 1+ STA, RTS,      HERE GTPRMS 8 + !             
 RI LDA, HORZ STA, HORZ 1+ STA,  RTS,                        -->
( PARAMS HORIZONTAL )           HERE GTPRMS 10 + !              
CLC, RO LDA, RI ADC, ROR.A, VERT STA, VERT 1+ STA, RTS,         
HERE GTPRMS 12 + !               RO LDA, VERT STA, VERT 1+ STA, 
 RTS,                           HERE GTPRMS 14 + !              
 RI LDA, VERT 2 + STA, VERT 3 +  STA, RTS,                      
HERE GTPRMS 16 + !               SEC, RO LDA, RI SBC, LSR.A,    
 VERT STA, VERT 1+ STA, RTS,     HERE GTPRMS 18 + !             
 RI LDA, VERT STA, VERT 1+ STA,  RTS,                        -->
════════════════════════════════════════════════════════════════   SCREEN 051
( OBJECT LISTS )                 FORTH                          
LABEL OBPRMS                     2 C, 1 C, 6 C, 0 , ( SPH )     
 4 C, 1 C, 6 C, 2 C, 7 C, ( TOR) 2 C, 1 C, 9 C, 0 , ( VCYL )    
 2 C, 4 C, 6 C, 0 , ( HCYL )     3 C, 8 C, 0 C, 2 C, 0 C, ( VSP)
 3 C, 3 C, 5 C, 7 C, 0 C, ( HSP) 3 C, 8 C, 2 C, 1 C, 0 C, ( VED)
 3 C, 3 C, 7 C, 6 C, 0 C, ( HED)                                
                                                                
                                                             -->
( ONEPRM DO-OBJ )                CODE ONEPRM  ASL.A, TAY,       
 GTPRMS ,Y LDA, HERE 10 + STA,   GTPRMS 1+ ,Y LDA, HERE 5 + STA,
 HERE JMP, C;                                                   
 CODE DO-OBJ ( # ... ) 0 ,X LDA, N STA, ASL.A, ASL.A, CLC, N    
 ADC, XSAVE STX, TAX, OBPRMS ,X  LDA, N STA,                    
 BEGIN, INX, OBPRMS ,X LDA,      ' ONEPRM JSR, N DEC,           
 EQ UNTIL, XSAVE LDX, POP JMP,   C;                             
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 052
( SCALING TO REAL SCREEN )       CODE SCLY 0 ,X LDY, XSAVE STX, 
 INY, MLPLER STY, 204 # LDA,     MLPCND STA, ' MULT JSR,        
 CLC, GMODE LDY, IFEQ, LSR.A,    THEN, XSAVE LDX,               
       0 ,X STA, NEXT JMP, C;                                   
 CODE SCLX CLC, 1 ,X LSR,        0 ,X ROR,                      
 GMODE LDA, 3 # AND, IFNE,       0 ,X LSR, THEN, NEXT JMP, C;   
                                                                
                                                             -->
( NORMALIZING FOR CURSOR )      ( : CHGSGN   addr ... )         
(  DUP C@ 0 SWAP - SWAP C! ;  )  : LES ( addr adrclip ... )     
   C@ OVER C@ MIN SWAP C! ;                                     
 : CLIPM                                 VERT CLIPU LES VERT 2 +
 CLIPU LES VERT 1+ CLIPD LES     VERT 3 + CLIPD LES             
         HORZ CLIPL LES HORZ 2 + CLIPL LES HORZ 1+ CLIPR LES    
 HORZ 3 + CLIPR LES ;                                           
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 053
( CCHK )                                                        
  LABEL TCLP 8 ALLOT             : CCHK CLIPL TCLP 8 CMOVE      
        319 XCENT @ - CLIPR @      MIN CLIPR ! XCENT @ CLIPL @  
   MIN CLIPL ! YCENT @ CLIPD @     MIN CLIPD ! 239 YCENT @ -    
   CLIPU @ MIN CLIPU ! ;         : OLCLP TCLP CLIPL 8 CMOVE ;   
 : CURPARM OBJ#                    VERT 4 ERASE HORZ 4 ERASE    
   CCHK DO-OBJ CLIPM OLCLP                      4 0 DO I VERT + 
                                                             -->
( CURPARM )                                                     
   DUP C@ SCLY SWAP C! I HORZ +    DUP C@ SCLX SWAP C! LOOP     
   XCENT @ SCLX TO XCR             GMODE IF 191 ELSE 95 THEN    
   YCENT C@ SCLY - TO YCR          2 0 DO                       
 I VERT + C@ I VERT 2 + + C@ =   IF 0 VERT 2 + I + C! THEN      
 I HORZ + C@ I HORZ 2 + + C@ =   IF 0 HORZ 2 + I + C! THEN      
      LOOP  ;                -->                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 054
( INCREMENTING STUFF )                                          
 CODE STFIT ( b1 b2 b3 addr ..)  0 ,X LDA, N STA, 1 ,X LDA,     
 N 1+ STA, 2 # LDY, INX, INX,    BEGIN, 0 ,X LDA, 16 # ORA,     
 N )Y STA, INX, INX, DEY, MI     UNTIL, NEXT JMP, C;            
 100 CONSTANT 100                10 CONSTANT 10                 
 : (PTN)  ( num addr ... )         >R 100 /MOD SWAP 10 /MOD SWAP
   R> STFIT ;                                                   
                             -->                                
( CURSORS )                      CODE ACUR                      
 OLDX INC, ' QPLT JSR, OLDX DEC, OLDY INC, ' QPLT JSR, OLDY DEC,
 OLDX DEC, ' QPLT JSR, OLDX INC, OLDY DEC, ' QPLT JSR, OLDY INC,
 RTS, C;                                                        
                                                                
                                                             -->
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 055
( CURSORS )  -->                 CODE 9CUR XSAVE STX, 128 # LDA,
 ?PLT STA, 255 # LDA, COLR            STA, XCR LDA, OLDX STA,   
 YCR LDA, OLDY STA, ' ACUR JSR,  1 # LDA, N 2 + STA,            
 BEGIN, N 2 + LDY, CLC, HORZ ,Y  LDA, IFNE, XCR ADC, OLDX STA,  
 YCR LDA, OLDY STA, ' ACUR JSR,  THEN, N 2 + LDY, CLC, VERT ,Y  
 LDA, IFNE, YCR ADC, OLDY STA,   XCR LDA, OLDX STA, ' ACUR JSR, 
 THEN, N 2 + DEC, MI UNTIL,                                  -->
                                                                
( CCHK )                          130 LOAD                      
 : FIXRI OBJ# 1 = OBJ# 3 > OR      IF RO @ RI @ MIN RI ! THEN ; 
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 056
 ( SOME WORDS FOR ADDRESSING )                                  
  : TCADR TCY 28*  TCX 9 * +        TXTWND + ;                  
  : TC>PR TCY TCX 4 * + 2*          XCENT +   ;                 
  : PUTPAR TC>PR @ TCADR 5 +        (PTN) ;                     
  VAR #CMD                                                      
                                                             -->
                                                                
                                                                
(   UTILITY WORDS         )      : UHOH BUZZ KEY 32 = ;         
                                 : DOMB ( inc mod addr ... )    
   ROT >R OVER R> + OVER @ +       ROT MOD SWAP ! ;             
 : NEWCMD IF 2DROP THEN            16  TO #CMD ;                
 : CLRSC NEWCMD  UHOH IF 9CUR      PBUFPT 7680 ERASE 9CUR       
         THEN ;                  : DOMN ( inc max addr ... )    
   ROT OVER @ + ROT MIN 0 MAX       SWAP ! ;                    
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 057
( BLOCK MOVES ) DCX              : SETSAV XCR HORZ C@ - PXB /   
 PXB *  TO XOLD YCR VERT C@        - 1- 0 MAX  TO YOLD          
  XCR HORZ 1+ C@ + XOLD - PXB /  3 + 40 MIN                     
     TO BRHZ YCR VERT 1+ C@ + 1+  YOLD - 1+ TO BRVT XOLD PXB /  
 1- 0 MAX TO XOLD   YOLD 28*         PBUFPT +  AT XOLD +! ;     
 : BLKMV     BRVT 0 DO DUP                                   I  
   28* XOLD + FLSTK I BRHZ * +     ROT IF SWAP THEN BRHZ CMOVE  
   LOOP DROP ;                                               -->
( MAKE OBJ      )                                               
 LABEL (OBEX)                    ' SPHER , ' TOROID , ' CYLNDR  
  DUP , , ' SPUUL DUP , ,        ' EDGTR DUP , ,                
                                                                
 CODE MKOBJ  0 ,X ASL, 0 ,X LDY, XSAVE STX,                     
 (OBEX) ,Y LDA, HERE 10 + STA,   (OBEX) 1+ ,Y LDA, HERE 5 + STA,
 HERE JSR, XSAVE LDX, POP JMP,   C;                             
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 058
( OKAY ... )                     : HV? OBJ# 1 AND OBJ# 2 < OR   
   IF 0 ELSE 128 THEN HVFLAG       C! ;                         
: OKAY NEWCMD  9CUR FIXRI              RO @ RADIUS ! RI @ HLEN  
  C! HV? SETSAV BRVT BRHZ *       2048 < DUP NOT IF DROP UHOH   
  ELSE 0 BLKMV 1 TO OLDST THEN    IF CCHK  OBJ# MKOBJ OLCLP     
          THEN    9CUR ;         : OOPS NEWCMD 9CUR             
        UHOH OLDST AND IF          1 BLKMV 0 TO OLDST THEN      
   9CUR ;                        140 LOAD                       
( INCREMENTING PARAMS )          : NMFLG <BUILDS , DOES> @ 1 ;  
                                 : Z01 0 1 ;                    
                                                                
                                 10 NMFLG X+10 -10 NMFLG X-10   
 1  NMFLG X+1   -1 NMFLG X-1    100 NMFLG X+100 -100 NMFLG X-100
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 059
( GETINC )                       : GETINC SPAUSE                
   GTKY                            SEL                          
   6 -> X-1  7 -> X+1             14 -> X+10 15 -> X-10         
  70 -> X-100 71 -> X+100         34 -> 0                       
   NOSEL Z01                      SELEND ;                      
                                                                
                                                                
                                                             -->
( CCHK )                                                        
                                 : PXC BEGIN GETINC WHILE -DUP  
   IF 9CUR 320 XCENT DOMB          PUTPAR CURPARM 9CUR THEN     
   REPEAT ;                                                  -->
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 060
( PYC )                          : PYC BEGIN GETINC WHILE -DUP  
   IF 9CUR 256 YCENT DOMB          PUTPAR CURPARM 9CUR THEN     
   REPEAT ;                                                     
 : PRO BEGIN GETINC WHILE -DUP     IF 9CUR 256 RO DOMB FIXRI    
   PUTPAR CURPARM 9CUR THEN        REPEAT ;                     
                                 : PRI BEGIN GETINC WHILE -DUP  
   IF 9CUR 256 RI DOMB FIXRI       PUTPAR CURPARM 9CUR THEN     
   REPEAT ;                                                  -->
( CLIPS ... )                                                   
 : CLIPS BEGIN GETINC WHILE -DUP   IF 9CUR 256 TC>PR DOMB       
   PUTPAR CURPARM 9CUR             THEN                         
   REPEAT ;                      : CHCIN GTKY DUP 7 = ;         
 : CHOUT SPAUSE 34 = ;                                          
 : (LIT) BEGIN CHCIN IF             BAKLIT C@ 1+ 2 MOD DUP      
    BAKLIT C! 3 * LITMEN +          TCADR 5 + 3 CMOVE THEN      
    CHOUT UNTIL ;                                            -->
════════════════════════════════════════════════════════════════   SCREEN 061
( SHAPE .. ) VAR SHD?            : (SHDS)                       
   BEGIN CHCIN IF SHD? 1+ 3 MOD  DUP TO SHD? DUP 64 * HTORRN C! 
   3 *                             SHDMEN + TCADR 5 + 3 CMOVE   
   THEN CHOUT                      UNTIL ;                      
                                 : (SHPS) BEGIN CHCIN           
   IF OBJ# 1+ 8 MOD TO OBJ#        OBJ# 6 * SHPMEN + TCADR 6 +  
   6 CMOVE THEN                    CHOUT                        
   UNTIL ;                                                   -->
( COLR ...)                      : (COLR?)                      
   BEGIN CHCIN IF TCY 1 AND IF     MCOL1 1+ NCLRS MOD DUP       
TO MCOL1 ELSE MCOL2 1+ NCLRS       MOD DUP TO MCOL2 THEN        
   TCADR 5 + (PTN)                 THEN                         
   CHOUT                           UNTIL ;                      
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 062
( TEST CURSOR )                  : TCUR TCADR DUP C@ 128 XOR    
   SWAP C! ;                                                    
 : INCR TCUR OVER @ + 4 MOD        SWAP ! TCUR ;                
                                 : TCR AT TCX 1 INCR ;          
 : TCL AT TCX 3  INCR ;          : TCU AT TCY 3 INCR ;          
 : TCD AT TCY 1 INCR ;                                          
                                                                
                                                             -->
( 3DCOMS )                       LABEL 3DCOM                    
 CF, PXC  CF, PYC CF, PRO        CF, PRI CF, CLIPS CF, CLIPS    
 CF, CLIPS CF, CLIPS CF, (COLR?) CF, (COLR?) CF, (SHDS)         
 CF, (LIT) CF, CLRSC             CF, OOPS CF, OKAY CF, (SHPS)   
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 063
( INITIALIZE )                   : INI3P                        
   255 CLIPL ! 255 CLIPR !         255 CLIPU ! 255 CLIPD !      
   0 TO OBJ# 0 TO SHD? 0 HTORRN    C! 0 TO TCY 0 TO TCX         
   60 RO ! 160 XCENT ! 120         YCENT ! 30 RI !  YND YMAX C! 
   XEND XMAX C! ;                                               
 : 3DOT TCY TCX 4 * + 2* 3DCOM     + @EX ;                      
                                 : INI3D INI3P CURPARM INIDSP   
  TCUR 9CUR ;                                                -->
( LAST WORD )                                                   
 : 3DRAW INI3D                      BEGIN GTKY                  
    SEL 7 -> TCR                        6 -> TCL                
       14 -> TCU                       15 -> TCD                
       34 -> 3DOT                    SELEND                     
   20 PAUSE                      ?TERMINAL UNTIL ;              
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 065
                                 CODE 9CUR XSAVE STX, 128 # LDA,
 ?PLT STA, 255 # LDA, COLR            STA, XCR LDA, OLDX STA,   
 YCR LDA, OLDY STA, ' ACUR JSR,  2 # LDA, N 2 + STA,            
 BEGIN, N 2 + LDY, CLC,          HORZ 1+ ,Y LDA, IFNE, XCR ADC, 
 IFCC, OLDX STA, YCR LDA, OLDY   STA, ' ACUR JSR, THEN, THEN,   
 N 2 + LDY,                      SEC, HORZ ,Y LDA, IFNE, XCR    
 LDA, HORZ ,Y SBC, IFCS, OLDX    STA, YCR LDA, OLDY STA, ' ACUR 
 JSR, THEN, THEN, CLC,           N 2 + LDY,                  -->
( 9CUR )                         VERT 1+ ,Y LDA, IFNE, YCR ADC, 
 IFCC, OLDY STA, XCR LDA, OLDX   STA, ' ACUR JSR, THEN, THEN,   
 N 2 + LDY,                      SEC, VERT ,Y LDA, IFNE, YCR    
 LDA, VERT ,Y SBC, IFCS, OLDY    STA, XCR LDA, OLDX STA, ' ACUR 
 JSR, THEN, THEN,                N 2 + DEC, N 2 + DEC, MI UNTIL,
 0 # LDA, ?PLT STA, XSAVE LDX,   NEXT JMP, C;                   
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 066
( DITHERING COMMANDS )  DCX                                     
: DTH 9CUR SHPMEN FLSTK 128 +    816 CMOVE   0 TO OLDST         
  0 GR. 5 0 DO I 640 .LN CR LOOP  CR ." Pick 0-4 "              
  KEY 48 - DUP 0 >= OVER 5 <      AND NOT IF DROP 0 THEN        
  TO DTH? INIDSP GTDTH 9CUR       MYCOL ;                       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 070
( QUIKDRW ) VAR ?BUT             CODE GETSTK ( ... h v )        
 DEX, DEX, 632  LDA, DEX, DEX,                       0 ,X STY,  
 1 ,X STY, 2 ,X STY, 3 ,X STY,   ROR.A, IFCC, 0 ,X INC, THEN,   
 ROR.A, IFCC, 0 ,X DEC,  1 ,X    DEC, THEN,  ROR.A, IFCC, 2 ,X  
 DEC, 3 ,X DEC, THEN, ROR.A,     IFCC, 2 ,X INC, THEN,          
 644 LDA, 1 # EOR, ?BUT STA,      NEXT JMP, C;                  
                                                                
                                                             -->
                                 CODE NOMV      0 ,X LDA, 2 ,X  
 ORA, IFNE, 1 # LDA, 77 STY,                         PSHA,      
 THEN, 3 ,X STY, 2 ,X STY, POP   JMP, C;                        
                                                                
                                                                
                                                                
                                                             -->
                                                                
════════════════════════════════════════════════════════════════   SCREEN 071
( CENTR  )                        VAR DST                       
 : CENTR ( dx dy f ... )           IF 9CUR DST * 240 YCENT      
   DOMB DST * 320 XCENT DOMB       XCENT @ TXTWND 5 + (PTN)     
   YCENT @ TXTWND 45 + (PTN)       CURPARM 9CUR THEN ;          
                                                                
                                                                
                                                                
                                                             -->
( PRO, PRI )                                                    
 : PRO IF                             9CUR DROP DST * 180 RO    
   DOMN   PUTPAR CURPARM 9CUR      THEN  ;                      
                                 : PRI IF                       
      9CUR DROP DST * 180 RI       DOMN   PUTPAR CURPARM 9CUR   
   THEN ;                                                       
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 072
( CLIPS [LIT] )                                                 
 : CLIPS IF                           9CUR DROP DST * 255       
    TC>PR DOMN PUTPAR CURPARM        9CUR                       
   THEN  ;                                                      
 : (LIT) IF   DROP IF BAKLIT C@    1+ 2 MOD DUP BAKLIT C! 3 *   
   LITMEN + TCADR 5 + 3 CMOVE      THEN THEN ;                  
                                                                
                                                             -->
( SHD SHP   )                    : (SHPS) IF DROP               
   IF 9CUR                            OBJ# 1+ 8 MOD TO OBJ#     
   OBJ# 6 * SHPMEN + TCADR 6 +     6 CMOVE CURPARM  9CUR  THEN  
   THEN ;                           137 LOAD                    
                                                             -->
 VAR SHD?                        : (SHDS) IF DROP IF SHD? 1+ 3  
   MOD DUP TO SHD? DUP 64 *        HTORRN C! 3 * SHDMEN + TCADR 
   5 + 3 CMOVE THEN THEN ;                                      
════════════════════════════════════════════════════════════════   SCREEN 073
 ( COLR? )                                                      
 : (COLR?) IF DROP IF               TCY 1 AND                   
    IF MCOL1 1+ NCLRS MOD DUP          TO MCOL1                 
    ELSE MCOL2 1+ NCLRS MOD DUP        TO MCOL2                 
    THEN                            TCADR 5 + (PTN)             
    THEN THEN ;                                                 
                                                                
                                                             -->
( TXT COMMANDS )                                                
 : TCUR TCADR DUP C@ 128 XOR       SWAP C! ;                    
 : INCR IF TCUR MINUS                           TCY + 4 + 4 MOD 
   TO TCY TCX + 4 + 4 MOD          TO TCX TCUR THEN ;           
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 074
( 3DCOMS )                       LABEL 3DCOM                    
 CF, CENTR CF, CENTR  CF, PRO    CF, PRI CF, CLIPS CF, CLIPS    
 CF, CLIPS CF, CLIPS CF, (COLR?) CF, (COLR?) CF, (SHDS)         
 CF, (LIT) CF, CLRSC             CF, OOPS CF, OKAY CF, (SHPS)   
 CF, INCR                                                       
  : CMDEX #CMD 2* 3DCOM  + @EX ;                                
                                                                
                                                             -->
( INCDST )                                                      
 : INCDST CLICK 20 PAUSE           DST 1 = IF 10                
   ELSE DST 10 = IF 50             ELSE 1 THEN THEN TO DST      
   DST TXTWND 37 + (PTN) ;                                      
 : 3DDT 1 TO ?BUT ;              : TCR 2DROP 1 0 ;              
 : TCL 2DROP -1 0 ;              : TCU 2DROP 0 1 ;              
 : TCD 2DROP 0 -1 ;                                             
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 075
( MAINLOOP )                     : 3DLOOP                       
   BEGIN GETSTK 764 C@ 255 764     C! SEL                       
    58 -> DTH                        7 -> TCR                   
     6 -> TCL                       14 -> TCU                   
    15 -> TCD                       34 -> 3DDT                  
    33 -> INCDST                   SELEND                       
   NOMV                            ?BUT IF CLICK 30 PAUSE       
   0 77 C!      0 TO ?BUT                                    -->
( MAIN LOOP )                    #CMD 16 =  IF TCX 4 * TCY +    
 TO #CMD                         ELSE 16 TO #CMD                
 THEN  THEN                       CMDEX                         
 20 PAUSE                        ?TERMINAL                      
 UNTIL ;                                                        
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 076
( INITIALIZE )                   : INI3P MYCOL ZIPFG VBOFF      
   255 CLIPL ! 255 CLIPR !         255 CLIPU ! 255 CLIPD !      
   0 TO OBJ# 0 TO SHD?                1 TO MCOL1 2 TO MCOL2     
             0 TO TCY 0 TO TCX     60 RO ! 160 XCENT ! 120      
   YCENT ! 30 RI !  YND YMAX C!    XEND XMAX C! 16 TO #CMD      
               FIXCTB ;                                         
 : INI3D INI3P CURPARM INIDSP     TCUR 9CUR 100 TO DST INCDST   
   0 BAKLIT C! ;             -->                                
( LAST WORD )                                                   
 : 3DRAW INI3D                      3DLOOP                      
    9CUR                            0 GR.  STVB 0 TO PDF        
 0 TO P->D 0 TO THFL ;             DCX HERE U. -->              
     HEX                         : VERS? [ ' SCHI  ] LITERAL    
   @ 500 = NOT IF                  ." WRONG VERSION !! "        
   BUZZ 80 PAUSE ELSE              3DRAW THEN ;                 
 DCX                              HERE U.                       
════════════════════════════════════════════════════════════════   SCREEN 077
                                 ' 3DRAW CFA MODST 5 + !        
   DP !                          OFF HEADLESS                   
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
  LABEL APL1                     ' APL0 LFA @ ' APL1 LFA !      
  FORGET APL1                    : SAVIT                        
   0 DO OVER I 128 * + OVER        I + 0 R/W LOOP 2DROP ;       
                                 ." 29400 30 50 SAVIT  "        
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 078
( DITHER STYLES )                LABEL BELLTEL 64 ALLOT         
 : MAKBEL 8 0 DO BELLTEL I 8 *     + 8 I 1+ 7 * FILL LOOP ;     
 : SAVDTH ( ADDR # .. )            644 + 0 R/W ;                
 : 8C, 8 0 DO C, LOOP ;          LABEL SQUARE                   
 49 48 47 46 45 44 43 42 8C,     50 25 24 23 22 21 20 41 8C,    
 51 26 9  8  7  6  19 40 8C,     52 27 10 1  0  5  18 39 8C,    
 53 28 11 2  3  4  17 38 8C,     54 29 12 13 14 15 16 37 8C,    
 55 30 31 32 33 34 35 36 8C,     56 57 58 59 60 61 62 63 8C, -->
( SHADE TABLE ) HEX             LABEL THRESH                    
 3F 37 0A 02 3D 35 08 00 8C,     2F 27 1A 12 2D 25 18 10 8C,    
 0F 06 3B 33 0C 04 39 31 8C,     1E 16 2B 23 1C 14 29 21 8C,    
 3C 34 09 01 3E 36 0B 03 8C,     2C 24 19 11 2E 26 1B 13 8C,    
 0D 05 38 30 0F 07 3A 32 8C,     1D 15 28 20 1F 17 2A 22 8C,    
DCX                                                             
                                                                
                                                             -->
