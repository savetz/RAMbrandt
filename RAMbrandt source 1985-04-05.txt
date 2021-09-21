════════════════════════════════════════════════════════════════   SCREEN 001
( SLO ROTATE )                  : XFORM TO YE TO XE XE PHI -    
 THUE  * YE PHIY - TLUM  * + 128  /   PHI + YE PHIY - THUE * XE 
 PHI - TLUM * - 128 / PHIY + ;  LABEL XCS 8 ALLOT               
LABEL YCS 8 ALLOT               : CRNI XFORM <ROT OVER XCS + !  
  YCS + ! ;                     : 4CRN 0 XOLD YOLD CRNI 2 XOLD  
  BRVT CRNI 4 BRHZ YOLD CRNI 6    BRHZ BRVT CRNI ;              
: XRNG XCS @ DUP 8 0 DO I XCS +   @ SWAP OVER MIN <ROT MAX SWAP 
  2 +LOOP SWAP 1+ XEND MIN SWAP   0 MAX ;                    -->
( SLO ROT )                     : YRNG YCS @ DUP 8 0 DO I YCS + 
  @ SWAP OVER MIN <ROT MAX SWAP   2 +LOOP SWAP 1+ SCLN 1- MIN   
  SWAP YMN MAX ;                CODE INBD 1 ,X LDA, 3 ,X ORA,   
 IFNE, INY, THEN,                     2 ,X LDA, XOLD CMP, IFCC, 
 INY, THEN, BRHZ CMP, IFCS, INY, THEN, 0 ,X LDA, YOLD CMP, IFCC,
 INY, THEN, BRVT CMP, IFCS, INY, THEN, TYA, IFEQ, 1 # LDA, ELSE,
 0 # LDA, THEN, PUSH0A JMP, C;                                  
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 002
( ROTATE )                      : ROTATE DROP   0->         4CRN
  1 AT BRHZ +! 1 AT BRVT +! TLUM MINUS TO TLUM  ZIPFL BLNK      
 YRNG DO XRNG DO I J XFORM INBD  IF BLOC COLOR I J PLOT ELSE    
 2DROP THEN                           LOOP *LV LOOP CLICK 1 ;   
                                                                
                                                                
                                                                
                                                                
( ANIMATION ROUTINES ) DCX      CODE BKDR XSAVE STX, ?SEQ LDX,  
 SEQAD ,X LDA, N STA, SEQAD 1+   ,X LDA, N 1+ STA, INX, INX,    
 #SEQ CPX, IFEQ, 0 # LDX, THEN,  ?SEQ STX, ?PAGE LDX, YND LDA,  
 100 # CMP, IFCS, SCR1 ,X LDA,   PHA, SCR1 1+ ,X LDA, ELSE,     
 SCR0 ,X LDA, PHA, SCR0 1+ ,X    LDA, THEN, N 3 + STA, PLA,     
 N 2 + STA, ANVT LDX,            BEGIN, 0 # LDY,                
  BEGIN, N )Y LDA, N 2 + )Y STA, INY, ANHZ CPY, EQ UNTIL, CLC,  
 40 # LDA, N ADC, N STA, IFCS,                               -->
════════════════════════════════════════════════════════════════   SCREEN 003
( ANIMATION ROUTINES - BKDR )    N 1+ INC, CLC, THEN, 40 # LDA, 
 N 2 + ADC, N 2 + STA, IFCS, N 3 + INC, CLC, THEN, DEX,         
 EQ UNTIL, SPA LDA, 540 STA,     BEGIN, 540 LDA, EQ UNTIL,      
 1 # LDA, FLP STA,               BEGIN, FLP LDA, EQ UNTIL,      
 764 LDY, 255 # LDX, 764 STX,    XSAVE LDX,  6 # CPY, ( slow )  
 IFEQ, SPA LDA, 1 # ADC, 63 #    AND, SPA STA, 0 # LDA, PUSH0A  
 JMP, THEN, 7 # CPY,  ( fast )   IFEQ, SPA LDA, 1 # SBC, 63 #   
 AND, SPA STA, 0 # LDA, PUSH0A   JMP, THEN,                  -->
( ANIMATION BKDR )              33 # CPY, IFEQ, 1 # LDA, PSHA,  
 THEN, 0 # LDA, PSHA, C;                                        
: POLL BEGIN 764 C@ 255 764 C!    0 SWAP DUP 33 = IF 2DROP 0 1  
  ELSE 28 = IF DROP 1 1 THEN      THEN UNTIL ;                  
: ANIMATE CLICK 0 TO ?PAGE #SEQ   NOT IF BUZZ ELSE 0 BRACT C! 0 
  TO ?SEQ BARS? IF BAROFF THEN            0 ANHZ 0 DO ANVT 0 DO 
       I 40*+S J + C@ OVER FLSTK  + C! 1+ LOOP LOOP DROP BEGIN  
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 004
( ANIMATION CONTINUED )           BEGIN BKDR UNTIL 10 PAUSE POLL
  UNTIL ?PAGE IF 1 TO FLP BEGIN   FLP 0= UNTIL THEN             
 0 ANHZ 0 DO ANVT 0 DO I 40*+S   J + OVER FLSTK + C@ SWAP C! 1+ 
 LOOP LOOP DROP OLDSCR THEN 0    TO SPA ;                       
 : ANWND DROP 0->                 0 TO #SEQ BRVT YOLD - 1+      
  TO ANVT BRHZ XOLD - DUP         TO ANACR PXB / 1+ TO ANHZ ANVT
  ANHZ * 1023 > IF BUZZ THEN 1 ;                                
                                                             -->
( ANIMATION ROUTINES )           : DEFSEQ CLICK 10 TO PDF       
   OLDSCR 10 PAUSE ;             : GETSEQ 2DROP                 
   CLICK #SEQ 64 = IF              BUZZ  ELSE XV YOFF +         
   C@ YV 40*+S + SEQAD #SEQ + !    2 AT #SEQ +! THEN 20 PAUSE ; 
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 005
( CONSTANTS FOR PAINT 10  )         BASE @ HEX   ON  HEADLESS   
 500  CONSTANT SCHI              5C0  CONSTANT SCLO             
9060  CONSTANT PXCN             AF50  CONSTANT UDF              
B0F8  CONSTANT PYCN             B1E0  CONSTANT MACRO            
B20A  CONSTANT PATT             B22B  CONSTANT FLSTK            
B42B  CONSTANT BRUSHES          B62B  CONSTANT CSET             
BA2B  CONSTANT DLTAB            BAAB  CONSTANT REGTAB           
BB2B  CONSTANT CDLTAB                                        -->
 ( CONSTANTS PAINT 10 )         BBAB  CONSTANT LOCTAB           
BCAB  CONSTANT SEQAD            BE00  CONSTANT OFFMSK           
BEA0  CONSTANT ONMSK            BF40  CONSTANT YOFF             
BFE0 CONSTANT PROB               LABEL COLTAB 10 ALLOT          
BFF0 CONSTANT RCOL               LABEL DEFCOL 9 ALLOT           
: KEY KEY 7F AND ;               LABEL DF%    0 C, 28 C, 52 C,  
 C4 C, 42 C, 84 C, 1E C, A2 C,    E C,                          
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 006
( CONSTANTS FOR P10           )  LABEL BRACT 4 ALLOT            
 LABEL OLDCOL A ALLOT            VAR ?MSK  VAR GMODE            
 : MDTAB <BUILDS DOES> GMODE       + C@ ;                       
 MDTAB BLF 3C C, 0 C, 12 C, 0 C, 3C C, (  12 C, )               
 MDTAB BRT 63 C, 3F C, 3E C, 3F  C, 63 C, ( 3E C, )             
 MDTAB GC 1 C, 2 C, 2 C, 2 C, 2  C, (  1 C, )                   
 : GC* GC * ;                    : PYF 6 GC* ;                  
                                                             -->
( CONSTANTS FOR PAINT         ) MDTAB NCLRS ( 9 C, FOR MD 5 )   
4 C, 10 C, 9 C, 10 C, 4 C,      MDTAB SCLN 60 C, C0 C, C0 C,    
C0 C, C0 C,                     MDTAB ASPR ( 6 C, )             
 B C, 42 C, 42 C, 42 C, 16 C,   MDTAB GR? ( 17 C, )             
17 C, 9 C, A C, B C, 18 C,      MDTAB XEND 9F C, 4F C, 4F C,    
4F C, 9F C, ( 4F C, )           : BBT 8 GC* ;                   
0 CONSTANT PLF                   VAR ANHZ VAR ANVT VAR #SEQ     
 VAR ANACR VAR ?SEQ VAR FLP                                  -->
════════════════════════════════════════════════════════════════   SCREEN 007
( CONSTANTS FOR PAINT          ) VAR THFL       VAR 4FL         
 VAR CH#        VAR PRFG         VAR YMN        VAR TRAN        
 VAR MCOL1      VAR MCOL2        VAR SPD        VAR TYPEFL      
 VAR STON       VAR STOFF        VAR CSRCH      VAR BXFG        
 VAR PHI        VAR PHIY         VAR PHIXY      VAR DRFL        
 VAR XE         VAR YE           VAR XV         VAR YV          
 VAR XB         VAR YB           VAR XOLD       VAR YOLD        
 VAR YMX        VAR RW           VAR HNT?  VAR HUN           -->
( CONSTANTS                    ) VAR DLIND      VAR DLRG        
 VAR LETSZ      VAR BR#          VAR BRHZ       VAR BRVT        
 VAR XR?        VAR BRADR        VAR BRFL       VAR QUITFL      
 VAR CSTAT      VAR DLFLAG       VAR BARS?      VAR DEV?        
 VAR XND        VAR YND          VAR OLDST      VAR WFIL?       
 VAR SCLFG      VAR PBUFPT       VAR PC#        VAR SPR         
 VAR P->D       VAR PDF          VAR SECPTR     VAR CREG        
 VAR TLUM       VAR THUE         VAR PERF                    -->
════════════════════════════════════════════════════════════════   SCREEN 008
( CONSTANTS                    )LABEL COL? 0 C,                 
LABEL CURCOL 0  ,               LABEL CRMX 3 C,                 
VAR PXS  50 TO PXS              VAR PYS  C0 TO PYS              
VAR CL1 11 TO CL1                LABEL CLR4 0 C, 55 C, AA C,    
 FF C,                           LABEL CLR16 0 C, 11 C, 22 C,   
 33 C, 44 C, 55 C, 66 C, 77 C,   88 C, 99 C, AA C, BB C, CC C,  
 DD C, EE C, FF C,              MDTAB A.? 10 C, 6 C, 6 C, 6 C,  
  8 C,                           VAR A.                      -->
( CONSTANTS                   )  VAR PXB VAR CLRS VAR MG?       
 LABEL MS4 C0 C, 30 C, C C, 3 C, LABEL MS2 F0 C, 0F C,          
 VAR POINT? VAR PMX VAR PMY      VAR PM?    VAR OLDL VAR B.     
 VAR STMX  4 TO STMX VAR 2B.     MDTAB CLO A C, 4 C, 5 C, 4 C,  
 A C, ( 5 C, )                   MDTAB PRT 8F C, 47 C, 47 C, 47 
 C, 8F C, (  4B C, )             MDTAB HMSK FF C, F0 C, FF C,   
 0F C, FF C, ( FF C, )           : 764 2FC ;                 -->
                                                                
════════════════════════════════════════════════════════════════   SCREEN 009
( MORE CONSTANTS )               VAR MACFL          VAR LSTK    
 VAR AGC VAR RES VAR CURN        MDTAB NCUR 3 C, F C, 7 C, F C, 
 3 C, ( 7 C,) VAR ?CSET VAR XBR  VAR MIND          VAR TFL      
           VAR QFLG              VAR QX VAR QY ( VAR NPTS  )    
LABEL QUILT 0 , 0 , 0 , 0 , 0 C, LABEL QOFF 80 C, 40 C, 20 C,   
 10 C, 8 C, 4 C, 2 C, 1 C,       LABEL CQUILT 9 ALLOT           
 VAR MEN? VAR BOT?  BASE @ DCX   : *LV 764 7 OVER C@ = IF LEAVE 
   THEN 255 SWAP C! ;            BASE !                      -->
( GR 7PLUS                     )CODE 7PLUS                ( -- )
  A9 C, 07 C, 85 C, 57 C, AD C,   30 C, 02 C, 85 C, N  C, AD C, 
  31 C, 02 C, 85 C, N 1+ C, HERE  B1 C, N  C, 29 C, FC C, C9 C, 
  40 C, F0 C, 14 C, B1 C, N C,    85 C, N 2+ C, 29 C,           
  0F C, C9 C, 0F C, D0 C, 06 C,   C6 C, N 2+ C, A5 C, N 2+ C,   
  91 C, N  C, C8 C, 4C C, ,       4C C, NEXT ,  C;              
CODE LO-HI ( wd -- lo hi )      1 ,X LDA, 1 ,X STY, PUSH0A JMP, 
 C; DCX                           : 128 128 ;                -->
════════════════════════════════════════════════════════════════   SCREEN 010
( HISPEED GRAPHICS TABLES )     BASE @ HEX                      
'( TRANSIENT TRANSIENT )(   )     OFF HEADLESS                  
0 VARIABLE LAB01                0 VARIABLE LAB02                
 F0         CONSTANT OLDX        OLDX 1 +   CONSTANT OLDY       
 OLDX 2 +   CONSTANT NEWX        OLDX 3 +   CONSTANT NEWY       
 DC         CONSTANT RX          DD         CONSTANT TSHF       
 DE         CONSTANT XM          DF         CONSTANT YM         
 DA         CONSTANT SMEM                                    -->
( ZERO PAGE EQUATES )                                           
 OLDX 4 +   CONSTANT DELX        OLDX 5 +   CONSTANT DELY       
 OLDX 6 +   CONSTANT TEMP        OLDX 7 +   CONSTANT COLR       
 OLDX 8 +   CONSTANT SCRN                                       
'( PERMANENT PERMANENT )(   )     ON HEADLESS                   
 OLDX A +   CONSTANT XMAX        OLDX B +   CONSTANT XMIN       
 OLDX C +   CONSTANT YMAX        OLDX D +   CONSTANT YMIN       
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 011
( LOCATE )                        1 AFL C!                      
 CODE BDS?   OLDX LDX, OLDY      LDY, FF # LDA, XMIN CPX, IFCC, 
 RTS, THEN, XMAX CPX, IFCS, RTS, THEN, YMIN CPY, IFCC, RTS,     
 THEN, YMAX CPY, IFCS, RTS,      THEN, 0 # LDA, RTS, C;         
 CODE (RLOC) ' BDS? JSR, FF #    CMP, IFEQ, RTS, THEN,          
       SCLO ,Y LDA, SCRN STA,    SCHI ,Y LDA, SCRN 1+ STA, YOFF 
 ,X LDY, SCRN )Y LDA, ONMSK ,X   AND, TAY, LOCTAB ,Y LDA, RTS,  
 C;                          -->                                
( PLOT EQUATES ) HEX            CODE (PLS) QFLG BIT, IFMI, OLDX 
 LDA, CQUILT AND, TAX, OLDY LDA, CQUILT AND, TAY, CQUILT 1+  ,Y 
 LDA,  QOFF ,X AND, IFEQ, MCOL1    LDY,  ELSE, MCOL2 LDY, THEN, 
COLTAB ,Y LDA, COLR STA, THEN,  OLDX LDX, TFL BIT,              
IFMI, OLDY LDA, PATT AND, ASL.A, ASL.A, COLR STA, TXA, PATT AND,
 TAY, YOFF ,Y LDA, COLR ORA,     TAY, PATT 1+ ,Y LDA, COLR STA, 
 THEN, TRAN BIT, IFMI, COLR LDA, ONMSK ,X AND,                  
  IFEQ, RTS, THEN, THEN,                                     -->
════════════════════════════════════════════════════════════════   SCREEN 012
( FPLT  MACROS  )                          OLDY LDY,            
 SCHI ,Y LDA, SCRN 1+            STA, SCLO ,Y LDA, SCRN STA,    
 YOFF ,X LDY, HUN BIT, IFMI,     SCRN )Y LDA, ONMSK ,X AND, TAX,
 LOCTAB ,X LDA, CSRCH CMP, IFNE, RTS, THEN, OLDX LDX, THEN,     
( XOR )       XBR BIT, IFMI,     COLR LDA, ONMSK ,X AND, SCRN )Y
 EOR, SCRN )Y STA, RTS, THEN,    SCRN )Y LDA,                   
 OFFMSK ,X AND, SCRN )Y STA,     COLR LDA, ONMSK ,X AND, SCRN )Y
 ORA, SCRN )Y STA, RTS, C;                             -->      
( PCHECK               )         LABEL XP: 50 C, 3A C, 0 C,     
 CODE XCIO XSAVE STX, 70 # LDX,  E456 JSR, XSAVE LDX, TYA,      
 PUSH0A JMP, C;                 : POS. 54 C! 55 ! ;             
 : PRCK C 3B2 C! XCIO DROP         8 3BA  !  XP: 3B4 !          
   3 3B2 C! XCIO 1 = ;                                          
CODE (4PLT)                       SEC, XND LDA, OLDX SBC,       
 XM STA, YND LDA, OLDY SBC, YM   STA, OLDY LDA, 78    STA,      
 OLDX LDA, 77 STA, ' (PLS) JSR,                              -->
════════════════════════════════════════════════════════════════   SCREEN 013
( PLOT EQUATES )                 4FL LDA, 81 # CMP, IFNE, XM    
 LDA, OLDX STA, ' (PLS) JSR,     4FL LDA, 82 # CMP, IFNE, YM    
 LDA, OLDY STA, ' (PLS) JSR,     2SWAP THEN, 77 LDA, OLDX STA,  
 YM LDA, OLDY STA, ' (PLS) JSR,  THEN, 77 LDA, OLDX STA, 78 LDA,
 OLDY STA, RTS, C;                                              
                                 CODE (FPLT)                    
 PRFG LDA, IFNE, CH# LDY, D20A   LDA, FF # CMP, IFNE, 0 # LDY,  
                             -->                                
( FPLT  MACROS  )                SEC, BEGIN, PROB ,Y CMP, CS    
 WHILE, INY, REPEAT, THEN, RCOL  ,Y LDA, TAY, COLTAB ,Y LDA,    
 COLR STA, OLDY LDY, THEN,       SPR BIT, IFMI, D20A LDA, D8 #  
 CMP, IFCC, RTS, THEN,  THEN,    MACFL BIT,                     
IFMI, MACRO LDA, 2 # CMP,        IFCS, N STA, 1 # LDY, N 1+ STY,
 OLDX LDA, N 2 + STA, OLDY LDA,  N 3 + STA,                     
  BEGIN,     CLC, N 3 + LDA,       MACRO ,Y ADC, OLDY STA, CLC, 
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 014
( FPLT )                           INY, N 2 + LDA, MACRO ,Y ADC,
   OLDX STA, INY, N 1+ STY,        HERE 1+ DUP LAB01 ! JSR, N 1+
   LDY, N CPY,                    EQ UNTIL, N 2 + LDA, OLDX STA,
 N 3 + LDA, OLDY STA, RTS, THEN, THEN,                          
 HERE LAB01 @ !                  ' BDS? JSR, FF # CMP,          
  IFEQ, RTS, THEN,                                              
                                                                
                                                             -->
( REST OF FPLT LOC. PLOT )                                      
 4FL BIT, IFMI, ' (4PLT) JMP,    THEN, ' (PLS) JMP, C;          
 CODE PLOT 0 ,X LDA, OLDY STA,   2 ,X LDA, OLDX STA,  XSAVE STX,
 ' (FPLT) JSR, XSAVE LDX,        POPTWO JMP, C;                 
                                 CODE LOC. (  x y .. v )        
 XSAVE STX,                      0 ,X LDA, OLDY STA, 2 ,X LDA,  
 OLDX STA, ' (RLOC) JSR,         XSAVE LDX, INX, INX, INX, INX, 
 PUSH0A JMP, C;                                              -->
════════════════════════════════════════════════════════════════   SCREEN 015
( FPLX COMMAND )                 CODE (FPLX) ' BDS? JSR, FF #   
 CMP, IFEQ, RTS, THEN,              OLDX LDX, OLDY LDY, SCLO ,Y 
    LDA, SCRN STA, SCHI ,Y LDA,     SCRN 1+ STA, YOFF ,X LDY,   
     ONMSK ,X LDA,                COL? BIT, IFPL, CL1 AND, ELSE,
    COLR AND, THEN,                           SCRN )Y EOR,      
    SCRN )Y STA, RTS, C;         CODE ?PL XR? BIT, IFMI,        
 ' (FPLT) JMP, THEN, ' (FPLX)    JMP, C;                        
 VAR SPA VAR ?PAGE VAR WNDFG                                 -->
( ANIMATION EQUATES & PAGE FLIP) LABEL SCR0 9150 , 8060 ,       
 LABEL SCR1 9150 , 7150 ,        LABEL SCH1 A000 , 8000 ,       
 CODE VBFP FLP LDA, IFNE, ?PAGE  LDX, YND LDA, 64 # CMP, IFCS,  
 SCR1 ,X LDA, 703A STA, SCR1 1+  ,X LDA, 703B STA, SCH1 ,X LDA, 
 709A STA, SCH1 1+ ,X LDA, 709B  STA, ELSE, SCR0 ,X LDA, 7F9C   
 STA, SCR0 1+ ,X LDA, 7F9D STA,  THEN, TXA, CLC,                
 2 # ADC, 3 # AND,               ?PAGE STA, FLP DEC, THEN, RTS, 
 C;                                                          -->
════════════════════════════════════════════════════════════════   SCREEN 016
( FDRAW COMMANDS )              CODE (FDRAW) E6 # LDY, SEC, NEWY
 LDA, OLDY SBC, IFCC, FF # EOR,  1 # ADC, C6 # LDY, THEN, DELY  
 STA, E6 # LDX, SEC, NEWX LDA,   OLDX SBC, IFCC, FF # EOR, 1 #  
 ADC, C6 # LDX, THEN, DELX STA,  DELY CMP, IFCS, 0 # CMP,       
 IFEQ, RTS, THEN,                HERE 1+ DUP LAB01 ! STY, HERE  
 1+ DUP LAB02 ! STX,             ' ?PL JSR,                     
                                    DELX LDA,  LSR.A,           
 BEGIN, SEC, DELY SBC, TEMP STA, IFCC, DELX ADC, TEMP STA,   -->
( FDRAW COMMAND )                                               
 HERE LAB01 @ ! OLDY INC, THEN,  HERE LAB02 @ ! OLDX INC,       
 ' ?PL JSR,                                                     
                                                                
                                 TEMP LDA, NEWX LDX, OLDX CPX,  
 EQ UNTIL, RTS, THEN,                                           
                                                             -->
                                                                
════════════════════════════════════════════════════════════════   SCREEN 017
( FDRAW COMMANDS )                                              
 HERE 1+ DUP LAB01 ! STY, HERE   1+ DUP LAB02 ! STX,            
  ' ?PL JSR,                          DELY LDA, LSR.A,          
 BEGIN, SEC, DELX SBC, TEMP STA, IFCC, DELY ADC, TEMP STA,      
                                                                
                                                                
                                                                
                             -->                                
( FDRAW COMMAND )                                               
 HERE LAB02 @ ! OLDX INC, THEN,  HERE LAB01 @ ! OLDY INC,       
                                 ' ?PL JSR,                     
                                                                
                                 TEMP LDA, NEWY LDY, OLDY CPY,  
 EQ UNTIL, RTS, C;                                              
                                                             -->
                                                                
════════════════════════════════════════════════════════════════   SCREEN 018
( DRAW )  HEX                   CODE DR. 0 ,X LDA, NEWY STA,    
 2 ,X LDA, NEWX STA, XSAVE STX,  FF # LDA, XR? STA,             
 ' (FDRAW) JSR, XSAVE LDX,       0 # LDA, XR? STA,              
 POPTWO JMP, C;                   0 AFL C!                      
                                                                
   1 AFL C!                                                     
                                                             -->
                                                                
( PATTERNED FILLS )                         DCX                 
 LABEL C&    128 C, 64 C,                   192 C, 0 C,         
            128 C, 128 C,                                       
                                                                
                                                                
HEX                                                             
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 019
( FILL ROUTINE )                                                
( THE ROUTINE REQUIRES THAT THE (  STACK BE N PAGES IT IS 4    )
(  HERE STMX CONTAINS N        )(  SCRN IS USED AS A TEMPORARY )
( POINTER )                     (   A MORE COMPLEX PLOT MAY BE )
( DESIRED JUST CHANGE PLOT CALL)                                
                             -->                                
                                                                
                                                                
( FILL SUBROUTINE ONTOSTACK )    CODE ONTSTK ' (RLOC) JSR,      
 CSRCH CMP, IFNE, RTS, THEN,     STON LDA, STOFF CMP, IFEQ,     
 STON 1+ LDA, STOFF 1+ CMP,      IFEQ, RTS, THEN, THEN,         
 2B # LDA, CLC, STON             ADC, SCRN STA, B2 #            
 LDA, STON 1+ ADC, SCRN 1+ STA,  0 # LDY, OLDX LDA, SCRN )Y STA,
 INY, OLDY LDA, SCRN )Y STA,     STON INC, STON INC, IFEQ, STON 
 1+ INC, STON 1+ LDA, STMX CMP,  IFEQ, 0 # LDA, STON 1+ STA,    
 THEN, THEN, RTS, C;                                         -->
════════════════════════════════════════════════════════════════   SCREEN 020
( BLANK )                        CODE PLFL ' (FPLT) JSR, OLDY   
   INC, ' ONTSTK JSR, OLDY DEC,    OLDY DEC, ' ONTSTK JSR, OLDY 
   INC, RTS, C;                  CODE GOL                       
   BEGIN, ' PLFL JSR,                   OLDX DEC, ' (RLOC) JSR, 
 CSRCH CMP, NE UNTIL, RTS, C;   CODE GOR                        
 BEGIN, OLDX INC, ' (RLOC) JSR,  CSRCH CMP, EQ WHILE, ' PLFL    
 JSR, REPEAT,  RTS, C;           CODE COLOR 0 ,X LDY, COLTAB ,Y 
  LDA, COLR STA, POP JMP, C;                                 -->
( FILL SUBROUTINE )                        HEX                  
CODE (FFILL)                     0 # LDA, STON STA, STON 1+ STA,
 STOFF STA, STOFF 1+ STA,        2B #           LDA, SCRN STA,  
 B2 #          LDA, SCRN 1+ STA, 0 # LDY, OLDX LDA, SCRN )Y STA,
 INY, OLDY LDA, SCRN )Y STA, 2   # LDA, STON STA, 0 # LDY,      
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 021
( FILL ROUTINE )                                                
 BEGIN, CLC, 2B #           LDA, STOFF ADC, SCRN STA, B2 #      
     LDA, STOFF 1+ ADC, SCRN 1+  STA, SCRN )Y LDA, OLDX STA,    
 INY, SCRN )Y LDA, OLDY STA,     ' (RLOC) JSR, CSRCH CMP,       
 IFEQ,                             OLDX INC, ' ONTSTK JSR, OLDX 
   DEC,  ' GOL    JSR,                                       -->
                                                                
                                                                
( FILL ROUTINE )                 OLDX INC,                      
 ' GOR   JSR,                    THEN,                          
       0 # LDY,                  STOFF INC, STOFF INC,          
 IFEQ, STOFF 1+ INC, STOFF 1+      LDA, STMX CMP, IFEQ, STOFF 1+
   STY, THEN, THEN,              STON LDA, STOFF CMP, IFEQ,     
 STON 1+ LDA, STOFF 1+ CMP,       IFEQ, INY, THEN, THEN,        
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 022
( FILL ROUTINE )                 D01F LDA, 7 # AND,             
 6 # CMP,                         IFEQ, INY, THEN,              
 0 # CPY,                        NE UNTIL,                      
 RTS,                            C;                             
                                                                
                                                                
                                                                
                                                             -->
( DIAMOND FILL )                CODE PHIL BRACT 3 + STY,        
 TRAN STY,  MACFL STY, HNT? STY, 0 ,X LDA, OLDY STA, 2 ,X LDA,  
 OLDX STA, XSAVE STX, BRACT STY, ' (RLOC) JSR, CSRCH STA,       
 MCOL1 CMP, IFEQ, XSAVE LDX,     POPTWO JMP, THEN,              
 MCOL2 CMP, IFEQ, XSAVE LDX,     POPTWO JMP, THEN, YMX LDA,     
 YMAX STA, MCOL1 LDY, COLTAB ,Y  LDA, COLR STA, YMN LDA, YMIN   
 STA, ' (FFILL) JSR, YND LDY,    INY, YMAX STY, 0 # LDA,        
 YMIN STA, XSAVE LDX, POPTWO     JMP, C;                     -->
════════════════════════════════════════════════════════════════   SCREEN 023
( DLINT FOR PAINT10 )  ( CHG )  HEX                             
                                CODE DLIROUT 48 C, 8A C, 48 C,  
 98 C, 48 C, AE C, D40B , BC C, REGTAB , 30 C, 9 C, BD C,       
CDLTAB , 8D C, D40A , 99 C,     D012 , 68 C, A8 C, 68 C, AA C,  
68 C, 40 C, C;                  DCX                             
                             -->                                
                                                                
                                                                
( PAUSE CURCHK  )                 DCX                           
: PAUSE 540 C! BEGIN 540 C@ 0=    UNTIL ;                       
: SPAUSE 10 PAUSE ;             : LPAUSE 25 PAUSE ;             
  CODE CURCK 0 ,X LDA, 6 # CMP,   IFNE, 14 # CMP, IFNE, 7 # CMP,
  IFNE, 34 # CMP,                 IFNE, 15 # CMP, IFNE, INY,    
  THEN, THEN, THEN, THEN, THEN,   TYA,  PUSH0A JMP, C;          
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 024
( RBAND )                                                       
CODE (RBND) (     ... )          YV   LDA, YE CMP, IFEQ, XV     
 LDA, XE CMP, IFEQ, NEXT   JMP,  THEN, THEN, XSAVE STX, XB LDA, 
 OLDX STA, YB LDA, OLDY STA,     XE LDA, NEWX STA, YE LDA, NEWY 
 STA, 0 # LDA, XR? STA, COL?     STA, ' (FDRAW) JSR,            
 YV   LDA, YE STA, NEWY STA, XV     LDA, XE STA, NEWX STA, XB   
 LDA, OLDX STA, YB LDA, OLDY     STA,          ' (FDRAW) JSR,   
 XSAVE LDX, NEXT   JMP, C;                                   -->
( SPLOT )                 DCX                                   
CODE SPLT POINT? BIT, IFMI, 255  # LDA, XR? STA, 0 ,X LDA,      
      NEWY STA, 2 ,X LDA, NEWX   STA, XSAVE STX, ' (FDRAW) JSR, 
 XSAVE LDX, POPTWO JMP,          THEN, 128 # LDA, POINT? STA,   
 0 ,X LDA, OLDY STA, 2 ,X LDA,   OLDX STA, XSAVE STX, ' (FPLT)  
 JSR, XSAVE LDX, POPTWO JMP, C;                                 
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 025
( RBOX )                        CODE RBOX ( ... )               
YV LDA, YE CMP, IFEQ,                  XV LDA, XE CMP, IFEQ,    
NEXT JMP, THEN, THEN, XSAVE STX, HERE N STY, XR? STY, COL? STY, 
 XB LDA, OLDX STA, YB LDA,       OLDY STA, NEWY STA, XE LDA,    
 NEWX STA, ' (FDRAW) JSR,        YE LDA, NEWY STA, ' (FDRAW)    
JSR, XB LDA, NEWX STA, ' (FDRAW) JSR, YB LDA, NEWY STA,         
 ' (FDRAW) JSR,                  N LDY, IFNE, XSAVE LDX, NEXT   
 JMP, THEN, INY, YV LDA, YE STA,                             -->
( RBOX  ONBX BOXIT )             XV LDA, XE STA, JMP, C;        
 0 AFL C!                        : ONBX 1 TO BXFG XV    YV      
  2DUP TO YE TO XE TO YB TO XB ; 1 AFL C!                       
 CODE QUITBX BXFG LDA, IFEQ,     NEXT JMP, THEN, BXFG DEC, 1    
 # LDY, ' RBOX 19 + JMP, C;                                     
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 026
( SCMP CODE ROUTINE )                                           
 CODE SCMP   PXB LDX,            (             byte in a )      
 N 2+ STA, DEX, 0 # LDA, N 3 +   STA,                           
 BEGIN, N 2+ LDA, ONMSK ,X AND,   IFEQ, ONMSK ,X LDA, N 3 + ORA,
  N 3 + STA, THEN, DEX,          MI UNTIL,  N 3 + LDA,          
 SCRN )Y AND, N 2+ ORA, RTS, C;                                 
                                                                
                                                             -->
( CODE FOR STMP )    DCX                                        
 CODE STMP                         BRADR LDA, N STA, BRADR 1+   
   LDA, N 1+ STA, XV LDX, YOFF     ,X LDA, OLDX STA, 40 # LDA,  
  SEC, OLDX SBC,                        BRHZ CMP, IFCS, BRHZ    
  LDA, THEN, NEWX STA, SEC,       GMODE LDX, ' SCLN 2+ ,X LDA,  
       YV SBC, BRVT CMP,          IFCS, BRVT LDA, THEN, NEWY    
  STA, CLC, YV LDY, SCLO ,Y LDA,  OLDX ADC, SCRN STA, SCHI ,Y   
  LDA, 0 # ADC, SCRN 1+ STA,                                 -->
════════════════════════════════════════════════════════════════   SCREEN 027
( STMP )                                                        
 BEGIN, 0 # LDY,                  BEGIN, N )Y LDA, XR? BIT,     
     IFPL, SCRN )Y EOR,              ELSE, ' SCMP JSR,          
     THEN,                          SCRN )Y STA,                
    INY, NEWX CPY,                EQ UNTIL,                     
  CLC, 40 # LDA, SCRN ADC, SCRN   STA, IFCS, SCRN 1+ INC, THEN, 
  N LDA, CLC, BRHZ ADC,           IFCS, N 1+ INC, THEN, N STA,  
                             -->                                
( STMP CLICK )                                                  
 NEWY DEC,                      EQ UNTIL, RTS, C;               
                                 CODE PUTBRS  XSAVE STX, 255 #  
 LDA, XR? STA, ' STMP  JSR,      0 # LDA, XR? STA, XSAVE LDX,   
 NEXT JMP, C;                                   HEX             
DCX                             : CLICK 53279 8 OVER C! 7 OVER  
  C! 8 SWAP     C! ;                                            
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 028
( 40*+S )                                                       
 CODE 40*+S ( y ... 40*y+sctop ) 0 ,X LDY, SCHI ,Y LDA, 1 ,X    
 STA, SCLO ,Y LDA, 0 ,X STA,     NEXT JMP, C;                   
                                                                
 DCX 155 LOAD DCX               ( VBLANK PMG )                  
 HEX                             HERE 40 C, 0 C, 20 C, 60       
 C,                                                             
                                                             -->
( PRINT LETTER )         HEX     CODE ATOI 0 ,X LDA, PHA, 1F #  
 AND, 0 ,X STA, PLA, E9 STY,     ASL.A, ASL.A, E9 ROL, ASL.A, E9
 ROL, E9 LDY,      ,Y LDA, 0 ,X  ORA, 0 ,X STA, NEXT JMP, HERE  
 C; DROP                         CODE BPLT ( byte ... plots )   
 XSAVE STX, 0 ,X LDA, EA STA,    XV LDA, OLDX STA, YV LDA, OLDY 
 STA, OLDX DEC, 8 # LDA, E9 STA, BEGIN, OLDX INC, CLC, EA   ASL,
 IFCS, ' (FPLT) JSR, THEN, E9    DEC, EQ UNTIL, XSAVE LDX, POP  
 JMP,       C; DCX                                           -->
════════════════════════════════════════════════════════════════   SCREEN 029
( VBLANK CODE FOR PADDLE )         DCX   OFF HEADLESS           
 '( TRANSIENT TRANSIENT )(   )    ASSEMBLER                     
  ASSEMBLER DEFINITIONS                                         
 LABEL (LB) 32 ALLOT                                            
 : LB! 2* (LB) + ! ;             : LB@ 2* (LB) + @ ;            
                                 FORTH                          
 FORTH DEFINITIONS               '( PERMANENT PERMANENT )(   )  
   ON HEADLESS LABEL SR 0 C,     LABEL TOLR  8 C,            -->
( VBLANK CODE FOR PADDLE )       LABEL SVBR 2 ALLOT ( addr  )   
 HEX                            ( E0 E1 CONTAIN X , Y )         
( FF MEANS PAD IS UP  )                                         
 LABEL VBR ASSEMBLER             HERE 0 LB!  ( MULT4 )          
PHA, LSR.A, LSR.A, LSR.A, LSR.A, LSR.A, LSR.A, TAX, PLA, ASL.A, 
 ASL.A, RTS,                     HERE 1 LB! ( MULT6 )           
 E8 STA, 0 LB@ JSR, CLC, E8 ADC, IFCS, INX, CLC, THEN, E8 ADC,  
 IFCS, INX, THEN, RTS,                                       -->
════════════════════════════════════════════════════════════════   SCREEN 030
( VBLANK PAD )                                                  
 HERE 2 LB! ( ACCUM )            CLC, E4 ADC, E4 STA, TXA, E5   
 ADC, E5 STA, RTS,               HERE 3 LB! ( FILTER )          
 0 # LDA, E5 STA, 6F0 ,Y LDA,    E4 STA, 6F1 ,Y LDA, 0 LB@ JSR, 
 2 LB@ JSR, 6F2 ,Y LDA, 1 LB@    JSR, 2 LB@ JSR,                
 6F3 ,Y LDA, 0 LB@ JSR, 2 LB@    JSR, 6F4 ,Y LDA, 0 # LDX,      
 2 LB@ JSR,                      E4 LDA, LSR.A, LSR.A, LSR.A,   
 LSR.A, E4 STA, E5 LDA,                                      -->
( VBLANK PAD )                                                  
 ASL.A, ASL.A, ASL.A, ASL.A,     E4 ORA, E4 STA,  5 # LDX,      
 BEGIN, E4 LDA, SEC,             6F0 ,Y SBC, IFCC, FF # EOR, 1 #
 ADC, THEN, TOLR CMP, IFCS,      FF # LDA, RTS, THEN, INY, DEX, 
 EQ UNTIL, TXA, E4 LDX, RTS,                                    
 HERE SVBR ! ( START OF VBLNK )                                 
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 031
( VBLANK PADDLE )               4 # LDX,                        
BEGIN, 6EF ,X LDA, 6F0 ,X STA,   6F4 ,X LDA, 6F5 ,X STA, DEX,   
EQ UNTIL,                        272 LDA, 6F0 STA, 273 LDA,     
 6F5 STA, 0 # LDY,  3 LB@ JSR,   0 # ORA, IFPL, E2 STX, 5 # LDY,
 3 LB@ JSR, 0 # ORA, IFPL, E3    STX, 5 # LDA, E2 CMP, IFCS,    
 E3 CMP, IFCS, FF # LDA, E2 STA, E3 STA, THEN, THEN, E2 LDA,    
 E0 STA, E3 LDA, E1 STA, THEN,   THEN,                          
                                                             -->
( VBLANK ROUTINES FOR MAGNIFY ) DCX  1 AFL C!                   
 16 # LDA,  SR STA,    XV LDA,   PHA, 3 # AND, RX STA, SEC, 4 # 
 LDA, RX SBC, 3 # AND, ASL.A,     ASL.A,  TAX,                  
 PLA, LSR.A, LSR.A, SEC, 6 #     SBC, IFCC, 0 # LDA, TAX, SR    
 STA, SEC, ELSE, 30 # CMP, IFCS, 30 # LDA, 0 # LDX, SR STX,     
 THEN, THEN, RX STA, 0 # CPX,     IFNE, 30 # CMP, IFNE, RX INC, 
 THEN, THEN, 54276 STX, AGC LDY, YV LDA, SEC,  12 # SBC, IFCC,  
 0 # LDA, ELSE, 2 # CPY, IFEQ,   168 # CMP, IFCS, 168 # LDA, -->
════════════════════════════════════════════════════════════════   SCREEN 032
(  VBLANK MAGNIFY  )             THEN, ELSE, 72 # CMP,          
 IFCS, 72 # LDA, THEN, THEN,     THEN,                      TAY,
 CLC, SCLO ,Y LDA, RX ADC,       SMEM STA, SCHI ,Y LDA, 0 # ADC,
 SMEM 1+ STA,                    HEX   EF STY,  83 # LDX, 48 #  
 LDA, SR ORA, TAY,               BEGIN, TYA, 600 ,X STA, INX,   
 SMEM LDA, 600 ,X STA, INX,      SMEM 1+ LDA, 600 ,X STA, INX,  
 CLC, 28 # LDA, SMEM ADC, IFCS,  SMEM 1+ INC, THEN, SMEM STA,   
 CB # CPX,                       EQ UNTIL,                   -->
( END OF VBLANK )                ' VBFP JSR,                    
                                                                
 ' PMG JMP,                      FORTH                          
                                 1 AFL C!                       
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 033
( STVB )                                                        
 CODE STVB XSAVE STX, SVBR       LDY, SVBR 1+ LDX, 7 # LDA,     
 E45C JSR, XSAVE LDX, NEXT JMP,  C;                             
                                DCX                             
                                                                
                                                                
                                                                
                                                             -->
(  QUITLN, DWND )               CODE QUITLN DRFL LDA, IFEQ,     
 NEXT JMP, THEN, DRFL DEC, XE    LDA, NEWX STA, 0 # LDA, XR?    
 STA, COL? STA, YE LDA, NEWY     STA, XB LDA, OLDX STA, YB LDA, 
 OLDY STA, XSAVE STX, ' (FDRAW)  JSR, XSAVE LDX, NEXT JMP, C;   
CODE DWND 1 # LDY, XV LDA, XE    STA, YV LDA, YE STA, ' RBOX 19 
 + JMP, C;                      CODE FNDEND XV LDA, ANACR ADC,  
 IFCS, XND LDA, ELSE, XND CMP,   IFCS, XND LDA, THEN, THEN, CLC,
 XB STA, YV LDA, ANVT ADC,                                   -->
════════════════════════════════════════════════════════════════   SCREEN 034
( CURSOR PLOT ) DCX              IFCS, YND LDA, ELSE, YND CMP,  
 IFCS, YND LDA, THEN, THEN, CLC, YB STA, NXT, C;                
CODE CURS! XSAVE STX,            0 # LDA, XR? STA, 255 # LDA,   
 COL? STA, ' STMP JSR,           XSAVE LDX,       NEXT JMP, C;  
                                                                
                             -->                                
                                                                
                                                                
( Q*/ )                          CODE Q*/ ( x y z ... xy/z )    
 3 # LDA, SETUP JSR, DEX, DEX,   TYA, 1 ,X STY, 8 # LDY,        
 BEGIN, ASL.A, 1 ,X ROL, N 2+    ASL, IFCS, CLC, N 4 +    ADC,  
 IFCS, 1 ,X INC, THEN, THEN,     DEY, EQ UNTIL, 0 ,X STA, 1 ,X  
 LDA, N ORA, IFMI, 1 ,X LSR,     0 ,X ROR, N LSR, THEN,         
 DEX, DEX, 2 ,X LDA, 0 ,X STA,   3 ,X LDA, 1 ,X STA, 3 ,X STY,  
 8 # LDY, BEGIN, 2 ,X ASL,       ROL.A, N CMP, IFCS, N SBC,     
 2 ,X INC, THEN, DEY, EQ UNTIL,  POP JMP, C;                 -->
════════════════════════════════════════════════════════════════   SCREEN 035
( Q* Fast multiply )               DCX  -->                     
CODE Q* ( x y ... xy )           XSAVE STX, DEX, DEX, 4 ,X LDA, 
 0 ,X STA, 5 ,X STY, TYA, 8 #    LDY,                           
 BEGIN,                            ASL.A, 5 ,X ROL, 2 ,X ASL,   
   IFCS, CLC, 0 ,X ADC,             IFCS, 5 ,X INC, THEN,       
   THEN,                           DEY,                         
 EQ UNTIL, 4 ,X STA, XSAVE LDX,  POP JMP, C;                    
                                                                
( DEV!! )  DCX                  CODE (DEV!!) 223 STY,           
DEX, DEX, DEX, DEX, DEX, DEX,   1 ,X STY, 3 ,X STY, 4 ,X STY,   
5 ,X STY, DEV? BIT, IFMI,       224 LDY, 255 # CPY, IFNE,       
PXCN ,Y LDA, ELSE, XV LDA, 223  INC, THEN, 2 ,X STA, 225 LDY,   
 255                             # CPY, IFNE, PYCN ,Y LDA, ELSE,
 YV LDA, 223 INC, THEN,  0 ,X    STA, 638 LDA, 639 AND,    223  
 ORA, IFNE, 0 # LDA, POINT? STA, 1 # LDA, THEN, 4 ,X STA,       
 NEXT JMP, THEN,                                             -->
════════════════════════════════════════════════════════════════   SCREEN 036
( DEV!! )                                                       
                                                                
  632 LDA, LSR.A, IFCC, YV LDY,   IFNE, YV DEC, THEN, THEN,     
  LSR.A, IFCC, YV INC, THEN,      LSR.A, IFCC, XV LDY, IFNE,    
  XV DEC, THEN, THEN,             LSR.A, IFCC, XV INC, THEN,    
  XV LDA, 2 ,X STA, YV LDA, 0 ,X  STA, 644 LDA, 4 ,X STA, IFNE, 
  0 # LDA, POINT? STA, THEN,      NEXT JMP, C;                  
                                   0 AFL C!                  -->
( ZERO FLAGS SAVE FLAGS )       LABEL SVFLG 9 ALLOT             
: ZIPFG 0 TO QFLG 0 TO TYPEFL     0 TO TRAN 0 TO MACFL 0 TO XBR 
  0 TO PRFG 0 TO TFL 0 TO HUN     0 TO SPR          ;  : P> SPR 
 HUN  XBR TFL PRFG MACFL TYPEFL   QFLG TRAN 9 0 DO I SVFLG + C! 
 LOOP ZIPFG ;                   : >P AT SPR AT HUN AT XBR AT TFL
 AT PRFG AT MACFL AT TYPEFL     AT QFLG AT TRAN 9 0 DO I SVFLG +
 C@ SWAP C! LOOP ;                                              
DCX                                  75 LOAD                    
════════════════════════════════════════════════════════════════   SCREEN 037
( MODULE TESTING )               29000 CONSTANT MODSTART        
: GETMOD <BUILDS   C,   ,         DOES> ->0 0 MODSTART !        
       DUP 1+ @ MODSTART SWAP     1 (RW) NOT SWAP C@ MODSTART C@
  = AND NOT                       IF      BUZZ BUZZ             
  ELSE MODSTART 1+ @ MODSTART     3 + @ 0 DO MODSTART I 128 *   
  + OVER I + 1 R/W LOOP  DROP    MODSTART 5 + @EX THEN 0-> ;    
 330 5 GETMOD GLO                                               
                                                                
( GRAPHICS SETUP )                  0 AFL C!                    
 : ?RG ( reg# ... new reg# )      GMODE DUP 1 =                 
  IF 2DROP 8                      ELSE DUP 3  =                 
   IF 2DROP 8                      ELSE DUP 0= SWAP 4 = OR      
     IF DUP 0= IF DROP 8 ELSE         3 + THEN                  
     THEN                          THEN                         
  THEN  ;  -->                   ( NO MODE 5 )                  
: CHK5 GMODE 5 = IF DUP IF        5 - THEN THEN ;               
════════════════════════════════════════════════════════════════   SCREEN 038
( GRAPHICS )                     HEX  0 AFL C!                  
: XTAB A0 0 DO I PXB /MOD YOFF I  + C! ?MSK + C@ DUP ONMSK I +  
  C! FF XOR OFFMSK I + C! LOOP ;                                
: LTAB 1 TO YOLD PXB 0 DO CLRS    0 DO I YOLD * LOCTAB + I SWAP 
  C! LOOP YOLD CLRS * TO YOLD     LOOP ;                        
                                                                
                                                                
                                                             -->
( GRAPHICS )                     : GRX ( A0 6A C! )             
           DUP TO GMODE GR? GR.    NCLRS 5 > IF 2 10 ELSE 4 DUP 
   THEN TO CLRS DUP TO PXB 4 =     IF MS4 CLR4 ELSE MS2 CLR16   
   THEN COLTAB 10 CMOVE TO ?MSK    XEND TO XND                  
   4 = IF 7PLUS THEN C0 0 DO       I 28 * 58 @ + LO-HI I SCHI + 
   C! I SCLO + C! LOOP XTAB        LTAB PXB 4 = IF 55 ELSE 66   
   THEN TO CL1 SCLN 1- TO YND      GC TO AGC YND 1+ TO YMX      
   ASPR DUP TO B. 2* TO 2B.        A.? TO A. ;               -->
════════════════════════════════════════════════════════════════   SCREEN 039
( GRAPHICS QWIK )               : GRF GR? GR. ( GMODE 5 = IF )  
 ( 80 26F C! THEN ) GMODE 4 = IF  7PLUS THEN ; DCX              
 : HUNT CLICK 1 TO HNT? 128 TO     HUN          ;               
: BLOB BARS? IF 0 TO YMN  MCOL1   COLOR SPR MACFL HUN XBR TRAN 0
  TO HUN 0 TO TRAN 0 TO XBR 0    TO MACFL 0 TO SPR          8 0 
DO XEND I PLOT XEND 7 - I DR.    LOOP TO TRAN TO XBR TO HUN     
 BBT TO YMN TO MACFL TO SPR THEN ;      : ZIPFL ZIPFG BLOB ;    
80 LOAD                                                         
( TILES AND QUILTS MENU )       : MVQLT  QUILT CQUILT 9 CMOVE ; 
: COL12 1 TO MCOL1 2 TO MCOL2 ; : TQMENU MCOL1 MCOL2 COL12      
  CLRBAR ROT 5 0 DO I 49 + OVER   EXECUTE FRU I 10 * 20 +       
 QFLG IF MVQLT THEN               8 0 DO I OVER + DUP 0 PLOT    
  BBT 1- DR. LOOP DROP LOOP       DROP TO MCOL2 TO MCOL1 >P ;   
: TLMEN P> 128 TO TFL 0 TO 4FL    ' TID CFA TQMENU ;            
                                : QLMEN P> 128 TO QFLG 0 TO 4FL 
 ' QUD CFA TQMENU ;                                             
════════════════════════════════════════════════════════════════   SCREEN 040
( BEGIN OF PAINT-X COLJARS )               DCX                  
: N640 BBT  40 * ;              : NSCM SCLN 40 * ;              
: 88@ 88 @       ;              : COLJARS P> 0 TO 4FL           
  88@ N640 2 FILL                 NCLRS CLO * 0 DO              
  I CLO / COLOR I BLF + DUP 0    PLOT BBT 1- DR. LOOP 1 TO BARS?
             >P BLOB     ;                                      
                                                                
                                                             -->
( PAINT SETCOL RETED TOBUF )                                    
: GTCOL 712 HMSK OVER C@ AND      SWAP C! ;                     
                                : SETCOL CLICK DEFCOL 704 9     
  CMOVE GTCOL ;                                                 
: B* BARS? N640 * ;             : BFTOBR PBUFPT 88@ N640 CMOVE ;
 : TOBUF 88@ B* + PBUFPT B* +     NSCM B* - CMOVE   ;           
: RETED CLICK 0 TO CSTAT 0        TO DLFLAG ;                   
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 041
( PAINT FRBUF CHCOL BAROFF )    : BRTOBF 88@ PBUFPT N640 CMOVE ;
 : FRBUF PBUFPT B* + 88@ B* +      NSCM B* - CMOVE ;            
 : BAROFF CLICK CSTAT NOT IF       BARS? IF 0 TO BARS?          
   BFTOBR                          0 TO YMN ELSE BRTOBF         
         COLJARS THEN THEN                  ;                   
 : CHCOL CLICK BARS? NOT IF      BAROFF THEN   1 TO CSTAT       
        ;                        : OLDSCR TOBUF 1 TO OLDST ;    
                                                             -->
( PAINT SVCOL MYCOL FILL COMDS )( OFF HEADLESS  )               
: SVCOL 704 OLDCOL 9 CMOVE  ;   : MYCOL OLDCOL 704 9 CMOVE GTCOL
  ; ( ON HEADLESS )             : FLCOM CLICK 2 TO PDF ZIPFL ;  
                                                                
                                                                
                                                                
: FILB OLDSCR           PHIL ;                               -->
                                                                
════════════════════════════════════════════════════════════════   SCREEN 042
( UNFILL DRCMD MIRROR PLCOM )                                   
: CLRBAR BARS? IF BAROFF THEN     BRTOBF 88@ N640 ERASE ;       
: DRCOM CLICK 1 TO PDF 0 TO DRFL  ZIPFL OLDSCR ;                
: DFSET 57344 TO ?CSET ;        : MIRROR CLICK 4FL IF 0 TO 4FL  
  ELSE BARS? IF BAROFF THEN 128   TO 4FL KEY DUP 72 = IF 129    
  TO 4FL DROP ELSE 86 = IF 130    TO 4FL THEN THEN THEN         
        ;                        : 49- 49 - ;                   
                                                             -->
( BRUSHES    80-84    ) DCX     : PLCOM CLICK 0 TO PDF 0 TO P->D
  0 TO THFL ZIPFL OLDSCR ;      : UNFILL CLICK OLDST IF FRBUF   
  THEN            ;             : ST3 BR# DUP 3 = IF DROP 0 THEN
 ; : BUZZ 200 0 DO CLICK LOOP ; : BRINIT BRACT 4  ERASE ;       
: BADKEY?  SPAUSE KEY DUP 49 <    OVER 53 > OR ;                
: BRINFO ST3 512 * BRUSHES +      DUP DUP 2+ TO BRADR C@        
    TO BRHZ 1+ C@ TO BRVT ;     : BRDKEY?  SPAUSE KEY DUP 49 <  
  OVER 52 > OR ;                                             -->
════════════════════════════════════════════════════════════════   SCREEN 043
( BRUSHES )                     : BRMX BR# 3 = IF 1534 ELSE     
  510 THEN ;                    : PICKBRS                       
  CLICK BRDKEY? DUP NOT IF        SWAP 49- DUP TO BR# BRACT +   
  C@ NOT OR ELSE SWAP DROP THEN   IF BUZZ ELSE 2 TO BRFL 4      
 TO PDF BRINFO 0 TO XR? OLDSCR    THEN ;                        
: MAKEBRS CLICK BRDKEY? IF BUZZ   DROP ELSE 49- DUP TO BR# 3 =  
  IF BRINIT ELSE 0 BRACT 3 + C!   THEN      0 TO BRFL 4 TO PDF  
  BR# IF DFSET THEN THEN ;                                   -->
( BRUSHES )                     : BZZ BUZZ 0 TO BRFL ;          
: GETBRS BRINFO                   XV XOLD - PXB / DUP 0 >=      
  YV YOLD -  DUP 0 >= ROT AND     IF XOLD PXB / TO XOLD 1+ DUP  
  TO BRVT BRADR 1 - C! 1+ DUP     TO BRHZ BRADR 2 - C!          
  BRVT BRHZ * BRMX <= IF           BRVT 0 DO BRHZ 0 DO I XOLD   
   + J YOLD + 40*+S + DUP C@       SWAP 0 SWAP C! I J BRHZ * +  
     BRADR + C! LOOP LOOP          1 BR# BRACT + C! 2 TO BRFL   
              ELSE BZZ THEN      ELSE 2DROP BZZ THEN ;       -->
════════════════════════════════════════════════════════════════   SCREEN 044
( BRUSHES -- DOBRUSH )          : DOBRUSH  CLICK 2DROP BRFL DUP 
 0= IF 1+ TO BRFL XV TO XOLD YV  TO YOLD ONBX ELSE 1 = IF QUITBX
 GETBRS          ELSE PUTBRS     THEN THEN LPAUSE   ;           
 : DOQLT ( addr mv# f# ... )       1 TO TYPEFL 0 TO TFL         
   128 TO QFLG 0 TO PRFG           CQUILT C! CQUILT 1+ SWAP     
   CMOVE CLICK BLOB ;           -->                             
: ?9B DUP 155 = - ;              : PABRT ?TERMINAL IF LEAVE     
   THEN ;                       : DMC -1 SCLN 1- DO DUP I 40*+S 
( NEW CHAR STUFF )               : LCNVRT                       
   ATOI 8 * ?CSET + ;           : FLCMS <BUILDS , DOES>         
  @ 2 1 DOQLT ;                  C& 4 + FLCMS VF                
 C& 2+ FLCMS HF                  C&  FLCMS MF                   
                                 : CHRFIL CLICK  LPAUSE LPAUSE  
   KEY LCNVRT 8 7 DOQLT ;                                       
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 045
( CIRCOM BXCMD THICK  )                                         
: THICK CLICK 1 TO THFL 0          TO P->D 0 TO PDF ZIPFL       
   OLDSCR ;                     : BXCOM CLICK 6 TO PDF 0 TO BXFG
  ZIPFL OLDSCR ;                : CIRCOM CLICK PDF 3 = IF       
  128 ELSE 0 THEN TO PERF                      0 TO P->D 3      
 TO PDF ZIPFL OLDSCR ;                                          
: XBRUSH CLICK 128 TO XBR               ;                       
                                                             -->
( PATTERN BRUSHES ETC ... )     : TOU 84 * + UDF + ROT CMOVE ;  
: FRU 84 * + UDF + SWAP ROT       CMOVE ;                       
                                : MAD 49- 42 SWAP MACRO 0 ROT ; 
 : TID 49- 33 SWAP PATT 42 ROT   ;                              
 : QUD 49- 9 SWAP QUILT 75 ROT   ;                              
: (RW) SWAP 720 /MOD 1+ SWAP 1+   SWAP ROT -DISK ; : R/W (RW) IF
  BUZZ THEN ;                   : ?RW (RW) IF BUZZ LEAVE THEN ; 
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 046
( BOUNDS   )                     : LOBND YMIN C! XMIN C!  ;     
                                 : HIBND YMAX C! XMAX C! ;      
: INBND 0 DUP LOBND XEND 1+ SCLN      HIBND ;                   
  VAR PYFF                      : ENDS 12 PXCN + C@ 12 PYCN + C@
 12 0 DO 2DUP I PYCN + C! I PXCN + C! LOOP 2DROP 204 PXCN + C@  
 204 PYCN + C@ 229 205 DO 2DUP I PYCN + C!   I   PXCN + C! LOOP 
 2DROP ;                        : KOALA SCLN TO PYS 0 TO PYFF   
  XEND 1+ TO PXS ;                                           -->
( FULLPAD BOTTOM CHECK FOR DLI)  : ATARI SCLN TO PYFF SCLN MINUS
   TO PYS XEND 1+ TO PXS ;       VAR ?PAD                       
 : INITPD 205 12 DO I 12 - PXS     192 */ PXCN I + C! I 12 - PYS
  192 */ PYFF + PYCN I + C! LOOP   ENDS ;                       
 : FLPAD ?PAD IF ATARI ELSE        KOALA THEN                   
   INITPD INBND ;                : BCHK BOT? PMY 204 > AND NOT  
   AND ;                                                        
   KOALA                                                     -->
════════════════════════════════════════════════════════════════   SCREEN 047
( 4KCHK SEQ/COL IO )  HEX         OFF HEADLESS TRANSIENT        
: 4KCK HERE 0FFF AND 1000 SWAP    - DUP 30 < IF ALLOT ELSE DROP 
THEN ;   PERMANENT ON HEADLESS   : MISSV OLDCOL FLSTK 9 CMOVE   
 ANHZ FLSTK A + C! ANVT FLSTK B  + C! ANACR FLSTK C + C! #SEQ   
 FLSTK D + C! SEQAD FLSTK E + 40 CMOVE ;                        
: MISLD FLSTK OLDCOL 9 CMOVE     FLSTK A + C@ TO ANHZ FLSTK B + 
C@ TO ANVT FLSTK C + C@ TO ANACR FLSTK D + C@ TO #SEQ FLSTK E + 
 SEQAD 40 CMOVE OLDCOL DEFCOL    9 CMOVE ; DCX               -->
( DLI'S !!! )                   ( NOTE CHECK FOR MG? )          
                                 : MVDLI 3 0 DO DLTAB I 128 * + 
   I SECPTR 61 + + RW ?RW LOOP ; : ONDLI MG? NOT IF             
         ' DLIROUT 512 ! 192       54286 C! THEN ;              
                                 : OFFDLI 64 54286 C! ;         
                                 : INIDLI DLTAB 384 ERASE REGTAB
   128 DUP FILL 0 TO DLFLAG ;                                   
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 048
( DLINT PNT10 )                                                 
: SETDLI OFFDLI 560 @ 128 0 DO    DUP I DLTAB + C@ DUP IF +     
  DUP C@ 128 OR SWAP C! ELSE      2DROP THEN LOOP DROP ONDLI ;  
                                : CKDLI DUP BBT 1+ < NOT OVER GC
  MOD NOT BCHK  AND OVER GC /     16 +  SWAP ;                  
                                : CONDLI DUP 95 < IF 5    ELSE 7
        THEN + ;                : CLKOFF CLICK OFFDLI ;         
                                                             -->
( DLINT PNT10 )                 : DLOK CLKOFF YV GC -           
    CKDLI MG? NOT AND ;         : DELDLI DLOK IF                
  0 OVER DLTAB + C! REGTAB + 128  SWAP C! CONDLI 560 @ + DUP C@ 
  127 AND SWAP C! ELSE 2DROP      THEN ONDLI ;  -->             
: DISDLI DLOK OVER                DLTAB + C@ *   IF REGTAB +    
  DUP C@ 128 OR SWAP C! DROP      ELSE 2DROP THEN ONDLI ;       
: ENDLI DLOK OVER                 DLTAB + C@ *   IF REGTAB +    
  DUP C@ 127 AND SWAP C! DROP     ELSE 2DROP THEN ONDLI ;    -->
════════════════════════════════════════════════════════════════   SCREEN 049
( DLIND PNT10 )                 : INSDLI DLOK              IF   
  DUP TO DLIND DLTAB + C@ 0= IF   CONDLI DUP DLIND DLTAB + C!   
  560 @ + DUP C@ 128 OR SWAP C!  BLF TO XV DLIND 0 ?RG 704 + C@ 
 OVER  CDLTAB + C! REGTAB +      0 ?RG    SWAP C!               
  ELSE DROP DLIND REGTAB +        DUP C@ 127 AND DUP ROT C! CLO 
  * 18 CLO 5 */ +                         TO XV THEN 1 TO DLFLAG
  CHCOL ELSE 2DROP THEN ONDLI ;                                 
                                                             -->
( DLIND PNT10 SOME I/O )        : COL@ DLFLAG IF TO DLRG        
       DLIND CDLTAB + ELSE       704 + THEN C@ ;                
: COL! DLFLAG IF DROP DLIND       CDLTAB + ELSE 704 + THEN C! ; 
: DLR! DLFLAG IF DLRG DLIND        REGTAB + C! THEN ;           
: KST 0 694 C! 64 702 C! ;       163 LOAD                       
: OTT> BARS? IF BAROFF THEN       OFFBOT SVCOL TOBUF ;          
: >INN FRBUF MYCOL ONBOT ;                                      
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 050
( SOME I/O WORDS ERSPIC )       : QU? CLICK KST                 
      ." Are you sure? (Y/N) "  CR KEY DUP EMIT 89 = ;          
: ->0  CLICK OTT> 0 GR. PMOFF ;                                 
: 0-> 0 TO MG? GRF 0 TO CSTAT     >INN PMON 0 TO MG?  ;         
: ERSPIC ->0 CR CR QU? IF         INIDLI     0-> 88@ NSCM ERASE 
   PBUFPT NSCM    ERASE COLJARS  ONBOT   ELSE 0-> THEN ;        
: PQUIT CLICK  ->0 QU? IF 1       TO QUITFL THEN 0-> ;          
: SL? RW IF ." Load " ELSE        ." Save " THEN ;           -->
( DISK IO  )                        DCX 680 CONSTANT DS         
 : DRIN 3 0 DO I 128 * FLSTK +     I DS  + 1 ?RW LOOP ;         
                                 : DROUT 3 0 DO I 128 * FLSTK + 
   I DS  + 0 ?RW LOOP ;          : QU1 CR ." Initialize (I)" CR 
 ." Directory (D)" CR            ." Print Directory (P)" CR     
  SL?      ." Picture (0-9)" CR    KST ;                        
 : INITD DRIN FLSTK 384 BL FILL    DROUT QU1 ;                  
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 051
( DISK IO )                     : .LN ( # blk  ... ) OVER 3 .R  
  SPACE SWAP  4 /MOD ROT + FLSTK  SWAP 1 R/W BL * FLSTK + BL    
  -TRAILING TYPE ;              : PCLST CLS                     
." Directory:" CR CR 10 0 DO CR   I DS .LN                 LOOP 
  CR CR    1 PFLAG ! QU1 ;      : ?SV RW NOT IF CLS FLSTK 512 + 
  BL DUP FILL CR ." Name of # "   PC# . CR CR QUERY 155 WORD    
  HERE 1+ FLSTK 512 + HERE C@     CMOVE DRIN FLSTK 512 + FLSTK  
  PC# BL * + BL CMOVE DROUT       THEN LPAUSE   ;            -->
( DISK I-O )                     : DSKIO PC# 64 * 1+ TO SECPTR  
  60 0 DO PBUFPT I 128 * +        SECPTR I + RW ?RW LOOP RW IF  
  FLSTK SECPTR 60 + RW R/W MISLD  ELSE MISSV                    
  FLSTK SECPTR 60 + RW R/W THEN   MVDLI      ?SV ;              
                                 : PRLST PRCK IF 3 PFLAG ! THEN 
   PCLST ;                       : MVUDF 692 4 0 DO DUP I +     
   I 128 *   UDF + SWAP RW ?RW     LOOP DROP FLPAD ;            
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 052
( MVPIC ) HEX                    : PCPIC DUP 2F > OVER 3A <     
   AND IF 30 - TO PC# DSKIO 1      SWAP THEN ;                  
                                 : MVPIC ->0 CLS CR             
 ." INSERT YOUR PICTURE DISK "   CR CR QU1 40 2BE C! BEGIN 0    
 2FC C@ FF = NOT IF KEY DUP      SEL 44 -> PCLST 50 -> PRLST    
 49 -> INITD NOSEL PCPIC SELEND  DROP  THEN                     
  ?TERMINAL OR UNTIL 0-> ;       DCX 174 LOAD                   
                                                             -->
( FONTS )                       DCX                             
: MVSET ( font# ... ) DUP 0=      IF DROP DFSET                 
  ELSE 1- 8 * 648 + 8 0 DO        CSET I 128 * + OVER I +       
 1 ?RW LOOP   BRACT 1+ 3 ERASE    DROP CSET TO ?CSET            
                         THEN ; : TOUDF CLICK 1 TO RW MVUDF ;   
: FRUDF CLICK 0 TO RW MVUDF ;                                   
 5 LOAD                          74 LOAD                        
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 053
( PAINT DISK I/O  RES TILES )   : SVPIC 0 TO RW MVPIC ;         
: LOADPIC 1 TO RW MVPIC ;  DCX  : REST CLICK BADKEY? NOT TFL AND
  IF QX 1+ PXB /                  QY 1+ 40*+S + TO QY PATT C@   
 1+ 0 DO PATT C@   PXB / 1+ 0 DO             QY I + J  40 * +   
 C@ J 4 * I + PATT 1+ + C! LOOP  LOOP PXB 4 = PATT C@ 1 = AND IF
 PATT 1+ C@ 240 AND DUP 16 / +   PATT 1+ C! PATT 5 + C@ 240 AND 
 DUP   16 / + PATT 5 + C! THEN           TID TOU ELSE DROP BUZZ 
 THEN          ; ( ^L )                                      -->
( CIRCLES ) DCX                                                 
                                 ." LOAD MODULES SCREEN 140 "   
 ." RESUME WITH SCREEN 109 "                                    
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 054
( CIRCLES )  -->                : CIRCLE ( X0 Y0 R ... ) YMN    
      YMIN C! 0 TO PHI 0 TO YE   TO XE TO YB TO XB              
                                  BEGIN PHI YE 2* + 1+ TO PHIY  
   PHIY   XE   2* - 1+ TO PHIXY    PLT8 PHIY TO PHI   1 AT YE +!
    PHIXY   ABS PHIY   ABS <        IF PHIXY TO PHI -1 AT XE +! 
    THEN XE   YE   < UNTIL 0        YMIN C! ;                   
                                : SQRT 128 7 0 DO 2DUP / + 2/   
  LOOP SWAP DROP ;                                           -->
( LETTERS )                                                     
: LETTER ( addrst ... )           MCOL1    COLOR                
  8 0 DO                            LETSZ   0 DO                
      DUP J + C@ BPLT 1 AT YV +!    LOOP                        
  LOOP DROP 8 AT XV +! ;        : BNDLET XV  8 + XEND > YV      
  LETSZ  8 * + SCLN   > OR                 ;                    
 : CLMC MAD FRU MACRO C@ 2 < IF    0 TO MACFL ELSE 128 TO MACFL 
  0 TO THFL THEN ;                                           -->
════════════════════════════════════════════════════════════════   SCREEN 055
( LETTERS )                      : TEXT 2DROP OLDSCR            
        YV TO YOLD SPAUSE         CLICK   BEGIN KEY DUP 27 =    
  BNDLET OR NOT WHILE LCNVRT                  LETTER YOLD TO YV 
  REPEAT DROP BUZZ LPAUSE   ;   : LETCOM  CLICK  OLDSCR ZIPFL   
                KEY                               DUP 48 > OVER 
  53 <   AND IF 48 - TO LETSZ            CLICK 7 TO PDF         
  ELSE DROP BUZZ THEN ;                                         
                                                             -->
( MACROS FOR PLOTS-SEE 91 ALSO ) : ENDMAC CLICK                 
          BADKEY? NOT IF CLICK     MIND MACRO C! MAD TOU        
   0 TO MACFL 0 TO PDF ELSE DROP   BUZZ THEN         ;          
 : DEFMAC CLICK  XV TO XOLD YV     TO YOLD 1 TO MIND MACRO 32   
   ERASE 0 TO MACFL 5 TO PDF       MCOL1 COLOR ;                
 79 LOAD                         : MKMA P> 1 COLOR 0 TO 4FL     
    XBRUSH 5 0 DO                 I 10 * 5 + BBT 2/ I 49 + CLMC 
   PLOT  LOOP >P ;                                           -->
════════════════════════════════════════════════════════════════   SCREEN 056
( MACROS )                       : CALLMAC CLICK CLRBAR         
      MKMA BADKEY? MKMA NOT IF      CLMC ELSE DROP BUZZ THEN    
   BFTOBR ;                                                     
: MAKMAC CLICK 2DUP PLOT YOLD     - MIND MACRO + C! XOLD -      
  MIND 1+ MACRO + C! 2 AT MIND    +! MIND 40 > IF BUZZ LPAUSE   
  BUZZ                            ENDMAC THEN LPAUSE   ;        
                                                                
                                                             -->
( DBOX QUILTS TILES )            : DBOX XB  YB  PLOT XB         
  YV  DR. XV  YV  DR. XV           YB  DR.  XB YB  DR. ;        
 : PATFIL                        CLICK QLMEN BADKEY? NOT IF     
 QUD FRU 0 TO TFL 0 TO PRFG 128 TO QFLG MVQLT                   
 1 TO TYPEFL  ELSE 0 TO QFLG     DROP BUZZ THEN BFTOBR ;        
 : TLFIL                         CLICK TLMEN BADKEY? NOT IF     
  TID  FRU 0 TO QFLG             128 TO TFL 0 TO PRFG ELSE 0    
TO TFL DROP BUZZ THEN BFTOBR ;                               -->
════════════════════════════════════════════════════════════════   SCREEN 057
( MSQR  DEFQ DEFT )              : MSQR CLICK KEY DUP DUP 48 >  
   SWAP 52 < AND IF 55 SWAP -    QOFF + C@ DUP 1- TO ?MSK XV    
 PXB DUP */  1- DUP 0< IF PXB    + THEN  DUP TO XV TO QX QX     
 OVER + 1+ TO XB YV TO QY YV +   1+ TO YB  XB XEND < YB SCLN <  
AND IF  PLCOM MCOL1 COLOR DBOX 1    ELSE BUZZ 0 THEN ELSE DROP 0
 BUZZ THEN ;                    : DEFQ MSQR TO QFLG             
  ?MSK QUILT C!          ; ( ^D) : DEFT MSQR TO TFL             
  ?MSK PATT C!          ; ( ^C)                              -->
( RESQ )                         : RESQ CLICK BADKEY? NOT QFLG  
 AND                             IF 1 AT QX +! 1 AT QY +!       
 QUILT C@  1+ 0 DO I 0           QUILT C@ 1+ 0 DO OVER QY + I   
           QX + SWAP LOC.                  IF I QOFF + C@ + THEN
         LOOP SWAP QUILT 1+ + C!           LOOP                 
  QUD TOU                        ELSE DROP BUZZ THEN          ; 
                                 ( ^R )                         
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 058
( DRCIRCLES MACROS )                       -->                  
                                 : GETRAD TO YOLD TO XOLD 1     
 TO P->D  ;                      : DRCIR YOLD -  7FIX     DUP * 
           SWAP XOLD               - DUP * 100 ASPR DUP * */ +  
   SQRT XOLD YOLD ROT CIRCLE 0     TO P->D          ;           
 : ACIRC CLICK  P->D IF DRCIR      ELSE  GETRAD THEN   LPAUSE ; 
                                                                
                             -->                                
( DRLN  PLOT )                   : DRLN CLICK  2DROP DRFL IF    
  QUITLN   XB YB PLOT XV YV DR.    ELSE 1 TO DRFL XV YV 2DUP    
TO YE TO XE TO YB TO XB THEN      LPAUSE ;                      
                                 : PLTS DO 2DUP I + PLOT LOOP ; 
                                 : PL15 THFL IF YMN YMIN C!     
   0 TO MACFL                     3 -2 PLTS SWAP 1+ SWAP 2 -1   
  PLTS SWAP -2 + SWAP 2 -1 PLTS   2DROP 0 YMIN C! ELSE SPLT     
  THEN ;                        ( 156 LOAD )                 -->
════════════════════════════════════════════════════════════════   SCREEN 059
( TRON/OFF PRON/OFF RUBBER )                                    
 : TRON 128 TO TRAN CLICK ;                                     
 : PRDRW CLICK 1 TO PRFG 0         TO QFLG 0 TO TFL BLOB        
   0 TO SPR ;                    : NOPRB CLICK ZIPFL BLOB ;     
                                 : RUBBER YMN YV MAX TO YV DRFL 
   IF (RBND) THEN BXFG IF RBOX     THEN ;                       
                                                                
                             -->                                
( PROBABILITY )                                                 
 : PRSET BARS? NOT IF BAROFF       THEN PLF XMIN C! 3 COLOR     
   0 TO MACFL                      NOPRB 0 TO PDF BBT DUP 2/    
   DO PLF I PLOT PRT I DR. LOOP    2 TO CSTAT PYF TO YV 0 TO CH#
   255 PROB C! 3 RCOL C! ;                                      
 : (PRSTK) DROP XMIN C@ MAX PRT    MIN TO XV PYF TO YV ;        
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 060
( PROBABILITY )                                                 
 : (PRTRG) CLICK XV 1+ PLF -       255 PRT 1+ PLF - */ CH# PROB 
   + C! MCOL1  CH# RCOL + C! CH#   NCLRS 1- = IF PRT TO XV THEN 
   MCOL1  COLOR BBT DUP 2/ DO      XMIN C@ I PLOT XV I DR. LOOP 
   XV PRT = NOT IF XV 1+ XMIN C!   1 AT CH# +! ELSE 60 PAUSE 0  
   TO CSTAT 0 XMIN C! COLJARS      PRDRW THEN   SPAUSE ;        
                                                                
                                                             -->
( BOXES CHK5 )                                                  
                                 : BOXIT CLICK                  
2DROP BXFG IF QUITBX  0 TO BXFG     DBOX ELSE ONBX THEN LPAUSE  
  ;                              : CPLT 2DUP 0< SWAP 0< OR      
   IF 2DROP ELSE PLOT THEN ;                                    
                                 171 LOAD ( SCALING )           
                                 134 LOAD ( ROTATE FLIP )       
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 061
( PDF TABLE  CF, )                                              
: CF, [COMPILE] ' CFA , ;                                       
 LABEL PDF?                      CF, PL15   CF, DRLN            
 CF, FILB   CF, ACIRC            CF, DOBRUSH    CF, MAKMAC      
 CF, BOXIT  ( CF, SPNPT  )       CF, TEXT  CF, DOSCL            
 CF, RIGID CF, GETSEQ                                           
                                                                
                                                             -->
( ETRIG )                       : FLCHC HNT? IF TO CSRCH 0 TO   
 HNT? ELSE  TYPEFL 0= IF DUP    TO MCOL2 TO MCOL1 ELSE WFIL?    
  IF TO MCOL2 0 ELSE TO MCOL1     1 THEN TO WFIL? THEN THEN ;   
: FBC FLCHC BLOB CLICK ;        : (ETRIG) PMY 205 > BOT? AND IF 
  DOBOT ELSE                              YV YMN < IF XV DUP    
  BLF > SWAP BRT < AND  IF        XV YV LOC. (   CHK5  )        
        FBC                            LPAUSE   THEN ELSE MCOL1 
  COLOR XV YV PDF 2* PDF? + @EX           THEN THEN ;        -->
════════════════════════════════════════════════════════════════   SCREEN 062
 ( MAGNIFY )                             HEX                    
 : 3DLST 680 3 70 FILL                          41 6CB C! 680   
   6CC ! 22F C@ 0 22F C! 680       230 ! 22F C! ;               
        DCX                      : MAG PXB 4 = IF CLICK MG? IF  
   OLDL 560 ! 0 TO MG? ONBOT       8 TOLR C!                    
 ELSE    560 @ TO OLDL OFFBOT    4 TOLR C! 3DLST 1 TO MG? THEN  
   THEN ;                                                       
                                                             -->
( PUTNUM   )                                                    
CODE STUFF 0 ,X LDY, 2 ,X LDA,  16 # ORA, LN1 33 + ,Y STA, INY, 
4 ,X LDA, 16 # ORA, LN1 33 + ,Y STA, INY, 6 ,X LDA, 16 # ORA,   
LN1 33 + ,Y STA, INX, INX, INX, INX, POPTWO JMP, C;             
                                : GETDIG 100 /MOD SWAP 10 /MOD  
  ROT  ;                        : PUTNUM YV GETDIG 4 STUFF      
  XV GETDIG 0 STUFF ;                                           
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 063
(  ESTICK P-STICK )                                             
 : M16 DUP 0< IF 16 + THEN 16      MOD ;                        
                                 : (ESTICK) SCLN 1- MIN 0 MAX   
TO YV XEND MIN 0 MAX TO XV DRFL    BXFG OR IF RUBBER THEN ;     
                                 : P-STICK BBT MIN 0 MAX TO YV  
   BRT MIN BLF MAX TO XV XV BLF    - CLO / ?RG DUP TO CREG COL@ 
   HMSK AND                           16 /MOD TO THUE TO TLUM ; 
                                                             -->
(         STICK                )HEX                             
HERE DUP 2DUP   0 , 1 , -1 , 0 ,                                
CODE STICK          ( n -- h v )                                
  B4 C, 00 C, B9 C, 78 C, 02 C,   48 C, CA C, CA C, 29 C, 03 C, 
  0A C, A8 C, B9 C,     , 95 C,   02 C, C8 C, B9 C,     , 95 C, 
  03 C, 68 C, 4A C, 4A C, 29 C,   03 C, 0A C, A8 C, B9 C,     , 
  95 C, 00 C, C8 C, B9 C,     ,   95 C, 01 C, 4C C, ' SWAP ,    
                                CURRENT @ CONTEXT !     DCX  -->
════════════════════════════════════════════════════════════════   SCREEN 064
( P-TRIG                       ) : P-TRIG                       
          DLR! CLICK BEGIN 25     PAUSE DEV? NOT IF 0 STICK     
  TLUM + M16 TO TLUM THUE + M16   TO THUE 644 C@ 0=             
  ELSE 626 C@ 16 228 */ 15 AND    TO TLUM 627 C@ 16 228 */ 15   
  AND TO THUE 638 C@ 0=           THEN  THUE 16 * TLUM + HMSK   
  AND CREG                        COL! UNTIL CLICK LPAUSE   ;   
                                                                
                                                             -->
( STIX TRIX OFFRUB            )                                 
 LABEL STX? CF, (ESTICK)         CF, P-STICK  CF, (PRSTK)       
                                 LABEL TRX? CF, (ETRIG)         
 CF, P-TRIG CF, (PRTRG)                                         
 : STIX CSTAT 2* STX? + @EX        PUTNUM ;                     
 : TRIX CSTAT 2* TRX? + @EX ;                                   
 : OFFRUB QUITLN QUITBX            0 TO P->D ;                  
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 065
( DEV!! CURSOR CONTROLS       )                                 
 : DEV!! (DEV!!) STIX NOT IF       TRIX THEN ;                  
                                 : UP CLICK XV YV 1- STIX ;     
                                 : DWN CLICK XV YV 1+ STIX ;    
                                 : LFT CLICK XV 1- YV STIX ;    
                                 : RGT CLICK XV 1+ YV STIX ;    
                                 : DOT CLICK TRIX          ;    
                                                             -->
( TOSTK TOPD  PNT#   )                                          
 : TOSTK CLICK 0 TO DEV? ;                                      
 : TOPD CLICK 128 TO DEV?                  ;                    
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 066
( COLOR CMMDS )                  CODE KVRT  0 # LDA,            
 0 ,X LDY, 58 #  CPY,             IFCS,      PSHA, THEN,        
  48 # CPY, IFCS, TYA, 48 # SBC,  0 ,X STA, 1 # LDA, PSHA, THEN,
  41 # CPY, IFEQ, 10 # LDA,       0 ,X STA, PSHA, THEN,         
  38 # CPY,                       IFCC, 33 # CPY, IFCS, TYA, 22 
  # SBC, 0 ,X STA, PSHA, THEN,    THEN, PSHA, C;                
: PNT# LSTK 764 C! KEY KVRT       IF NCLRS 1- MIN FBC           
  ELSE DROP THEN ;                                           -->
( MORE FONT STUFF )             : FONTS PDF 4 = NOT IF   ->0    
 ." Insert Program Disk" CR      ." Press <RETURN>" CR          
  KEY DROP                       ." Fonts:" CR CR               
 5 0 DO I 700 .LN CR LOOP CR     ." Pick one:" CR               
 KEY DUP 48 < OVER 52 > OR       IF DROP 0 ELSE 48 - THEN       
 CR ." Font Loading ..."         MVSET 0-> ELSE BUZZ THEN   ;   
 DCX 141 LOAD                                                   
                                                                
════════════════════════════════════════════════════════════════   SCREEN 067
( SIN/COSINE )                  LABEL SNT 0 C, 11 C, 33 C, 43 C,
 54 C, 63 C, 73 C, 82 C, 90 C,   98 C, 104 C, 110 C, 116 C,     
 120 C, 123 C, 126 C, 127 C,     128 C, 128 C,                  
                                 : SN0 5 /    SNT + C@ ;        
 : SN90 180 SWAP - SN0 ;         : SN18  180 - SN0 MINUS ;      
 : SN27 360 SWAP - SN0 MINUS ;   LABEL SIN ' SN0 CFA , ' SN90   
 CFA , ' SN18 CFA , ' SN27 CFA , : SINE DUP 90 / 2* SIN + @EX ; 
 : COS 90 + 360 MOD SINE ;                                   -->
( BLOC VARS  128/     )          VAR DONE? VAR BKOFF VAR DEG    
CODE BLOC                        0 ,X LDY, 2 ,X LDA, XSAVE STX, 
 TAX, CLC, SCLO ,Y LDA, BKOFF    ADC, SCRN STA, SCHI ,Y LDA,    
 BKOFF 1+ ADC, SCRN 1+ STA,      YOFF ,X LDY, SCRN )Y LDA, ONMSK
 ,X AND, TAY, LOCTAB ,Y LDA,     XSAVE LDX, INX, INX, 0 ,X STA, 
 NXT, C;                                   -->                  
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 068
( VFLIP HFLIP )                 : KKB 764 C@  DUP 255 = NOT IF  
  255 764 C! THEN ;             : VFLIP DROP 0-> ZIPFL BRHZ 1+  
 XOLD DO BRVT 1+ YOLD DO J I     BLOC COLOR BRHZ J - XOLD + I   
 PLOT LOOP LOOP CLICK 1     ;   : HFLIP DROP 0-> ZIPFL BRHZ 1+  
 XOLD DO BRVT 1+ YOLD DO J I     BLOC COLOR J BRVT I - YOLD +   
 PLOT LOOP LOOP CLICK 1     ;   : BLHK          BRHZ 1+ XOLD DO 
  BRVT 1+ YOLD DO                 J I PLOT LOOP LOOP ;          
 : BLNK 0 COLOR BLHK ;                                       -->
( ROTATE  DEFINE RIGID )                                        
                                : DFRIG 9 TO PDF ZIPFL 0        
  TO DONE? SCLN 128 > IF 8192   ELSE 4336 THEN TO BKOFF CLICK ; 
 2 LOAD                                                         
: WASH DROP 0-> BLHK CLICK 1 ;                                  
                                                                
                             -->                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 069
( ROTATE CHOICES )              : POSDEG DUP TO DEG 10 DUP 2DUP 
POS. 3 SPACES POS. .   SPAUSE ; : INCDEG DEG 5 + 360 MOD        
  POSDEG ;                      : DECDEG DEG 5 - DUP 0< IF 360 +
  THEN     POSDEG  ;                                            
: ?ROT CLS 10 9 POS. ." angle"    0 POSDEG BEGIN   KKB SEL      
  7 -> INCDEG 6 -> DECDEG         SELEND                        
      ?TERMINAL UNTIL DEG         COS TO THUE DEG SINE TO TLUM  
  BRHZ XOLD + 2/ TO PHI BRVT      YOLD + 2/ TO PHIY ROTATE ; -->
( RGCHC )                       : RETSC 0-> DROP 1 ;            
: RGCHC OLDSCR ->0 CR CR         ." Wipe(W)" CR 0 TO MACFL      
       ." VFlip(V)" CR             ." HFlip(H)" CR ." Rotate(R)"
 CR ." Animate(A)"               CR ." Exit(E)" BEGIN 0 KKB     
      SEL 16 -> VFLIP            57 -> HFLIP 40 -> ?ROT         
 46 -> WASH  63 -> ANWND         42 -> RETSC SELEND             
                   UNTIL ;                                      
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 071
( PMOFF/ON )                                                    
 : CURTOG ( ^O ) CLICK PM? IF      PMOFF 0 TO PM?               
   ELSE PMON  1 TO PM?  THEN             ;                      
                                 : AIR PLCOM 128 TO SPR         
   53 CLMC ;                                                    
                                                                
                                                                
                                                             -->
( ADVANCED KEYS )               : ADVN PNT#          LSTK       
  SEL                           13 -> INSDLI ( I )              
46 -> DELDLI ( W )              104 -> RESQ ( ^R )              
122 -> DEFQ ( ^D )              40  -> MAKEBRS ( R )            
63 -> PRDRW   ( A )              22 -> NOPRB ( X )              
 43 -> PRSET ( Y )               35 -> PICKBRS ( N )            
                             -->                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 072
( ADVANCED KEYS )                                               
 106 -> ENDMAC ( ^E )             54 -> TRON ( < )              
 101 -> DEFMAC ( ^M )             64 -> REST ( ^L )             
  82 -> DEFT  ( ^C )              86 -> XBRUSH ( ^X  )          
 121 -> HUNT   ( ^H )             87 -> SCALIT ( ^Z )           
 110 -> DFRIG  ( ^W )            128 -> TOUDF  ( ctrl L )       
 190 -> FRUDF  ( ctrl S )        191 -> ANIMATE ( ctrl A )      
 186 -> DEFSEQ ( ctrl D )        SELEND      ;               -->
( PAINT10 KEYSTUFF             ): KEYCOM 764 C@ KELIM           
  DUP 255 = IF DROP ELSE                             255 764 C! 
 DUP                             TO LSTK CURCK                  
 IF OFFRUB THEN SEL             37 -> MF ( M )                  
16 -> VF ( V )                  57 -> HF ( H )                  
11 -> UNFILL ( U )              34 -> DOT    ( . )              
61 -> GLO     ( G )             248 -> NXTMEN                   
 85 -> BOTTOM ( ^B )                                         -->
════════════════════════════════════════════════════════════════   SCREEN 073
( PAINT10 KEYBD  )              ( 125 -> GHI     ^G )           
102 -> HLP10 (  ^? )              1  -> AIR  ( J )              
  5  -> ROLLIT  ( K )           ( 2  -> FAST      : )           
127 -> FONTS  ( ^A )             44 -> MIRROR  ( TAB )          
  6 -> LFT  ( CURSORS )          14 -> UP 78 -> WRBNK ( ^up )   
  7 -> RGT                      184 -> CHRFIL                -->
                                                                
                                                                
( PAINT10  )                     15 -> DWN  79 -> RDBNK ( ^dn ) 
21 -> BXCOM   ( B )              0 -> LOADPIC ( L )             
47 -> PQUIT   ( Q )             56 -> FLCOM  ( F )              
58 -> DRCOM   ( D )             10 -> PLCOM   ( P )             
62 -> SVPIC ( S )               42 -> ERSPIC  ( E )             
18 -> CHCOL   ( C )              8 -> SETCOL  ( O )             
23 -> CIRCOM  ( Z )             45 -> THICK   ( T )             
 32 -> LETCOM ( , )                                          -->
════════════════════════════════════════════════════════════════   SCREEN 074
( PAINT10 KEY       )             75 -> CALLMAC ( ^U )          
 109 -> TLFIL ( ^T )            120 -> PATFIL ( ^F )            
 33 -> BAROFF  ( space bar )     126 -> TOSTK ( ^S )            
  74 -> TOPD  ( ^P )                                            
  28 -> RETED  ( ESC )            39 -> MAG    ( ATARI )        
  72 -> CURTOG ( ^O )             NOSEL ADVN                    
  SELEND LSTK CURCK IF LPAUSE     THEN DROP THEN ;              
( 65 -> ONSPLN - ^J )                                        -->
( PAINT10 INITIALIZE   )        : P10-INI   144 106 C! 0 TO BOT?
 0 TO POINT? GMODE GRX SETCOL     559 C@ 0 559 C! INBND ZIPFL   
  BRINIT 0 TO PDF  0 TO DRFL      0 TO BXFG 255 TO XR?          
  1 COLOR 0 TO QUITFL 39 TO XV   50 TO YV 1 TO MCOL1            
0 TO CSTAT 1 RCOL C! 255 PROB C! 0 TO CH# 1 TO MCOL2 0 TO OLDST 
 0 TO SPD COLJARS 0 TO CSTAT     37200 DUP TO PBUFPT            
  NSCM ERASE INIDLI OFFDLI        FLPAD  STVB 559 C! KST ONBOT  
                0 MVSET PMON ;                               -->
════════════════════════════════════════════════════════════════   SCREEN 075
( MENU 1  .... ) VAR ARROW?     169 LOAD                        
: ONARR ARROW? 14 + 13 SWAP POS. ." >" ;                        
: OFFARR ARROW? 14 + 13 SWAP     POS. 2 SPACES ;                
                                 LABEL SELCH ' CHMODE CFA ,     
                ' CHPAD CFA ,                                   
: CHGOPT OFFARR ARROW? 1 XOR      TO ARROW? ONARR CLICK ;       
: CHGSEL CLICK ARROW? 14 + 26     SWAP POS. ARROW? 2* SELCH + @ 
 EXECUTE ;                                                   -->
( MENU1 )                       : MENU1 OPLIST ONARR            
2 18 POS. ." Mode 1 has 1 COL " ." Mode 3 has 1 LUM"            
10 20 POS. ." OPTION to move >" 10 21 POS. ." SELECT to change" 
 10 22 POS. ." START to begin"   BEGIN  SPAUSE 53279 C@ DUP 6 = 
  NOT WHILE SEL 3 -> CHGOPT        5 -> CHGSEL SELEND REPEAT    
  DROP  ;                                                       
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 076
( PAINT10 )                     0 VARIABLE CRCFA                
                                                                
: PAINT10 P10-INI                 BEGIN SPD PAUSE               
        PMCURS            DEV!!                         KEYCOM  
  QUITFL      UNTIL               0 GR.              PMOFF      
       CRCFA @ EXECUTE   ;                                      
                                                                
                                                             -->
( PAINT10 W/HEADER )             OFF HEADLESS                   
: CREDIT  0 GR. 18 710 C!        10 709 C!  1 752 C! 15 2 POS.  
  ." RAMbrandt" 19 4 POS.                ." by " 12 6 POS.      
       ." Bard Ermentrout "      DF% DEFCOL 9 CMOVE             
 11  8 POS. ." (c) Copyright"    18  9 POS. ." 1985 "           
 0 TO ARROW? 0 TO GMODE 0        TO ?PAD           MENU1        
 CHKBNK 2/ TO BKMX PAINT10 ;                                    
  ' CREDIT CFA CRCFA !                                       -->
════════════════════════════════════════════════════════════════   SCREEN 077
( TITLER !! )                                                   
                                                                
: TITLER 2 TO GMODE P10-INI       ->0                  TOUDF    
1 TO RW 9 TO PC# DSKIO           0-> OFFBOT PMOFF               
         BEGIN ?TERMINAL UNTIL    CREDIT ;                      
                                                                
                                                                
                                                                
( VERTICAL BLANK PMG ) HEX                                      
                                 LABEL RTC                      
506 , 605 , 505 , 5 , ( rubst ) 407 , 604 , 404 , 4 , ( fill )  
506 , 605 , 404 , 4 , ( plot )  506 , 505 , 505 , 6 , ( draw )  
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 078
( VERTICAL BLANK PMG ) HEX      107 , 202 , 704 , 0 , ( circle) 
506 , 605 , 505 , 6 , ( box )   207 , 202 , 202 , 0 , ( text )  
403 , 204 , 101 , 6 , ( scale ) 505 , 505 , 707 , 5 , ( wndow ) 
705 , 507 , 505 , 5 , ( macro ) 502 , 705 , 505 , 5 , ( anim )  
 LABEL LTC                      A0E0 , E0 , 0 , 0 ,   ( mode 4) 
A0E0 , E0A0 , 0 , 0 , ( mode 0) 90F0 , F0 , 0 , 0 ,   ( GTIA  ) 
90F0 , F090 , 0 , 0 , ( mode 5) 6060 , F060 , 60F0 , 6060 ,     
                             -->                                
( VERTICAL BLANK STUFF )         LABEL L?                       
 8 C, 10 C, 10 C, 10 C, 0 C,     18 C,                          
 LABEL R?                        10 C, 18 C, 8 C, 20 C, 0 C,    
 48 C, 28 C,  30 C, 38 C, 40 C,  50 C,                          
 : PMOFF 0 TO PM?  BD00 100        ERASE 0 D01D C! 22 22F C! ;  
                                 : PMON PMOFF B8 D407 C!        
   1 TO PM? 2 D01D C! 3A 22F C!    26F 20 OVER C@ OR SWAP C! ;  
 LABEL *CH 0 C,                                              -->
════════════════════════════════════════════════════════════════   SCREEN 079
( PLAYER MISSILE CURSORS )      ( ZERO PAGE 70-74 USED   )      
CODE PMG 0 # LDA, *CH STA,       PM? LDA, IFEQ, 3 # LDX,        
 BEGIN, D000 ,X STA, DEX,        MI UNTIL, E462 JMP, THEN,      
 MG? LDA,                        IFEQ, GMODE LDY, L? ,Y LDA,    
   73 STA, XV LDA, XND LDX,        80 # CPX,                    
   IFCC, ASL.A,          THEN,     CLC, 2F # ADC, 70 STA,       
   YV LDA, YND LDX, 80 # CPX,      IFCC, ASL.A,          THEN,  
   CLC, 1F # ADC, 71 STA,        ELSE,                       -->
( MAGNIFY PMGRAPH )                                             
   20 # LDA, 73 STA,               XV LDA, 18 # CMP,            
    IFCC, ASL.A, ASL.A,             ELSE, 90 # CMP,             
      IFCS, A0 # LDA, XV SBC,               ASL.A, ASL.A,       
            70 STA, A0 # LDA,               70 SBC, 1 # ADC,    
      ELSE, 50 # LDA,                 THEN,                     
    THEN,                          CLC, 30 # ADC, 70 STA,       
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 080
( PMGRAPHICS Y MAGNIFY )           YV LDA, D # CMP,             
    IFCC, ASL.A, ASL.A, ASL.A,      ELSE, GMODE LDX,            
     IFNE, B4 # CMP,                   IFCS, C0 # LDA, YV SBC,  
        ASL.A, ASL.A, ASL.A,            71 STA, C0 # LDA,       
        71 SBC, 1 # ADC,               ELSE, 60 # LDA,          
       THEN,                         ELSE, 54 # CMP,            
       IFCS, 60 # LDA, YV SBC,          ASL.A, ASL.A, ASL.A,    
        71 STA, C0 # LDA,               71 SBC, 1 # ADC,     -->
( PMGRAPHICS )                         ELSE, 60 # LDA,          
       THEN,                         THEN,                      
    THEN,                           CLC, 20 # ADC, 71 STA,      
  THEN,                                                         
                                 70 LDA, D001 STA, 72   LDY,    
 PMX STA,                        0 # LDA, 8 # LDX,              
 BEGIN,                             BD00 ,Y STA, INY, DEX,      
 EQ UNTIL, 71 LDX, PMY STX,                                  -->
════════════════════════════════════════════════════════════════   SCREEN 081
( FINISH UP )                            72 STX, PDF LDY, CLC,  
 R? ,Y LDA, RTC FF AND # ADC,    70 STA, 0 # LDA, RTC 100 / #   
 ADC, 71 STA, CLC,               73 LDA, LTC FF AND # ADC, 73   
 STA, 0 # LDA, LTC 100 / # ADC,  74 STA, 0 # LDY,               
 BEGIN, 70 )Y LDA, 73 )Y ORA,     BD00 ,X STA, INY, INX,        
  8 # CPY,                       EQ UNTIL,                      
 E462 JMP, C;                                                   
                                                                
( BOTTOM BARS VARIABLES   ) HEX  4KCK       LABEL LN1           
 6C30 , 0074 , 7224 , 0077 ,     6926 , 006C , 6923 , 0072 ,    
 6F22 , 0078 , 7834 , 0074 ,     6E35 , 0064 , 6122 , 0072 ,    
 0000 , 0000 , 0000 , 0000 ,     4KCK    LABEL LN2              
 692D , 0078 , 7228 , 007A ,     7236 , 0074 , 692D , 0072 ,    
 6823 , 0067 , 6834 , 006B ,     6921 , 0072 , 6133 , 0076 ,    
 6F2C , 0061 , 782E , 0074 ,     4KCK    LABEL LN3              
 7222 , 0073 , 7433 , 0070 ,     6C31 , 0074 , 6934 , 006C , -->
════════════════════════════════════════════════════════════════   SCREEN 082
( BOTTOM BARS )                  7230 , 0062 , 7234 , 006E ,    
 6E28 , 0074 , 6333 , 006C ,     6E37 , 0064 , 782E , 0074 ,    
 LABEL DLTB 14 ALLOT             LABEL DLB1                     
            42 C, LN1 , 42 C,    LN2 , 41 C, 7036 ,             
 LABEL DLB2                            42 C, LN1 , 42 C, LN2 ,  
 41 C, 7F98 ,                    LABEL OFST 66 C,               
 VAR DXRG VAR DLCL               LABEL DLTEM 9 ALLOT            
 : 66+ OFST C@ + ;                                           -->
( BOTTOMS ) DCX                  LABEL DL0F 92 C, 182 C, 182 C, 
 182 C, 182 C, 92 C,             : DLOF DL0F GMODE + C@ ; HEX   
 : TODLI OFFDLI 230 @ DLOF +       DUP DLTEM 9   CMOVE DLTAB    
  66+ DLTB 14 CMOVE  REGTAB 66+ C@ TO DXRG CDLTAB 66+ C@ TO DLCL
 GC 2 =  IF DLB1 ELSE DLB2 THEN   OVER 9   CMOVE DROP DLTAB  66+
 14 ERASE                         DLTAB 66+ DLOF GC - SWAP C!   
                   9  REGTAB 66+ C!   0 CDLTAB 66+ C!           
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 083
( BOTTOMS )                                                     
  SCLN 8 GC * - TO YMX            SETDLI ;                      
: FRDLI OFFDLI DLTB DLTAB 66+    14 CMOVE      DXRG REGTAB 66+  
  C!  DLCL CDLTAB                  66+ C! DLTEM 230 @ DLOF + 9  
   CMOVE SETDLI SCLN TO YMX ;                                   
                                                                
                                                                
                                                             -->
( BOTTOMS )                                                     
 : BOTTOM MG? NOT IF CLICK BOT?    IF FRDLI 0 TO BOT? ELSE TODLI
   1 TO BOT? 1 TO MEN? THEN THEN  ;                             
 : OFFBOT BOT? IF BOTTOM THEN      0 TO BOT? ; 1 TO MEN?        
                                 : NXTMEN MEN? 1 = IF 2 LN3 ELSE
   1 LN2 THEN 230 @ DLOF +         4 + ! TO MEN? CLICK 20 PAUSE 
 ;                               : ONBOT BOT? NOT IF BOTTOM     
    THEN 1 TO BOT? ;                                         -->
════════════════════════════════════════════════════════════════   SCREEN 084
( BOTTOM KEYS )                  DCX                            
 LABEL KYCN                      10 C, 58 C, 56 C, 23 C, 21 C,  
32 C, 11 C,  33 C, 255 C, 255 C, 37 C, 57 C, 16 C, 44 C, 18 C,  
45 C, 1 C, 62 C, 0 C,   248  C,  75 C, 35 C, 120 C, 109 C, 63 C,
 54 C, 121 C, 87 C, 110 C,  248   C,                            
: DOBOT PMX 46 - 16 / PMY 213 >   MEN? * 10 * + KYCN + C@ 764 C!
           ;                                                    
                                                                
( MENUS )                       : MODELST 2 12 POS. 37 SPACES   
 8 12 POS. GMODE . 15 12 POS.    NCLRS . 22 12 POS. XEND 1+ .   
 29 12 POS. SCLN . ;            : OPLIST 6 11 POS. ." mode" 12  
11 POS. ." #colors" 20 11 POS.  ." across" 28 11 POS. ." down"  
 MODELST 16 14 POS. ." mode" 26  14 POS. GMODE .                
                                 16 15 POS. ." pad" 26 15 POS.  
 ." KOALA" ;                                                    
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 085
( MORE MENUS )                                                  
: CHMODE GMODE 1+ 5 MOD TO GMODE  GMODE . MODELST ;             
                                                                
: CHPAD ?PAD IF 0 ." KOALA" ELSE  1 ." ATARI" THEN TO ?PAD  ;   
                                                                
                                ( chg 5 to 6 in chmode for #5 ) 
                                                                
                                                                
( SCALING ... )                 VAR STEP DCX                    
                                : CHKDEL QUITBX YOLD - 1+ SWAP  
XOLD - 1+ 2DUP 0> SWAP 0> AND ;                                 
 : NOPE 2DROP 0 TO STEP BUZZ ;                                  
 : STP02 TO YOLD TO XOLD ONBX      1 AT STEP +! ;               
                                 : STEP1 CHKDEL                 
 IF TO PHI TO PHIY XOLD TO TLUM    YOLD TO THUE 2 TO STEP       
 ELSE NOPE THEN ;                                            -->
════════════════════════════════════════════════════════════════   SCREEN 086
( SCALING ... )                 : ZAPFL TRAN XBR HUN ZIPFL      
  TO HUN TO XBR TO TRAN ;                                       
 : STEP3 CHKDEL                   IF TO BRHZ TO BRVT   ZAPFL    
   OLDSCR                         BRHZ 0 DO BRVT 0 DO           
  J PHI  BRHZ  Q*/  TLUM +        I PHIY  BRVT Q*/ THUE + LOC.  
  COLOR                           J XOLD + I YOLD + PLOT        
  LOOP *LV LOOP  0 TO STEP CLICK  ELSE NOPE                     
  THEN ;                                                     -->
( SCALING ... )                  : SCALIT CLICK ZIPFL 8 TO PDF  
 0 TO STEP          ;                                           
 LABEL STP? ' STP02 CFA ,        ' STEP1 CFA , ' STP02 CFA ,    
 ' STEP3 CFA ,                                                  
 : DOSCL CLICK STEP 2* STP? +                @EX LPAUSE   ;     
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 087
( BANK SELECT STUFF ) HEX         VAR BKMX                      
: PBNK FFC0 OVER + C! ;         : ?BNK PBNK   1234 C200 2DUP    
  DUP @ >R ! DUP @ R> ROT ! = ; : CHKBNK 0 BEGIN DUP ?BNK WHILE 
  1+ REPEAT ;                   : BMV RW IF ROT SWAP THEN CMOVE 
 ;                              : BKIO OTT> 2*    DUP PBNK 9150 
 C000 1000 BMV 1+ PBNK A150 C000  E00    BMV DLTAB CE00 180 BMV 
  RW IF FLSTK CF80 80 BMV MISLD   ELSE MISSV FLSTK CF80 80 BMV  
  THEN >INN ;                                                -->
( BANK SELECT FINISH UP )        : GETBNK BKMX IF CLICK KEY     
   30 - >R R 0< R BKMX >= OR NOT   R> SWAP IF BKIO ELSE DROP    
   BUZZ THEN THEN ;              : RDBNK 1 TO RW GETBNK ;       
 : WRBNK 0 TO RW GETBNK ;        DCX                            
                                                                
                                                                
                                                                
                                                                
