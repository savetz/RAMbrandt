════════════════════════════════════════════════════════════════   SCREEN 016
( LDS )                         LABEL RW 1 C,                   
: LDS ( ADR SEC # ... )           O+S DO DUP I 1 R/W 128 +      
  LOOP DROP ;                                                   
                                                                
                                                                
                                                                
                                                                
                                                             -->
( DLI TABLES )                                                  
 HEX                             LABEL GPR?                     
 0 C, 40 C, 80 C, C0 C, 0 C,     LABEL DLS?                     
 2 C, 4 C, 4 C, 4 C, 6 C,                                       
 LABEL NEWDL ASSEMBLER           PHA, TXA, PHA, TYA, PHA,       
 0 # LDA, CC # LDX, C4 # LDY,    D40A STA, D01B STA,            
      D017 STY, D018 STX, PLA,   TAY, PLA, TAX, PLA, RTI,       
 FORTH                                                       -->
════════════════════════════════════════════════════════════════   SCREEN 017
( TURN ON GR )                                                  
: INIDSP 0 22F C! 8C20 DUP GMODE  DLS? + C@ 2 LDS 230 !         
  8D00 8 2 LDS                    GMODE GPR? + C@ 26F C!        
  NEWDL 200 ! C0 D40E C!          22 22F C!                     
        BRACT 4 ERASE DFSET        GMODE IF -1000 ELSE -1F10    
   THEN TO BKOFF ;                                              
( THIS WORKS !! )                                               
                                                                
                                 : TEST 100 9150 100 CMOVE      
   INIDSP WAIT 0 GR. ;                                          
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 025
( ZERO PAGE EQUATES )                                           
HEX                              F2 CONSTANT MLPCND             
 F2 CONSTANT RADCND              F3 CONSTANT MLPLER             
 F4 CONSTANT PROD                DC CONSTANT DVDND              
 DA CONSTANT DVSOR               DA CONSTANT TEMP               
 DE CONSTANT RMNDR                                              
 LABEL ROOT 0 ,                                                 
                                                             -->
( MULT SQUARE )                                                 
 CODE MULT 0 # LDA, 8 # LDX,      BEGIN, MLPLER LSR, IFCS,      
  CLC, MLPCND ADC, THEN, ROR.A,   PROD ROR, DEX,                
  EQ UNTIL, PROD 1+ STA, RTS, C;                                
 CODE SQUARE MLPCND LDA, IFMI,   SEC, 0 # LDA, MLPCND SBC,      
 MLPCND STA, THEN, MLPLER STA,   ' MULT JMP, C;                 
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 026
( DIVIDE )                      CODE DIVIDE 0 # LDA, RMNDR STA, 
 RMNDR 1+ STA, 10 # LDX,         BEGIN, DVDND ROL, DVDND 1+ ROL,
  RMNDR ROL, RMNDR 1+ ROL, SEC,   RMNDR LDA, DVSOR SBC, TAY,    
  RMNDR 1+ LDA, DVSOR 1+ SBC,     IFCS, RMNDR STY, RMNDR 1+ STA,
 THEN, DEX, EQ UNTIL, DVDND ROL, DVDND 1+ ROL, RMNDR ASL, RMNDR 
 1+ ROL, IFCC, SEC, DVSOR LDA,   RMNDR SBC, DVSOR 1+ LDA, RMNDR 
 1+ SBC, IFCS, RTS, THEN, THEN,  DVDND INC, IFEQ, DVDND 1+ INC, 
 THEN, RTS, C;                                               -->
( SQUARE ROOT )                  CODE SQRT 8 # LDX, 0 # LDA,    
 ROOT STA, ROOT 1+ STA, TEMP     STA, TEMP 1+ STA,              
 BEGIN, ROOT ASL, ROOT 1+ ROL,   ROOT INC, IFEQ, ROOT 1+ INC,   
 THEN, RADCND ASL, RADCND 1+     ROL, TEMP ROL, TEMP 1+ ROL,    
 RADCND ASL, RADCND 1+ ROL, TEMP ROL, TEMP 1+ ROL, SEC, TEMP    
 LDA, ROOT SBC, TAY, TEMP 1+     LDA, ROOT 1+ SBC, 90 C, 12 C,  
 TEMP 1+ STA, TEMP STY, ROOT     INC, IFEQ, ROOT 1+ INC, THEN,  
 DEX, EQ UNTIL, HERE 14 + JMP,                               -->
════════════════════════════════════════════════════════════════   SCREEN 027
( SQUARE ROOT CONTINUED )        SEC, ROOT LDA, 1 # SBC, ROOT   
 STA, IFCC, ROOT 1+ DEC, THEN,   DEX, D0 C, B1 C, ROOT 1+ ROR,  
 ROOT ROR, RTS, C;                                              
                                ( THESE HAVE BEEN TESTED     )  
                                                                
 HEX                             LABEL RAM 8F80 ,               
 : *+ RAM @ CONSTANT RAM +! ;                                   
                                                             -->
( MODULE # 4 3D OBJECTS )  HEX   2 *+ XCENT     2 *+ YCENT      
 2 *+ RO        2 *+ RI          2 *+ CLIPL     2 *+ CLIPR      
 2 *+ CLIPU     2 *+ CLIPD       1 *+ YREL      2 *+ YSHD       
 2 *+ ZREL      2 *+ ZWX         2 *+ RADIUS    2 *+ TONE       
 2 *+ TNTMP     1 *+ HEMI        1 *+ BAKLIT    1 *+ HVFLAG     
 2 *+ TEM2      1 *+ CNTX        1 *+ CNTY      1 *+ MAXX       
 1 *+ HLEN      2 *+ RS          1 *+ RT        1 *+ RC         
 2 *+ XSHD      1 *+ XREL                                    -->
════════════════════════════════════════════════════════════════   SCREEN 028
( MORE RAM STUFF )               1 *+ PLTFLG                    
 2 *+ XPLT                       1 *+ YPLT                      
 2 *+ VALUE                      1 *+ HTORRN                    
 1 *+ NOSCAL                     2 *+ XSQR      1 *+ XMAXX      
 HLEN CONSTANT R0                PROD CONSTANT SQR              
 DVDND CONSTANT QUOT             MLPCND CONSTANT ARG            
 0 VARIABLE CHCLUP               0 VARIABLE PTPLT2              
 0 VARIABLE RHEMI                                            -->
( PLOTTING STUFF )               VAR ?PLT       HEX             
 8DC0 CONSTANT THRESH                                           
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 029
( PLOTTING )         HEX         CODE GETCOL                    
 HTORRN LDA,                     IFEQ, ( dither ) OLDX LDA, 7 # 
 AND, N STA, OLDY LDA, 7 # AND,  ASL.A, ASL.A, ASL.A, N ORA,    
 TAX, MCOL1 LDY, THRESH ,X LDA,  VALUE CMP, IFMI, MCOL2 LDY,    
 THEN, COLTAB ,Y LDA, COLR STA,  RTS, THEN,                     
 IFMI, ( GTIA ) VALUE LDA,       LSR.A, LSR.A, TAY, COLTAB ,Y   
 LDA, COLR STA, RTS, THEN,       ( random ) MCOL1 LDY, D20A LDA,
 LSR.A, LSR.A, VALUE CMP,                                    -->
( PLOTTING  )                    IFMI, MCOL2 LDY, THEN,         
 COLTAB ,Y LDA, COLR STA, RTS,   C;                             
                                  0 VARIABLE XPL2               
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 030
( QPLT )                         CODE QPLT ' BDS? JSR, IFMI,    
 RTS, THEN, CLC,                SCLO ,Y LDA, BKOFF ADC, SCRN    
 STA, SCHI ,Y LDA, BKOFF 1+ ADC, SCRN 1+ STA, YOFF ,X LDY,      
 ?PLT LDA, IFMI, ( xor ) COLR     LDA, ONMSK ,X AND, SCRN )Y    
 EOR, SCRN )Y STA, RTS, THEN,    SCRN )Y LDA, OFFMSK ,X AND,    
 SCRN )Y STA, COLR LDA, ONMSK ,X AND, SCRN )Y ORA,              
 SCRN )Y STA, RTS, C;                                           
                             -->                                
( PLTSHD ) DCX                   CODE PLTSHD YPLT LDY, INY,     
 MLPLER STY, 204 # LDA, MLPCND   STA, ' MULT JSR, YPLT STA, SEC,
 191 # LDA, YPLT SBC, YPLT STA,  GMODE LDY, IFEQ, 1 # AND,      
 IFNE, RTS, THEN, YPLT LSR,      THEN, YPLT LDA, OLDY STA,      
 XPLT LDA, XPL2 STA, XPLT 1+     LDA, XPL2 1+ STA,              
 GMODE LDA, 3 # AND, IFEQ,       XPL2 LDA, 1 # AND, IFNE, RTS,  
 THEN, CLC, XPL2 1+ LSR, XPL2    ROR, ELSE, XPL2 LDA, 3 # AND,  
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 031
( PLOT TESTER )                  IFNE, RTS, THEN, CLC, XPL2 1+  
 LSR, XPL2 ROR, XPL2 LSR, THEN,  XPL2 LDA,  OLDX STA, ' GETCOL  
 JSR, ' QPLT JMP, C;                                            
  ;S                                                            
                                CODE PLOT ( val y x ... )       
 0 ,X LDA, XPLT STA, 1 ,X LDA,   XPLT 1+ STA, 2 ,X LDA, YPLT    
 STA, 4 ,X LDA, VALUE STA,       INX, INX, XSAVE STX, ' PLTSHD  
 JSR, XSAVE LDX, POPTWO JMP, C;                                 
( INITIALIZE )                  DCX                             
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 035
( MODULE # 4 3D OBJECTS )  HEX   2 *+ XCENT     2 *+ YCENT      
 2 *+ RO        2 *+ RI          2 *+ CLIPL     2 *+ CLIPR      
 2 *+ CLIPU     2 *+ CLIPD       1 *+ YREL      2 *+ YSHD       
 2 *+ ZREL      2 *+ ZWX         2 *+ RADIUS    2 *+ TONE       
 2 *+ TNTMP     1 *+ HEMI        1 *+ BAKLIT    1 *+ HVFLAG     
 2 *+ TEM2      1 *+ CNTX        1 *+ CNTY      1 *+ MAXX       
 1 *+ HLEN      2 *+ RS          1 *+ RT        1 *+ RC         
 2 *+ XSHD      1 *+ XREL                                    -->
( MODULE 4 3D OBJECTS )          HEX                            
 2 *+ XSQR      1 *+ XMAXX                                      
 HLEN CONSTANT R0                PROD CONSTANT SQR              
 DVDND CONSTANT QUOT             MLPCND CONSTANT ARG            
                                 0 VARIABLE CHCLUP              
 0 VARIABLE PTPLT2               0 VARIABLE RHEMI               
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 036
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
════════════════════════════════════════════════════════════════   SCREEN 037
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
════════════════════════════════════════════════════════════════   SCREEN 038
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
════════════════════════════════════════════════════════════════   SCREEN 039
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
════════════════════════════════════════════════════════════════   SCREEN 041
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
════════════════════════════════════════════════════════════════   SCREEN 042
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
════════════════════════════════════════════════════════════════   SCREEN 043
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
════════════════════════════════════════════════════════════════   SCREEN 044
( SPOOL )                        DVSOR STA, ' SDIV JSR, QUOT    
 LDA, SEC, CNTY SBC, YSHD STA,   QUOT 1+ LDA, 0 # SBC, YSHD 1+  
 STA, ' PTPLOT JSR, CNTY LDA,    MAXX CMP,                      
 NE WHILE, CNTY INC,             REPEAT, CNTX LDA, RT CMP,      
 NE WHILE, CNTX INC,             REPEAT, RTS, C;                
                                                                
                                                                
                                                             -->
( DEFAULTS   )                    DCX                           
 CODE DSPHERE                    XSAVE STX, ' SPHER  JSR,       
 XSAVE LDX, NXT, C;                                             
 CODE DCYLNDR                    XSAVE STX, ' CYLNDR JSR,       
 XSAVE LDX, NXT, C;                                             
 CODE DEDGTOR                    XSAVE STX, ' EDGTR JSR,        
 XSAVE LDX, NXT, C;                                             
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 045
( DEFAULTS )                                                    
 CODE DTORUS                     XSAVE STX, ' TOROID JSR,       
 XSAVE LDX, NXT, C;                                             
 CODE DSPOOL                     XSAVE STX, ' SPUUL  JSR,       
 XSAVE LDX, NXT, C;                                             
                                                                
                                                                
                                                                
( SET UP PARAMS FOR CURSORS )                                   
LABEL HORZ 4 ALLOT              LABEL VERT 4 ALLOT              
 LABEL GTPRMS 20 ALLOT                                          
 ASSEMBLER                                                      
                                                                
                             -->                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 046
( PARAMS HORIZONTAL )           HERE GTPRMS !                   
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
════════════════════════════════════════════════════════════════   SCREEN 047
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
════════════════════════════════════════════════════════════════   SCREEN 049
( CURPARM )                      VAR YCR VAR XCR                
 : CURPARM ( obj # ...)            VERT 4 ERASE HORZ 4 ERASE    
   DO-OBJ CLIPM 4 0 DO I VERT +    DUP C@ SCLY SWAP C! I HORZ + 
   DUP C@ SCLX SWAP C! LOOP        XCENT @ SCLX TO XCR          
   GMODE IF 191 ELSE 95 THEN       YCENT C@ SCLY - TO YCR       
   2 0 DO                        I VERT + C@ I VERT 2 + + C@ =  
 IF 0 VERT 2 + I + C! THEN       I HORZ + C@ I HORZ 2 + + C@ =  
 IF 0 HORZ 2 + I + C! THEN            LOOP                   -->
( CURPARM )                      VERT    CHGSGN                 
 VERT 2 + CHGSGN                 HORZ    CHGSGN                 
 HORZ 2 + CHGSGN ;                                              
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 051
( CURSORS )                      CODE 9CUR XSAVE STX, 128 # LDA,
 ?PLT STA, 255 # LDA, COLR            STA, XCR LDA, OLDX STA,   
 YCR LDA, OLDY STA, ' ACUR JSR,  3 # LDA, N 2 + STA,            
 BEGIN, N 2 + LDY, CLC, HORZ ,Y  LDA, IFNE, XCR ADC, OLDX STA,  
 YCR LDA, OLDY STA, ' ACUR JSR,  THEN, N 2 + LDY, CLC, VERT ,Y  
 LDA, IFNE, YCR ADC, OLDY STA,   XCR LDA, OLDX STA, ' ACUR JSR, 
 THEN, N 2 + DEC, MI UNTIL,      0 # LDA, ?PLT STA, XSAVE LDX,  
 NEXT JMP, C;                                                   
( BNDCHK )                       VAR OBJ# VAR PAR#              
 ;                               : BNDCHK XCENT @ 319 OVER -    
   CLIPR @ MIN CLIPR !             CLIPL @ MIN CLIPL !          
   YCENT @ 239 OVER - CLIPU @      MIN CLIPU ! CLIPD @ MIN CLIPD
   !    OBJ# DUP 1= SWAP 3 > OR    IF RI @ RO @ MIN RI ! ELSE   
   RO @ RADIUS ! RI @ HLEN C!      THEN ;                       
                                                                
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 060
( CONSTANTS FOR PAINT 10  )      148 LOAD                       
 DCX                             LABEL SCHI 192 ALLOT           
 LABEL SCLO 192 ALLOT            LABEL OFFMSK 160 ALLOT         
 LABEL ONMSK 160 ALLOT           LABEL YOFF 160 ALLOT           
                                                                
                                                                
                                                                
                                                             -->
 ( CONSTANTS PAINT 10 )          LABEL COLTAB 16 ALLOT          
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 061
( CONSTANTS FOR P10           )  LABEL BRACT 4 ALLOT            
 LABEL OLDCOL 10 ALLOT           VAR ?MSK  VAR GMODE            
 : MDTAB <BUILDS DOES> GMODE       + C@ ;                       
 VAR XOLD VAR YOLD VAR BRVT      VAR BRHZ                       
                                                                
                                                                
 HEX                                                            
                                                             -->
( CONSTANTS FOR PAINT         ) MDTAB NCLRS ( 9 C, FOR MD 5 )   
4 C, 10 C, 9 C, 10 C, 4 C,      MDTAB SCLN 60 C, C0 C, C0 C,    
C0 C, C0 C,                     MDTAB GR? ( 17 C, )             
17 C, 9 C, A C, B C, 18 C,      MDTAB XEND 9F C, 4F C, 4F C,    
4F C, 9F C, ( 4F C, )                                        -->
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 064
( MORE CONSTANTS )                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
( GR 7PLUS                     )CODE 7PLUS                ( -- )
  A9 C, 07 C, 85 C, 57 C, AD C,   30 C, 02 C, 85 C, N  C, AD C, 
  31 C, 02 C, 85 C, N 1+ C, HERE  B1 C, N  C, 29 C, FC C, C9 C, 
  40 C, F0 C, 14 C, B1 C, N C,    85 C, N 2+ C, 29 C,           
  0F C, C9 C, 0F C, D0 C, 06 C,   C6 C, N 2+ C, A5 C, N 2+ C,   
  91 C, N  C, C8 C, 4C C, ,       4C C, NEXT ,  C;              
CODE LO-HI ( wd -- lo hi )      1 ,X LDA, 1 ,X STY, PUSH0A JMP, 
 C; DCX                           : 128 128 ;                -->
════════════════════════════════════════════════════════════════   SCREEN 065
( HISPEED GRAPHICS TABLES )     BASE @ HEX                      
                                                                
                                 F0         CONSTANT OLDX       
 OLDX 1 +   CONSTANT OLDY                                       
                                                                
                                                                
                                                                
                                                             -->
( ZERO PAGE EQUATES )                                           
 OLDX A +   CONSTANT XMAX        OLDX B +   CONSTANT XMIN       
 OLDX C +   CONSTANT YMAX        OLDX D +   CONSTANT YMIN       
 OLDX 7 +   CONSTANT COLR        OLDX 8 +   CONSTANT SCRN       
                                                             -->
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 067
VAR BKOFF                       : DFSET ;                       
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
( GRAPHICS )                     HEX  0 AFL C!                  
: XTAB A0 0 DO I PXB /MOD YOFF I  + C! ?MSK + C@ DUP ONMSK I +  
  C! FF XOR OFFMSK I + C! LOOP ;                                
                                                                
                                                                
                             -->                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 075
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
════════════════════════════════════════════════════════════════   SCREEN 076
( MORE REDEFINITIONS )                                          
 : : 0 AFL C! [COMPILE] : ;      IMMEDIATE                      
                                 : CODE 1 AFL C! [COMPILE] CODE 
 ; IMMEDIATE                                                    
 : C; 0 AFL C! [COMPILE] C; ;     IMMEDIATE                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
