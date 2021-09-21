════════════════════════════════════════════════════════════════   SCREEN 005
( TABLE PRODUCTION )             LABEL CONTAB 256 ALLOT         
 LABEL CONCOL 6 ALLOT            LABEL CONCODE 126 ALLOT        
 0 VARIABLE CMAX  4 CONSTANT H4  0 VARIABLE COL#                
 0 VARIABLE LEFT                 0 VARIABLE RIGHT               
 LABEL YELLOW 480 ALLOT          LABEL RED 480 ALLOT            
 LABEL BLUE 480 ALLOT            0 VARIABLE ROW#                
 0 VARIABLE ROWMSK               LABEL ROWLST 84 ALLOT          
 LABEL (MSK) 3 C, 12 C, 48 C,    192 C,                      -->
( CHOOSER )                      0 CONSTANT STR  10 CONSTANT AR 
 : GETNUM CR QUERY BL WORD HERE   NUMBER DROP ;                 
 : STBK DUP DUP 710 C! 12 + 16     MOD 709 C! 16 /MOD ;         
 160 CONSTANT NCL                                               
  50 LOAD                                                       
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 006
( CONVERSION )                                                  
 CODE (CONVRT) ( oct addr ... )  0 ,X LDA, N STA, 1 ,X LDA, N 1+
 STA,                            BEGIN, 2 ,X ROR,               
  IFCS, 1 # LDA, N )Y STA, THEN,  INY, 3 # CPY,                 
 EQ UNTIL, 0 # LDY,              BEGIN, 2 ,X ROR,               
  IFCS, 2 # LDA, N )Y ORA,              N )Y STA, THEN,         
  INY, 3 # CPY, EQ UNTIL,         POPTWO JMP, C;                
                                                             -->
( CONVERTER )                                                   
 : CONV1 CONCOL 6 ERASE            LEFT C@ CONCOL (CONVRT)      
  RIGHT C@ CONCOL 3 + (CONVRT)    6 0 DO CONCOL I + DUP C@      
  85 * SWAP C! LOOP CONCOL        CONTAB COL# C@ 6 * + 6 CMOVE ;
                                 : CONVERT                      
   CMAX C@ 0 DO I 3 * CONCODE +    DUP 1+ C@ LEFT C! 2 + C@     
   RIGHT                           C! I COL# C! CONV1 LOOP ;    
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 007
( LOCATE )                           40 LOAD -->                
 41216 CONSTANT PBUFPT           LABEL MASKS 192 C, 48 C, 12 C, 
 3 C,                            LABEL LOCTAB 256 ALLOT         
 : 40* 2* 2* 2* DUP 2* 2* + ;    : MAKLOC 1 4 0 DO              
            4 0 DO DUP I * I       SWAP LOCTAB + C! LOOP 4 *    
   LOOP DROP ;                   : LOC. ( row col ... )         
   4 /MOD SWAP MASKS + C@ >R       SWAP 40* PBUFPT + + C@ R>    
   AND LOCTAB + C@ ;                                         -->
( SETTING UP STUFF FOR PRINT )                                  
 LABEL ACREG 16 ALLOT                                           
 CODE SETCOL                     ( addr1 # addr2 ... addr1 )    
 0 ,X LDA, N STA, 1 ,X LDA, N 1+ STA, 2 ,X LDY, 4 ,X LDA, N 2 + 
 STA, 5 ,X LDA, N 3 + STA,       N 2 + )Y LDA, ROWMSK AND, 0 #  
 LDY, N )Y ORA, N )Y STA,        POPTWO JMP, C;                 
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 008
( ONEROW )                            43 LOAD -->               
 : INIROW                          YELLOW 480 ERASE BLUE 480    
   ERASE RED 480 ERASE ;         : ONEROW ( row# ... )          
   NCL 0 DO DUP I 10 * AR /       LOC. ACREG + C@ 6 * CONTAB +  
  0 YELLOW I 2* + 32 + SETCOL     1 RED    I 2* + 32 + SETCOL   
  2 BLUE   I 2* + 32 + SETCOL     3 YELLOW I 2* 1+ + 32 + SETCOL
  4 RED    I 2* + 1+ 32 + SETCOL  5 BLUE   I 2* + 1+ 32 + SETCOL
    DROP                           LOOP DROP ;               -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 009
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 010
( GR 7PLUS )                        42 LOAD -->                 
 HEX                            CODE 7PLUS                ( -- )
  A9 C, 07 C, 85 C, 57 C, AD C,   30 C, 02 C, 85 C, N  C, AD C, 
  31 C, 02 C, 85 C, N 1+ C, HERE  B1 C, N  C, 29 C, FC C, C9 C, 
  40 C, F0 C, 14 C, B1 C, N C,    85 C, N 2+ C, 29 C,           
  0F C, C9 C, 0F C, D0 C, 06 C,   C6 C, N 2+ C, A5 C, N 2+ C,   
  91 C, N  C, C8 C, 4C C, ,       4C C, NEXT ,  C;  DCX         
 : GRX 24 GR. 7PLUS ;            : OFFDLI 64 54286 C! ;      -->
( DLINT FOR PAINT10 )           LABEL DLTAB 128 ALLOT           
LABEL REGTAB 128 ALLOT          LABEL CDLTAB 128 ALLOT          
                                HEX                             
                                CODE DLIROUT 48 C, 8A C, 48 C,  
 98 C, 48 C, AE C, D40B , BC C, REGTAB , 30 C, 9 C, BD C,       
CDLTAB , 8D C, D40A , 99 C,     D012 , 68 C, A8 C, 68 C, AA C,  
68 C, 40 C, C;                  DCX                             
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 011
( MORE DLI STUFF )               0 VARIABLE DLIND               
 0 VARIABLE SECPTR                                              
 : ONDLI ' DLIROUT 512 ! 192       54286 C! ;                   
 : MVDLI DLTAB SECPTR @ 61 + 1     R/W REGTAB SECPTR @ 62 + 1   
   R/W CDLTAB SECPTR @ 63 + 1      R/W ;                        
 : SETDLI OFFDLI 560 @ 128 0 DO    DUP I DLTAB + C@ DUP IF + DUP
   C@ 128 OR SWAP C! ELSE 2DROP    THEN LOOP DROP ONDLI ;       
                             -->                                
( MOVEPIC )                                                     
 : MVPIC ( pic# ... )              160 106 C! 0 GR. 64 *        
   1+ SECPTR ! 60 0 DO I 128 *     PBUFPT + SECPTR @ I + 1 R/W  
   LOOP DLTAB SECPTR @ 60 + 1      R/W GRX PBUFPT 88 @ 7680     
   CMOVE  DLTAB 704 9 CMOVE        MVDLI SETDLI WAIT ;          
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 012
( INIREG )                          41 LOAD -->                 
: INIREG                          712 C@ CONCODE C!             
  708 C@ CONCODE 3 + C!           709 C@ CONCODE 6 + C!         
 710 C@ CONCODE 9 + C! 4 COL# C!  STR 10 + ' AR ! AR 160 * 10 / 
  ' NCL !                         4 0 DO I ACREG I + C! LOOP    
  ROWLST 9 + C@ ROW# C!   ;      : RX DUP 8 = IF DROP 0 ELSE    
   3 - THEN ;                    0 VARIABLE PAR                 
                                                             -->
( FNDDLI )                                                      
 : FNDDLI H4 CMAX C! 255 ROWLST    H4 2* 1+ + C!                
   128 0 DO I REGTAB + C@ DUP      10 < I DLTAB + C@ 0= NOT AND 
 IF RX ROWLST CMAX C@ 2* + C!         CDLTAB I + C@ CONCODE     
      CMAX C@ 3 * + C!                DLTAB I + C@ DUP 100 <    
      IF 3 ELSE 5 THEN - ROWLST       CMAX C@ 2* 1+ + C! CMAX C@
      1+ DUP 41 > IF DROP 41          THEN CMAX C!              
   ELSE DROP  THEN LOOP 255 CMAX  C@ 2* ROWLST 1+ + C! ;     -->
════════════════════════════════════════════════════════════════   SCREEN 013
( TEST1 )                                                       
 : INITAB MAKLOC                   CONTAB  256 ERASE            
   CONCODE 126 ERASE               ROWLST 84 ERASE ;            
                                                                
                                                                
                                                                
                                                                
                                                             -->
( CODE STUFF )                                                  
 CODE PCHK PAR LDA, IFNE,        CLC, 0 ,X LSR, THEN, 0 ,X LDA, 
 127 # AND, 0 ,X STA, NEXT JMP,   C;                            
                                 CODE DLCHK  0 ,X LDA, ROW# CMP,
 IFEQ, XSAVE STX, COL# LDA,      ASL.A, TAY, ROWLST ,Y LDA, TAX,
 COL# LDA, ACREG ,X STA, COL#    INC, COL# LDA, ASL.A, TAY,     
 ROWLST 1+ ,Y LDA, ROW# STA,     XSAVE LDX, THEN, NEXT JMP, C;  
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 014
( DUMPING STUFF )                                               
 : SENDROWS                      138 EMIT 13 EMIT 153 EMIT      
 460 0 DO YELLOW I + C@ PCHK     EMIT LOOP CR                   
 460 0 DO RED I + C@ PCHK EMIT   LOOP CR                        
 460 0 DO BLUE I + C@ PCHK EMIT  LOOP CR ;                      
 : 4ROWS INIROW                    4 0 DO DUP I + DLCHK         
   I 4 MOD (MSK) + C@ ROWMSK C!    ONEROW LOOP ;                
                             -->                                
( DUMPING )                                                     
 : 7ROWS 0 PAR ! 4ROWS SENDROWS    1 PAR ! 3 + 4ROWS SENDROWS   
   DROP ;                                                       
                                 : INITIALIZE  (  pic# ... )    
   INITAB  MVPIC                   FNDDLI INIREG 0 GR.          
   CHOOSER PBUFPT 7680 + 200       ERASE            ;           
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 015
( DUMPIT )                                                      
 : DUMPIT                         INITIALIZE  CONVERT H4 COL# C!
   2 PFLAG ! 27 EMIT 37 EMIT       0 559 C! CR CR               
   27 0 DO I 7 * 7ROWS LOOP         189 0 PAR ! 4ROWS SENDROWS  
    CR CR 34 559 C! 145 EMIT       1 PFLAG ! ;                  
                                                                
                                                                
                                                                
( DUMPIT )                                                      
 : DUMPIT                          INITIALIZE  CONVERT          
   2 PFLAG ! 27 EMIT 37 EMIT       0 559 C! CR CR               
   28 0 DO I 7 * 7ROWS LOOP         CR CR 34 559 C! 145 EMIT    
   1 PFLAG ! ;                                                  
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 020
( 10 LOCTAB - SCREEN 14 )       LABEL LOCTAB 256 ALLOT          
LABEL MASKS 240 C, 15 C,        41216 CONSTANT PBUFPT           
 9 CONSTANT H9                   : 40* 2* 2* 2* DUP 2* 2* + ;   
 : MAKLOC 1 2 0 DO                         16 0 DO DUP I * I    
           H9 MOD                 SWAP LOCTAB + C! LOOP 16 *    
  LOOP DROP ;                    : LOC. ( row col ... reg )     
   2 /MOD SWAP MASKS + C@ >R       SWAP 40* PBUFPT + + C@ R>    
   AND LOCTAB + C@ ;             9 ' H4 !                       
( INIREG MODE 10 SCR 24 )          45 LOAD                      
 : INIREG 9 0 DO I 704 + C@        I 3 * CONCODE + C! LOOP      
   9 COL# C!                       9 0 DO I ACREG I + C! LOOP   
   ROWLST H4 2* + 1+ C@ ROW# C!    STR 10 + ' AR ! 80 AR * 10 / 
   ' NCL ! 11FIX ;               : RX ;                         
0 VARIABLE PAR                                                  
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 021
( MODE 10 GR.  SCREEN 20 )                                      
 : GRX 10 GR. ;                                                 
 : OFFDLI 64 54286 C! ;                                         
 : GRX 11 GR. 16 ' H9 ! ;                                       
                                                                
                                                                
                                                                
                                                                
( ONEROW  SCREEN 16 )            : 4* 2* 2* ;                   
 : ONEROW ( row# ... )            NCL 0 DO DUP I 10 * AR /      
 LOC. ACREG +   C@ 6 * CONTAB +   0 YELLOW I 4* + 32 + SETCOL   
  1 RED    I 4* + 32 + SETCOL     2 BLUE   I 4* + 32 + SETCOL   
  3 YELLOW I 4* 2+ + 32 + SETCOL  4 RED    I 4* + 2+ 32 + SETCOL
  5 BLUE   I 4* + 2+ 32 + SETCOL    DROP  LOOP DROP             
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 022
( REST OF 10-FIX )               NCL 2* 0                       
  DO I 2* 32 + DUP YELLOW +      DUP C@ SWAP 1+ C! DUP BLUE +   
 DUP C@ SWAP 1+ C! RED + DUP C@  SWAP 1+ C! LOOP ;              
                                 : INIROW                       
   YELLOW 480 ERASE BLUE 480       ERASE RED 480 ERASE ;        
                                                                
                                                                
                                                                
                                 : 11FIX 16 CMAX ! 255 ROW# C!  
   16 0 DO I ACREG I + C!          I 16 * 712 C@ 15 AND + I     
   3 * CONCODE + C! LOOP 16        COL# C! ;                    
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 025
( SETUP DLI )                            HEX                    
LABEL VBR ASSEMBLER              PHA, 0 # LDA, F0 STA, PLA,     
 E462 JMP,                       FORTH                          
                                LABEL PICOL 18 ALLOT            
                                CODE SETVB XSAVE STX, 0 ,X LDA, 
 PHA, 1 ,X LDA, TAX, PLA, TAY,   7 # LDA, E45C JSR, XSAVE LDX,  
 POP JMP, C;                                                    
                                                             -->
( DLI SCREEN )                                                  
 CODE MENDL PHA, TYA, PHA,       F0 LDY, PICOL ,Y LDA, TAY,     
 FF # EOR,  D40A STA, D018 STY,  D017 STA,  F0 INC, PLA, TAY,   
 PLA, RTI, C;                                                   
 : INISCR 0 GR. 1D 6 DO 230 @      I + DUP C@ 80 OR SWAP C!     
   LOOP VBR SETVB ' MENDL 200 !    230 @ 3 + DUP C@ 80 OR SWAP  
   C!                              C0 D40E C! ;                 
 DCX                                                         -->
════════════════════════════════════════════════════════════════   SCREEN 026
( CHOOSER PATCH )                                               
 : POS. 84 C! 85 ! ;                                            
 : SCAD ( col row ... addr )       40 * 88 @ + + ;              
                                 : SETPT ( b x y ... ) SCAD >R  
   16 + R> C! ;                                                 
 : GETPT ( x y ... b ) SCAD C@     16 - ;                       
                                                                
                                                             -->
( CHOOSER  VARIABLES )                                          
 0 VARIABLE HP                   0 VARIABLE VP                  
 0 VARIABLE COLST                0 VARIABLE COLEND              
                                 23 CONSTANT LSTY               
 LABEL HPOS 15 C, 22 C, 27 C,    35 C,                          
                                 : ZCOL ( col ... off )         
   COLST C@ - 1+ ;                                              
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 027
( DO-LINE )                      : DO-LINE ( col# ... ) >R R 3 *
   CONCODE + DUP C@ DUP R ZCOL     1 - PICOL + C! 16 /MOD       
   4 R ZCOL POS. . 10 R ZCOL       POS. . DUP 1+ C@ 8 /MOD 15 R 
   ZCOL POS. . 22 R ZCOL POS. .    2 + C@ 8 /MOD 27 R ZCOL POS. 
   . 35 R> ZCOL POS. . ;                                        
                                                                
                                                                
                                                             -->
( INIMEN CURS )                                                 
 : INIMEN INISCR 2 0 POS.          ." COLOR  LUM" 15 0 POS.     
   ." T-LEFT-B" 27 0 POS.          ." T-RIGHT-B"                
   COLEND @ COLST @ 2DUP           DO I DO-LINE LOOP - ' LSTY ! 
   1 752 C! ;                                                   
 : CURS HP C@ HPOS + C@ VP C@      SCAD 128 OVER C@ XOR SWAP C! 
 ;                                                              
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 028
( CURSOR CMMDS )                 : FIXVT ( x ... x ) DUP 0= IF  
   DROP LSTY ELSE DUP LSTY 1+      = IF DROP 1 THEN THEN ;      
 : NOKY 255 764 C! ;             : LFT CURS HP C@ 1+ 4 MOD HP C!
   CURS NOKY ;                   : RGT CURS HP C@ 3 + 4 MOD HP  
   C! CURS NOKY ;                : UPP CURS VP C@ 1 - FIXVT VP  
   C! CURS NOKY ;                : DON CURS VP C@ 1+ FIXVT VP C!
   CURS NOKY ;                   : RNG DUP 0 >= OVER 8 < AND ;  
                                                             -->
( DOT REVERT )                  : DOT KEY 48 - RNG NOT IF DROP  
  ELSE   HP C@ HPOS +             C@ VP C@ SETPT HP C@ 1+ 4 /MOD
 IF VP C@ 1+ FIXVT VP C! THEN    HP C! CURS THEN ;              
: REVERT                          COLEND @ COLST @ DO           
  I ZCOL >R 15 R GETPT 8 *        22 R GETPT +  27 R GETPT 8 *  
  35 R> GETPT + I 3 * CONCODE +   1+ SWAP OVER 1+ C!  C!        
 LOOP ;                          : PAUSE ( jif ... ) 540 C!     
   BEGIN 540 C@ 0= UNTIL ;                                   -->
════════════════════════════════════════════════════════════════   SCREEN 029
( CHOOSER )                      : (CHOOS)                      
            INIMEN 1 VP ! 0 HP !   CURS BEGIN 764 C@ DUP 255 =  
   IF DROP ELSE                    SEL 15 -> DON                
       7  -> LFT                       6  -> RGT                
       14 -> UPP                       NOSEL DOT                
   SELEND                          20 PAUSE                     
   THEN                           ?TERMINAL UNTIL CURS REVERT   
   0 23 POS. ;                                               -->
( CHOOSER )                                                     
 : CHOOSER CMAX C@ DUP 23 >        IF DROP 23 COLEND ! 0 COLST !
      (CHOOS) 30 PAUSE 23             COLST ! CMAX C@ COLEND !  
      (CHOOS)                      ELSE COLEND ! 0 COLST !      
      (CHOOS)                      THEN ;                       
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 050
( TABLE PRODUCTION )             LABEL CONTAB 256 ALLOT         
 LABEL CONCOL 6 ALLOT            LABEL CONCODE 126 ALLOT        
 0 VARIABLE CMAX  4 CONSTANT H4  0 VARIABLE COL#                
 0 VARIABLE LEFT                 0 VARIABLE RIGHT               
 LABEL YELLOW 480 ALLOT          LABEL RED 480 ALLOT            
 LABEL BLUE 480 ALLOT            0 VARIABLE ROW#                
 0 VARIABLE ROWMSK               LABEL ROWLST 84 ALLOT          
 LABEL (MSK) 3 C, 12 C, 48 C,    192 C,                      -->
( CHOOSER )                      0 CONSTANT STR  10 CONSTANT AR 
 : GETNUM CR QUERY BL WORD HERE   NUMBER DROP ;                 
 : STBK DUP DUP 710 C! 12 + 16     MOD 709 C! 16 /MOD ;         
 160 CONSTANT NCL                                               
  50 LOAD                                                       
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 051
( CONVERSION )                                                  
 CODE (CONVRT) ( oct addr ... )  0 ,X LDA, N STA, 1 ,X LDA, N 1+
 STA,                            BEGIN, 2 ,X ROR,               
  IFCS, 1 # LDA, N )Y STA, THEN,  INY, 3 # CPY,                 
 EQ UNTIL, 0 # LDY,              BEGIN, 2 ,X ROR,               
  IFCS, 2 # LDA, N )Y ORA,              N )Y STA, THEN,         
  INY, 3 # CPY, EQ UNTIL,         POPTWO JMP, C;                
                                                             -->
( CONVERTER )                                                   
 : CONV1 CONCOL 6 ERASE            LEFT C@ CONCOL (CONVRT)      
  RIGHT C@ CONCOL 3 + (CONVRT)    6 0 DO CONCOL I + DUP C@      
  85 * SWAP C! LOOP CONCOL        CONTAB COL# C@ 6 * + 6 CMOVE ;
                                 : CONVERT                      
   CMAX C@ 0 DO I 3 * CONCODE +    DUP 1+ C@ LEFT C! 2 + C@     
   RIGHT                           C! I COL# C! CONV1 LOOP ;    
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 052
( LOCATE )                           40 LOAD -->                
 41216 CONSTANT PBUFPT           LABEL MASKS 192 C, 48 C, 12 C, 
 3 C,                            LABEL LOCTAB 256 ALLOT         
 : 40* 2* 2* 2* DUP 2* 2* + ;    : MAKLOC 1 4 0 DO              
            4 0 DO DUP I * I       SWAP LOCTAB + C! LOOP 4 *    
   LOOP DROP ;                   : LOC. ( row col ... )         
   4 /MOD SWAP MASKS + C@ >R       SWAP 40* PBUFPT + + C@ R>    
   AND LOCTAB + C@ ;                                         -->
( SETTING UP STUFF FOR PRINT )                                  
 LABEL ACREG 16 ALLOT                                           
 CODE SETCOL                     ( addr1 # addr2 ... addr1 )    
 0 ,X LDA, N STA, 1 ,X LDA, N 1+ STA, 2 ,X LDY, 4 ,X LDA, N 2 + 
 STA, 5 ,X LDA, N 3 + STA,       N 2 + )Y LDA, ROWMSK AND, 0 #  
 LDY, N )Y ORA, N )Y STA,        POPTWO JMP, C;                 
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 053
( ONEROW )                            43 LOAD -->               
 : INIROW                          YELLOW 480 ERASE BLUE 480    
   ERASE RED 480 ERASE ;         : ONEROW ( row# ... )          
   NCL 0 DO DUP I 10 * AR /       LOC. ACREG + C@ 6 * CONTAB +  
  0 YELLOW I 2* + 32 + SETCOL     1 RED    I 2* + 32 + SETCOL   
  2 BLUE   I 2* + 32 + SETCOL     3 YELLOW I 2* 1+ + 32 + SETCOL
  4 RED    I 2* + 1+ 32 + SETCOL  5 BLUE   I 2* + 1+ 32 + SETCOL
    DROP                           LOOP DROP ;               -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 054
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 055
( GR 7PLUS )                        42 LOAD -->                 
 HEX                            CODE 7PLUS                ( -- )
  A9 C, 07 C, 85 C, 57 C, AD C,   30 C, 02 C, 85 C, N  C, AD C, 
  31 C, 02 C, 85 C, N 1+ C, HERE  B1 C, N  C, 29 C, FC C, C9 C, 
  40 C, F0 C, 14 C, B1 C, N C,    85 C, N 2+ C, 29 C,           
  0F C, C9 C, 0F C, D0 C, 06 C,   C6 C, N 2+ C, A5 C, N 2+ C,   
  91 C, N  C, C8 C, 4C C, ,       4C C, NEXT ,  C;  DCX         
 : GRX 24 GR. 7PLUS ;            : OFFDLI 64 54286 C! ;      -->
( DLINT FOR PAINT10 )           LABEL DLTAB 128 ALLOT           
LABEL REGTAB 128 ALLOT          LABEL CDLTAB 128 ALLOT          
                                HEX                             
                                CODE DLIROUT 48 C, 8A C, 48 C,  
 98 C, 48 C, AE C, D40B , BC C, REGTAB , 30 C, 9 C, BD C,       
CDLTAB , 8D C, D40A , 99 C,     D012 , 68 C, A8 C, 68 C, AA C,  
68 C, 40 C, C;                  DCX                             
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 056
( MORE DLI STUFF )               0 VARIABLE DLIND               
 0 VARIABLE SECPTR                                              
 : ONDLI ' DLIROUT 512 ! 192       54286 C! ;                   
 : MVDLI DLTAB SECPTR @ 61 + 1     R/W REGTAB SECPTR @ 62 + 1   
   R/W CDLTAB SECPTR @ 63 + 1      R/W ;                        
 : SETDLI OFFDLI 560 @ 128 0 DO    DUP I DLTAB + C@ DUP IF + DUP
   C@ 128 OR SWAP C! ELSE 2DROP    THEN LOOP DROP ONDLI ;       
                             -->                                
( MOVEPIC )                                                     
 : MVPIC ( pic# ... )              160 106 C! 0 GR. 64 *        
   1+ SECPTR ! 60 0 DO I 128 *     PBUFPT + SECPTR @ I + 1 R/W  
   LOOP DLTAB SECPTR @ 60 + 1      R/W GRX PBUFPT 88 @ 7680     
   CMOVE  DLTAB 704 9 CMOVE        MVDLI SETDLI WAIT ;          
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 057
( INIREG )                          41 LOAD -->                 
: INIREG                          712 C@ CONCODE C!             
  708 C@ CONCODE 3 + C!           709 C@ CONCODE 6 + C!         
 710 C@ CONCODE 9 + C! 4 COL# C!  STR 10 + ' AR ! AR 160 * 10 / 
  ' NCL !                         4 0 DO I ACREG I + C! LOOP    
  ROWLST 9 + C@ ROW# C!   ;      : RX DUP 8 = IF DROP 0 ELSE    
   3 - THEN ;                    0 VARIABLE PAR                 
                                                             -->
( FNDDLI )                                                      
 : FNDDLI H4 CMAX C! 255 ROWLST    H4 2* 1+ + C!                
   128 0 DO I REGTAB + C@ DUP      10 < I DLTAB + C@ 0= NOT AND 
 IF RX ROWLST CMAX C@ 2* + C!         CDLTAB I + C@ CONCODE     
      CMAX C@ 3 * + C!                DLTAB I + C@ DUP 100 <    
      IF 3 ELSE 5 THEN - ROWLST       CMAX C@ 2* 1+ + C! CMAX C@
      1+ DUP 41 > IF DROP 41          THEN CMAX C!              
   ELSE DROP  THEN LOOP 255 CMAX  C@ 2* ROWLST 1+ + C! ;     -->
════════════════════════════════════════════════════════════════   SCREEN 060
( EPSND ) 6  VARIABLE LFD        : EPSND 2 PFLAG !              
   79 0 DO 32 EMIT LOOP ." . "     27 EMIT 60 EMIT              
   27 EMIT 51 EMIT LFD C@  EMIT    CR    1 PFLAG ! ;            
                                  : TSTDEL DIGDAT 300 ERASE     
    EPSND SNDON GETDAT SNDOFF ;                                 
                                                                
                                                                
                                                             -->
( DIGITIZING TABLES )            LABEL CNVRT 229 ALLOT          
 3 VARIABLE LOLUM                228 VARIABLE HILUM             
 175 VARIABLE LOSQR              225 VARIABLE DIFF              
 : INBND 15 MIN 0 MAX ;          : LINTAB HILUM @ LOLUM @ - DIFF
   ! 229 0 DO I HILUM @ MIN        LOLUM @ MAX LOLUM @ - 16 *   
   DIFF C@ /  INBND  15 SWAP -     I CNVRT + C! LOOP ;          
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 061
( DIGITIZING TEST )                                             
 : SQRT 127 8 0 DO 2DUP            5000 SWAP */ SWAP 2/ + LOOP  
   SWAP DROP ;                   : SQRTAB HILUM @ SQRT LOLUM @  
   SQRT DUP LOSQR ! - DIFF !       229 0 DO I HILUM @ MIN       
   LOLUM @ MAX SQRT LOSQR @ -      16 DIFF @ */ INBND 15 SWAP - 
   I CNVRT + C! LOOP ;                                          
                                                                
                                                             -->
( DIGITIZER )                    LABEL FILTR 0 ,                
 : FILT0 C@ ;                    : DOFILT FILTR @EX ;           
  ' FILT0 CFA FILTR !            : SETMAX                       
   BEGIN 624 C@ . ?TERMINAL        UNTIL ;                      
 0 VARIABLE SCAD                ( 126 LOAD -->  )               
 : ONELIN FRSTDAT 80 O+S           DO I DOFILT  CNVRT + C@ 2*   
 2* 2* 2* I 1+ DOFILT  CNVRT      + C@ +   SCAD @ C! 1 SCAD +!  
   2 +LOOP ;                                                 -->
════════════════════════════════════════════════════════════════   SCREEN 062
( DIGITIZER )                    LABEL PBUFPT 7680 ALLOT        
                                                                
                                 : DIGITIZE                     
   9 GR. 88 @ SCAD !               192 0 DO DIGDAT 300 ERASE    
   EPSND SNDON GETDAT SNDOFF       ONELIN ?TERMINAL IF LEAVE    
   THEN LOOP WAIT 88 @            PBUFPT 7680 CMOVE 0 GR. ;     
                                                                
                                                                
( FILTERING WORDS )             : D/ ( unsign divide )          
  0 SWAP U/ SWAP DROP ;          LABEL WEIGHTS                  
 151 , 253 , 141 , 28 , -35 ,    -40 , -15 , 5 , 9 , 3 ,        
    0 VARIABLE SUM               : CONVOL ( addr ... aver )     
   0 SUM !                        10 0 DO I OVER + C@ OVER I -  
        C@ + WEIGHTS I 2* + @           * SUM +! LOOP DROP      
    SUM @ 1000 D/ 3 MAX 228 MIN   ;                             
                                                                
════════════════════════════════════════════════════════════════   SCREEN 063
( ALTERNATE ONELIN )            ( AVERAGE 2 POINTS )            
                                : ONELIN                        
  FRSTDAT 160 O+S                 DO I C@ I 1+ C@ + 2/          
     CNVRT + C@ 2* 2* 2* 2*          I 2 + C@ I 3 + C@ + 2/     
     CNVRT + C@ +                    SCAD @ C! 1 SCAD +!        
  4 +LOOP ;                                                     
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
════════════════════════════════════════════════════════════════   SCREEN 065
                                                                
 : SAVSCR ( pic# ... )             64 * 1+ 60 0 DO I 128 *      
   PBUFPT + OVER I + 0 R/W LOOP    DROP ;                       
                                 : LOADSCR ( pic# ... )         
   64 * 1+ 60 0 DO I 128 *         PBUFPT + OVER I + 1 R/W LOOP 
   DROP 9 GR. PBUFPT 88 @          7680 CMOVE WAIT ;            
                                                                
                                                                
( Quan:  TO  AT                )                                
: TO                              -FIND 0= 0 ?ERROR DROP        
  STATE @                         IF ,                          
  ELSE EXECUTE                    ENDIF ; IMMEDIATE             
                                : AT                            
  -FIND 0= 0 ?ERROR DROP          2+ STATE @                    
  IF ,                            ELSE EXECUTE                  
  ENDIF ; IMMEDIATE               ( corrected )              -->
════════════════════════════════════════════════════════════════   SCREEN 066
( Quan:  [2@6]  [2!4]          )                                
ASSEMBLER HEX                                                   
LABEL (2@6)                       A0 C, 06 C, B1 C, W  C, 48 C, 
  C8 C, B1 C, W  C, 4C C, PUSH ,                                
LABEL (2!4)                       A0 C, 04 C, B5 C, 00 C, 91 C, 
  W  C, C8 C, B5 C, 01 C, 91 C,   W  C, 4C C, POP ,             
                                                                
                                                             -->
( Quan:  [2V6]                 )                                
LABEL (2V6)                       A0 C, 07 C, B1 C, W  C, 48 C, 
  88 C, B1 C, W  C, 85 C, W  C,   68 C, 85 C, W 1+ C,           
  A0 C, 00 C, 4C C, W 1- ,                                      
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 067
( Quan:  patch for CREATE      )                                
DCX                                                             
: (PTCH)              ( system )  SWAP >R R = 251 R = 249 R> =  
  OR OR ;                                                       
: PTCH                ( system )  IF [ ' (PTCH) CFA ] LITERAL   
  ELSE [ ' = CFA ] LITERAL        ENDIF                         
  [ ' CREATE 63 + ] LITERAL ! ;                                 
                                                             -->
( Quan:  QUAN  VECT            )                                
: VAR                             ON PTCH LABEL -2 ALLOT        
  (2@6) , (2!4) ,                 [ ' VARIABLE 4 + ] LITERAL ,  
  2 ALLOT OFF PTCH ;                                            
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 068
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
( SUPPORT WORDS )                192 CONSTANT SCLN              
 : *LV 764 7 OVER C@ = IF LEAVE   THEN 255 SWAP C!  ;           
 LABEL OLDCOL 9 ALLOT            : ?RG DUP 0= IF 8 ELSE 4 THEN  
   + ;                           LABEL PBUFPT 7680 ALLOT        
 : LOADIT 64 * 1+                  60 0 DO PBUFPT I 128 * +     
   OVER I + 1 R/W LOOP DROP         9 GR. PBUFPT 88 @ 7680      
   CMOVE WAIT 0 GR. ;            : PRCK 1 ;                     
 : 28* 2* 2* 2* DUP 2* 2* + ;    : 7CHK EMIT ;               -->
════════════════════════════════════════════════════════════════   SCREEN 069
( MODULE GTIA   )                HERE DCX                       
 MODST DP !                      5 C, 460 , 20 , ' NOOP CFA ,   
                                                                
                                                                
                                                                
                                                                
                             -->                                
                                                                
( PRINTER PICK )                                                
 LABEL CSTP -1 ,                 LABEL EPS 0 C,                 
 LABEL RSTP 1 ,                  : PRPK CLS                     
   CR ." 1. Epson " CR             ." 2. ITOH " CR KEY DUP EMIT 
   50 = IF 1 128 -1 ELSE           -1 0 1 THEN RSTP ! EPS C!    
   CSTP ! CR ;                                                  
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 070
( COLORS )                      LABEL GREYS                     
 255 C, 255 C, 255 C, 255 C,     255 C, 255 C, 187 C, 255 C,    
 238 C, 255 C, 187 C, 255 C,     204 C, 255 C, 187 C, 255 C,    
 204 C, 255 C,  51 C, 255 C,     204 C, 238 C,  51 C, 255 C,    
 204 C, 238 C,  51 C, 187 C,     204 C, 204 C,  51 C, 187 C,    
 204 C, 204 C,  51 C,  51 C,     204 C, 204 C,  51 C,  34 C,    
 204 C, 136 C,  51 C,  34 C,                                 -->
                                                                
( GREY MEN )                                                    
 204 C, 136 C,  51 C,   0 C,     204 C,   0 C,  51 C,   0 C,    
 204 C,   0 C,  34 C,   0 C,     136 C,   0 C,  34 C,   0 C,    
   0 C,   0 C,   0 C,   0 C,                                    
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 071
( PRINTER STUFF )               LABEL 4BYTE 0 , 0 ,             
                                                                
 CODE 1STUP EPS BIT, IFMI,       15 # LDA, 0 ,X AND, 0 ,X STA,  
 ELSE,                           0 ,X LSR, 0 ,X LSR, 0 ,X LSR,  
 0 ,X LSR,                       THEN, NXT, C;                  
 CODE 2NDUP EPS BIT, IFMI,       0 ,X LSR, 0 ,X LSR, 0 ,X LSR,  
 0 ,X LSR, ELSE,                 15 # LDA, 0 ,X AND, 0 ,X STA,  
 THEN, NXT, C;                                               -->
( PRINTER STUFF )                : COLMS EPS C@                 
       IF SCLN 0 ELSE -1 SCLN 1-   THEN ;                       
                                 : ROWZ  EPS C@                 
       IF -1 39 ELSE 40 0 THEN ;                                
 VAR FRS  VAR LST                : *LEV 764 7 OVER C@ = IF      
   LST TO FRS THEN 255 SWAP        C! ;                         
                                                                
                             -->                                
════════════════════════════════════════════════════════════════   SCREEN 072
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
( PRINTER STUFF )                : EMTS 1 O+S C@ TYPE ;         
 LABEL IT7                       6 C, 27 C, 83 C, 48 C, 55 C,   
 54 C, 56 C,                     LABEL EP7                      
 4 C, 27 C, 76 C, 0 C, 3 C,      LABEL EPI                      
 3 C, 27 C, 65 C, 8 C,           LABEL ITI  6 C, 27 C, 81 C,    
 27 C, 84 C, 49 C, 54 C,         LABEL EPX 3 C, 27 C, 65 C,     
  12 C,                          LABEL IPX 4 C, 27 C, 65 C,     
  27 C, 69 C,                                                -->
════════════════════════════════════════════════════════════════   SCREEN 073
( FANCY PRINT )                                                 
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
( FANCY PRINT )                  CODE MAKFOR 0 ,X LDA, ASL.A,   
 ASL.A, XSAVE STX, TAX, 4 # LDA, N STA,                         
 BEGIN, GREYS ,X LDA, 4BYTE ,Y    STA, INY, INX, N DEC,         
 EQ UNTIL, XSAVE LDX, POP JMP,   C;                             
                                                                
                                                                
                                 : PRCOL EPS C@ IF IT7 ELSE EP7 
   THEN EMTS ;                                               -->
════════════════════════════════════════════════════════════════   SCREEN 074
 ( PRINTER STUFF )               : 4EMIT  4 0 DO 4BYTE I + C@   
   EMIT  LOOP ;                  : 1STCOL ( col# row ... col# ) 
   28* OVER + C@ 1STUP MAKFOR      4EMIT ;                      
 : 2NDCOL ( col# row ... col# )    28* OVER + C@ 2NDUP          
   MAKFOR 4EMIT ;                : ONECL ( col# ... ) PRCOL     
COLMS DO I 1STCOL CSTP @ +LOOP    CR PRCOL COLMS                
  DO I 2NDCOL  CSTP @  +LOOP     DROP CR ;                   -->
                                                                
                                : DUMP  ROWZ TO FRS TO LST      
  BEGIN FRS PBUFPT + ONECL        RSTP @ AT FRS +! *LEV         
  FRS LST = UNTIL ;                                             
: FANCY GMODE 4 MOD IF PRPK     PRCK IF 0 559 C! 2 PFLAG ! EPS  
  C@  IF ITI ELSE EPI THEN EMTS          DUMP EPS C@            
      IF IPX ELSE EPX THEN EMTS   CR CR                         
  1 PFLAG ! 34 559 C!                THEN   ELSE                
 ." NOT A GTIA MODE - BYE ! "    120 PAUSE THEN ;            -->
════════════════════════════════════════════════════════════════   SCREEN 079
( GRAPHICS FOR 7+ DIGITIZE )     HEX                            
CODE 7PLUS                ( -- )  A9 C, 07 C, 85 C, 57 C, AD C, 
  30 C, 02 C, 85 C, N  C, AD C,   31 C, 02 C, 85 C, N 1+ C, HERE
  B1 C, N  C, 29 C, FC C, C9 C,   40 C, F0 C, 14 C, B1 C, N C,  
  85 C, N 2+ C, 29 C,             0F C, C9 C, 0F C, D0 C, 06 C, 
  C6 C, N 2+ C, A5 C, N 2+ C,     91 C, N  C, C8 C, 4C C, ,     
  4C C, NEXT ,  C;  DCX          : GR7+ 24 GR. 7PLUS            
 0 712 C! 4 708 C!  8 709 C!      14 710 C! ;                -->
( 7PLUS PLOTTING )                                              
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 080
( DELAY )                         185 VARIABLE TIME             
 LABEL DIGDAT 400 ALLOT          DIGDAT 20 + CONSTANT FRSTDAT   
 54 VARIABLE #DIGS               : DELAY TIME @ 0 DO LOOP ;     
 : GETDAT FRSTDAT #DIGS @ OVER      + DO 624 C@ I C!            
   DELAY -1 +LOOP ;                      HEX                    
 : SNDON  3 D20F C! 3 232 C!       0 D208 C! 30 D200 C! AA D201 
   C! ;                          : SNDOFF 0 D201 C! ;           
                     DCX                                     -->
( EPSND ) 6  VARIABLE LFD        : EPSND 2 PFLAG !              
   79 0 DO 32 EMIT LOOP ." . "     27 EMIT 60 EMIT              
   27 EMIT 51 EMIT LFD C@  EMIT    CR    1 PFLAG ! ;            
                                  : TSTDEL DIGDAT 300 ERASE     
    EPSND SNDON GETDAT SNDOFF ;                                 
                                                                
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 081
( DIGITIZING TABLES )            LABEL CNVRT 229 ALLOT          
 3 VARIABLE LOLUM                228 VARIABLE HILUM             
 175 VARIABLE LOSQR              225 VARIABLE DIFF              
 : INBND  3 MIN 0 MAX ;          : LINTAB HILUM @ LOLUM @ - DIFF
   ! 229 0 DO I HILUM @ MIN        LOLUM @ MAX LOLUM @ -  4 *   
   DIFF C@ /  INBND   3 SWAP -     I CNVRT + C! LOOP ;          
                                                                
                                                             -->
( DIGITIZING TEST )                                             
 : SQRT 127 8 0 DO 2DUP            5000 SWAP */ SWAP 2/ + LOOP  
   SWAP DROP ;                   : SQRTAB HILUM @ SQRT LOLUM @  
   SQRT DUP LOSQR ! - DIFF !       229 0 DO I HILUM @ MIN       
   LOLUM @ MAX SQRT LOSQR @ -       4 DIFF @ */ INBND  3 SWAP - 
   I CNVRT + C! LOOP ;                                          
                                                                
                                                             -->
════════════════════════════════════════════════════════════════   SCREEN 082
( DIGITIZER )                    LABEL FILTR 0 ,                
 : FILT0 C@ ;                    : DOFILT FILTR @EX ;           
  ' FILT0 CFA FILTR !            : SETMAX                       
   BEGIN 624 C@ . ?TERMINAL        UNTIL ;                      
 0 VARIABLE SCAD                  166 LOAD                      
 : ONELIN FRSTDAT 160 O+S          DO I 4BYTE SCAD @ C! 1 SCAD  
 +! 4 +LOOP ;                                                   
                                                             -->
( DIGITIZER )                    LABEL PBUFPT 7680 ALLOT        
                                                                
                                 : DIGITIZE                     
   GR7+  88 @ SCAD !               192 0 DO DIGDAT 300 ERASE    
   EPSND SNDON GETDAT SNDOFF       ONELIN ?TERMINAL IF LEAVE    
   THEN LOOP WAIT 88 @            PBUFPT 7680 CMOVE 0 GR. ;     
                                                                
                                                                
