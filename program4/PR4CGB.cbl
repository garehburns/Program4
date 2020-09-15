        IDENTIFICATION DIVISION.
        PROGRAM-ID.  PR4CGB.
        AUTHOR.  GARRETT BURNS.
       
      * * * * * * * * * * * * * * * * * * * * * * * * *
      *
      * INPUT:
      *   (128-CHARACTER RECORD ON DISK)
      *     INPUT RECORD LAYOUT CONTAINS THE FOLLOWING DATA FOR EACH RECORD:
      *         1. WAREHOUSE ID                 1-4
      *         2. VENDOR ID                    5
      *         3. PRODUCT ID                   6-8
      *         4. PRODUCT DATA (MAXIMUM OF 5)  9-128
      *
      *     INSIDE EACH PIECE OF DATA CONTAINS THE FOLLOWING INFORMATION:
      *         1. AN - NV10
      *         2. I - MADEINHOUSE
      *            T - TANSIA CORP.
      *            A - AMEL LTD.
      *            W - WEST CORP.
      *         3. C01 - C10
      *            O01 - O10
      *         4. PRODUCT NAME - X(13)
      *            PRODUCT SIZE - A
      *                             X - EXTRA LARGE
      *                             L - LARGE
      *                             M - MEDIUM
      *                             S - SMALL
      *                             A - SAMPLE
      *            PRODUCT TYPE - A
      *                             C - CREAM
      *                             O - OIL
      *            NUMBER IN STOCK - S9(4)
      *            PURCHASE PRICE - S999V99  //S9(3)V9(2)
      
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        SOURCE-COMPUTER.   LAPTOP-U5VKK9JE.
        OBJECT-COMPUTER.   LAPTOP-U5VKK9JE.
       
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
        
      * INPUT FIRST RECORD
            SELECT INPUT-RECORD-1
                ASSIGN TO 'PR4F19-CA20.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.                
      * INPUT SECOND RECORD
            SELECT INPUT-RECORD-2
                ASSIGN TO 'PR4F19-NV10.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.
      * INPUT THIRD RECORD
            SELECT INPUT-RECORD-3
                ASSIGN TO 'PR4F19-WA30.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.
            
      * ASSIGN INDIVIDUALLY SORTED RECORDS
            SELECT SORTED-RECORD-1
                ASSIGN TO 'SORTED-CA20.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.
            SELECT SORTED-RECORD-2
                ASSIGN TO 'SORTED-NV10.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.
            SELECT SORTED-RECORD-3
                ASSIGN TO 'SORTED-WA30.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.
                    
      * ASSIGN MERGED AND SORTED RECORDS
            SELECT MERGED-RECORD
                ASSIGN TO 'MERGEDSORTED-RECORD.TXT'
                    ORGANIZATION IS LINE SEQUENTIAL.
                    
      * ASSIGN OUTPUT AND ERROR REPORT
            SELECT OUTPUT-REPORT
                ASSIGN TO PRINTER 'PR4-OUT'.
                
            SELECT ERROR-REPORT
                ASSIGN TO PRINTER 'PR4-ERR'.
      
      *** TEMPORARY SORT AND MERGE RECORDS BELOW ***
            SELECT SORTING-MERGING-FILE
                ASSIGN TO 'SORTING.TMP'.
                
        DATA DIVISION.
        FILE SECTION.
        
      * INPUT RECORD 1
        FD  INPUT-RECORD-1
            RECORD CONTAINS 128 CHARACTERS.
           
        01  UNSORTED-RECORD-1.
            05  U1-WAREHOUSE-ID              PIC X(4).
            05  U1-VENDOR-ID                 PIC X(1).
            05  U1-PRODUCT-ID                PIC X(3).
            05  U1-PRODUCT-DATA OCCURS 5 TIMES.
                10  U1-PRODUCT-NAME          PIC X(13).
                10  U1-PRODUCT-SIZE          PIC A(1).
                10  U1-PRODUCT-TYPE          PIC A(1).
                10  U1-NUM-IN-STOCK          PIC S9(4).
                10  U1-PURCHASE-PRICE        PIC S999V99.
                                            
      * INPUT RECORD 2
        FD  INPUT-RECORD-2
            RECORD CONTAINS 128 CHARACTERS.
           
        01  UNSORTED-RECORD-2.
            05  U2-WAREHOUSE-ID              PIC X(4).
            05  U2-VENDOR-ID                 PIC X(1).
            05  U2-PRODUCT-ID                PIC X(3).
            05  U2-PRODUCT-DATA OCCURS 5 TIMES.
                10  U2-PRODUCT-NAME          PIC X(13).
                10  U2-PRODUCT-SIZE          PIC A(1).
                10  U2-PRODUCT-TYPE          PIC A(1).
                10  U2-NUM-IN-STOCK          PIC S9(4).
                10  U2-PURCHASE-PRICE        PIC S999V99.
        
      * INPUT RECORD 3
        FD  INPUT-RECORD-3
            RECORD CONTAINS 128 CHARACTERS.
           
        01  UNSORTED-RECORD-3.
            05  U3-WAREHOUSE-ID              PIC X(4).
            05  U3-VENDOR-ID                 PIC X(1).
            05  U3-PRODUCT-ID                PIC X(3).
            05  U3-PRODUCT-DATA OCCURS 5 TIMES.
                10  U3-PRODUCT-NAME          PIC X(13).
                10  U3-PRODUCT-SIZE          PIC A(1).
                10  U3-PRODUCT-TYPE          PIC A(1).
                10  U3-NUM-IN-STOCK          PIC S9(4).
                10  U3-PURCHASE-PRICE        PIC S999V99.
        
      ***** SORTED RECORDS 1-3 *****
      
        FD  SORTED-RECORD-1
            RECORD CONTAINS 128 CHARACTERS.
            
        01  SORTED-1.
            05  S1-WAREHOUSE-ID              PIC X(4).
            05  S1-VENDOR-ID                 PIC X(1).
            05  S1-PRODUCT-ID                PIC X(3).
            05  S1-PRODUCT-DATA OCCURS 5 TIMES.
                10  S1-PRODUCT-NAME          PIC X(13).
                10  S1-PRODUCT-SIZE          PIC A(1).
                10  S1-PRODUCT-TYPE          PIC A(1).
                10  S1-NUM-IN-STOCK          PIC S9(4).
                10  S1-PURCHASE-PRICE        PIC S999V99.
        
        FD  SORTED-RECORD-2
            RECORD CONTAINS 128 CHARACTERS.
            
        01  SORTED-2.
            05  S2-WAREHOUSE-ID              PIC X(4).
            05  S2-VENDOR-ID                 PIC X(1).
            05  S2-PRODUCT-ID                PIC X(3).
            05  S2-PRODUCT-DATA OCCURS 5 TIMES.
                10  S2-PRODUCT-NAME          PIC X(13).
                10  S2-PRODUCT-SIZE          PIC A(1).
                10  S2-PRODUCT-TYPE          PIC A(1).
                10  S2-NUM-IN-STOCK          PIC S9(4).
                10  S2-PURCHASE-PRICE        PIC S999V99.
        
        FD  SORTED-RECORD-3
            RECORD CONTAINS 128 CHARACTERS.
        
        01  SORTED-3.
            05  S3-WAREHOUSE-ID              PIC X(4).
            05  S3-VENDOR-ID                 PIC X(1).
            05  S3-PRODUCT-ID                PIC X(3).
            05  S3-PRODUCT-DATA OCCURS 5 TIMES.
                10  S3-PRODUCT-NAME          PIC X(13).
                10  S3-PRODUCT-SIZE          PIC A(1).
                10  S3-PRODUCT-TYPE          PIC A(1).
                10  S3-NUM-IN-STOCK          PIC S9(4).
                10  S3-PURCHASE-PRICE        PIC S999V99.
        
        FD  MERGED-RECORD
            RECORD CONTAINS 128 CHARACTERS.
        
        01  MERGED.
            05  M-WAREHOUSE-ID              PIC X(4).
            05  M-VENDOR-ID                 PIC X(1).
            05  M-PRODUCT-ID                PIC X(3).
            05  M-PRODUCT-DATA OCCURS 5 TIMES.
                10  M-PRODUCT-NAME          PIC X(13).
                10  M-PRODUCT-SIZE          PIC A(1).
                10  M-PRODUCT-TYPE          PIC A(1).
                10  M-NUM-IN-STOCK          PIC S9(4).
                10  M-PURCHASE-PRICE        PIC S999V99.
                
        SD  SORTING-MERGING-FILE
            RECORD CONTAINS 128 CHARACTERS.
            
        01  SORTING-MERGING-REC.
            05  SM-WAREHOUSE-ID              PIC X(4).
            05  SM-VENDOR-ID                 PIC X(1).
            05  SM-PRODUCT-ID                PIC X(3).
            05  FILLER                       PIC X(120).
        
      * SEND ALL TO OUTPUT RECORD
        FD  OUTPUT-REPORT
            RECORD CONTAINS 65 CHARACTERS.
           
        01  RECORD-REPORT                    PIC X(65).
        
        
        FD  ERROR-REPORT
            RECORD CONTAINS 128 CHARACTERS.
            
        01  ERROR-RECORD                     PIC X(128).
        
        WORKING-STORAGE SECTION.
       
        01  FLAGS-N-SWITCHES.
            05  EOF-FLAG                     PIC X       VALUE ' '.
                88 NO-MORE-DATA                          VALUE 'N'.
                88 MORE-RECORDS                          VALUE 'Y'.
            05  FIRST-RECORD                 PIC X(3)    VALUE 'YES'.
            05  SUB                          PIC 99.
        
        01  REPORT-FIELDS.
            05  PROPER-SPACING               PIC S9      VALUE +1.
            05  PAGE-NO                      PIC S9(2)   VALUE +0.
            05  ERROR-COUNTER                PIC 99      VALUE 0.
        
        01  WS-CURRENT-DATE.
            05  WS-YEAR                      PIC 99.
            05  WS-MONTH                     PIC 99.
            05  WS-DAY                       PIC 99.
            
      * FIELD FOR EXPORTING MY ERROR FILE
        01  ERROR-FIELD.
            05  EF-WAREHOUSE-ID              PIC X(4).
            05  EF-VENDOR-ID                 PIC X(1).
            05  EF-PRODUCT-ID                PIC X(3).
            05  EF-DATA                      PIC X(120).
        
      *** HARD-CODING THE VENDORS ***
        01  VENDOR-TEXT.
            05          PIC X(14)   VALUE 'IMadeInHouse'.
            05          PIC X(14)   VALUE 'TTansia Corp.'.
            05          PIC X(14)   VALUE 'AAMEL Ltd'.
            05          PIC X(14)   VALUE 'WWESTCorp'.
            05          PIC X(14)   VALUE 'DDENIO Corp.'.
            05          PIC X(14)   VALUE 'VVISSION Corp.'.
            05          PIC X(14)   VALUE 'NNETON Ltd'.
        
        01  VENDOR-TABLE REDEFINES VENDOR-TEXT.
            05  VENDOR-ITEM OCCURS 7 TIMES
                INDEXED BY VENDOR-INDEX.
                10  VENDOR-ID                PIC X(1).
                10  VENDOR-NAME              PIC X(13).
        
        01  DETAIL-FIELDS.
            05  DF-PRODUCT-NAME              PIC X(13).
            05  DF-PRODUCT-ID                PIC X(3).
            05  DF-PRODUCT-SIZE              PIC X(11).
            05  DF-PRODUCT-TYPE              PIC X(5).
            05  DF-IN-STOCK                  PIC Z999.
            05  DF-TOTAL-COST                PIC $ZZ,ZZ9.99.
        
        01  HOLDING-FIELDS.
            05  H-WAREHOUSE-HOLD             PIC X(4).
            05  H-VENDOR-HOLD                PIC X(1).
            05  H-VENDOR-HOLD2               PIC X(20).
            05  H-PRODUCT-HOLD               PIC X(3).
            05  H-PRODUCT-SIZE-HOLD          PIC X(10).
            05  H-PRODUCT-TYPE-HOLD          PIC X(5).
            05  H-PURCHASE-PRICE-HOLD        PIC S9(7)V9(2).
        
        01  STRING-HOLDING.
            05  SH-WAREHOUSE-ID              PIC X(4).
            05  SH-VENDOR-ID                 PIC X(1).
            05  SH-PRODUCT-ID                PIC X(3).
            
        01  TOTAL-FIELDS.
            05  TF-PRODUCT-TOTAL             PIC S9(7)V9(2).
            05  TF-VENDOR-TOTAL              PIC S9(7)V9(2).
            05  TF-WAREHOUSE-TOTAL           PIC S9(8)V9(2).
            05  TF-GRAND-TOTAL               PIC S9(8)V9(2).
        
        
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                           OUTPUT AREA                           *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
        01  HEADING-ONE.
            05                    PIC X(35).
            05                    PIC X(9)   VALUE 'DR. CHEEB'.
            05                    PIC X(21).
            
        01  HEADING-TWO.
            05                    PIC X(10).
            05  H1-DATE.
                10  H1-MONTH      PIC Z9.
                10                PIC X      VALUE '/'.
                10  H1-DAY        PIC 99.
                10                PIC X      VALUE '/'.
                10  H1-YEAR       PIC 99.
            05                    PIC X(12).
            05                    PIC X(16)  VALUE 'INVENTORY REPORT'.
            05                    PIC X(8).
            05                    PIC X(6)   VALUE 'PAGE: '.
            05  H1-PAGE-NO        PIC Z9B.
        
        01  WAREHOUSE-LINE.
            05                    PIC X(2).
            05                    PIC X(11)  VALUE 'WAREHOUSE: '.
            05  WL-WAREHOUSE-ID   PIC X(4).
            05                    PIC X(48).
           
        01  VENDOR-LINE.
            05                    PIC X(5).
            05                    PIC X(8)   VALUE 'VENDOR: '.
            05  VL-VENDOR-ID      PIC X(12).
            05                    PIC X(40).
        
        01  HEADING-THREE.
            05                    PIC X(8).
            05                    PIC X(7)   VALUE 'PRODUCT'.
            05                    PIC X(7).
            05                    PIC X(4)   VALUE 'PROD'.
            05                    PIC X(4).
            05                    PIC X(7)   VALUE 'PRODUCT'.
            05                    PIC X(4).
            05                    PIC X(4)   VALUE 'PROD'.
            05                    PIC X(4).
            05                    PIC X(2)   VALUE 'IN'.
            05                    PIC X(7).
            05                    PIC X(5)   VALUE 'TOTAL'.
            05                    PIC X(2).
            
        01  HEADING-FOUR.
            05                    PIC X(10).
            05                    PIC X(4)   VALUE 'NAME'.
            05                    PIC X(9).
            05                    PIC X(2)   VALUE 'ID'.
            05                    PIC X(6).
            05                    PIC X(4)   VALUE 'SIZE'.
            05                    PIC X(6).
            05                    PIC X(4)   VALUE 'TYPE'.
            05                    PIC X(3).
            05                    PIC X(5)   VALUE 'STOCK'.
            05                    PIC X(5).
            05                    PIC X(4)   VALUE 'COST'.
            05                    PIC X(3).
            
        01  DETAIL-LINE.
            05                    PIC X(5).
            05  DL-PRODUCT-NAME   PIC X(13).
            05                    PIC X(4).
            05  DL-PRODUCT-ID     PIC X(3).
            05                    PIC X(2).
            05  DL-PRODUCT-SIZE   PIC X(11).
            05                    PIC X(2).
            05  DL-PRODUCT-TYPE   PIC X(5).
            05                    PIC X(3).
            05  DL-IN-STOCK       PIC Z999.
            05                    PIC X(3).
            05  DL-TOTAL-COST     PIC $ZZ,ZZ9.99.
            
        01  PRODUCT-TOTAL-LINE.
            05                    PIC X(21).
            05                    PIC X(9)   VALUE 'PRODUCT: '.
            05  PTL-PRODUCT-NAME  PIC X(13).
            05                    PIC X(10)  VALUE ' TOTAL:   '.
            05  PTL-PRODUCT-TOTAL PIC $ZZZ,ZZ9.99.
            
        01  VENDOR-TOTAL-LINE.
            05                    PIC X(13).
            05                    PIC X(18)  VALUE 'TOTAL FOR VENDOR: '.
            05  VTL-VENDOR-ID     PIC X(12).
            05                    PIC X(10).
            05  VTL-VENDOR-TOTAL  PIC $ZZZ,ZZ9.99.
            
        01  WAREHOUSE-TOTAL-LINE.
            05                    PIC X(10).
            05                    PIC X(21)  VALUE 
                                                'TOTAL FOR WAREHOUSE: '.
            05  WTL-WAREHOUSE-ID  PIC X(4).
            05                    PIC X(17).
            05  WTL-WAREHOUSE-TOTAL PIC $Z,ZZZ,ZZ9.99.
            
        01  GRAND-TOTAL-LINE.
            05                    PIC X(22).
            05                    PIC X(17)  VALUE 'GRAND TOTAL COST:'.
            05                    PIC X(12).
            05  GTL-GRAND-TOTAL   PIC $ZZ,ZZZ,ZZ9.99.
            
        01  ERROR-LINE.
            05                    PIC X(13).
            05                    PIC X(14)  VALUE 'TOTAL ERRORS: '.
            05  EL-ERROR-COUNTER  PIC 99.
            
        PROCEDURE DIVISION.
        
        10-CONTROL-MODULE.
            PERFORM 15-SORT-AND-MERGE
            PERFORM 20-HOUSEKEEPING
            PERFORM 30-READ-FILE
            PERFORM 700-EOJ
            PERFORM 800-PRINT-GRAND-TOTALS
            PERFORM 900-FINALE
        .
        
        15-SORT-AND-MERGE.
            
            SORT SORTING-MERGING-FILE
                ON ASCENDING KEY SM-WAREHOUSE-ID,
                                 SM-VENDOR-ID,
                                 SM-PRODUCT-ID
                USING INPUT-RECORD-1
                GIVING SORTED-RECORD-1
            SORT SORTING-MERGING-FILE
                ON ASCENDING KEY SM-WAREHOUSE-ID,
                                 SM-VENDOR-ID,
                                 SM-PRODUCT-ID
                USING INPUT-RECORD-2
                GIVING SORTED-RECORD-2
            SORT SORTING-MERGING-FILE
                ON ASCENDING KEY SM-WAREHOUSE-ID,
                                 SM-VENDOR-ID,
                                 SM-PRODUCT-ID
                USING INPUT-RECORD-3
                GIVING SORTED-RECORD-3
            MERGE SORTING-MERGING-FILE
                ON ASCENDING KEY SM-WAREHOUSE-ID,
                                 SM-VENDOR-ID,
                                 SM-PRODUCT-ID
                USING SORTED-RECORD-1,
                      SORTED-RECORD-2,
                      SORTED-RECORD-3
                GIVING MERGED-RECORD
        .
        
        20-HOUSEKEEPING.
       
            OPEN INPUT MERGED-RECORD
                OUTPUT OUTPUT-REPORT,
                       ERROR-REPORT
            ACCEPT WS-CURRENT-DATE FROM DATE
            MOVE WS-MONTH TO H1-MONTH
            MOVE WS-DAY TO H1-DAY
            MOVE WS-YEAR TO H1-YEAR
            
            PERFORM 40-HEADER-ROUTINE
        .
        
        30-READ-FILE.
        
            PERFORM UNTIL NO-MORE-DATA
                READ MERGED-RECORD
                    AT END
                        MOVE 'N' TO EOF-FLAG
                    NOT AT END
                        IF M-WAREHOUSE-ID = 'NV10' OR 'CA20' OR 'WA30'
                            PERFORM 100-PROCESS-DATA
                        ELSE
                            MOVE MERGED TO ERROR-RECORD
                            WRITE ERROR-RECORD
                            ADD 1 TO ERROR-COUNTER
                        END-IF
                END-READ
            END-PERFORM
        .
        
        35-SEARCH-VENDOR.
                        
            SET VENDOR-INDEX TO 1
            
            SEARCH VENDOR-ITEM
                AT END
                    STRING
                        'INVALID' DELIMITED BY ' '
                            ' - ' DELIMITED BY SIZE
                        H-VENDOR-HOLD DELIMITED BY ' '
                        INTO VL-VENDOR-ID
                    END-STRING
                    WHEN H-VENDOR-HOLD = VENDOR-ID(VENDOR-INDEX)
                        MOVE VENDOR-NAME(VENDOR-INDEX) TO VL-VENDOR-ID,
                                                          VTL-VENDOR-ID
            END-SEARCH
        .
        
        40-HEADER-ROUTINE.
            ADD 1 TO PAGE-NO
            MOVE PAGE-NO TO H1-PAGE-NO
           
            WRITE RECORD-REPORT FROM HEADING-ONE
                AFTER ADVANCING PROPER-SPACING
               
            MOVE HEADING-TWO TO RECORD-REPORT
            WRITE RECORD-REPORT FROM HEADING-TWO
                AFTER ADVANCING PROPER-SPACING
        .
  
        45-PRINT-WAREHOUSE-HEADER.
            MOVE M-WAREHOUSE-ID TO WL-WAREHOUSE-ID
            EVALUATE WL-WAREHOUSE-ID
                WHEN 'NV10'
                    WRITE RECORD-REPORT FROM WAREHOUSE-LINE
                        AFTER ADVANCING 2 LINES
                WHEN 'CA20'
                    WRITE RECORD-REPORT FROM WAREHOUSE-LINE
                        AFTER ADVANCING 2 LINES
                WHEN 'WA30'
                    WRITE RECORD-REPORT FROM WAREHOUSE-LINE
                        AFTER ADVANCING 2 LINES
            END-EVALUATE
        .
        
        50-PRINT-VENDOR-HEADER.
            STRING
                'INVALID' DELIMITED BY ' '
                    ' - ' DELIMITED BY SIZE
                VENDOR-ID(SUB) DELIMITED BY ' '
                INTO VL-VENDOR-ID
            END-STRING
                    
            PERFORM 35-SEARCH-VENDOR
            
            WRITE RECORD-REPORT FROM VENDOR-LINE
                AFTER ADVANCING 2 LINES
            PERFORM 60-HEADER-ROUTINE-2
            
            
        .
        
        60-HEADER-ROUTINE-2.
            WRITE RECORD-REPORT FROM HEADING-THREE
                AFTER ADVANCING 2 LINES
               
            MOVE HEADING-FOUR TO RECORD-REPORT
            PERFORM 200-WRITE-A-LINE
        .
        
        100-PROCESS-DATA.
            
                EVALUATE TRUE
                    WHEN FIRST-RECORD = 'YES'
                        MOVE 'NO' TO FIRST-RECORD
                        MOVE M-WAREHOUSE-ID TO H-WAREHOUSE-HOLD
                        MOVE M-VENDOR-ID TO H-VENDOR-HOLD
                        MOVE M-PRODUCT-ID TO H-PRODUCT-HOLD
                        
                        PERFORM 45-PRINT-WAREHOUSE-HEADER
                        PERFORM 50-PRINT-VENDOR-HEADER
                    WHEN M-WAREHOUSE-ID NOT = H-WAREHOUSE-HOLD
                        PERFORM 300-WAREHOUSE-BREAK
                        PERFORM 45-PRINT-WAREHOUSE-HEADER
                        PERFORM 50-PRINT-VENDOR-HEADER
                    WHEN H-VENDOR-HOLD NOT = M-VENDOR-ID
                        PERFORM 400-VENDOR-BREAK
                        PERFORM 50-PRINT-VENDOR-HEADER
                    WHEN M-PRODUCT-ID NOT = H-PRODUCT-HOLD
                        PERFORM 500-PRODUCT-BREAK
                END-EVALUATE
            
            PERFORM 150-TRAVERSE-DATA
                VARYING SUB FROM 1 BY 1
                    UNTIL SUB > 5
        .
        
        150-TRAVERSE-DATA.
            
            
            
            MOVE M-PRODUCT-ID TO DL-PRODUCT-ID
            
            PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
                EVALUATE TRUE
                    WHEN SUB = 1
                        MOVE M-PRODUCT-NAME(SUB) TO DL-PRODUCT-NAME,
                                                    PTL-PRODUCT-NAME
                    WHEN OTHER
                        MOVE SPACES TO DL-PRODUCT-NAME
                END-EVALUATE
                
                MOVE M-PRODUCT-SIZE(SUB) TO DL-PRODUCT-SIZE
                MOVE M-PRODUCT-TYPE(SUB) TO DL-PRODUCT-TYPE
                MOVE M-NUM-IN-STOCK(SUB) TO DL-IN-STOCK
                MOVE M-PURCHASE-PRICE(SUB) TO DL-TOTAL-COST
                
                EVALUATE DL-PRODUCT-SIZE
                    WHEN 'X'
                        MOVE 'EXTRA LARGE' TO DL-PRODUCT-SIZE
                    WHEN 'L'
                        MOVE 'LARGE' TO DL-PRODUCT-SIZE
                    WHEN 'M'
                        MOVE 'MEDIUM' TO DL-PRODUCT-SIZE
                    WHEN 'S'
                        MOVE 'SMALL' TO DL-PRODUCT-SIZE
                    WHEN 'A'
                        MOVE 'SAMPLE' TO DL-PRODUCT-SIZE
                    WHEN OTHER
                        STRING
                            'BAD' DELIMITED BY ' '
                                '- *' DELIMITED BY SIZE
                            DL-PRODUCT-SIZE DELIMITED BY ' '
                            INTO H-PRODUCT-SIZE-HOLD
                        END-STRING
                        MOVE H-PRODUCT-SIZE-HOLD TO DL-PRODUCT-SIZE
                END-EVALUATE
                
                EVALUATE DL-PRODUCT-TYPE
                    WHEN 'C'
                        MOVE 'CREAM' TO DL-PRODUCT-TYPE
                    WHEN 'O'
                        MOVE 'OIL' TO DL-PRODUCT-TYPE
                    WHEN OTHER
                        STRING
                            'BAD' DELIMITED BY ' '
                                ' *' DELIMITED BY SIZE
                            DL-PRODUCT-TYPE DELIMITED BY ' '
                            INTO H-PRODUCT-TYPE-HOLD
                        END-STRING
                        MOVE H-PRODUCT-TYPE-HOLD TO DL-PRODUCT-TYPE
                END-EVALUATE
                
                COMPUTE H-PURCHASE-PRICE-HOLD = M-NUM-IN-STOCK(SUB) *
                                                M-PURCHASE-PRICE(SUB)
                COMPUTE TF-PRODUCT-TOTAL = H-PURCHASE-PRICE-HOLD +
                                           TF-PRODUCT-TOTAL
                COMPUTE TF-VENDOR-TOTAL = H-PURCHASE-PRICE-HOLD +
                                           TF-VENDOR-TOTAL
                COMPUTE TF-WAREHOUSE-TOTAL = H-PURCHASE-PRICE-HOLD +
                                           TF-WAREHOUSE-TOTAL
                COMPUTE TF-GRAND-TOTAL = H-PURCHASE-PRICE-HOLD +
                                           TF-GRAND-TOTAL
                
                MOVE H-PURCHASE-PRICE-HOLD TO DL-TOTAL-COST
                
            WRITE RECORD-REPORT FROM DETAIL-LINE
                AFTER ADVANCING PROPER-SPACING
        .
        
        200-WRITE-A-LINE.
            WRITE RECORD-REPORT
                AFTER ADVANCING PROPER-SPACING
        .
        
        300-WAREHOUSE-BREAK.
            PERFORM 400-VENDOR-BREAK
            
            MOVE H-WAREHOUSE-HOLD TO WTL-WAREHOUSE-ID
      * WILL BE REMOVING TO WORK WITH ERROR'D WAREHOUSES
      
            MOVE TF-WAREHOUSE-TOTAL TO WTL-WAREHOUSE-TOTAL
            MOVE WAREHOUSE-TOTAL-LINE TO RECORD-REPORT
            MOVE 2 TO PROPER-SPACING
            PERFORM 200-WRITE-A-LINE
            MOVE 1 TO PROPER-SPACING
            MOVE ZEROS TO WTL-WAREHOUSE-TOTAL
            MOVE ZEROS TO TF-WAREHOUSE-TOTAL
            
            
            MOVE M-WAREHOUSE-ID TO H-WAREHOUSE-HOLD
        .
        
        400-VENDOR-BREAK.
            PERFORM 500-PRODUCT-BREAK
            
            MOVE TF-VENDOR-TOTAL TO VTL-VENDOR-TOTAL
            MOVE VENDOR-TOTAL-LINE TO RECORD-REPORT
            MOVE 2 TO PROPER-SPACING
            PERFORM 200-WRITE-A-LINE
            MOVE 1 TO PROPER-SPACING
            MOVE ZEROS TO VTL-VENDOR-TOTAL
            MOVE ZEROS TO TF-VENDOR-TOTAL
            
            MOVE M-VENDOR-ID TO H-VENDOR-HOLD
        .
        
        500-PRODUCT-BREAK.
            MOVE TF-PRODUCT-TOTAL TO PTL-PRODUCT-TOTAL
            MOVE PRODUCT-TOTAL-LINE TO RECORD-REPORT
            MOVE 2 TO PROPER-SPACING
            PERFORM 200-WRITE-A-LINE
            MOVE 1 TO PROPER-SPACING
            MOVE ZEROS TO PTL-PRODUCT-TOTAL
            MOVE ZEROS TO TF-PRODUCT-TOTAL
            
            MOVE M-PRODUCT-ID TO H-PRODUCT-HOLD
        .
        
        700-EOJ.
            PERFORM 300-WAREHOUSE-BREAK
            
            MOVE ERROR-COUNTER TO EL-ERROR-COUNTER
            WRITE RECORD-REPORT FROM ERROR-LINE
                AFTER ADVANCING 2 LINES
        .
        
        800-PRINT-GRAND-TOTALS.
            MOVE TF-GRAND-TOTAL TO GTL-GRAND-TOTAL
            MOVE GRAND-TOTAL-LINE TO RECORD-REPORT
            MOVE 3 TO PROPER-SPACING
            PERFORM 200-WRITE-A-LINE
        .
        
        900-FINALE.
            
            CLOSE MERGED-RECORD
                  OUTPUT-REPORT
                  ERROR-REPORT
            STOP RUN
        .
