IDENTIFICATION DIVISION.
PROGRAM-ID. READ-TRANSACTIONS.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT IN-FILE ASSIGN TO "transactions.dat"
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
    FILE SECTION.
        *> input file definition
        FD IN-FILE.
        01 IN-RECORD.
            05 CID                  PIC 9(10).
            05 PID                  PIC 9(11).
            05 PRODUCT-ORDERED      PIC 9(6).
            05 SALE-CODE            PIC A.

    WORKING-STORAGE SECTION.
        01 WRITE-ERROR      PIC X(11) VALUE "WRITE-ERROR".

        01 SWITCHES.
            *> flag for reaching end of file
            05 EOF-SWITCH       PIC A VALUE "N".
            05 ALL-VALID        PIC A VALUE "N".

        01 ERRORS.*> used to build error report
            05 INVALID-CUSTOMER PIC A(17) VALUE "INVALID CUSTOMER ".
            05 INVALID-PRODUCT  PIC A(17) VALUE "INVALID PRODUCT  ".

        01 COUNTERS.
            *> keeps track of records read in from file
            05 REC-COUNTER      PIC 9(3) VALUE 0.

    LINKAGE SECTION.
        *> customer table argument
        01 CUSTOMER-TABLE.
            05 CUSTOMER                 OCCURS 10 TIMES INDEXED BY I.
                10 CUSTOMER-ID          PIC 9(10).
                10 CUSTOMER-NAME        PIC A(23).
                10 CUSTOMER-ADDRESS     PIC X(23).
                10 CUSTOMER-CITY        PIC A(13).
                10 CUSTOMER-STATE       PIC A(12).
                10 CUSTOMER-OWES        PIC S9(3)V9(2).

        *> inventory table argument
        01 INVENTORY-TABLE.
            05 INVENTORY                OCCURS 24 TIMES INDEXED BY J.
                10 PRODUCT-ID           PIC 9(11).
                10 PRODUCT-NAME         PIC X(25).
                10 PRODUCT-IN-STOCK     PIC 9(7).
                10 PRODUCT-MIN-STOCK    PIC 9(7).
                10 PRODUCT-PRICE        PIC S9(2)V9(2).

PROCEDURE DIVISION USING CUSTOMER-TABLE, INVENTORY-TABLE.

000-MAIN.
    PERFORM 100-INIT.
    PERFORM 200-PROCESS UNTIL EOF-SWITCH = "Y".
    PERFORM 900-TERMINATE.
    GOBACK.

*> Begins file input, opens file
100-INIT.
    OPEN INPUT IN-FILE.
    
    READ IN-FILE
        AT END
            MOVE "Y" TO EOF-SWITCH
        NOT AT END
            ADD 1 TO REC-COUNTER
    END-READ.

*> Reads current record from file inserts the data into the table
200-PROCESS.

    PERFORM 300-CHECK-CID.

    IF ALL-VALID = "Y" THEN
        *>      TODO
        *>  PERFORM CALCULATION OF NEW COST
        *>  PERFORM INVENTORY UPDATE
        *>  PERFORM GENERATE OUTPUT FILE
        *>
        DISPLAY "PROCEED WITH CALCULATIONS" *> DEBUG ONLY
    END-IF.

    READ IN-FILE*> retrieve next record from file
        AT END
            MOVE "Y" TO EOF-SWITCH
        NOT AT END
            ADD 1 TO REC-COUNTER
    END-READ.

*> Linear Search
*> check-cid is a linear search to determine whether or not
*> the customer number read in from transactions.dat is a valid
*> customer number. If it's now, an error is generated.
300-CHECK-CID.
    SET I TO 1.
    SEARCH CUSTOMER
        AT END CALL WRITE-ERROR USING INVALID-CUSTOMER, CID, PID, PRODUCT-ORDERED
        WHEN CUSTOMER-ID(I) = CID
        PERFORM 310-CHECK-PID *> valid customer, check product
    END-SEARCH.

*> Linear Search
*> check-pid is a linear search to determine whether or not
*> the product number read in from transactions.dat is a valid
*> product number. If it's now, an error is generated.
310-CHECK-PID.
    SET J TO 1.
    SEARCH INVENTORY
        AT END CALL WRITE-ERROR USING INVALID-PRODUCT, CID, PID, PRODUCT-ORDERED
        WHEN PRODUCT-ID(J) = PID
        MOVE "Y" TO ALL-VALID *> no errors with product. 
    END-SEARCH.

*> Clean up
900-TERMINATE.
    CLOSE IN-FILE.

