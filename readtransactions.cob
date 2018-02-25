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
        01 IN-RECORD. *> input from file
            05 CID                  PIC 9(10). *>customer id
            05 PID                  PIC 9(11). *> product id
            05 PRODUCT-ORDERED      PIC 9.  *> # ordered
            05 EMPTY                PIC X(5). *> white space in file
                                              *> was interupting input
            05 SALE-CODE            PIC A(1).  *> sale code

    WORKING-STORAGE SECTION.
        *> reference to write-error program
        01 WRITE-ERROR      PIC X(11) VALUE "WRITE-ERROR".

        01 SWITCHES.
            *> flag for reaching end of file
            05 EOF-SWITCH       PIC A VALUE "N".
            05 ALL-VALID        PIC A VALUE "N".

        01 ERRORS.*> used to build error report
            05 INVALID-CUSTOMER PIC A(17) VALUE "INVALID CUSTOMER ".
            05 INVALID-PRODUCT  PIC A(17) VALUE "INVALID PRODUCT  ".

        01 TEMP PIC 999v99 VALUE 000.00.
        01 SUB-TOTAL PIC 999v99 VALUE 000.00.
        01 TOTAL PIC 999V99.

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
                10 PRODUCT-PRICE        PIC 99V99.

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

*> Reads current record from file and processes
200-PROCESS.
    
    MOVE "N" TO ALL-VALID.
    PERFORM 300-CHECK-CID.

    IF ALL-VALID = "Y" THEN

        *>      TODO
        *>  PERFORM CALCULATION OF NEW COST

        DISPLAY "number ordered " PRODUCT-ORDERED
        DISPLAY "original amount owed " CUSTOMER-OWES(I)
        DISPLAY "product price " PRODUCT-PRICE(J)

        COMPUTE SUB-TOTAL = (PRODUCT-PRICE(J) * PRODUCT-ORDERED)

        EVALUATE SALE-CODE
            WHEN "A" *> 10 percent off
                COMPUTE SUB-TOTAL = SUB-TOTAL * .9
                DISPLAY "sub-total (10% off) " SUB-TOTAL
            WHEN "B" *> 20 percent off
                COMPUTE SUB-TOTAL = SUB-TOTAL * .8
                DISPLAY "sub-total (20% off) " SUB-TOTAL
            WHEN "C" *> 25 percent off
                COMPUTE SUB-TOTAL = SUB-TOTAL * .75
                DISPLAY "sub-total (25% off) " SUB-TOTAL
            WHEN "D" *> buy at least 3, get one free
                IF PRODUCT-ORDERED > 3
                    COMPUTE SUB-TOTAL = (PRODUCT-PRICE(J) * (PRODUCT-ORDERED - 1))
                END-IF
                DISPLAY "sub-total (buy at least 3, get 1) " SUB-TOTAL
            WHEN "E" *> buy one, get one free
                COMPUTE SUB-TOTAL = (PRODUCT-PRICE(J) * (PRODUCT-ORDERED / 2))
                DISPLAY "sub-total (BOGO) " SUB-TOTAL
            WHEN "Z" *> no discount
                DISPLAY "sub-total (no discount) " SUB-TOTAL
        END-EVALUATE

        COMPUTE TEMP = CUSTOMER-OWES(I) + SUB-TOTAL
        
        DISPLAY "new ammount owed " TEMP
        DISPLAY "------------------"

        *>  PERFORM INVENTORY UPDATE
	
        *>  PERFORM GENERATE OUTPUT FILE

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