*> Reads transaction.dat and processes each entry one at a time
*>  Checks if customer ID associated with the transaction is valid
*>  Checks if product ID associated with the transaction is valid
*>      - if either is invalid, an error entry is generated
*>  Computes the gross cost owed by a customer
*>      - applies cost of new order including discount, if it exists
*>  Updates the inventory table
*>      - subtracts orders from inventory
*>      - if an inventory item reaches minimum stock point then a report
*>          is generated denoting how much inventory to order
*>  Generates report of all processed transactions
IDENTIFICATION DIVISION.
PROGRAM-ID. READ-TRANSACTIONS.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT IN-FILE ASSIGN TO "transactions.dat"
        ORGANIZATION IS LINE SEQUENTIAL.
    SELECT TRANSACTION-FILE ASSIGN TO "transact.txt"
            ORGANIZATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL.
	SELECT REORDER-FILE ASSIGN TO "reorder.txt"
			ORGANIZATION IS LINE SEQUENTIAL
			ACCESS MODE IS SEQUENTIAL.
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

    *> transaction processed output file definition
    FD TRANSACTION-FILE.
    01 TRANSACTION-RECORD.
        05 TRANSACT-CID                 PIC 9(10). *> transaction id
        05 TRANSACT-NAME                PIC A(23).
        05 TRANSACT-STREET              PIC X(23).
        05 TRANSACT-CITY                PIC A(13).
        05 TRANSACT-STATE               PIC A(12).
        05 TRANSACT-PID                 PIC 9(11).
        05 TRANSACT-ITEM                PIC X(25).
        05 TRANSACT-SPACEA              PIC A(1).
        05 TRANSACT-QUANTITY            PIC 9(3).
        05 TRANSACT-SPACEB              PIC A(1).
        05 TRANSACT-GROSS               PIC S9(3)V9(2).
        05 TRANSACT-SPACEC              PIC A(1).
        05 TRANSACT-DISCOUNT            PIC S9(3)V9(2).
        05 TRANSACT-SPACED              PIC A(1).
        05 TRANSACT-NET                 PIC S9(3)V9(2).
        05 TRANSACT-SPACEE              PIC A(1).
        05 TRANSACT-OWES                PIC S9(3)V9(2).
        
    *>reorder file definition
    FD REORDER-FILE.
    01 REORDER-RECORD.
        05 REORDER-PID					PIC 9(11).
        05 REORDER-QUANTITY				PIC 9(3).

WORKING-STORAGE SECTION.
    *> reference to write-error program
    01 WRITE-ERROR      PIC X(11) VALUE "WRITE-ERROR".

    01 SWITCHES.
        *> flag for reaching end of file
        05 EOF-SWITCH       PIC A VALUE "N".
        *> flag for determining if CID and PID are valid
        05 ALL-VALID        PIC A VALUE "N".

    01 ERRORS.*> used to build error report
        05 INVALID-CUSTOMER PIC A(17) VALUE "INVALID CUSTOMER ".
        05 INVALID-PRODUCT  PIC A(17) VALUE "INVALID PRODUCT  ".

    01 REC-COUNTER          PIC 9 VALUE 0.

    01 TEMP PIC 999v99 VALUE 000.00. *> Holds temporary computations
    01 GROSS-COST PIC 999v99 VALUE 000.00.
    01 DISCOUNT PIC 999v99 VALUE 000.00.
    01 NET-COST PIC 999v99 VALUE 000.00.

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
            10 PRODUCT-IN-STOCK     PIC 9(2).
            10 EMPTY-INV            PIC 9(5).
            10 PRODUCT-MIN-STOCK    PIC 9(2).
            10 EMPTY-INV-2          PIC 9(5).
            10 PRODUCT-PRICE        PIC 99V99.

PROCEDURE DIVISION USING CUSTOMER-TABLE, INVENTORY-TABLE.
*> Program controller
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
        PERFORM 400-COMPUTE-COST
        PERFORM 600-OUTPUT-TRANSACTIONS
        PERFORM 500-UPDATE-INVENTORY
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

*> Computes gross cost of a transaction,
*> applies sale discount if applicable,
*> updates customer table to reflect new amount customer owes
400-COMPUTE-COST.
    COMPUTE GROSS-COST = (PRODUCT-PRICE(J) * PRODUCT-ORDERED).
    EVALUATE SALE-CODE
        WHEN "A" *> 10 percent off
            COMPUTE DISCOUNT = GROSS-COST * .1
        WHEN "B" *> 20 percent off
            COMPUTE DISCOUNT = GROSS-COST * .2
        WHEN "C" *> 25 percent off
            COMPUTE DISCOUNT = GROSS-COST * .25
        WHEN "D" *> buy at least 3, get one free
            IF PRODUCT-ORDERED > 3
                COMPUTE DISCOUNT = PRODUCT-PRICE(J)
            END-IF
        WHEN "E" *> buy one, get one free
            COMPUTE DISCOUNT = (PRODUCT-PRICE(J) * (PRODUCT-ORDERED / 2))
    END-EVALUATE.
    COMPUTE NET-COST = GROSS-COST - DISCOUNT.
    COMPUTE TEMP = CUSTOMER-OWES(I) + NET-COST.
    MOVE TEMP to CUSTOMER-OWES(I).

*>updates the inventory table to reflect new amount in stock after order,
*> if new stock is less than the minimum stock required,
*> an amount to order to bring stock to minimum is calculated
500-UPDATE-INVENTORY.
    COMPUTE TEMP = (PRODUCT-IN-STOCK(J) - PRODUCT-ORDERED).
    MOVE TEMP TO PRODUCT-IN-STOCK(J).
    IF PRODUCT-IN-STOCK(J) <= PRODUCT-MIN-STOCK(J)
        *> DISPLAY "NEED TO ORDER MORE"
        EVALUATE PRODUCT-MIN-STOCK(J)
            WHEN 1 *> order enough to have 3 in stock
                COMPUTE TEMP = (3 - PRODUCT-IN-STOCK(J))
            WHEN 2 THRU 5 *> order enough to have 6 in stock
                COMPUTE TEMP = (6 - PRODUCT-IN-STOCK(J))
            WHEN 6 THRU 10 *> order enough to have 12 in stock
                COMPUTE TEMP = (12 - PRODUCT-IN-STOCK(J))
            WHEN 11 THRU 20 *> order enough to have 25 in stock
                COMPUTE TEMP = (25 - PRODUCT-IN-STOCK(J))
            WHEN OTHER *> order enough to have 30 in stock
                COMPUTE TEMP = (30 - PRODUCT-IN-STOCK(J))
        END-EVALUATE
        DISPLAY "ORDERING " TEMP
        PERFORM 510-OUTPUT-REORDERS
    END-IF.

*> Moves Reorder inventory information to file definition
*> Writes reorder file definition to file
510-OUTPUT-REORDERS.
    OPEN EXTEND REORDER-FILE.
    MOVE PRODUCT-ID(J) TO REORDER-PID.
    MOVE TEMP TO REORDER-QUANTITY.
    WRITE REORDER-RECORD.
    CLOSE REORDER-FILE.

*> Moves required fields to the output file definition
*> writes transactions pocessed file definition to file
600-OUTPUT-TRANSACTIONS.
    OPEN EXTEND TRANSACTION-FILE.
    MOVE CUSTOMER-ID(I) to TRANSACT-CID.
    MOVE CUSTOMER-NAME(I) to TRANSACT-NAME.
    MOVE CUSTOMER-ADDRESS(I) to TRANSACT-STREET.
    MOVE CUSTOMER-CITY(I) to TRANSACT-CITY.
    MOVE CUSTOMER-STATE(I) TO TRANSACT-STATE.
    MOVE PRODUCT-ID(J) to TRANSACT-PID.
    MOVE PRODUCT-NAME(J) to TRANSACT-ITEM.
    MOVE PRODUCT-ORDERED to TRANSACT-QUANTITY.
    MOVE GROSS-COST to TRANSACT-GROSS.
    MOVE DISCOUNT to TRANSACT-DISCOUNT.
    MOVE NET-COST to TRANSACT-NET.
    MOVE TEMP to TRANSACT-OWES.
    MOVE " " to TRANSACT-SPACEA.
    MOVE " " to TRANSACT-SPACEB.
    MOVE " " to TRANSACT-SPACEC.
    MOVE " " to TRANSACT-SPACED.
    MOVE " " to TRANSACT-SPACEE.
    WRITE TRANSACTION-RECORD.
    CLOSE TRANSACTION-FILE.

*> Clean up
900-TERMINATE.
    CLOSE IN-FILE.