IDENTIFICATION DIVISION.
PROGRAM-ID. MAIN.
ENVIRONMENT DIVISION.
DATA DIVISION.
WORKING-STORAGE SECTION.
    01 READ-CUSTOMERS        PIC X(14) VALUE "READ-CUSTOMERS".
    01 READ-INVENTORY        PIC X(14) VALUE "READ-INVENTORY".
    01 READ-TRANSACTIONS     PIC X(17) VALUE "READ-TRANSACTIONS".
    *> customer table used for holding customer information read from file
    01 CUSTOMER-TABLE.
        05 CUSTOMER                 OCCURS 10 TIMES INDEXED BY I.
            10 CUSTOMER-ID          PIC 9(10).
            10 CUSTOMER-NAME        PIC A(23).
            10 CUSTOMER-ADDRESS     PIC X(23).
            10 CUSTOMER-CITY        PIC A(13).
            10 CUSTOMER-STATE       PIC A(12).
            10 CUSTOMER-OWES        PIC S9(3)V9(2).
    *> Inventory table used to hold data read from file
    01 INVENTORY-TABLE.
        05 INVENTORY                OCCURS 24 TIMES INDEXED BY J.
            10 PRODUCT-ID           PIC 9(11).
            10 PRODUCT-NAME         PIC X(25).
            10 PRODUCT-IN-STOCK     PIC 9(2).
            10 EMPTY-INV            PIC 9(5).
            10 PRODUCT-MIN-STOCK    PIC 9(2).
            10 EMPTY-INV-2          PIC 9(5).
            10 PRODUCT-PRICE        PIC 99V99.

 PROCEDURE DIVISION.
*>  Main program controller. Calls subprograms
 000-MAIN.
    CALL READ-CUSTOMERS USING CUSTOMER-TABLE.
    CALL READ-INVENTORY USING INVENTORY-TABLE.
    CALL READ-TRANSACTIONS USING CUSTOMER-TABLE, INVENTORY-TABLE.
    STOP RUN.