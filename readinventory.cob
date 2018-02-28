*> Reads inventory.dat and stores data in a table
IDENTIFICATION DIVISION.
PROGRAM-ID. READ-INVENTORY.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT IN-FILE ASSIGN TO "inventory.dat"
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
        FD IN-FILE. *> inventory.dat file definition
        01 IN-RECORD.
            05 PID          PIC 9(11). *> product id
            05 NAME         PIC X(25).
            05 IN-STOCK     PIC 9(2).
            05 EMPTY        PIC 9(5). *> spaces
            05 MIN-STOCK    PIC 9(2).
            05 EMPTY2       PIC 9(5). *> spaces
            05 PRICE        PIC 99V99.

    WORKING-STORAGE SECTION.
        01 SWITCHES.
            *> flag for reaching end of file
            05 EOF-SWITCH       PIC X VALUE "N".

*> ARGUMENTS
LINKAGE SECTION.
*> Table of size 24, contains INVENTORY from input file
        01 INVENTORY-TABLE.
            05 INVENTORY                OCCURS 24 TIMES INDEXED BY I.
                10 PRODUCT-ID           PIC 9(11).
                10 PRODUCT-NAME         PIC X(25).
                10 PRODUCT-IN-STOCK     PIC 9(2).
                10 EMPTY-INV            PIC 9(5).
                10 PRODUCT-MIN-STOCK    PIC 9(2).
                10 EMPTY-INV-2          PIC 9(5).
                10 PRODUCT-PRICE        PIC 99V99.
                
PROCEDURE DIVISION USING INVENTORY-TABLE.
*> Main program controller
000-MAIN.
    PERFORM 100-INIT.
    PERFORM 200-PROCESS-INVENTORY UNTIL EOF-SWITCH = "Y".
    PERFORM 300-TERMINATE.
    GOBACK.

*> Begins file input, opens file
100-INIT.
    OPEN INPUT IN-FILE.
    SET I TO 0.*> initialize table index to 0
    READ IN-FILE
        AT END
            MOVE "Y" TO EOF-SWITCH
        NOT AT END
            ADD 1 TO I
    END-READ.

*> Reads current record from file inserts the data into the table
200-PROCESS-INVENTORY.
    MOVE IN-RECORD TO INVENTORY(I).*> insert record into table
    READ IN-FILE*> retrieve next record from file
        AT END
            MOVE "Y" TO EOF-SWITCH
        NOT AT END
            ADD 1 TO I
    END-READ.

*> Clean up
300-TERMINATE.
    CLOSE IN-FILE.

