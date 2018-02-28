*> Reads customers.dat and stores data in a table
IDENTIFICATION DIVISION.
PROGRAM-ID. READ-CUSTOMERS.
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT IN-FILE ASSIGN TO "customers.dat"
        ORGANIZATION IS LINE SEQUENTIAL.
DATA DIVISION.
    FILE SECTION.
        FD IN-FILE.
        *> customers.dat file definition
        01 IN-RECORD.
            05 CID         PIC 9(10). *> customer id
            05 NAME        PIC A(23). *> customer name
            05 STREET      PIC X(23).
            05 CITY        PIC A(13).
            05 STATE       PIC A(12).
            05 OWES        PIC S9(3)V9(2). *> current ammount owed

    WORKING-STORAGE SECTION.
        01 SWITCHES.
            *> flag for reaching end of file
            05 EOF-SWITCH       PIC X VALUE "N".
    
    *> ARGUMENTS
    LINKAGE SECTION.
        *> Table of size 10, contains customers from input file
        01 CUSTOMER-TABLE.
            05 CUSTOMER                 OCCURS 10 TIMES INDEXED BY I.
                10 CUSTOMER-ID          PIC 9(10).
                10 CUSTOMER-NAME        PIC A(23).
                10 CUSTOMER-ADDRESS     PIC X(23).
                10 CUSTOMER-CITY        PIC A(13).
                10 CUSTOMER-STATE       PIC A(12).
                10 CUSTOMER-OWES        PIC S9(3)V9(2).

PROCEDURE DIVISION USING CUSTOMER-TABLE.

*> Program controller
000-MAIN.
    PERFORM 100-INIT.
    PERFORM 200-PROCESS-CUSTOMERS UNTIL EOF-SWITCH = "Y".
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
200-PROCESS-CUSTOMERS.
    MOVE IN-RECORD TO CUSTOMER(I).*> insert record into table
    *> retrieve next record from file
    READ IN-FILE
        AT END
            MOVE "Y" TO EOF-SWITCH
        NOT AT END
            ADD 1 TO I
    END-READ.

*> Clean up
300-TERMINATE.
    CLOSE IN-FILE.

