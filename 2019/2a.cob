       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-2a.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO "2.input"
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile RECORD IS VARYING IN SIZE FROM 1 TO 9999 CHARACTERS.
       01 InputRec  PIC X(9999).

       WORKING-STORAGE SECTION.
       01 OpTable.
          02 Len    PIC 9(5).
          02 Op     PIC 9(15) OCCURS 0 TO 9999 TIMES DEPENDING ON Len.
       01 SepNumber PIC 9(5).
       01 Ptr       PIC 9(5).
       01 Addr      PIC 9(5).
       01 Val       PIC X(15).
       01 Res       PIC 9(15).
       01 ResFmt    PIC Z(15).

       PROCEDURE DIVISION.
       Begin.
           OPEN INPUT InputFile.
           READ InputFile.

           INSPECT InputRec TALLYING SepNumber FOR ALL ",".
           IF SepNumber < 9999
            ADD 1 TO SepNumber
           ELSE
            MOVE 9999 TO SepNumber
           END-IF.

           MOVE 1 TO Ptr.
           PERFORM SepNumber TIMES
            UNSTRING InputRec DELIMITED BY "," INTO Val
             WITH POINTER Ptr
            END-UNSTRING
            ADD 1 TO Len
            COMPUTE Op(Len) = FUNCTION NUMVAL(Val)
           END-PERFORM.

           CLOSE InputFile.

           MOVE 12 TO Op(2)
           MOVE 2 TO Op(3)

           MOVE 1 TO Ptr.
           PERFORM UNTIL Ptr IS GREATER THAN Len
            EVALUATE Op(Ptr)
            WHEN 1
             ADD 1 TO Ptr
             COMPUTE Addr = Op(Ptr) + 1
             MOVE Op(Addr) TO Res
             ADD 1 TO Ptr
             COMPUTE Addr = Op(Ptr) + 1
             ADD Op(Addr) TO Res
             ADD 1 TO Ptr
             COMPUTE Addr = Op(Ptr) + 1
             MOVE Res TO Op(Addr)
            WHEN 2
             ADD 1 TO Ptr
             COMPUTE Addr = Op(Ptr) + 1
             MOVE Op(Addr) TO Res
             ADD 1 TO Ptr
             COMPUTE Addr = Op(Ptr) + 1
             MULTIPLY Op(Addr) BY Res
             ADD 1 TO Ptr
             COMPUTE Addr = Op(Ptr) + 1
             MOVE Res TO Op(Addr)
            WHEN 99
             MOVE Op(1) TO ResFmt
             DISPLAY ResFmt
             STOP RUN
            END-EVALUATE
            ADD 1 TO Ptr
           END-PERFORM.

