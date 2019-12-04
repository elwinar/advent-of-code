       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-2b.

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
       01 Len    PIC 9(5).
       01 InputTable.
          02 IOp     PIC 9(15) OCCURS 0 TO 9999 TIMES DEPENDING ON Len.
       01 OpTable.
          02 Op      PIC 9(15) OCCURS 0 TO 9999 TIMES DEPENDING ON Len.
       01 SepNumber  PIC 9(5).
       01 Ptr        PIC 9(5).
       01 Addr       PIC 9(5).
       01 Val        PIC X(15).
       01 Res        PIC 9(15).
       01 ResFmt     PIC Z(15).
       01 Noun       PIC 9(5).
       01 Verb       PIC 9(5).

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
            COMPUTE IOp(Len) = FUNCTION NUMVAL(Val)
           END-PERFORM.

           CLOSE InputFile.

           PERFORM VARYING Noun FROM 0 UNTIL Noun GREATER THAN 99
            PERFORM VARYING Verb FROM 0 UNTIL Verb GREATER THAN 99

             PERFORM VARYING Ptr FROM 1 UNTIL Ptr IS GREATER THAN Len
              MOVE IOp(Ptr) TO Op(Ptr)
             END-PERFORM

             MOVE Noun TO Op(2)
             MOVE Verb TO Op(3)

             MOVE 1 TO Ptr
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
               EXIT PERFORM
              END-EVALUATE
              ADD 1 TO Ptr
             END-PERFORM

             IF Res EQUAL 19690720
             THEN
              COMPUTE Res = 100 * Noun + Verb
              MOVE Res TO ResFmt
              DISPLAY ResFmt
              STOP RUN
             END-IF

            END-PERFORM
           END-PERFORM.

