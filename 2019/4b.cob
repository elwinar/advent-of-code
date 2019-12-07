       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-4b.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN USING InputPath
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile RECORD IS VARYING IN SIZE FROM 1 TO 13 CHARACTERS.
       01 InputRec.
          02 LowBoundStr  PIC X(6).
          02 FILLER       PIC X(1).
          02 HighBoundStr PIC X(6).

       WORKING-STORAGE SECTION.
       01 InputPath           PIC X(50).
       01 LowBound            PIC 9(6).
       01 HighBound           PIC 9(6).
       01 VInt                PIC 9(6).
       01 FILLER              REDEFINES VInt.
          02 VArr             PIC 9(1) OCCURS 6 TIMES.
       01 C                   PIC 9(6).
       01 Groups              PIC 9(1) OCCURS 6 TIMES.
       01 Idx                 PIC 9(1).
       01 PrevIdx             PIC 9(1).
       01 CounterUpper PIC 9(1).


       PROCEDURE DIVISION.
       Begin SECTION.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.
           READ InputFile.

           COMPUTE LowBound = FUNCTION NUMVAL(LowBoundStr).
           COMPUTE HighBound = FUNCTION NUMVAL(HighBoundStr).
           MOVE 0 TO C.

           PERFORM VARYING VInt FROM LowBound 
            UNTIL VInt IS GREATER THAN HighBound
            IF VArr(1) IS GREATER THAN VArr(2)
             OR VArr(2) IS GREATER THAN VArr(3)
             OR VArr(3) IS GREATER THAN VArr(4)
             OR VArr(4) IS GREATER THAN VArr(5)
             OR VArr(5) IS GREATER THAN VArr(6)
             EXIT PERFORM CYCLE
            END-IF

            IF VArr(1) IS NOT EQUAL TO VArr(2)
             AND VArr(2) IS NOT EQUAL TO VArr(3)
             AND VArr(3) IS NOT EQUAL TO VArr(4)
             AND VArr(4) IS NOT EQUAL TO VArr(5)
             AND VArr(5) IS NOT EQUAL TO VArr(6)
             EXIT PERFORM CYCLE
            END-IF

            PERFORM VARYING Idx FROM 1 UNTIL Idx IS GREATER THAN 6
             MOVE 0 TO Groups(Idx)
            END-PERFORM
            MOVE 1 TO PrevIdx
            MOVE 1 TO CounterUpper
            PERFORM VARYING Idx FROM 2 UNTIL Idx IS GREATER THAN 6
             IF VArr(PrevIdx) IS EQUAL TO VArr(Idx)
              ADD 1 TO CounterUpper
             ELSE
              ADD 1 TO Groups(CounterUpper)
              MOVE 1 TO CounterUpper
             END-IF
             MOVE Idx TO PrevIdx
            END-PERFORM
            ADD 1 TO Groups(CounterUpper)
            IF Groups(2) IS EQUAL TO 0
             EXIT PERFORM CYCLE
            END-IF

            ADD 1 TO C
           END-PERFORM.

           DISPLAY C.

           CLOSE InputFile.
           STOP RUN.

