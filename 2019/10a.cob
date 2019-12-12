       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-10a.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO InputPath
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile RECORD IS VARYING IN SIZE FROM 1 TO 9999 CHARACTERS
           DEPENDING ON InputLen.
       01 InputRec.
        02 Cell PIC X(1) OCCURS 1 TO 9999 TIMES DEPENDING ON InputLen.

       WORKING-STORAGE SECTION.
       01 InputPath PIC X(50).
       01 InputLen PIC 9(4).

       01 Roids OCCURS 1 TO 9999 TIMES DEPENDING ON RoidsLen.
        02 Abs PIC 9(2).
        02 Ord PIC 9(2).
       01 RoidsLen PIC 9(4).

       01 Idx PIC 9(4).
       01 Jdx PIC 9(4).
       01 Kdx PIC 9(4).

       01 cX USAGE FLOAT-LONG.
       01 cY USAGE FLOAT-LONG.
       01 cR USAGE FLOAT-LONG.
       01 cP USAGE FLOAT-LONG.
       01 Angles USAGE FLOAT-LONG OCCURS 1 TO 9999 TIMES
           DEPENDING ON RoidsLen.
       01 LoS PIC 9(4).
       01 BestLoS PIC 9(4).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.

           READ InputFile.
           MOVE 1 TO Idx.
           MOVE 0 TO RoidsLen.
           PERFORM FOREVER
            PERFORM VARYING Jdx FROM 1 UNTIL Jdx > InputLen
             IF Cell(Jdx) IS NOT EQUAL TO "#" 
              EXIT PERFORM CYCLE
             END-IF

             ADD 1 TO RoidsLen
             MOVE Idx TO Abs(RoidsLen)
             MOVE Jdx TO Ord(RoidsLen)
            END-PERFORM

            ADD 1 TO Idx
            READ InputFile AT END EXIT PERFORM
           END-PERFORM.

           CLOSE InputFile.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > RoidsLen
            MOVE 0 TO LoS

            PERFORM VARYING Jdx FROM 1 UNTIL Jdx > RoidsLen
             COMPUTE cX = Abs(Jdx) - Abs(Idx)
             COMPUTE cY = Ord(Jdx) - Ord(Idx)
             COMPUTE cR = FUNCTION SQRT(cX**2 + cY**2)
             COMPUTE cP = (180 * FUNCTION ACOS(
               cX / FUNCTION SQRT(cX**2 + cY**2)
              )) / FUNCTION PI()
             IF cY IS LESS THAN 0
              COMPUTE cP = -cP
             END-IF

             MOVE cP TO Angles(Jdx)
             PERFORM VARYING Kdx FROM 1 UNTIL Kdx >= Jdx
              IF Angles(Kdx) IS EQUAL TO cP
               EXIT PERFORM
              END-IF
             END-PERFORM
             IF Kdx IS EQUAL TO Jdx
              ADD 1 TO LoS
             END-IF

            END-PERFORM

            DISPLAY Idx " " Abs(Idx) "," Ord(Idx) " " LoS
            IF LoS IS GREATER THAN BestLoS
             MOVE LoS TO BestLoS
            END-IF
           END-PERFORM.

           DISPLAY " "
           DISPLAY BestLoS.


