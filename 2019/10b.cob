       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-10b.

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
       01 InputPos PIC 9(4).

       01 Roids OCCURS 1 TO 9999 TIMES DEPENDING ON RoidsLen.
        02 Abs PIC 9(2).
        02 Ord PIC 9(2).
        02 Dist USAGE FLOAT-LONG.
        02 Ang USAGE FLOAT-LONG.
       01 RoidsLen PIC 9(4).

       01 Idx PIC 9(4).
       01 Jdx PIC 9(4).
       01 Kdx PIC 9(4).
       01 N PIC 9(4).

       01 cX USAGE FLOAT-LONG.
       01 cY USAGE FLOAT-LONG.
       01 cR USAGE FLOAT-LONG.
       01 cP USAGE FLOAT-LONG.

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           ACCEPT InputPos FROM ARGUMENT-VALUE.
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
             COMPUTE Abs(RoidsLen) = Jdx - 1
             COMPUTE Ord(RoidsLen) = Idx - 1
            END-PERFORM

            ADD 1 TO Idx
            READ InputFile AT END EXIT PERFORM
           END-PERFORM.

           CLOSE InputFile.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > RoidsLen
            COMPUTE cX = Abs(Idx) - Abs(InputPos)
            COMPUTE cY = Ord(Idx) - Ord(InputPos)
            COMPUTE cR = FUNCTION SQRT(cX**2 + cY**2)
            COMPUTE cP = (180 * FUNCTION ACOS(
              cX / FUNCTION SQRT(cX**2 + cY**2)
             )) / FUNCTION PI()
            IF cY IS LESS THAN 0
             COMPUTE cP = FUNCTION MOD(360 - cP, 360)
            END-IF
            COMPUTE cP = FUNCTION MOD(360 + cP - 270, 360)
            MOVE cP TO Ang(Idx)
            MOVE cR TO Dist(Idx)
           END-PERFORM.

           SORT Roids ON 
            ASCENDING KEY Ang
            ASCENDING KEY Dist.

           MOVE Ang(1) TO cP
           PERFORM VARYING Idx FROM 2 UNTIL Idx > RoidsLen
            IF Ang(Idx) IS EQUAL TO cP
             DISPLAY "Offsetting" " " Idx
             COMPUTE Ang(Idx) = Ang(Idx - 1) + 360
             EXIT PERFORM CYCLE
            END-IF
            MOVE Ang(Idx) TO cP
           END-PERFORM.
           SORT Roids ON 
            ASCENDING KEY Ang
            ASCENDING KEY Dist.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > RoidsLen
            DISPLAY Idx " " Abs(Idx) "," Ord(Idx) " " Ang(Idx) ","
            Dist(Idx)
           END-PERFORM.
