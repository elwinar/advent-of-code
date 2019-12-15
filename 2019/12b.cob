       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-12b.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO InputPath
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile RECORD IS VARYING IN SIZE FROM 1 TO 9999 CHARACTERS.
      * The input file has been aligned by hand so the parsing is easier,
      * because I'm lazy.
       01 InputRec.
        02 FILLER PIC X(3).
        02 InputPosX PIC X(3).
        02 FILLER PIC X(4).
        02 InputPosY PIC X(3).
        02 FILLER PIC X(4).
        02 InputPosZ PIC X(3).

       WORKING-STORAGE SECTION.
       01 InputPath PIC X(50).

       01 Idx       PIC 9(10).
       01 Jdx       PIC 9(10).

       01 Moons OCCURS 4 TIMES.
        02 PosX PIC S9(5).
        02 PosY PIC S9(5).
        02 PosZ PIC S9(5).
        02 VelX PIC S9(5) VALUE 0.
        02 VelY PIC S9(5) VALUE 0.
        02 VelZ PIC S9(5) VALUE 0.

       01 OrigSystem.
        02 FILLER OCCURS 4 TIMES.
           03 OrigPos PIC S9(5).
           03 OrigVel PIC S9(5).
       01 System.
        02 FILLER OCCURS 4 TIMES.
         03 Pos PIC S9(5).
         03 Vel PIC S9(5).
       01 Steps PIC 9(20).
       01 Diff PIC S9(1).

       01 Prime PIC 9(20).
       01 Factors.
        02 Factor PIC 9(20) OCCURS 99 TIMES.
       01 FactorsLen PIC 9(2).
       01 TotalFactors.
        02 TotalFactor PIC 9(20) OCCURS 99 TIMES.
       01 TotalFactorsLen PIC 9(2).
       01 MergedFactors.
        02 MergedFactor PIC 9(20) OCCURS 99 TIMES.
       01 MergedFactorsLen PIC 9(2).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            READ InputFile
            COMPUTE PosX(Idx) = NUMVAL(InputPosX)
            COMPUTE PosY(Idx) = NUMVAL(InputPosY)
            COMPUTE PosZ(Idx) = NUMVAL(InputPosZ)
           END-PERFORM.

           CLOSE InputFile.

           MOVE 0 TO TotalFactorsLen.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            MOVE PosX(Idx) TO Pos(Idx)
            MOVE VelX(Idx) TO Vel(Idx)
           END-PERFORM.
           PERFORM FindLoop.
           DISPLAY "X " Steps.
           PERFORM FactorizeLoop.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            MOVE PosY(Idx) TO Pos(Idx)
            MOVE VelY(Idx) TO Vel(Idx)
           END-PERFORM.
           PERFORM FindLoop.
           DISPLAY "Y " Steps.
           PERFORM FactorizeLoop.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            MOVE PosZ(Idx) TO Pos(Idx)
            MOVE VelZ(Idx) TO Vel(Idx)
           END-PERFORM.
           PERFORM FindLoop.
           DISPLAY "Z " Steps.
           PERFORM FactorizeLoop.

           MOVE 1 TO Steps.
           PERFORM VARYING Idx FROM 1 UNTIL Idx > TotalFactorsLen
            MULTIPLY Steps BY TotalFactor(Idx) GIVING Steps
           END-PERFORM.
           DISPLAY Steps.

           STOP RUN.

       FindLoop SECTION.
           MOVE System TO OrigSystem.

           MOVE 0 TO Steps.
           PERFORM FOREVER
            ADD 1 TO Steps
            PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
             COMPUTE Jdx = Idx + 1
             PERFORM VARYING Jdx FROM Jdx UNTIL Jdx > 4
              COMPUTE Diff = SIGN (Pos(Idx) - Pos(Jdx))
              SUBTRACT Diff FROM Vel(Idx)
              ADD Diff TO Vel(Jdx)
             END-PERFORM
            END-PERFORM

            PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
             COMPUTE Pos(Idx) = Pos(Idx) + Vel(Idx)
            END-PERFORM

            IF System IS EQUAL TO OrigSystem
             EXIT PERFORM
            END-IF
           END-PERFORM.

       FactorizeLoop SECTION.
           MOVE 2 TO Prime.
           MOVE 0 TO FactorsLen.

           PERFORM UNTIL Steps IS EQUAL TO 1
            IF MOD(Steps, Prime) IS NOT EQUAL TO 0
             ADD 1 TO Prime
             EXIT PERFORM CYCLE
            END-IF

            ADD 1 TO FactorsLen
            MOVE Prime TO Factor(FactorsLen)
            DIVIDE Steps BY Prime GIVING Steps
           END-PERFORM.

           MOVE 1 TO Idx.
           MOVE 1 TO Jdx.
           MOVE 0 TO MergedFactorsLen.
           PERFORM UNTIL Idx > FactorsLen OR Jdx > TotalFactorsLen
            ADD 1 TO MergedFactorsLen
            IF Factor(Idx) IS EQUAL TO TotalFactor(Jdx)
             MOVE Factor(Idx) TO MergedFactor(MergedFactorsLen)
             ADD 1 TO Idx
             ADD 1 TO Jdx
            ELSE IF Factor(Idx) IS LESS THAN TotalFactor(Jdx)
             MOVE Factor(Idx) TO MergedFactor(MergedFactorsLen)
             ADD 1 TO Idx
            ELSE
             MOVE TotalFactor(Jdx) TO MergedFactor(MergedFactorsLen)
             ADD 1 TO Jdx
            END-IF
           END-PERFORM.

           PERFORM VARYING Idx FROM Idx UNTIL Idx > FactorsLen
            ADD 1 TO MergedFactorsLen
            MOVE Factor(Idx) TO MergedFactor(MergedFactorsLen)
           END-PERFORM.

           PERFORM VARYING Jdx FROM Jdx UNTIL Jdx > TotalFactorsLen
            ADD 1 TO MergedFactorsLen
            MOVE TotalFactor(Jdx) TO MergedFactor(MergedFactorsLen)
           END-PERFORM.

           MOVE MergedFactors TO TotalFactors.
           MOVE MergedFactorsLen TO TotalFactorsLen.
