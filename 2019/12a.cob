       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-12a.

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

       01 Idx PIC 9(4).
       01 Jdx PIC 9(4).
       01 Kdx PIC 9(4).

       01 Moons OCCURS 4 TIMES.
        02 PosX PIC S9(4).
        02 PosY PIC S9(4).
        02 PosZ PIC S9(4).
        02 VelX PIC S9(4) VALUE 0.
        02 VelY PIC S9(4) VALUE 0.
        02 VelZ PIC S9(4) VALUE 0.
       01 Diff PIC S9(1).
       01 Pot PIC 9(4).
       01 Kin PIC 9(4).
       01 Energy PIC 9(4).
       01 TotalEnergy PIC 9(8).
       01 Steps PIC 9(4).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           ACCEPT Steps FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            READ InputFile
            COMPUTE PosX(Idx) = NUMVAL(InputPosX)
            COMPUTE PosY(Idx) = NUMVAL(InputPosY)
            COMPUTE PosZ(Idx) = NUMVAL(InputPosZ)
           END-PERFORM.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            DISPLAY "x=" PosX(Idx) ",y=" PosY(Idx) ",z=" PosZ(Idx)
                  " vx=" VelX(Idx) ",vy=" VelY(Idx) ",vz=" VelZ(Idx)
           END-PERFORM
           DISPLAY " "

           PERFORM VARYING Kdx FROM 1 UNTIL Kdx > Steps
            PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
             COMPUTE Jdx = Idx + 1
             PERFORM VARYING Jdx FROM Jdx UNTIL Jdx > 4
              COMPUTE Diff = SIGN (PosX(Idx) - PosX(Jdx))
              SUBTRACT Diff FROM VelX(Idx)
              Add Diff TO VelX(Jdx)

              COMPUTE Diff = SIGN (PosY(Idx) - PosY(Jdx))
              SUBTRACT Diff FROM VelY(Idx)
              Add Diff TO VelY(Jdx)

              COMPUTE Diff = SIGN (PosZ(Idx) - PosZ(Jdx))
              SUBTRACT Diff FROM VelZ(Idx)
              Add Diff TO VelZ(Jdx)
             END-PERFORM
            END-PERFORM

            PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
             COMPUTE PosX(Idx) = PosX(Idx) + VelX(Idx)
             COMPUTE PosY(Idx) = PosY(Idx) + VelY(Idx)
             COMPUTE PosZ(Idx) = PosZ(Idx) + VelZ(Idx)
            END-PERFORM

            DISPLAY "Step " Kdx
            PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
             DISPLAY "x=" PosX(Idx) ",y=" PosY(Idx) ",z=" PosZ(Idx)
                   " vx=" VelX(Idx) ",vy=" VelY(Idx) ",vz=" VelZ(Idx)
            END-PERFORM
            DISPLAY " "
           END-PERFORM.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 4
            COMPUTE Pot = ABS(PosX(Idx))
                        + ABS(PosY(Idx))
                        + ABS(PosZ(Idx))
            COMPUTE Kin = ABS(VelX(Idx))
                        + ABS(VelY(Idx))
                        + ABS(VelZ(Idx))
            COMPUTE Energy = Pot * Kin
            DISPLAY Pot " " Kin " " Energy
            ADD Energy TO TotalEnergy
           END-PERFORM.
           DISPLAY TotalEnergy

           CLOSE InputFile.

