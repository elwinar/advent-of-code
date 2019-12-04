       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-1b.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT InputFile ASSIGN TO "1.input"
               ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile
           RECORD IS VARYING IN SIZE
           FROM 1 TO 10 CHARACTERS.
       01 InputRec    PIC X(9).
       88 EndOfFile   VALUE HIGH-VALUES.

       WORKING-STORAGE SECTION.
       01 ModuleMass   PIC 9(9).
       01 ModuleFuel   PIC S9(9).
       01 TotalFuel    PIC 9(9).
       01 TotalFuelFmt PIC Z(9).

       PROCEDURE DIVISION.
       Begin.
           OPEN INPUT InputFile.
           READ InputFile
               AT END SET EndOfFile TO TRUE
           END-READ.
           PERFORM UNTIL EndOfFile
             COMPUTE ModuleMass = FUNCTION NUMVAL(InputRec)
             PERFORM UNTIL ModuleMass = 0
                 COMPUTE ModuleFuel = (ModuleMass / 3) - 2
                 IF ModuleFuel IS LESS THAN 0
                     SET ModuleFuel TO 0
                 END-IF
                 COMPUTE TotalFuel = TotalFuel + ModuleFuel
                 SET ModuleMass TO ModuleFuel
             END-PERFORM
             READ InputFile
               AT END SET EndOfFile TO TRUE
             END-READ
           END-PERFORM.
           CLOSE InputFile.
           MOVE TotalFuel TO TotalFuelFmt.
           DISPLAY TotalFuelFmt.
           STOP RUN.
