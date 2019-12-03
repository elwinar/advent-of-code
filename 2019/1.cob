       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-1.

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
       01 InputRec.
           88 EndOfFile  VALUE HIGH-VALUES.
           02 ModuleMass PIC X(9).

       WORKING-STORAGE SECTION.
       01 ModuleFuel   PIC 9(9).
       01 TotalFuel    PIC 9(9).

       PROCEDURE DIVISION.
       Begin.
           OPEN INPUT InputFile.
           READ InputFile
               AT END SET EndOfFile TO TRUE
           END-READ.
           PERFORM UNTIL EndOfFile
             COMPUTE ModuleFuel = FUNCTION NUMVAL(ModuleMass) / 3 - 2
             COMPUTE TotalFuel = TotalFuel + ModuleFuel
             SET EndOfFile TO TRUE
             READ InputFile
               AT END SET EndOfFile TO TRUE
             END-READ
           END-PERFORM.
           CLOSE InputFile.
           DISPLAY TotalFuel.
           STOP RUN.
