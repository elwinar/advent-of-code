       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-8b.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO InputPath
       ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputRec.
        02 Pixel PIC X(1) OCCURS 150 TIMES.

       WORKING-STORAGE SECTION.
       01 InputPath PIC X(50).
       01 EOF       PIC 9(1).

       01 Image.
        02 Pixel    PIC X(1) OCCURS 150 TIMES.
       01 Layers    REDEFINES Image.
        02 Layer    PIC X(25) OCCURS 6 TIMES.
       01 Remaining PIC 9(3).
       01 Idx       PIC 9(3).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.

           MOVE 150 TO Remaining.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 150
            MOVE 2 TO Pixel OF Image (Idx)
           END-PERFORM.

           PERFORM FOREVER
            READ InputFile
             AT END MOVE 1 TO EOF
            END-READ

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 150
            IF Pixel OF Image (Idx) IS EQUAL TO 2
             AND Pixel OF InputRec (Idx) IS NOT EQUAL TO 2
             MOVE Pixel OF InputRec (Idx) TO Pixel OF Image (Idx)
             SUBTRACT 1 FROM Remaining
            END-IF
           END-PERFORM

           IF Remaining IS EQUAL TO 0
            EXIT PERFORM
           END-IF

            IF EOF IS EQUAL TO 1
             EXIT PERFORM
            END-IF
           END-PERFORM.

           PERFORM VARYING Idx FROM 1 UNTIL Idx > 6
            DISPLAY Layer(Idx)
           END-PERFORM.

           CLOSE InputFile.

