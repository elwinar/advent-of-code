       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-8a.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO InputPath
       ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile.
       01 InputRec.
        02 Layer PIC X(150).

       WORKING-STORAGE SECTION.
       01 InputPath PIC X(50).
       01 EOF       PIC 9(1).
       01 NumZero   PIC 9(3).
       01 NumOne    PIC 9(3).
       01 NumTwo    PIC 9(3).
       01 Best      PIC 9(3).
       01 Total     PIC 9(10).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.

           MOVE HIGH-VALUE TO Best.

           PERFORM FOREVER

            READ InputFile
             AT END MOVE 1 TO EOF
            END-READ

            MOVE 0 TO NumZero
            MOVE 0 TO NumOne
            MOVE 0 TO NumTwo
            INSPECT Layer TALLYING NumZero FOR ALL "0"
            IF NumZero IS LESS THAN Best
             INSPECT Layer TALLYING NumOne FOR ALL "1"
             INSPECT Layer TALLYING NumTwo FOR ALL "2"
             COMPUTE Total = NumOne * NumTwo
             MOVE NumZero TO Best
             DISPLAY Best " " Total
            END-IF

            IF EOF IS EQUAL TO 1
             EXIT PERFORM
            END-IF

           END-PERFORM.

           CLOSE InputFile.

           DISPLAY Best " " Total.

