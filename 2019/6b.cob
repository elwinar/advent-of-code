       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-6b.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO InputPath
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile   RECORD CONTAINS 7 CHARACTERS.
       01 InputRec.
          02 Orbitee  PIC X(3).
          02 FILLER   PIC X(1).
          02 Orbiter  PIC X(3).
          02 FILLER   PIC X(10).
       88 EndOfFile   VALUE HIGH-VALUES.

       WORKING-STORAGE SECTION.
       01 InputPath  PIC X(50).

       01 ObjsLen    PIC 9(4).
       01 Objs       PIC X(3) OCCURS 1 TO 9999 TIMES
           DEPENDING ON ObjsLen.
       01 Orbs       PIC 9(4) OCCURS 1 TO 9999 TIMES
           DEPENDING ON ObjsLen.

       01 Name       PIC X(3).
       01 Obj        PIC 9(4).

       01 YouLen     PIC 9(4).
       01 YouPath    PIC 9(4) OCCURS 1 TO 9999 TIMES
           DEPENDING ON YouLen.

       01 SanLen     PIC 9(4).
       01 SanPath    PIC 9(4) OCCURS 1 TO 9999 TIMES
           DEPENDING ON SanLen.

       01 Idx        PIC 9(4).
       01 Jdx        PIC 9(4).
       01 Total      PIC 9(4).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.
           READ InputFile
            AT END SET EndOfFile TO TRUE
           END-READ.

           MOVE 0 TO ObjsLen.

           PERFORM UNTIL EndOfFile

            MOVE Orbitee OF InputRec TO Name
            PERFORM FindObj
            IF Obj GREATER THAN ObjsLen
             MOVE NAME TO Objs(Obj)
             ADD 1 TO ObjsLen
            END-IF
            MOVE Obj TO Jdx

            MOVE Orbiter OF InputRec TO Name
            PERFORM FindObj
            IF Obj GREATER THAN ObjsLen
             MOVE NAME TO Objs(Obj)
             ADD 1 TO ObjsLen
            END-IF
            MOVE Jdx TO Orbs(Obj)

            READ InputFile
             AT END SET EndOfFile TO TRUE
            END-READ
           END-PERFORM.

           CLOSE InputFile.

           MOVE "YOU" TO Name.
           PERFORM FindObj.
           MOVE 1 TO YouLen.
           MOVE Obj TO YouPath(1).

           PERFORM UNTIL Objs(Obj) EQUAL TO "COM"
            MOVE Orbs(Obj) TO Obj
            ADD 1 TO YouLen
            MOVE Obj TO YouPath(YouLen)
           END-PERFORM.

           MOVE "SAN" TO Name.
           PERFORM FindObj.
           MOVE 1 TO SanLen.
           MOVE Obj TO SanPath(1).

           PERFORM UNTIL Objs(Obj) EQUAL TO "COM"
            MOVE Orbs(Obj) TO Obj
            ADD 1 TO SanLen
            MOVE Obj TO SanPath(SanLen)
           END-PERFORM.

           PERFORM VARYING Idx FROM 1 UNTIL Idx GREATER THAN YouLen
            IF YouPath(YouLen - Idx) NOT EQUAL TO SanPath(SanLen - Idx)
             EXIT PERFORM
            END-IF
           END-PERFORM.

           COMPUTE Total = SanLen - Idx + YouLen - Idx - 2.
           DISPLAY Total.


       FindObj SECTION.
           PERFORM VARYING Idx FROM 1 UNTIL Idx > ObjsLen
            IF Objs(Idx) EQUAL TO Name
             EXIT PERFORM
            END-IF
           END-PERFORM.
           MOVE Idx TO Obj.
