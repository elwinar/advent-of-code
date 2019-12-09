       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-6a.

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

       01 OrbsLen    PIC 9(4).
       01 Orbs       OCCURS 1 TO 9999 TIMES
           DEPENDING ON OrbsLen.
          02 Orbitee PIC 9(4).
          02 Orbiter PIC 9(4).

       01 Name       PIC X(3).
       01 Obj        PIC 9(4).

       01 ListLen    PIC 9(4).
       01 List       PIC 9(4) OCCURS 1 TO 9999 TIMES 
           DEPENDING ON ListLen.
       01 Lvl        PIC 9(4).
       01 LvlEnd     PIC 9(4).
       01 Total      PIC 9(20).

       01 Idx        PIC 9(4).
       01 Jdx        PIC 9(4).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.
           READ InputFile
            AT END SET EndOfFile TO TRUE
           END-READ.

           MOVE 0 TO ObjsLen.
           MOVE 1 TO OrbsLen.

           PERFORM UNTIL EndOfFile

            MOVE Orbitee OF InputRec TO Name
            PERFORM FindObj
            IF Obj IS GREATER THAN ObjsLen
             MOVE NAME TO Objs(Obj)
             ADD 1 TO ObjsLen
            END-IF
            MOVE Obj TO Orbitee OF Orbs (OrbsLen)

            MOVE Orbiter OF InputRec TO Name
            PERFORM FindObj
            IF Obj IS GREATER THAN ObjsLen
             MOVE NAME TO Objs(Obj)
             ADD 1 TO ObjsLen
            END-IF
            MOVE Obj TO Orbiter OF Orbs (OrbsLen)

            ADD 1 TO OrbsLen

            READ InputFile
             AT END SET EndOfFile TO TRUE
            END-READ
           END-PERFORM.

           CLOSE InputFile.

           MOVE "COM" TO Name.
           PERFORM FindObj.
           MOVE Obj TO List(1).
           MOVE 1 TO ListLen.
           MOVE 1 TO LvlEnd.
           MOVE 0 TO Lvl.
           MOVE 0 TO Total.

           PERFORM VARYING Idx FROM 1 UNTIL Idx IS GREATER THAN ListLen
            IF Idx IS GREATER THAN LvlEnd
             MOVE ListLen TO LvlEnd
             ADD 1 TO Lvl
            END-IF
            PERFORM VARYING Jdx FROM 1 UNTIL Jdx IS GREATER THAN OrbsLen
             IF Orbitee OF Orbs (Jdx) IS EQUAL TO List(Idx)
              COMPUTE Total = Total + Lvl + 1
              ADD 1 TO ListLen
              MOVE Orbiter OF Orbs (Jdx) TO List(ListLen)
             END-IF
            END-PERFORM
           END-PERFORM.

           DISPLAY Total.

       FindObj SECTION.
           PERFORM VARYING Idx FROM 1 UNTIL Idx > ObjsLen
            IF Objs(Idx) IS EQUAL TO Name
             EXIT PERFORM
            END-IF
           END-PERFORM.
           MOVE Idx TO Obj.
