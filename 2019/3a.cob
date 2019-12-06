       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-3a.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN USING InputPath
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile RECORD IS VARYING IN SIZE FROM 1 TO 9999 CHARACTERS.
       01 InputRec  PIC X(9999).

       WORKING-STORAGE SECTION.
       01 InputPath           PIC X(50).
       01 Chunk.
          02 Dir              PIC X(1).
          02 RawDist          PIC X(10).
       01 Ptr                 PIC 9(5).
       01 Seps                PIC 9(5).
       01 Dist                PIC 9(10).
       01 Abs                 PIC S9(10).
       01 Ord                 PIC S9(10).
       01 Len                 PIC 9(5).
       01 FILLER              OCCURS 0 TO 9999 DEPENDING ON Len.
          02 PosAbs           PIC S9(10).
          02 PosOrd           PIC S9(10).
       01 PrevAbs             PIC S9(10).
       01 PrevOrd             PIC S9(10).
       01 Idx                 PIC 9(5).
       01 PrevIdx             PIC 9(5).
       01 CrossAbs            PIC S9(10).
       01 CrossOrd            PIC S9(10).
       01 Manhattan           PIC S9(10).
       01 BestManhattan       PIC S9(10).
       01 BestManhattanFmt    PIC Z(10).



       PROCEDURE DIVISION.
       Begin SECTION.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.

      * Read the first line.
           READ InputFile.
           PERFORM CountSeps.

           MOVE 0 TO Abs.
           MOVE 0 TO Ord.
           MOVE 1 TO Ptr.
           MOVE 0 TO PosAbs(1).
           MOVE 0 TO PosOrd(1).
           MOVE 1 TO Len.
           PERFORM Seps TIMES
            PERFORM ReadRec
            ADD 1 TO Len
            MOVE Abs TO PosAbs(Len)
            MOVE Ord TO PosOrd(Len)
           END-PERFORM.

      * Read the second line.
           READ InputFile.
           PERFORM CountSeps.

           MOVE 0 TO Abs.
           MOVE 0 TO Ord.
           MOVE 0 TO PrevAbs.
           MOVE 0 TO PrevOrd.
           MOVE 1 TO Ptr.
           MOVE HIGH-VALUE TO BestManhattan.

           PERFORM Seps TIMES
            MOVE Abs TO PrevAbs
            MOVE Ord TO PrevOrd
            PERFORM ReadRec

            MOVE 1 TO PrevIdx
            PERFORM VARYING Idx FROM 2 UNTIL Idx IS GREATER THAN Len
             IF PrevAbs = Abs AND PosOrd(PrevIdx) = PosOrd(Idx)
              IF (
               (PosAbs(PrevIdx) <= Abs AND Abs <= PosAbs(Idx))
               OR (PosAbs(Idx) <= Abs AND Abs <= PosAbs(PrevIdx))
              ) AND (
               (PrevOrd <= PosOrd(Idx) AND PosOrd(Idx) <= Ord)
               OR (Ord <= PosOrd(Idx) AND PosOrd(Idx) <= PrevOrd)
              )
               MOVE Abs TO CrossAbs
               MOVE PosOrd(Idx) TO CrossOrd
               PERFORM EvaluateManhattan
              END-IF
             ELSE IF PrevOrd = Ord AND PosAbs(PrevIdx) = PosAbs(Idx)
              IF (
               (PosOrd(PrevIdx) <= Ord AND Ord <= PosOrd(Idx))
               OR (PosOrd(Idx) <= Ord AND Ord <= PosOrd(PrevIdx))
              ) AND (
               (PrevAbs <= PosAbs(Idx) AND PosAbs(Idx) <= Abs)
               OR (Abs <= PosAbs(Idx) AND PosAbs(Idx) <= PrevAbs)
              )
               MOVE PosAbs(Idx) TO CrossAbs
               MOVE Ord TO CrossOrd
               PERFORM EvaluateManhattan
              END-IF
             END-IF
             MOVE Idx TO PrevIdx
            END-PERFORM
           END-PERFORM.
           MOVE BestManhattan TO BestManhattanFmt.
           DISPLAY BestManhattanFmt.

           CLOSE InputFile.
           STOP RUN.

      * Subroutine CountSeps.
      *
      * Count the number of sections in the input record.
      *
      * Uses: InputRec, Seps
       CountSeps SECTION.
           MOVE 0 TO Seps.
           INSPECT InputRec TALLYING Seps FOR ALL ",".
           IF Seps < 9999
            ADD 1 TO Seps
           ELSE
            MOVE 9999 TO Seps
           END-IF.

      * Subroutine ReadRec.
      *
      * Read a section of the input record, parsing the direction and
      * distance and resulting into the Abs and Ord values being
      * updated.
      *
      * Uses: InputRec, Chunk, Ptr, Dist, Dir, Ord, Abs
       ReadRec SECTION.
           UNSTRING InputRec DELIMITED BY "," INTO Chunk
            WITH POINTER Ptr
           END-UNSTRING.
           COMPUTE Dist = FUNCTION NUMVAL(RawDist).
           EVALUATE Dir
            WHEN "U"
             COMPUTE Ord = Ord + Dist
            WHEN "D"
             COMPUTE Ord = Ord - Dist
            WHEN "R"
             COMPUTE Abs = Abs + Dist
            WHEN "L"
             COMPUTE Abs = Abs - Dist
           END-EVALUATE.

      * Subroutine EvaluateManhattan.
      *
      * Calculate the Manhattan distance for the current Cross values,
      * and keep the minimal one in the result variable.
      *
      * Uses: CrossAbs, CrossOrd, Manhattan, BestManhattan
       EvaluateManhattan SECTION.
           MOVE FUNCTION ABS(CrossAbs) TO Manhattan.
           ADD FUNCTION ABS(CrossOrd) TO Manhattan.
           IF Manhattan NOT EQUAL TO 0
           AND Manhattan IS LESS THAN BestManhattan
            MOVE Manhattan TO BestManhattan
           END-IF.

