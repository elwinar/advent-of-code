       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-5b.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT InputFile ASSIGN TO InputPath
       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD InputFile RECORD IS VARYING IN SIZE FROM 1 TO 9999 CHARACTERS.
       01 InputRec  PIC X(9999).

       WORKING-STORAGE SECTION.
       01 InputPath PIC X(50).
       01 Seps      PIC 9(5).
       01 Str       PIC X(15).
       01 Ptr       PIC 9(5).

       01 Asm.
          02 Src           PIC S9(15) OCCURS 9999 TIMES.
          02 SrcLen        PIC 9(5).
          02 FILLER        OCCURS 9999 TIMES.
             03 Op         PIC S9(15).
             03 FILLER     REDEFINES Op.
                04 OpParam PIC 9(1) OCCURS 13 TIMES.
                04 OpCode  PIC 9(2).
          02 OpPtr         PIC 9(5).
          02 Inputs        PIC S9(15) OCCURS 99 TIMES.
          02 InputPtr      PIC 9(2).
          02 Outputs       PIC S9(15) OCCURS 99 TIMES.
          02 OutputPtr     PIC 9(2).
          02 RA            PIC S9(15).
          02 RB            PIC S9(15).
          02 RX            PIC S9(15).
          02 RY            PIC S9(15).
          02 RZ            PIC S9(15).

       01 Seq              PIC 9(5).
       01 FILLER           REDEFINES Seq.
          02 Setting       PIC 9(1) OCCURS 5 TIMES.
       01 ValidSeq         PIC 9(1).
       01 Idx              PIC 9(5).
       01 Jdx              PIC 9(5).

       01 MaxSignal        PIC 9(15).
       01 BestSeq          PIC 9(5).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.
           READ InputFile.
           CLOSE InputFile.

           PERFORM ReadAsm.

           MOVE 0 TO MaxSignal.

           PERFORM VARYING Seq FROM 01234 UNTIL Seq GREATER THAN 43210
            MOVE 1 TO ValidSeq
            PERFORM VARYING Idx FROM 1 UNTIL Idx IS GREATER THAN 5
             IF Setting(Idx) IS GREATER THAN 4
              MOVE 0 TO ValidSeq
              EXIT PERFORM
             END-IF
             PERFORM VARYING Jdx FROM Idx UNTIL Jdx IS GREATER THAN 5
              IF Idx IS NOT EQUAL TO Jdx 
               AND Setting(Idx) IS EQUAL TO Setting(Jdx)
               MOVE 0 TO ValidSeq
               EXIT PERFORM
              END-IF
             END-PERFORM
             IF ValidSeq IS EQUAL TO 0
              EXIT PERFORM
             END-IF
            END-PERFORM
            IF ValidSeq IS EQUAL TO 0
             EXIT PERFORM CYCLE
            END-IF

            MOVE 0 TO Outputs(1)
            PERFORM VARYING Idx FROM 1 UNTIL Idx IS GREATER THAN 5
             MOVE Setting(Idx) TO Inputs(1)
             MOVE Outputs(1) TO Inputs(2)
             PERFORM ExecuteAsm
            END-PERFORM
            IF Outputs(1) IS GREATER THAN MaxSignal
             MOVE Outputs(1) TO MaxSignal
             MOVE Seq TO BestSeq
            END-IF
           END-PERFORM.

           DISPLAY BestSeq.
           DISPLAY MaxSignal.

           STOP RUN.

       ReadAsm SECTION.
           INSPECT InputRec TALLYING Seps FOR ALL ",".
           IF Seps < 9999
            ADD 1 TO Seps
           ELSE
            MOVE 9999 TO Seps
           END-IF.

           MOVE 1 TO Ptr.
           MOVE 0 TO SrcLen.
           PERFORM Seps TIMES
            UNSTRING InputRec DELIMITED BY "," INTO Str
             WITH POINTER Ptr
            END-UNSTRING
            ADD 1 TO SrcLen
            COMPUTE Src(SrcLen) = FUNCTION NUMVAL(Str)
           END-PERFORM.

       ExecuteAsm SECTION.
           MOVE 1 TO OpPtr.
           MOVE 1 TO InputPtr.
           MOVE 1 TO OutputPtr.

           PERFORM VARYING Ptr FROM 1 UNTIL Ptr IS GREATER THAN SrcLen
            MOVE Src(Ptr) TO Op(Ptr)
           END-PERFORM.

           PERFORM UNTIL OpPtr IS GREATER THAN SrcLen
      *     DISPLAY "Instruction " OpPtr " " OpCode(OpPtr)
            COMPUTE RX = OpPtr + 1
            MOVE 13 TO RY
            EVALUATE OpCode(OpPtr)
             WHEN 1  PERFORM AddInstruction
             WHEN 2  PERFORM MultiplyInstruction
             WHEN 3  PERFORM InputInstruction
             WHEN 4  PERFORM OutputInstruction
             WHEN 5  PERFORM JumpIfTrueInstruction
             WHEN 6  PERFORM JumpIfFalseInstruction
             WHEN 7  PERFORM LessThanInstruction
             WHEN 8  PERFORM EqualsInstruction
             WHEN 99 EXIT PERFORM
             WHEN OTHER DISPLAY "ERROR" STOP RUN
            END-EVALUATE
           END-PERFORM.

       AddInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RB.
           PERFORM ReadParam.
           ADD Op(RZ) TO RB.
           PERFORM ReadParam.
           MOVE RB TO Op(RZ).
           ADD 4 TO OpPtr.

       MultiplyInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RB.
           PERFORM ReadParam.
           MULTIPLY Op(RZ) BY RB.
           PERFORM ReadParam.
           MOVE RB TO Op(RZ).
           ADD 4 TO OpPtr.

       InputInstruction SECTION.
           PERFORM ReadParam.
           MOVE Inputs(InputPtr) TO Op(RZ).
           ADD 1 TO InputPtr.
           ADD 2 TO OpPtr.

       OutputInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO Outputs(OutputPtr)
           ADD 1 TO OutputPtr.
           ADD 2 TO OpPtr.

       JumpIfTrueInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RB.
           PERFORM ReadParam.
           IF RB IS NOT EQUAL TO 0
            COMPUTE OpPtr = Op(RZ) + 1
           ELSE
            ADD 3 TO OpPtr
           END-IF.

       JumpIfFalseInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RB.
           PERFORM ReadParam.
           IF RB IS EQUAL TO 0
            COMPUTE OpPtr = Op(RZ) + 1
           ELSE
            ADD 3 TO OpPtr
           END-IF.

       LessThanInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RA.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RB.
           PERFORM ReadParam.
           IF RA IS LESS THAN RB
            MOVE 1 TO Op(RZ)
           ELSE
            MOVE 0 TO Op(RZ)
           END-IF.
           ADD 4 TO OpPtr.

       EqualsInstruction SECTION.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RA.
           PERFORM ReadParam.
           MOVE Op(RZ) TO RB.
           PERFORM ReadParam.
           IF RA IS EQUAL TO RB
            MOVE 1 TO Op(RZ)
           ELSE
            MOVE 0 TO Op(RZ)
           END-IF.
           ADD 4 TO OpPtr.

       ReadParam SECTION.
           IF OpParam(OpPtr, RY) IS EQUAL TO 0
            COMPUTE RZ = Op(RX) + 1
           ELSE 
            MOVE RX TO RZ
           END-IF.
           ADD 1 TO RX.
           SUBTRACT 1 FROM RY.
