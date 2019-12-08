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

       01 Opcode.
          02 FILLER       OCCURS 9999 TIMES.
             03 Cell      PIC S9(15).
             03 FILLER    REDEFINES Cell.
                04 Params PIC 9(1) OCCURS 13 TIMES.
                04 Op     PIC 9(2).
          02 OpPtr        PIC 9(5).
          02 OpLen        PIC 9(5).
          02 Inputs       PIC S9(15) OCCURS 99 TIMES.
          02 InputPtr     PIC 9(2).
          02 Outputs      PIC S9(15) OCCURS 99 TIMES.
          02 OutputPtr    PIC 9(2).
          02 RA           PIC S9(15).
          02 RB           PIC S9(15).
          02 RX           PIC S9(15).
          02 RY           PIC S9(15).
          02 RZ           PIC S9(15).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.
           READ InputFile.
           CLOSE InputFile.

           INSPECT InputRec TALLYING Seps FOR ALL ",".
           IF Seps < 9999
            ADD 1 TO Seps
           ELSE
            MOVE 9999 TO Seps
           END-IF.

           MOVE 1 TO Ptr.
           MOVE 0 TO OpLen.
           PERFORM Seps TIMES
            UNSTRING InputRec DELIMITED BY "," INTO Str
             WITH POINTER Ptr
            END-UNSTRING
            ADD 1 TO OpLen
            COMPUTE Cell(OpLen) = FUNCTION NUMVAL(Str)
           END-PERFORM.

           MOVE 1 TO OpPtr.
           MOVE 1 TO InputPtr.
           MOVE 1 TO OutputPtr.

           MOVE 5 TO Inputs(1).

           DISPLAY "Execution".
           PERFORM UNTIL OpPtr IS GREATER THAN OpLen
            DISPLAY " "
            DISPLAY "Instruction " OpPtr " " Op(OpPtr) " " Cell(OpPtr)
            COMPUTE RX = OpPtr + 1
            MOVE 13 TO RY
            EVALUATE Op(OpPtr)
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

           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "Output".

           PERFORM VARYING Ptr FROM 1 
            UNTIL Ptr IS GREATER OR EQUAL TO OutputPtr
            DISPLAY Ptr " " Outputs(Ptr)
           END-PERFORM.
           STOP RUN.

       AddInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RB.
           PERFORM ReadParam.
           ADD Cell(RZ) TO RB.
           PERFORM ReadParam.
           MOVE RB TO Cell(RZ).
           ADD 4 TO OpPtr.

       MultiplyInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RB.
           PERFORM ReadParam.
           MULTIPLY Cell(RZ) BY RB.
           PERFORM ReadParam.
           MOVE RB TO Cell(RZ).
           ADD 4 TO OpPtr.

       InputInstruction SECTION.
           PERFORM ReadParam.
           MOVE Inputs(InputPtr) TO Cell(RZ).
           ADD 1 TO InputPtr.
           ADD 2 TO OpPtr.

       OutputInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO Outputs(OutputPtr)
           ADD 1 TO OutputPtr.
           ADD 2 TO OpPtr.

       JumpIfTrueInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RB.
           PERFORM ReadParam.
           IF RB IS NOT EQUAL TO 0
            COMPUTE OpPtr = Cell(RZ) + 1
           ELSE
            ADD 3 TO OpPtr
           END-IF.

       JumpIfFalseInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RB.
           PERFORM ReadParam.
           IF RB IS EQUAL TO 0
            COMPUTE OpPtr = Cell(RZ) + 1
           ELSE
            ADD 3 TO OpPtr
           END-IF.

       LessThanInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RA.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RB.
           PERFORM ReadParam.
           IF RA IS LESS THAN RB
            MOVE 1 TO Cell(RZ)
           ELSE
            MOVE 0 TO Cell(RZ)
           END-IF.
           ADD 4 TO OpPtr.

       EqualsInstruction SECTION.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RA.
           PERFORM ReadParam.
           MOVE Cell(RZ) TO RB.
           PERFORM ReadParam.
           IF RA IS EQUAL TO RB
            MOVE 1 TO Cell(RZ)
           ELSE
            MOVE 0 TO Cell(RZ)
           END-IF.
           ADD 4 TO OpPtr.

       ReadParam SECTION.
           DISPLAY "Reading param " RY ": " Params(OpPtr, RY)
           IF Params(OpPtr, RY) IS EQUAL TO 0
            COMPUTE RZ = Cell(RX) + 1
           ELSE 
            MOVE RX TO RZ
           END-IF.
           ADD 1 TO RX.
           SUBTRACT 1 FROM RY.
