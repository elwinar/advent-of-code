       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-7b.

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

       01 Src           PIC S9(15) OCCURS 9999 TIMES.
       01 SrcLen        PIC 9(5).

       01 Asms             OCCURS 5 TIMES.
          02 FILLER        OCCURS 9999 TIMES.
             03 FILLER         PIC S9(15).
          02 FILLER PIC 9(5).
          02 FILLER PIC S9(15) OCCURS 99 TIMES.
          02 FILLER PIC 9(2).
          02 FILLER PIC 9(2).
          02 FILLER PIC S9(15) OCCURS 99 TIMES.
          02 FILLER PIC 9(2).

       01 Asm.
          02 FILLER        OCCURS 9999 TIMES.
             03 Op         PIC S9(15).
             03 FILLER     REDEFINES Op.
                04 OpParam PIC 9(1) OCCURS 13 TIMES.
                04 OpCode  PIC 9(2).
          02 OpPtr         PIC 9(5).
          02 Inputs        PIC S9(15) OCCURS 99 TIMES.
          02 InputsLen     PIC 9(2).
          02 InputPtr      PIC 9(2).
          02 Outputs       PIC S9(15) OCCURS 99 TIMES.
          02 OutputsLen    PIC 9(2).
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
       01 Asmx             PIC 9(5).
       01 Pipe             PIC S9(15) OCCURS 99 TIMES.
       01 PipeLen          PIC 9(2).
       01 Interrupt        PIC 9(1).
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

      * Compute the valid sequences by cycling through the potential
      * values and skipping invalid sequences.
           PERFORM VARYING Seq FROM 56789 UNTIL Seq GREATER THAN 98765
            MOVE 1 TO ValidSeq
            PERFORM VARYING Idx FROM 1 UNTIL Idx IS GREATER THAN 5
      * Ensure each number of the sequence is between 5 and 9.
             IF Setting(Idx) IS LESS THAN 5
              MOVE 0 TO ValidSeq
              EXIT PERFORM
             END-IF
      * Ensure there is no duplicate.
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
      * If the sequence is invalid, skip to the next.
            IF ValidSeq IS EQUAL TO 0
             EXIT PERFORM CYCLE
            END-IF

      * Reset the current state, and copy it to the saved states to
      * initialize the 5 routines necessary to run the software.
            PERFORM ResetAsm
            MOVE 1 TO InputsLen
            PERFORM VARYING Asmx FROM 1 UNTIL Asmx > 5
             MOVE Setting(Asmx) TO Inputs (1)
             MOVE Asm TO Asms(Asmx)
            END-PERFORM

      * Initialize the pipe with the first input.
            MOVE 0 TO Pipe(1)
            MOVE 1 TO PipeLen

      * Cycle through the chained programs.
            MOVE 0 TO Asmx
            PERFORM FOREVER
             ADD 1 TO Asmx
             IF Asmx IS EQUAL TO 6
              MOVE 1 TO Asmx
             END-IF

      * Move the saved state of the previous run of the Asmx program to
      * the current state. Add the pipe content to the input table.
             MOVE Asms(Asmx) TO Asm
             PERFORM VARYING Ptr FROM 1 UNTIL Ptr > PipeLen
              MOVE Pipe(Ptr) TO Inputs (InputsLen + Ptr)
             END-PERFORM
             ADD PipeLen TO InputsLen

      * Actually execute the program.
            PERFORM ExecuteAsm

      * Save the state of the Asmx program. Add the new outputs to the
      * pipe.
             PERFORM VARYING Ptr FROM 1 UNTIL Ptr > OutputsLen
              MOVE Outputs(Ptr) TO Pipe(Ptr)
             END-PERFORM
             MOVE OutputsLen TO PipeLen
             MOVE 0 TO OutputsLen
             MOVE Asm TO Asms(Asmx)

      * If the program stopped due to the stop instruction and this is
      * the last program of the chain, everything should be stopped and
      * we've got to the end of the loop.
             IF OpCode(OpPtr) IS EQUAL TO 99
              AND Asmx IS EQUAL TO 5
              EXIT PERFORM
             END-IF
            END-PERFORM

            IF Pipe(PipeLen) IS GREATER THAN MaxSignal
             MOVE Pipe(PipeLen) TO MaxSignal
             MOVE Seq TO BestSeq
            END-IF
           END-PERFORM.

           DISPLAY BestSeq.
           DISPLAY MaxSignal.

           STOP RUN.

      * Read the program from the input file. Save it to the Src table,
      * so any number of copies of the program can be run.
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

      * Reset the current state of the program by getting a fresh copy
      * of the instructions, and setting the pointers to 1.
       ResetAsm SECTION.
           MOVE 1 TO OpPtr.
           MOVE 1 TO InputPtr.
           MOVE 0 TO InputsLen.
           MOVE 0 TO OutputsLen.
           PERFORM VARYING Ptr FROM 1 UNTIL Ptr > SrcLen
            MOVE Src(Ptr) TO Op(Ptr)
           END-PERFORM.

      * Execute the current program by executing the operation of the
      * operation pointer. Each instruction then advance the pointer by
      * the relevant value. Handle the interruption flag requesting the
      * program to stop.
       ExecuteAsm SECTION.
           MOVE 0 TO Interrupt.
           PERFORM UNTIL OpPtr IS GREATER THAN SrcLen
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
            IF Interrupt IS EQUAL TO 1
             EXIT PERFORM
            END-IF
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
           IF InputPtr > InputsLen
            MOVE 1 TO Interrupt
            EXIT SECTION
           END-IF.
           PERFORM ReadParam.
           MOVE Inputs(InputPtr) TO Op(RZ).
           ADD 1 TO InputPtr.
           ADD 2 TO OpPtr.

       OutputInstruction SECTION.
           PERFORM ReadParam.
           ADD 1 TO OutputsLen.
           MOVE Op(RZ) TO Outputs(OutputsLen)
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
