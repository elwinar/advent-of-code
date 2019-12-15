       IDENTIFICATION DIVISION.
       PROGRAM-ID. AdventOfCode2019-13a.

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
       01 Str       PIC X(20).
       01 Ptr       PIC 9(5).

       01 Src           PIC S9(20) VALUE IS 0 OCCURS 9999 TIMES.
       01 SrcLen        PIC 9(5).
       01 OpSize        PIC 9(5) VALUE IS 20.

       01 Asm.
          02 FILLER        OCCURS 9999 TIMES.
             03 Op         PIC S9(20).
             03 FILLER     REDEFINES Op.
                04 OpParam PIC 9(1) OCCURS 18 TIMES.
                04 OpCode  PIC 9(2).
          02 OpPtr         PIC 9(20).
          02 Inputs        PIC S9(20) OCCURS 9999 TIMES.
          02 InputsLen     PIC 9(4).
          02 InputPtr      PIC 9(4).
          02 Outputs       PIC S9(20) OCCURS 9999 TIMES.
          02 OutputsLen    PIC 9(4).
          02 Interrupt     PIC 9(1).
          02 RelativeBase  PIC S9(20).
          02 RA            PIC S9(20).
          02 RB            PIC S9(20).
          02 RX            PIC S9(20).
          02 RY            PIC S9(20).
          02 RZ            PIC S9(20).

       01 Tiles.
        02 FILLER OCCURS 99 TIMES.
         03 Tile PIC 9(1) OCCURS 99 TIMES.
       01 PosX PIC S9(3).
       01 PosY PIC S9(3).
       01 Joystick PIC S9(1).
       01 JoystickMove PIC X(1).
       01 Score PIC 9(20).

       PROCEDURE DIVISION.
       Begin.
           ACCEPT InputPath FROM ARGUMENT-VALUE.
           OPEN INPUT InputFile.
           READ InputFile.
           CLOSE InputFile.

           PERFORM ReadAsm.
           PERFORM ResetAsm.
           MOVE 2 TO Op(1).

           PERFORM FOREVER
            PERFORM ExecuteAsm
            EVALUATE Interrupt
             WHEN 0
              EXIT PERFORM
             WHEN 3
              PERFORM DisplayScreen
              MOVE 1 TO InputsLen
              MOVE 1 TO InputPtr
              DISPLAY " "
              DISPLAY "Input " WITH NO ADVANCING
              ACCEPT JoystickMove
              EVALUATE JoystickMove
               WHEN "q"
                MOVE -1 TO Joystick
               WHEN "s"
                MOVE 0 TO Joystick
               WHEN "d"
                MOVE 1 TO Joystick
              END-EVALUATE
              MOVE Joystick TO Inputs(InputsLen)
              EXIT PERFORM CYCLE
             WHEN 4
              PERFORM ExecuteAsm
              PERFORM ExecuteAsm
              MOVE Outputs(1) TO PosX
              MOVE Outputs(2) TO PosY
              IF PosX IS EQUAL TO -1
               MOVE Outputs(3) TO Score
              ELSE
               MOVE Outputs(3) TO Tile(PosX + 1, PosY + 1)
              END-IF
              MOVE 0 TO OutputsLen
            END-EVALUATE
           END-PERFORM.
           PERFORM DisplayScreen

           STOP RUN.

       DisplayScreen SECTION.
           CALL 'SYSTEM' using 'clear'.
           PERFORM VARYING PosY FROM 1 UNTIL PosY > 24
                     AFTER PosX FROM 1 UNTIL PosX > 42
            IF PosX IS EQUAL TO 1
             DISPLAY " "
            END-IF
            EVALUATE Tile(PosX, PosY)
             WHEN 0 DISPLAY "  " WITH NO ADVANCING
             WHEN 1 DISPLAY "++" WITH NO ADVANCING
             WHEN 2 DISPLAY "[]" WITH NO ADVANCING
             WHEN 3 DISPLAY "--" WITH NO ADVANCING
             WHEN 4 DISPLAY "o " WITH NO ADVANCING
            END-EVALUATE
           END-PERFORM.
           DISPLAY " ".
           DISPLAY " ".
           DISPLAY "Score=" Score.

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
            COMPUTE RY = OpSize - 2
            EVALUATE OpCode(OpPtr)
             WHEN 1  PERFORM AddInstruction
             WHEN 2  PERFORM MultiplyInstruction
             WHEN 3  PERFORM InputInstruction
             WHEN 4  PERFORM OutputInstruction
             WHEN 5  PERFORM JumpIfTrueInstruction
             WHEN 6  PERFORM JumpIfFalseInstruction
             WHEN 7  PERFORM LessThanInstruction
             WHEN 8  PERFORM EqualsInstruction
             WHEN 9  PERFORM AdjustRelativeBaseInstruction
             WHEN 99 EXIT PERFORM
             WHEN OTHER
              DISPLAY "Invalid Op"
              STOP RUN
            END-EVALUATE
            IF Interrupt IS NOT EQUAL TO 0
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
            MOVE OpCode(OpPtr) TO Interrupt
            EXIT SECTION
           END-IF.
           PERFORM ReadParam.
           MOVE Inputs(InputPtr) TO Op(RZ).
           ADD 1 TO InputPtr.
           ADD 2 TO OpPtr.

       OutputInstruction SECTION.
           PERFORM ReadParam.
           ADD 1 TO OutputsLen.
           MOVE Op(RZ) TO Outputs(OutputsLen).
           MOVE OpCode(OpPtr) TO Interrupt.
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

       AdjustRelativeBaseInstruction SECTION.
           PERFORM ReadParam.
           ADD Op(RZ) TO RelativeBase.
           ADD 2 TO OpPtr.

       ReadParam SECTION.
           EVALUATE OpParam(OpPtr, RY)
      * Position mode is a pointer.
            WHEN 0
             COMPUTE RZ = Op(RX) + 1
      * Immediate mode is a value.
            WHEN 1 
             MOVE RX TO RZ
      * Relative mode is a relative pointer.
            WHEN 2
             COMPUTE RZ = Op(RX) + RelativeBase + 1
           END-EVALUATE

           ADD 1 TO RX.
           SUBTRACT 1 FROM RY.
