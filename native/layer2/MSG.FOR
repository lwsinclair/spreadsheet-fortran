C     MSG.FOR - Message String Module
C
C     Purpose: Store and retrieve application messages
C
C     Features:
C       - Error messages
C       - Help text
C       - Prompt strings
C       - Message categories
C
C     Message Categories:
C       1 = Errors
C       2 = Help
C       3 = Prompts
C
C     Storage:
C       Each message stored as INTEGER array (ASCII codes)
C       Fixed maximum message length (80 characters)
C       Fixed number of messages per category (20)
C
C     Dependencies: STRUTIL.FOR
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C======================================================================

C======================================================================
C     MSGINI - Initialize message system
C======================================================================
      SUBROUTINE MSGINI
C     Initialize message storage with predefined messages

C     Error messages (category 1)
      INTEGER ERR1(80), ERR2(80), ERR3(80), ERR4(80), ERR5(80)
      INTEGER ERRLEN(20)

C     Help messages (category 2)
      INTEGER HLP1(80), HLP2(80), HLP3(80)
      INTEGER HLPLEN(20)

C     Prompt messages (category 3)
      INTEGER PRM1(80), PRM2(80), PRM3(80)
      INTEGER PRMLEN(20)

      COMMON /MSGDAT/ ERR1, ERR2, ERR3, ERR4, ERR5, ERRLEN,
     &                HLP1, HLP2, HLP3, HLPLEN,
     &                PRM1, PRM2, PRM3, PRMLEN

      INTEGER I

C     Clear all message lengths
      DO 10 I = 1, 20
        ERRLEN(I) = 0
        HLPLEN(I) = 0
        PRMLEN(I) = 0
10    CONTINUE

C     Error Messages
C     Error 1: "Invalid formula"
      DATA ERR1 /73, 110, 118, 97, 108, 105, 100, 32,
     &           102, 111, 114, 109, 117, 108, 97, 65*32/
      ERRLEN(1) = 15

C     Error 2: "Circular reference"
      DATA ERR2 /67, 105, 114, 99, 117, 108, 97, 114, 32,
     &           114, 101, 102, 101, 114, 101, 110, 99, 101, 62*32/
      ERRLEN(2) = 18

C     Error 3: "Division by zero"
      DATA ERR3 /68, 105, 118, 105, 115, 105, 111, 110, 32,
     &           98, 121, 32, 122, 101, 114, 111, 64*32/
      ERRLEN(3) = 16

C     Error 4: "Out of memory"
      DATA ERR4 /79, 117, 116, 32, 111, 102, 32,
     &           109, 101, 109, 111, 114, 121, 67*32/
      ERRLEN(4) = 13

C     Error 5: "Cell not found"
      DATA ERR5 /67, 101, 108, 108, 32, 110, 111, 116, 32,
     &           102, 111, 117, 110, 100, 66*32/
      ERRLEN(5) = 14

C     Help Messages
C     Help 1: "XL Spreadsheet v1.0"
      DATA HLP1 /88, 76, 32, 83, 112, 114, 101, 97, 100, 115,
     &           104, 101, 101, 116, 32, 118, 49, 46, 48, 61*32/
      HLPLEN(1) = 19

C     Help 2: "Commands: /B /E /C /M /F /W /R /H /Q"
      DATA HLP2 /67, 111, 109, 109, 97, 110, 100, 115, 58, 32,
     &           47, 66, 32, 47, 69, 32, 47, 67, 32, 47, 77, 32,
     &           47, 70, 32, 47, 87, 32, 47, 82, 32, 47, 72, 32,
     &           47, 81, 44*32/
      HLPLEN(2) = 36

C     Help 3: "Press ? for help"
      DATA HLP3 /80, 114, 101, 115, 115, 32, 63, 32,
     &           102, 111, 114, 32, 104, 101, 108, 112, 64*32/
      HLPLEN(3) = 16

C     Prompt Messages
C     Prompt 1: "Enter formula:"
      DATA PRM1 /69, 110, 116, 101, 114, 32, 102, 111, 114, 109,
     &           117, 108, 97, 58, 66*32/
      PRMLEN(1) = 14

C     Prompt 2: "Command:"
      DATA PRM2 /67, 111, 109, 109, 97, 110, 100, 58, 72*32/
      PRMLEN(2) = 8

C     Prompt 3: "Cell:"
      DATA PRM3 /67, 101, 108, 108, 58, 75*32/
      PRMLEN(3) = 5

      RETURN
      END

C======================================================================
C     MSGGET - Get message by category and number
C======================================================================
      SUBROUTINE MSGGET(CAT, NUM, TEXT, TLEN)
      INTEGER CAT, NUM
      INTEGER TEXT(80)
      INTEGER TLEN

C     Message storage
      INTEGER ERR1(80), ERR2(80), ERR3(80), ERR4(80), ERR5(80)
      INTEGER ERRLEN(20)
      INTEGER HLP1(80), HLP2(80), HLP3(80)
      INTEGER HLPLEN(20)
      INTEGER PRM1(80), PRM2(80), PRM3(80)
      INTEGER PRMLEN(20)

      COMMON /MSGDAT/ ERR1, ERR2, ERR3, ERR4, ERR5, ERRLEN,
     &                HLP1, HLP2, HLP3, HLPLEN,
     &                PRM1, PRM2, PRM3, PRMLEN

      INTEGER I

C     Error messages (category 1)
      IF (CAT .NE. 1) GO TO 100

      IF (NUM .EQ. 1) GO TO 10
      IF (NUM .EQ. 2) GO TO 20
      IF (NUM .EQ. 3) GO TO 30
      IF (NUM .EQ. 4) GO TO 40
      IF (NUM .EQ. 5) GO TO 50
      GO TO 900

10    TLEN = ERRLEN(1)
      DO 11 I = 1, TLEN
        TEXT(I) = ERR1(I)
11    CONTINUE
      RETURN

20    TLEN = ERRLEN(2)
      DO 21 I = 1, TLEN
        TEXT(I) = ERR2(I)
21    CONTINUE
      RETURN

30    TLEN = ERRLEN(3)
      DO 31 I = 1, TLEN
        TEXT(I) = ERR3(I)
31    CONTINUE
      RETURN

40    TLEN = ERRLEN(4)
      DO 41 I = 1, TLEN
        TEXT(I) = ERR4(I)
41    CONTINUE
      RETURN

50    TLEN = ERRLEN(5)
      DO 51 I = 1, TLEN
        TEXT(I) = ERR5(I)
51    CONTINUE
      RETURN

C     Help messages (category 2)
100   IF (CAT .NE. 2) GO TO 200

      IF (NUM .EQ. 1) GO TO 110
      IF (NUM .EQ. 2) GO TO 120
      IF (NUM .EQ. 3) GO TO 130
      GO TO 900

110   TLEN = HLPLEN(1)
      DO 111 I = 1, TLEN
        TEXT(I) = HLP1(I)
111   CONTINUE
      RETURN

120   TLEN = HLPLEN(2)
      DO 121 I = 1, TLEN
        TEXT(I) = HLP2(I)
121   CONTINUE
      RETURN

130   TLEN = HLPLEN(3)
      DO 131 I = 1, TLEN
        TEXT(I) = HLP3(I)
131   CONTINUE
      RETURN

C     Prompt messages (category 3)
200   IF (CAT .NE. 3) GO TO 900

      IF (NUM .EQ. 1) GO TO 210
      IF (NUM .EQ. 2) GO TO 220
      IF (NUM .EQ. 3) GO TO 230
      GO TO 900

210   TLEN = PRMLEN(1)
      DO 211 I = 1, TLEN
        TEXT(I) = PRM1(I)
211   CONTINUE
      RETURN

220   TLEN = PRMLEN(2)
      DO 221 I = 1, TLEN
        TEXT(I) = PRM2(I)
221   CONTINUE
      RETURN

230   TLEN = PRMLEN(3)
      DO 231 I = 1, TLEN
        TEXT(I) = PRM3(I)
231   CONTINUE
      RETURN

C     Message not found
900   TLEN = 0
      RETURN
      END
