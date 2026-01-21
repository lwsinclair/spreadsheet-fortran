C     XLMAIN.FOR - XL Spreadsheet Main Program
C
C     Purpose: Interactive spreadsheet for CP-V with VT-52 terminal
C
C     Features:
C       - Full-screen grid display
C       - Cell navigation with arrow keys
C       - Formula entry and evaluation
C       - Automatic recalculation
C       - Slash commands (future)
C
C     Target: Xerox Sigma 7 CP-V with VT-52 CRT terminal
C
C     Usage:
C       $ RUN XL
C
C     Exit:
C       Type /QUIT and press RETURN
C
C     Author: Claude Code
C     Date: 2026-01-19
C
C     FORTRAN IV Notes:
C       - Fixed-format source (columns 1-72)
C       - Arithmetic IF only
C       - GO TO for control flow
C       - No PARAMETER in main program (F66 limitation)
C
C======================================================================

      PROGRAM XL
C     XL Spreadsheet - Interactive calculation system

C     System state
      INTEGER MODE, COL, ROW
      INTEGER KEY, VALID
      INTEGER RUNNING

C     Welcome message
      INTEGER WELMSG(40)
      INTEGER WELLEN

C     Initialize all subsystems
      CALL CELINI
      CALL DEPINI
      CALL UIINI
      CALL DSPINI

C     Display welcome
      DATA WELMSG /88,76,32,83,112,114,101,97,100,115,
     &             104,101,101,116,32,118,49,46,48,32,
     &             45,32,67,80,45,86,47,86,84,45,53,50,
     &             32,32,32,32,32,32,32,32/
      WELLEN = 32

C     Show welcome at bottom of screen
      CALL DSPMSG(WELMSG, WELLEN)

C     Wait 2 seconds (poor man's delay - loop)
C     Note: This is platform-dependent, adjust for CP-V
      DO 5 KEY = 1, 100000
5     CONTINUE

C     Draw initial screen
      CALL DSPFUL

C     Main event loop
      RUNNING = 1

10    IF (RUNNING .EQ. 0) GO TO 999

C       Read keystroke
        CALL TMKEY(KEY, VALID)

C       Check if key available
        IF (VALID .EQ. 0) GO TO 10

C       Get current state
        CALL UIGET(MODE, COL, ROW)

C       Handle keystroke based on mode
        IF (MODE .EQ. 1) THEN
C         NAV mode
          CALL NAVKEY(KEY, RUNNING)

        ELSE IF (MODE .EQ. 2) THEN
C         ENTRY mode
          CALL ENTKEY(KEY)

        ELSE IF (MODE .EQ. 4) THEN
C         COMMAND mode
          CALL CMDKEY(KEY, RUNNING)

        END IF

C       Scroll viewport if needed
        CALL UIGET(MODE, COL, ROW)
        CALL DSPSCR(COL, ROW)

C       Refresh display
        CALL DSPFUL

C       Continue loop
        GO TO 10

999   CONTINUE

C     Clean shutdown
      CALL TMCLR
      CALL TMHOME
      CALL TMREST

      STOP
      END

C======================================================================
C     NAVKEY - Handle keystroke in NAV mode
C======================================================================
      SUBROUTINE NAVKEY(KEY, RUNNING)
      INTEGER KEY
      INTEGER RUNNING
C     Process keystroke in navigation mode
C
C     Arrow keys: 128=Up, 129=Down, 130=Right, 131=Left
C     /: Enter COMMAND mode
C     Printable char: Enter ENTRY mode

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Check for arrow keys
      IF (KEY .EQ. 128) THEN
C       Up arrow
        IF (UIROW .GT. 1) THEN
          UIROW = UIROW - 1
        END IF

      ELSE IF (KEY .EQ. 129) THEN
C       Down arrow
        IF (UIROW .LT. 255) THEN
          UIROW = UIROW + 1
        END IF

      ELSE IF (KEY .EQ. 130) THEN
C       Right arrow
        IF (UICOL .LT. 255) THEN
          UICOL = UICOL + 1
        END IF

      ELSE IF (KEY .EQ. 131) THEN
C       Left arrow
        IF (UICOL .GT. 1) THEN
          UICOL = UICOL - 1
        END IF

C     Check for Ctrl+Arrow keys (jump to edge)
      ELSE IF (KEY .EQ. 132) THEN
C       Ctrl+Up - jump to row 1
        UIROW = 1

      ELSE IF (KEY .EQ. 133) THEN
C       Ctrl+Down - jump to last row
        UIROW = 255

      ELSE IF (KEY .EQ. 134) THEN
C       Ctrl+Right - jump to last column
        UICOL = 255

      ELSE IF (KEY .EQ. 135) THEN
C       Ctrl+Left - jump to column A
        UICOL = 1

C     Check for slash (command mode)
      ELSE IF (KEY .EQ. 47) THEN
C       Enter COMMAND mode
        CUMODE = 4
        UIBLEN = 0

C     Check for Delete/Backspace (clear cell in NAV mode)
      ELSE IF (KEY .EQ. 127 .OR. KEY .EQ. 8) THEN
C       Delete current cell contents
        CALL CELDEL(UICOL, UIROW)

C     Check for printable characters (start ENTRY mode)
      ELSE IF (KEY .GE. 32 .AND. KEY .LE. 126) THEN
C       Enter ENTRY mode
        CUMODE = 2
        UIBLEN = 1
        INBUF(1) = KEY

      END IF

      RETURN
      END

C======================================================================
C     ENTKEY - Handle keystroke in ENTRY mode
C======================================================================
      SUBROUTINE ENTKEY(KEY)
      INTEGER KEY
C     Process keystroke in entry mode
C
C     RETURN (13): Finish entry, evaluate, return to NAV
C     ESC (27): Cancel entry, return to NAV
C     BACKSPACE (8/127): Delete last character
C     Printable: Add to buffer

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

      REAL VALUE
      REAL ATOR
      INTEGER CTYPE
      INTEGER TOKENS(100, 4)
      INTEGER NTOK
      INTEGER DEPS(100, 2)
      INTEGER NDEPS
      INTEGER ERR
      INTEGER I, J
      INTEGER ISNUM, CH
      INTEGER ALIGN, TXTBUF(80), TXTLEN

C     Check for RETURN (CR=13 or LF=10, platform-dependent)
      IF (KEY .EQ. 13 .OR. KEY .EQ. 10) THEN
C       Finish entry

        IF (UIBLEN .GT. 0) THEN
C         Check for label prefix (Lotus 1-2-3 convention)
C         ' (39) = left align, " (34) = right align, ^ (94) = center
          ALIGN = 0
          IF (INBUF(1) .EQ. 39) THEN
C           Single quote = left align
            ALIGN = 1
          ELSE IF (INBUF(1) .EQ. 34) THEN
C           Double quote = right align
            ALIGN = 2
          ELSE IF (INBUF(1) .EQ. 94) THEN
C           Caret = center align
            ALIGN = 3
          END IF

C         If label prefix found, store as text cell
          IF (ALIGN .GT. 0) THEN
C           Copy text without the prefix
            TXTLEN = UIBLEN - 1
            IF (TXTLEN .GT. 0) THEN
              DO 3 J = 1, TXTLEN
                TXTBUF(J) = INBUF(J + 1)
3             CONTINUE
              CALL CELTXT(UICOL, UIROW, TXTBUF, TXTLEN, ALIGN)
            END IF
            GO TO 18
          END IF

C         Check if input is all whitespace - if so, clear cell
          ISNUM = 1
          DO 4 I = 1, UIBLEN
            IF (INBUF(I) .NE. 32) ISNUM = 0
4         CONTINUE
          IF (ISNUM .EQ. 1) THEN
C           All spaces - delete cell contents
            CALL CELDEL(UICOL, UIROW)
            GO TO 18
          END IF

C         Check if input is a plain number (digits, decimal, sign only)
          ISNUM = 1
          DO 5 I = 1, UIBLEN
            CH = INBUF(I)
            IF (CH .GE. 48 .AND. CH .LE. 57) GO TO 5
            IF (CH .EQ. 46) GO TO 5
            IF (CH .EQ. 45) GO TO 5
            IF (CH .EQ. 43) GO TO 5
            IF (CH .EQ. 32) GO TO 5
            ISNUM = 0
5         CONTINUE

C         If plain number, use ATOR directly
          IF (ISNUM .EQ. 1) THEN
            VALUE = ATOR(INBUF, UIBLEN)
            CALL CELPUT(UICOL, UIROW, 1, VALUE)
            GO TO 18
          END IF

C         Try to parse as formula
          CALL PARSE(INBUF, UIBLEN, TOKENS, NTOK, ERR)

          IF (ERR .EQ. 0 .AND. NTOK .GT. 0) THEN
C           Extract dependencies from tokens (type 2 = cell references)
            NDEPS = 0
            DO 15 I = 1, NTOK
              IF (TOKENS(I, 1) .EQ. 2) THEN
C               Cell reference - add to dependencies
                NDEPS = NDEPS + 1
                DEPS(NDEPS, 1) = TOKENS(I, 3)
                DEPS(NDEPS, 2) = TOKENS(I, 4)
              END IF
15          CONTINUE

C           Valid formula - evaluate and store
            CALL EVAL(TOKENS, NTOK, VALUE, ERR)

            IF (ERR .EQ. 0) THEN
C             Store result
              CALL CELPUT(UICOL, UIROW, 2, VALUE)

C             Store formula string (for edit line display)
              CALL CELPFS(UICOL, UIROW, INBUF, UIBLEN)

C             Add dependencies
              CALL DEPSAD(UICOL, UIROW, DEPS, NDEPS)

C             Recalculate dependents (only if AUTO mode)
              CALL UIRCGT(CTYPE)
              IF (CTYPE .EQ. 1) THEN
                CALL RECALC(UICOL, UIROW)
              END IF
            ELSE
C             EVAL failed - treat as left-aligned text
              CALL CELTXT(UICOL, UIROW, INBUF, UIBLEN, 1)
            END IF
          ELSE
C           Not a number or formula - treat as left-aligned text
C           (Lotus 1-2-3 default behavior for plain text)
            CALL CELTXT(UICOL, UIROW, INBUF, UIBLEN, 1)
          END IF
        END IF

C       Done processing input
18      CONTINUE

C       Return to NAV mode and move cursor DOWN (Enter = down)
        CUMODE = 1
        UIBLEN = 0
        IF (UIROW .LT. 255) THEN
          UIROW = UIROW + 1
        END IF

C     Check for TAB (move RIGHT after entry)
      ELSE IF (KEY .EQ. 9) THEN
C       Store value and move right (same logic as RETURN)
        IF (UIBLEN .GT. 0) THEN
C         Check for label prefix
          ALIGN = 0
          IF (INBUF(1) .EQ. 39) THEN
            ALIGN = 1
          ELSE IF (INBUF(1) .EQ. 34) THEN
            ALIGN = 2
          ELSE IF (INBUF(1) .EQ. 94) THEN
            ALIGN = 3
          END IF

          IF (ALIGN .GT. 0) THEN
C           Label with prefix - store text without prefix
            TXTLEN = UIBLEN - 1
            IF (TXTLEN .GT. 0) THEN
              DO 23 J = 1, TXTLEN
                TXTBUF(J) = INBUF(J + 1)
23            CONTINUE
              CALL CELTXT(UICOL, UIROW, TXTBUF, TXTLEN, ALIGN)
            END IF
          ELSE
C           Check if all whitespace
            ISNUM = 1
            DO 24 I = 1, UIBLEN
              IF (INBUF(I) .NE. 32) ISNUM = 0
24          CONTINUE
            IF (ISNUM .EQ. 1) THEN
              CALL CELDEL(UICOL, UIROW)
            ELSE
C             Check if plain number
              ISNUM = 1
              DO 25 I = 1, UIBLEN
                CH = INBUF(I)
                IF (CH .GE. 48 .AND. CH .LE. 57) GO TO 25
                IF (CH .EQ. 46) GO TO 25
                IF (CH .EQ. 45) GO TO 25
                IF (CH .EQ. 43) GO TO 25
                IF (CH .EQ. 32) GO TO 25
                ISNUM = 0
25            CONTINUE

              IF (ISNUM .EQ. 1) THEN
C               Plain number
                VALUE = ATOR(INBUF, UIBLEN)
                CALL CELPUT(UICOL, UIROW, 1, VALUE)
              ELSE
C               Not a number - store as left-aligned text
                CALL CELTXT(UICOL, UIROW, INBUF, UIBLEN, 1)
              END IF
            END IF
          END IF
        END IF
        CUMODE = 1
        UIBLEN = 0
        IF (UICOL .LT. 255) THEN
          UICOL = UICOL + 1
        END IF

C     Check for ESC
      ELSE IF (KEY .EQ. 27) THEN
C       Cancel entry
        CUMODE = 1
        UIBLEN = 0

C     Check for BACKSPACE
      ELSE IF (KEY .EQ. 8 .OR. KEY .EQ. 127) THEN
        IF (UIBLEN .GT. 0) THEN
          UIBLEN = UIBLEN - 1
        END IF

C     Check for printable characters
      ELSE IF (KEY .GE. 32 .AND. KEY .LE. 126) THEN
        IF (UIBLEN .LT. 80) THEN
          UIBLEN = UIBLEN + 1
          INBUF(UIBLEN) = KEY
        END IF

      END IF

      RETURN
      END

C======================================================================
C     CMDKEY - Handle keystroke in COMMAND mode
C======================================================================
      SUBROUTINE CMDKEY(KEY, RUNNING)
      INTEGER KEY
      INTEGER RUNNING
C     Process keystroke in command mode
C
C     Commands: QUIT, SAVE filename, OPEN filename, COPY range dest
C     RETURN: Execute command
C     ESC: Cancel command
C     BACKSPACE: Delete character
C     Printable: Add to buffer

C     UI state
      INTEGER CUMODE, UICOL, UIROW
      INTEGER INBUF(80), UIBLEN
      INTEGER UIFRM(80), UIFLEN
      INTEGER RCMODE
      COMMON /UIDAT/ CUMODE, UICOL, UIROW,
     &               INBUF, UIBLEN,
     &               UIFRM, UIFLEN,
     &               RCMODE

C     Command: QUIT (Q,U,I,T = 81,85,73,84)
      INTEGER QCMD(4)
      DATA QCMD /81,85,73,84/

C     Command: SAVE (S,A,V,E = 83,65,86,69)
      INTEGER SCMD(4)
      DATA SCMD /83,65,86,69/

C     Command: OPEN (O,P,E,N = 79,80,69,78)
      INTEGER OCMD(4)
      DATA OCMD /79,80,69,78/

C     Command: COPY (C,O,P,Y = 67,79,80,89)
      INTEGER CCMD(4)
      DATA CCMD /67,79,80,89/

C     Command: GOTO - commented out (not needed for 255x255 grid)
C     INTEGER GCMD(4)
C     DATA GCMD /71,79,84,79/

C     Command: RECALC (R,E,C,A,L,C = 82,69,67,65,76,67)
      INTEGER RCMD(6)
      DATA RCMD /82,69,67,65,76,67/

C     Mode keywords: AUTO (A,U,T,O = 65,85,84,79)
      INTEGER ACMD(4)
      DATA ACMD /65,85,84,79/

C     Mode keywords: MANUAL (M,A,N,U,A,L = 77,65,78,85,65,76)
      INTEGER MCMD(6)
      DATA MCMD /77,65,78,85,65,76/

C     Command: WIDTH (W,I,D,T,H = 87,73,68,84,72)
      INTEGER WCMD(5)
      DATA WCMD /87,73,68,84,72/

C     Command: IR - Insert Row (I,R = 73,82)
      INTEGER IRCMD(2)
      DATA IRCMD /73,82/

C     Command: DR - Delete Row (D,R = 68,82)
      INTEGER DRCMD(2)
      DATA DRCMD /68,82/

C     Command: IC - Insert Column (I,C = 73,67)
      INTEGER ICCMD(2)
      DATA ICCMD /73,67/

C     Command: DC - Delete Column (D,C = 68,67)
      INTEGER DCCMD(2)
      DATA DCCMD /68,67/

      INTEGER I, MATCH, ERR, CH
      INTEGER FNAME(80), FLEN
      INTEGER SC1, SR1, SC2, SR2, DC, DR
      INTEGER RMODE
      INTEGER WCOL, WWID
      INTEGER IROW, ICOL

C     Check for RETURN (CR=13 or LF=10, platform-dependent)
      IF (KEY .EQ. 13 .OR. KEY .EQ. 10) THEN
C       Execute command

C       Check for QUIT (4 chars exactly, case insensitive)
        IF (UIBLEN .EQ. 4) THEN
          MATCH = 1
          DO 10 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. QCMD(I)) THEN
              MATCH = 0
            END IF
10        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Quit command
            RUNNING = 0
            RETURN
          END IF
        END IF

C       Check for SAVE filename (case insensitive)
        IF (UIBLEN .GE. 6) THEN
          MATCH = 1
          DO 20 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. SCMD(I)) MATCH = 0
20        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Extract filename (skip "SAVE ")
            FLEN = UIBLEN - 5
            DO 25 I = 1, FLEN
              FNAME(I) = INBUF(5 + I)
25          CONTINUE

C           Check extension: .xlb = binary, else = JSON
C           Look for .xlb or .XLB at end of filename
            CALL FLXSAV(FNAME, FLEN, ERR)
            IF (ERR .EQ. 0) THEN
              CALL DSPMSG(1)
            ELSE
              CALL DSPMSG(2)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for OPEN filename (case insensitive)
        IF (UIBLEN .GE. 6) THEN
          MATCH = 1
          DO 30 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. OCMD(I)) MATCH = 0
30        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Extract filename (skip "OPEN ")
            FLEN = UIBLEN - 5
            DO 35 I = 1, FLEN
              FNAME(I) = INBUF(5 + I)
35          CONTINUE

C           Check extension: .xlb = binary, else try both
            CALL FLXLOD(FNAME, FLEN, ERR)
            IF (ERR .EQ. 0) THEN
              CALL DSPMSG(3)
              CALL DSPFUL
            ELSE
              CALL DSPMSG(4)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for COPY range dest (at least COPY + space + A1:B2 + space + C1 = 14)
        IF (UIBLEN .GE. 10) THEN
          MATCH = 1
          DO 40 I = 1, 4
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. CCMD(I)) MATCH = 0
40        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Parse COPY command: COPY A1:B5 C1 or COPY A1 C1
            CALL CPYPRS(INBUF, UIBLEN, SC1, SR1, SC2, SR2, DC, DR,
     &                  ERR)
            IF (ERR .EQ. 0) THEN
C             Execute copy
              CALL CPYRNG(SC1, SR1, SC2, SR2, DC, DR)
              CALL DSPMSG(5)
              CALL DSPFUL
            ELSE
              CALL DSPMSG(6)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for RECALC command (case insensitive)
C       /RECALC - force recalculate all
C       /RECALC AUTO - set auto mode
C       /RECALC MANUAL - set manual mode
        IF (UIBLEN .GE. 6) THEN
          MATCH = 1
          DO 45 I = 1, 6
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. RCMD(I)) MATCH = 0
45        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /RECALC (force recalc)
            IF (UIBLEN .EQ. 6) THEN
C             Force recalculate all cells
              CALL RECALL
              CALL DSPMSG(7)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /RECALC AUTO (7 = space + 4 letters)
            IF (UIBLEN .GE. 11 .AND. INBUF(7) .EQ. 32) THEN
              MATCH = 1
              DO 46 I = 1, 4
                CH = INBUF(7 + I)
                IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
                IF (CH .NE. ACMD(I)) MATCH = 0
46            CONTINUE

              IF (MATCH .EQ. 1) THEN
C               Set AUTO mode
                CALL UIRCST(1)
                CALL DSPMSG(8)
                CUMODE = 1
                UIBLEN = 0
                RETURN
              END IF
            END IF

C           Check for /RECALC MANUAL (7 = space + 6 letters)
            IF (UIBLEN .GE. 13 .AND. INBUF(7) .EQ. 32) THEN
              MATCH = 1
              DO 47 I = 1, 6
                CH = INBUF(7 + I)
                IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
                IF (CH .NE. MCMD(I)) MATCH = 0
47            CONTINUE

              IF (MATCH .EQ. 1) THEN
C               Set MANUAL mode
                CALL UIRCST(2)
                CALL DSPMSG(9)
                CUMODE = 1
                UIBLEN = 0
                RETURN
              END IF
            END IF
          END IF
        END IF

C       Check for WIDTH command (case insensitive)
C       /WIDTH col width - Set column width (e.g., /WIDTH A 12)
        IF (UIBLEN .GE. 7) THEN
          MATCH = 1
          DO 48 I = 1, 5
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. WCMD(I)) MATCH = 0
48        CONTINUE

          IF (MATCH .EQ. 1 .AND. INBUF(6) .EQ. 32) THEN
C           Parse WIDTH command: WIDTH A 12 or WIDTH AA 15
            CALL WIDPRS(INBUF, UIBLEN, WCOL, WWID, ERR)
            IF (ERR .EQ. 0) THEN
C             Set column width
              CALL CWSET(WCOL, WWID)
              CALL DSPMSG(10)
              CALL DSPFUL
            ELSE
              CALL DSPMSG(11)
            END IF
            CUMODE = 1
            UIBLEN = 0
            RETURN
          END IF
        END IF

C       Check for IR (Insert Row) command (case insensitive)
C       /IR - Insert row at current cursor position
C       /IR n - Insert row at row n
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 60 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. IRCMD(I)) MATCH = 0
60        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /IR (use cursor row)
            IF (UIBLEN .EQ. 2) THEN
              CALL ROWINS(UIROW)
              CALL DSPMSG(12)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /IR n (parse row number)
            IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
              CALL PRSNUM(INBUF, UIBLEN, 4, IROW, ERR)
              IF (ERR .EQ. 0 .AND. IROW .GE. 1 .AND. IROW .LE. 255) THEN
                CALL ROWINS(IROW)
                CALL DSPMSG(12)
                CALL DSPFUL
              ELSE
                CALL DSPMSG(13)
              END IF
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF
          END IF
        END IF

C       Check for DR (Delete Row) command (case insensitive)
C       /DR - Delete row at current cursor position
C       /DR n - Delete row n
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 61 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. DRCMD(I)) MATCH = 0
61        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /DR (use cursor row)
            IF (UIBLEN .EQ. 2) THEN
              CALL ROWDEL(UIROW)
              CALL DSPMSG(14)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /DR n (parse row number)
            IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
              CALL PRSNUM(INBUF, UIBLEN, 4, IROW, ERR)
              IF (ERR .EQ. 0 .AND. IROW .GE. 1 .AND. IROW .LE. 255) THEN
                CALL ROWDEL(IROW)
                CALL DSPMSG(14)
                CALL DSPFUL
              ELSE
                CALL DSPMSG(13)
              END IF
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF
          END IF
        END IF

C       Check for IC (Insert Column) command (case insensitive)
C       /IC - Insert column at current cursor position
C       /IC A - Insert column at column A
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 62 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. ICCMD(I)) MATCH = 0
62        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /IC (use cursor column)
            IF (UIBLEN .EQ. 2) THEN
              CALL COLINS(UICOL)
              CALL DSPMSG(15)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /IC col (parse column letter)
            IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
              CALL PRSCOL(INBUF, UIBLEN, 4, ICOL, ERR)
              IF (ERR .EQ. 0 .AND. ICOL .GE. 1 .AND. ICOL .LE. 255) THEN
                CALL COLINS(ICOL)
                CALL DSPMSG(15)
                CALL DSPFUL
              ELSE
                CALL DSPMSG(16)
              END IF
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF
          END IF
        END IF

C       Check for DC (Delete Column) command (case insensitive)
C       /DC - Delete column at current cursor position
C       /DC A - Delete column A
        IF (UIBLEN .GE. 2) THEN
          MATCH = 1
          DO 63 I = 1, 2
            CH = INBUF(I)
            IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
            IF (CH .NE. DCCMD(I)) MATCH = 0
63        CONTINUE

          IF (MATCH .EQ. 1) THEN
C           Check if just /DC (use cursor column)
            IF (UIBLEN .EQ. 2) THEN
              CALL COLDEL(UICOL)
              CALL DSPMSG(17)
              CALL DSPFUL
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF

C           Check for /DC col (parse column letter)
            IF (UIBLEN .GE. 4 .AND. INBUF(3) .EQ. 32) THEN
              CALL PRSCOL(INBUF, UIBLEN, 4, ICOL, ERR)
              IF (ERR .EQ. 0 .AND. ICOL .GE. 1 .AND. ICOL .LE. 255) THEN
                CALL COLDEL(ICOL)
                CALL DSPMSG(17)
                CALL DSPFUL
              ELSE
                CALL DSPMSG(16)
              END IF
              CUMODE = 1
              UIBLEN = 0
              RETURN
            END IF
          END IF
        END IF

C       GOTO command - commented out (not needed for 255x255 grid)
C       IF (UIBLEN .GE. 6) THEN
C         MATCH = 1
C         DO 50 I = 1, 4
C           CH = INBUF(I)
C           IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
C           IF (CH .NE. GCMD(I)) MATCH = 0
C50       CONTINUE
C
C         IF (MATCH .EQ. 1 .AND. INBUF(5) .EQ. 32) THEN
C           Parse GOTO command: GOTO AA1000
C           CALL GOTPRS(INBUF, UIBLEN, GCOL, GROW, ERR)
C           IF (ERR .EQ. 0) THEN
C             Move cursor to cell
C             UICOL = GCOL
C             UIROW = GROW
C             Scroll viewport to show cell
C             CALL DSPSCR(UICOL, UIROW)
C             CALL DSPFUL
C           END IF
C           CUMODE = 1
C           UIBLEN = 0
C           RETURN
C         END IF
C       END IF

C       Unknown command - ignore
        CUMODE = 1
        UIBLEN = 0

C     Check for ESC
      ELSE IF (KEY .EQ. 27) THEN
        CUMODE = 1
        UIBLEN = 0

C     Check for BACKSPACE
      ELSE IF (KEY .EQ. 8 .OR. KEY .EQ. 127) THEN
        IF (UIBLEN .GT. 0) THEN
          UIBLEN = UIBLEN - 1
        END IF

C     Check for printable characters
      ELSE IF (KEY .GE. 32 .AND. KEY .LE. 126) THEN
        IF (UIBLEN .LT. 80) THEN
          UIBLEN = UIBLEN + 1
          INBUF(UIBLEN) = KEY
        END IF

      END IF

      RETURN
      END

C======================================================================
C     CPYPRS - Parse COPY command arguments
C     Supports 1-2 letter columns (A-ZZ)
C======================================================================
      SUBROUTINE CPYPRS(BUF, BLEN, SC1, SR1, SC2, SR2, DC, DR, ERR)
      INTEGER BUF(*)
      INTEGER BLEN
      INTEGER SC1, SR1, SC2, SR2, DC, DR
      INTEGER ERR

C     Parse: COPY AA1:BB5 CC1 or COPY A1 C1
C     BUF starts with "COPY "

      INTEGER POS, CH

      ERR = 0
      POS = 6       ! Skip "COPY "

C     Parse source start column (1-2 letters)
      SC1 = 0
5     IF (POS .GT. BLEN) GO TO 900
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 7
      SC1 = SC1 * 26 + (CH - 64)
      POS = POS + 1
      GO TO 5
7     IF (SC1 .EQ. 0) GO TO 900

C     Parse source start row
      SR1 = 0
10    IF (POS .GT. BLEN) GO TO 900
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 20
      SR1 = SR1 * 10 + (CH - 48)
      POS = POS + 1
      GO TO 10
20    IF (SR1 .EQ. 0) GO TO 900

C     Check for colon (range) or space (single cell)
      IF (CH .EQ. 58) THEN
C       Range - parse end cell
        POS = POS + 1
        IF (POS .GT. BLEN) GO TO 900

C       Parse source end column (1-2 letters)
        SC2 = 0
25      CH = BUF(POS)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 27
        SC2 = SC2 * 26 + (CH - 64)
        POS = POS + 1
        IF (POS .LE. BLEN) GO TO 25
27      IF (SC2 .EQ. 0) GO TO 900

C       Parse source end row
        SR2 = 0
30      IF (POS .GT. BLEN) GO TO 900
        CH = BUF(POS)
        IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 40
        SR2 = SR2 * 10 + (CH - 48)
        POS = POS + 1
        GO TO 30
40      IF (SR2 .EQ. 0) GO TO 900
      ELSE
C       Single cell - end = start
        SC2 = SC1
        SR2 = SR1
      END IF

C     Expect space before destination
      IF (CH .NE. 32) GO TO 900
      POS = POS + 1

C     Skip extra spaces
50    IF (POS .GT. BLEN) GO TO 900
      IF (BUF(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 50
      END IF

C     Parse destination column (1-2 letters)
      DC = 0
55    IF (POS .GT. BLEN) GO TO 57
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 57
      DC = DC * 26 + (CH - 64)
      POS = POS + 1
      GO TO 55
57    IF (DC .EQ. 0) GO TO 900

C     Parse destination row
      DR = 0
60    IF (POS .GT. BLEN) GO TO 70
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 70
      DR = DR * 10 + (CH - 48)
      POS = POS + 1
      GO TO 60

70    IF (DR .EQ. 0) GO TO 900

C     Validate ranges (255 cols = A-IU, 255 rows)
      IF (SC1 .LT. 1 .OR. SC1 .GT. 255) GO TO 900
      IF (SC2 .LT. 1 .OR. SC2 .GT. 255) GO TO 900
      IF (SR1 .LT. 1 .OR. SR1 .GT. 255) GO TO 900
      IF (SR2 .LT. 1 .OR. SR2 .GT. 255) GO TO 900
      IF (DC .LT. 1 .OR. DC .GT. 255) GO TO 900
      IF (DR .LT. 1 .OR. DR .GT. 255) GO TO 900

      RETURN

900   ERR = 1
      RETURN
      END

C======================================================================
C     CPYRNG - Copy range of cells with formula adjustment
C======================================================================
      SUBROUTINE CPYRNG(SC1, SR1, SC2, SR2, DC, DR)
      INTEGER SC1, SR1, SC2, SR2, DC, DR

C     Copy cells from (SC1,SR1):(SC2,SR2) to destination starting at (DC,DR)
C     Adjust relative cell references in formulas

      INTEGER SCOL, SROW, DCOL, DROW
      INTEGER COLDIF, ROWDIF
      INTEGER CTYPE, ALIGN
      REAL VALUE
      INTEGER FSTR(80), FLEN
      INTEGER NFSTR(80), NFLEN
      INTEGER TXTBUF(80), TXTLEN

C     Calculate offset
      COLDIF = DC - SC1
      ROWDIF = DR - SR1

C     Ensure SC1 <= SC2 and SR1 <= SR2
      IF (SC1 .GT. SC2) THEN
        SCOL = SC1
        SC1 = SC2
        SC2 = SCOL
      END IF
      IF (SR1 .GT. SR2) THEN
        SROW = SR1
        SR1 = SR2
        SR2 = SROW
      END IF

C     Copy each cell
      DO 100 SCOL = SC1, SC2
        DO 90 SROW = SR1, SR2
          DCOL = SCOL + COLDIF
          DROW = SROW + ROWDIF

C         Skip if destination out of bounds (255 cols, 255 rows)
          IF (DCOL .LT. 1 .OR. DCOL .GT. 255) GO TO 90
          IF (DROW .LT. 1 .OR. DROW .GT. 255) GO TO 90

C         Get source cell
          CALL CELGET(SCOL, SROW, CTYPE, VALUE)

          IF (CTYPE .EQ. 0) THEN
C           Empty - delete destination
            CALL CELDEL(DCOL, DROW)

          ELSE IF (CTYPE .EQ. 1) THEN
C           Number - copy directly
            CALL CELPUT(DCOL, DROW, 1, VALUE)

          ELSE IF (CTYPE .EQ. 2) THEN
C           Formula - adjust references and copy
            CALL CELGFS(SCOL, SROW, FSTR, 80, FLEN)
            IF (FLEN .GT. 0) THEN
C             Adjust cell references
              CALL CPYADJ(FSTR, FLEN, NFSTR, NFLEN, COLDIF, ROWDIF)
C             Store adjusted formula
              CALL CELPFS(DCOL, DROW, NFSTR, NFLEN)
C             Recalculate
              CALL RECALC(DCOL, DROW)
            END IF

          ELSE IF (CTYPE .EQ. 3) THEN
C           Text - copy directly
            CALL CELGTX(SCOL, SROW, TXTBUF, 80, TXTLEN, ALIGN)
            CALL CELTXT(DCOL, DROW, TXTBUF, TXTLEN, ALIGN)

          END IF

90      CONTINUE
100   CONTINUE

      RETURN
      END

C======================================================================
C     CPYADJ - Adjust cell references in formula by offset
C     Supports 1-2 letter columns (A-ZZ)
C======================================================================
      SUBROUTINE CPYADJ(FSTR, FLEN, NFSTR, NFLEN, COLDIF, ROWDIF)
      INTEGER FSTR(*), FLEN
      INTEGER NFSTR(*), NFLEN
      INTEGER COLDIF, ROWDIF

C     Copy formula string, adjusting relative cell references
C     Absolute references ($AA$1) are preserved unchanged
C     Mixed references ($AA1, AA$1) adjust only the relative part

      INTEGER I, J, K, CH, COL, ROW, NEWCOL, NEWROW
      INTEGER ROWSTR(10), RLEN
      INTEGER COLSTR(10), CLEN
      INTEGER ABSCOL, ABSROW

      NFLEN = 0
      I = 1

100   IF (I .GT. FLEN) RETURN

      CH = FSTR(I)

C     Check for $ (start of absolute reference)
      ABSCOL = 0
      ABSROW = 0
      IF (CH .EQ. 36) THEN
C       $ found - check if followed by column letter
        IF (I + 1 .LE. FLEN) THEN
          CH = FSTR(I + 1)
          IF ((CH .GE. 65 .AND. CH .LE. 90) .OR.
     &        (CH .GE. 97 .AND. CH .LE. 122)) THEN
C           $A form - absolute column
            ABSCOL = 1
            I = I + 1
            CH = FSTR(I)
          END IF
        END IF
      END IF

C     Check for cell reference (letter A-Z or a-z)
      IF ((CH .GE. 65 .AND. CH .LE. 90) .OR.
     &    (CH .GE. 97 .AND. CH .LE. 122)) THEN
C       Parse column letters (1-2 letters, base-26)
        COL = 0
        J = I
105     IF (J .GT. FLEN) GO TO 107
        CH = FSTR(J)
        IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
        IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 107
        COL = COL * 26 + (CH - 64)
        J = J + 1
        GO TO 105
107     IF (COL .EQ. 0) GO TO 150

C       Check if followed by $ (absolute row marker)
        IF (J .LE. FLEN .AND. FSTR(J) .EQ. 36) THEN
          ABSROW = 1
          J = J + 1
        END IF

C       Check if followed by digits (row number)
        ROW = 0
110     IF (J .GT. FLEN) GO TO 120
        CH = FSTR(J)
        IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 120
        ROW = ROW * 10 + (CH - 48)
        J = J + 1
        GO TO 110

120     IF (ROW .GT. 0) THEN
C         Valid cell reference - adjust based on absolute flags
          IF (ABSCOL .EQ. 0) THEN
            NEWCOL = COL + COLDIF
          ELSE
            NEWCOL = COL
          END IF
          IF (ABSROW .EQ. 0) THEN
            NEWROW = ROW + ROWDIF
          ELSE
            NEWROW = ROW
          END IF

C         Clamp to valid range (255 cols = A-IU, 255 rows)
          IF (NEWCOL .LT. 1) NEWCOL = 1
          IF (NEWCOL .GT. 255) NEWCOL = 255
          IF (NEWROW .LT. 1) NEWROW = 1
          IF (NEWROW .GT. 255) NEWROW = 255

C         Output reference with $ markers preserved
          IF (ABSCOL .EQ. 1) THEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = 36
          END IF

C         Convert column to letters (A-ZZ)
          CALL COLTOA(NEWCOL, COLSTR, 10, CLEN)
          DO 125 K = 1, CLEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = COLSTR(K)
125       CONTINUE

          IF (ABSROW .EQ. 1) THEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = 36
          END IF

C         Convert row to string
          CALL ITOA(NEWROW, ROWSTR, 10, RLEN)
          DO 130 K = 1, RLEN
            NFLEN = NFLEN + 1
            NFSTR(NFLEN) = ROWSTR(K)
130       CONTINUE

          I = J
          GO TO 100
        END IF
      END IF

C     Copy character as-is
150   NFLEN = NFLEN + 1
      NFSTR(NFLEN) = FSTR(I)
      I = I + 1
      GO TO 100

      END

C======================================================================
C     GOTPRS - Parse GOTO command argument (cell reference)
C     COMMENTED OUT - Not needed for 255x255 grid
C======================================================================
C     SUBROUTINE GOTPRS(BUF, BLEN, COL, ROW, ERR)
C     INTEGER BUF(*)
C     INTEGER BLEN
C     INTEGER COL, ROW
C     INTEGER ERR
C
C     Parse: GOTO AA1000
C     BUF starts with "GOTO "
C
C     INTEGER POS, CH
C
C     ERR = 0
C     POS = 6       ! Skip "GOTO "
C     COL = 0
C     ROW = 0
C
C     Skip extra spaces
C5    IF (POS .GT. BLEN) GO TO 900
C     IF (BUF(POS) .EQ. 32) THEN
C       POS = POS + 1
C       GO TO 5
C     END IF
C
C     Parse column letters (1-2 letters, base-26)
C10   IF (POS .GT. BLEN) GO TO 20
C     CH = BUF(POS)
C     IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
C     IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 20
C     COL = COL * 26 + (CH - 64)
C     POS = POS + 1
C     GO TO 10
C
C20   IF (COL .EQ. 0) GO TO 900
C
C     Parse row number
C30   IF (POS .GT. BLEN) GO TO 40
C     CH = BUF(POS)
C     IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 40
C     ROW = ROW * 10 + (CH - 48)
C     POS = POS + 1
C     GO TO 30
C
C40   IF (ROW .EQ. 0) GO TO 900
C
C     Validate bounds (255 cols = A-IU, 255 rows)
C     IF (COL .LT. 1 .OR. COL .GT. 255) GO TO 900
C     IF (ROW .LT. 1 .OR. ROW .GT. 255) GO TO 900
C
C     RETURN
C
C900  ERR = 1
C     RETURN
C     END

C======================================================================
C     WIDPRS - Parse WIDTH command arguments
C     Supports 1-2 letter columns (A-ZZ)
C======================================================================
      SUBROUTINE WIDPRS(BUF, BLEN, COL, WIDTH, ERR)
      INTEGER BUF(*)
      INTEGER BLEN
      INTEGER COL, WIDTH
      INTEGER ERR

C     Parse: WIDTH A 12 or WIDTH AA 15
C     BUF starts with "WIDTH "

      INTEGER POS, CH

      ERR = 0
      POS = 7       ! Skip "WIDTH "
      COL = 0
      WIDTH = 0

C     Skip extra spaces
5     IF (POS .GT. BLEN) GO TO 900
      IF (BUF(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 5
      END IF

C     Parse column letters (1-2 letters, base-26)
10    IF (POS .GT. BLEN) GO TO 20
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 20
      COL = COL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 10

20    IF (COL .EQ. 0) GO TO 900

C     Skip spaces between column and width
30    IF (POS .GT. BLEN) GO TO 900
      IF (BUF(POS) .EQ. 32) THEN
        POS = POS + 1
        GO TO 30
      END IF

C     Parse width number
40    IF (POS .GT. BLEN) GO TO 50
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 50
      WIDTH = WIDTH * 10 + (CH - 48)
      POS = POS + 1
      GO TO 40

50    IF (WIDTH .EQ. 0) GO TO 900

C     Validate column (1-255) and width (3-40)
      IF (COL .LT. 1 .OR. COL .GT. 255) GO TO 900
      IF (WIDTH .LT. 3 .OR. WIDTH .GT. 40) GO TO 900

      RETURN

900   ERR = 1
      RETURN
      END

C======================================================================
C     PRSNUM - Parse a number from buffer starting at position START
C======================================================================
      SUBROUTINE PRSNUM(BUF, BLEN, START, NUM, ERR)
      INTEGER BUF(*)
      INTEGER BLEN, START, NUM, ERR

      INTEGER POS, CH

      ERR = 0
      NUM = 0
      POS = START

C     Parse digits
10    IF (POS .GT. BLEN) GO TO 20
      CH = BUF(POS)
      IF (CH .LT. 48 .OR. CH .GT. 57) GO TO 20
      NUM = NUM * 10 + (CH - 48)
      POS = POS + 1
      GO TO 10

20    IF (NUM .EQ. 0) THEN
        ERR = 1
      END IF

      RETURN
      END

C======================================================================
C     PRSCOL - Parse a column letter (A-IU) from buffer
C======================================================================
      SUBROUTINE PRSCOL(BUF, BLEN, START, COL, ERR)
      INTEGER BUF(*)
      INTEGER BLEN, START, COL, ERR

      INTEGER POS, CH

      ERR = 0
      COL = 0
      POS = START

C     Parse letters (1 or 2)
10    IF (POS .GT. BLEN) GO TO 20
      CH = BUF(POS)
      IF (CH .GE. 97 .AND. CH .LE. 122) CH = CH - 32
      IF (CH .LT. 65 .OR. CH .GT. 90) GO TO 20
      COL = COL * 26 + (CH - 64)
      POS = POS + 1
      GO TO 10

20    IF (COL .EQ. 0) THEN
        ERR = 1
      END IF

      RETURN
      END
