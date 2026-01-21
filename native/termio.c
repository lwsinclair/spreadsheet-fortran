/*
 * termio.c - Raw terminal I/O for XL Spreadsheet
 *
 * Provides unbuffered single-character input for FORTRAN
 * Callable from FORTRAN as external subroutines
 *
 * Functions:
 *   tminitc_() - Initialize raw terminal mode
 *   tmreadc_(ch, valid) - Read single character (non-blocking)
 *   tmrstc_() - Restore normal terminal mode
 */

#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>

static struct termios orig_termios;
static int raw_mode = 0;

/*
 * tminitc_ - Initialize terminal for raw input
 * Called from FORTRAN as: CALL TMINITC
 */
void tminitc_(void) {
    if (raw_mode) return;

    /* Save original terminal settings */
    tcgetattr(STDIN_FILENO, &orig_termios);

    /* Set raw mode */
    struct termios raw = orig_termios;
    raw.c_lflag &= ~(ICANON | ECHO);  /* Disable canonical mode and echo */
    raw.c_cc[VMIN] = 0;   /* Non-blocking */
    raw.c_cc[VTIME] = 1;  /* 0.1 second timeout */

    tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
    raw_mode = 1;
}

/*
 * tmreadc_ - Read single character
 * Called from FORTRAN as: CALL TMREADC(CH, VALID)
 *   CH: INTEGER - receives ASCII code of character
 *   VALID: INTEGER - 1 if character read, 0 if no input
 */
void tmreadc_(int *ch, int *valid) {
    unsigned char c;
    ssize_t n = read(STDIN_FILENO, &c, 1);

    if (n == 1) {
        *ch = (int)c;
        *valid = 1;
    } else {
        *ch = 0;
        *valid = 0;
    }
}

/*
 * tmrstc_ - Restore normal terminal mode
 * Called from FORTRAN as: CALL TMRSTC
 */
void tmrstc_(void) {
    if (raw_mode) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
        raw_mode = 0;
    }
}

/*
 * Terminal output functions - handle escape sequences properly
 * FORTRAN can't output arbitrary ASCII codes (like ESC=27) correctly
 */

/*
 * tmclrc_ - Clear screen (ANSI)
 * ESC [ H ESC [ 2 J
 */
void tmclrc_(void) {
    printf("\033[H\033[2J");
    fflush(stdout);
}

/*
 * tmhomec_ - Home cursor (ANSI)
 * ESC [ H
 */
void tmhomec_(void) {
    printf("\033[H");
    fflush(stdout);
}

/*
 * tmcursc_ - Position cursor (ANSI)
 * ESC [ row ; col H
 */
void tmcursc_(int *row, int *col) {
    printf("\033[%d;%dH", *row, *col);
    fflush(stdout);
}

/*
 * tmceolc_ - Clear to end of line (ANSI)
 * ESC [ K
 */
void tmceolc_(void) {
    printf("\033[K");
    fflush(stdout);
}

/*
 * tmrvonc_ - Reverse video on (ANSI)
 * ESC [ 7 m
 */
void tmrvonc_(void) {
    printf("\033[7m");
    fflush(stdout);
}

/*
 * tmrvofc_ - Reverse video off (ANSI)
 * ESC [ 0 m
 */
void tmrvofc_(void) {
    printf("\033[0m");
    fflush(stdout);
}

/*
 * tmputcc_ - Output single character by ASCII code
 */
void tmputcc_(int *ch) {
    putchar(*ch);
    fflush(stdout);
}

/*
 * tmputsc_ - Output string (array of ASCII codes)
 */
void tmputsc_(int *str, int *len) {
    for (int i = 0; i < *len; i++) {
        putchar(str[i]);
    }
    fflush(stdout);
}

/*
 * tmputnc_ - Output integer (right-aligned in 6 chars)
 */
void tmputnc_(int *num) {
    printf("%6d", *num);
    fflush(stdout);
}

/*
 * tmputn3c_ - Output integer right-aligned in 3 chars (for row numbers)
 */
void tmputn3c_(int *num) {
    printf("%3d", *num);
    fflush(stdout);
}

/*
 * tmputn5c_ - Output integer right-aligned in 5 chars (for 5-digit row numbers)
 */
void tmputn5c_(int *num) {
    printf("%5d", *num);
    fflush(stdout);
}

/*
 * tmputrc_ - Output real with precision (10 chars)
 */
void tmputrc_(float *num, int *prec) {
    printf("%10.*f", *prec, *num);
    fflush(stdout);
}

/*
 * tmputr8c_ - Output real with precision in 8 chars (for cell values)
 */
void tmputr8c_(float *num, int *prec) {
    printf("%8.*f", *prec, *num);
    fflush(stdout);
}

/*
 * tmputrwc_ - Output real with precision in variable width (for variable cell widths)
 */
void tmputrwc_(float *num, int *prec, int *width) {
    printf("%*.*f", *width, *prec, *num);
    fflush(stdout);
}

/*
 * tmflshc_ - Flush output
 */
void tmflshc_(void) {
    fflush(stdout);
}
