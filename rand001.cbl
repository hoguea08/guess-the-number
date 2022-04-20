IDENTIFICATION DIVISION.
PROGRAM-ID. RAND0001.

DATA DIVISION.
WORKING-STORAGE SECTION.

01  RAND-NUM                PIC 9.
01  GENERATED-NUM           PIC 99V999.
01  SWITCH                 	PIC X VALUES "Y".

01	HEADER.
	05	LINE1				PIC X(54) VALUES "******************************************************".
	05	LINE2				PIC X(54) VALUES "*    *     *     *     *     *     *     *      *    *".
	05	LINE3				PIC X(54) VALUES "* Hi! Would you like to play Guess the Number? (Y/N) *".
	05	LINE4				PIC X(54) VALUES "*    *     *     *     *     *     *     *      *    *".
	05	LINE5				PIC X(54) VALUES "******************************************************".
											 
01  GUESSES.
    05  NUM-GUESSES         PIC 9 VALUES ZEROS.
    05  CURRENT-GUESS       PIC 9.

01  DISP-MESSAGES.
    05  TOO-HIGH            PIC X(40) VALUES "That guess is too high.".
    05  TOO-LOW             PIC X(40) VALUES "That guess is too low.".
    05  CORRECT             PIC X(40) VALUES "You got it!".
    05  WRONG               PIC X(50) VALUES "Sorry, that was your last guess. The number was".
    05  TRY-AGAIN           PIC X(40) VALUES "Try again: ".

PROCEDURE DIVISION.

DISPLAY LINE1.
DISPLAY LINE2.
DISPLAY LINE3.
DISPLAY LINE4.
DISPLAY LINE5.
ACCEPT SWITCH.

IF SWITCH = "Y"
    PERFORM 000-GUESS-THE-NUM
        UNTIL SWITCH = "N".
    MOVE "*                     Good Bye!                      *" TO LINE3.
	DISPLAY LINE1.
    DISPLAY LINE2.
    DISPLAY LINE3.
    DISPLAY LINE4.
    DISPLAY LINE5.
STOP RUN.
    
000-GUESS-THE-NUM.
    PERFORM 200-GENERATE-RAND-NUM.
    DISPLAY "A random number from 1 - 9 has been generated. You get 3 guesses. Enter your first guess: ".
    PERFORM 100-VALIDATE-GUESS
        UNTIL NUM-GUESSES >=3.
    MOVE 0 TO NUM-GUESSES.
    DISPLAY "Would you like to play Guess the Number again? (Y/N)".
    ACCEPT SWITCH.

100-VALIDATE-GUESS.
    ACCEPT CURRENT-GUESS.
    IF NUM-GUESSES = 2 AND CURRENT-GUESS NOT = RAND-NUM
        DISPLAY WRONG " " RAND-NUM.
        
    IF CURRENT-GUESS = RAND-NUM
        DISPLAY CORRECT
        MOVE 3 TO NUM-GUESSES
    ELSE
        IF (CURRENT-GUESS > RAND-NUM) AND (NUM-GUESSES < 2)
            DISPLAY TOO-HIGH
        ELSE
            IF (CURRENT-GUESS < RAND-NUM) AND (NUM-GUESSES < 2)
                DISPLAY TOO-LOW.
                
    ADD 1 TO NUM-GUESSES.
    IF NUM-GUESSES NOT >= 3
        DISPLAY TRY-AGAIN.
    
200-GENERATE-RAND-NUM.
    COMPUTE GENERATED-NUM = FUNCTION RANDOM.
    COMPUTE RAND-NUM = GENERATED-NUM * 10.