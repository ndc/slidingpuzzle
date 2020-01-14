DECLARE SUB clearing ()
DECLARE SUB redraw ()
DECLARE SUB configure ()
DECLARE SUB scramble ()
DECLARE SUB moving (dir AS SINGLE)
DECLARE SUB resetting ()
COMMON SHARED row, column, pulloutx, pullouty, head$, plane$, info$
COMMON SHARED sign() AS STRING * 1, real() AS STRING * 1
COMMON SHARED nowx, nowy, starting, ending
DATA "A","B","C","D","E","F","G","H","I","J","K","L","M"
DATA "N","O","P","Q","R","S","T","U","V","W","X","Y","Z"
DATA "a","b","c","d","e","f","g","h","i","j","k","l","m"
DATA "n","o","p","q","r","s","t","u","v","w","x","y","z"
DATA "0","1","2","3","4","5","6","7","8","9","-","="
RANDOMIZE TIMER
SCREEN 8: COLOR 6, 7
LOCATE 5, 18: PRINT " �  � � ��  ��� ��    ��  � � ��� ��� �   ���"
LOCATE 6, 18: PRINT "� � � � � � �   � �   � � � �   �   � �   �"
LOCATE 7, 18: PRINT "�   � � � � �   � �   � � � �   �   � �   �"
LOCATE 8, 18: PRINT " �  � � ��  ��  ��    ��  � �  �   �  �   ��"
LOCATE 9, 18: PRINT "  � � � �   �   � �   �   � � �   �   �   �"
LOCATE 10, 18: PRINT "� � � � �   �   � �   �   � � �   �   �   �"
LOCATE 11, 18: PRINT " �   �  �   ��� � �   �    �  ��� ��� ��� ���"
LOCATE 13, 35: PRINT "version 1.0"
LOCATE 20, 12: PRINT "If you have any suggestion, mail it to me:"
LOCATE 21, 15: PRINT "Endy"
LOCATE 22, 15: PRINT "Jl. ------- no. - , ----------"
LOCATE 23, 15: PRINT "------ -----": COLOR 1
DO: LOOP WHILE INKEY$ = "": CLS
row = 4: column = 16: pulloutx = row: pullouty = column
head$ = "ul": plane$ = "h": info$ = "y"
nowx = pulloutx: nowy = pullouty
DIM sign(1 TO row, 1 TO column) AS STRING * 1
DIM real(1 TO row, 1 TO column) AS STRING * 1
RESTORE: FOR a = 1 TO row: FOR b = 1 TO column
READ real(a, b): sign(a, b) = real(a, b)
NEXT: NEXT
sign(pulloutx, pullouty) = "�": real(pulloutx, pullouty) = "�"
CALL redraw
DO: SELECT CASE INKEY$
 CASE "c"
  CALL configure
 CASE "s"
  COLOR 10: LOCATE 5, 50: PRINT "S   - Scramble": COLOR 1
  CALL scramble
  COLOR 8: LOCATE 5, 50: PRINT "S   - Scramble": COLOR 1
 CASE "r"
  CALL resetting
 CASE CHR$(27)
  END
 CASE CHR$(0) + CHR$(72)
  CALL moving(72)
 CASE CHR$(0) + CHR$(75)
  CALL moving(75)
 CASE CHR$(0) + CHR$(77)
  CALL moving(77)
 CASE CHR$(0) + CHR$(80)
  CALL moving(80)
END SELECT: LOOP

SUB clearing
FOR a = 13 TO 23: LOCATE a, 50
PRINT STRING$(29, " "): NEXT
END SUB

SUB configure
COLOR 6
LOCATE 14, 50: PRINT "Do you want random"
LOCATE 15, 50: INPUT "configuration (y/n):", random$
IF LCASE$(LEFT$(random$, 1)) = "y" THEN
 LOCATE 16, 50: INPUT "Including the size (y/n) : ", random$
 IF LCASE$(LEFT$(random$, 1)) = "y" THEN
n: row = INT(9 * RND) + 3: column = INT(21 * RND) + 3
   IF row * column > 64 THEN GOTO n
   GOTO m
 END IF
j: CALL clearing: LOCATE 14, 50: PRINT "rows * columns <= 64"
 LOCATE 15, 50: INPUT "rows (1-11)   : ", row: row = INT(row)
 IF row >= 1 AND row <= 11 THEN
 REM
 ELSE
 BEEP: GOTO j
 END IF
 LOCATE 16, 50: INPUT "columns (1-23): ", column: column = INT(column)
 IF column >= 1 AND column <= 23 THEN
 REM
 ELSE
 BEEP: GOTO j
 END IF
 IF row * column > 64 THEN
 BEEP: GOTO j
 END IF
m: pulloutx = INT(RND * row) + 1: pullouty = INT(RND * column) + 1
 head = INT(RND * 4)
 SELECT CASE head
  CASE 0
  head$ = "ul"
  CASE 1
  head$ = "ur"
  CASE 2
  head$ = "ll"
  CASE 3
  head$ = "lr"
 END SELECT
 plane = INT(RND * 2)
 SELECT CASE plane
  CASE 0
  plane$ = "h"
  CASE 1
  plane$ = "v"
 END SELECT: GOTO f
END IF
b: CALL clearing: LOCATE 14, 50: PRINT "rows * columns <= 64"
LOCATE 15, 50: INPUT "rows (1-11)   : ", row: row = INT(row)
IF row >= 1 AND row <= 11 THEN
REM
ELSE
BEEP: GOTO b
END IF
LOCATE 16, 50: INPUT "columns (1-23): ", column: column = INT(column)
IF column >= 1 AND column <= 23 THEN
REM
ELSE
BEEP: GOTO b
END IF
IF row * column > 64 THEN
BEEP: GOTO b
END IF
c: CALL clearing: LOCATE 14, 50: PRINT "Hole coordinate"
LOCATE 15, 50: PRINT "row (1 - "; row; ") : "; : INPUT "", pulloutx
pulloutx = INT(pulloutx)
IF pulloutx <= row AND pulloutx >= 1 THEN
REM
ELSE
BEEP: GOTO c
END IF
LOCATE 16, 50: PRINT "column (1 - "; column; ") : "; : INPUT "", pullouty
pullouty = INT(pullouty)
IF pullouty >= 1 AND pullouty <= column THEN
REM
ELSE
BEEP: GOTO c
END IF
d: CALL clearing: LOCATE 14, 50: PRINT "Place of 'A' (ul, ur,"
LOCATE 15, 50: INPUT "ll, lr) : ", head$
IF head$ <> "ul" AND head$ <> "ur" AND head$ <> "ll" AND head$ <> "lr" THEN
BEEP: GOTO d
END IF
e: CALL clearing: LOCATE 14, 50: INPUT "Plane (h/v):", plane$
IF plane$ <> "h" AND plane$ <> "v" THEN
BEEP: GOTO e
END IF
f: RESTORE
REDIM real(1 TO row, 1 TO column) AS STRING * 1
REDIM sign(1 TO row, 1 TO column) AS STRING * 1
SELECT CASE head$
CASE "ul"
 IF plane$ = "h" THEN
 FOR a = 1 TO row: FOR b = 1 TO column: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
 IF plane$ = "v" THEN
 FOR b = 1 TO column: FOR a = 1 TO row: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
CASE "ur"
 IF plane$ = "h" THEN
 FOR a = 1 TO row: FOR b = column TO 1 STEP -1: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
 IF plane$ = "v" THEN
 FOR b = column TO 1 STEP -1: FOR a = 1 TO row: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
CASE "ll"
 IF plane$ = "h" THEN
 FOR a = row TO 1 STEP -1: FOR b = 1 TO column: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
 IF plane$ = "v" THEN
 FOR b = 1 TO column: FOR a = row TO 1 STEP -1: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
CASE "lr"
 IF plane$ = "h" THEN
 FOR a = row TO 1 STEP -1: FOR b = column TO 1 STEP -1: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
 IF plane$ = "v" THEN
 FOR b = column TO 1 STEP -1: FOR a = row TO 1 STEP -1: READ real(a, b)
 sign(a, b) = real(a, b): NEXT: NEXT
 END IF
END SELECT
real(pulloutx, pullouty) = "�": sign(pulloutx, pullouty) = "�"
nowx = pulloutx: nowy = pullouty
g: CALL clearing: LOCATE 14, 50: PRINT "Do you want these informations"
LOCATE 15, 50: INPUT "appear on screen (y/n) : ", info$
IF info$ <> "y" AND info$ <> "n" THEN
BEEP: GOTO g
END IF
COLOR 1
CALL redraw
END SUB

SUB moving (dir AS SINGLE)
COLOR 1
SELECT CASE dir
 CASE 72
 IF nowx = 1 THEN
 PLAY "o6 l64 c": EXIT SUB
 END IF
 sign(nowx, nowy) = sign(nowx - 1, nowy)
 LOCATE nowx * 2, nowy * 2: PRINT sign(nowx - 1, nowy)
 nowx = nowx - 1
 CASE 77
 IF nowy = column THEN
 PLAY "o6 l64 c": EXIT SUB
 END IF
 sign(nowx, nowy) = sign(nowx, nowy + 1)
 LOCATE nowx * 2, nowy * 2: PRINT sign(nowx, nowy + 1)
 nowy = nowy + 1
 CASE 80
 IF nowx = row THEN
 PLAY "o6 l64 c": EXIT SUB
 END IF
 sign(nowx, nowy) = sign(nowx + 1, nowy)
 LOCATE nowx * 2, nowy * 2: PRINT sign(nowx + 1, nowy)
 nowx = nowx + 1
 CASE 75
 IF nowy = 1 THEN
 PLAY "o6 l64 c": EXIT SUB
 END IF
 sign(nowx, nowy) = sign(nowx, nowy - 1)
 LOCATE nowx * 2, nowy * 2: PRINT sign(nowx, nowy - 1)
 nowy = nowy - 1
END SELECT
LOCATE nowx * 2, nowy * 2: COLOR 14: PRINT "�": COLOR 1
sign(nowx, nowy) = "�": PLAY "o0 l64 c"
IF nowx = pulloutx AND nowy = pullouty THEN
 FOR a = 1 TO row: FOR b = 1 TO column
 IF sign(a, b) <> real(a, b) THEN EXIT SUB
 NEXT: NEXT
 LOCATE row, column: COLOR 11
 PRINT "Puzzle Solved !": COLOR 1
 PLAY "l16 o1 ceg > c": ending = TIMER
 IF starting <> 0 THEN
 COLOR 4: LOCATE 23, 50: PRINT ending - starting; "sec.": COLOR 1
 END IF
 DO: LOOP WHILE INKEY$ = ""
 CALL resetting
END IF
END SUB

SUB redraw
CLS
FOR a = 1 TO row: FOR b = 1 TO column
LOCATE a * 2, b * 2: PRINT sign(a, b)
NEXT: NEXT
LOCATE pulloutx * 2, 2 * pullouty: COLOR 14: PRINT "�": COLOR 5
FOR a = 0 TO column * 16 STEP 16: LINE (a + 2, 4)-(a + 2, row * 16 + 4): NEXT
FOR b = 0 TO row * 16 STEP 16: LINE (2, b + 4)-(2 + column * 16, b + 4): NEXT
LINE (384, 0)-(384, 480): COLOR 8
LOCATE 2, 50: PRINT "Keys : "
LOCATE 4, 50: PRINT "C   - Configure"
LOCATE 5, 50: PRINT "S   - Scramble"
LOCATE 6, 50: PRINT "R   - Reset puzzle"
LOCATE 7, 50: PRINT "ESC - quit the game"
LOCATE 8, 50: PRINT "arrows - move the tiles"
IF info$ = "y" THEN
COLOR 9
LOCATE 10, 50: PRINT "Size : "; row; " x "; column
LOCATE 11, 50: PRINT "'A' at "; head$; " corner, "
LOCATE 12, 50: PRINT plane$; " arrangement"
END IF
COLOR 1
END SUB

SUB resetting
FOR a = 1 TO row: FOR b = 1 TO column: sign(a, b) = real(a, b): NEXT: NEXT
nowx = pulloutx: nowy = pullouty
starting = 0: ending = 0
CALL redraw
END SUB

SUB scramble
FOR a = 1 TO 7 * row * column
movement = INT(RND * 4): SELECT CASE movement
 CASE 0
 IF nowx = 1 THEN
 a = a - 1: GOTO i
 END IF
 sign(nowx, nowy) = sign(nowx - 1, nowy)
 nowx = nowx - 1
 CASE 1
 IF nowy = column THEN
 a = a - 1: GOTO i
 END IF
 sign(nowx, nowy) = sign(nowx, nowy + 1)
 nowy = nowy + 1
 CASE 2
 IF nowx = row THEN
 a = a - 1: GOTO i
 END IF
 sign(nowx, nowy) = sign(nowx + 1, nowy)
 nowx = nowx + 1
 CASE 3
 IF nowy = 1 THEN
 a = a - 1: GOTO i
 END IF
 sign(nowx, nowy) = sign(nowx, nowy - 1)
 nowy = nowy - 1
END SELECT: sign(nowx, nowy) = "�"
i: NEXT
FOR a = 1 TO row: FOR b = 1 TO column
LOCATE a * 2, b * 2: PRINT sign(a, b)
NEXT: NEXT
COLOR 14, 7: LOCATE nowx * 2, 2 * nowy: PRINT "�": COLOR 1
starting = TIMER
END SUB

