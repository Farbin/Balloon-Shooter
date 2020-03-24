.MODEL SMALL

.STACK 100H

.DATA


;ALL STRINGS TO DRAW

character db " -^-",13
          db "  0",13
          db " /|\-->",13
          db "  |",13
          db " / \",0
          
balloon db " _",13
        db "| |",13
        db "-",13
        db " |",0 

bullet db '-'
        

ScoreShowString db "SCORE : " 
ScoreLeft db 48
ScoreRight db 48 , 'e'
ScoreTotal db 0
scoreX db 1
scoreY db 2

LivesShow db "LIVES REMAINING  : " 
Lives db 53,0
livesX db 55
livesY db 2     
 
GameOverShow db "        _____   ___  ___  ___ _____     _____  _   _  _____ ______ ",13
             db "       |  __ \ / _ \ |  \/  ||  ___|   |  _  || | | ||  ___|| ___ \",13
             db "       | |  \// /_\ \| .  . || |__     | | | || | | || |__  | |_/ /",13
             db "       | | __ |  _  || |\/| ||  __|    | | | || | | ||  __| |    / ",13
             db "       | |_\ \| | | || |  | || |___    \ \_/ /\ \_/ /| |___ | |\ \ ",13
             db "        \____/\_| |_/\_|  |_/\____/     \___/  \___/ \____/ \_| \_|",13
             db "                                                                ",13
             db "                                                                ",13
             db "       ------------------------------------------------------------",13
             
             db "                     YOUR FINAL SCORE IS -  "
             
FinalScoreLeft   db 48
FinalScoreRight  db 48,0
 
gameOverX db 0
gameOverY db 10


titleShow db   "         _____     _ _                _____ _           _          ", 13  
          db   "        | __  |___| | |___ ___ ___   |   __| |_ ___ ___| |_ ___ ___", 13   
          db   "        | __ -| .'| | | . | . |   |  |__   |   | . | . |  _| -_|  _|", 13  
          db   "        |_____|__,|_|_|___|___|_|_|  |_____|_|_|___|___|_| |___|_|",0
titleX db 0
titleY db 0


StartMessage db "---------------------------------------------------------------------------------",13
             db "                      PRESS ANY KEY TO START A NEW GAME",13
             db "----------------------------------------------------------------------------------",0
StartX db 0
StartY db 16


Border db "________________________________________________________________________________",13
       db "                                                                                 ",13
       db "                                                                                 ",13
       db "________________________________________________________________________________",0
       
BorderX db 0
BorderY db 0       



;ALL COLORS 
          
color  db 181 ;purple
color2 db 176 ;black

black         db 176
blue          db 177 
green         db 178
cyan          db 179
red           db 180
magenta       db 181
brown         db 182
white         db 183
gray          db 184
light_blue    db 185
light_green   db 186
light_cyan    db 187
light_red     db 188
light_magenta db 189
yellow        db 190
intense_white db 191


;POSITIONS FOR BALLOON, BULLET AND PLAYER

x     db 0     
y     db 4    
          
posX  db 0
posY  db 4

bulletX db 6
bulletY db 6

balloonX db 20
balloonY db 20

InitialBulletX db 6
InitialBulletY db 6

new_balloon_pos db 40, 30, 35, 40, 30, 45
pos_size db 5
pos_count db 0


;FLAGS

ifShoot db 0
ifMoved db 0
ifCollided db 0
ifEnded db 0



;FOR TIMER 

timer_flag db 0
new_timer_vec   dw  ?,?
old_timer_vec   dw  ?,?


;OTHERS

error db 5  
ten db 10


            
.CODE

MAIN PROC
    initialization:
        MOV AX, @DATA
        MOV DS, AX
    
    CALL set_display_mode
    CALL wait_to_start            
    CALL initialize_all
    CALL set_time
    CALL run_iDraw
    CALL game_over
    
    
    ending :
        MOV AH, 4CH
        INT 21H

MAIN ENDP




;////////////////////////
;ALL INITIAL FUNCTIONS///
;////////////////////////

wait_to_start proc near
    CALL draw_start_message
    CALL draw_title
 
    mov ah, 1
    int 16h
    jnz t1
    
    t1:
        mov ah, 0
        int 16h
                
    finish_wait:
        CALL draw_start_message
        CALL draw_title
        ret
wait_to_start endp

;----------------------------

set_display_mode Proc
  mov  ah,0
  mov  al,12h  ;640 X 480 X 16.
  int  10H
  
  mov ah, 11
  mov bh, 0
  mov bl, black
  int 10h
 
  ret
set_display_mode ENDP

;-----------------------------

initialize_all proc near
    CALL draw_character
    CALL draw_balloon
    CALL draw_score
    CALL draw_lives
    CALL draw_border
    ret
initialize_all endp

;-----------------------------


;///////////////////////////////
;ALL FUNCTIONS FOR TIMER////////
;///////////////////////////////


timer_tick proc
    push ds
    push ax
 
    mov ax,seg timer_flag
    mov ds,ax
    mov timer_flag,1
        
    pop ax
    pop ds
    
    iret 
timer_tick endp

;-----------------------------

set_time proc
     
    push ax
    push cx
    push dx

    MOV new_timer_vec, offset timer_tick
    MOV new_timer_vec+2, CS
    MOV AL, 1CH; interrupt type
    LEA DI, old_timer_vec
    LEA SI, new_timer_vec
    CALL setup_int
        
    pop dx
    pop cx
    pop ax
    
    ret 
    
    
set_time endp

;-----------------------------

setup_int Proc
    
; save old vector and set up new vector
; input: al = interrupt number
;    di = address of buffer for old vector
;    si = address of buffer containing new vector
; save old interrupt vector
    MOV AH, 35h ; get vector
    INT 21h
    MOV [DI], BX    ; save offset
    MOV [DI+2], ES  ; save segment
; setup new vector
    MOV DX, [SI]    ; dx has offset
    PUSH DS     ; save ds
    MOV DS, [SI+2]  ; ds has the segment number
    MOV AH, 25h ; set vector
    INT 21h
    POP DS
    RET
setup_int EndP


;-----------------------------

run_iDraw proc
   
    tt:
    CMP timer_flag, 1
    JNE tt
    MOV timer_flag, 0
    CALL check_user_input
    CALL check_collision
    CALL iDraw
    
    tt2:
    CMP timer_flag, 1
    JNE tt2
    MOV timer_flag, 0
    
    
    CMP ifEnded, 1
    JE end_iDraw
    
    jmp tt
    
    end_iDraw:
    ret
run_iDraw endp

;-----------------------------




;////////////////////////////////////////
;///////ALL DRAWING FUNCTIONS////////////
;////////////////////////////////////////



draw_start_message PROC near

  mov  di, offset startMessage
  mov al, startX
  mov x, al
  mov al, startY
  mov y, al
  
  
  while6:      
      ;SET CURSOR POSITION FOR CURRENT CHAR.  
      mov dl, x
      mov dh, y
      mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
      mov bh, 0 ;PAGE.
      int 10h
  
      mov  al, [ di ]  ;CHAR TO DISPLAY.
      cmp  al, 13    ;IF CHAR == 13
      je   linebreak6 ;THEN JUMP TO LINEBREAK.
      cmp  al, 0   ;IF CHAR == 0
      je   finish6  ;THEN JUMP TO FINISH.
      
      
      mov  ah, 9
      mov  bh, 0
      mov  bl, light_red  
      mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
      int  10h
      
      inc  x  
      jmp  next_char6
  
  linebreak6:  
      inc  y  ;MOVE TO NEXT LINE.    
      mov  x, 0  ;X GOES TO THE LEFT.
  
  next_char6:
      inc  di  
      jmp  while6    

  
  finish6 :
      ret  

draw_start_message endp

;-----------------------------

draw_title PROC near

  mov  di, offset titleShow
  mov al, titleX
  mov x, al
  mov al, titleY
  mov y, al
  
  
  while7:      
      ;SET CURSOR POSITION FOR CURRENT CHAR.  
      mov dl, x
      mov dh, y
      mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
      mov bh, 0 ;PAGE.
      int 10h
  
      mov  al, [ di ]  ;CHAR TO DISPLAY.
      cmp  al, 13    ;IF CHAR == 13
      je   linebreak7 ;THEN JUMP TO LINEBREAK.
      cmp  al, 0   ;IF CHAR == 0
      je   finish7  ;THEN JUMP TO FINISH.
      
      
      mov  ah, 9
      mov  bh, 0
      mov  bl, light_red
      mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
      int  10h
      
      inc  x  
      jmp  next_char7
  
      linebreak7:  
      inc  y  ;MOVE TO NEXT LINE.    
      mov  x, 0  ;X GOES TO THE LEFT.
  
      next_char7:
      inc  di  
      jmp  while6    

  
      finish7 :
      ret  

draw_title endp

;-----------------------------


draw_character PROC near

  mov  di, offset character
  mov al, posX
  mov x, al
  mov al, posY
  mov y, al
  
  
  while1:      
  ;SET CURSOR POSITION FOR CURRENT CHAR.  
  mov dl, x
  mov dh, y
  mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
  mov bh, 0 ;PAGE.
  int 10h
  
  mov  al, [ di ]  ;CHAR TO DISPLAY.
  cmp  al, 13    ;IF CHAR == 13
  je   linebreak ;THEN JUMP TO LINEBREAK.
  cmp  al, 0   ;IF CHAR == 0
  je   finish  ;THEN JUMP TO FINISH.

  mov  ah, 9
  mov  bh, 0
  mov  bl, light_blue
  mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
  int  10h
  
  inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
  jmp  next_char
linebreak:  
  inc  y  ;MOVE TO NEXT LINE.    
  mov  x, 0  ;X GOES TO THE LEFT.
next_char:
  inc  di  
  jmp  while1    

  
finish :
    ret  

draw_character endp

;-----------------------------

draw_border PROC near

  mov  di, offset Border
  mov al, BorderX
  mov x, al
  mov al, BorderY
  mov y, al
  
  
  while8:      
      ;SET CURSOR POSITION FOR CURRENT CHAR.  
      mov dl, x
      mov dh, y
      mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
      mov bh, 0 ;PAGE.
      int 10h
  
      mov  al, [ di ]  ;CHAR TO DISPLAY.
      cmp  al, 13    ;IF CHAR == 13
      je   linebreak8 ;THEN JUMP TO LINEBREAK.
      cmp  al, 0   ;IF CHAR == 0
      je   finish8  ;THEN JUMP TO FINISH.
      
      
      mov  ah, 9
      mov  bh, 0
      mov  bl, yellow  
      mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
      int  10h
      
      inc  x  
      jmp  next_char8
  
      linebreak8:  
      inc  y  ;MOVE TO NEXT LINE.    
      mov  x, 0  ;X GOES TO THE LEFT.
  
      next_char8:
      inc  di  
      jmp  while8    

  
      finish8 :
      ret  

draw_border endp

;-----------------------------




draw_score proc near
  mov  di, offset ScoreShowString
  mov al, scoreX
  mov x, al
  mov al, scoreY
  mov y, al
  
  
  while3:      
  ;SET CURSOR POSITION FOR CURRENT CHAR.  
  mov dl, x
  mov dh, y
  mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
  mov bh, 0 ;PAGE.
  int 10h
  
  mov  al, [ di ]  ;CHAR TO DISPLAY.
  cmp  al, 13    ;IF CHAR == 13
  je   linebreak3 ;THEN JUMP TO LINEBREAK.
  cmp  al, 'e'   ;IF CHAR == 0
  je   finish3  ;THEN JUMP TO FINISH.
  
  
  
  mov  ah, 9
  mov  bh, 0
  mov  bl, light_red  ;ANY COLOR.
  mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
  int  10h
  
  inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
  jmp  next_char3
 linebreak3:  
  inc  y  ;MOVE TO NEXT LINE.    
  mov  x, 0  ;X GOES TO THE LEFT.
 next_char3:
  inc  di  
  jmp  while3    

  
finish3 :
    ret  

draw_score endp

;-----------------------------

draw_lives proc near
  push ax
  push bx
  push cx
  push dx

  
  mov  di, offset LivesShow
  mov al, livesX
  mov x, al
  mov al, livesY
  mov y, al
  
  
  while4:      
  ;SET CURSOR POSITION FOR CURRENT CHAR.  
  mov dl, x
  mov dh, y
  mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
  mov bh, 0 ;PAGE.
  int 10h
  
  mov  al, [ di ]  ;CHAR TO DISPLAY.
  cmp  al, 13    ;IF CHAR == 13
  je   linebreak4 ;THEN JUMP TO LINEBREAK.
  cmp  al, 0   
  je   finish4  ;JUMP TO FINISH.
 
    
  mov  ah, 9
  mov  bh, 0
  mov  bl, light_red 
  mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
  int  10h
  
  inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
  jmp  next_char4
  linebreak4:  
  inc  y  ;MOVE TO NEXT LINE.    
  mov  x, 0  ;X GOES TO THE LEFT.
  next_char4:
  inc  di  
  jmp  while4    

  
  finish4 :
    pop dx
    pop cx
    pop bx
    pop ax
    ret  

draw_lives endp

;-----------------------------

draw_game_over proc near

  push ax
  push bx
  push cx
  push dx


  mov  di, offset GameOverShow
  mov al, gameOverX
  mov x, al
  mov al, gameOverY
  mov y, al
  
  
  while5:      
  ;SET CURSOR POSITION FOR CURRENT CHAR.  
  mov dl, x
  mov dh, y
  mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
  mov bh, 0 ;PAGE.
  int 10h
  
  mov  al, [ di ]  ;CHAR TO DISPLAY.
  cmp  al, 13    ;IF CHAR == 13
  je   linebreak5 ;THEN JUMP TO LINEBREAK.
  cmp  al, 0   ;IF CHAR == 0
  je   finish5  ;THEN JUMP TO FINISH.
  
  
  mov  ah, 9
  mov  bh, 0
  mov  bl, light_red
  mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
  int  10h
  
  inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
  jmp  next_char5
  linebreak5:  
  inc  y  ;MOVE TO NEXT LINE.    
  mov  x, 0  ;X GOES TO THE LEFT.
  next_char5:
  inc  di  
  jmp  while5    

  
  finish5 :
    pop dx
    pop cx
    pop bx
    pop ax
    ret  

draw_game_over endp


;-----------------------------

draw_balloon PROC near

  mov  di, offset balloon
  mov al, balloonX
  mov x, al
  mov al, balloonY
  mov y, al
  
  
  while2:      
  ;SET CURSOR POSITION FOR CURRENT CHAR.  
  mov dl, x
  mov dh, y
  mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
  mov bh, 0 ;PAGE.
  int 10h
  
  mov  al, [ di ]  ;CHAR TO DISPLAY.
  cmp  al, 13    ;IF CHAR == 13
  je   linebreak1 ;THEN JUMP TO LINEBREAK.
  cmp  al, 0   ;IF CHAR == 0
  je   finish1  ;THEN JUMP TO FINISH.
    
  
  mov  ah, 9
  mov  bh, 0
  mov  bl, light_magenta
  mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
  int  10h
  
  inc  x  ;NEXT CHARACTER GOES TO THE RIGHT.
  jmp  next_char1
  
  linebreak1:  
  inc  y  ;MOVE TO NEXT LINE.
  dec x
  dec x

  next_char1:
  inc  di  
  jmp  while2    


  
  finish1 :
    ret  

draw_balloon endp




;---------------------------------------

draw_bullet proc
    push ax
    push cx
    push dx
    
    mov di,offset bullet

    mov dl, bulletX
    mov dh, bulletY
    mov ah, 2 ;SERVICE TO SET CURSOR POSITION.
    mov bh, 0 ;PAGE.
    int 10h
  
  
    mov  al, [ di ]
  
    mov  ah, 9
    mov  bh, 0
    mov  bl, red
    mov  cx, 1  ;HOW MANY TIMES TO DISPLAY CHAR.
    int  10h
     
    pop dx
    pop cx
    pop ax
    ret
draw_bullet endp

;-----------------------------




;/////////////////////////////////////////
;ALL FUNCTIONS TO MOVE THINGS/////////////
;/////////////////////////////////////////


move_character proc near
        mov ah, 0
        int 16h
            
        cmp ah,48h ;go to upKey if up button is pressed
        je up_key
        
        cmp ah, 50h ;go to downKey if down button is pressed
        je down_key
        jmp end_move_character
        up_key:
            CALL move_character_up
            jmp end_move_character
        down_key: 
            CALL move_character_down

    end_move_character:
        mov ifMoved, 0
        ret
move_character endp

;-----------------------------

move_character_up proc near
    mov al,posY
    cmp al,4
    JLE endmov_up
    
    dec InitialBulletY
    CALL draw_character
    dec posY
    CALL draw_character
    
    endmov_up:
    ret
move_character_up endp

;-----------------------------

move_character_down proc near  
   mov al,posY
   cmp al,26
   JGE endmov_down
   
   inc InitialBulletY
   CALL draw_character
   inc posY
   CALL draw_character
   
   endmov_down:
   mov InitialBulletX, 6
   ret
move_character_down endp


;-----------------------------

bullet_sound proc near

   PUSH    AX
  PUSH    BX
  PUSH    CX
  PUSH    DX
  PUSH    DI
  MOV AL, 0B6H
  OUT 43H, AL
  MOV DX, 14H
  MOV AX, 4F38H
  DIV DI
  OUT 42H, AL
  MOV AL, AH
  OUT 42H, AL
  IN  AL, 61H
  MOV AH, AL
  OR  AL, 3
  OUT 61H, AL
L1: MOV CX, 6801
L2: LOOP    L2
  DEC BX
  JNZ L1
  MOV AL, AH
  OUT 61H, AL
  POP DI
  POP DX
  POP CX
  POP BX
  POP AX
  
   ret
bullet_sound endp

;-----------------------------


move_bullet proc
    call draw_bullet
    inc bulletX
    inc bulletX
    
    cmp bulletX, 75
    JNG call_draw_bullet
    
    mov ifShoot,0
    
    cmp ifCollided,0
    JNE after_collide_check
    
    ;player has missed a shot
    CMP lives, 48
    JNE decrease_life 
    
    ;game over
    mov ifEnded, 1
    ret
    
    decrease_life:
    
    CALL draw_lives
    dec lives    
    CALL draw_lives
    
    

    after_collide_check:
    
    
    mov ifCollided, 0
    push ax
    mov al,InitialBulletY
    mov bulletY,al
    mov al,InitialBulletX
    mov bulletX,al
    pop ax   
    
    jmp return
    
    call_draw_bullet:
    call draw_bullet
    
    return:
    ret
move_bullet endp

;-------------------------------

moveBalloon proc near
    CALL draw_balloon
    dec balloonY
    
    cmp balloonY, 4
    jne end_move_balloon
    
    loopBalloon:
        mov balloonY, 28
        
    end_move_balloon:
        CALL draw_balloon
        ret
moveBalloon endp

;--------------------------------

change_balloon_index proc near
    push ax
    push bx    
    push si
    CALL draw_balloon
    
 
    inc pos_count
    mov bl, pos_count
    mov bh, 0
    mov si,offset new_balloon_pos
    
    mov al, pos_count
    cmp al, pos_size
    jne do_work
    

    mov pos_count, 0
    
    do_work:
    mov ax, [si + bx] 
    mov ah, 0
    mov balloonX, al 
    mov balloonY, 50
    
    CALL draw_balloon

    end_change_balloon_index:
        pop si
        pop bx
        pop ax
        
        ret
    
change_balloon_index endp

;--------------------------------


;////////////////////////////////////////////////////////////////
;//////////FUNCTIONS THAT ARE CALLED FROM TIMER//////////////////
;////////////////////////////////////////////////////////////////


check_user_input proc near
        mov ah, 1
        int 16h
        jnz pressed_key
        
        mov ax, 3
        int 33h
        cmp bx, 1  ; mouse clicked
        je  set_shoot_flag
        jmp finish_check_user_input        
        
        set_shoot_flag:
           CALL bullet_sound
           CALL shoot
           
           mov ifShoot, 1                     
           jmp finish_check_user_input
        
        pressed_key:
            mov ifMoved, 1

    finish_check_user_input:
        ret
check_user_input endp

;------------------------------------------------

check_collision proc near
    push ax
    push dx
    cmp ifShoot, 1
    jne end_check_collision

    
    
    mov al, balloonX
    sub al, bulletX
    
    cmp al, error
    jnb end_check_collision
    
    
    mov al,bulletX
    sub al,balloonX
    cmp al, error
    jb end_check_collision
    
    mov al, balloonY
    sub al, bulletY
    
    cmp al, error
    jnb end_check_collision
    
    mov al,bulletX
    sub al,balloonX
    cmp al, error
    jb end_check_collision
    
        
    mov ifCollided, 1
    CALL draw_score
    inc ScoreTotal
    CALL set_score
    CALL draw_score
    
    CALL change_balloon_index
    
    mov ah, 2
    mov dl, '4'
    int 21h   
    
    end_check_collision: 
        pop dx
        pop ax
        ret

check_collision endp

;-----------------------------------------------

iDraw proc near
    
    ;all animations are called from here
    
    CALL moveBalloon
    
    call_move_bullet:
        CMP ifShoot, 1
        JNE  call_move_character   
        CALL move_bullet
       
    call_move_character:
        CMP ifMoved, 1
        JNE  finish_iDraw
        CALL move_character    
    
        
    finish_iDraw:    
    ret
iDraw endp




;/////////////////////////////////////////////
;//////SHOOTING, SCORING, GAME OVER///////////
;/////////////////////////////////////////////

shoot proc near
    cmp ifShoot, 1
    je end_shoot
        
    push ax
    
    mov al, posY
    add al, 2
    mov bulletY, al
    
    pop ax
    
    CALL draw_bullet
    end_shoot:
    ret

shoot endp

;--------------------------------

set_score proc near
    
    push ax
    
    xor ax, ax
    mov al, ScoreTotal
    div ten
    mov ScoreLeft, al
    add ScoreLeft, '0'
    
    mov ScoreRight, ah
    add ScoreRight, '0'
        
    pop ax
    ret
set_score endp

;------------------------------
set_final_score proc
   push ax
   mov al,ScoreLeft
   mov FinalScoreLeft,al
   
   mov al,ScoreRight
   mov FinalScoreRight,al
   
   pop ax

   ret
set_final_score endp

;------------------------------

game_over proc near
    CALL initialize_all
    
    CALL set_final_score
    
    CALL draw_game_over
    ret
game_over endp

;-----------------------------





END MAIN    