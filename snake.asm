
org 0x7c00

; constants
; A0000h
; Variables after screen memory
sprites equ 0FA00h
seed equ 0FA00h
playerDir equ 0FA02h
playerLength equ 0FA04h
playerBody equ 0FA05h
; playerPos 2b


SNAKE_COLOR equ 02h
SEED_COLOR equ 04h
SCREEN_W equ 300
SCREEN_H equ 200
DIR_UP equ -SCREEN_W
DIR_RIGHT equ 1
DIR_DOWN equ SCREEN_W
DIR_LEFT equ -1


; Setup VGA video mode
mov ax, 0013h
int 10h

push 0A000h
pop es


mov di, sprites
mov si, sprite_bitmaps
mov cl, 10
rep movsd
movsb

push es   ; es:di
pop ds    ; ds:si

game_loop:
    ; color the background
    xor ax, ax
    xor bx, bx
    xor di, di
    mov cx, SCREEN_W*SCREEN_H
    rep stosb

    get_input:
        mov si, playerDir
        mov ah, 01h
        int 16h
        
        test al, 48h
        jz .move_up

        test al, 4Dh
        jz .move_right

        test al, 50h
        jz .move_down

        test al, 4Bh
        jz .move_left

        mov cx, [si]
        jmp move_dir

        .move_up:
            mov cx, DIR_UP
            jmp .test_opposite_direction

        .move_right:
            mov cx, DIR_RIGHT
            jmp .test_opposite_direction

        .move_down:
            mov cx, DIR_DOWN
            jmp .test_opposite_direction
            
        .move_left:
            mov cx, DIR_LEFT
            jmp .test_opposite_direction

         ;; can't move to opposite direction of head
        .test_opposite_direction:
            mov ax, cx
            xor ax, 0FFFFh
            add ax, 1
            test ax, [si]
            jz move_dir

            mov [si], cx

        move_dir:
            mov bl, [si+2]     ; playerLength
            add bl, bl

            .move_body:
                xor dx, dx
                mov ax, cx
                add ax, [si+3+bx-2]
                mov [si+3+bx-2], ax
                
                ;div SCREEN_W            ; do not go down 1 line but stay on same line
                ;test ax, 0

                sub bl, 2
                cmp bl, 0
                jnz .move_body
    
    ; draw player
    xor bl, bl
    mov si, playerBody
    mov bl, [si-1]
    add bl, bl

    draw_body:
        mov al, SNAKE_COLOR
        mov cx, word [si+bx-2]   ; column = row + colNum
        mov di, cx
        stosb
        
        sub bl, 2
        cmp bl, 0
        jg draw_body

    ; spawn snake
    ; spawn seed
    ; move snake
    ; save snake last pixel
    ; if seed hit then highlight last pixel
    ; spawn seed
    
    mov bp, 4369
    mov si, 4369
    delay2:
        dec bp
        nop
        jnz delay2
        dec si
        cmp si,0    
        jnz delay2
            
        ;mov ax, [046Ch]
        ;add ax, 1
        ;.delay:
        ;    cmp ax, [046Ch]
        ;    jl .delay

    jmp game_loop

game_over:
    cli
    hlt

;; DATA
sprite_bitmaps:
    db 0b00011000 ;; seeds bitmap
    db 0b00011000

    dw DIR_RIGHT      ;; player dir
    db 18             ;; player length
    dw 100*320+160    ;; head row * column
    dw 100*320+159
    dw 100*320+158
    dw 100*320+157
    dw 100*320+156
    dw 100*320+155
    dw 100*320+154
    dw 100*320+153
    dw 100*320+152
    dw 101*320+160    ;; head row + 1 * column
    dw 101*320+159
    dw 101*320+158
    dw 101*320+157
    dw 101*320+156
    dw 101*320+155
    dw 101*320+154
    dw 101*320+153
    dw 101*320+152
    

times 510 -($-$$) db 0
db 0x55
db 0xaa
