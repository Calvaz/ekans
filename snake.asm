
org 0x7c00

; constants
; A0000h
; Variables after screen memory
sprites equ 0FA00h
seed equ 0FA00h
playerDir equ 0FA12h
playerLength equ 0FA14h
playerBody equ 0FA15h


SNAKE_COLOR equ 02h
SEED_COLOR equ 04h
SCREEN_W equ 320
SCREEN_H equ 200
DIR_UP equ -SCREEN_W
DIR_RIGHT equ 1
DIR_DOWN equ SCREEN_W
DIR_LEFT equ -1
SNAKE_PIX_W equ 3
SNAKE_PIX_H equ 3
SCALE_BY equ SNAKE_PIX_W*SNAKE_PIX_H


; Setup VGA video mode
mov ax, 0013h
int 10h

push 0A000h
pop es


mov di, sprites
mov si, sprite_bitmaps
mov cl, 40
rep movsd
movsb

push es   ; es:di
pop ds    ; ds:si

call random_seed_pos

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

        mov cx, [si]
        jz move_dir

        xor ah, ah
        int 16h
        
        cmp al, 'k'
        je .move_up

        cmp al, 'l'
        je .move_right

        cmp al, 'j'
        je .move_down

        cmp al, 'h'
        je .move_left

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
            inc ax
            cmp ax, [si]
            je move_dir
            mov [si], cx

        move_dir:
            mov cx, [si]

            mov bl, [si+2]      ; playerLength
            shl bl, 1
            mov si, playerBody
            .move_body:
                mov ax, [si+bx-2-SCALE_BY]
                mov [si+bx-2], ax
                sub bl, 2
                cmp bl, SCALE_BY
                jg .move_body

            .move_head:
                ;cmp cx, DIR_UP
                ;je .adjust_position

                ;.adjust_position:
                    

                
                add [si+bx-2], cx
                sub bl, 2
                cmp bl, 0
                jg .move_head


    
    ; draw player
    xor ah, ah
    mov bl, [si-1]
    shl bl, 1
    mov al, SNAKE_COLOR

    draw_body:
        mov cx, [si+bx-2]   ; column = row + colNum
        mov di, cx
        stosb
        
        sub bl, 2
        cmp bl, 0
        jg draw_body

    mov bl, 0
    mov si, seed
    mov al, SEED_COLOR
    draw_seed:
        mov di, [si+bx]
        stosb

        add bl, 2
        cmp bl, 18
        jl draw_seed

    ; spawn snake
    ; spawn seed
    ; save snake last pixel
    ; if seed hit then highlight last pixel
    ; spawn seed
    
    mov bp, 40
    mov si, 40
    delay2:
        dec bp
        nop
        jnz delay2
        dec si
        cmp si, 0
        jnz delay2

    jmp game_loop

game_over:
    cli
    hlt

random_seed_pos:
    push si
    rdtsc
    xor dx, dx
    mov cx, SCREEN_W*SCREEN_H - 1 + 2
    div cx
    mov ax, dx
    add ax, 2

    mov si, seed
    mov cl, 0
    inc ax
    .expand_seed:

        ;; TODO: TEST IF AX < 0 or AX > height
        mov bl, 0
        .fill_line:
            mov word [si], ax
            add si, 2
            dec ax
            
            inc bl
            cmp bl, 3
            jl .fill_line
        
        add ax, SCREEN_W
        add ax, 3
        inc cl
        cmp cl, 3
        jl .expand_seed
            
    pop si
    ret
    

;; DATA
sprite_bitmaps:
    dw 0 ;; seeds position
    dw 0
    dw 0
    dw 0
    dw 0
    dw 0
    dw 0
    dw 0
    dw 0

    dw DIR_RIGHT      ;; player dir
    db 54             ;; player length

    ;; snake positions
    dw 100*SCREEN_W+160    ;; head row * column
    dw 101*SCREEN_W+160    ;; head row + 1 * column
    dw 102*SCREEN_W+160    ;; head row + 2 * column
    dw 100*SCREEN_W+159
    dw 101*SCREEN_W+159
    dw 102*SCREEN_W+159
    dw 100*SCREEN_W+158
    dw 101*SCREEN_W+158
    dw 102*SCREEN_W+158
    dw 100*SCREEN_W+157
    dw 101*SCREEN_W+157
    dw 102*SCREEN_W+157
    dw 100*SCREEN_W+156
    dw 101*SCREEN_W+156
    dw 102*SCREEN_W+156
    dw 100*SCREEN_W+155
    dw 101*SCREEN_W+155
    dw 102*SCREEN_W+155
    dw 100*SCREEN_W+154
    dw 101*SCREEN_W+154
    dw 102*SCREEN_W+154
    dw 100*SCREEN_W+153
    dw 101*SCREEN_W+153
    dw 102*SCREEN_W+153
    dw 100*SCREEN_W+152
    dw 101*SCREEN_W+152
    dw 102*SCREEN_W+152
    dw 100*SCREEN_W+151    ;; head row * column
    dw 101*SCREEN_W+151    ;; head row + 1 * column
    dw 102*SCREEN_W+151    ;; head row + 1 * column
    dw 100*SCREEN_W+150
    dw 101*SCREEN_W+150
    dw 102*SCREEN_W+150
    dw 100*SCREEN_W+149
    dw 101*SCREEN_W+149
    dw 102*SCREEN_W+149
    dw 100*SCREEN_W+148
    dw 101*SCREEN_W+148
    dw 102*SCREEN_W+148
    dw 100*SCREEN_W+147
    dw 101*SCREEN_W+147
    dw 102*SCREEN_W+147
    dw 100*SCREEN_W+146
    dw 101*SCREEN_W+146
    dw 102*SCREEN_W+146
    dw 100*SCREEN_W+145
    dw 101*SCREEN_W+145
    dw 102*SCREEN_W+145
    dw 100*SCREEN_W+144
    dw 101*SCREEN_W+144
    dw 102*SCREEN_W+144
    dw 100*SCREEN_W+143
    dw 101*SCREEN_W+143
    dw 102*SCREEN_W+143
    

times 510 -($-$$) db 0
db 0x55
db 0xaa
