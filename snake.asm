
org 0x7c00

; constants
; A0000h
; Variables after screen memory
sprites equ 0FA00h
score equ 0FA00h
letters equ 0FA01h
letter_g equ 0FA01h
letter_a equ 0FA07h
letter_m equ 0FA0Dh
letter_e equ 0FA13h
letter_o equ 0FA19h
letter_v equ 0FA1Fh
letter_r equ 0FA25h
seed equ 0FA2Bh
playerDir equ 0FA3Dh
playerLength equ 0FA3Fh
playerBody equ 0FA40h


SNAKE_COLOR equ 02h
SEED_COLOR equ 04h
GAMEOVER_COLOR equ 07h
SCORE_COLOR equ 0Eh
SCREEN_W equ 320
SCREEN_H equ 200
DIR_UP equ -SCREEN_W*3
DIR_RIGHT equ 3
DIR_DOWN equ SCREEN_W*3
DIR_LEFT equ -3
SNAKE_PIX_W equ 3
SNAKE_PIX_H equ 3
SCALE_BY equ SNAKE_PIX_W*SNAKE_PIX_H*2
MOVE_SPEED equ 240


; Setup VGA video mode
mov ax, 0013h
int 10h

push 0A000h
pop es


mov di, sprites
mov si, sprite_bitmaps
mov cl, 50
rep movsd
movsb

push es   ; es:di
pop ds    ; ds:si

call random_seed_pos

game_loop:
    hlt
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
        ;; input: CX = direction
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
            shl bx, 1
            mov si, playerBody

            .move_body:
                mov ax, [si+bx-2-18]
                xchg [si+bx-2], ax

                sub bx, 2
                cmp bx, 18
                jg .move_body

            .move_head:
                add [si+bx-2], cx
                sub bx, 2
                cmp bx, 0
                jg .move_head
    

    ; draw frame
    draw_frame:
        xor ah, ah
        mov di, 0
        mov cx, SCREEN_W*2
        mov al, GAMEOVER_COLOR
        rep stosb

        mov cx, SCREEN_H-4
        .draw_borders:
            stosb
            stosb
            add di, SCREEN_W-4
            stosb
            stosb
            loop .draw_borders

        mov cx, SCREEN_W*2
        mov al, GAMEOVER_COLOR
        rep stosb
    

    ; draw player
    mov bl, [si-1]
    shl bx, 1
    mov al, SNAKE_COLOR
    
    draw_body:
        mov cx, [si+bx-2]
        mov di, cx

        cmp byte [di], SNAKE_COLOR
        je game_over

        cmp byte [di], GAMEOVER_COLOR
        je game_over
        stosb
        
        sub bx, 2
        cmp bx, 0
        jg draw_body

    mov bx, 0
    mov si, seed
    mov al, SEED_COLOR
    draw_seed:
        mov di, [si+bx]
        cmp byte [di], SNAKE_COLOR
        je .new_seed

        cmp byte [di], SEED_COLOR
        je .new_pos
        stosb

        add bl, 2
        cmp bl, 18
        jl draw_seed
        jmp timer

        .new_seed:
            call random_seed_pos
            add byte [si+20], 9     ; increase length
            inc byte [si-2bh]        ; increase score
            jmp timer

        .new_pos:
            call random_seed_pos
            jmp draw_seed

    timer:
        inc bp
        cmp bp, MOVE_SPEED
        jl timer
        jmp game_loop


game_over:
    mov di, 90*SCREEN_W+120
    mov si, letter_g
    mov dl, [si-1]       ; score
    call draw_letter
    mov si, letter_a
    call draw_letter
    mov si, letter_m
    call draw_letter
    mov si, letter_e
    call draw_letter
    mov si, letter_o
    call draw_letter
    mov si, letter_v
    call draw_letter
    mov si, letter_e
    call draw_letter
    mov si, letter_r
    call draw_letter

    cli
    hlt


draw_letter:
    mov cx, 6
    .draw_row:
        mov al, byte [si]

        mov bl, 8
        .draw_pixel_loop:
            test al, 10000000b
            jz .skip_pixel
            
            cmp dl, 0
            jnz .color_with_score

            mov byte [di], GAMEOVER_COLOR
            jmp .skip_pixel

            .color_with_score:
                mov byte [di], SCORE_COLOR
                dec dx

    .skip_pixel:
        inc di
        shl al, 1
        dec bl
        jnz .draw_pixel_loop
        add di, SCREEN_W-8
        inc si
        loop .draw_row
    
    sub di, SCREEN_W*6-10
    ret

random_seed_pos:
    rdtsc
    xor dx, dx
    mov cx, SCREEN_W*SCREEN_H - 1 + 2
    div cx
    mov ax, dx
    add ax, 3

    push si
    mov si, seed
    mov cl, 0
    .expand_seed:

        mov bl, 0
        .fill_line:
            mov word [si], ax
            add si, 2
            dec ax
            
            inc bl
            cmp bl, 3
            jl .fill_line
        
        add ax, SCREEN_W+3
        inc cl
        cmp cl, 3
        jl .expand_seed
            
    pop si
    ret

;; DATA
sprite_bitmaps:
    db 0  ; score

;g_bitmap:
    db 00111110b
    db 01000000b
    db 01000000b
    db 01001110b
    db 01000010b
    db 00111110b

;a_bitmap:
    db 00111000b
    db 01000100b
    db 01000100b
    db 01111100b
    db 01000100b
    db 01000100b

;m_bitmap:
    db 01000010b
    db 01100110b
    db 01011010b
    db 01000010b
    db 01000010b
    db 01000010b

;e_bitmap:
    db 01111100b
    db 01000000b
    db 01111100b
    db 01000000b
    db 01000000b
    db 01111100b

;o_bitmap:
    db 00111100b
    db 01000010b
    db 01000010b
    db 01000010b
    db 01000010b
    db 00111100b

;v_bitmap:
    db 01000001b
    db 01000001b
    db 01000001b
    db 00100010b
    db 00100010b
    db 00011100b

;r_bitamp:
    db 01111100b
    db 01000010b
    db 01000010b
    db 01111100b
    db 01001000b
    db 01000110b

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
    db 27             ;; player length

    ;; snake positions
    dw 100*SCREEN_W+162    ;; head row * column
    dw 101*SCREEN_W+162    ;; head row + 1 * column
    dw 102*SCREEN_W+162    ;; head row + 2 * column
    dw 100*SCREEN_W+161
    dw 101*SCREEN_W+161
    dw 102*SCREEN_W+161
    dw 100*SCREEN_W+160
    dw 101*SCREEN_W+160
    dw 102*SCREEN_W+160
    dw 100*SCREEN_W+159
    dw 101*SCREEN_W+159
    dw 102*SCREEN_W+159
    dw 100*SCREEN_W+158
    dw 101*SCREEN_W+158
    dw 102*SCREEN_W+158
    dw 100*SCREEN_W+157
    dw 101*SCREEN_W+157
    dw 102*SCREEN_W+157

    

times 510 -($-$$) db 0
db 0x55
db 0xaa
