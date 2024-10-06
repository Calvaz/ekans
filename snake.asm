
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
DIR_UP equ -SCREEN_W*3
DIR_RIGHT equ 3
DIR_DOWN equ SCREEN_W*3
DIR_LEFT equ -3
SNAKE_PIX_W equ 3
SNAKE_PIX_H equ 3
SCALE_BY equ SNAKE_PIX_W*SNAKE_PIX_H*2

struc GameState
    .score resb 0
endstruc


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
            mov ax, [si+bx-6]
            mov [si+bx], ax
            mov ax, [si+bx-4]
            mov [si+bx+2], ax
            mov ax, [si+bx-2]
            mov [si+bx+4], ax

            .move_body:
                mov ax, [si+bx-2-18]
                mov [si+bx-2], ax

                sub bx, 2
                cmp bx, 18
                jg .move_body

            .move_head:
                add [si+bx-2], cx
                push cx
                mov cx, [si+bx-2]
                push bx
                mov bx, 0

                ; am i hitting seed
                .is_hitting_seed:
                    cmp [si-15+bx], cx     ; seed pos
                    je .new_seed

                    add bx, 2
                    cmp bx, 18
                    jne .is_hitting_seed
                    jmp .move_head_2

                .new_seed:
                    call random_seed_pos
                    add byte [si-1], 9     ; increase length
            
            .move_head_2:
                pop bx
                pop cx
                sub bx, 2
                cmp bx, 0
                jg .move_head
    
    ; draw player
    xor ah, ah
    mov bl, [si-1]
    shl bx, 1
    mov al, SNAKE_COLOR
    
    draw_body:
        mov cx, [si+bx-2]   ; column = row + colNum
        mov di, cx
        stosb
        
        sub bx, 2
        cmp bx, 0
        jg draw_body

    xor bh, bh
    mov bl, 0
    mov si, seed
    mov al, SEED_COLOR
    draw_seed:
        mov di, [si+bx]
        stosb

        add bl, 2
        cmp bl, 18
        jl draw_seed

    mov bp, 100
    mov si, 100
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
