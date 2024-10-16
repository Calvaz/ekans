I wanted to make a game that could fit into a 512 bytes bootloader, in 16 bit real mode.
This is usable without any Operating System, by any IBM PC compatible machine.
The score is calculated by drawing the "Game Over" text with yellow pixels. The more you score the more the text will be colored with yellow pixels.

![](https://i.imgur.com/ii2rbD8.gif)

## Steps:

### Install 
- [nasm]
- [qemu]

### Build
```console
$ nasm snake.asm -o snake
```

### Run

```console
$ qemu-system-i386 snake
```

[nasm]: https://www.nasm.us/
[qemu]: https://www.qemu.org/
