# Air
## A stack-based assembly-like language

- Two byte-based stacks
- Designed to work well with brain*
    - Tape is the stack
    - Compiles to brain*
- One byte per cell (due to brain*)

Code examples:
```
push 5 ; stack = [5]
push 6 ; stack = [5, 6]
swp    ; stack = [6, 5]
add    ; stack = [11]
dup    ; stack = [11, 11]
mul    ; stack = [121]
dup    ; stack = [121, 121]
out    ; stack = [121], prints "y"
inc    ; stack = [122]
out    ; stack = [], prints "z"
```

```
; print A-Z plus a newline

push 64 ; start
push 26 ; i

loop

; stack = [start, i]
swp

; stack = [i, start]
push 1

; stack = [i, start, 1]
add

; stack = [i, start++]
dup

; stack = [i, start, start]
out

; stack = [i, start]
swp

; stack = [start, i]
push 1

; stack = [start, i, 1]
sub

; stack = [start, i--]
end

; stack = [start=91, i=0]
; print newline
push 10
out
```
