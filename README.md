# Air
## A stack-based assembly-like language

- Four-byte-cell stack
- Designed to work well with brain*
    - Tape is the stack
    - Compiles to brain*
- One byte of storage per cell (due to brain*)

Basic syntax highlighting for VS Code can be found at [`Kyllingene/vscode-airc`](https://github.com/Kyllingene/vscode-airc).

Code examples:
```air
push 5 ; stack = [5]
push 6 ; stack = [5, 6]
swp    ; stack = [6, 5]
add    ; stack = [11]
dup    ; stack = [11, 11]
mul    ; stack = [121]
dup    ; stack = [121, 121]
pout   ; stack = [121], prints "y"
inc    ; stack = [122]
pout   ; stack = [], prints "z"
```

```air
push 'A'     ; stack = [65 ('A')]

push 1
loop         ; pops top value (condition)
    out      ; stack = [65]; doesn't pop top value
    add 1    ; stack = [66]

    dup      ; stack = [66, 66]
    neq 'Z'  ; stack = [66, 1]
end ; pops top value (condition)

pout     ; stack = [], prints "Z"
out '\n' ; prints literal, no effect on stack
```
