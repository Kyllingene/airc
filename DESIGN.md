# Design for Air (brain* backend)

Air is designed to compile to brain*, so that's where all of its design decisions come from.

When talking about air, I tend to use "cell" to refer to an air data unit and "byte" to refer to an individual byte of data (a brain* "cell"). In docs, I use "value" to refer to the value in a cell; cells don't actually move, but values do.

The brain* tape is divided into 4-byte cells, with the exception of the first 4 bytes. This allows for convenient home-finding and cell-finding. Each cell looks like this:
    - cell[0]: used (toggle)
    - cell[1]: data
    - cell[2]: temp storage
    - cell[3]: tag

The toggle byte is set for every cell that is currently occupied, and unset for free cells (and the home cell). There is one exception: when an operation requires going home, it needs to find its way back (i.e. `swpn`). In this case, the toggle byte is unset until the operation returns, when it gets reset. This allows homing with something like this (to return): `< [>>>>] >`.

The data and temp bytes are self-explanatory, and the tag byte is used for operations like `cid` to ID a specific cell. This lets you perform a loop like `>> --[++>>>>--]++ <<` to find a specific cell, without having to rely on absolute position in the stack. The tag byte is *not* cleared when a cell is popped because it allows clever stack usage to place values into a tagged cell afterwards.

I have chosen to emphasize functionality over convenience. There are some instructions (such as `rot`) that aren't strictly necessary, but those are either there because I added them before realizing the amount of instructions I would require, or because they are very useful in stack-based programming. Once the MVP has been reached, I will possibly add more instructions; I may also take a macro approach, defining new functions simply as combinations of existing instructions.
