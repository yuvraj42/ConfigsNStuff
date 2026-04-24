# Vim cheat sheet (with emacs mappings)

## Non-default behaviour in this vimrc

Things that differ from vanilla vim due to settings in your vimrc. Worth knowing so you're not confused on a bare remote machine.

| Setting | What it changes | Vanilla vim default |
|---|---|---|
| `set ignorecase` + `set smartcase` | Search is case-insensitive unless you type a capital | Case-sensitive always |
| `set hlsearch` + `Esc` → `:nohlsearch` | Search matches stay highlighted; `Esc` clears them | No persistent highlight; no Esc mapping |
| `set splitbelow` + `set splitright` | New splits open below / to the right | Splits open above / to the left |
| `set scrolloff=8` | Cursor never gets closer than 8 lines to the edge | `scrolloff=0` |
| `set wildmenu` + `wildmode=longest:full,full` | Tab in `:` commands shows a navigable menu | Basic single completion |
| `set mouse=a` | Mouse works in all modes | Mouse off |
| `set clipboard=unnamed` | `y`/`p` use the system clipboard directly | Vim's own clipboard, separate from system |
| `set undofile` + `undodir=~/.vim/undodir//` | Undo history persists across sessions; `//` encodes full path in undo filename so two `foo.py` in different dirs don't collide | No persistent undo |
| `set nobackup` + `set nowritebackup` + `set noswapfile` | No `filename~` or `.filename.swp` files created | Swap files on, write backup on |
| `set number` | Line numbers shown | No line numbers |
| `set cursorline` | Current line highlighted | No highlight |
| `gd / gr / K / [d / ]d` | LSP navigation | No LSP; `gd` goes to local definition only |
| `Tab` in insert mode | Triggers completion menu | Inserts a literal tab (or spaces per expandtab) |

---

## Files and directories

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `:e path/to/file` | Open file; Tab completes path | `C-x C-f` |
| `:e %:h/<Tab>` | Open from current file's directory | `C-x C-f` from current dir |
| `:find filename<Tab>` | Find file recursively across project | `C-x C-f` fuzzy; needs `set path+=**` (in vimrc) |
| `:Explore` or `-` | Open netrw file browser | dired; `-` needs vim-vinegar |
| `:Sexplore` / `:Vexplore` | Netrw in horizontal / vertical split | |
| `gf` | Open file path under cursor | |
| `:Files` / `:Telescope find_files` | Fuzzy file finder | ivy/helm `C-x C-f` |

Inside netrw: `Enter` opens, `-` goes up, `d` makes dir, `%` makes file, `D` deletes, `R` renames.

---

## Buffers

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `:ls` or `:buffers` | List open buffers | `C-x C-b` |
| `:b substring<Tab>` | Switch to buffer by partial name | `C-x b foo<Tab>` |
| `Ctrl-^` or `:b#` | Toggle to previous buffer | `C-x b <Enter>` |
| `:bn` / `:bp` | Next / previous buffer | |
| `:bd` | Close buffer (closes window too if it's the only one) | `C-x k` |
| `:bp \| bd #` | Close buffer, keep window layout | `C-x k` without losing splits |
| `:Buffers` / `:Telescope buffers` | Fuzzy buffer switcher | |

---

## Windows and splits

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `:split` or `C-w s` | Horizontal split | `C-x 2` |
| `:vsplit` or `C-w v` | Vertical split | `C-x 3` |
| `C-w w` | Cycle to next window | `C-x o` |
| `C-w h/j/k/l` | Move to window left/down/up/right | directional `C-x o` |
| `:close` or `C-w c` | Close current window | `C-x 0` |
| `:only` or `C-w o` | Close all other windows | `C-x 1` |
| `:sp file` / `:vsp file` | Open file in new split | `C-x 4 f` |
| `C-w =` | Balance all window sizes | |
| `C-w +` / `C-w -` | Taller / shorter | |
| `C-w >` / `C-w <` | Wider / narrower | |
| `C-w _` / `C-w \|` | Max height / max width | |
| `C-w H/J/K/L` | Move window to far edge | |

Closing a buffer with `:bd` closes its window too if no other buffer is shown there — same as killing a buffer in emacs closing a side window.

---

## Terminal

| Vim | What it does | Notes |
|---|---|---|
| `Ctrl-z` | Suspend vim, drop to shell | `fg` to return; best option if you like Ghostty |
| `:!cmd` | Run one shell command | e.g. `:!ls` or `:!python %` |
| `:r !cmd` | Read command output into buffer | |
| `:term` | Built-in terminal in current window | |
| `:vert term` | Built-in terminal in vertical split | |
| `C-w N` (in :term) | Terminal → normal mode | Lets you yank terminal output |
| `i` (in :term normal mode) | Back to terminal | |

---

## Navigation

| Vim | What it does |
|---|---|
| `%` | Jump to matching bracket / paren / brace |
| `{` / `}` | Jump to previous / next empty line (paragraph motion) |
| `(` / `)` | Jump to previous / next sentence |
| `H` / `M` / `L` | Jump to top / middle / bottom of screen |
| `zz` / `zt` / `zb` | Scroll so cursor is at middle / top / bottom of screen |
| `gj` / `gk` | Move by visual line (useful when `wrap` is on and a line wraps) |
| `g_` | Jump to last non-blank character of line |
| `''` | Jump back to position before last big jump (`%`, `G`, `gg`, etc.) |

---

## Selecting and copying

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `v` | Start char-wise visual select, move to extend | `C-SPC` |
| `V` | Start line-wise visual select | `C-SPC` then `C-n`/`C-p` |
| `C-v` | Start visual block (rectangle) | `C-x SPC` |
| `y` (in visual) | Yank (copy) selection | `M-w` |
| `d` (in visual) | Delete (cut) selection | `C-w` |
| `p` / `P` | Paste after / before cursor | `C-y` |
| `yy` / `dd` | Yank / delete whole line | |
| `yw` / `dw` | Yank / delete word | |
| `y$` / `d$` | Yank / delete to end of line | |
| `yap` / `dip` | Yank a paragraph / delete inner paragraph | text objects |
| `ci"` / `ca(` | Change inside quotes / including parens | text objects |
| `gg VG` | Select all lines | |
| `gg yG` | Copy entire file | |
| `gg dG` | Delete entire file contents | |
| `:%y` | Yank entire file without moving cursor | |

With `clipboard=unnamed` in your vimrc, plain `y`/`p` already use the system clipboard.

**Text objects** are the biggest ergonomic win over emacs: `iw` inner word, `aw` a word with surrounding space, `ip`/`ap` paragraph, `i"`/`a"`, `i(`/`a(`, `it`/`at` for HTML tags. Use them with any operator: `y`, `d`, `c`, `v`.

---

## Registers — vim's multiple clipboards

| Vim | What it does |
|---|---|
| `"ayy` | Yank line into register `a` |
| `"ap` | Paste from register `a` |
| `"0p` | Paste last yank specifically — not overwritten by `d` like the default register |
| `:reg` | Show all register contents |

`"0` is the one people discover late and wish they'd known earlier: when you yank something then delete something else, the default register `"` gets overwritten by the delete. `"0` always holds your last explicit yank, unaffected by deletes.

---

## Rectangle / visual block

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `C-v`, select, `I`, type, `Esc` | Insert text at start of each line | `C-x r t` |
| `C-v`, select, `A`, type, `Esc` | Append text at end of each line | |
| `C-v`, select, `d` | Delete block | `C-x r d` |
| `C-v`, select, `y` | Yank block | `C-x r y` |
| `C-v`, select, `r<Space>` | Clear block (replace with spaces) | `C-x r c` |

The `Esc` after typing in block-insert mode is what propagates the text down all selected rows.

---

## Editing

| Vim | What it does |
|---|---|
| `o` / `O` | Open new line below / above and enter insert mode |
| `A` | Append at end of line |
| `I` | Insert at start of line (first non-blank character) |
| `C` | Change to end of line (equivalent to `c$`) |
| `D` | Delete to end of line (equivalent to `d$`) |
| `S` | Delete whole line and enter insert mode (equivalent to `cc`) |
| `x` | Delete character under cursor |
| `~` | Toggle case of character under cursor |
| `J` | Join current line with line below |
| `gq` + motion | Reflow / hard-wrap text to `textwidth` (useful in comments and markdown) |
| `>>` / `<<` | Indent / dedent current line |
| `>` / `<` in visual | Indent / dedent selection (stays in visual so you can repeat with `.`) |
| `C-a` / `C-x` | Increment / decrement number under cursor |

---

## Macros

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `qa` | Start recording into register `a` (any letter works) | `C-x (` |
| `q` (while recording) | Stop recording | `C-x )` |
| `@a` | Play macro in register `a` | `C-x e` |
| `@@` | Replay last macro | `e` after `C-x e` |
| `100@a` | Play macro 100 times | `C-u 100 C-x e` |
| `"ap` | Paste register `a` to inspect what was recorded | |

---

## Marks — bookmarks within and across files

| Vim | What it does |
|---|---|
| `ma` | Set mark `a` at current position (any letter) |
| `` `a `` | Jump to exact position of mark `a` |
| `'a` | Jump to line of mark `a` |
| `` `. `` | Jump to position of last edit |
| `` `[ `` / `` `] `` | Jump to start / end of last yank or change |
| `''` | Jump back to position before last big jump |

---

## Search and replace

| Vim | What it does |
|---|---|
| `/pattern` | Search forward |
| `?pattern` | Search backward |
| `n` / `N` | Next / previous match |
| `*` / `#` | Search for word under cursor (forward / backward) |
| `:%s/old/new/g` | Replace all in file |
| `:%s/old/new/gc` | Replace all, confirm each |
| `:'<,'>s/old/new/g` | Replace in visual selection (select first, then `:s`) |

---

## Code navigation (LSP)

| Vim | What it does | Emacs equivalent |
|---|---|---|
| `gd` | Go to definition | `M-.` (elpy-goto-definition) |
| `gr` | Show all references / usages | |
| `gy` | Go to type definition | |
| `gi` | Go to implementation | |
| `K` | Hover docs / type info | |
| `C-o` | Jump back (jumplist) | `M-,` / pop-tag-mark |
| `C-i` or `Tab` | Jump forward (jumplist) | |
| `[d` / `]d` | Previous / next diagnostic | flycheck next/prev error |
| `C-]` / `C-t` | Jump to def / back (ctags, built-in) | needs `ctags -R .` |

---

## Folds

| Vim | What it does |
|---|---|
| `za` | Toggle fold open / closed |
| `zR` | Open all folds |
| `zM` | Close all folds |
| `zf` + motion | Create manual fold (e.g. `zfap` folds a paragraph) |
| `zd` | Delete fold at cursor |

---

## Markdown (vim-markdown + markdown-preview.nvim)

| Vim | What it does |
|---|---|
| `:MarkdownPreview` | Open live preview in browser (auto-updates on save) |
| `:MarkdownPreviewStop` | Close browser preview |
| `]]` / `[[` | Next / previous heading |
| `]h` / `[h` | Next / previous heading at same level |
| `ze` / `zc` | Fold / unfold at heading |
| `:Toc` | Open table of contents; navigate with Enter |
| `:set conceallevel=2` | Hide markup symbols for cleaner reading |

---

## Command line

| Vim | What it does |
|---|---|
| `q:` | Open command history in an editable window |
| `q/` | Open search history in an editable window |
| `C-r C-w` | (in `:` mode) paste word under cursor into command line |

`C-r C-w` is particularly useful — put cursor on a function name and do `:%s/<C-r><C-w>/newname/g` to rename it without typing it out.

---

## General / misc

| Vim | What it does |
|---|---|
| `:w` | Save |
| `:wq` or `ZZ` | Save and quit |
| `:q!` or `ZQ` | Quit without saving |
| `:wqa` | Save all buffers and quit |
| `u` / `C-r` | Undo / redo (note: redo is `C-r`, not `C-/`) |
| `.` | Repeat last change — the dot command, use it constantly |
| `:N` or `Ngg` | Go to line N |
| `:e` (no args) | Reload file from disk; `:e!` discards unsaved changes |
| `C-g` or `:f` | Show current file path and status |
| `:help topic` | Open help, e.g. `:help :bd` or `:help C-w` |
| `:verbose set backup?` | See which file last set an option (debug backup issues) |
