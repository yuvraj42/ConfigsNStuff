" ====================================================
" vim-plug bootstrap: auto-installs plug.vim if absent
" ====================================================
let data_dir = '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" ====================================================
" Plugins
" ====================================================
call plug#begin('~/.vim/plugged')

" File finding
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }  " fzf binary
Plug 'junegunn/fzf.vim'                               " :Files :Buffers :Rg

" Git
Plug 'tpope/vim-fugitive'         " :Git, :Gdiffsplit, :Git blame

" File browser
Plug 'tpope/vim-vinegar'          " press - to open netrw; fixes papercuts

" Editing essentials
Plug 'tpope/vim-commentary'       " gcc to comment line, gc + motion
Plug 'tpope/vim-surround'         " cs"' ysiw] ds( etc.

" LSP / completion / linting (heavy but self-contained)
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" After install, run these in vim to get language servers:
"   :CocInstall coc-pyright       " Python (replaces elpy)
"   :CocInstall coc-hls            " Haskell (needs haskell-language-server on PATH)
"   :CocInstall coc-json coc-yaml  " bonus

" Syntax / language support
Plug 'neovimhaskell/haskell-vim'  " better Haskell syntax/indentation
Plug 'vim-python/python-syntax'   " richer Python highlighting
Plug 'chrisbra/csv.vim'           " CSV alignment and navigation

" Visual
Plug 'Yggdroot/indentLine'        " indent guides (like highlight-indent-guides)
Plug 'preservim/vim-markdown'     " Markdown syntax, folding, :Toc
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app && npx --yes yarn install' }
" :MarkdownPreview opens a live browser preview of the current .md file

call plug#end()

" ====================================================
" Basic settings
" ====================================================
set nocompatible
set encoding=utf-8
set title

" Appearance
set number
set cursorline
set scrolloff=8
set wrap
set showmatch

" Indentation
set tabstop=4
set shiftwidth=4
set expandtab
set smartindent
set autoindent

" Search
set ignorecase
set smartcase
set incsearch
set hlsearch

" File finding
set path+=**
set wildignore+=*/node_modules/*,*/.git/*,*.pyc,*/__pycache__/*

" Usability
set backspace=indent,eol,start
set mouse=a
set clipboard=unnamedplus
set undofile
set undodir=~/.vim/undodir//  " trailing // encodes full file path in undo filename,
                              " so two files named foo.py in different dirs don't collide
set noswapfile
set nobackup
set nowritebackup     " prevents the .filename~ temp file during writes

" Ensure undodir exists
if !isdirectory(expand('~/.vim/undodir'))
  call mkdir(expand('~/.vim/undodir'), 'p')
endif

" Performance
set lazyredraw
set ttyfast

" Visual feedback
set showcmd
set showmode
set ruler
set laststatus=2
set wildmenu
set wildmode=longest:full,full

" Splits open naturally
set splitbelow
set splitright

" ====================================================
" Mappings
" ====================================================

" Clear search highlight with Escape
nnoremap <Esc> :nohlsearch<CR>

" ====================================================
" coc.nvim settings (LSP / completion)
" ====================================================

" Tab to trigger completion, navigate completion menu
inoremap <silent><expr> <Tab>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()
inoremap <expr><S-Tab> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

" Enter to confirm completion
inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm() : "\<CR>"

" LSP navigation (replaces elpy M-. / M-,)
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)

" Jump back: use C-o (vim's native jumplist - works for LSP jumps too)

" Hover docs
nnoremap <silent> K :call CocActionAsync('doHover')<CR>

" Rename symbol
nmap <leader>rn <Plug>(coc-rename)

" Diagnostics (flycheck equivalent)
nmap <silent> [d <Plug>(coc-diagnostic-prev)
nmap <silent> ]d <Plug>(coc-diagnostic-next)
