" Dependencies
"   https://github.com/VundleVim/Vundle.vim
"   https://github.com/powerline/fonts
set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'ap/vim-css-color'
Plugin 'digitaltoad/vim-jade'
Plugin 'easymotion/vim-easymotion'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'floobits/floobits-neovim'
Plugin 'groenewege/vim-less'
Plugin 'itchyny/lightline.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'mattn/emmet-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/nerdtree'
Plugin 'sjl/gundo.vim'
Plugin 'tmhedberg/matchit'
Plugin 'tommcdo/vim-exchange'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'Yggdroot/indentLine'

call vundle#end()

" basic
filetype plugin indent on
syntax on
set lazyredraw

" theme and color
set t_Co=256
set background=dark
colorscheme koehler

" numbering and rulers
set relativenumber
set number
set cursorline
set colorcolumn=80
highlight ColorColumn ctermbg=7

" fix normal keys, and lock mouse
set backspace=indent,eol,start
set mouse=

" new window or pane should be appended to bottom right
set splitbelow
set splitright

" handy mapping
set pastetoggle=<leader>p
nnoremap ; :
nnoremap j gj
nnoremap k gk

if bufwinnr(1)
  " pane resize vertically = -
  " and horizontally + _
  map = 5<c-w>>
  map - 5<c-w><
  map + 5<c-w>+
  map _ 5<c-w>-
endif

" tab stops
set tabstop=2
set softtabstop=2
set shiftwidth=2
set expandtab

" searching
set showcmd
set hlsearch
set modifiable
set smartcase
set ignorecase
map <space> :noh<cr>

" show hidden chars
set listchars=tab:>-,trail:.
set list

" text format
set wrap
set showmatch

" disable swap files
set nobackup
set nowritebackup
set noswapfile

" large file handle
let g:LargeFile = 10 * 1024 * 1024
augroup LargeFile
  autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END
function! LargeFile()
  set eventignore+=FileType " disable filetype related features
  noswapfile
  setlocal bufhidden=unload " save memory when other file is viewed
  setlocal buftype=nowrite
  setlocal undolevels=1
  autocmd VimEnter *  echo "Entering large-file-mode as file is larger than " . (g:LargeFile / 1024 / 1024) . "MB"
endfunction

" sudo switch with w!!
cmap w!! w !sudo tee % >/dev/null


" neovim
if has('nvim')
  " terminal
  tnoremap <Esc> <C-\><C-n>

  tnoremap <C-w>h <C-\><C-n><C-w>h
  tnoremap <C-w>j <C-\><C-n><C-w>j
  tnoremap <C-w>k <C-\><C-n><C-w>k
  tnoremap <C-w>l <C-\><C-n><C-w>l

  tnoremap <C-w>H <C-\><C-n><C-w>H
  tnoremap <C-w>J <C-\><C-n><C-w>J
  tnoremap <C-w>K <C-\><C-n><C-w>K
  tnoremap <C-w>L <C-\><C-n><C-w>L

  autocmd BufWinEnter,WinEnter term://* startinsert
  autocmd BufLeave term://* stopinsert
endif


"--------------------------------- PLUGIN -------------------------------------"


" Emmet
" @see https://github.com/mattn/emmet-vim
"   tab to expand
imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")


" Exchange
" @see https://github.com/tommcdo/vim-exchange
let g:exchange_no_mappings=1
nmap cx <Plug>(Exchange)
vmap X <Plug>(Exchange)
nmap cxc <Plug>(ExchangeClear)
nmap cxx <Plug>(ExchangeLine)


" Gundo
" @see https://github.com/sjl/gundo.vim
let g:gundo_right=1
let g:gundo_close_on_revert = 1
let g:gundo_preview_height=25
nnoremap <leader>u :GundoToggle<cr>


" Indent line
" @see https://github.com/Yggdroot/indentLine
let g:indentLine_color_term = 239
let g:indentLine_char = '┆'


" Light line
" @see https://github.com/itchyny/lightline.vim
set laststatus=2
let g:lightline = {
\ 'colorscheme': 'wombat',
\ 'active': {
\   'left': [
\     [ 'mode', 'paste' ],
\     [ 'readonly', 'filename', 'modified' ]
\   ],
\   'right': [
\     [ 'lineinfo' ],
\     [ 'percent' ],
\     [ 'fileformat', 'fileencoding', 'filetype' ]
\   ]
\ },
\ 'component_function': {
\   'readonly': 'LightlineReadonly',
\   'modified': 'LightlineModified',
\   'filename': 'LightlineFilename'
\ },
\ 'separator': { 'left': '', 'right': ''  },
\ 'subseparator': { 'left': '', 'right': ''  }
\ }

function! LightlineModified()
  if &filetype == "help"
    return ""
  elseif &modified
    return "+"
  elseif &modifiable
    return ""
  else
    return ""
  endif
endfunction

function! LightlineReadonly()
  if &filetype == "help"
    return ""
  elseif &readonly
    return ""
  else
    return ""
  endif
endfunction

function! LightlineFilename()
  return ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
  \ ('' != expand('%:t') ? expand('%:t') : '[Unnamed]') .
  \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction


" Nerdtree
" @see https://github.com/scrooloose/nerdtree
"   Ctrl + N to toggle
"   and show-on folder open
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>


"--------------------------------- EXTRA -------------------------------------"


" Load local vimrc, if any
silent! source ~/.vimrc.local
