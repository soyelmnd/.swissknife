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
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
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
set colorcolumn=80
highlight ColorColumn ctermbg=7

" fix normal keys, and lock mouse
set backspace=indent,eol,start
set mouse=

" window and pane
set splitbelow
set splitright

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

" hidden chars
set listchars=tab:>-,trail:.
set list

" large file
let g:LargeFile = 10 * 1024 * 1024
augroup LargeFile
  autocmd BufReadPre * let f=getfsize(expand("<afile>")) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
  augroup END

  function LargeFile()
    set eventignore+=FileType " disable filetype related features
    noswapfile
    setlocal bufhidden=unload " save memory when other file is viewed
    setlocal buftype=nowrite
    setlocal undolevels=1
    autocmd VimEnter *  echo "Entering large-file-mode as file is larger than " . (g:LargeFile / 1024 / 1024) . "MB"
endfunction


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
"   tab to expand
imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")


" Exchange
let g:exchange_no_mappings=1
nmap cx <Plug>(Exchange)
vmap X <Plug>(Exchange)
nmap cxc <Plug>(ExchangeClear)
nmap cxx <Plug>(ExchangeLine)


" Gundo
let g:gundo_right=1
let g:gundo_close_on_revert = 1
let g:gundo_preview_height=25
nnoremap <F5> :GundoToggle<CR>


" Indent line
let g:indentLine_color_term = 239
let g:indentLine_char = '┆'


" Light line
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
"   Ctrl + N to toggle
"   and show-on folder open
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
map <C-n> :NERDTreeToggle<CR>


"--------------------------------- PLUGIN -------------------------------------"


" Load local vimrc, if any
silent! source ~/.vimrc.local
