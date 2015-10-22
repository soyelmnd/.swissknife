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
Plugin 'groenewege/vim-less'
Plugin 'itchyny/lightline.vim'
Plugin 'jiangmiao/auto-pairs'
Plugin 'mattn/emmet-vim'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/nerdtree'
Plugin 'sjl/gundo.vim'
Plugin 'tmhedberg/matchit'
Plugin 'tommcdo/vim-exchange'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-surround'
Plugin 'Yggdroot/indentLine'

call vundle#end()
filetype plugin indent on
syntax on

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

" tab stops
set tabstop=4
set softtabstop=4
set shiftwidth=4
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
\     [ 'fugitive' ],
\     [ 'readonly', 'filename', 'modified' ]
\   ],
\   'right': [
\     [ 'lineinfo' ],
\     [ 'percent' ],
\     [ 'fileformat', 'fileencoding', 'filetype' ]
\   ]
\ },
\ 'component_function': {
\   'fugitive': 'LightlineFugitive',
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

function! LightlineFugitive()
  if exists("*fugitive#head")
    let _ = fugitive#head()
    return strlen(_) ? ' '._ : ''
  endif
  return ''
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
