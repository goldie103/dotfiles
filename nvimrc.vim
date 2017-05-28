" Kelly Stewart

" Install plugin manager if it doesn't exit
if empty(glob('$XDG_CONFIG_HOME/nvim/autoload/plug.vim'))
  silent !curl -fLo $XDG_CONFIG_HOME/nvim/autoload/plug.vim --create-dirs
   \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('$XDG_CONFIG_HOME/nvim/plugged')
Plug 'tpope/vim-sensible'             " sane defaults
Plug 'tpope/vim-surround'             " deal with surrounding elements
Plug 'bling/vim-airline'              " status bar additions
Plug 'kien/ctrlp'                     " fuzzy file finding
Plug 'morhetz/gruvbox'                " theeeemee
call plug#end()

" CtrlP 
let g:ctrlp_match_window='bottom,order:ttb' " order files top to bottom
let g:ctrlp_switch_buffer=0                 " always open in a new buffer
let g:ctrlp_working_path_mode=0             " make ctrlp use vim working dir

" Airline
let g:airline_powerline_fonts = 1
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
" powerline
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
" unicode
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.whitespace = 'Ξ'

color gruvbox
set background=dark
set showmatch                  " highlight matching brackets
set matchpairs=(:),{:},[:],<:> " include angle brackets in matching
set cursorline                 " highlight current line
set lazyredraw	       	       " don't redraw when executing macros
set scrolloff=3                " lines to keep visible above and below cursor
set showcmd	            	     " display incomplete commands
set noeb novb 	               " don't beep or flash
set cmdheight=2                " prevent frequent need to scroll long commands
set number relativenumber      " show both relative and absolute line numbers

set guicursor+=a:blinkon0      " no cursor blinking
set guioptions-=T              " no toolbar

set shiftround                 " round to indent width
set expandtab sw=2 ts=2 sts=2  " tab settings
set wrap textwidth=80          " wrap lines at 100 cols
set ignorecase smartcase incsearch " search settings
set fic wic                     " ignore case in file names
set hidden                      " allow hidden buffers to show
set nobackup nowb noswapfile	  " turn off all backup/temp files
set autoread			              " autoread a file when changed from outside
set mouse=a	                    " enable mouse if possible
set encoding=utf-8              " unicode forever
set gdefault                    " global substitutions as default
set autochdir                   " change to dir of file in buffer
set clipboard=unnamedplus       " use system clipboards

" For some reason, enabling this makes
" fd <Esc> mapping cease working
"set paste                       " paste from system

let mapleader = ','             " easier to reach

" Return to last edit position when opening files
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

" file operations
noremap <leader>w :w<CR>
noremap <leader>e <C-p>
noremap <leader>b :bn<CR>
" stop C-U in insert mode deleting so much
inoremap <C-U> <C-G>u<C-U>
" easier to reach commands for common movements
nnoremap Y y$
noremap " %
noremap <SPACE> :
inoremap fd <Esc>
vnoremap fd <Esc>
cnoremap fd <Esc>
" move through wrapped lines visually
noremap j gj
noremap k gk
noremap gj j
noremap gk k
" split movement
nnoremap <C-l> <C-w>l
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-h> <C-w>h

" vim:foldenable:foldmethod=marker:foldlevel=0

