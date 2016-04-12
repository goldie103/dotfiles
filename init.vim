" Kelly Stewart

" Plugins {{{
" Vundle setup {{{
filetype off
if has("win32") || has("win64")
    set rtp+=$VIMDOTDIR\bundle\Vundle.vim
    call vundle#begin('$HOME\vimfiles\bundle')
else
    set rtp+=$VIMDOTDIR/bundle/Vundle.vim
    call vundle#begin()
endif

Plugin 'gmarik/Vundle.vim'

" TODO vim-args
Plugin 'nefo-mi/nyan-modoki.vim'
Plugin 'tpope/vim-commentary'           " comment manipulation
Plugin 'tpope/vim-surround'             " deal with surrounding elements
Plugin 'bling/vim-airline'              " status bar additions
Plugin 'bling/vim-bufferline'           " buffer list in command or status bar
Plugin 'tpope/vim-fugitive'             " call git commands within vim with G prefix
Plugin 'kien/ctrlp.vim'                 " fuzzy file search

Plugin 'morhetz/gruvbox'
"Plugin 'whatyouhide/vim-gotham'

"Plugin 'dogrover/vim-pentadactyl'        " pentadactylrc syntax highlighting
Plugin 'vimperator/vimperator.vim'       " vimperatorrc syntax highlighting

call vundle#end()
filetype plugin indent on
" }}}
" solarized {{{
"Plugin 'altercation/vim-colors-solarized'
"let g:solarized_menu=0
"let g:solarized_termtrans=1     " use terminal transparency
" }}}
" ctrlp.vim {{{
" Fuzzy file search
let g:ctrlp_match_window='bottom,order:ttb' " order files top to bottom
let g:ctrlp_switch_buffer=0                 " always open in a new buffer
let g:ctrlp_working_path_mode=0             " make ctrlp use vim working dir
" }}}
" vim-airline {{{
set encoding=utf8
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

" }}}
" nyan-modoki.vim {{{
set laststatus=2
set statusline=%{g:NyanModoki()}
let g:nyan_modoki_select_cat_face_number=2
let g:nyan_modoki_animation_enabled=1
" }}}
" }}}
" Appearance {{{
syntax on
colo gruvbox
set background=dark
set showmatch                  " highlight matching brackets
set matchtime=2                " 10ths of second to blink when matching brackets
set matchpairs=(:),{:},[:],<:> " include angle brackets in matching
set cursorline                 " highlight current line
set lazyredraw	       	       " don't redraw when executing macros
set scrolloff=3                " lines to keep visible above and below cursor
set ruler	            	   " show the cursor position in statusline
set wildmenu        		   " better command line completion
set showcmd	            	   " display incomplete commands
set laststatus=2	           " always display status line
set noeb novb 	               " don't beep or flash
set cmdheight=2                " prevent frequent need to scroll long commands
set number relativenumber      " show both relative and absolute line numbers

set guicursor+=a:blinkon0      " turn off cursor blinking
set guioptions-=T              " no toolbar

" }}}
" Formatting {{{
filetype plugin indent on
set autoindent smartindent
set shiftround                  " round to indent width
set expandtab smarttab          " turn tabs to spaces
set sw=4 ts=4 sts=4	            " indents of width 4
set wrap textwidth=80           " wrap lines at 80 cols

" File Specific Formatting
augroup configgroup
    autocmd!
    " 2-space indents
    autocmd FileType html setlocal sw=2 ts=2 sts=2
    autocmd FileType css setlocal sw=2 ts=2 sts=2
    autocmd FileType sh setlocal sw=2 ts=2 sts=2
augroup END
" }}}
" Behavior {{{
set formatoptions+=j            " delete comment char when joining lines
set viminfo+=n$XDG_CACHE_HOME/vim/viminfo " don't clutter my home directory
set ttimeout ttimeoutlen=100    " time out period for keypresses
set ignorecase smartcase   		" use case sensitive search when case is used
set incsearch		    		" do incremental searching
set confirm			         	" ask to confirm close without writing
set fic wic                     " ignore case in file names
set hidden                      " allow hidden buffers to show
set nobackup nowb noswapfile	" turn off all backup/temp files
set backspace=indent,eol,start	" allow backspacing over everything
set autoread			    	" autoread a file when changed from outside
set mouse=a	                	" enable mouse if possible
set modelines=1                 " for vimrc-specific folding
set encoding=utf-8              " unicode forever
set gdefault                    " global substitutions as default
set autochdir                   " change to dir of file in buffer
set clipboard=unnamedplus       " use system clipboards
set paste                       " paste from system
let mapleader = ','             " easier to reach
cd ~

" Return to last edit position when opening files
autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

" defines command to see changes made if not already defined
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                \ | wincmd p | diffthis
endif
" }}}
" Mappings {{{
" insert a single char
nnoremap \ a_<Esc>r
" file operations
nnoremap <leader>w :w<CR>
nnoremap <leader>e <C-p>
" q to quit, and Q to record macro. I never use Ex mode anyway.
nnoremap q :q<CR>
nnoremap Q q
" backspace in Visual mode deletes selection
vnoremap <BS> d
" stops C-U in insert mode deleting so much
inoremap <C-U> <C-G>u<C-U>
" more consistent behavior with change and delete
noremap Y y$
" easier to reach commands for common movements
noremap a ^
noremap s $
noremap " %
noremap <TAB> za
noremap <f1> :h 
noremap <SPACE> :
noremap gb :bn<CR>
noremap gB :bp<CR>
inoremap jk <ESC>
" move through wrapped lines visually
noremap j gj
noremap k gk
" ... and move linewise with old visual line commands
noremap gj j
noremap gk k
" split movement
nnoremap <C-l> <C-w>l
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-h> <C-w>h
" }}}

" vim:foldenable:foldmethod=marker:foldlevel=0
