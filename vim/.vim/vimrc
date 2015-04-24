" Kelly Stewart
set nocompatible
" Plugins {{{
" Vundle setup {{{
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

"Plugin 'scrooloose/nerdtree'            " visualized file browser
Plugin 'scrooloose/nerdcommenter'       " comment and uncomment blocks
"TODO: get commentary in here
Plugin 'bling/vim-airline'              " status bar additions
Plugin 'bling/vim-bufferline'           " buffer list in command or status bar
Plugin 'tpope/vim-fugitive'             " call git commands within vim with G prefix
Plugin 'tpope/vim-surround'             " deal with surrounding elements
"Plugin 'mileszs/ack.vim'                " in-file/folder definition search
"Plugin 'rstacruz/sparkup'               " HTML expansion (Zen Coding port)
Plugin 'kien/ctrlp.vim'                 " fuzzy file search
"Plugin 'scrooloose/syntastic'

Plugin 'altercation/vim-colors-solarized'
"Plugin 'morhetz/gruvbox'
"Plugin 'whatyouhide/vim-gotham'

Plugin 'dogrover/vim-pentadactyl'        " pentadactylrc syntax highlighting
"Plugin 'klen/python-mode'

call vundle#end()
filetype plugin indent on
" }}}
" solarized {{{
set background=dark
let g:solarized_termtrans=1     " use terminal transparency
" }}}
" ctrlp.vim {{{
" Fuzzy file search
let g:ctrlp_match_window='bottom,order:ttb' " order files top to bottom
let g:ctrlp_switch_buffer=0                 " always open in a new buffer
let g:ctrlp_working_path_mode=0             " make ctrlp use vim working dir
" }}}
" syntastic {{{
" Style and syntax checker. Look into issues with python-mode.
"let g:syntastic_always_populate_loc_list = 1    " always update error list on running check
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1               " check on open
"let g:syntastic_check_on_wq = 0                 " don't check on wq
" show warning msg in statusline
"set statusline+=%#warningmsg# %{SyntasticStatuslineFlag()} %*
" }}}
" python-mode {{{
" Python functionality suite. Include linting, doc reference, autocompletion
" and other.
" let g:pymode_rope = 1                        " activate rope
" let g:pymode_doc = 1                         " documentation
" let g:pymode_doc_key = 'K'                   " show doc on key
" let g:pymode_lint = 1                        " linting
" let g:pymode_lint_checkers = ["pyflakes","pep8"]  " checker and convention
" let g:pymode_lint_write = 1                  " auto check on save
" let g:pymode_virtualenv = 1                  " support virtualenv
" let g:pymode_breakpoint = 1                  " breakpoints
" let g:pymode_breakpoint_bind = '<leader'b'   " breakpoint key
" let g:pymode_folding=0                       " don't autofold code
" " syntax
" let g:pymode_syntax = 1
" let g:pymode_syntax_all = 1
" let g:pymode_syntax_indent_errors = g:pymode_syntax_all
" let g:pymode_syntax_space_errors = g:pymode_syntax_all
" }}}
" vim-airline {{{
set encoding=utf8
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

" powerline
let g:airline_left_sep = '▶'
let g:airline_left_alt_sep = '»'
let g:airline_right_sep = '◀'
let g:airline_right_alt_sep = '«'

"let g:airline_left_sep = ''
"let g:airline_left_alt_sep = ''
"let g:airline_right_sep = ''
"let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = ''
" unicode
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.whitespace = 'Ξ'

" }}}
" }}}
" Appearance {{{
" Formatting {{{
filetype plugin indent on
set autoindent
set et sta sw=4 ts=4 sts=4	    " 4-space indents
set wrap textwidth=80           " wrap lines at 80 cols

" File Specific Formatting
augroup configgroup
    autocmd!
    " 2-space indent for html and css
    autocmd FileType html setlocal sw=2 ts=2
    autocmd FileType css setlocal sw=2 ts=2
augroup END
" }}}
" Editing
syntax on
set hlsearch
color solarized
set showmatch mat=2     " highlight matching brackets and 10ths of a second to blink
set cursorline          " highlight current line
set lazyredraw	       	" redraw only when needed
set scrolloff=3         " lines to keep visible above and below cursor

" Vim UI
set ruler	            	" show the cursor position in statusline
set wildmenu        		" better command line completion
set showcmd	            	" display incomplete commands
set laststatus=2	        " always display status line
set noeb novb 	            " don't beep or flash
set cmdheight=2             " prevent frequent need to scroll long commands
set number relativenumber   " show both relative and absolute line numbers
" }}}
" Behavior {{{
set ignorecase smartcase   		" use case sensitive search only when needed
set incsearch		    		" do incremental searching
set confirm			         	" ask to confirm close without writing
set hidden                      " allow hidden buffers to show
set nobackup nowb noswapfile	" turn off backups
set dir=$TEMP                   " use correct folder for temp files
set backspace=indent,eol,start	" allow backspacing over everything
set autoread			    	" autoread a file when changed from outside
set mouse=a	                	" enable mouse if possible
set modelines=1                 " for vimrc-specific folding
set encoding=utf-8              " unicode forever
set gdefault                    " global substitutions as default
let mapleader = ','             " easier to reach
cd ~

" Mappings {{{
" file save
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
nnoremap <leader>o <C-p>
nnoremap <leader>x :x $myvimrc<CR>
" backspace in Visual mode deletes selection
vnoremap <BS> d
" stops C-U in insert mode deleting so much
inoremap <C-U> <C-G>u<C-U>
" more consistent behavior with change and delete
noremap Y y$
" move through wrapped lines visually
noremap j gj
noremap k gk
" jk to exit insert mode
inoremap jk <ESC>
" clear searches through <CR> press
nnoremap <silent><CR> :noh<CR><CR>
" <SPACE> to command
noremap <SPACE> :
" copy and paste using clipboard register by default
noremap y "*y
noremap p "*p
" split movement
nnoremap <C-l> <C-w>l
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-h> <C-w>h
" Bubble lines
nnoremap <silent> <leader>k :move-2<CR>==
nnoremap <silent> <leader>j :move+<CR>==
" }}}

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

" vim:foldenable:foldmethod=marker:foldlevel=0
