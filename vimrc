                                "Plugin Stuff
execute pathogen#infect()
filetype plugin indent on

":SCROLL
"pyclewn, frisk, vim-statline, syntastic, gundo.
let mapleader = '-'
let $PAGER='' "Part of resetting man's pager from less to vim
let g:scriptDir = '~/.vim/rcFunctions/'
let g:posHoldReg = 'n'
let g:spacing = 4
let g:bashInt = '/bin/bash'
let g:perlInt = '/usr/bin/perl'
let g:pythonInt = '/usr/bin/python'

"My highlight groups
highlight ExtraWhitespace ctermbg=blue 
highlight MakeTabGroup ctermbg=red

syntax enable
colorscheme default
set nocompatible
set smartindent
set smartcase
set expandtab
execute "set tabstop=" . g:spacing
execute "set shiftwidth=" . g:spacing
set nohlsearch
set hidden       "Don't ask to write buffer before i switch to a new buffer.
set autowrite
set laststatus=2 "laststatus is overriden by the statusline plugin.
set undolevels=1001 "the default is 1000. Can't be too cautious.
set statusline=  "the presence of my statusline shows that the plugin is
                 "read later than this nullification.
set autochdir "makes the cwd of a buffer the directory of its file (this may
              "have side-effects with plugins
set novisualbell
set confirm "rather than spitting errors, ask if i know what i'm doing
set encoding=utf-8
set showcmd "puts normal mode commands so far in the command-line section.
set backspace=2 "allows backspace to delete newline characters.
set tildeop "makes ~ act like an operator (e.g. y) rather than a macro.
set timeoutlen=700 "less than the default, just to see how it goes
set nostartofline "prevents <C-U> and <C-D> from modifying horizontal
                  "cursor position

                          "Text-Editing time-savers
"Swaps character under cursor with next character. This is a frequent fix
"for typos. Later, in this rcfile, <leader>s is mapped to calling this
"function.
function Swap()
   let b:currentline = getline(".")
   let b:currentchar = b:currentline[col(".")-1]
   let b:nextchar = b:currentline[col(".")]
   execute "normal r" . b:nextchar
   execute "normal lr" . b:currentchar . "h"
endfunction

"Relative numbering shows, on the far left of the screen, the number of lines
"away any line is from the current line. This is useful when you want to
"search and replace within a certain block of test, but it is too large
"to visually know how to address it.
function ToggleRelativeNumber()
    if &relativenumber
        set norelativenumber
    else
        set relativenumber
    endif
endfunction

"Highlights defined types in c files. Uses the inelegant method of using
"the actual file as a holding place for data input from the shell. Further
"lacks of elegance include using the position of fields in awk to determine
"what is a new type. sed and awk fare well in basic use cases and they are
"historically interesting, they leads to these sorts of handy, low-quality
"hacks.


"precondition: normal mode
"postcondition: normal mode
function BsTab()
    let i = 0
    normal i
    while i < &tabstop
        execute "normal a\<BS>"
        let i += 1
    endwhile
endfunction
    
function HiTypedefs()
    read! cat % | awk 'BEGIN {ORS=" "} $0 ~ "typedef" {print $3}' | sed 's/;//g'
    let line = getline(".")
    let newTypes = split(line) "converts space, sepearated string into list
    "of values from the words.
    for item in newTypes
        execute "syn keyword Type " . item
    endfor
    execute "echom len(newTypes) is " . len(newTypes)
    "the conditional deletion of the holding place line is there because
    "if zero typedefs are found, awk makes no extra line, so i need not delete.
    if len(newTypes) != 0
        normal dd
    endif
endfunction

"Trims tailing spaces from the end of lines.
function TrimSpaces()
    % s/\s\s*$//
endfunction

"A call to a program that I wrote that makes getters and setters for variables
"in the private section of a c++ class.
function GetSetGen()
    execute "!" . g:scriptDir . "GetSetGen.bash %"
endfunction

autocmd BufReadPost *
   \ if line("'\"") > 0 && line("'\"") <= line("$") |
   \ execute "normal! g`\"" |
   \ endif
"Next two lines are handy for changing the left hand side or right hand side
"of an assignment statement in c-like languages.
onoremap lhs :<c-u>execute "normal! ^vt="
onoremap rhs :<c-u>execute "normal! $vT="
"mapping jk to <ESC> is highly ergonomical and not a problem so long as
"one is not texting like a teenager.
inoremap jk <ESC>
vnoremap jk <ESC>
"The mapping below provides an 'undo' of actions so far done in insert mode,
"leaving one still "in insert mode.
inoremap <c-l> <ESC>ui
"When making headings, one often wants to center the current line and
"keep the next line left-justified.
inoremap cxc <ESC>:center<CR>j:left<CR>i
inoremap <leader><BS> <ESC>l:call BsTab()<CR>i
nnoremap cxc <ESC>:center<CR>j:left<CR>i
nnoremap <leader>s :call Swap()<CR>
"For making blank lines without leaving normal mode.
nnoremap <leader>o o<ESC>
nnoremap <leader>O O<ESC>
"On my keyboard, the first letter registers as upper-case if I enter command
"line mode quickly. Perhaps this is a problem with apple keyboards.
"These save a lot of red popping up.
command W w
command Q q
command Wq wq
"Plugin commands are long, so here are some abbreviations.
command St SyntasticToggleMode
command Gt GundoToggle
command! -nargs=+ Calc :py print <args>
python from math import *
"Opens up all files passed to vim as command line arguments in their
"own tab.
autocmd VimEnter * argdo tabedit
autocmd VimEnter * tabclose "This is here because the above command creates
"a tab for an empty file
"
" :args    shows list of arguments

"ex commands are anything simply starting with a colon
":changes   list of changes.
":chdir     changes working directory
"

"A stub for writing vim functions with the python interface. If you are
"confused by the python interface not working, try looking at the results of
"this ex-command: echo has('python'). If it returns 0, you will need to
"recompile vim so that it is configured with support for the python interface.
function PyRowCol()
python << endpython
import vim
(row,col) = vim.current.window.cursor
print row, col
endpython
endfunction

function SetInterpreter(interp)
    execute "normal I#!" . a:interp . "jko"
endfunction

   "Functions That Allow Operator-Pending Commenting-in and Commenting-out.
function! Com(commentSign, markOne, markTwo)
   let comment = ComStr() . ' '
   if a:commentSign == 1
      execute "normal! '" . a:markOne . "_\<C-V>'" . a:markTwo . 'I' . comment
   elseif a:commentSign == 0
      "this is fucking unreadable
      execute "'".a:markOne",'".a:markTwo . 's/^\(\s*\)' . escape(comment,'/') . '/\1/e'
   endif
endfunction

function DoCommentOp(type)
   call Com(1,'[',']')
endfunction

function UnCommentOp(type)
   call Com(0,'[',']')
endfunction

function ComStr()
   if (&ft=='c' || &ft=='cpp')
      return '//'
   elseif (&ft=='haskell')
      return '--'
   elseif (&ft=='perl' || &ft=='python' || &ft=='make' || &ft=='sh')
      return '#'
   elseif (&ft=='vim')
      return '"'
   else
      return ''
   endif
endfunction

"Before one can use those operator-pending comment operations, one has to
"set opfunc. Since this is error prone, here are some reliable mappings.
nnoremap <leader>c :set opfunc=DoCommentOp<CR>g@
nnoremap <leader>C :set opfunc=UnCommentOp<CR>g@
vnoremap <leader>c :call Com(1,'<','>')<CR>
vnoremap <leader>C :call Com(0,'<','>')<CR>
nnoremap <leader>h :set cursorcolumn
nnoremap <leader>H :set nocursorcolumn
nnoremap <leader>r :call ToggleRelativeNumber()<CR>

"These augroup makes newly written scripts automatically executable
"when one exists vim to test them out.
augroup scriptStartup
  autocmd!
  "the above line clears current definitions of autocommands in the
  "scriptStartup augroup.
  autocmd BufNewFile *.sh,*.bash call SetInterpreter(g:bashInt)
  autocmd BufNewFile *.pl call SetInterpreter(g:perlInt)
  autocmd BufNewFile *.py call SetInterpreter(g:pythonInt)
  autocmd VimLeavePre *.pl,*.py,*.bash,*.sh write
  autocmd VimLeave *.pl,*.py,*.bash,*.sh !chmod +x %
augroup END

augroup HighLighting
    "My function, Trim
    autocmd!
    autocmd InsertLeave * match ExtraWhitespace /\s\s*$/
augroup END

augroup FileTypes
    autocmd!
    autocmd FileType *.b set filetype=brainfuck
    autocmd FileType *.b set matchpairs=[:]
    autocmd FileType c ab pr printf
    autocmd FileType c,cpp set cindent
    autocmd FileType c,cpp  ab inc include
    "Preventing spaces/tabs mishaps in Makefiles.
    autocmd FileType make set noexpandtab "this overrides my default expandtab
    autocmd FileType make match MakeTabGroup /\t/
    autocmd FileType haskell set cinwords=where,do
    autocmd FileType html set matchpairs+=<:>
    "Making local foldmethods
    autocmd FileType python setlocal foldmethod=indent
    autocmd FileType python set cursorcolumn
    autocmd FileType *.xtm set filetype=Extempore
    autocmd FileType *.xtm source /home/alex/.vim/plugin/extempore.vim
augroup END
                            "Vim Filesystem Info.
"~/.vim/colors/
"   the color command looks for colorschemes here
"~/.vim/plugin
"   files in here will be run once every time Vim starts. They are meant to
"   contain startup code.
"~/.vim/ftdetect
"   functionally equivalent to plugin dir. These files should have filetype
"   specific settings, however.
"~/ftplugin
"   iff the name of the file is equal to the filetype variable, that file will
"run.
"This system is handy for my customizations, but it quickly becomes a mess
"with downloaded plugins. For that reason, the pathogen plugin exists. It
"handles namespacing issues by adding stuff to the runtimepath.

                               "Vimscript Notes
"After installing gvim, the 'vim' command still starts it up in the terminal
"window.
"Sourcing does not get rid of current configuration, it adds to it.
" . is for string concatentation
"Programming in vimscript:
"& means interpret it as an option, not a variable. e.g. let &expandtab = 0
"@ means interpret it as a register. e.g. let @e = 'mcsquared'
"Scoping
"  b:var    b:var is a variable local to the current buffer.
"  a:var    a:var is an argument in a function. Can be called by name or as
"     positional argument
"  g:var    g:var is a global variable (among buffers) (this is the default)
"     it is not, however, persistent among sessions.
"I can treat things as a variable when a hardcoded string is expected by
"doing the command in an execute string with the variable concatenated into
"the command string.
"when mapping with searches, I need to conclude the searches with a \<CR>
"in execute strings, I need to quote the < > characters to give them their
"special meaning.
"f is the 'search forward for a single char' operator
"F is the 'search backwards for a single char' operator
"t is to f as e is to w. T is to F as E is to W.
"
"the current file symbol, %, is technically a register name.
