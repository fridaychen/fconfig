"config status line

set laststatus=2
set statusline=

set statusline+=%#DiffAdd#%{(mode()=='n')?'\ \ NORMAL\ ':''}
set statusline+=%#DiffChange#%{(mode()=='i')?'\ \ INSERT\ ':''}
set statusline+=%#DiffDelete#%{(mode()=='r')?'\ \ RPLACE\ ':''}
set statusline+=%#Cursor#%{(mode()=='v')?'\ \ VISUAL\ ':''}

set statusline+=%4l:%3c "
set statusline+=%#Cursor#

set statusline+=\ %r
set statusline+=\ %m

set statusline+=%#CursorLine#
set statusline+=\ %f

set statusline+=%=

set statusline+=%#CursorLine#
set statusline+=\ %n\ "
set statusline+=%#Visual#
set statusline+=\ %3p%%\ "
