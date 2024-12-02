""""
" This is a project-specific vimrc
"
" Set `PROJECT_SPECIFIC_VIMRC=path/to/a/vimrc` in the environment, with
" direnv(1) for instance and add the following snippet to the general vimrc:
"
"     " Load project-specific vimrc if $PROJECT_SPECIFIC_VIMRC path exists
"     if filereadable($PROJECT_SPECIFIC_VIMRC)
"       function DelayedEcho(timer)
"         echomsg
"           \ "Sourcing project-specific vimrc: `"
"           \   .. $PROJECT_SPECIFIC_VIMRC .. "'"
"       endfunction
"       " Delay echoing the message to avoid hit-enter-prompt
"       call timer_start(100, 'DelayedEcho')
"       source $PROJECT_SPECIFIC_VIMRC
"     endif
""""

" Couple maps for comfort
nmap <Leader>e <Cmd>up<CR><Cmd>!cabal run %:r<CR>
