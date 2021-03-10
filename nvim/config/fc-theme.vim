" config color theme

if $TERM == "linux"
   colorscheme iceberg
else
   if $FC_COLORFUL == "true"
      set termguicolors
   endif

   set background=dark
   colorscheme tender
endif
