#!/bin/bash

function init_darwin_font {
    # install personal fonts
    local font_src_path=${FCHOME}/fonts/
    local font_target_path=~/Library/Fonts/

    fc-copy ${font_src_path}/fzsongkebenxiukai.ttf ${font_target_path}/fzsongkebenxiukai.ttf
    fc-copy ${font_src_path}/STFangSong.ttf ${font_target_path}/STFangSong.ttf
    fc-copy ${font_src_path}/仓耳今楷01-27533-W03.ttf ${font_target_path}/仓耳今楷01-27533-W03.ttf

    fc-copy ${font_src_path}/FiraCode-BoldItalic.otf ${font_target_path}/FiraCode-BoldItalic.otf
    fc-copy ${font_src_path}/FiraCode-RegularItalic.otf ${font_target_path}/FiraCode-RegularItalic.otf

    # instal system fonts
    brew cask install font-firacode font-hack
}

function init_linux_font {
    fc-copy ${FCHOME}/fonts/fonts.conf ~/.config/fontconfig/fonts.conf

    # install personal fonts
    local font_src_path=${FCHOME}/fonts/
    local font_target_path=~/.local/share/fonts/

    fc-copy ${font_src_path}/fzsongkebenxiukai.ttf ${font_target_path}/fzsongkebenxiukai.ttf
    fc-copy ${font_src_path}/msyh.ttf ${font_target_path}/msyh.ttf
    fc-copy ${font_src_path}/msyhbd.ttf ${font_target_path}/msyhbd.ttf
    fc-copy ${font_src_path}/STFangSong.ttf ${font_target_path}/STFangSong.ttf
    fc-copy ${font_src_path}/仓耳今楷01-27533-W03.ttf ${font_target_path}/仓耳今楷01-27533-W03.ttf

    fc-copy ${font_src_path}/FiraCode-BoldItalic.otf ${font_target_path}/FiraCode-BoldItalic.otf
    fc-copy ${font_src_path}/FiraCode-RegularItalic.otf ${font_target_path}/FiraCode-RegularItalic.otf

    # instal system fonts
    sudo apt install fonts-firacode fonts-hack fonts-noto-color-emoji

    fc-cache
}

case $(uname) in
    Darwin)
        init_darwin_font
        ;;

    Linux)
        init_linux_font
        ;;
esac

unset -f init_darwin_font
unset -f init_linux_font
