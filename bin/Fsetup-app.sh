#!/bin/bash

case $(uname) in
    Darwin)
        brew install bear cmake htop iterm2 make most mpg123 pandoc sqlite tmux the_silver_searcher tidy-html5

        brew install --cask adobe-acrobat-reader amazon-music calibre goldendict kitty libreoffice microsoft-excel microsoft-powerpoint microsoft-word mpv openemu quodlibet
        ;;

    Linux)
        . /etc/os-release

        case $ID in
            debian)
                sudo apt install build-essential cowsay bear cmake espeak-ng fasd figlet ffmpeg fortunes graphviz htop lolcat mbrola mbrola-us2 meld most mpg123 mpv neovim pandoc python3-pip ripgrep scons silversearcher-ag tidy
                ;;

            ubuntu)
                sudo add-apt-repository ppa:mmstick76/alacritty
                sudo apt install alacritty bat build-essential cowsay bear cmake espeak-ng fasd figlet ffmpeg graphviz htop lolcat mbrola mbrola-us2 meld most mpg123 mpv neovim pandoc ripgrep scons silversearcher-ag tidy

                for x in lsd shfmt; do
                    sudo snap install $x
                done

                # snap install alacritty --channel latest/edge --classic

                # setup ruby
                # sudo apt install ruby ruby-dev
                # for x in colorls; do
                #     sudo gem install colorls
                # done

                ;;

            arch | manjaro*)
                sudo pacman -S --needed alacritty avahi base-devel bat cowsay cmake espeak-ng fasd figlet ffmpeg go graphviz htop lolcat meld most mpg123 mpv neovim nss-mdns p7zip scons shfmt ripgrep tidy
                ;;
        esac
        ;;
esac
