#!/bin/bash

case $(uname) in
    Darwin)
        pkgs=(
            autoconf
            automake
            bc
            bear
            cmake
            gcc
            git
            giflib
            gnutls
            htop
            jansson
            jpeg
            libgccjit
            libreoffice
            libtiff
            libxml2
            make
            mpg123
            most
            mpv
            openemu
            pandoc
            pkg-config
            quodlibet
            sqlite
            texinfo
            the_silver_searcher
            tidy-html5
        )

        cask_pkg=(
            goldendict
            kitty
        )

        brew install ${pkgs[@]}
        brew install --cask ${cask_pkgs[@]}
        ;;

    Linux)
        . /etc/os-release

        case $ID in
            debian)
                sudo apt install bc build-essential cowsay bear cmake espeak-ng fasd figlet ffmpeg fortunes graphviz htop lolcat mbrola mbrola-us2 meld most mpg123 mpv neovim pandoc python3-pip ripgrep scons silversearcher-ag tidy
                ;;

            ubuntu)
                sudo add-apt-repository ppa:mmstick76/alacritty
                sudo apt install alacritty bat bc build-essential cowsay bear cmake espeak-ng fasd figlet ffmpeg graphviz htop lolcat mbrola mbrola-us2 meld most mpg123 mpv neovim pandoc ripgrep scons silversearcher-ag tidy

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
                sudo pacman -S --needed alacritty avahi base-devel bat bc cowsay cmake espeak-ng fasd figlet ffmpeg go graphviz htop lolcat meld most mpg123 mpv neovim nss-mdns p7zip scons shfmt ripgrep tidy
                ;;
        esac
        ;;
esac
