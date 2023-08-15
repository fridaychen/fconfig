#!/usr/bin/env bash

case $(uname) in
    Darwin)
        if [ ! $(which brew) ]; then
            curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh
        fi

        pkgs=(
            autoconf
            automake
            bash
            bc
            bear
            cmake
            coreutils
            fasd
            fzf
            gcc
            git
            giflib
            gnutls
            gsed
            htop
            jansson
            jpeg
            libgccjit
            libreoffice
            libtiff
            libxml2
            mactex
            make
            mpg123
            most
            mpv
            openemu
            pandoc
            pkg-config
            python
            quodlibet
            shfmt
            sqlite
            texinfo
            the_silver_searcher
            tidy-html5
            translate-shell
        )

        cask_pkgs=(
            goldendict
            google-chrome
            kitty
        )

        brew install ${pkgs[@]}
        brew install ${cask_pkgs[@]}

        if [ ! $(grep homebrew /etc/shells) ]; then
            sudo cat /opt/homebrew/bin/bash >>/etc/shells
            chsh -s /opt/homebrew/bin/bash
        fi
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
