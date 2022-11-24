# -*- mode: sh; sh-shell: bash; -*-

case $(uname) in
    Darwin)
        XARGS=gxargs
        FIND=gfind
        GREP=ggrep
        SED=gsed
        DATE=gdate
        ;;
    Linux)
        XARGS=xargs
        FIND=find
        GREP=grep
        SED=sed
        DATE=date
        ;;
esac
