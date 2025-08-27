while getopts $ARGUMENTS OPT; do
    if [[ $OPT == "h" ]]; then
        echo -e $USAGE
        exit
    elif [[ $OPT == "?" ]]; then
        echo -e "\nInvalid option!!!\n"
        echo -e $USAGE
        exit
    fi

    arg-set $OPT $OPTARG
done

shift $((OPTIND - 1))
