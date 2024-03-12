BEGIN {
    total=0
    conflicted=0
}

{ total++ }

$1 == "??" { new++ }
$1 == "A" { add++ }
$1 == "D" { deleted++ }
$1 == "M" { modified++ }
$1 == "UU" { conflicted++ }

END {
    if (total != 0)
        printf(" %d", total)

    if (conflicted)
        printf(" 💥%d", conflicted)
}
