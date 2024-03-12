$1 == "??" { untracked++ }
$1 == "A" { added++ }
$1 == "D" { deleted++ }
$1 == "M" { modified++ }
$1 == "UU" { conflicted++ }

END {
    if (NR != 0) {
        if (untracked)
            elts[i++] = "?" untracked
        if (modified)
            elts[i++] = "!" modified
        if (deleted)
            elts[i++] = "✘" deleted
        if (added)
            elts[i++] = "+" added
        if (conflicted)
            elts[i++] = "💥" conflicted

        detail = elts[0]
        for (j = 1; j < i; j++) {
            detail = detail " " elts[j]
        }

        printf(" %d [%s]", NR, detail)
    }
}
