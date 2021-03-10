BEGIN {
	total=0;
	conflicted=0;
}

{
	total++;
	if ($1 == "??")
		new++;
	else if ($1 == "A")
		add++;
	else if ($1 == "D")
		deleted++;
	else if ($1 == "M")
		modified++;
	else if ($1 == "UU")
		conflicted++;
}

END {
	if (total != 0)
		printf(" %d", total);

	if (conflicted)
		printf("💥");
}
