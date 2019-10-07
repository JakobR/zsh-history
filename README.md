# zsh-history

A command-line tool to manipulate [Zsh](https://www.zsh.org/) history files.


## Unison Integration

[Unison](https://github.com/bcpierce00/unison) is a file synchronization tool
that can resolve conflicts automatically via external merge tools.
The following line in a unison profile enables automatic conflict resolution
for Zsh history files (conflicts are resolved by merging the command history):

```
merge = Name .zsh_history -> zsh-history format --format zsh --output NEW --sort --dedup-lite CURRENT1 CURRENT2
```

See [Unison's user manual](https://www.cis.upenn.edu/~bcpierce/unison/docs.html)
for additional related options.
For example, adding `backupcurrent = Name .zsh_history` instructs unison to
keep backups of the original files before merging.
