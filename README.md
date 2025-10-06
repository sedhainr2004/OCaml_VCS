# GitCaml

## Team Members

- **Akul Maheshwari** 
- **Anthony Ahn** 
- **Jared Viani** 
- **Naveen Ramasamy** 
- **Rohan Sedhain** 

## Commands

### Ocaml Git Commands

| Command | Description |
|---------|-------------|
| `give <filename1, filename2,…> <message>` | Save a new version of files with a commit message |
| `get <id>` | Retrieve a specific version of files based on the given ID |
| `del <id>` | Delete a specific version of files based on the given ID |
| `show` | Display all saved versions of the files with ID, commit date, files changed, and commit messages |
| `diff <id1> <id2> <filename>` | Show the differences between two versions of a file based on the given IDs|
| `changes` | Check for changes in tracked files |

### Editor Commands

| Command | Description |
|---------|-------------|
| `find <id> <target> <replacement> <filename> (optional)` | Replace target text with replacement text in a file (optional parameter) |
| `insert <id> <filename> <line_number> <text>` | Insert text at the specified line number in the file of the given ID |
| `count <id> <filename>` | Returns the word count for the specified file of the given ID |
| `avg <id>` | Returns the average number of characters in all files of the given ID |
| `common_words <id> <n>` | Returns the 'n' most common words in all files of the given ID |
| `longest_words <id> <n>` | Returns the 'n' longest words in all files of the given ID |
| `line_count <id> <filename>` | Returns the number of lines in a specified file of the given ID |
| `summary <id> <filename>` | Generates a summary for the file including word count, line count, and character count of a specified file of the given ID |

### Other Commands

| Command | Description |
|---------|-------------|
| `explain <command>` | Displays a detailed explanation of given command |
| `quit` | Exit Version Control System |
