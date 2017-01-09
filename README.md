linotes
-----

`linotes` is a small terminal application for taking notes.

Installation
------------

    stack build
    stack install

Make sure to have `~/.local/bin` in your `PATH` variable.

Usage
-----

To take a note run

    note [NOTE CONTENTS]

e.g. `note star linotes repo on Github` or
    
    note

and then type the note content. Double `Enter` saves note in local database.

To view notes run

    notes

`notes` keybindings explained:
- `↑`, `↓`, `PgUp`, `PgDn`, `End`, `Home` - navigation
- `Ctrl` + `↑`, `Ctrl` + `↓` - note content scrolling
- `Enter` - open selected note
- `d` / `Del` - delete selected note
- `q` / `Esc` - quit application

Documentation
-------------
Documentation can be generated with haddock:

    stack haddock
    stack haddock --open

Sync
----
You can sync your notes between computers by keeping the `~/.linotes/notes.sqlite3` file in the cloud (e.g. using symlink).
