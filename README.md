# mvtv

Move TV show files into a media collection.

## Usage

```
$ mvtv FILE [, ...]
```

## Installation

```
$ git clone https://github.com/pbrisbin/mvtv
$ cd mvtv
$ cabal install
```

## Examples

A collection is assumed to resemble the following:

```
/mnt/media/TV_shows
|- Some show
|  |- season_1
|  `- season_2
`- Another show
   |- Season 1
   `- Season 2
```

### Existing Values

```
$ mvtv *.mkv
/mnt/media/TV_shows:
1 - Some show
2 - Another show

Select an entry or enter a new value: 1
/mnt/media/TV_shows/Some show:
1 - season_1
2 - season_2

Select an entry or enter a new value: 2
```

At this point, every file that matched `*.mkv` will be moved into 
`/mnt/media/TV_shows/Some show/season_2`.

### Alternate Collection Path

A non-default collection path can be passed by setting the `TV_SHOWS` 
environment variable. Place an `export` line in a shell startup file to 
make this permanent.

```
$ TV_SHOWS="$HOME/Shows" mvtv *.mkv
```

### New Values

Add new shows or seasons by typing the name. Note that any invalid 
choice is treated as a new directory and will be created. Use `^C` if 
you mean to abort.

```
$ mvtv *.mkv
/mnt/media/TV_shows:
1 - Some show
2 - Another show

Select an entry or enter a new value: New show
/mnt/media/TV_shows/New show:

Select an entry or enter a new value: season_1
```

This will move `*.mkv` into `/mnt/media/TV_shows/New show/season_1`, 
creating the directories as needed.
