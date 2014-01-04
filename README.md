# mvtv

Move TV show files into a media collection.

## Usage

```
$ mvtv FILE [, ...]
```

For each `FILE` passed, `mvtv` will:

* Find a show under `TV_SHOWS` which matches the file
* Parse a season number out of the file
* Move the file to `TV_SHOWS/show/season/basename`

If show cannot be found, `mvtv` will error and skip that file. If a 
season cannot be parsed, it will be copied in at show-level. If a season 
directory does not yet exist, one will be created.

`TV_SHOWS` can be set as an environment variable and defaults to 
`/mnt/media/TV_shows`.

## Installation

```
$ git clone https://github.com/pbrisbin/mvtv
$ cd mvtv
$ cabal install
```
