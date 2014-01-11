# mvtv

Find and move TV show files into a media collection.

## Usage

```
$ mvtv [--watch]
```

Find video files in the current directory. For each file found:

* Find a show under `$TV_SHOWS` which matches the file
* Parse a season number out of the file
* Move the file to `$TV_SHOWS/show/season/basename`

If a show cannot be found, `mvtv` will error and skip that file. If a 
season cannot be parsed, it will be copied in at show-level. If a season 
directory does not yet exist, one will be created.

`TV_SHOWS` can be set as an environment variable and defaults to 
`/mnt/media/TV_shows`.

If `--watch` is used, wait and execute the above whenever files are 
created in the current directory.

## Installation

```
$ git clone https://github.com/pbrisbin/mvtv
$ cd mvtv
$ cabal install
```
