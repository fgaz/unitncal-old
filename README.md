# unitncal

**calendar scraper for unitn.it, live at http://unitncal.fgaz.me**

[![Build Status](https://travis-ci.org/fgaz/unitncal.svg?branch=master)](https://travis-ci.org/fgaz/unitncal)

This program downloads the calendar files used by unitn's "orari" webapp and
converts them from the custom format to standard iCal, which is then
easily imported in many calendar apps.

![example result](static/screenshots/android/06.png)

## Usage

Simply use `cabal run` (`cabal install`, then `unitncal` also works, assuming
that `~/.cabal/bin` is in your `PATH`).

`unitncal` will do an initial scraping before starting the actual server.

You can choose the default port, host, scraping url, scraping interval etc by editing Config.hs
(sorry, no external config file for now)

